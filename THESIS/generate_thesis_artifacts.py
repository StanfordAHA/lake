#!/usr/bin/env python3
"""Top-level driver: produce every figure + table referenced by
``main_thesis.tex`` (or a subset selected via ``--only``).

Reads ``THESIS/pipeline/registry.py`` — that file is the source of truth
for which artifacts exist, where they land, what data they need, and
which generator (if any) makes them.

For each registry entry:

    status=real   → call generator(ctx, out). On MissingDataError, fall
                    back to a red BOGUS placeholder and note it in
                    output/logs/missing.log.
    status=bogus  → always write BOGUS placeholder (data doesn't exist
                    yet). Also logged to missing.log.
    status=manual → skip. Manual artifacts live in the tex tree and are
                    tracked here only so nothing is forgotten. Logged
                    once to output/logs/manual.log for visibility.

The comments below each per-figure invocation summarize what data feeds
that figure and what still needs to be produced upstream.

Usage:
    python3 THESIS/generate_thesis_artifacts.py
    python3 THESIS/generate_thesis_artifacts.py --top /path/to/THESIS_BUILDS
    python3 THESIS/generate_thesis_artifacts.py --only port_area_vs_data_width
    python3 THESIS/generate_thesis_artifacts.py --list
    python3 THESIS/generate_thesis_artifacts.py --only iter_dom_area_vs_dim --view pct
    python3 THESIS/generate_thesis_artifacts.py --status real --preview-views /tmp/views

Sweep figures (``tile_figures.VIEWABLE``) draw raw values, % change per x
step, or both, per their registry ``view``. ``--view`` overrides it for the
run; ``--preview-views DIR`` instead writes each selected sweep figure in all
three views to DIR (PNGs + index.html side by side) without touching output/.
"""

from __future__ import annotations

import argparse
import base64
import dataclasses
import sys
from datetime import datetime
from pathlib import Path

# Make ``pipeline`` importable when run as a script.
REPO_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(REPO_ROOT))

from THESIS.pipeline import MissingDataError  # noqa: E402
from THESIS.pipeline.bogus import make_bogus_figure, make_bogus_table  # noqa: E402
from THESIS.pipeline.build_query import load_builds_df  # noqa: E402
from THESIS.pipeline.generators import GenContext  # noqa: E402
from THESIS.pipeline.registry import REGISTRY, Entry  # noqa: E402
from THESIS.pipeline.tile_figures import VIEWABLE, VIEWS  # noqa: E402

OUTPUT_ROOT = REPO_ROOT / "THESIS" / "output"
LOG_DIR = OUTPUT_ROOT / "logs"
DEFAULT_TOP = Path("/Users/maxwellstrange/THESIS_BUILDS")


def _log(path: Path, msg: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a") as f:
        f.write(f"{datetime.now().isoformat(timespec='seconds')}  {msg}\n")


def _handle_entry(e: Entry, ctx: GenContext | None, view: str | None = None) -> str:
    """Produce ``e``'s output artifact (``view`` overrides ``e.view``). Returns
    a short status string."""
    out = OUTPUT_ROOT / e.output

    if e.status == "manual":
        _log(LOG_DIR / "manual.log", f"[{e.id}] manual — {e.source}")
        return "manual (skipped)"

    if e.status == "bogus":
        reason = e.notes or "data not yet available"
        if e.kind in ("table", "listing"):
            make_bogus_table(out, title=e.label, reason=reason)
        else:
            make_bogus_figure(out, title=e.label, reason=reason)
        _log(LOG_DIR / "missing.log", f"[{e.id}] BOGUS emitted — {reason}")
        return "bogus"

    if e.status == "real":
        if e.generator is None:
            raise RuntimeError(f"registry entry {e.id} is 'real' but has no generator")
        if ctx is None:
            raise RuntimeError("no build context — pass --top or ensure DEFAULT_TOP exists")
        try:
            e.generator(dataclasses.replace(ctx, view=view or e.view), out)
            return "real"
        except MissingDataError as err:
            reason = f"MissingDataError: {err}"
            if e.kind in ("table", "listing"):
                make_bogus_table(out, title=e.label, reason=reason)
            else:
                make_bogus_figure(out, title=e.label, reason=reason)
            _log(LOG_DIR / "missing.log", f"[{e.id}] BOGUS fallback — {reason}")
            return "bogus (fallback)"

    raise RuntimeError(f"unknown status {e.status!r} for {e.id}")


def _preview_views(entries: list[Entry], ctx: GenContext, outdir: Path) -> None:
    """Every selected sweep figure in each view, as DIR/<id>__<view>.png, plus
    DIR/index.html (self-contained) showing the views side by side."""
    outdir.mkdir(parents=True, exist_ok=True)
    rows = []
    for e in entries:
        if e.status != "real" or e.generator not in VIEWABLE:
            continue
        cells = []
        for v in VIEWS:
            png = outdir / f"{e.id}__{v}.png"
            try:
                e.generator(dataclasses.replace(ctx, view=v), png)
            except MissingDataError as err:
                cells.append(f"<td class=miss>{v}: {err}</td>")
                continue
            b64 = base64.b64encode(png.read_bytes()).decode()
            cells.append(f'<td><img src="data:image/png;base64,{b64}" alt="{e.id} {v}"></td>')
        rows.append(f"<tr><th>{e.id}<br><small>registry view: {e.view}</small></th>{''.join(cells)}</tr>")
        print(f"  {e.id:44s} → previewed", file=sys.stderr)
    head = "".join(f"<th>{v}</th>" for v in VIEWS)
    (outdir / "index.html").write_text(
        "<!doctype html><meta charset=utf-8><title>Figure views</title><style>"
        "body{font:13px system-ui;margin:16px;background:#fff;color:#111}"
        "table{border-collapse:collapse}td,th{border:1px solid #ddd;padding:6px;vertical-align:top}"
        "th{text-align:left;font-weight:600}img{width:330px}.miss{color:#a33;width:330px}"
        f"</style><table><tr><th>figure</th>{head}</tr>{''.join(rows)}</table>")
    print(f"\nPreviews under: {outdir}", file=sys.stderr)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--top", type=Path, default=DEFAULT_TOP,
                    help="THESIS_BUILDS root (only needed for 'real' generators).")
    ap.add_argument("--only", nargs="*", metavar="ID",
                    help="Only process these registry IDs.")
    ap.add_argument("--kind", choices=["figure", "table", "listing"],
                    help="Filter by kind.")
    ap.add_argument("--status", choices=["real", "bogus", "manual"],
                    help="Filter by status.")
    ap.add_argument("--list", action="store_true",
                    help="List registry entries and exit.")
    ap.add_argument("--refresh-cache", action="store_true",
                    help="Force re-walk of THESIS_BUILDS instead of using cached CSV.")
    ap.add_argument("--view", choices=VIEWS,
                    help="Draw every selected sweep figure in this view instead of its registry view.")
    ap.add_argument("--preview-views", type=Path, metavar="DIR",
                    help="Write each selected sweep figure in all views to DIR (+ index.html); "
                         "output/ is left alone.")
    args = ap.parse_args(argv)

    entries = list(REGISTRY)
    if args.only:
        wanted = set(args.only)
        entries = [e for e in entries if e.id in wanted]
    if args.kind:
        entries = [e for e in entries if e.kind == args.kind]
    if args.status:
        entries = [e for e in entries if e.status == args.status]

    if args.list:
        for e in entries:
            print(f"{e.status:6s}  {e.kind:6s}  {e.id:44s}  {e.output}")
        return 0

    # Only spin up the build DataFrame if there's a real generator to run.
    ctx = None
    if any(e.status == "real" for e in entries):
        if args.top.is_dir():
            df = load_builds_df(args.top, refresh=args.refresh_cache)
            ctx = GenContext(df=df, top_builds=args.top.resolve())
            print(f"loaded {len(df)} builds from {args.top}", file=sys.stderr)
        else:
            print(f"warning: --top {args.top} not found; 'real' entries will BOGUS-fall-back", file=sys.stderr)

    if args.preview_views:
        if ctx is None:
            print("error: --preview-views needs the build context (--top)", file=sys.stderr)
            return 1
        _preview_views(entries, ctx, args.preview_views)
        return 0

    counts: dict[str, int] = {}
    for e in entries:
        try:
            status = _handle_entry(e, ctx, args.view)
        except Exception as err:  # noqa: BLE001 — one bad generator shouldn't kill the run
            status = f"ERROR: {err}"
            _log(LOG_DIR / "errors.log", f"[{e.id}] {err}")
        counts[status] = counts.get(status, 0) + 1
        print(f"  {e.id:44s} → {status}", file=sys.stderr)

    print("\nSummary:", file=sys.stderr)
    for k, v in sorted(counts.items()):
        print(f"  {k:20s}  {v}", file=sys.stderr)
    print(f"\nOutputs under: {OUTPUT_ROOT}", file=sys.stderr)
    print(f"Logs under:    {LOG_DIR}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
