#!/usr/bin/env python3
"""Top-level driver for the app-mapping harness.

Cross-iterates ``design_points.DESIGNS_SINGLE_LEVEL × registry.APPS``,
invokes the round-trip flow per cell, and drops one ``results.json`` at
``THESIS/data/apps/<design_id>/<app_id>/results.json`` containing:

    {
      "design_id": "...",
      "app_id": "...",
      "sim_status": "PASS" | "FAIL" | "SKIP",
      "active_cycles": <int>,        # sum across tiles from tb.sv util.txt
      "total_cycles":  <int>,
      "utilization":   <float>,      # active/total, cell-level
      "per_tile_util": [{...}, ...], # per-tile breakdown for later plotting
      "tile_count":    <int>,        # number of memory tiles in this app
      "synth_total_area_um2":   <float>,   # from extract_power_area
      "synth_logic_area_um2":   <float>,   # total - storage
      "synth_storage_area_um2": <float>,
      "synth_power_w":          <float>,   # None until ptpx-synth sweep runs
      "clock_period_ps":        <float>,
      "crit_path_delay_ps":     <float>,
      "roundtrip_returncode":   <int>,
    }

The heavy lifting is delegated to ``ASPLOS_EXP/run_roundtrip_sweep.sh``
(now parameterized with ``--app-dir`` / ``--testname``); this driver
composes the per-cell sweep-config line, shells out, then parses
``summary.txt`` + per-tile ``outputs/util.txt`` + the extractor
DataFrame row for the design's build dir.

Usage:
    python3 -m THESIS.apps.run_matrix --apps all --designs all
    python3 -m THESIS.apps.run_matrix --apps matmul_agg --designs port_fw4_dw16_sc8k_sp2x2
    python3 -m THESIS.apps.run_matrix --dry-run   # compose config files without dispatch
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path

from .design_points import DESIGNS_SINGLE_LEVEL, DesignPoint
from .registry import APPS, AppSpec, unverified

REPO_ROOT = Path(__file__).resolve().parents[2]
RESULTS_ROOT = REPO_ROOT / "THESIS" / "data" / "apps"
DEFAULT_SWEEP_SCRIPT = REPO_ROOT / "ASPLOS_EXP" / "run_roundtrip_sweep.sh"
DEFAULT_AHA_ROOT = "/aha"
DEFAULT_BUILDS_ROOT = Path(
    os.environ.get("THESIS_BUILDS", "/Users/maxwellstrange/THESIS_BUILDS")
)


# ---------- config-line composition ----------------------------------------


def _sweep_args_from_design(dp: DesignPoint) -> str:
    """Build the ``thesis_sweep.py`` CLI args string for this design."""
    parts = [
        f"--fetch_width {dp.fetch_width}",
        f"--data_width {dp.data_width}",
        f"--storage_capacity {dp.storage_cap_bytes}",
        f"--in_ports {dp.in_ports}",
        f"--out_ports {dp.out_ports}",
    ]
    if dp.dual_port:
        parts.append("--dual_port")
    return " ".join(parts)


def _spec_kwargs_from_design(dp: DesignPoint) -> str:
    """Build the ``dict(...)`` kwargs string consumed by
    ``lake.utils.clockwork_roundtrip.write_roundtrip_artifacts``."""
    return (
        f"storage_capacity={dp.storage_cap_bytes}, "
        f"data_width={dp.data_width}, "
        f"fetch_width={dp.fetch_width}, "
        f"in_ports={dp.in_ports}, "
        f"out_ports={dp.out_ports}, "
        f"dual_port={bool(dp.dual_port)}"
    )


def _app_dir_from_spec(app: AppSpec, aha_root: str) -> str:
    """Resolve AppSpec.halide_app_dir to an absolute path under the AHA
    Halide-to-Hardware checkout."""
    return f"{aha_root.rstrip('/')}/Halide-to-Hardware/apps/hardware_benchmarks/{app.halide_app_dir.lstrip('/')}"


def compose_sweep_line(dp: DesignPoint, app: AppSpec, aha_root: str) -> str:
    """Return the pipe-delimited line for run_roundtrip_sweep.sh."""
    return "|".join([
        dp.id,
        _sweep_args_from_design(dp),
        _spec_kwargs_from_design(dp),
        _app_dir_from_spec(app, aha_root),
        app.testname,
    ])


# ---------- parse post-run artifacts ---------------------------------------


def _parse_sim_status(summary_path: Path | str, name: str) -> str:
    """Grep run_roundtrip_sweep.sh's summary.txt for the design's verdict."""
    summary_path = Path(summary_path)
    if not summary_path.is_file():
        return "MISSING"
    for line in summary_path.read_text().splitlines():
        if line.startswith(f"{name}:"):
            _, _, verdict = line.partition(":")
            return verdict.strip().split()[0] if verdict.strip() else "UNKNOWN"
    return "UNKNOWN"


def _read_util_txt(util_path: Path) -> dict:
    """Same shape as run_roundtrip_sim.parse_util_txt — duplicated here to
    avoid dragging in that module (which pulls sim-only deps)."""
    if not util_path.is_file():
        return {}
    try:
        parts = util_path.read_text().split()
        active, total = int(parts[0]), int(parts[1])
    except (OSError, ValueError, IndexError):
        return {}
    out = {"active_cycles": active, "total_cycles": total}
    if total > 0:
        out["utilization"] = active / total
    return out


def _aggregate_util(run_cell_dir: Path) -> dict:
    """Walk ``run_cell_dir/cfg_*_sim/outputs/util.txt`` and roll up per-tile
    counts into a single cell-level utilization number."""
    per_tile = []
    tile_dirs = sorted(run_cell_dir.glob("cfg_*_sim"))
    for tdir in tile_dirs:
        u = _read_util_txt(tdir / "outputs" / "util.txt")
        if u:
            per_tile.append({"tile": tdir.name, **u})
    if not per_tile:
        return {"tile_count": len(tile_dirs), "per_tile_util": []}
    total_a = sum(t["active_cycles"] for t in per_tile)
    total_t = sum(t["total_cycles"] for t in per_tile)
    agg = {
        "tile_count": len(tile_dirs),
        "per_tile_util": per_tile,
        "active_cycles": total_a,
        "total_cycles": total_t,
    }
    if total_t > 0:
        agg["utilization"] = total_a / total_t
    return agg


def _lookup_ppa(design: DesignPoint, builds_root: Path) -> dict:
    """Return the extractor row for this design as a plain dict, or {}
    if the builds root or the matching row isn't available."""
    if not builds_root or not Path(builds_root).is_dir():
        return {}
    try:
        from ..pipeline.build_query import load_builds_df
        from ..pipeline.tables import _find_design_row
    except Exception:
        return {}
    try:
        df = load_builds_df(Path(builds_root))
    except FileNotFoundError:
        return {}
    row = _find_design_row(df, design)
    if row is None:
        return {}
    keep = [
        "synth_total_area_um2",
        "synth_cell_area_um2",
        "synth_storage_area_um2",
        "pnr_total_area_um2",
        "pnr_macro_area_um2",
        "synth_power_w",
        "pnr_power_w",
        "clock_period_ps",
        "wns_ps",
        "crit_path_delay_ps",
        "build_dir",
    ]
    ppa = {k: (None if _isnan(row.get(k)) else _to_native(row.get(k))) for k in keep if k in row.index}
    total = row.get("synth_total_area_um2")
    storage = row.get("synth_storage_area_um2")
    if not _isnan(total) and not _isnan(storage):
        ppa["synth_logic_area_um2"] = float(total) - float(storage)
    return ppa


def _isnan(v) -> bool:
    try:
        return v is None or (isinstance(v, float) and v != v)
    except Exception:
        return False


def _to_native(v):
    """Convert numpy scalars to plain python so json.dump doesn't choke."""
    if hasattr(v, "item"):
        return v.item()
    return v


# ---------- dispatch --------------------------------------------------------


def run_one_cell(
    design: DesignPoint,
    app: AppSpec,
    *,
    dry_run: bool = False,
    aha_root: str = DEFAULT_AHA_ROOT,
    builds_root: Path | str | None = None,
    sweep_script: Path | str | None = None,
    timeout_s: int = 1800,
) -> dict:
    """Run one (design, app) cell end-to-end and return the results dict.

    Steps:
      1. Compose a one-line sweep-config: ``<id>|<sweep-args>|<kwargs>|
         <app_dir>|<testname>`` and write it under the cell's output dir.
      2. Shell out to ``ASPLOS_EXP/run_roundtrip_sweep.sh``.
      3. Parse ``summary.txt`` for PASS/FAIL and walk the per-tile
         ``outputs/util.txt`` files for utilization.
      4. Look up PPA for this design in the extractor DataFrame
         (``THESIS_BUILDS``). Missing builds root → PPA fields null.

    ``dry_run=True`` composes and writes the config file but does not
    dispatch — useful for verifying the plumbing without a Halide checkout.
    """
    cell_root = RESULTS_ROOT / design.id / app.id
    cell_root.mkdir(parents=True, exist_ok=True)
    run_dir = cell_root / "run"
    run_dir.mkdir(exist_ok=True)

    sweep_line = compose_sweep_line(design, app, aha_root)
    cfg_path = cell_root / "sweep.cfg"
    cfg_path.write_text(sweep_line + "\n")

    base = {
        "design_id": design.id,
        "app_id": app.id,
        "sweep_config": sweep_line,
    }

    if dry_run:
        return {**base, "sim_status": "SKIP", "note": "dry run — no dispatch"}

    sweep_script = Path(sweep_script) if sweep_script else DEFAULT_SWEEP_SCRIPT
    if not sweep_script.is_file():
        return {**base, "sim_status": "FAIL",
                "error": f"sweep script not found: {sweep_script}"}

    stdout_path = cell_root / "roundtrip.log"
    try:
        with stdout_path.open("w") as fh:
            proc = subprocess.run(
                ["bash", str(sweep_script), str(cfg_path), str(run_dir)],
                stdout=fh, stderr=subprocess.STDOUT,
                timeout=timeout_s, check=False,
            )
        rc = proc.returncode
    except subprocess.TimeoutExpired:
        rc = 124
        with stdout_path.open("a") as fh:
            fh.write(f"\n[run_matrix] TIMEOUT after {timeout_s}s\n")

    sim_status = _parse_sim_status(run_dir / "summary.txt", design.id)
    util = _aggregate_util(run_dir / design.id)
    builds = Path(builds_root) if builds_root else DEFAULT_BUILDS_ROOT
    ppa = _lookup_ppa(design, builds)

    try:
        log_ref = str(stdout_path.relative_to(REPO_ROOT))
    except ValueError:
        log_ref = str(stdout_path)  # results dir outside repo — keep absolute

    return {
        **base,
        "sim_status": sim_status,
        "roundtrip_returncode": rc,
        "roundtrip_log": log_ref,
        **util,
        **ppa,
    }


def write_result(design: DesignPoint, app: AppSpec, result: dict) -> Path:
    outdir = RESULTS_ROOT / design.id / app.id
    outdir.mkdir(parents=True, exist_ok=True)
    outpath = outdir / "results.json"
    outpath.write_text(json.dumps(result, indent=2))
    return outpath


# ---------- CLI -------------------------------------------------------------


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--apps", nargs="+", default=["all"],
                    help="App IDs (see registry.APPS) or 'all'.")
    ap.add_argument("--designs", nargs="+", default=["all"],
                    help="Design IDs (see design_points.DESIGNS_SINGLE_LEVEL) or 'all'.")
    ap.add_argument("--dry-run", action="store_true",
                    help="Compose config files but skip mflowgen dispatch.")
    ap.add_argument("--aha-root", default=DEFAULT_AHA_ROOT,
                    help=f"Root of the AHA checkout (default: {DEFAULT_AHA_ROOT}).")
    ap.add_argument("--builds-root", default=None,
                    help="THESIS_BUILDS dir for PPA lookup "
                         f"(default: $THESIS_BUILDS or {DEFAULT_BUILDS_ROOT}).")
    ap.add_argument("--sweep-script", default=None,
                    help=f"Round-trip shell script (default: {DEFAULT_SWEEP_SCRIPT}).")
    ap.add_argument("--timeout-s", type=int, default=1800,
                    help="Per-cell wall-clock timeout for the round-trip flow.")
    ap.add_argument("--skip-unverified-check", action="store_true",
                    help="Skip the '??' guard on registry.APPS (for testing only).")
    args = ap.parse_args(argv)

    if not args.skip_unverified_check and unverified():
        print("error: some AppSpec fields still contain '??' — pin them down before running:",
              file=sys.stderr)
        for a in unverified():
            print(f"  - {a.id}: {a.halide_app_dir}", file=sys.stderr)
        return 2

    if not DESIGNS_SINGLE_LEVEL:
        print("error: DESIGNS_SINGLE_LEVEL is empty — populate design_points.py first",
              file=sys.stderr)
        return 2

    apps = APPS if args.apps == ["all"] else [a for a in APPS if a.id in set(args.apps)]
    designs = (DESIGNS_SINGLE_LEVEL if args.designs == ["all"]
               else [d for d in DESIGNS_SINGLE_LEVEL if d.id in set(args.designs)])

    if not apps:
        print(f"error: no apps matched {args.apps}", file=sys.stderr)
        return 2
    if not designs:
        print(f"error: no designs matched {args.designs}", file=sys.stderr)
        return 2

    n_fail = 0
    for design in designs:
        for app in apps:
            print(f"→ {design.id} × {app.id}", file=sys.stderr)
            result = run_one_cell(
                design, app,
                dry_run=args.dry_run,
                aha_root=args.aha_root,
                builds_root=args.builds_root,
                sweep_script=args.sweep_script,
                timeout_s=args.timeout_s,
            )
            outpath = write_result(design, app, result)
            status = result.get("sim_status", "?")
            print(f"  wrote {outpath} [{status}]", file=sys.stderr)
            if status not in ("PASS", "SKIP"):
                n_fail += 1

    return 0 if n_fail == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
