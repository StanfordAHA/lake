"""LaTeX-table renderers for the thesis pipeline.

Each ``emit_*`` function writes one ``.tex`` snippet (a bare
``\\begin{tabular}`` block, suitable for ``\\input{}``) to the given
path. Anything undetermined at generation time is left as a
``% TODO: <what's missing>`` LaTeX comment so it's obvious in-file.

The "populate now from existing data" tables are here:
  - emit_ul_ppa_summary  — per-DesignPoint PPA rollup
  - emit_ul_design_points — DesignPoint axis enumeration
  - emit_exploration_applications — render AppSpec list
  - emit_ul_perf — per-app memories, output rate and runtime (app results)

The two "structural skeletons with prose TODOs":
  - emit_lake_interfaces — scraped from lake/spec/*.py
  - emit_compiler_info   — scraped from the same + collateral schema
"""

from __future__ import annotations

import re
from pathlib import Path

import pandas as pd

from .errors import MissingDataError

REPO_ROOT = Path(__file__).resolve().parents[2]


# ---- shared helpers --------------------------------------------------------


def _fmt(v) -> str:
    """LaTeX-safe cell formatter. Returns ``--`` for null."""
    if v is None or (isinstance(v, float) and pd.isna(v)):
        return "--"
    if isinstance(v, bool):
        return r"\checkmark" if v else ""
    if isinstance(v, float):
        return f"{v:.3g}"
    if isinstance(v, int):
        return str(v)
    return str(v).replace("_", r"\_").replace("&", r"\&").replace("%", r"\%")


def _find_design_row(df: pd.DataFrame, dp) -> pd.Series | None:
    """Look up the extractor row matching a DesignPoint by parameters."""
    mask = (
        (df["experiment"] == dp.experiment)
        & (df["storage_cap"] == dp.storage_cap_bytes)
        & (df["data_width"] == dp.data_width)
        & (df["fw"] == dp.fetch_width)
    )
    if dp.in_ports > 1 or dp.out_ports > 1:
        mask &= (df["inp"] == dp.in_ports) & (df["outp"] == dp.out_ports)
    else:
        mask &= df["inp"].isna() & df["outp"].isna()
    hits = df[mask]
    if hits.empty:
        return None
    return hits.iloc[0]


# ---- tab:ul_ppa_summary ----------------------------------------------------


def _memtile_power_row(memtile: pd.DataFrame | None, dp, mode: str = "static") -> pd.Series | None:
    """A design point's in-CGRA MemTile row of the tile sweep (``tile_sweep.load()``):
    the thesis tile of its geometry in mode (``garnet_configs.garnet_config``), with
    its synth-level active power (garnet ``--memtile-power``, memtile_power.csv)."""
    from ..apps.garnet_configs import garnet_config

    if memtile is None or "config" not in memtile or "synth_active_total_power" not in memtile:
        return None
    hits = memtile[memtile["config"] == garnet_config(dp, mode, "thesis")]
    hits = hits.dropna(subset=["synth_active_total_power"])
    return None if hits.empty else hits.iloc[0]


def emit_ul_ppa_summary(df: pd.DataFrame, outpath: Path, memtile: pd.DataFrame | None = None) -> None:
    """PPA per design point: area and timing from the standalone builds (df), power
    from the final run's in-CGRA MemTile sweep (memtile, ``tile_sweep.load()`` of
    thesis_dse): the static thesis tile's synth-level active power at its clock."""
    from ..apps.design_points import DESIGNS_SINGLE_LEVEL

    if not DESIGNS_SINGLE_LEVEL:
        raise MissingDataError("DESIGNS_SINGLE_LEVEL is empty — populate it first")

    header = [
        "Design",
        r"Logic area (\si{\micro\meter\squared})",
        r"SRAM area (\si{\micro\meter\squared})",
        r"Total area (\si{\micro\meter\squared})",
        r"$f_{\mathrm{max}}$-slack (\si{\pico\second})",
        r"Crit-path (\si{\pico\second})",
        r"MemTile power (\si{\milli\watt})",
        r"MemTile clock (\si{\mega\hertz})",
    ]
    lines = [
        r"\begin{tabular}{lrrrrrrr}",
        r"\toprule",
        " & ".join(header) + r" \\",
        r"\midrule",
    ]

    missing_rows: list[str] = []
    missing_power: list[str] = []
    for dp in DESIGNS_SINGLE_LEVEL:
        row = _find_design_row(df, dp)
        if row is None:
            missing_rows.append(dp.id)
            cells = [_fmt(dp.display)] + [_fmt(None)] * 5
        else:
            logic = None
            if pd.notna(row.get("synth_total_area_um2")) and pd.notna(row.get("synth_storage_area_um2")):
                logic = float(row["synth_total_area_um2"]) - float(row["synth_storage_area_um2"])
            cells = [
                _fmt(dp.display),
                _fmt(logic),
                _fmt(row.get("synth_storage_area_um2")),
                _fmt(row.get("synth_total_area_um2")),
                _fmt(row.get("wns_ps")),
                _fmt(row.get("crit_path_delay_ps")),
            ]
        tile = _memtile_power_row(memtile, dp)
        if tile is None:
            missing_power.append(dp.id)
            cells += [_fmt(None)] * 2
        else:
            mhz = tile.get("clock_mhz")
            if pd.isna(mhz) and pd.notna(tile.get("clock_period_ps")):
                mhz = 1e6 / float(tile["clock_period_ps"])
            cells += [_fmt(float(tile["synth_active_total_power"]) * 1000),
                      _fmt(None if pd.isna(mhz) else round(float(mhz)))]
        lines.append(" & ".join(cells) + r" \\")

    lines.extend([r"\bottomrule", r"\end{tabular}"])
    body = "\n".join(lines) + "\n"

    notes = []
    if missing_rows:
        notes.append(f"missing extractor rows for: {', '.join(missing_rows)}")
    if missing_power:
        notes.append(f"TODO: MemTile power blank for {', '.join(missing_power)} -- no thesis_dse "
                     "row (python3 -m THESIS.pipeline.tile_sweep <thesis_dse zip>)")
    if notes:
        body = "% " + " | ".join(notes) + "\n" + body

    outpath.parent.mkdir(parents=True, exist_ok=True)
    outpath.write_text(body)


# ---- tab:ul_design_points --------------------------------------------------


def emit_ul_design_points(df: pd.DataFrame, outpath: Path) -> None:
    """One row per DesignPoint axis (config identity + roundtrip status)."""
    from ..apps.design_points import DESIGNS_SINGLE_LEVEL

    if not DESIGNS_SINGLE_LEVEL:
        raise MissingDataError("DESIGNS_SINGLE_LEVEL is empty — populate it first")

    header = [
        "ID", "Experiment", "fw", "cap (B)", "dw",
        "in-p", "out-p", "DP", "roundtrip",
    ]
    lines = [
        r"\begin{tabular}{lllrrrrcc}",
        r"\toprule",
        " & ".join(header) + r" \\",
        r"\midrule",
    ]
    for dp in DESIGNS_SINGLE_LEVEL:
        cells = [
            _fmt(dp.id),
            _fmt(dp.experiment),
            _fmt(dp.fetch_width),
            _fmt(dp.storage_cap_bytes),
            _fmt(dp.data_width),
            _fmt(dp.in_ports),
            _fmt(dp.out_ports),
            _fmt(dp.dual_port) if dp.dual_port is not None else "--",
            _fmt(dp.roundtrip_validated),
        ]
        lines.append(" & ".join(cells) + r" \\")
    lines.extend([r"\bottomrule", r"\end{tabular}"])

    outpath.parent.mkdir(parents=True, exist_ok=True)
    outpath.write_text("\n".join(lines) + "\n")


# ---- tab:ul_perf ------------------------------------------------------------


def _span(values: pd.Series, fmt) -> str:
    """``a`` when every value is a, else ``lo--hi``; ``--`` when there is none."""
    v = values.dropna()
    if v.empty:
        return "--"
    lo, hi = fmt(v.min()), fmt(v.max())
    return lo if lo == hi else f"{lo}--{hi}"


def emit_ul_perf(df: pd.DataFrame, outpath: Path, mode: str = "static") -> None:
    """One row per app of the mode's app results (``apps_query``): the UL
    memories it places (``tile_count``), output words per cycle and kernel
    cycles, each a range over the design points it ran on. Apps that record
    nothing in the mode (``bundle_blocked``, ``bundle_modes``) are left out."""
    from ..apps import garnet_configs as gc
    from ..apps.design_points import DESIGNS_SINGLE_LEVEL
    from ..apps.registry import APPS

    if df.empty:
        raise MissingDataError(f"no {mode} app results")
    n_configs = len({gc.garnet_config(dp, mode) for dp in DESIGNS_SINGLE_LEVEL} - {None})
    header = ["Application", "UL memories", "Output words per cycle", "Total runtime (cycles)", "Notes"]
    lines = [
        r"\begin{tabular}{lcccl}",
        r"\toprule",
        " & ".join(header) + r" \\",
        r"\midrule",
    ]
    for a in APPS:
        if a.bundle_blocked or (a.bundle_modes and mode not in a.bundle_modes.split(",")):
            continue
        rows = df[df["app_id"] == a.id]
        notes = [f"{rows['config'].nunique() if 'config' in rows else len(rows)} of {n_configs} design points"]
        if a.level == "two":
            notes.append("PE-tile pond")
        arrays = {f"{int(w)}x{int(h)}" for w, h in zip(rows.get("cgra_width", []), rows.get("cgra_height", []))
                  if pd.notna(w) and pd.notna(h) and (w, h) != (16, 16)}
        notes += [f"{x} array" for x in sorted(arrays)]
        none = pd.Series(dtype=float)
        cells = [_fmt(a.display),
                 _span(rows.get("tile_count", none), lambda v: str(int(v))),
                 _span(rows.get("output_per_cycle", none), lambda v: f"{v:.3g}"),
                 _span(rows.get("total_cycles", none), lambda v: f"{int(v):,}"),
                 _fmt("; ".join(notes))]
        lines.append(" & ".join(cells) + r" \\")
    lines.extend([r"\bottomrule", r"\end{tabular}"])

    outpath.parent.mkdir(parents=True, exist_ok=True)
    outpath.write_text("\n".join(lines) + "\n")


# ---- tab:exploration_applications ------------------------------------------


def emit_exploration_applications(outpath: Path) -> None:
    """Render the app registry as a LaTeX table."""
    from ..apps.registry import APPS, unverified

    if not APPS:
        raise MissingDataError("APPS registry is empty")

    header = ["App", "Schedule variant", "Memory access pattern"]
    lines = [
        r"\begin{tabular}{llp{0.55\linewidth}}",
        r"\toprule",
        " & ".join(header) + r" \\",
        r"\midrule",
    ]
    for a in APPS:
        cells = [_fmt(a.display), _fmt(a.schedule_variant), _fmt(a.memory_notes)]
        lines.append(" & ".join(cells) + r" \\")
    lines.extend([r"\bottomrule", r"\end{tabular}"])

    body = "\n".join(lines) + "\n"
    # Warn if any AppSpec still has "??" markers so it's obvious in-file.
    todo = [a.id for a in unverified()]
    if todo:
        body = f"% TODO: verify halide_app_dir/testname for: {', '.join(todo)}\n" + body

    outpath.parent.mkdir(parents=True, exist_ok=True)
    outpath.write_text(body)


# ---- tab:lake_interfaces (structural skeleton) -----------------------------


_LAKE_SPEC_MODULES = [
    ("Port",            "lake/spec/port.py",             "Port"),
    ("Storage",         "lake/spec/storage.py",          "Storage"),
    ("MemoryPort",      "lake/spec/memory_port.py",      "MemoryPort"),
    ("IterationDomain", "lake/spec/iteration_domain.py", "IterationDomain"),
    ("AddressGenerator","lake/spec/address_generator.py","AddressGenerator"),
    ("ScheduleGenerator","lake/spec/schedule_generator.py","ScheduleGenerator"),
]

_INIT_SIG_RE = re.compile(r"def __init__\s*\(\s*self,?\s*([^)]*)\)")


def _scrape_init_params(path: Path, class_name: str) -> str | None:
    """Best-effort extract of the ``__init__`` positional signature."""
    if not path.is_file():
        return None
    src = path.read_text()
    class_re = re.compile(rf"^class\s+{class_name}\b", re.MULTILINE)
    m = class_re.search(src)
    if not m:
        return None
    tail = src[m.end():]
    m2 = _INIT_SIG_RE.search(tail)
    if not m2:
        return None
    raw = " ".join(m2.group(1).split())
    return raw or "(no init args)"


def emit_lake_interfaces(outpath: Path) -> None:
    """Component × constructor-signature table (prose TODO)."""
    header = ["Component", "Module", "Constructor signature", "Description"]
    lines = [
        r"\begin{tabular}{llp{0.35\linewidth}p{0.25\linewidth}}",
        r"\toprule",
        " & ".join(header) + r" \\",
        r"\midrule",
    ]
    missing: list[str] = []
    for display, rel_path, cls in _LAKE_SPEC_MODULES:
        sig = _scrape_init_params(REPO_ROOT / rel_path, cls)
        if sig is None:
            missing.append(f"{display} ({cls} in {rel_path})")
            sig = "(class not found)"
        cells = [
            _fmt(display),
            _fmt(rel_path),
            _fmt(sig),
            r"% TODO: prose description",
        ]
        lines.append(" & ".join(cells) + r" \\")
    lines.extend([r"\bottomrule", r"\end{tabular}"])
    body = "\n".join(lines) + "\n"
    body = (
        "% skeleton — signatures auto-scraped from lake/spec/*.py; prose "
        "descriptions and any missing-class rows are hand-authored.\n"
        + body
    )
    if missing:
        body = "% missing classes (verify path/name): " + "; ".join(missing) + "\n" + body

    outpath.parent.mkdir(parents=True, exist_ok=True)
    outpath.write_text(body)


# ---- tab:compiler_info (structural skeleton) -------------------------------


def emit_compiler_info(outpath: Path) -> None:
    """Compiler-facing metadata per Component (prose TODO).

    Structure only — the *what does the compiler need / produce* prose
    is thesis-voice work. We list the Components and mark placeholder
    columns so the tex can be edited in-place.
    """
    header = ["Component", "Metadata the compiler needs", "Metadata the compiler emits"]
    lines = [
        r"\begin{tabular}{lp{0.4\linewidth}p{0.4\linewidth}}",
        r"\toprule",
        " & ".join(header) + r" \\",
        r"\midrule",
    ]
    for display, _rel_path, _cls in _LAKE_SPEC_MODULES:
        cells = [
            _fmt(display),
            r"% TODO: compiler input (e.g. loop bounds, port count)",
            r"% TODO: compiler output (e.g. bitstream fields, port config)",
        ]
        lines.append(" & ".join(cells) + r" \\")
    lines.extend([r"\bottomrule", r"\end{tabular}"])
    body = (
        "% skeleton — Component list scraped from lake/spec/*.py; the "
        "per-column prose is hand-authored (see lake_collateral.json "
        "schema and lake/utils/clockwork_roundtrip.py::write_roundtrip_artifacts "
        "for reference).\n"
        + "\n".join(lines) + "\n"
    )

    outpath.parent.mkdir(parents=True, exist_ok=True)
    outpath.write_text(body)
