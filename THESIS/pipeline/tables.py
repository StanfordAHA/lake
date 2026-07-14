"""LaTeX-table renderers for the thesis pipeline.

Each ``emit_*`` function writes one ``.tex`` snippet (a bare
``\\begin{tabular}`` block, suitable for ``\\input{}``) to the given
path. Anything undetermined at generation time is left as a
``% TODO: <what's missing>`` LaTeX comment so it's obvious in-file.

The three "populate now from existing data" tables are here:
  - emit_ul_ppa_summary  — per-DesignPoint PPA rollup
  - emit_ul_design_points — DesignPoint axis enumeration
  - emit_exploration_applications — render AppSpec list

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


def emit_ul_ppa_summary(df: pd.DataFrame, outpath: Path) -> None:
    """PPA per design point (area + timing today; power drops in later)."""
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
        r"Synth power (\si{\milli\watt})",
    ]
    lines = [
        r"\begin{tabular}{lrrrrrr}",
        r"\toprule",
        " & ".join(header) + r" \\",
        r"\midrule",
    ]

    missing_rows: list[str] = []
    missing_power = False
    for dp in DESIGNS_SINGLE_LEVEL:
        row = _find_design_row(df, dp)
        if row is None:
            missing_rows.append(dp.id)
            cells = [_fmt(dp.display)] + [_fmt(None)] * 6
        else:
            logic = None
            if pd.notna(row.get("synth_total_area_um2")) and pd.notna(row.get("synth_storage_area_um2")):
                logic = float(row["synth_total_area_um2"]) - float(row["synth_storage_area_um2"])
            sram = row.get("synth_storage_area_um2")
            total = row.get("synth_total_area_um2")
            wns = row.get("wns_ps")
            crit = row.get("crit_path_delay_ps")
            pwr_w = row.get("synth_power_w")
            if pd.isna(pwr_w):
                missing_power = True
            pwr_mw = float(pwr_w) * 1000 if pd.notna(pwr_w) else None
            cells = [
                _fmt(dp.display),
                _fmt(logic),
                _fmt(sram),
                _fmt(total),
                _fmt(wns),
                _fmt(crit),
                _fmt(pwr_mw),
            ]
        lines.append(" & ".join(cells) + r" \\")

    lines.extend([r"\bottomrule", r"\end{tabular}"])
    body = "\n".join(lines) + "\n"

    notes = []
    if missing_rows:
        notes.append(f"missing extractor rows for: {', '.join(missing_rows)}")
    if missing_power:
        notes.append("TODO: synth power blank until ptpx-synth flow runs")
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


# ---- tab:exploration_applications ------------------------------------------


def emit_exploration_applications(outpath: Path) -> None:
    """Render the app registry as a LaTeX table."""
    from ..apps.registry import APPS

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
    unverified = [a.id for a in APPS if "??" in " ".join(a.__dict__.values())]
    if unverified:
        body = f"% TODO: verify halide_app_dir/testname for: {', '.join(unverified)}\n" + body

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
