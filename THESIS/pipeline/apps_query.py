"""Load the app-mapping harness output (``THESIS/data/apps/**/results.json``)
into a tidy DataFrame the exploration-figure generators can group over.

Sibling of ``build_query.py``. Unlike that module, there's no cache — the
tree is tiny (one small JSON per cell) so re-walking it every generator
run is essentially free.

Schema written by ``THESIS/apps/run_matrix.py`` (per-cell results.json):
    design_id, app_id, sim_status, active_cycles, total_cycles,
    utilization, per_tile_util, tile_count, synth_total_area_um2,
    synth_logic_area_um2, synth_storage_area_um2, synth_power_w,
    clock_period_ps, crit_path_delay_ps, ...

Only rows with ``sim_status == "PASS"`` are returned by default — a
failed sim's utilization/perf numbers aren't meaningful.

``THESIS/apps/cgra_power_ingest.py`` writes the same schema from garnet's
whole-CGRA app power, plus ``mode`` (static | rv), ``fabric`` (thesis |
stock) and ``pond_id``, one cell directory per combination. ``select`` /
the keyword filters of ``load_app_results_df`` pick one combination, so
static and RV rows of a design are never averaged together.
"""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_RESULTS_ROOT = REPO_ROOT / "THESIS" / "data" / "apps"
# What a results.json without the field means (run_matrix's cells: static,
# single level). A field with no default here (fabric) matches any value when
# a row lacks it.
FIELD_DEFAULTS = {"mode": "static", "pond_id": "default"}
SINGLE_LEVEL_PONDS = ("default", "builtin")  # cgra_power_ingest.pond_id of an unchanged pond


def select(df: pd.DataFrame, **want) -> pd.DataFrame:
    """Rows whose field equals ``want[field]`` (a tuple: is one of); value None =
    any. A missing field reads as FIELD_DEFAULTS, or matches if it has none."""
    for field, value in want.items():
        if value is None or df.empty:
            continue
        values = value if isinstance(value, tuple) else (value,)
        col = df[field] if field in df.columns else pd.Series(None, index=df.index, dtype=object)
        if field in FIELD_DEFAULTS:
            col = col.where(col.notna(), FIELD_DEFAULTS[field])
            df = df[col.isin(values)]
        else:
            df = df[col.isna() | col.isin(values)]
    return df.copy()


def load_app_results_df(
    results_root: Path | str | None = None,
    *,
    pass_only: bool = True,
    **want,
) -> pd.DataFrame:
    """Walk ``<root>/<cell>/<app_id>/results.json`` and return a DataFrame,
    filtered by ``select(df, **want)`` (e.g. ``mode="rv"``).

    Returns an empty DataFrame if the tree doesn't exist yet — generators
    should treat that as ``MissingDataError`` upstream.
    """
    root = Path(results_root) if results_root else DEFAULT_RESULTS_ROOT
    if not root.is_dir():
        return pd.DataFrame()

    rows: list[dict] = []
    for result_path in sorted(root.glob("*/*/results.json")):
        try:
            data = json.loads(result_path.read_text())
        except (OSError, json.JSONDecodeError):
            continue
        # Flatten only scalar fields — per_tile_util is a list and would
        # explode the frame. Callers who need it can read the JSON directly.
        row = {k: v for k, v in data.items() if not isinstance(v, (list, dict))}
        row["_source"] = str(result_path.relative_to(root))
        rows.append(row)

    df = pd.DataFrame(rows)
    if pass_only and not df.empty and "sim_status" in df.columns:
        df = df[df["sim_status"] == "PASS"].copy()
    return select(df, **want)
