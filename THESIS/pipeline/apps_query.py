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
"""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_RESULTS_ROOT = REPO_ROOT / "THESIS" / "data" / "apps"


def load_app_results_df(
    results_root: Path | str | None = None,
    *,
    pass_only: bool = True,
) -> pd.DataFrame:
    """Walk ``<root>/<design_id>/<app_id>/results.json`` and return a DataFrame.

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
    return df
