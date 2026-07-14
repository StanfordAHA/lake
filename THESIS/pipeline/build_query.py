"""API for querying the mflowgen ``THESIS_BUILDS`` tree.

This is the shim between the thesis pipeline and the existing
``ASPLOS_EXP/extract_power_area.py`` extractor. Generators call
``load_builds_df(top)`` to get a pandas DataFrame indexed by
``(experiment, sweep_group, sweep_name)``, then slice/filter it into
whatever the figure needs.

The extractor is imported directly (not shelled out) so we don't fork a
python subprocess per generator; the CSV is cached under
``THESIS/data/_cache/builds.csv`` and re-used across generators in the
same run.
"""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import pandas as pd

REPO_ROOT = Path(__file__).resolve().parents[2]
EXTRACT_PATH = REPO_ROOT / "ASPLOS_EXP" / "extract_power_area.py"
CACHE_DIR = REPO_ROOT / "THESIS" / "data" / "_cache"


def _load_extractor():
    spec = importlib.util.spec_from_file_location("extract_power_area", EXTRACT_PATH)
    mod = importlib.util.module_from_spec(spec)
    sys.modules["extract_power_area"] = mod
    spec.loader.exec_module(mod)  # type: ignore[union-attr]
    return mod


def load_builds_df(top: Path, refresh: bool = False) -> pd.DataFrame:
    """Collect a DataFrame of every build under ``top``.

    Cached to ``THESIS/data/_cache/builds.csv``. Pass ``refresh=True`` to
    force a re-walk (or call with a different ``top``).
    """
    top = Path(top)
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    cache = CACHE_DIR / f"builds__{top.name}.csv"
    if cache.is_file() and not refresh:
        return pd.read_csv(cache)

    if not top.is_dir():
        raise FileNotFoundError(f"THESIS_BUILDS root not found: {top}")

    ex = _load_extractor()
    rows, fieldnames = ex.collect(top)
    df = pd.DataFrame(rows, columns=fieldnames)
    df.to_csv(cache, index=False)
    return df


def filter_experiment(df: pd.DataFrame, experiment: str) -> pd.DataFrame:
    """Return only rows for one EXPERIMENT dir (case-sensitive match)."""
    return df[df["experiment"] == experiment].copy()
