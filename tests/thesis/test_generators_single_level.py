"""Unit tests for the 5 Ch. 5 exploration generators in
THESIS/pipeline/generators.py that read THESIS/data/apps/**/results.json."""

from __future__ import annotations

import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")  # headless — CI-safe

import pandas as pd
import pytest

from THESIS.pipeline import apps_query, generators
from THESIS.pipeline.errors import MissingDataError


ALL_GENERATORS = [
    "single_level_power",
    "single_level_performance",
    "single_level_area",
    "single_level_utilization",
    "single_level_energy_efficiency",
]


@pytest.fixture
def empty_ctx():
    return generators.GenContext(df=pd.DataFrame(), top_builds=Path("/tmp"))


@pytest.fixture
def apps_root(tmp_path, monkeypatch):
    """Point apps_query.DEFAULT_RESULTS_ROOT at a tmp dir for the test."""
    root = tmp_path / "apps"
    root.mkdir()
    monkeypatch.setattr(apps_query, "DEFAULT_RESULTS_ROOT", root)
    return root


def _write_pass_cell(root: Path, design_id: str, app_id: str, **fields) -> None:
    d = root / design_id / app_id
    d.mkdir(parents=True, exist_ok=True)
    row = {"design_id": design_id, "app_id": app_id, "sim_status": "PASS", **fields}
    (d / "results.json").write_text(json.dumps(row))


def _seed_full_matrix(root: Path) -> None:
    """3 apps x 2 designs, all fields populated (power+cycles+area+util+clock)."""
    designs = [
        ("port_baseline", {"synth_total_area_um2": 14000.0,
                           "synth_logic_area_um2":  6500.0,
                           "synth_storage_area_um2": 7500.0,
                           "synth_power_w":          0.012,
                           "clock_period_ps":        1428.0}),
        ("mem_small_sp",  {"synth_total_area_um2":  9000.0,
                           "synth_logic_area_um2":  6100.0,
                           "synth_storage_area_um2":2900.0,
                           "synth_power_w":          0.008,
                           "clock_period_ps":        1428.0}),
    ]
    apps = [("matmul_agg", 0.70, 500), ("gaussian_agg", 0.42, 800),
            ("resnet_agg", 0.55, 1200)]
    for did, ppa in designs:
        for aid, util, cyc in apps:
            _write_pass_cell(root, did, aid,
                             active_cycles=int(util * cyc), total_cycles=cyc,
                             utilization=util, tile_count=2, **ppa)


# ---------- MissingDataError on empty tree ---------------------------------


@pytest.mark.parametrize("name", ALL_GENERATORS)
def test_empty_tree_raises_missing_data_error(apps_root, empty_ctx, name, tmp_path):
    with pytest.raises(MissingDataError, match="THESIS/data/apps"):
        getattr(generators, name)(empty_ctx, tmp_path / "x.pdf")


# ---------- All 5 land a PDF on a fully-populated tree ---------------------


@pytest.mark.parametrize("name", ALL_GENERATORS)
def test_populated_tree_produces_pdf(apps_root, empty_ctx, name, tmp_path):
    _seed_full_matrix(apps_root)
    out = tmp_path / f"{name}.pdf"
    getattr(generators, name)(empty_ctx, out)
    assert out.is_file()
    # PDFs are >>500 bytes; guard against generators writing zero-length files.
    assert out.stat().st_size > 1000


# ---------- Per-generator required-column gating ---------------------------


def test_power_requires_synth_power_w(apps_root, empty_ctx, tmp_path):
    # Cell exists but power is missing.
    _write_pass_cell(apps_root, "d", "a", total_cycles=100, utilization=0.5,
                     synth_total_area_um2=10000.0, clock_period_ps=1428.0)
    with pytest.raises(MissingDataError, match="synth_power_w"):
        generators.single_level_power(empty_ctx, tmp_path / "x.pdf")


def test_performance_requires_total_cycles(apps_root, empty_ctx, tmp_path):
    _write_pass_cell(apps_root, "d", "a", utilization=0.5, synth_power_w=0.01)
    with pytest.raises(MissingDataError, match="total_cycles"):
        generators.single_level_performance(empty_ctx, tmp_path / "x.pdf")


def test_area_requires_synth_total_area(apps_root, empty_ctx, tmp_path):
    _write_pass_cell(apps_root, "d", "a", total_cycles=100)
    with pytest.raises(MissingDataError, match="synth_total_area_um2"):
        generators.single_level_area(empty_ctx, tmp_path / "x.pdf")


def test_utilization_requires_utilization_col(apps_root, empty_ctx, tmp_path):
    _write_pass_cell(apps_root, "d", "a", total_cycles=100, synth_power_w=0.01)
    with pytest.raises(MissingDataError, match="utilization"):
        generators.single_level_utilization(empty_ctx, tmp_path / "x.pdf")


def test_energy_efficiency_gates_on_power(apps_root, empty_ctx, tmp_path):
    # Everything except power.
    _write_pass_cell(apps_root, "d", "a", total_cycles=100, utilization=0.5,
                     clock_period_ps=1428.0)
    with pytest.raises(MissingDataError, match="synth_power_w"):
        generators.single_level_energy_efficiency(empty_ctx, tmp_path / "x.pdf")


def test_energy_efficiency_gates_on_clock_period(apps_root, empty_ctx, tmp_path):
    _write_pass_cell(apps_root, "d", "a", total_cycles=100, utilization=0.5,
                     synth_power_w=0.01)
    with pytest.raises(MissingDataError, match="clock_period_ps"):
        generators.single_level_energy_efficiency(empty_ctx, tmp_path / "x.pdf")


def test_energy_efficiency_gates_on_total_cycles(apps_root, empty_ctx, tmp_path):
    _write_pass_cell(apps_root, "d", "a", utilization=0.5, synth_power_w=0.01,
                     clock_period_ps=1428.0)
    with pytest.raises(MissingDataError, match="total_cycles"):
        generators.single_level_energy_efficiency(empty_ctx, tmp_path / "x.pdf")


# ---------- FAIL rows must not leak into the aggregates -------------------


def test_fail_rows_are_excluded(apps_root, empty_ctx, tmp_path):
    _seed_full_matrix(apps_root)
    # A FAIL cell — utilization number is meaningless and should be dropped.
    d = apps_root / "port_baseline" / "harris_agg"
    d.mkdir(parents=True)
    (d / "results.json").write_text(json.dumps({
        "design_id": "port_baseline", "app_id": "harris_agg",
        "sim_status": "FAIL", "utilization": 0.99, "total_cycles": 1,
        "synth_power_w": 999.0, "clock_period_ps": 1428.0,
    }))
    df = apps_query.load_app_results_df(apps_root)
    assert "harris_agg" not in set(df["app_id"])
    # And a chart still renders fine.
    generators.single_level_utilization(empty_ctx, tmp_path / "x.pdf")


# ---------- _grouped_bar helper edge cases --------------------------------


def test_grouped_bar_raises_when_all_rows_null(tmp_path, empty_ctx):
    """A partially-populated column that's all-NaN after dropna must raise."""
    df = pd.DataFrame([
        {"app_id": "a", "design_id": "d", "x_val": None},
    ])
    with pytest.raises(MissingDataError):
        generators._grouped_bar(
            df, x="app_id", hue="design_id", y="x_val",
            title="t", xlabel="x", ylabel="y", outpath=tmp_path / "x.pdf",
        )
