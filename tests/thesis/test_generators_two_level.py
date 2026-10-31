"""THESIS/pipeline/generators.py two_level_*: each two-level app against its
registry single_level_app on every design point, from the RV app results."""

from __future__ import annotations

import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")  # headless — CI-safe

import pandas as pd  # noqa: E402 (after matplotlib.use)
import pytest  # noqa: E402 (after matplotlib.use)

pytest.importorskip("sklearn")  # THESIS.pipeline.regression (the generators) needs it

from THESIS.pipeline import apps_query, generators  # noqa: E402 (after matplotlib.use)
from THESIS.pipeline.errors import MissingDataError  # noqa: E402 (after matplotlib.use)


ALL_GENERATORS = ["two_level_power", "two_level_performance", "two_level_area",
                  "two_level_utilization", "two_level_energy_efficiency"]
PPA = {"synth_power_w": 0.02, "total_cycles": 9000, "output_words": 2352, "output_per_cycle": 2352 / 9000,
       "utilization": 0.4, "synth_total_area_um2": 2e5, "clock_period_ps": 1754.0}


@pytest.fixture
def ctx():
    return generators.GenContext(df=pd.DataFrame(), top_builds=Path("/tmp"))


@pytest.fixture
def apps_root(tmp_path, monkeypatch):
    root = tmp_path / "apps"
    root.mkdir()
    monkeypatch.setattr(apps_query, "DEFAULT_RESULTS_ROOT", root)
    return root


def _cell(root: Path, design: str, mode: str, app_id: str, **fields) -> None:
    d = root / f"{design}__{mode}__thesis" / app_id
    d.mkdir(parents=True, exist_ok=True)
    row = {"design_id": design, "app_id": app_id, "sim_status": "PASS", "mode": mode,
           "fabric": "thesis", **fields}
    (d / "results.json").write_text(json.dumps(row))


def _seed(root: Path) -> None:
    # resnet maps single level on the 2-input point only; the pond matmul has no
    # single-level partner anywhere
    _cell(root, "port_fw4_dw16_sc8k_sp2x2", "rv", "resnet_agg", **PPA)
    _cell(root, "port_fw4_dw16_sc8k_sp2x2", "rv", "resnet_pond_agg", level="two", **PPA)
    _cell(root, "mem_fw1_dw16_sc4k_dp1x1", "rv", "resnet_pond_agg", level="two", **PPA)
    _cell(root, "mem_fw1_dw16_sc4k_dp1x1", "rv", "matmul_pond_agg", level="two", **PPA)
    _cell(root, "mem_fw1_dw16_sc4k_dp1x1", "rv", "gaussian_agg", **PPA)  # not a pair
    _cell(root, "mem_fw1_dw16_sc4k_dp1x1", "static", "resnet_agg", **PPA)  # static


@pytest.mark.parametrize("name", ALL_GENERATORS)
def test_empty_tree_raises(apps_root, ctx, name, tmp_path):
    with pytest.raises(MissingDataError, match="THESIS/data/apps"):
        getattr(generators, name)(ctx, tmp_path / "x.pdf")


@pytest.mark.parametrize("name", ALL_GENERATORS)
def test_pairs_produce_a_pdf(apps_root, ctx, name, tmp_path):
    _seed(apps_root)
    out = tmp_path / f"{name}.pdf"
    getattr(generators, name)(ctx, out)
    assert out.is_file() and out.stat().st_size > 1000


def test_pairs_are_rv_rows_of_registry_pairs(apps_root, ctx, tmp_path, monkeypatch):
    _seed(apps_root)
    seen = {}
    monkeypatch.setattr(generators, "_paired_bar", lambda df, **kw: seen.update(df=df))
    generators.two_level_performance(ctx, tmp_path / "x.pdf")
    got = sorted(zip(seen["df"]["pair"], seen["df"]["level"], seen["df"]["design_id"]))
    assert got == [("Matrix Multiply", "two", "mem_fw1_dw16_sc4k_dp1x1"),
                   ("ResNet block", "single", "port_fw4_dw16_sc8k_sp2x2"),
                   ("ResNet block", "two", "mem_fw1_dw16_sc4k_dp1x1"),
                   ("ResNet block", "two", "port_fw4_dw16_sc8k_sp2x2")]


def test_static_only_pairs_raise(apps_root, ctx, tmp_path):
    _cell(apps_root, "d", "static", "resnet_agg", **PPA)
    _cell(apps_root, "d", "static", "matmul_pond_agg", level="two", **PPA)
    with pytest.raises(MissingDataError, match="rv"):
        generators.two_level_power(ctx, tmp_path / "x.pdf")


def test_energy_needs_the_clock_and_output_words(apps_root, ctx, tmp_path):
    _cell(apps_root, "d", "rv", "resnet_pond_agg", level="two", synth_power_w=0.02, total_cycles=10)
    with pytest.raises(MissingDataError, match="clock_period_ps"):
        generators.two_level_energy_efficiency(ctx, tmp_path / "x.pdf")
    _cell(apps_root, "d", "rv", "resnet_pond_agg", level="two", synth_power_w=0.02, total_cycles=10,
          clock_period_ps=1754.0)
    with pytest.raises(MissingDataError, match="output_words"):
        generators.two_level_energy_efficiency(ctx, tmp_path / "x.pdf")


def test_performance_and_energy_are_per_output_word(apps_root, ctx, tmp_path, monkeypatch):
    # a tile=14 kernel against the whole 28x28 image: 4x the words in ~15x the cycles
    _cell(apps_root, "d", "rv", "resnet_agg", synth_power_w=0.02, total_cycles=11661,
          clock_period_ps=1000.0, output_words=588, output_per_cycle=588 / 11661)
    _cell(apps_root, "d", "rv", "resnet_pond_agg", level="two", synth_power_w=0.01,
          total_cycles=177697, clock_period_ps=1000.0, output_words=2352, output_per_cycle=2352 / 177697)
    seen = {}
    monkeypatch.setattr(generators, "_paired_bar", lambda df, y, **kw: seen.update({y: df.set_index("level")[y]}))
    generators.two_level_performance(ctx, tmp_path / "x.pdf")
    generators.two_level_energy_efficiency(ctx, tmp_path / "x.pdf")
    assert seen["output_per_cycle"]["two"] == pytest.approx(2352 / 177697)
    assert seen["energy_pj"]["single"] == pytest.approx(0.02 * 11661 * 1000.0 / 588)
    assert seen["energy_pj"]["two"] == pytest.approx(0.01 * 177697 * 1000.0 / 2352)
