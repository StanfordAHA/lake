"""Unit tests for the 5 Ch. 5 exploration generators in
THESIS/pipeline/generators.py that read THESIS/data/apps/**/results.json."""

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


ALL_GENERATORS = [
    "single_level_power",
    "single_level_performance",
    "single_level_area",
    "single_level_energy_efficiency",
    "single_level_pareto",
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
                           "synth_logic_area_um2": 6500.0,
                           "synth_storage_area_um2": 7500.0,
                           "synth_power_w": 0.012,
                           "clock_period_ps": 1428.0,
                           "mem_tiles_placed": 2, "mem_tile_area_um2": 3750.0,
                           "pe_tiles_placed": 2, "pe_tile_area_um2": 1000.0}),
        ("mem_small_sp", {"synth_total_area_um2": 9000.0,
                          "synth_logic_area_um2": 6100.0,
                          "synth_storage_area_um2": 2900.0,
                          "synth_power_w": 0.008,
                          "clock_period_ps": 1428.0,
                          "mem_tiles_placed": 2, "mem_tile_area_um2": 1450.0,
                          "pe_tiles_placed": 2, "pe_tile_area_um2": 1000.0}),
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
    # the seed's design ids aren't real design points, so they have no data width
    kw = {"data_width": None} if name == "single_level_pareto" else {}
    getattr(generators, name)(empty_ctx, out, **kw)
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


def test_area_requires_placed_tiles(apps_root, empty_ctx, tmp_path):
    """The area bars count placed tiles only, so synth_total_area_um2 alone isn't enough."""
    _write_pass_cell(apps_root, "d", "a", total_cycles=100, synth_total_area_um2=1e4)
    with pytest.raises(MissingDataError, match="placed tile area"):
        generators.single_level_area(empty_ctx, tmp_path / "x.pdf")


def test_performance_requires_the_clock(apps_root, empty_ctx, tmp_path):
    _write_pass_cell(apps_root, "d", "a", total_cycles=100, synth_power_w=0.01)
    with pytest.raises(MissingDataError, match="clock_period_ps"):
        generators.single_level_performance(empty_ctx, tmp_path / "x.pdf")


def test_performance_is_wall_time_at_each_design_points_clock(apps_root, empty_ctx, tmp_path, monkeypatch):
    _write_pass_cell(apps_root, "port_fw4_dw16_sc8k_sp2x2", "a", total_cycles=1000, clock_period_ps=1666.0)
    _write_pass_cell(apps_root, "mem_fw2_dw16_sc2k_sp1x1", "a", total_cycles=1000, clock_period_ps=2000.0)
    seen = {}
    monkeypatch.setattr(generators, "_app_bar", lambda df, y, **kw: seen.update(
        {y: df.set_index("design_id")[y], "ylabel": kw["ylabel"]}))
    generators.single_level_performance(empty_ctx, tmp_path / "x.pdf")
    assert seen["wall_us"]["port_fw4_dw16_sc8k_sp2x2"] == pytest.approx(1.666)
    assert seen["wall_us"]["mem_fw2_dw16_sc2k_sp1x1"] == pytest.approx(2.0)
    assert seen["ylabel"] == "Wall time (µs)"


def test_bars_show_only_the_app_data_width(monkeypatch):
    """Rows on an 8- or 32-bit design point drop out; 16-bit and unknown ids stay."""
    df = pd.DataFrame({"design_id": ["port_fw4_dw16_sc8k_sp2x2", "port_fw4_dw8_sc8k_sp2x2",
                                     "port_fw2_dw32_sc8k_dp2x2", "mem_fw2_dw16_sc2k_sp1x1", "zz"]})
    assert list(generators._at_width(df)["design_id"]) == [
        "port_fw4_dw16_sc8k_sp2x2", "mem_fw2_dw16_sc2k_sp1x1", "zz"]


def test_a_design_point_keeps_its_color_in_every_figure():
    from THESIS.pipeline.tile_figures import SERIES
    colors = generators._design_colors()
    assert colors["port_fw4_dw16_sc8k_sp2x2"] == SERIES[0]
    assert colors["mem_fw2_dw16_sc2k_sp1x1"] == SERIES[2]  # after the 32-bit dual-port point
    assert "mem_fw4_dw16_sc8k_sp2x2" not in colors  # drawn as the baseline


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
    generators.single_level_power(empty_ctx, tmp_path / "x.pdf")


# ---------- _app_bar helper edge cases ------------------------------------


def test_app_bar_raises_when_all_rows_null(tmp_path, empty_ctx):
    """A partially-populated column that's all-NaN after dropna must raise."""
    df = pd.DataFrame([
        {"app_id": "a", "design_id": "d", "x_val": None},
    ])
    with pytest.raises(MissingDataError):
        generators._app_bar(df, y="x_val", ylabel="y", outpath=tmp_path / "x.pdf")


def test_app_designs_draw_one_bar_per_config():
    """The PORT and MEM fw4 dw16 8KB SP 2x2 points are one config: one bar, the
    first in design_points order; unknown ids follow."""
    ids = {"mem_fw4_dw16_sc8k_sp2x2", "port_fw4_dw16_sc8k_sp2x2", "mem_fw2_dw16_sc2k_sp1x1", "zz"}
    assert generators._app_designs(ids) == [
        ("port_fw4_dw16_sc8k_sp2x2", "fw4 dw16 8KB SP 2x2"),
        ("mem_fw2_dw16_sc2k_sp1x1", "fw2 dw16 2KB SP 1x1"),
        ("zz", "zz"),
    ]
    assert generators._app_designs({"mem_fw4_dw16_sc8k_sp2x2"}) == [
        ("mem_fw4_dw16_sc8k_sp2x2", "fw4 dw16 8KB SP 2x2")]


def test_area_is_relative_to_the_first_design_under_sram_nda(apps_root, empty_ctx, tmp_path, monkeypatch):
    from THESIS.pipeline import tile_figures
    _seed_full_matrix(apps_root)
    calls = []
    monkeypatch.setattr(generators, "_app_bar", lambda df, **kw: calls.append(kw))
    monkeypatch.setattr(tile_figures, "SRAM_NDA", True)
    generators.single_level_area(empty_ctx, tmp_path / "x.pdf")
    monkeypatch.setattr(tile_figures, "SRAM_NDA", False)
    generators.single_level_area(empty_ctx, tmp_path / "x.pdf")
    assert calls[0]["relative"] and calls[0]["ylabel"] == "Area (× mem_small_sp)"
    assert calls[1]["y"] == "area_k" and not calls[1].get("relative")


# ---------- static vs RV (LI) and fabrics stay apart ----------------------


LI_GENERATORS = [f"{n}_LI" for n in ALL_GENERATORS if n != "single_level_pareto"]  # static only


def _write_cell(root: Path, cell: str, app_id: str, **fields) -> None:
    """A cgra_power_ingest-style cell: directory <design>__<mode>__<fabric>."""
    d = root / cell / app_id
    d.mkdir(parents=True, exist_ok=True)
    row = {"design_id": cell.split("__")[0], "app_id": app_id, "sim_status": "PASS", **fields}
    (d / "results.json").write_text(json.dumps(row))


@pytest.mark.parametrize("name", LI_GENERATORS)
def test_li_generators_read_only_rv_rows(apps_root, empty_ctx, name, tmp_path):
    _seed_full_matrix(apps_root)  # static (no mode field)
    with pytest.raises(MissingDataError, match="rv"):
        getattr(generators, name)(empty_ctx, tmp_path / "x.pdf")
    for cell in ("port_baseline__rv__thesis", "mem_small_sp__rv__thesis"):
        _write_cell(apps_root, cell, "gaussian_agg", mode="rv",
                         fabric="thesis", total_cycles=900, utilization=0.4, synth_power_w=0.02,
                         synth_total_area_um2=1e4, synth_logic_area_um2=6e3,
                         synth_storage_area_um2=4e3, clock_period_ps=1754.0,
                         mem_tiles_placed=2, mem_tile_area_um2=3e3,
                         pe_tiles_placed=4, pe_tile_area_um2=1e3)
    out = tmp_path / f"{name}.pdf"
    getattr(generators, name)(empty_ctx, out)
    assert out.is_file() and out.stat().st_size > 1000


def test_static_figures_skip_rv_and_stock_rows(apps_root, empty_ctx, tmp_path, monkeypatch):
    clk = {"clock_period_ps": 1000.0}
    _write_cell(apps_root, "d__static__thesis", "a", mode="static", fabric="thesis", total_cycles=100, **clk)
    _write_cell(apps_root, "d__rv__thesis", "a", mode="rv", fabric="thesis", total_cycles=10_000, **clk)
    _write_cell(apps_root, "d__static__stock", "a", mode="static", fabric="stock", total_cycles=20_000, **clk)
    _write_cell(apps_root, "d__static__thesis", "mm_pond", mode="static", fabric="thesis", level="two",
                total_cycles=30_000, **clk)
    seen = {}
    monkeypatch.setattr(generators, "_app_bar", lambda df, **kw: seen.update(df=df))
    generators.single_level_performance(empty_ctx, tmp_path / "x.pdf")
    assert list(seen["df"]["total_cycles"]) == [100]


# ---------- single_level_pareto -----------------------------------------------


def test_design_ppa_is_the_geomean_over_apps_relative_to_the_baseline():
    """Two apps on the baseline (port_fw4_dw16_sc8k_sp2x2, the first design point) and
    on mem_fw2_dw16_sc2k_sp1x1, one of them also on port_fw4_dw8_sc8k_sp2x2. Runtime
    counts the clock: the dw8 point's 2x clock halves runtime at equal cycles."""
    rows = []
    for d, area, power, period, apps in (
            ("port_fw4_dw16_sc8k_sp2x2", 1.0, 1.0, 1000.0, ("a", "b")),
            ("mem_fw2_dw16_sc2k_sp1x1", 2.0, 4.0, 1000.0, ("a", "b")),
            ("port_fw4_dw8_sc8k_sp2x2", 0.5, 1.0, 500.0, ("a",))):
        for i, a in enumerate(apps):
            scale = 1 + i  # app b is twice app a everywhere: ratios are per app
            # placed area = 2 MEM tiles x 0.25 area + 1 PE tile x 0.5 area = area
            rows.append({"app_id": a, "design_id": d, "synth_total_area_um2": 99.0,
                         "mem_tiles_placed": 2 * scale, "mem_tile_area_um2": area / 4,
                         "pe_tiles_placed": scale, "pe_tile_area_um2": area / 2,
                         "synth_power_w": power * scale, "total_cycles": 100 * scale,
                         "clock_period_ps": period})
    pts = generators._design_ppa(pd.DataFrame(rows))
    got = {d: (n, v) for d, _, n, v in pts}
    assert pts[0][0] == "port_fw4_dw16_sc8k_sp2x2"
    assert got["port_fw4_dw16_sc8k_sp2x2"] == (2, {"area": 1.0, "perf": 1.0, "power": 1.0})
    assert got["mem_fw2_dw16_sc2k_sp1x1"] == (2, pytest.approx({"area": 2.0, "perf": 1.0, "power": 4.0}))
    assert got["port_fw4_dw8_sc8k_sp2x2"] == (1, pytest.approx({"area": 0.5, "perf": 2.0, "power": 1.0}))


def test_placed_area_leaves_out_route_only_tiles():
    """Placed MEM + PE tiles x their tile areas; route-only tiles and
    synth_total_area_um2 (which counts them) don't enter. NaN without tile counts."""
    df = pd.DataFrame([{"mem_tiles_placed": 3, "mem_tiles_route": 10, "mem_tile_area_um2": 100.0,
                        "pe_tiles_placed": 4, "pe_tiles_route": 20, "pe_tile_area_um2": 10.0,
                        "synth_total_area_um2": 1540.0}])
    assert list(generators._placed_area(df)) == [340.0]
    assert generators._placed_area(pd.DataFrame([{"synth_total_area_um2": 1.0}])).isna().all()


def test_pareto_plots_only_one_data_width(apps_root, empty_ctx, tmp_path, monkeypatch):
    """16-bit design points only by default; a width with no results is a MissingDataError."""
    for d in ("port_fw4_dw16_sc8k_sp2x2", "mem_fw2_dw16_sc2k_sp1x1", "port_fw4_dw8_sc8k_sp2x2"):
        _write_pass_cell(apps_root, d, "gaussian_agg", total_cycles=100, synth_power_w=0.01,
                         clock_period_ps=1000.0, mem_tiles_placed=2, mem_tile_area_um2=1e3,
                         pe_tiles_placed=2, pe_tile_area_um2=5e2)
    seen, real = [], generators._design_ppa
    monkeypatch.setattr(generators, "_design_ppa",
                        lambda df: seen.append(set(df["design_id"])) or real(df))
    generators.single_level_pareto(empty_ctx, tmp_path / "a.pdf")
    generators.single_level_pareto(empty_ctx, tmp_path / "b.pdf", data_width=None)
    assert seen == [{"port_fw4_dw16_sc8k_sp2x2", "mem_fw2_dw16_sc2k_sp1x1"},
                    {"port_fw4_dw16_sc8k_sp2x2", "mem_fw2_dw16_sc2k_sp1x1",
                     "port_fw4_dw8_sc8k_sp2x2"}]
    with pytest.raises(MissingDataError, match="64-bit"):
        generators.single_level_pareto(empty_ctx, tmp_path / "c.pdf", data_width=64)


def test_pareto_keeps_points_nothing_beats_on_both_axes():
    pts = [(1.0, 1.0), (2.0, 3.0), (1.5, 0.5), (3.0, 2.0), (0.5, 0.8)]
    assert generators._pareto(pts, higher=True) == [(0.5, 0.8), (1.0, 1.0), (2.0, 3.0)]
    assert generators._pareto(pts, higher=False) == [(0.5, 0.8), (1.5, 0.5)]
