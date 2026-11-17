"""THESIS/pipeline/filter_path_ab.py: the standalone filter-path A/B figure (not in the
thesis), on made-up sweep and app power tables."""

from __future__ import annotations

import zipfile
from pathlib import Path

import matplotlib
matplotlib.use("Agg")  # headless — CI-safe

import pandas as pd  # noqa: E402 (after matplotlib.use)
import pytest  # noqa: E402 (after matplotlib.use)

from THESIS.pipeline import filter_path_ab as fp  # noqa: E402 (after matplotlib.use)
from THESIS.pipeline.errors import MissingDataError  # noqa: E402 (after matplotlib.use)

G_SMALL = "fw2_dw16_sc2048_sp_in1_out1_vc2"
G_BIG = "fw4_dw32_sc8192_sp_in2_out2_vc2"


def _tiles() -> tuple[pd.DataFrame, pd.DataFrame]:
    rows = [(G_BIG, "ff8", 110.0, 0.0012, 0.010), (G_BIG, "nofp", 100.0, 0.0010, 0.010),
            (G_SMALL, "ff8", 60.0, 0.0006, 0.004), (G_SMALL, "nofp", 50.0, 0.0005, 0.005)]
    names = [f"{g}_{v}_portcg_thesis_rv" for g, v, *_ in rows]
    corr = pd.DataFrame({"config_name": names, "clock_mhz": 700, "tile_pnr_total_area": [r[2] for r in rows]})
    power = pd.DataFrame({"config_name": names, "clock_mhz": 700, "pnr_idle_total_power": [r[3] for r in rows],
                          "pnr_active_total_power": [r[4] for r in rows]})
    return corr, power


def _app() -> pd.DataFrame:
    cfg = "fw4_dw16_sc4096_sp_in2_out2_vc2_{}_portcg_thesis_rv"
    rows = []
    for variant, kind, scale in (("conv_A", "ff8", 1.05), ("conv_B", "ff8", 1.25), ("conv_A0", "nofp", 1.0)):
        rows.append({"config": cfg.format(kind), "variant": variant, "app_W": 0.024 * scale,
                     "Tile_MemCore_placed_W": 0.006 * scale, "Tile_MemCore_route_W": 0.002 * scale,
                     "Tile_PE_placed_W": 0.013 * scale, "Tile_PE_route_W": 0.003 * scale,
                     "kernel_cycles": 4150, "d_app_W_pct": 0.0})
    rows.append({"config": "fw2_dw16_sc2048_sp_in1_out1_vc2_ff8_portcg_thesis_rv", "variant": "conv_A",
                 "app_W": 0.01, "kernel_cycles": 1})  # no A0 / B there: left out
    return pd.DataFrame(rows)


def test_tile_deltas_are_ff8_over_nofp_in_width_order():
    d = fp.tile_deltas(*_tiles())
    assert list(d.index) == [f"{G_SMALL}_thesis_rv", f"{G_BIG}_thesis_rv"]  # dw16 before dw32
    assert d.loc[f"{G_SMALL}_thesis_rv"].tolist() == pytest.approx([20.0, 20.0, -20.0])
    assert d.loc[f"{G_BIG}_thesis_rv"].tolist() == pytest.approx([10.0, 20.0, 0.0])


def test_tile_deltas_need_both_builds():
    corr, power = _tiles()
    keep = ~corr["config_name"].str.contains("nofp")
    with pytest.raises(MissingDataError, match="both"):
        fp.tile_deltas(corr[keep], power[keep])


def test_app_shares_are_percent_of_a0_on_the_same_geometry():
    shares = fp.app_shares(_app())
    assert list(shares) == ["fw4_dw16_sc4096_sp_in2_out2_vc2_thesis_rv"]
    rows = shares["fw4_dw16_sc4096_sp_in2_out2_vc2_thesis_rv"]
    assert list(rows.index) == ["A0", "A", "B"]
    assert rows["total"].tolist() == pytest.approx([100.0, 105.0, 125.0])
    assert rows.loc["A0", ["MEM placed", "MEM route", "PE placed", "PE route"]].sum() == pytest.approx(100.0)


def test_geometry_label():
    assert fp.geometry("fw1_dw16_sc4096_dp_in1_out1_ff8_portcg_thesis_rv") == "fw1 dw16\n4KB DP\n1x1"


def test_main_reads_zips_and_dirs_and_draws(tmp_path):
    corr, power = _tiles()
    tiles = tmp_path / "thesis_filter_ab.zip"
    with zipfile.ZipFile(tiles, "w") as z:
        z.writestr("thesis_filter_ab/correlation.csv", corr.to_csv(index=False))
        z.writestr("thesis_filter_ab/memtile_power.csv", power.to_csv(index=False))
        z.writestr("thesis_filter_ab/some_config/correlation.csv", "config_name\n")  # deeper: ignored
    app = tmp_path / "fp_ab" / "cgra_app_power_thesis_fp_ab"
    app.mkdir(parents=True)
    _app().to_csv(app / "cgra_power.csv", index=False)
    out = tmp_path / "x.pdf"
    assert fp.main(["--tiles", str(tiles), "--app", str(app.parent), "-o", str(out)]) == 0
    assert out.stat().st_size > 1000


def test_missing_zip_says_where_to_put_it(tmp_path):
    with pytest.raises(MissingDataError, match="not found"):
        fp.read_csv(tmp_path / "nope.zip", "correlation.csv")
