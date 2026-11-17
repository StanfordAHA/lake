"""THESIS/pipeline/tile_sweep.py load(): a thesis-fabric sweep (garnet --recipe
thesis_dse) under the stock config names the tile figures look up."""

from __future__ import annotations

import pandas as pd
import pytest

from THESIS.pipeline import tile_sweep


@pytest.mark.parametrize("name,stock", [
    ("fw4_dw16_sc8192_sp_in2_out2_vc2_sharectr_portcg_thesis", "fw4_dw16_sc8192_sp_in2_out2_vc2"),
    ("fw4_dw16_sc8192_sp_in2_out2_vc2_nofp_portcg_thesis_rv", "fw4_dw16_sc8192_sp_in2_out2_vc2_rv"),
    ("fw1_dw16_sc8192_sp_in2_out2_dim4_msw1024_nofp_portcg_thesis_rv", "fw1_dw16_sc8192_sp_in2_out2_dim4_msw1024_rv"),
    ("fw4_dw16_sc8192_sp_in2_out2_vc2_rv", "fw4_dw16_sc8192_sp_in2_out2_vc2_rv"),  # stock: unchanged
    ("fw4_dw16_sc8192_sp_in2_out2_vc2_portcg_rv", "fw4_dw16_sc8192_sp_in2_out2_vc2_portcg_rv"),
])
def test_stock_name(name, stock):
    assert tile_sweep.stock_name(name) == stock


def _csv(tmp_path, names):
    path = tmp_path / "sweep.csv"
    pd.DataFrame([{"config": n, "status": "PASS", "syn_cell": 1.0, "base_config": n} for n in names]
                 ).to_csv(path, index=False)
    return path


def test_load_renames_thesis_configs_and_pairs_static_with_rv(tmp_path):
    df = tile_sweep.load(_csv(tmp_path, ["fw1_dw16_sc4096_sp_in1_out1_sharectr_portcg_thesis",
                                         "fw1_dw16_sc4096_sp_in1_out1_nofp_portcg_thesis_rv"]))
    assert list(df["config"]) == ["fw1_dw16_sc4096_sp_in1_out1", "fw1_dw16_sc4096_sp_in1_out1_rv"]
    assert list(df["base_config"]) == ["fw1_dw16_sc4096_sp_in1_out1"] * 2
    assert list(df["garnet_config"]) == ["fw1_dw16_sc4096_sp_in1_out1_sharectr_portcg_thesis",
                                         "fw1_dw16_sc4096_sp_in1_out1_nofp_portcg_thesis_rv"]


def test_load_keeps_a_stock_sweep(tmp_path):
    df = tile_sweep.load(_csv(tmp_path, ["fw2_dw16_sc2048_sp_in1_out1_vc2", "fw2_dw16_sc2048_sp_in1_out1_vc2_rv"]))
    assert list(df["config"]) == list(df["garnet_config"])
    assert list(df["base_config"]) == ["fw2_dw16_sc2048_sp_in1_out1_vc2"] * 2


def test_load_refuses_a_config_under_both_names(tmp_path):
    with pytest.raises(ValueError, match="fw2_dw16_sc2048_sp_in1_out1_vc2"):
        tile_sweep.load(_csv(tmp_path, ["fw2_dw16_sc2048_sp_in1_out1_vc2",
                                        "fw2_dw16_sc2048_sp_in1_out1_vc2_sharectr_portcg_thesis"]))
