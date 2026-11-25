"""THESIS/pipeline/sb_ppa.py + sb_figures.py: switch-box parts of the thesis PE and
Memory Tiles from made-up Genus hierarchical area reports."""

from __future__ import annotations

import zipfile
from pathlib import Path

import matplotlib
matplotlib.use("Agg")  # headless — CI-safe

import pandas as pd  # noqa: E402 (after matplotlib.use)
import pytest  # noqa: E402 (after matplotlib.use)

from THESIS.pipeline import sb_figures, sb_ppa  # noqa: E402 (after matplotlib.use)
from THESIS.pipeline.errors import MissingDataError  # noqa: E402 (after matplotlib.use)

HEAD = """============================================================
  Module:                 Tile_PE
============================================================

  Instance   Module   Cell-Count  Cell-Area  Net-Area   Total-Area
------------------------------------------------------------------
"""


def _row(depth: int, inst: str, module: str, area: float) -> str:
    return f"{'  ' * depth}{inst}    {module}    10  {area:.3f}  1.000  {area + 1:.3f}\n"


def _report(rv: bool, dw: int, reg: float) -> str:
    """One tile: a B{dw} and a B1 switch box, 2 track registers each, plus tile-level gates."""
    rows = [_row(0, "Tile_PE", "NA", 5000.0)]
    for w, r in ((dw, reg), (1, 1.0 if not rv else 13.0)):
        sb = f"SB_ID0_5TRACKS_B{w}_PE"
        kids = [("REG_T0_EAST_B%d" % w, r), ("REG_T1_WEST_B%d" % w, r), ("MUX_SB_T0_EAST_SB_OUT_B%d" % w, 10.0),
                ("RMUX_T0_EAST_B%d" % w, 4.0), ("config_reg_0", 30.0 if rv else 20.0),
                ("and_inst0", 0.2), ("and_inst1", 0.2)]
        if rv:
            kids += [("SB_T0_EAST_SB_IN_B%d_fan_in" % w, 3.0), ("SB_T0_EAST_SB_OUT_B%d_FANOUT" % w, 0.5),
                     ("and_inst2", 0.2), ("and_inst3", 0.2)]
        total = sum(a for _, a in kids) + 2.0  # + SB-level logic outside its children
        rows.append(_row(1, sb, f"Tile_PE_{sb}", total))
        rows += [_row(2, k, "Tile_PE_m", a) for k, a in kids]
    if rv:
        rows += [_row(1, "and4_inst0", "Tile_PE_coreir_and_width1", 0.2),
                 _row(1, "PE_output_width_16_num_0_loopback", "Tile_PE_ReadyValidLoopBack", 0.7),
                 _row(1, "SB_T0_EAST_SB_OUT_B16_ready_and", "Tile_PE_and_cell", 0.5)]
    rows.append(_row(1, "PE_inst0", "Tile_PE_PE", 1000.0))
    return HEAD + "".join(rows)


def test_tile_parts_bins_the_switch_box_children():
    p = sb_ppa.tile_parts(sb_ppa.genus_tree(_report(True, 16, 50.0)))
    assert p["tile_area"] == 5000.0
    assert p["regs"] == pytest.approx(2 * 50 + 2 * 13)
    assert (p["regs_dw_n"], p["regs_dw_area"], p["regs_1b_n"]) == (2, 100.0, 2)
    assert p["muxes"] == pytest.approx(2 * (10 + 4))
    assert p["config_regs"] == pytest.approx(60.0)
    assert (p["fan_in"], p["fanout"], p["ands_n"]) == (pytest.approx(6.0), pytest.approx(1.0), 8)
    assert p["other"] == pytest.approx(4.0)  # the two SBs' own logic
    assert p["sb_area"] == pytest.approx(sum(p[k] for k in sb_ppa.PARTS))
    assert (p["tile_valid_ands"], p["loopbacks"], p["pd_ready_ands"]) == (0.2, 0.7, 0.5)


def test_flattened_report_raises():
    with pytest.raises(MissingDataError, match="SB_ID0"):
        sb_ppa.tile_parts(sb_ppa.genus_tree(HEAD + _row(0, "Tile_PE", "NA", 1.0)))


def _tree(tmp_path: Path, step: str, name: str) -> Path:
    z = tmp_path / f"{name}.zip"
    with zipfile.ZipFile(z, "w") as f:
        for dw, sc in ((8, 8192), (16, 4096)):
            for suffix, rv in (("sharectr_portcg_thesis", False), ("nofp_portcg_thesis_rv", True)):
                cfg = f"fw4_dw{dw}_sc{sc}_sp_in2_out2_vc2_{suffix}"
                f.writestr(f"{name}/{cfg}/spec_config.json", "{}")
                f.writestr(f"{name}/{cfg}/{step}-cadence-genus-synthesis/results_syn/final_area.rpt",
                           _report(rv, dw, 3.0 * dw if rv else dw))
    return z


@pytest.fixture
def sb_csv(tmp_path, monkeypatch):
    df = sb_ppa.ingest(_tree(tmp_path, "15", "pe"), _tree(tmp_path, "14", "mem"))
    out = tmp_path / "sb_area.csv"
    df.to_csv(out, index=False)
    monkeypatch.setattr(sb_ppa, "OUT", out)
    return df


def test_ingest_both_tiles_and_the_fork_logic(sb_csv):
    df = sb_csv
    assert sorted(set(zip(df["tile"], df["mode"], df["data_width"]))) == sorted(
        (t, m, dw) for t in ("PE", "MEM") for m in ("static", "rv") for dw in (8, 16))
    rv = df[(df["tile"] == "PE") & (df["mode"] == "rv") & (df["data_width"] == 16)].iloc[0]
    st = df[(df["tile"] == "PE") & (df["mode"] == "static") & (df["data_width"] == 16)].iloc[0]
    # static: 2 clock-enable ANDs per SB; RV: 2 more per SB gate valid -> fork logic
    assert st["valid_ands"] == 0 and rv["valid_ands"] == pytest.approx(4 * 0.2)
    assert rv["fork_area"] == pytest.approx(6.0 + 4 * 0.2 + 0.2 + 0.7)
    assert st["fork_area"] == 0
    assert rv["regs_dw_each"] == pytest.approx(48.0) and st["regs_dw_each"] == pytest.approx(16.0)
    # the config-register part doesn't overwrite the config name
    assert rv["config"].endswith("_nofp_portcg_thesis_rv")
    assert rv["config_regs"] == pytest.approx(60.0)


@pytest.mark.parametrize("fig", ["interconnect_fifo_replace_ff", "interconnect_fork_ready_valid_data"])
def test_figures_draw(sb_csv, tmp_path, fig):
    out = tmp_path / f"{fig}.pdf"
    getattr(sb_figures, fig)(None, out)
    assert out.stat().st_size > 1000


def test_figures_need_the_ingest(tmp_path, monkeypatch):
    monkeypatch.setattr(sb_ppa, "OUT", tmp_path / "none.csv")
    with pytest.raises(MissingDataError, match="sb_ppa"):
        sb_figures.interconnect_fifo_replace_ff(None, tmp_path / "x.pdf")


def test_fork_parts_add_up(sb_csv):
    d = sb_figures._fork_parts(sb_csv)
    parts = sum(d[c] for c, _, _ in sb_figures.FORK_PARTS)
    assert (parts - d["total"]).abs().max() < 1e-9
    assert (d["fan_in"] + d["valid"] + d["loopbacks"] - d["fork_area"]).abs().max() < 1e-9
    assert isinstance(d, pd.DataFrame)
