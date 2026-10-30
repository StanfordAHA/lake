"""Unit tests for the 5 LaTeX-table emitters in THESIS/pipeline/tables.py."""

from __future__ import annotations

from pathlib import Path

import pandas as pd
import pytest

from THESIS.pipeline import tables
from THESIS.pipeline.errors import MissingDataError


# ---------- _fmt helper -----------------------------------------------------


@pytest.mark.parametrize("v,expected", [
    (None, "--"),
    (float("nan"), "--"),
    (True, r"\checkmark"),
    (False, ""),
    (12345.678, "1.23e+04"),  # 3 sig figs
    (0.0125, "0.0125"),
    (42, "42"),
    ("plain", "plain"),
    ("has_underscore", r"has\_underscore"),
    ("a & b", r"a \& b"),
    ("50%", r"50\%"),
])
def test_fmt_various_types(v, expected):
    assert tables._fmt(v) == expected


# ---------- _find_design_row ------------------------------------------------


def _dp(**kw):
    """Build a minimal DesignPoint-shaped object for _find_design_row."""
    from THESIS.apps.design_points import DesignPoint
    defaults = dict(
        id="x", display="x", experiment="PORT_EXP", sweep_group="tsg700",
        fetch_width=4, storage_cap_bytes=8192, data_width=16,
        in_ports=2, out_ports=2, dual_port=False, roundtrip_validated=True,
    )
    defaults.update(kw)
    return DesignPoint(**defaults)


def _fake_extractor_df() -> pd.DataFrame:
    return pd.DataFrame([
        # PORT_EXP fw=4 dw=16 8k 2x2 SP
        {"experiment": "PORT_EXP", "storage_cap": 8192, "data_width": 16, "fw": 4,
         "inp": 2, "outp": 2, "synth_total_area_um2": 14000.0,
         "synth_storage_area_um2": 7500.0, "synth_power_w": None,
         "wns_ps": 5.8, "crit_path_delay_ps": 1422.0},
        # MEMORY_EXP fw=1 dw=16 4k 1x1 DP
        {"experiment": "MEMORY_EXP", "storage_cap": 4096, "data_width": 16, "fw": 1,
         "inp": None, "outp": None, "synth_total_area_um2": 16400.0,
         "synth_storage_area_um2": 13500.0, "synth_power_w": 0.011,
         "wns_ps": 13.9, "crit_path_delay_ps": 1414.0},
    ])


def test_find_design_row_matches_multi_port(_fake=_fake_extractor_df):
    df = _fake_extractor_df()
    row = tables._find_design_row(df, _dp())
    assert row is not None
    assert row["synth_total_area_um2"] == 14000.0


def test_find_design_row_matches_single_port_via_null_inp(_fake=_fake_extractor_df):
    df = _fake_extractor_df()
    dp = _dp(experiment="MEMORY_EXP", storage_cap_bytes=4096, data_width=16,
             fetch_width=1, in_ports=1, out_ports=1, dual_port=True)
    row = tables._find_design_row(df, dp)
    assert row is not None
    assert row["synth_total_area_um2"] == 16400.0


def test_find_design_row_returns_none_when_no_match():
    dp = _dp(storage_cap_bytes=99999)  # nothing this big
    assert tables._find_design_row(_fake_extractor_df(), dp) is None


# ---------- emit_ul_ppa_summary --------------------------------------------


def test_ul_ppa_summary_writes_valid_tabular(tmp_path):
    df = _fake_extractor_df()
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(df, out)
    body = out.read_text()
    assert r"\begin{tabular}" in body
    assert r"\end{tabular}" in body
    assert r"\toprule" in body and r"\bottomrule" in body


def test_ul_ppa_summary_flags_missing_power_in_comment(tmp_path):
    """Without the MemTile sweep, a % TODO header names the blank power rows."""
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(_fake_extractor_df(), out)
    body = out.read_text()
    assert body.startswith("%")
    assert "MemTile power blank for port_fw4_dw16_sc8k_sp2x2" in body and "thesis_dse" in body


def test_ul_ppa_summary_power_from_the_memtile_sweep(tmp_path):
    """Power is the static thesis MemTile's synth active power at its clock, not
    the standalone build's synth_power_w."""
    from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL
    from THESIS.apps.garnet_configs import garnet_config
    dp = DESIGNS_SINGLE_LEVEL[0]
    memtile = pd.DataFrame([
        {"config": garnet_config(dp, "static", "thesis"), "synth_active_total_power": 0.0123,
         "clock_period_ps": 1666.67},
        {"config": garnet_config(dp, "rv", "thesis"), "synth_active_total_power": 0.0456,
         "clock_period_ps": 1428.57},
        {"config": garnet_config(dp, "static"), "synth_active_total_power": 0.0789,  # stock fabric
         "clock_period_ps": 1333.0},
    ])
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(_fake_extractor_df(), out, memtile)
    row = next(ln for ln in out.read_text().splitlines() if ln.startswith(tables._fmt(dp.display) + " &"))
    assert row.endswith(r" & 12.3 & 600 \\")
    assert "MemTile power blank for port_fw4_dw16_sc8k_sp2x2" not in out.read_text()


def test_ul_ppa_summary_flags_missing_extractor_rows(tmp_path):
    """When no extractor rows are found for any DesignPoint, the % header
    lists them."""
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(pd.DataFrame(columns=[
        "experiment", "storage_cap", "data_width", "fw", "inp", "outp",
        "synth_total_area_um2", "synth_storage_area_um2", "synth_power_w",
        "wns_ps", "crit_path_delay_ps",
    ]), out)
    body = out.read_text()
    assert body.startswith("%")
    assert "missing extractor rows" in body


def test_ul_ppa_summary_row_count_matches_designs(tmp_path):
    from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL
    df = _fake_extractor_df()
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(df, out)
    data_lines = [ln for ln in out.read_text().splitlines()
                  if ln.endswith(r" \\") and not ln.strip().startswith("%")]
    header_and_body = data_lines
    # 1 header row + N design rows
    assert len(header_and_body) == 1 + len(DESIGNS_SINGLE_LEVEL)


def test_ul_ppa_summary_raises_when_no_designs(tmp_path, monkeypatch):
    from THESIS.apps import design_points
    monkeypatch.setattr(design_points, "DESIGNS_SINGLE_LEVEL", [])
    with pytest.raises(MissingDataError, match="empty"):
        tables.emit_ul_ppa_summary(_fake_extractor_df(), tmp_path / "x.tex")


# ---------- emit_ul_perf ------------------------------------------------------


def _app_rows():
    def r(app, config, tiles, cycles, words, **kw):
        return {"app_id": app, "config": config, "tile_count": tiles, "total_cycles": cycles,
                "output_per_cycle": words / cycles, "cgra_width": 16, "cgra_height": 16, **kw}
    return pd.DataFrame([
        r("gaussian_agg", "a", 2, 4096, 4096), r("gaussian_agg", "b", 3, 4096, 4096),
        r("camera_pipeline_agg", "a", 9, 12562, 3 * 4096, cgra_width=28),
        r("matmul_pond_agg", "a", 4, 1000, 256, cgra_width=None),  # a float column, as with a gap
    ])


def test_ul_perf_one_row_per_app_with_ranges(tmp_path):
    out = tmp_path / "perf.tex"
    tables.emit_ul_perf(_app_rows(), out)
    lines = out.read_text().splitlines()
    row = {ln.split(" & ")[0]: ln for ln in lines if " & " in ln}
    assert row["Gaussian"] == r"Gaussian & 2--3 & 1 & 4,096 & 2 of 7 design points \\"
    assert row["Camera Pipeline 2x2"].endswith(r"& 0.978 & 12,562 & 1 of 7 design points; 28x16 array \\")
    assert "PE-tile pond" in row["Matrix Multiply (pond)"]
    assert row["Unsharp"] == r"Unsharp & -- & -- & -- & 0 of 7 design points \\"
    # matmul_agg is blocked and resnet_pond_agg records RV only: not in the static table
    assert "Matrix Multiply &" not in out.read_text() and "ResNet block (pond)" not in row
    rv = tmp_path / "perf_rv.tex"
    tables.emit_ul_perf(_app_rows(), rv, mode="rv")
    assert "ResNet block (pond) &" in rv.read_text()


def test_ul_perf_raises_without_results(tmp_path):
    with pytest.raises(MissingDataError):
        tables.emit_ul_perf(pd.DataFrame(), tmp_path / "x.tex")


# ---------- emit_ul_design_points ------------------------------------------


def test_ul_design_points_writes_one_row_per_design(tmp_path):
    from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL
    tables.emit_ul_design_points(_fake_extractor_df(), tmp_path / "dp.tex")
    body = (tmp_path / "dp.tex").read_text()
    for dp in DESIGNS_SINGLE_LEVEL:
        # id may contain underscores that get LaTeX-escaped.
        assert dp.id.replace("_", r"\_") in body or dp.id in body


def test_ul_design_points_marks_roundtrip_validated(tmp_path):
    tables.emit_ul_design_points(_fake_extractor_df(), tmp_path / "dp.tex")
    body = (tmp_path / "dp.tex").read_text()
    # At least one design in DESIGNS_SINGLE_LEVEL is roundtrip_validated=True.
    assert r"\checkmark" in body


# ---------- emit_exploration_applications ----------------------------------


def test_exploration_applications_lists_all_apps(tmp_path):
    from THESIS.apps.registry import APPS
    out = tmp_path / "apps.tex"
    tables.emit_exploration_applications(out)
    body = out.read_text()
    assert r"\begin{tabular}" in body
    for a in APPS:
        # display name should appear
        assert a.display in body or a.display.replace("&", r"\&") in body


def test_exploration_applications_uses_wrapping_column(tmp_path):
    """The memory-access-pattern column should wrap — check for p{...}."""
    out = tmp_path / "apps.tex"
    tables.emit_exploration_applications(out)
    body = out.read_text()
    assert "p{" in body


# ---------- emit_lake_interfaces (skeleton with prose TODOs) --------------


def test_lake_interfaces_writes_skeleton(tmp_path):
    out = tmp_path / "lake_if.tex"
    tables.emit_lake_interfaces(out)
    body = out.read_text()
    assert r"\begin{tabular}" in body
    # skeleton includes the % skeleton header
    assert body.lstrip().startswith("%")
    # at least one TODO comment for the prose column
    assert "TODO" in body


def test_lake_interfaces_scrapes_known_components(tmp_path):
    """Should include the well-known component names (Port, Storage, etc.)."""
    out = tmp_path / "lake_if.tex"
    tables.emit_lake_interfaces(out)
    body = out.read_text()
    for name in ["Port", "Storage", "MemoryPort", "IterationDomain",
                 "AddressGenerator", "ScheduleGenerator"]:
        assert name in body, f"{name} missing from lake_interfaces table"


# ---------- emit_compiler_info (skeleton) ----------------------------------


def test_compiler_info_writes_skeleton(tmp_path):
    out = tmp_path / "compiler.tex"
    tables.emit_compiler_info(out)
    body = out.read_text()
    assert r"\begin{tabular}" in body
    assert body.lstrip().startswith("%")
    assert "TODO" in body


# ---------- output-path handling -------------------------------------------


def test_emitters_create_parent_dirs(tmp_path):
    """All emit_* funcs should mkdir -p their outpath.parent."""
    deep = tmp_path / "a" / "b" / "c" / "out.tex"
    tables.emit_lake_interfaces(deep)
    assert deep.is_file()
