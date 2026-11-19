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


def _fake_memtile(skip=()):
    """Static thesis-tile rows of the final sweep for every design point (index i:
    std-cell (1 + i) * 1000 um2, SRAM 1000 um2, 10 + i mW, 600 MHz), plus the first
    one's RV tile first, which the table must skip."""
    from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL
    from THESIS.apps.garnet_configs import garnet_config
    rows = [{"config": garnet_config(DESIGNS_SINGLE_LEVEL[0], "rv", "thesis"), "syn_std": 1.0,
             "syn_sram": 1.0, "synth_active_total_power": 0.5, "clock_period_ps": 1428.57}]
    rows += [{"config": garnet_config(dp, "static", "thesis"), "syn_std": (1 + i) * 1000.0,
              "syn_sram": 1000.0, "synth_active_total_power": (10 + i) * 1e-3,
              "clock_period_ps": 1666.67, "tile_synth_wns_ps": 0.5}
             for i, dp in enumerate(DESIGNS_SINGLE_LEVEL) if dp.id not in skip]
    return pd.DataFrame(rows)


def _ppa_row(out, dp):
    return next(ln for ln in out.read_text().splitlines() if ln.startswith(tables._fmt(dp.display) + " &"))


def test_ul_ppa_summary_writes_valid_tabular(tmp_path):
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(_fake_extractor_df(), out, _fake_memtile())
    body = out.read_text()
    assert r"\begin{tabular}" in body
    assert r"\end{tabular}" in body
    assert r"\toprule" in body and r"\bottomrule" in body
    assert r"\si{" not in body  # main.tex does not load siunitx


def test_ul_ppa_summary_flags_missing_rows_in_comment(tmp_path):
    """A design point without a thesis_dse row is blank and named in a % TODO header."""
    from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL
    dp = DESIGNS_SINGLE_LEVEL[1]
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(_fake_extractor_df(), out, _fake_memtile(skip={dp.id}))
    body = out.read_text()
    assert body.startswith("%")
    assert f"blank for {dp.id}" in body and "thesis_dse" in body
    assert _ppa_row(out, dp).endswith(r" & -- & -- & -- & -- & -- \\")


def test_ul_ppa_summary_from_the_final_memtile_sweep(tmp_path):
    """Every column comes from the static thesis MemTile: std-cell area absolute,
    tile area and power relative to the baseline (SRAM_NDA), its clock and slack."""
    from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(_fake_extractor_df(), out, _fake_memtile())
    assert _ppa_row(out, DESIGNS_SINGLE_LEVEL[0]).endswith(r" & 1.00 & 1.00 & 1.00 & 600 & 0.5 \\")
    # index 1: (2000 + 1000) / (1000 + 1000) area, 11 / 10 mW
    assert _ppa_row(out, DESIGNS_SINGLE_LEVEL[1]).endswith(r" & 2.00 & 1.50 & 1.10 & 600 & 0.5 \\")


def test_ul_ppa_summary_raises_without_the_baseline(tmp_path):
    from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL
    from THESIS.apps.garnet_configs import garnet_config
    base = garnet_config(DESIGNS_SINGLE_LEVEL[0], "static", "thesis")
    # the MEM fw4 dw16 8KB point is the same tile as the baseline
    same = {dp.id for dp in DESIGNS_SINGLE_LEVEL if garnet_config(dp, "static", "thesis") == base}
    with pytest.raises(MissingDataError, match="baseline"):
        tables.emit_ul_ppa_summary(_fake_extractor_df(), tmp_path / "x.tex", _fake_memtile(skip=same))


def test_ul_ppa_summary_row_count_matches_designs(tmp_path):
    from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(_fake_extractor_df(), out, _fake_memtile())
    data_lines = [ln for ln in out.read_text().splitlines()
                  if ln.endswith(r" \\") and not ln.strip().startswith("%")]
    # 1 header row + N design rows
    assert len(data_lines) == 1 + len(DESIGNS_SINGLE_LEVEL)


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


def _apps_table(tmp_path, static=None, rv=None):
    out = tmp_path / "apps.tex"
    tables.emit_exploration_applications(_app_rows() if static is None else static,
                                         _app_rows() if rv is None else rv, out)
    body = out.read_text()
    return body, {ln.split(" & ")[0]: ln for ln in body.splitlines() if " & " in ln}


def test_exploration_applications_lists_every_recorded_app(tmp_path):
    from THESIS.apps.registry import APPS
    body, row = _apps_table(tmp_path)
    assert r"\begin{tabular}" in body and r"\multicolumn{2}{c}{Memory Tiles}" in body
    for a in APPS:
        # matmul_agg never maps: it records nothing, so it has no row
        assert (a.display in row) == (not a.bundle_blocked), a.id


def test_exploration_applications_memory_tiles_per_mode(tmp_path):
    rv = _app_rows().assign(tile_count=lambda d: d["tile_count"] - 1)
    rv = pd.concat([rv, pd.DataFrame([{"app_id": "resnet_pond_agg", "config": "a", "tile_count": 5}])])
    body, row = _apps_table(tmp_path, rv=rv)
    cells = {k: v.removesuffix(r" \\").split(" & ") for k, v in row.items()}
    assert cells["Gaussian"][2:] == ["2--3", "1--2", "Line Buffer"]
    assert cells["Matrix Multiply (pond)"][2:4] == ["4", "3"]
    # resnet_pond_agg records ready/valid only: its static cell is "--" with no TODO
    assert cells["ResNet block (pond)"][2:4] == ["--", "5"]
    # Unsharp has no results in either mode: blank, and called out in a comment
    assert cells["Unsharp"][2:4] == ["--", "--"]
    assert body.startswith("% TODO: no app results for ") and "unsharp_agg (static)" in body
    assert "resnet_pond_agg (static)" not in body


def test_exploration_applications_adds_a_non_default_array(tmp_path):
    _, row = _apps_table(tmp_path)
    assert r"per cycle, on a 28$\times$16 array & " in row["Camera Pipeline 2x2"]
    assert "array" not in row["Gaussian"]


def test_exploration_applications_classes_are_the_three_defined():
    from THESIS.apps.registry import APPS, MEMORY_CLASSES
    for a in APPS:
        for c in a.memory_classes.split(", "):
            assert c.removesuffix(" (pond)") in MEMORY_CLASSES, (a.id, c)


def test_exploration_applications_raises_without_results(tmp_path):
    with pytest.raises(MissingDataError):
        tables.emit_exploration_applications(pd.DataFrame(), pd.DataFrame(), tmp_path / "x.tex")


def test_exploration_applications_wraps_the_text_columns(tmp_path):
    body, _ = _apps_table(tmp_path)
    assert body.count(r">{\raggedright\arraybackslash}p{") == 3


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


def test_ul_ppa_summary_power_from_a_loaded_thesis_sweep(tmp_path):
    """tile_sweep.load() renames thesis configs to stock names; the table looks the
    design point up by the garnet name it keeps in garnet_config."""
    from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL
    from THESIS.apps.garnet_configs import garnet_config
    from THESIS.pipeline import tile_sweep
    dp = DESIGNS_SINGLE_LEVEL[0]
    csv = tmp_path / "sweep.csv"
    pd.DataFrame([
        {"config": garnet_config(dp, "static", "thesis"), "status": "PASS", "syn_cell": 1.0,
         "syn_std": 1000.0, "syn_sram": 1000.0, "synth_active_total_power": 0.0123,
         "clock_period_ps": 1666.67},
    ]).to_csv(csv, index=False)
    out = tmp_path / "ppa.tex"
    tables.emit_ul_ppa_summary(_fake_extractor_df(), out, tile_sweep.load(csv))
    row = next(ln for ln in out.read_text().splitlines() if ln.startswith(tables._fmt(dp.display) + " &"))
    assert row.endswith(r" & 1.00 & 1.00 & 1.00 & 600 & -- \\")
