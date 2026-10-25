"""Unit tests for THESIS/apps/garnet_configs.py: garnet sweep config names <->
lake design points, and the bundle / app-power command matrix."""

from __future__ import annotations

import pytest

from THESIS.apps import garnet_configs as gc
from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL, DesignPoint


def _dp(**kw):
    base = dict(id="d", display="d", experiment="PORT_EXP", sweep_group="g", fetch_width=4,
                storage_cap_bytes=8192, data_width=16, in_ports=2, out_ports=2, dual_port=False)
    return DesignPoint(**{**base, **kw})


@pytest.mark.parametrize("name", [
    "fw4_dw16_sc8192_sp_in2_out2_vc2",
    "fw4_dw16_sc8192_sp_in2_out2_vc2_rv",
    "fw1_dw16_sc4096_sp_in1_out1",
    "fw2_dw16_sc4096_dp_in2_out2_vc2",
    "fw1_dw16_sc8192_sp_in2_out2_dim4_msw1024_rv",
    "fw1_dw16_sc8192_sp_in2_out2_me4096",
    "fw4_dw16_sc8192_sp_in2_out2_vc2_ff8_rv",
    "fw4_dw16_sc4096_sp_in2_out2_vc2_sharectr_portcg_thesis",
    "fw4_dw16_sc4096_sp_in2_out2_vc2_nofp_portcg_thesis_rv",
    "fw4_dw16_sc4096_sp_in2_out2_vc2_ff8_portcg_thesis_rv",
])
def test_parse_and_name_round_trip(name):
    p = gc.parse_config_name(name)
    assert gc.config_name(p["spec"], p["mode"], p["thesis"]) == name


def test_parse_reads_spec_mode_fabric():
    p = gc.parse_config_name("fw2_dw16_sc4096_dp_in2_out2_vc2_nofp_portcg_thesis_rv")
    assert p["mode"] == "rv" and p["thesis"]
    assert p["spec"] == dict(vec_width=2, dual_port=True, add_filter_path=False, clock_gate_ports=True)


def test_parse_rejects_other_names():
    with pytest.raises(ValueError):
        gc.parse_config_name("stock_rv")


def test_garnet_config_per_fabric_and_mode():
    dp = _dp()
    assert gc.garnet_config(dp, "static", "stock") == "fw4_dw16_sc8192_sp_in2_out2_vc2"
    assert gc.garnet_config(dp, "rv", "stock") == "fw4_dw16_sc8192_sp_in2_out2_vc2_rv"
    assert gc.garnet_config(dp, "static", "thesis") == "fw4_dw16_sc8192_sp_in2_out2_vc2_sharectr_portcg_thesis"
    assert gc.garnet_config(dp, "rv", "thesis") == "fw4_dw16_sc8192_sp_in2_out2_vc2_nofp_portcg_thesis_rv"
    assert gc.garnet_config(_dp(fetch_width=1, in_ports=1, out_ports=1, dual_port=True,
                                storage_cap_bytes=4096), "static", "stock") == "fw1_dw16_sc4096_dp_in1_out1"


def test_designs_for_spec_ignores_fabric_options_and_defaults():
    dps = [_dp(id="a"), _dp(id="b", storage_cap_bytes=4096)]
    spec = gc.parse_config_name("fw4_dw16_sc8192_sp_in2_out2_vc2_nofp_portcg_thesis_rv")["spec"]
    assert [d.id for d in gc.designs_for_spec(spec, dps)] == ["a"]
    assert [d.id for d in gc.designs_for_spec(dict(storage_capacity=4096), dps)] == ["b"]
    # a non-default geometry key (vec_capacity 4) is another design point
    assert gc.designs_for_spec(dict(storage_capacity=8192, vec_width=4, vec_capacity=4), dps) == []


def test_baseline_set_has_the_known_duplicate():
    assert ["port_fw4_dw16_sc8k_sp2x2", "mem_fw4_dw16_sc8k_sp2x2"] in gc.duplicate_designs(DESIGNS_SINGLE_LEVEL)


def test_rv_refuses_wide_vec_capacity():
    assert gc.unsupported(dict(vec_width=4, vec_capacity=4), "rv")
    assert gc.unsupported(dict(vec_width=4, vec_capacity=4), "static") is None


def test_app_map_by_basename_with_overrides():
    m = gc.app_map({"tests/matmul_rvtry3": "matmul_agg"})
    assert m["gaussian"] == "gaussian_agg"
    assert m["harris_color"] == "harris_agg"
    assert m["matmul_rvtry3"] == "matmul_agg"
    assert gc.app_basename("thesis_x/gaussian/") == "gaussian"


def test_bundle_commands_one_tree_per_app_balance_only_on_stock_rv():
    dps = [_dp(id="a"), _dp(id="b")]  # same spec: b's configs are skipped
    cmds, notes = gc.bundle_commands(dps, ["apps/gaussian", "apps/unsharp"], ["static", "rv"],
                                     "thesis", "/b", "/t", "/p.json")
    assert list(cmds) == ["apps/gaussian", "apps/unsharp"]
    assert [len(c) for c in cmds.values()] == [2, 2]
    assert sum("same config as an earlier design point" in n for n in notes) == 2
    every = [c for cs in cmds.values() for c in cs]
    assert not any("--rv-balance" in c for c in every)
    assert all("--cgra-tiles --pond-spec /p.json" in c for c in every)
    assert all("--aha-dir /t/unsharp" in c for c in cmds["apps/unsharp"])
    stock, _ = gc.bundle_commands(dps[:1], ["apps/gaussian"], ["static", "rv"], "stock", "/b", "/t", "/p.json")
    assert ["--rv-balance 8" in c for c in stock["apps/gaussian"]] == [False, True]
    assert ("--config fw4_dw16_sc8192_sp_in2_out2_vc2_rv --app apps/gaussian --bundle-dir /b/gaussian"
            in stock["apps/gaussian"][1])


def test_bundle_script_runs_one_job_per_app():
    lines = gc.bundle_script([_dp()], ["apps/gaussian", "apps/unsharp"], ["static", "rv"],
                             "thesis", "/b", "/t", "/b/p.json")
    text = "\n".join(lines)
    assert text.startswith("#!/bin/bash")
    assert "make_tree /t/gaussian" in text and "make_tree /t/unsharp" in text
    assert text.count(") > /b/gaussian.log 2>&1 &") == 1 and lines[-1] == "wait"


def test_power_command_clock_per_fabric():
    assert "--clocks-from /c.json" in gc.power_command(["apps/gaussian"], "/b", "/o", "thesis", "/c.json")
    cmd = gc.power_command(["apps/gaussian", "apps/unsharp"], "/b", "/o", "stock", "/c.json")
    assert "--clock-mhz 400" in cmd and "--variants gaussian=/b/gaussian,unsharp=/b/unsharp" in cmd
