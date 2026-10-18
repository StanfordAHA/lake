"""MemoryTileBuilder hybrid_policy and the dense ready-valid PE (OnyxDenseRVPE).

A hybrid port (HybridPortAddr) is a ready-valid controller port that can also run static.
"bypass" (default): a tile FIFO plus *_bypass_rv / *_fine_grain_fifo_bypass config regs;
"rv": the tile FIFO, no bypass regs; "static": no FIFO and no tile valid/ready.
garnet.py --thesis builds its static fabric with "static" and its RV fabric with "rv".
"""
import re

import kratos as kts
import pytest

from lake.top.core_combiner import CoreCombiner
from lake.modules.onyx_dense_rv_pe import OnyxDenseRVPE
from lake.spec.spec_memory_controller import SpecMemoryController, build_spec


def _tile_verilog(tmp_path, name, controllers, policy, ready_valid, prefix, mem_depth=512):
    cc = CoreCombiner(data_width=16, mem_width=64, mem_depth=mem_depth, name=name,
                      controllers=controllers, use_sim_sram=True, do_config_lift=False,
                      io_prefix=prefix, fifo_depth=2, tech_map_name="GF",
                      ready_valid=ready_valid, hybrid_policy=policy)
    path = tmp_path / f"{name}.sv"
    kts.verilog(cc.dut, filename=str(path), optimize_if=False)
    return cc, path.read_text()


def _top_ports(verilog, module):
    m = re.search(rf"^module {module} \((.*?)\);", verilog, re.S | re.M)
    assert m, module
    return re.findall(r"(?:input|output) logic (?:\[[^\]]*\] )*(\w+)", m.group(1))


def _static_mem(policy, tmp_path):
    # A static spec marks its data ports hybrid (they also have valid/ready)
    spec = build_spec(storage_capacity=8192, data_width=16, vec_width=4, in_ports=2, out_ports=2,
                      config_passthru=True, comply_17=False)
    return _tile_verilog(tmp_path, f"MEM_{policy}", [SpecMemoryController(spec=spec)], policy,
                         ready_valid=policy != "static", prefix="MEM_", mem_depth=1024)


def test_static_policy_has_no_fifos_or_ready_valid(tmp_path):
    cc, v = _static_mem("static", tmp_path)
    ports = _top_ports(v, "MEM_static")
    assert "MEM_input_width_16_num_0" in ports and "MEM_output_width_16_num_1" in ports
    assert not [p for p in ports if p.endswith(("_valid", "_ready"))]
    assert "reg_fifo" not in v and "bypass_rv" not in v and "fine_grain" not in v
    assert cc.dut.get_hybrid_bypass_regs("lakespec") == []


def test_bypass_policy_is_the_default(tmp_path):
    cc, v = _static_mem("bypass", tmp_path)
    assert "reg_fifo" in v
    regs = cc.dut.get_hybrid_bypass_regs("lakespec")
    assert len(regs) == 4 and all(r.endswith("_bypass_rv") for r in regs)


def test_static_policy_refuses_a_true_ready_valid_port(tmp_path):
    with pytest.raises(ValueError, match="cannot run static"):
        _tile_verilog(tmp_path, "PE_bad", [OnyxDenseRVPE(data_width=16, fifo_depth=2)], "static",
                      ready_valid=False, prefix="PE_")


def test_dense_rv_pe_tile(tmp_path):
    cc, v = _tile_verilog(tmp_path, "PE_rv", [OnyxDenseRVPE(data_width=16, fifo_depth=2)], "rv",
                          ready_valid=True, prefix="PE_")
    assert cc.get_port_remap()["alu"] == {
        "data0": "PE_input_width_16_num_0", "data1": "PE_input_width_16_num_1",
        "data2": "PE_input_width_16_num_2", "res": "PE_output_width_16_num_0",
        "bit0": "PE_input_width_1_num_0", "bit1": "PE_input_width_1_num_1",
        "bit2": "PE_input_width_1_num_2", "res_p": "PE_output_width_1_num_0"}
    ports = _top_ports(v, "PE_rv")
    # every data and bit port is ready-valid, with its tile FIFO (and pre-fill config)
    for p in cc.get_port_remap()["alu"].values():
        assert f"{p}_valid" in ports and f"{p}_ready" in ports, p
        assert f"{p}_fifo_bogus_init_num" in ports, p
    assert "bypass_rv" not in v and "fine_grain" not in v
    assert "bypass_prim_outfifo" in v  # the configurable primitive output FIFO stays
