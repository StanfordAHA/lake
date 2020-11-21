from lake.models.sram_wrapper_model import SRAMWrapperModel
from lake.modules.sram_wrapper import SRAMWrapper
from lake.modules.spec.agg_formal import AggFormal
from lake.passes.passes import lift_config_reg
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


def test_agg_formal():

    agg_dut = AggFormal()

    lift_config_reg(agg_dut.internal_generator)

    magma_dut = k.util.to_magma(agg_dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    config = {}
    config["agg_write_sched_gen_0_sched_addr_gen_starting_addr"] = 0
    config["agg_write_addr_gen_0_strides_0"] = 54657
    config["agg_write_addr_gen_0_strides_1"] = 0
    config["agg_write_addr_gen_0_strides_2"] = 0
    config["agg_write_addr_gen_0_strides_3"] = 0
    config["agg_write_addr_gen_0_strides_4"] = 0
    config["agg_write_addr_gen_0_strides_5"] = 0
    config["agg_read_addr_gen_0_strides_0"] = 65032
    config["agg_read_addr_gen_0_strides_1"] = 0
    config["agg_read_addr_gen_0_strides_2"] = 0
    config["agg_read_addr_gen_0_strides_3"] = 0
    config["agg_read_addr_gen_0_strides_4"] = 0
    config["agg_read_addr_gen_0_strides_5"] = 0
    config["agg_write_loops_0_ranges_0"] = 2
    config["agg_write_loops_0_ranges_1"] = 0
    config["agg_write_loops_0_ranges_2"] = 0
    config["agg_write_loops_0_ranges_3"] = 0
    config["agg_write_loops_0_ranges_4"] = 0
    config["agg_write_loops_0_ranges_5"] = 0
    config["agg_write_loops_0_dimensionality"] = 1
    config["agg_write_addr_gen_0_starting_addr"] = 0
    config["agg_read_loops_0_ranges_0"] = 0
    config["agg_read_loops_0_ranges_1"] = 0
    config["agg_read_loops_0_ranges_2"] = 0
    config["agg_read_loops_0_ranges_3"] = 0
    config["agg_read_loops_0_ranges_4"] = 0
    config["agg_read_loops_0_ranges_5"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_starting_addr"] = 4
    config["agg_read_output_sched_gen_sched_addr_gen_strides_0"] = 4
    config["agg_read_output_sched_gen_sched_addr_gen_strides_1"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_strides_2"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_strides_3"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_strides_4"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_strides_5"] = 0
    config["agg_read_addr_gen_0_starting_addr"] = 0
    config["agg_write_sched_gen_0_sched_addr_gen_strides_0"] = 1

    config["agg_write_loops_1_ranges_0"] = 0
    config["agg_write_loops_1_ranges_1"] = 0
    config["agg_write_loops_1_ranges_2"] = 0
    config["agg_write_loops_1_ranges_3"] = 0
    config["agg_write_loops_1_ranges_4"] = 0
    config["agg_write_loops_1_ranges_5"] = 0
    config["agg_write_sched_gen_0_enable"] = 1
    config["agg_write_sched_gen_1_enable"] = 1
    config["agg_write_addr_gen_0_strides_0"] = 1
    config["agg_write_addr_gen_0_strides_1"] = 0
    config["agg_write_addr_gen_0_strides_2"] = 0
    config["agg_write_addr_gen_0_strides_3"] = 0
    config["agg_write_addr_gen_0_strides_4"] = 0
    config["agg_write_addr_gen_0_strides_5"] = 0
    config["port_sel_addr_starting_addr"] = 0
    config["agg_write_sched_gen_1_sched_addr_gen_starting_addr"] = 3
    config["agg_write_loops_1_dimensionality"] = 0
    config["agg_read_addr_gen_1_strides_0"] = 0
    config["agg_read_addr_gen_1_strides_1"] = 0
    config["agg_read_addr_gen_1_strides_2"] = 0
    config["agg_read_addr_gen_1_strides_3"] = 0
    config["agg_read_addr_gen_1_strides_4"] = 0
    config["agg_read_addr_gen_1_strides_5"] = 0
    config["agg_read_loops_0_ranges_0"] = 54
    config["agg_read_loops_0_ranges_1"] = 0
    config["agg_read_loops_0_ranges_2"] = 0
    config["agg_read_loops_0_ranges_3"] = 0
    config["agg_read_loops_0_ranges_4"] = 0
    config["agg_read_loops_0_ranges_5"] = 0
    config["agg_read_addr_gen_1_starting_addr"] = 2
    config["agg_write_loops_0_dimensionality"] = 1
    config["agg_read_loops_1_dimensionality"] = 0
    config["agg_select_loops_dimensionality"] = 6
    config["agg_read_output_sched_gen_sched_addr_gen_starting_addr"] = 4
    config["agg_select_loops_ranges_0"] = 0
    config["agg_select_loops_ranges_1"] = 4
    config["agg_select_loops_ranges_2"] = 0
    config["agg_select_loops_ranges_3"] = 0
    config["agg_select_loops_ranges_4"] = 0
    config["agg_select_loops_ranges_5"] = 4
    config["agg_write_addr_gen_1_starting_addr"] = 4
    config["agg_read_loops_0_dimensionality"] = 1
    config["agg_read_output_sched_gen_sched_addr_gen_strides_0"] = 4
    config["agg_read_output_sched_gen_sched_addr_gen_strides_1"] = 4
    config["agg_read_output_sched_gen_sched_addr_gen_strides_2"] = 4
    config["agg_read_output_sched_gen_sched_addr_gen_strides_3"] = 4
    config["agg_read_output_sched_gen_sched_addr_gen_strides_4"] = 4
    config["agg_read_output_sched_gen_sched_addr_gen_strides_5"] = 4
    config["agg_read_output_sched_gen_enable"] = 1
    config["port_sel_addr_strides_0"] = 2
    config["port_sel_addr_strides_1"] = 2
    config["port_sel_addr_strides_2"] = 2
    config["port_sel_addr_strides_3"] = 2
    config["port_sel_addr_strides_4"] = 2
    config["port_sel_addr_strides_5"] = 2
    config["agg_write_addr_gen_1_strides_0"] = 0
    config["agg_write_addr_gen_1_strides_1"] = 0
    config["agg_write_addr_gen_1_strides_2"] = 0
    config["agg_write_addr_gen_1_strides_3"] = 0
    config["agg_write_addr_gen_1_strides_4"] = 0
    config["agg_write_addr_gen_1_strides_5"] = 0
    config["agg_write_addr_gen_0_starting_addr"] = 0
    config["agg_read_addr_gen_0_starting_addr"] = 0
    config["agg_write_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["agg_write_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["agg_write_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["agg_write_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["agg_write_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["agg_write_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["agg_read_addr_gen_0_strides_0"] = 1
    config["agg_read_addr_gen_0_strides_1"] = 0
    config["agg_read_addr_gen_0_strides_2"] = 0
    config["agg_read_addr_gen_0_strides_3"] = 0
    config["agg_read_addr_gen_0_strides_4"] = 0
    config["agg_read_addr_gen_0_strides_5"] = 0
    config["agg_write_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["agg_write_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["agg_write_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["agg_write_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["agg_write_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["agg_write_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["agg_write_sched_gen_0_sched_addr_gen_starting_addr"] = 0
    config["agg_write_loops_0_ranges_0"] = 1022
    config["agg_write_loops_0_ranges_1"] = 0
    config["agg_write_loops_0_ranges_2"] = 0
    config["agg_write_loops_0_ranges_3"] = 0
    config["agg_write_loops_0_ranges_4"] = 0
    config["agg_write_loops_0_ranges_5"] = 0
    config["agg_read_loops_1_ranges_0"] = 0
    config["agg_read_loops_1_ranges_1"] = 0
    config["agg_read_loops_1_ranges_2"] = 0
    config["agg_read_loops_1_ranges_3"] = 0
    config["agg_read_loops_1_ranges_4"] = 0
    config["agg_read_loops_1_ranges_5"] = 0
    # configuration registers passed through from top level
    for key, value in config.items():
        setattr(tester.circuit, key, value)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1

    rand.seed(0)

    num_iters = 500
    data_in = 0
    for i in range(num_iters):

        tester.circuit.data_in = data_in

        tester.eval()

        data_in = data_in + 1

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "dump_agg_formal"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"],
                               disp_type="realtime")


if __name__ == "__main__":
    test_agg_formal()
