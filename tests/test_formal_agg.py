from lake.modules.spec.agg_formal import AggFormal
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


def test_agg():

    config = {}
    config["tile_en"] = 1
    config["clk_en"] = 1

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

    new_config = config
    dut = AggFormal()
    magma_dut = k.util.to_magma(dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1

    for key in new_config.keys():
        setattr(tester.circuit, key, new_config[key])

    num_iters =1000 
    for i in range(num_iters):

        tester.circuit.data_in = i

        tester.eval()

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir="agg_formal"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    test_agg()
