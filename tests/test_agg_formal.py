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

    agg_dut = AggFormal(data_width=16,  # CGRA Params
                        mem_width=64,
                        mem_depth=512,
                        banks=1,
                        input_addr_iterator_support=6,
                        output_addr_iterator_support=6,
                        input_sched_iterator_support=6,
                        output_sched_iterator_support=6,
                        config_width=16,
                        interconnect_input_ports=1,  # Connection to int
                        interconnect_output_ports=1,
                        mem_input_ports=1,
                        mem_output_ports=1,
                        read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                        rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                        agg_height=4)

    lift_config_reg(agg_dut.internal_generator)

    magma_dut = k.util.to_magma(agg_dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    config = {}
    config["agg_select_loops_ranges_0"] = 485
    config["agg_select_loops_ranges_1"] = 0
    config["agg_select_loops_ranges_2"] = 0
    config["agg_select_loops_ranges_3"] = 0
    config["agg_select_loops_ranges_4"] = 0
    config["agg_select_loops_ranges_5"] = 0
    config["agg_write_addr_gen_0_starting_addr"] = 32780
    config["agg_read_output_sched_gen_sched_addr_gen_starting_addr"] = 4
    config["agg_write_addr_gen_0_strides_0"] = 65521
    config["agg_write_addr_gen_0_strides_1"] = 1
    config["agg_write_addr_gen_0_strides_2"] = 65525
    config["agg_write_addr_gen_0_strides_3"] = 15
    config["agg_write_addr_gen_0_strides_4"] = 65535
    config["agg_write_addr_gen_0_strides_5"] = 15
    config["agg_select_loops_dimensionality"] = 0
    config["agg_read_loops_0_ranges_0"] = 62
    config["agg_read_loops_0_ranges_1"] = 0
    config["agg_read_loops_0_ranges_2"] = 3
    config["agg_read_loops_0_ranges_3"] = 65151
    config["agg_read_loops_0_ranges_4"] = 65535
    config["agg_read_loops_0_ranges_5"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_strides_0"] = 4
    config["agg_read_output_sched_gen_sched_addr_gen_strides_1"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_strides_2"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_strides_3"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_strides_4"] = 0
    config["agg_read_output_sched_gen_sched_addr_gen_strides_5"] = 0
    config["agg_write_loops_0_dimensionality"] = 0
    config["agg_write_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["agg_write_sched_gen_0_sched_addr_gen_strides_1"] = 2
    config["agg_write_sched_gen_0_sched_addr_gen_strides_2"] = 1
    config["agg_write_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["agg_write_sched_gen_0_sched_addr_gen_strides_4"] = 1
    config["agg_write_sched_gen_0_sched_addr_gen_strides_5"] = 65
    config["agg_read_addr_gen_0_strides_0"] = 65533
    config["agg_read_addr_gen_0_strides_1"] = 1
    config["agg_read_addr_gen_0_strides_2"] = 65533
    config["agg_read_addr_gen_0_strides_3"] = 3
    config["agg_read_addr_gen_0_strides_4"] = 65535
    config["agg_read_addr_gen_0_strides_5"] = 0
    config["agg_write_loops_0_ranges_0"] = 1934
    config["agg_write_loops_0_ranges_1"] = 0
    config["agg_write_loops_0_ranges_2"] = 0
    config["agg_write_loops_0_ranges_3"] = 4116
    config["agg_write_loops_0_ranges_4"] = 1
    config["agg_write_loops_0_ranges_5"] = 1
    config["agg_read_loops_0_dimensionality"] = 0
    config["agg_read_addr_gen_0_starting_addr"] = 32775
    config["agg_write_sched_gen_0_sched_addr_gen_starting_addr"] = 0

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
        tempdir = "dump_agg"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    test_agg_formal()