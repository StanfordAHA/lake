from lake.models.sram_wrapper_model import SRAMWrapperModel
from lake.modules.sram_wrapper import SRAMWrapper
from lake.modules.spec.sram_formal import SRAMFormal
from lake.passes.passes import lift_config_reg
from lake.utils.util import transform_strides_and_ranges
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


def test_sram_formal():

    sram_dut = SRAMFormal(data_width=16,  # CGRA Params
                          mem_width=64,
                          mem_depth=512,
                          banks=1,
                          input_addr_iterator_support=6,
                          output_addr_iterator_support=6,
                          input_sched_iterator_support=6,
                          output_sched_iterator_support=6,
                          config_width=16,
                          #  output_config_width=16,
                          interconnect_input_ports=1,  # Connection to int
                          interconnect_output_ports=1,
                          mem_input_ports=1,
                          mem_output_ports=1,
                          read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                          rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                          agg_height=4)

    lift_config_reg(sram_dut.internal_generator)

    magma_dut = k.util.to_magma(sram_dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    in_ranges = [2, 8, 1]
    in_addr_strides = [1, 2, 0]
    in_addr_strt = 0
    in_sched_strides = [4, 8, 0]
    in_sched_strt = 4
    dim = 3

    (write_ranges, tform_in_addr) = transform_strides_and_ranges(ranges=in_ranges,
                                                                 strides=in_addr_strides,
                                                                 dimensionality=dim)
    (write_ranges, tform_in_sched) = transform_strides_and_ranges(ranges=in_ranges,
                                                                  strides=in_sched_strides,
                                                                  dimensionality=dim)

    out_ranges = [2, 8, 1]
    out_addr_strides = [1, 2, 0]
    out_addr_strt = 0
    out_sched_strides = [4, 8, 0]
    out_sched_strt = 6
    dim = 3

    (read_ranges, tform_out_addr) = transform_strides_and_ranges(ranges=out_ranges,
                                                                 strides=out_addr_strides,
                                                                 dimensionality=dim)
    (read_ranges, tform_out_sched) = transform_strides_and_ranges(ranges=out_ranges,
                                                                  strides=out_sched_strides,
                                                                  dimensionality=dim)

    config = {}
    # config["sram_write_loops_ranges_0"] = 485
    # config["sram_write_loops_ranges_1"] = 0
    # config["sram_write_loops_ranges_2"] = 0
    # config["sram_write_loops_ranges_3"] = 0
    # config["sram_write_loops_ranges_4"] = 0
    # config["sram_write_loops_ranges_5"] = 0
    # # config["sram_read_sched_gen_sched_addr_gen_starting_addr"] = 5
    # # config["sram_read_sched_gen_sched_addr_gen_strides_0"] = 3
    # # config["sram_read_sched_gen_sched_addr_gen_strides_1"] = 1
    # # config["sram_read_sched_gen_sched_addr_gen_strides_2"] = 65535
    # # config["sram_read_sched_gen_sched_addr_gen_strides_3"] = 65535
    # # config["sram_read_sched_gen_sched_addr_gen_strides_4"] = 65535
    # # config["sram_read_sched_gen_sched_addr_gen_strides_5"] = 65535
    # config["sram_write_addr_gen_strides_0"] = 65534
    # config["sram_write_addr_gen_strides_1"] = 0
    # config["sram_write_addr_gen_strides_2"] = 0
    # config["sram_write_addr_gen_strides_3"] = 0
    # config["sram_write_addr_gen_strides_4"] = 0
    # config["sram_write_addr_gen_strides_5"] = 0
    # config["sram_write_addr_gen_starting_addr"] = 33791
    # config["sram_write_sched_gen_sched_addr_gen_starting_addr"] = 4
    # config["sram_read_addr_gen_strides_0"] = 65535
    # config["sram_read_addr_gen_strides_1"] = 65535
    # config["sram_read_addr_gen_strides_2"] = 65535
    # config["sram_read_addr_gen_strides_3"] = 65535
    # config["sram_read_addr_gen_strides_4"] = 65535
    # config["sram_read_addr_gen_strides_5"] = 65535
    # config["sram_write_loops_dimensionality"] = 0
    # # config["sram_read_loops_dimensionality"] = 0
    # # config["sram_read_loops_ranges_0"] = 0
    # # config["sram_read_loops_ranges_1"] = 65535
    # # config["sram_read_loops_ranges_2"] = 65535
    # # config["sram_read_loops_ranges_3"] = 65535
    # # config["sram_read_loops_ranges_4"] = 65535
    # # config["sram_read_loops_ranges_5"] = 65535
    # config["sram_read_addr_gen_starting_addr"] = 511
    # config["sram_write_sched_gen_sched_addr_gen_strides_0"] = 4
    # config["sram_write_sched_gen_sched_addr_gen_strides_1"] = 0
    # config["sram_write_sched_gen_sched_addr_gen_strides_2"] = 0
    # config["sram_write_sched_gen_sched_addr_gen_strides_3"] = 0
    # config["sram_write_sched_gen_sched_addr_gen_strides_4"] = 0
    # config["sram_write_sched_gen_sched_addr_gen_strides_5"] = 0

    # config["sram_read_loops_dimensionality"] = 0
    # config["sram_read_sched_gen_sched_addr_gen_strides_0"] = 4
    # config["sram_read_sched_gen_sched_addr_gen_strides_1"] = 65535
    # config["sram_read_sched_gen_sched_addr_gen_strides_2"] = 65535
    # config["sram_read_sched_gen_sched_addr_gen_strides_3"] = 65535
    # config["sram_read_sched_gen_sched_addr_gen_strides_4"] = 65535
    # config["sram_read_sched_gen_sched_addr_gen_strides_5"] = 65535
    # config["sram_read_sched_gen_sched_addr_gen_starting_addr"] = 5
    # config["sram_read_loops_ranges_0"] = 398
    # config["sram_read_loops_ranges_1"] = 65535
    # config["sram_read_loops_ranges_2"] = 65535
    # config["sram_read_loops_ranges_3"] = 65535
    # config["sram_read_loops_ranges_4"] = 65535
    # config["sram_read_loops_ranges_5"] = 65535

    config["sram_read_sched_gen_sched_addr_gen_starting_addr"] = 5
    config["sram_write_loops_ranges_0"] = 65535
    config["sram_write_loops_ranges_1"] = 65535
    config["sram_write_loops_ranges_2"] = 65535
    config["sram_write_loops_ranges_3"] = 65535
    config["sram_write_loops_ranges_4"] = 65535
    config["sram_write_loops_ranges_5"] = 65535
    config["sram_read_sched_gen_sched_addr_gen_strides_0"] = 4
    config["sram_read_sched_gen_sched_addr_gen_strides_1"] = 65535
    config["sram_read_sched_gen_sched_addr_gen_strides_2"] = 65535
    config["sram_read_sched_gen_sched_addr_gen_strides_3"] = 65535
    config["sram_read_sched_gen_sched_addr_gen_strides_4"] = 65535
    config["sram_read_sched_gen_sched_addr_gen_strides_5"] = 65535
    config["sram_write_addr_gen_strides_0"] = 65535
    config["sram_write_addr_gen_strides_1"] = 65535
    config["sram_write_addr_gen_strides_2"] = 65535
    config["sram_write_addr_gen_strides_3"] = 65535
    config["sram_write_addr_gen_strides_4"] = 65535
    config["sram_write_addr_gen_strides_5"] = 65535
    config["sram_write_addr_gen_starting_addr"] = 65535
    config["sram_write_sched_gen_sched_addr_gen_starting_addr"] = 4
    config["sram_read_addr_gen_strides_0"] = 65535
    config["sram_read_addr_gen_strides_1"] = 65535
    config["sram_read_addr_gen_strides_2"] = 65535
    config["sram_read_addr_gen_strides_3"] = 65535
    config["sram_read_addr_gen_strides_4"] = 65535
    config["sram_read_addr_gen_strides_5"] = 65535
    config["sram_write_loops_dimensionality"] = 0
    config["sram_read_loops_dimensionality"] = 0
    config["sram_read_loops_ranges_0"] = 398
    config["sram_read_loops_ranges_1"] = 65535
    config["sram_read_loops_ranges_2"] = 65535
    config["sram_read_loops_ranges_3"] = 65535
    config["sram_read_loops_ranges_4"] = 65535
    config["sram_read_loops_ranges_5"] = 65535
    config["sram_read_addr_gen_starting_addr"] = 65535
    config["sram_write_sched_gen_sched_addr_gen_strides_0"] = 4
    config["sram_write_sched_gen_sched_addr_gen_strides_1"] = 65535
    config["sram_write_sched_gen_sched_addr_gen_strides_2"] = 65535
    config["sram_write_sched_gen_sched_addr_gen_strides_3"] = 65535
    config["sram_write_sched_gen_sched_addr_gen_strides_4"] = 65535
    config["sram_write_sched_gen_sched_addr_gen_strides_5"] = 65535

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

    im_size = 40
    num_iters = im_size * im_size
    data_in = 0
    for i in range(num_iters):

        for i in range(4):
            setattr(tester.circuit, f'data_in_{i}', data_in + i)

        tester.eval()

        data_in = data_in + 4

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_sram_formal()
