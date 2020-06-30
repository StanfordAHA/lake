from lake.modules.lake_new import LakeTestTop
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


def test_sram():

    dut = LakeTestTop()
    
    magma_dut = k.util.to_magma(dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.clk_en = 1
    tester.circuit.flush = 0
    tester.circuit.clk = 0
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1

    # configuration registers
    read_dimensionality = 1
    read_cycle_starting_addr = 3
    read_range_0 = 10
    read_cycle_stride_0 = 2
    read_data_starting_addr = 0
    read_data_stride_0 = 1

    write_dimensionality = 1
    write_cycle_starting_addr = 0
    write_range_0 = 10
    write_cycle_stride_0 = 2
    write_data_starting_addr = 0
    write_data_stride_0 = 1

    tester.circuit.input_addr_gen_dimensionality = write_dimensionality
    tester.circuit.input_addr_gen_ranges_0 = write_range_0
    tester.circuit.input_addr_gen_starting_addr = write_data_starting_addr
    tester.circuit.input_addr_gen_strides_0 = write_data_stride_0
    tester.circuit.input_sched_gen_sched_addr_gen_dimensionality = write_dimensionality
    tester.circuit.input_sched_gen_sched_addr_gen_ranges_0 = write_range_0
    tester.circuit.input_sched_gen_sched_addr_gen_starting_addr = write_cycle_starting_addr
    tester.circuit.input_sched_gen_sched_addr_gen_strides_0 = write_cycle_stride_0

    tester.circuit.output_addr_gen_dimensionality = read_dimensionality
    tester.circuit.output_addr_gen_ranges_0 = read_range_0
    tester.circuit.output_addr_gen_starting_addr = read_data_starting_addr
    tester.circuit.output_addr_gen_strides_0 = read_data_stride_0
    tester.circuit.output_sched_gen_sched_addr_gen_dimensionality = read_dimensionality
    tester.circuit.output_sched_gen_sched_addr_gen_ranges_0 = read_range_0
    tester.circuit.output_sched_gen_sched_addr_gen_starting_addr = read_cycle_starting_addr
    tester.circuit.output_sched_gen_sched_addr_gen_strides_0 = read_cycle_stride_0

    rand.seed(0)

    fetch_width = 1
    word_width = 16
    num_iters = 300
    for i in range(num_iters):
        tester.circuit.data_in = rand.randint(0, 2**word_width - 1)

        tester.eval()

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir="sram"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    test_sram()
