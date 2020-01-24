from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile


def top_test():
    ### DUT
    lt_dut = LakeTop(data_width=16,
                     mem_width=64,
                     mem_depth=512,
                     banks=2,
                     input_iterator_support=6,
                     output_iterator_support=6,
                     interconnect_input_ports=1,
                     interconnect_output_ports=1,
                     mem_input_ports=1,
                     mem_output_ports=1,
                     use_sram_stub=1,
                     agg_height=8,
                     transpose_height=8,
                     max_agg_schedule=64,
                     input_max_port_sched=64,
                     output_max_port_sched=64,
                     align_input=1,
                     max_line_length=2048,
                     tb_height=4,
                     tb_range_max=2048,
                     tb_sched_max=64)
    magma_dut = kts.util.to_magma(lt_dut, flatten_array=True, check_multiple_driver=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###
    # for key, value in new_config.items():
    #     setattr(tester.circuit, key, value)

    tester.circuit.line_length = 5

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    for i in range(10):
        tester.step(2)

    tester.circuit.data_in = 7
    tester.circuit.valid_in = 1

    for i in range(100):
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "./top_dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    top_test()
