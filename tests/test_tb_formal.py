from lake.models.sram_wrapper_model import SRAMWrapperModel
from lake.modules.sram_wrapper import SRAMWrapper
from lake.modules.spec.tb_formal import TBFormal
from lake.passes.passes import lift_config_reg
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.skip
def test_tb_formal():

    tb_dut = TBFormal(data_width=16,  # CGRA Params
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

    lift_config_reg(tb_dut.internal_generator)

    magma_dut = k.util.to_magma(tb_dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    config = {}
    config["tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["output_write_loops_dimensionality"] = 0
    config["tb_read_addr_gen_0_starting_addr"] = 65532
    config["tb_read_loops_0_dimensionality"] = 0
    config["tb_write_sched_gen_sched_addr_gen_strides_0"] = 4
    config["tb_write_sched_gen_sched_addr_gen_strides_1"] = 65535
    config["tb_write_sched_gen_sched_addr_gen_strides_2"] = 65535
    config["tb_write_sched_gen_sched_addr_gen_strides_3"] = 65535
    config["tb_write_sched_gen_sched_addr_gen_strides_4"] = 65535
    config["tb_write_sched_gen_sched_addr_gen_strides_5"] = 65535
    config["tb_read_loops_0_ranges_0"] = 65535
    config["tb_read_loops_0_ranges_1"] = 65535
    config["tb_read_loops_0_ranges_2"] = 65535
    config["tb_read_loops_0_ranges_3"] = 65535
    config["tb_read_loops_0_ranges_4"] = 65535
    config["tb_read_loops_0_ranges_5"] = 65535
    config["tb_write_loops_0_ranges_0"] = 65535
    config["tb_write_loops_0_ranges_1"] = 65535
    config["tb_write_loops_0_ranges_2"] = 65535
    config["tb_write_loops_0_ranges_3"] = 65535
    config["tb_write_loops_0_ranges_4"] = 65535
    config["tb_write_loops_0_ranges_5"] = 65535
    config["tb_write_addr_gen_0_strides_0"] = 65533
    config["tb_write_addr_gen_0_strides_1"] = 65535
    config["tb_write_addr_gen_0_strides_2"] = 65535
    config["tb_write_addr_gen_0_strides_3"] = 65535
    config["tb_write_addr_gen_0_strides_4"] = 65535
    config["tb_write_addr_gen_0_strides_5"] = 65535
    config["tb_write_sched_gen_sched_addr_gen_starting_addr"] = 6
    config["tb_read_addr_gen_0_strides_0"] = 65521
    config["tb_read_addr_gen_0_strides_1"] = 65535
    config["tb_read_addr_gen_0_strides_2"] = 65535
    config["tb_read_addr_gen_0_strides_3"] = 65535
    config["tb_read_addr_gen_0_strides_4"] = 65535
    config["tb_read_addr_gen_0_strides_5"] = 65535
    config["output_write_loops_ranges_0"] = 574
    config["output_write_loops_ranges_1"] = 65535
    config["output_write_loops_ranges_2"] = 65535
    config["output_write_loops_ranges_3"] = 65535
    config["output_write_loops_ranges_4"] = 65535
    config["output_write_loops_ranges_5"] = 65535
    config["tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 65535
    config["tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 65535
    config["tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 65535
    config["tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 65535
    config["tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 65535
    config["tb_write_loops_0_dimensionality"] = 0
    config["tb_write_addr_gen_0_starting_addr"] = 65535
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

    num_iters = 64
    data_in = 0
    for i in range(num_iters):

        for i in range(4):
            setattr(tester.circuit, f"data_in_{i}", data_in)

        tester.eval()

        data_in = data_in + 1

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_tb_formal()
