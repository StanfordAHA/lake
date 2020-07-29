from lake.models.sram_wrapper_model import SRAMWrapperModel
from lake.modules.sram_wrapper import SRAMWrapper
from lake.modules.spec.sram_formal import SRAMFormal
from lake.passes.passes import lift_config_reg
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

    magma_dut = k.util.to_magma(dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    new_config = {}
    new_config['sram_read_addr_gen_starting_addr'] = 1
    new_config['sram_read_addr_gen_strides_0'] = 1
    new_config['sram_read_addr_gen_strides_1'] = 1
    new_config['sram_read_addr_gen_strides_2'] = 1
    new_config['sram_read_loops_dimensionality'] = 1
    new_config['sram_read_loops_ranges_0'] = 1
    new_config['sram_read_loops_ranges_1'] = 1
    new_config['sram_read_loops_ranges_2'] = 1
    new_config['sram_read_sched_gen_sched_addr_gen_starting_addr'] = 1
    new_config['sram_read_sched_gen_sched_addr_gen_strides_0'] = 1
    new_config['sram_read_sched_gen_sched_addr_gen_strides_1'] = 1
    new_config['sram_read_sched_gen_sched_addr_gen_strides_2'] = 1
    new_config['sram_write_addr_gen_starting_addr'] = 1
    # configuration registers passed through from top level
    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1

    rand.seed(0)

    input logic [3:0] [15:0] data_in,
    output logic [3:0] [15:0] data_out,
    input logic [15:0] sram_write_addr_gen_starting_addr,
    input logic [5:0] [15:0] sram_write_addr_gen_strides,
    input logic [3:0] sram_write_loops_dimensionality,
    input logic [5:0] [15:0] sram_write_loops_ranges,
    input logic [15:0] sram_write_sched_gen_sched_addr_gen_starting_addr,
    input logic [5:0] [15:0] sram_write_sched_gen_sched_addr_gen_strides,

    num_iters = 300
    for i in range(num_iters):
        data_in = []
        for j in range(fw_int):
            data_in.append(rand.randint(0, 2**data_width - 1))

        for j in range(fw_int):
            setattr(tester.circuit, f"mem_data_in_bank_{j}", data_in[j])

        addr = rand.randint(0, 2**address_width - 1)
        tester.circuit.mem_addr_in_bank = addr

        cen = rand.randint(0, 1)
        tester.circuit.mem_cen_in_bank = cen

        wen = rand.randint(0, 1)
        tester.circuit.mem_wen_in_bank = wen

        wtsel = 1
        tester.circuit.wtsel = wtsel

        rtsel = 1
        tester.circuit.rtsel = 1

        model_data_out, model_valid_data = \
            model_sram_wrapper.interact(data_in, addr, cen, wen, wtsel, rtsel)

        tester.eval()

        tester.circuit.valid_data.expect(model_valid_data)
        if model_valid_data:
            for j in range(fw_int):
                getattr(tester.circuit, f"mem_data_out_bank_0_{j}").expect(model_data_out[j])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_sram_formal()
