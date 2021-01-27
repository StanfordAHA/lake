from lake.models.sram_wrapper_model import SRAMWrapperModel
from lake.modules.sram_wrapper import SRAMWrapper
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


def test_sram_wrapper(use_sram_stub=True,
                      sram_name="default_name",
                      data_width=16,
                      fw_int=4,
                      mem_depth=512,
                      mem_input_ports=1,
                      mem_output_ports=1,
                      address_width=9,
                      bank_num=1,
                      num_tiles=1,
                      enable_chain_input=0,
                      enable_chain_output=0,
                      chain_idx_input=0,
                      chain_idx_output=0):

    model_sram_wrapper = SRAMWrapperModel(use_sram_stub,
                                          sram_name,
                                          data_width,
                                          fw_int,
                                          mem_depth,
                                          mem_input_ports,
                                          mem_output_ports,
                                          address_width,
                                          bank_num,
                                          num_tiles,
                                          enable_chain_input,
                                          enable_chain_output,
                                          chain_idx_input,
                                          chain_idx_output)

    dut = SRAMWrapper(use_sram_stub,
                      sram_name,
                      data_width,
                      fw_int,
                      mem_depth,
                      mem_input_ports,
                      mem_output_ports,
                      address_width,
                      bank_num,
                      num_tiles)

    magma_dut = k.util.to_magma(dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    new_config = {}
    new_config["enable_chain_input"] = 0
    new_config["enable_chain_output"] = 0
    new_config["chain_idx_input"] = 0
    new_config["chain_idx_output"] = 0

    # configuration registers passed through from top level
    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    tester.circuit.clk_en = 1

    rand.seed(0)

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
    test_sram_wrapper(use_sram_stub=True,
                      sram_name="default_name",
                      data_width=16,
                      fw_int=4,
                      mem_depth=512,
                      mem_input_ports=1,
                      mem_output_ports=1,
                      address_width=9,
                      bank_num=1,
                      num_tiles=1,
                      enable_chain_input=0,
                      enable_chain_output=0,
                      chain_idx_input=0,
                      chain_idx_output=0)
