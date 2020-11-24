from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.passes.passes import lift_config_reg, change_sram_port_names
from lake.utils.sram_macro import SRAMMacroInfo


def test_m1():
    #  module two_dp_srams (
    #   input logic chain_chain_en,
    #   input logic [1:0] [15:0] chain_data_in,
    #   input logic clk,
    #   input logic clk_en,
    #   input logic [7:0] config_addr_in,
    #   input logic [31:0] config_data_in,
    #   input logic [7:0] config_en,
    #   input logic config_read,
    #   input logic config_write,
    #   input logic [1:0] [15:0] data_in,
    #   input logic flush,
    #   input logic [1:0] mode,
    #   input logic [1:0] [15:0] raddr,
    #   input logic [1:0] ren_in,
    #   input logic rst_n,
    #   input logic tile_en,
    #   input logic [1:0] [15:0] waddr,
    #   input logic [1:0] wen_in,
    #   output logic [7:0] [31:0] config_data_out,
    #   output logic [1:0] [15:0] data_out,
    #   output logic [1:0] valid_out
    # );

    new_config = {}

    # chaining
    new_config["chain_chain_en"] = 0
    new_config["mode"] = 0
    new_config["tile_en"] = 1

    tsmc_info = SRAMMacroInfo("tsmc_name")
    use_sram_stub = False

    banks = 2
    data_width = 16
    fifo_mode = False
    mem_width = 16
    mem_depth = 1024
    # Dual port or not...
    rw_same_cycle = True
    input_ports = 2
    output_ports = 2
    gen_addr = False

    lake_name = "two_dp_srams"
    stcl_valid = False

    lt_dut = LakeTop(data_width=data_width,
                     banks=banks,
                     mem_width=mem_width,
                     mem_depth=mem_depth,
                     sram_macro_info=tsmc_info,
                     use_sram_stub=use_sram_stub,
                     fifo_mode=fifo_mode,
                     interconnect_input_ports=input_ports,
                     interconnect_output_ports=output_ports,
                     gen_addr=gen_addr,
                     add_clk_enable=True,
                     add_flush=True,
                     stencil_valid=stcl_valid,
                     name=lake_name)

    # Run the config reg lift
    # lift_config_reg(lt_dut.internal_generator)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    tester.zero_inputs()
    tester.circuit.clk_en = 1
    tester.eval()
    ###
    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    for i in range(100):
        # Rand data
        tester.circuit.waddr_0 = i
        tester.circuit.waddr_1 = i
        tester.circuit.data_in_0 = rand.randint(0, 2**16 - 1)
        tester.circuit.data_in_1 = rand.randint(0, 2**16 - 1)
        tester.circuit.wen_in[0] = 1
        tester.circuit.wen_in[1] = 1
        if i > 50:
            tester.circuit.raddr_0 = i - 50
            tester.circuit.raddr_1 = i - 50
            tester.circuit.ren_in[0] = 1
            tester.circuit.ren_in[1] = 1

        tester.eval()

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    test_m1()
