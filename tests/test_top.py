from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile


def top_test(data_width=16,
             mem_width=64,
             mem_depth=512,
             banks=2,
             input_iterator_support=6,
             output_iterator_support=6,
             interconnect_input_ports=1,
             interconnect_output_ports=3,
             mem_input_ports=1,
             mem_output_ports=1,
             use_sram_stub=1,
             agg_height=8,
             transpose_height=8,
             max_agg_schedule=64,
             input_max_port_sched=64,
             output_max_port_sched=64,
             align_input=1,
             max_line_length=256,
             tb_height=1,
             tb_range_max=256,
             tb_sched_max=64,
             num_tb=1):

    new_config = {}

    # Agg buffer
    new_config["agg_in_period"] = 1  # I don't actually know
    new_config["agg_in_sched_0"] = 0
    new_config["agg_out_period"] = 1
    new_config["agg_out_sched"] = 0

    # Kavya
    new_config["indices_tba_0_0"] = 0
    new_config["indices_tba_0_1"] = 1
    new_config["indices_tba_0_2"] = 2
    new_config["range_inner_tba_0"] = 3
    new_config["range_outer_tba_0"] = 62
    new_config["stride_tba_0"] = 1

    new_config["indices_tba_1_0"] = 0
    new_config["indices_tba_1_1"] = 1
    new_config["indices_tba_1_2"] = 2
    new_config["range_inner_tba_1"] = 3
    new_config["range_outer_tba_1"] = 62
    new_config["stride_tba_1"] = 1

    new_config["indices_tba_2_0"] = 0
    new_config["indices_tba_2_1"] = 1
    new_config["indices_tba_2_2"] = 2
    new_config["range_inner_tba_2"] = 3
    new_config["range_outer_tba_2"] = 62
    new_config["stride_tba_2"] = 1
    # new_config["tb_index_for_data"] = 0

    # Aligner
    new_config["line_length"] = 64

    # Input addr ctrl
    new_config["dimensionality_i"] = 1
    new_config["range_i_0_0"] = 2048
    new_config["starting_addr_i_0"] = 0
    new_config["stride_i_0_0"] = 1

    new_config["dimensionality_o_0"] = 2
    new_config["dimensionality_o_1"] = 2
    new_config["dimensionality_o_2"] = 2
    new_config["range_o_0_0"] = 16
    new_config["range_o_0_1"] = 64
    new_config["range_o_1_0"] = 16
    new_config["range_o_1_1"] = 64
    new_config["range_o_2_0"] = 16
    new_config["range_o_2_1"] = 64
    new_config["starting_addr_o_0"] = 0
    new_config["starting_addr_o_1"] = 16
    new_config["starting_addr_o_2"] = 32
    new_config["stride_o_0_0"] = 1
    new_config["stride_o_0_1"] = 16
    new_config["stride_o_1_0"] = 1
    new_config["stride_o_1_1"] = 16
    new_config["stride_o_2_0"] = 1
    new_config["stride_o_2_1"] = 16

    ### DUT
    lt_dut = LakeTop(data_width=data_width,
                     mem_width=mem_width,
                     mem_depth=mem_depth,
                     banks=banks,
                     input_iterator_support=input_iterator_support,
                     output_iterator_support=output_iterator_support,
                     interconnect_input_ports=interconnect_input_ports,
                     interconnect_output_ports=interconnect_output_ports,
                     mem_input_ports=mem_input_ports,
                     mem_output_ports=mem_output_ports,
                     use_sram_stub=use_sram_stub,
                     agg_height=agg_height,
                     max_agg_schedule=max_agg_schedule,
                     input_max_port_sched=input_max_port_sched,
                     output_max_port_sched=output_max_port_sched,
                     align_input=align_input,
                     max_line_length=max_line_length,
                     tb_height=tb_height,
                     tb_range_max=tb_range_max,
                     tb_sched_max=tb_sched_max,
                     num_tb=num_tb)

    magma_dut = kts.util.to_magma(lt_dut, flatten_array=True, check_multiple_driver=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###
    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    for i in range(10):
        tester.step(2)

    # Configure the thing
    tester.circuit.arb_wen_in = 1
    tester.circuit.arb_ren_in = 0
    tester.circuit.data_in = 0
    tester.circuit.valid_in = 1

    for i in range(200):
        tester.circuit.data_in += 1
        tester.step(2)

    tester.step(1)
    tester.circuit.arb_ren_in = 1
    tester.circuit.data_in += 1
    tester.eval()
    tester.step(1)

    for i in range(100):
        tester.step(2)
        tester.circuit.data_in += 1

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "./top_dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    top_test()
