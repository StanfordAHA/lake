from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import pytest
import tempfile
import os

from lake.passes.passes import lift_config_reg, change_sram_port_names
from lake.utils.sram_macro import SRAMMacroInfo
# input and output data
from lake.utils.parse_clkwork_csv import generate_data_lists
# configurations
from lake.utils.parse_clkwork_config import *
from lake.utils.util import get_configs_dict, set_configs_sv
from lake.utils.util import extract_formal_annotation
from lake.utils.util import check_env
from lake.collateral2compiler.top_lake import TopLake
from lake.collateral2compiler.mem_port import MemPort

def base_lake_tester(config_path,
                     in_file_name,
                     out_file_name,
                     in_ports,
                     out_ports,
                     lt_dut,
                     stencil_valid=False):

    magma_dut = kts.util.to_magma(lt_dut,
#                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    configs = []
    configs_list = []
    return lt_dut, configs, configs_list, magma_dut, tester


def gen_test_lake(config_path,
                  stream_path,
                  lt_dut,
                  in_file_name="input",
                  out_file_name="output",
                  in_ports=2,
                  out_ports=2):

    lt_dut, configs, configs_list, magma_dut, tester = \
        base_lake_tester(config_path,
                         in_file_name,
                         out_file_name,
                         in_ports,
                         out_ports, 
                         lt_dut)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1
    tester.eval()

    # args are input ports, output ports
    in_data, out_data, valids = generate_data_lists(stream_path, in_ports, out_ports)
    #tester.circuit.agg1_forloop_dimensionality =
    #tester.circuit.agg1_forloop_ranges =
    #tester.circuit.agg1write_addr_gen_starting_addr =
    #tester.circuit.agg1write_addr_gen_strides =
    #tester.circuit.agg1write_sched_gen_sched_addr_gen_starting_addr =
    #tester.circuit.agg1write_sched_gen_sched_addr_gen_strides =
    tester.circuit.agg_agg1_sram_edge_forloop_dimensionality = 2
    tester.circuit.agg_agg1_sram_edge_forloop_ranges_0 = 4
    tester.circuit.agg_agg1_sram_edge_forloop_ranges_1 = 16
    tester.circuit.agg_agg1_sram_edge_read_addr_gen_starting_addr = 0
    # all 0
    tester.circuit.agg_agg1_sram_edge_read_addr_gen_strides_0 = 0

    tester.circuit.agg_agg1_sram_edge_sched_gen_sched_addr_gen_starting_addr = 4
    tester.circuit.agg_agg1_sram_edge_sched_gen_sched_addr_gen_strides_0 = 4
    tester.circuit.agg_agg1_sram_edge_sched_gen_sched_addr_gen_strides_1 = 16

    tester.circuit.agg_agg1_sram_edge_write_addr_gen_starting_addr = 0
    tester.circuit.agg_agg1_sram_edge_write_addr_gen_strides_0 = 1
    tester.circuit.agg_agg1_sram_edge_write_addr_gen_strides_1 = 4

    tester.circuit.agg_forloop_dimensionality = 3
    tester.circuit.agg_forloop_ranges_0 = 4 
    tester.circuit.agg_forloop_ranges_1 = 4
    tester.circuit.agg_forloop_ranges_2 = 16

    tester.circuit.aggwrite_addr_gen_starting_addr = 0
    tester.circuit.aggwrite_addr_gen_strides_0 = 1 

    tester.circuit.aggwrite_sched_gen_sched_addr_gen_starting_addr = 0
    tester.circuit.aggwrite_sched_gen_sched_addr_gen_strides_0 = 1
    tester.circuit.aggwrite_sched_gen_sched_addr_gen_strides_1 = 4
    tester.circuit.aggwrite_sched_gen_sched_addr_gen_strides_2 = 16

    tester.circuit.sram_tb_tb1_edge_forloop_dimensionality = 3
    tester.circuit.sram_tb_tb1_edge_forloop_ranges_0 = 2
    tester.circuit.sram_tb_tb1_edge_forloop_ranges_1 = 4
    tester.circuit.sram_tb_tb1_edge_forloop_ranges_2 = 14

    tester.circuit.sram_tb_tb1_edge_read_addr_gen_starting_addr = 0
    tester.circuit.sram_tb_tb1_edge_read_addr_gen_strides_0 = 4
    tester.circuit.sram_tb_tb1_edge_read_addr_gen_strides_1 = 1
    tester.circuit.sram_tb_tb1_edge_read_addr_gen_strides_2 = 4

    tester.circuit.sram_tb_tb1_edge_sched_gen_sched_addr_gen_starting_addr = 27
    tester.circuit.sram_tb_tb1_edge_sched_gen_sched_addr_gen_strides_0 = 2
    tester.circuit.sram_tb_tb1_edge_sched_gen_sched_addr_gen_strides_1 = 4
    tester.circuit.sram_tb_tb1_edge_sched_gen_sched_addr_gen_strides_2 = 16

    tester.circuit.sram_tb_tb1_edge_write_addr_gen_starting_addr = 0
    tester.circuit.sram_tb_tb1_edge_write_addr_gen_strides_0 = 8
    tester.circuit.sram_tb_tb1_edge_write_addr_gen_strides_1 = 1
    tester.circuit.sram_tb_tb1_edge_write_addr_gen_strides_2 = 4

    tester.circuit.tb1_forloop_dimensionality = 3
    tester.circuit.tb1_forloop_ranges_0 = 4
    tester.circuit.tb1_forloop_ranges_1 = 4
    tester.circuit.tb1_forloop_ranges_2 = 14

    tester.circuit.tb1read_sched_gen_sched_addr_gen_starting_addr = 32
    tester.circuit.tb1read_sched_gen_sched_addr_gen_strides_0 = 1
    tester.circuit.tb1read_sched_gen_sched_addr_gen_strides_1 = 4
    tester.circuit.tb1read_sched_gen_sched_addr_gen_strides_2 = 16

    tester.circuit.tb1write_addr_gen_starting_addr = 0
    tester.circuit.tb1write_addr_gen_strides_0 = 8
    tester.circuit.tb1write_addr_gen_strides_1 = 1
    tester.circuit.tb1write_addr_gen_strides_2 = 4

    tester.circuit.tb_forloop_dimensionality = 3
    tester.circuit.tb_forloop_ranges_0 = 4
    tester.circuit.tb_forloop_ranges_1 = 4
    tester.circuit.tb_forloop_ranges_2 = 14

    tester.circuit.tbread_sched_gen_sched_addr_gen_starting_addr = 32
    tester.circuit.tbread_sched_gen_sched_addr_gen_strides_0 = 1
    tester.circuit.tbread_sched_gen_sched_addr_gen_strides_1 = 4
    tester.circuit.tbread_sched_gen_sched_addr_gen_strides_2 = 16

    tester.circuit.tbwrite_addr_gen_starting_addr = 0
    tester.circuit.tbwrite_addr_gen_strides_0 = 8
    tester.circuit.tbwrite_addr_gen_strides_1 = 1
    tester.circuit.tbwrite_addr_gen_strides_2 = 4

    for i in range(len(out_data[0])):
        for j in range(len(in_data)):
            if i < len(in_data[j]):
                setattr(tester.circuit, f"data_in_{j}", in_data[j][i])
        # tester.circuit.data_in_0 = i

        tester.eval()

        #for j in range(len(out_data)):
        #    if i < len(out_data[j]):
        #        if len(valids) != 0 and valids[i] == 1:
        #            getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])
        #        if len(valids) == 0:
        #            getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir="hw"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal", "--trace"])


def test_conv_3_3(lt_dut):
    lc, ls = check_env()
    config_path = lc + "conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf"
    stream_path = ls + "conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf_0_top_SMT.csv"

    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  lt_dut=lt_dut)


@pytest.mark.skip
def test_gaussian():
    lc, ls = check_env()
    config_path = lc + "gaussian/hw_input_stencil_op_hcompute_hw_input_stencil_2_to_hw_input_stencil_op_hcompute_blur_unnormalized_stencil_1_11_ubuf"
    stream_path = ls + "buf.csv"
    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  in_file_name="op_hcompute_hw_input_stencil",
                  out_file_name="op_hcompute_blur_unnormalized_stencil_1")


@pytest.mark.skip
def test_cascade_1():
    lc, ls = check_env()
    config_path = lc + "cascade/buf1_input_10_to_buf1_conv_15_ubuf"
    stream_path = ls + "buf1.csv"
    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  out_file_name="conv")


@pytest.mark.skip
def test_cascade_2():
    lc, ls = check_env()
    config_path = lc + "cascade/buf2_conv_12_to_buf2_output_3_ubuf"
    stream_path = ls + "buf2.csv"
    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  in_file_name="conv")


@pytest.mark.skip
def test_harris_1():
    lc, ls = check_env()
    config_path = lc + "harris/padded16_global_wrapper_stencil_op_hcompute_padded16_global_wrapper_stencil_0_to_padded16_global_wrapper_stencil_op_hcompute_grad_x_stencil_49_ubuf"
    stream_path = ls + "buf.csv"
    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  in_file_name="op_hcompute_padded16_global_wrapper_stencil",
                  out_file_name="op_hcompute_grad_x_stencil")


@pytest.mark.skip
def test_harris_2():
    lc, ls = check_env()
    config_path = lc + "harris/cim_stencil_op_hcompute_cim_stencil_55_to_cim_stencil_op_hcompute_cim_output_stencil_63_ubuf"
    stream_path = ls + "buf.csv"
    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  in_file_name="op_hcompute_cim_stencil",
                  out_file_name="op_hcompute_cim_output_stencil")


@pytest.mark.skip
def test_harris_3():
    lc, ls = check_env()
    config_path = lc + "harris/lxx_stencil_op_hcompute_lxx_stencil_7_to_lxx_stencil_op_hcompute_lgxx_stencil_1_32_ubuf"
    stream_path = ls + "buf.csv"
    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  in_file_name="op_hcompute_lxx_stencil",
                  out_file_name="op_hcompute_lgxx_stencil_1")


@pytest.mark.skip
def test_harris_4():
    lc, ls = check_env()
    config_path = lc + "harris/lxy_stencil_op_hcompute_lxy_stencil_4_to_lxy_stencil_op_hcompute_lgxy_stencil_1_22_ubuf"
    stream_path = ls + "buf.csv"
    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  in_file_name="op_hcompute_lxy_stencil",
                  out_file_name="op_hcompute_lgxy_stencil_1")


@pytest.mark.skip
def test_harris_5():
    lc, ls = check_env()
    config_path = lc + "harris/lyy_stencil_op_hcompute_lyy_stencil_2_to_lyy_stencil_op_hcompute_lgyy_stencil_1_12_ubuf"
    stream_path = ls + "buf.csv"
    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  in_file_name="op_hcompute_lyy_stencil",
                  out_file_name="op_hcompute_lgyy_stencil_1")

# example of DSL (makes current mem tile with 2 agg,
# wide SRAM, 2 tb

# mem_collateral is part of TopLake for collateral
# to compiler, not exposed to user

# LAKE OBJECT MUST BE FIRST INSTANTIATED
# IMPORTANT: PORTS MUST BE INSTANTIATED BEFORE MEMORIES
# MEMORIES SHOULD BE INSTANTIATED BEFORE EDGES

# word_width, input_ports, output_ports
tile = TopLake(16, 2, 2)

# MemPort attributes are latency, initiation interval
agg_write_port = MemPort(1, 0)
agg_read_port = MemPort(0, 0)

agg_params = {"name": "agg",
              "capacity": 4,
              "word_width": 16,
              "read_port_width": 4,
              "write_port_width": 1}

# add_memory takes in params, W ports, R ports, R/W ports
tile.add_memory(agg_params, [agg_write_port], [agg_read_port])

agg1_write_port = MemPort(1, 0)
agg1_read_port = MemPort(0, 0)
agg1_params = {"name": "agg1",
               "capacity": 4,
               "word_width": 16,
               "read_port_width": 4,
               "write_port_width": 1}

tile.add_memory(agg1_params, [agg1_write_port], [agg1_read_port])

sram_write_read_port = MemPort(1, 0)

sram_params = {"name": "sram",
               "capacity": 512,
               "word_width": 16,
               "read_write_port_width": 4}

tile.add_memory(sram_params, read_write_ports=[sram_write_read_port])

tile.add_edge({"from_signal": "agg",
               "to_signal": "sram",
               # these are defaults, so not specified for further edges
               "dim": 6,
               "max_range": 65535,
               "max_stride": 65535})

tile.add_edge({"from_signal": "agg1",
               "to_signal": "sram"})

tb_write_port = MemPort(1, 0)
tb_read_port = MemPort(0, 0)

tb_params = {"name": "tb",
             "capacity": 8,
             "word_width": 16,
             "read_port_width": 1,
             "write_port_width": 4}

tile.add_memory(tb_params, [tb_write_port], [tb_read_port])

tb1_write_port = MemPort(1, 0)
tb1_read_port = MemPort(0, 0)

tb1_params = {"name": "tb1",
              "capacity": 8,
              "word_width": 16,
              "read_port_width": 1,
              "write_port_width": 4}

tile.add_memory(tb1_params, [tb1_write_port], [tb1_read_port])

tile.add_edge({"from_signal": "sram",
               "to_signal": "tb"})

tile.add_edge({"from_signal": "sram",
               "to_signal": "tb1"})

# for both compiler collateral and HW generation
hw = tile.construct_lake()

test_conv_3_3(hw)

    # gaussian
    # test_gaussian()

    # cascade_1
    # test_cascade_1()

    # cascade_2
    # test_cascade_2()

    # harris_1
    # test_harris_1()

    # harris_2
    # test_harris_2()

    # harris_3
    # test_harris_3()

    # harris_4
    # test_harris_4()

    # harris_5
    # test_harris_5()
