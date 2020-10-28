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
from lake.collateral2compiler.dsl import * 

def base_lake_tester(config_path,
                     in_file_name,
                     out_file_name,
                     in_ports,
                     out_ports,
                     lt_dut,
                     stencil_valid=False):

    magma_dut = kts.util.to_magma(lt_dut,
                                  # flatten_array=True,
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


hw = tile.test_magma_lake()
test_conv_3_3(hw)
