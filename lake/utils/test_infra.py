from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import tempfile
import os
import sys

from lake.passes.passes import lift_config_reg, change_sram_port_names
from lake.utils.sram_macro import SRAMMacroInfo
# input and output data
from lake.utils.parse_clkwork_csv import generate_data_lists
# configurations
from lake.utils.parse_clkwork_config import *
from lake.utils.util import get_configs_dict, set_configs_sv
from lake.utils.util import extract_formal_annotation
from lake.utils.util import check_env


def base_lake_tester(config_path,
                     in_file_name,
                     out_file_name,
                     in_ports,
                     out_ports,
                     stencil_valid=False):

    lt_dut = LakeTop(interconnect_input_ports=in_ports,
                     interconnect_output_ports=out_ports,
                     stencil_valid=stencil_valid)

    configs = lt_dut.get_static_bitstream(config_path, in_file_name, out_file_name)
    configs_list = set_configs_sv(lt_dut, "configs.sv", get_configs_dict(configs))

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)

    return lt_dut, configs, configs_list, magma_dut, tester


def gen_test_lake(config_path,
                  stream_path,
                  in_file_name="input",
                  out_file_name="output",
                  in_ports=2,
                  out_ports=2):

    lt_dut, configs, configs_list, magma_dut, tester = \
        base_lake_tester(config_path,
                         in_file_name,
                         out_file_name,
                         in_ports,
                         out_ports)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1
    tester.eval()

    # args are input ports, output ports
    in_data, out_data, valids = generate_data_lists(stream_path, in_ports, out_ports)

    config = {}
    '''config["strg_ub_agg_write_addr_gen_0_starting_addr"] = 8
    config["strg_ub_agg_write_addr_gen_0_strides_0"] = 1
    config["strg_ub_agg_write_addr_gen_0_strides_1"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_2"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_port_sel_addr_strides"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_0"] = 1
    config["strg_ub_agg_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_out_port_sel_addr_starting_addr"] = 13
    config["strg_ub_agg_read_addr_gen_1_starting_addr"] = 1
    config["strg_ub_agg_write_addr_gen_1_starting_addr"] = 4
    config["strg_ub_loops_in2buf_0_ranges_0"] = 254
    config["strg_ub_loops_in2buf_0_ranges_1"] = 0
    config["strg_ub_loops_in2buf_0_ranges_2"] = 0
    config["strg_ub_loops_in2buf_0_ranges_3"] = 0
    config["strg_ub_loops_in2buf_0_ranges_4"] = 0
    config["strg_ub_loops_in2buf_0_ranges_5"] = 0
    config["strg_ub_loops_in2buf_autovec_write_dimensionality"] = 1
    config["strg_ub_loops_in2buf_1_dimensionality"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr"] = 65492
    config["strg_ub_input_sched_gen_sched_addr_gen_starting_addr"] = 4
    config["strg_ub_out_port_sel_addr_strides_0"] = 0
    config["strg_ub_out_port_sel_addr_strides_1"] = 0
    config["strg_ub_out_port_sel_addr_strides_2"] = 0
    config["strg_ub_out_port_sel_addr_strides_3"] = 0
    config["strg_ub_out_port_sel_addr_strides_4"] = 0
    config["strg_ub_out_port_sel_addr_strides_5"] = 0
    config["strg_ub_loops_in2buf_0_dimensionality"] = 1
    config["strg_ub_loops_in2buf_1_ranges_0"] = 0
    config["strg_ub_loops_in2buf_1_ranges_1"] = 0
    config["strg_ub_loops_in2buf_1_ranges_2"] = 0
    config["strg_ub_loops_in2buf_1_ranges_3"] = 0
    config["strg_ub_loops_in2buf_1_ranges_4"] = 0
    config["strg_ub_loops_in2buf_1_ranges_5"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_0"] = 4
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_1"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_2"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_3"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_4"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_5"] = 0
    config["strg_ub_agg_read_addr_gen_0_starting_addr"] = 14
    config["strg_ub_agg_write_addr_gen_1_strides_0"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_1"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_2"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_0"] = 63
    config["strg_ub_loops_in2buf_autovec_write_ranges_1"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_2"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_3"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_4"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_5"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_port_sel_addr_starting_addr"] = 0'''
    '''config["strg_ub_agg_write_addr_gen_0_starting_addr"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_0"] = 1
    config["strg_ub_agg_write_addr_gen_0_strides_1"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_2"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_port_sel_addr_strides"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_0"] = 1
    config["strg_ub_agg_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_out_port_sel_addr_starting_addr"] = 9
    config["strg_ub_agg_read_addr_gen_1_starting_addr"] = 5
    config["strg_ub_agg_write_addr_gen_1_starting_addr"] = 12
    config["strg_ub_loops_in2buf_0_ranges_0"] = 254
    config["strg_ub_loops_in2buf_0_ranges_1"] = 0
    config["strg_ub_loops_in2buf_0_ranges_2"] = 0
    config["strg_ub_loops_in2buf_0_ranges_3"] = 0
    config["strg_ub_loops_in2buf_0_ranges_4"] = 0
    config["strg_ub_loops_in2buf_0_ranges_5"] = 0
    config["strg_ub_loops_in2buf_autovec_write_dimensionality"] = 1
    config["strg_ub_loops_in2buf_1_dimensionality"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr"] = 65484
    config["strg_ub_input_sched_gen_sched_addr_gen_starting_addr"] = 4
    config["strg_ub_out_port_sel_addr_strides_0"] = 0
    config["strg_ub_out_port_sel_addr_strides_1"] = 0
    config["strg_ub_out_port_sel_addr_strides_2"] = 0
    config["strg_ub_out_port_sel_addr_strides_3"] = 0
    config["strg_ub_out_port_sel_addr_strides_4"] = 0
    config["strg_ub_out_port_sel_addr_strides_5"] = 0
    config["strg_ub_loops_in2buf_0_dimensionality"] = 1
    config["strg_ub_loops_in2buf_1_ranges_0"] = 257
    config["strg_ub_loops_in2buf_1_ranges_1"] = 257
    config["strg_ub_loops_in2buf_1_ranges_2"] = 257
    config["strg_ub_loops_in2buf_1_ranges_3"] = 257
    config["strg_ub_loops_in2buf_1_ranges_4"] = 257
    config["strg_ub_loops_in2buf_1_ranges_5"] = 257
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_0"] = 4
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_1"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_2"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_3"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_4"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_5"] = 0
    config["strg_ub_agg_read_addr_gen_0_starting_addr"] = 14
    config["strg_ub_agg_write_addr_gen_1_strides_0"] = 15
    config["strg_ub_agg_write_addr_gen_1_strides_1"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_2"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_4"] = 15
    config["strg_ub_agg_write_addr_gen_1_strides_5"] = 15
    config["strg_ub_loops_in2buf_autovec_write_ranges_0"] = 63
    config["strg_ub_loops_in2buf_autovec_write_ranges_1"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_2"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_3"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_4"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_5"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0"] = 65535
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4"] = 65535
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5"] = 65535
    config["strg_ub_port_sel_addr_starting_addr"] = 0'''

    '''config["strg_ub_agg_write_addr_gen_0_starting_addr"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_0"] = 1
    config["strg_ub_agg_write_addr_gen_0_strides_1"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_2"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_agg_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_agg_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_port_sel_addr_strides"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_0"] = 1
    config["strg_ub_agg_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_agg_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_out_port_sel_addr_starting_addr"] = 11
    config["strg_ub_agg_read_addr_gen_1_starting_addr"] = 1
    config["strg_ub_agg_write_addr_gen_1_starting_addr"] = 0
    config["strg_ub_loops_in2buf_0_ranges_0"] = 254
    config["strg_ub_loops_in2buf_0_ranges_1"] = 0
    config["strg_ub_loops_in2buf_0_ranges_2"] = 0
    config["strg_ub_loops_in2buf_0_ranges_3"] = 0
    config["strg_ub_loops_in2buf_0_ranges_4"] = 0
    config["strg_ub_loops_in2buf_0_ranges_5"] = 0
    config["strg_ub_loops_in2buf_autovec_write_dimensionality"] = 1
    config["strg_ub_loops_in2buf_1_dimensionality"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr"] = 65520
    config["strg_ub_input_sched_gen_sched_addr_gen_starting_addr"] = 4
    config["strg_ub_out_port_sel_addr_strides_0"] = 0
    config["strg_ub_out_port_sel_addr_strides_1"] = 0
    config["strg_ub_out_port_sel_addr_strides_2"] = 0
    config["strg_ub_out_port_sel_addr_strides_3"] = 0
    config["strg_ub_out_port_sel_addr_strides_4"] = 0
    config["strg_ub_out_port_sel_addr_strides_5"] = 0
    config["strg_ub_loops_in2buf_0_dimensionality"] = 1
    config["strg_ub_loops_in2buf_1_ranges_0"] = 0
    config["strg_ub_loops_in2buf_1_ranges_1"] = 0
    config["strg_ub_loops_in2buf_1_ranges_2"] = 0
    config["strg_ub_loops_in2buf_1_ranges_3"] = 0
    config["strg_ub_loops_in2buf_1_ranges_4"] = 0
    config["strg_ub_loops_in2buf_1_ranges_5"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_0"] = 4
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_1"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_2"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_3"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_4"] = 0
    config["strg_ub_input_sched_gen_sched_addr_gen_strides_5"] = 0
    config["strg_ub_agg_read_addr_gen_0_starting_addr"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_0"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_1"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_2"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_agg_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_0"] = 63
    config["strg_ub_loops_in2buf_autovec_write_ranges_1"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_2"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_3"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_4"] = 0
    config["strg_ub_loops_in2buf_autovec_write_ranges_5"] = 0
    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_port_sel_addr_starting_addr"] = 0'''

    config["tile_en"] = 1
    config["clk_en"] = 1
    '''config["strg_ub_agg_write_addr_gen_0_starting_addr"] = 0

    config["strg_ub_agg_write_addr_gen_0_strides_0"] = 1

    config["strg_ub_agg_write_addr_gen_0_strides_1"] = 0

    config["strg_ub_agg_write_addr_gen_0_strides_2"] = 0

    config["strg_ub_agg_write_addr_gen_0_strides_3"] = 0

    config["strg_ub_agg_write_addr_gen_0_strides_4"] = 0

    config["strg_ub_agg_write_addr_gen_0_strides_5"] = 0

    config["strg_ub_agg_read_addr_gen_1_strides_0"] = 0

    config["strg_ub_agg_read_addr_gen_1_strides_1"] = 0

    config["strg_ub_agg_read_addr_gen_1_strides_2"] = 0

    config["strg_ub_agg_read_addr_gen_1_strides_3"] = 0

    config["strg_ub_agg_read_addr_gen_1_strides_4"] = 0

    config["strg_ub_agg_read_addr_gen_1_strides_5"] = 0

    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_0"] = 1

    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_1"] = 0

    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_2"] = 0

    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_3"] = 0

    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_4"] = 0

    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_5"] = 0

    config["strg_ub_port_sel_addr_strides"] = 0

    config["strg_ub_agg_read_addr_gen_0_strides_0"] = 1

    config["strg_ub_agg_read_addr_gen_0_strides_1"] = 0

    config["strg_ub_agg_read_addr_gen_0_strides_2"] = 0

    config["strg_ub_agg_read_addr_gen_0_strides_3"] = 0

    config["strg_ub_agg_read_addr_gen_0_strides_4"] = 0

    config["strg_ub_agg_read_addr_gen_0_strides_5"] = 0

    config["strg_ub_out_port_sel_addr_starting_addr"] = 8

    config["strg_ub_agg_read_addr_gen_1_starting_addr"] = 4

    config["strg_ub_agg_write_addr_gen_1_starting_addr"] = 0

    config["strg_ub_loops_in2buf_0_ranges_0"] = 254

    config["strg_ub_loops_in2buf_0_ranges_1"] = 0

    config["strg_ub_loops_in2buf_0_ranges_2"] = 0

    config["strg_ub_loops_in2buf_0_ranges_3"] = 0

    config["strg_ub_loops_in2buf_0_ranges_4"] = 0

    config["strg_ub_loops_in2buf_0_ranges_5"] = 0

    config["strg_ub_loops_in2buf_autovec_write_dimensionality"] = 1

    config["strg_ub_loops_in2buf_1_dimensionality"] = 0

    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_starting_addr"] = 65520

    config["strg_ub_input_sched_gen_sched_addr_gen_starting_addr"] = 4

    config["strg_ub_out_port_sel_addr_strides_0"] = 0

    config["strg_ub_out_port_sel_addr_strides_1"] = 0

    config["strg_ub_out_port_sel_addr_strides_2"] = 0

    config["strg_ub_out_port_sel_addr_strides_3"] = 0

    config["strg_ub_out_port_sel_addr_strides_4"] = 0

    config["strg_ub_out_port_sel_addr_strides_5"] = 0

    config["strg_ub_loops_in2buf_0_dimensionality"] = 1

    config["strg_ub_loops_in2buf_1_ranges_0"] = 0

    config["strg_ub_loops_in2buf_1_ranges_1"] = 0

    config["strg_ub_loops_in2buf_1_ranges_2"] = 0

    config["strg_ub_loops_in2buf_1_ranges_3"] = 0

    config["strg_ub_loops_in2buf_1_ranges_4"] = 255

    config["strg_ub_loops_in2buf_1_ranges_5"] = 255

    config["strg_ub_input_sched_gen_sched_addr_gen_strides_0"] = 4

    config["strg_ub_input_sched_gen_sched_addr_gen_strides_1"] = 0

    config["strg_ub_input_sched_gen_sched_addr_gen_strides_2"] = 0

    config["strg_ub_input_sched_gen_sched_addr_gen_strides_3"] = 0

    config["strg_ub_input_sched_gen_sched_addr_gen_strides_4"] = 0

    config["strg_ub_input_sched_gen_sched_addr_gen_strides_5"] = 0

    config["strg_ub_agg_read_addr_gen_0_starting_addr"] = 0

    config["strg_ub_agg_write_addr_gen_1_strides_0"] = 15

    config["strg_ub_agg_write_addr_gen_1_strides_1"] = 0

    config["strg_ub_agg_write_addr_gen_1_strides_2"] = 0

    config["strg_ub_agg_write_addr_gen_1_strides_3"] = 0

    config["strg_ub_agg_write_addr_gen_1_strides_4"] = 15

    config["strg_ub_agg_write_addr_gen_1_strides_5"] = 15

    config["strg_ub_loops_in2buf_autovec_write_ranges_0"] = 63

    config["strg_ub_loops_in2buf_autovec_write_ranges_1"] = 0

    config["strg_ub_loops_in2buf_autovec_write_ranges_2"] = 0

    config["strg_ub_loops_in2buf_autovec_write_ranges_3"] = 0

    config["strg_ub_loops_in2buf_autovec_write_ranges_4"] = 0

    config["strg_ub_loops_in2buf_autovec_write_ranges_5"] = 0

    config["strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr"] = 0

    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_0"] = 65535

    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_1"] = 0

    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_2"] = 0

    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_3"] = 0

    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_4"] = 65535

    config["strg_ub_agg_write_sched_gen_1_sched_addr_gen_strides_5"] = 65535

    config["strg_ub_port_sel_addr_starting_addr"] = 0'''

    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 32768
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 1
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 4
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_starting_addr"] = 5
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 1
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_0"] = 4
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_1"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_2"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_3"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_4"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_0"] = 7
    config["strg_ub_loops_buf2out_autovec_read_ranges_1"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_2"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_3"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_4"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_5"] = 0
    config["strg_ub_loops_buf2out_autovec_read_dimensionality"] = 1
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 8
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 0'''

    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 32768
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 15
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 2
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 15
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_starting_addr"] = 5
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 1
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_0"] = 4
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_1"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_2"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_3"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_4"] = 0
    config["strg_ub_output_sched_gen_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_0"] = 6
    config["strg_ub_loops_buf2out_autovec_read_ranges_1"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_2"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_3"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_4"] = 0
    config["strg_ub_loops_buf2out_autovec_read_ranges_5"] = 0
    config["strg_ub_loops_buf2out_autovec_read_dimensionality"] = 1
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 10
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 2'''

    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 32768
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 3
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 8
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 7
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 1
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 11
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0'''

    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 32768
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 2
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 4
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 14
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 2
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 11
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0'''

    # works no flipping
    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 32768
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 2
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 7
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 9
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 1
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 11
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0'''

    # also no flipping
    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 32768
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 2
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 7
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 9
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 1
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 11
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0'''

    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 32768
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 2
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 7
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 9
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 1
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 11
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0'''

    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 32768
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 2
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 4
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 14
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 2
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 11
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0'''

    # doesn't work at all
    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 13
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 10
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 11
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 11
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 10
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 9
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 9
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 9
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 5
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 1
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 1
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 1
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 7
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 12
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 5
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0'''

    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 13
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 1
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 12
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 7
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 1
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 1
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 13
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 5
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0'''

    # flipped
    '''config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 13
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 1
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 12
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 12
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 1
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 13
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 5
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0'''

    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 6
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_0"] = 13
    config["strg_ub_tb_write_addr_gen_0_strides_1"] = 7
    config["strg_ub_tb_write_addr_gen_0_strides_2"] = 2
    config["strg_ub_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_0_strides_4"] = 1
    config["strg_ub_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_write_addr_gen_0_starting_addr"] = 7
    config["strg_ub_tb_read_addr_gen_0_starting_addr"] = 3
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_0"] = 2
    config["strg_ub_tb_write_addr_gen_1_strides_1"] = 14
    config["strg_ub_tb_write_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_loops_buf2out_read_1_dimensionality"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_0"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_0"] = 1
    config["strg_ub_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_write_addr_gen_1_starting_addr"] = 13
    config["strg_ub_loops_buf2out_read_0_ranges_0"] = 92
    config["strg_ub_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_loops_buf2out_read_0_ranges_5"] = 0

    for (f1, f2) in configs:
        setattr(tester.circuit, f1, f2)

    for key in config.keys():
        setattr(tester.circuit, key, config[key])

    for i in range(len(out_data[0])):
        for j in range(len(in_data)):
            if i < len(in_data[j]):
                setattr(tester.circuit, f"data_in_{j}", in_data[j][i])
        # tester.circuit.data_in_0 = i

        tester.eval()

        for j in range(len(out_data)):
#            if i < len(out_data[j]):
#                if len(valids) != 0 and valids[i] == 1:
#                    getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])
                if len(valids) == 0:
                    getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir="nestan_tb1"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal", "--trace"])


def lake_test_app_args(app):
    if app == "conv_3_3":
        return conv_3_3_args()

    print(f"{app} is not supported.")
    sys.exit()


def conv_3_3_args():
    lc, ls = check_env()
    config_path = "/nobackupkiwi/skavya/lake/clockwork/lake_controllers/identity_stream/lake_collateral/buf" #"/nobackupkiwi/skavya/configs_id"#/joeyliu/aha/poly/clockwork/lake_controllers/identity_stream"#lc + "conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf"
    stream_path = ls + "conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf_0_top_SMT.csv"
    in_file_name = ""#"input_"
    out_file_name = ""#output_2_"
    return config_path, stream_path, in_file_name, out_file_name
