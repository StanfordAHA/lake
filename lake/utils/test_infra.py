import kratos as kts
import fault
import tempfile
import os
import sys

# input and output data
from lake.utils.parse_clkwork_csv import generate_data_lists
# configurations
from lake.utils.parse_clkwork_config import *
from lake.utils.util import get_configs_dict, set_configs_sv
from lake.utils.util import extract_formal_annotation
from lake.utils.util import check_env
from lake.utils.util import transform_strides_and_ranges


def base_lake_tester(config_path,
                     in_file_name,
                     out_file_name,
                     stencil_valid=False,
                     get_configs_list=False):

    # TODO integrate with JSON from halide apps?
    pass
    # lt_dut, need_config_lift, s, t = get_lake_dut(stencil_valid=stencil_valid)

    # configs = lt_dut.get_static_bitstream(config_path, in_file_name, out_file_name)
    # if get_configs_list:
    #     # prints out list of configs for compiler team
    #     configs_list = set_configs_sv(lt_dut, "configs.sv", get_configs_dict(configs))

    # magma_dut = kts.util.to_magma(lt_dut,
    #                               flatten_array=True,
    #                               check_multiple_driver=False,
    #                               optimize_if=False,
    #                               check_flip_flop_always_ff=False)

    # tester = fault.Tester(magma_dut, magma_dut.clk)

    # if get_configs_list:
    #     return lt_dut, configs, configs_list, magma_dut, tester
    # else:
    #     return lt_dut, configs, magma_dut, tester


def gen_test_lake(config_path,
                  stream_path,
                  in_file_name="input",
                  out_file_name="output",
                  **lake_dut_kwargs):

    lt_dut, configs, magma_dut, tester = \
        base_lake_tester(config_path,
                         in_file_name,
                         out_file_name,
                         **lake_dut_kwargs)

    in_ports = 2
    out_ports = 2

    if "interconnect_input_ports" in lake_dut_kwargs.keys():
        in_ports = lake_dut_kwargs["interconnect_input_ports"]
    if "interconnect_output_ports" in lake_dut_kwargs.keys():
        out_ports = lake_dut_kwargs["interconnect_output_ports"]

    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1
    tester.eval()

    # args are input ports, output ports
    in_data, out_data, valids = generate_data_lists(stream_path, in_ports, out_ports)

    for (f1, f2) in configs:
        setattr(tester.circuit, f1, f2)

    for i in range(len(out_data[0])):
        for j in range(len(in_data)):
            if i < len(in_data[j]):
                setattr(tester.circuit, f"data_in_{j}", in_data[j][i])

        tester.eval()

        for j in range(len(out_data)):
            if i < len(out_data[j]):
                if len(valids) != 0 and valids[i] == 1:
                    getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])


def lake_test_app_args(app):
    if app == "conv_3_3":
        return conv_3_3_args()
    elif app == "separate":
        return separate_args()
    print(f"{app} is not supported.")
    sys.exit()


def conv_3_3_args():
    lc, ls = check_env()
    config_path = lc + "conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf"
    stream_path = ls + "conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf_0_top_SMT.csv"
    in_file_name = "input_"
    out_file_name = "output_2_"
    return config_path, stream_path, in_file_name, out_file_name


# This is a weird custom test path until we can get consistent paths
def separate_args():
    clkwrk_dir = os.getenv("CLOCKWORK_DIR")
    if clkwrk_dir is None:
        raise RuntimeError
    config_path = clkwrk_dir + "/aha_garnet_design_new/conv_3_3/lake_collateral/ub_hw_input_global_wrapper_stencil_op_hcompute_hw_input_global_wrapper_stencil_2_to_hw_input_global_wrapper_stencil_op_hcompute_conv_stencil_1_11"
    stream_path = clkwrk_dir + "/aha_garnet_design/conv_3_3/lake_stream/hw_input_global_wrapper_stencil_op_hcompute_hw_input_global_wrapper_stencil_2_to_hw_input_global_wrapper_stencil_op_hcompute_conv_stencil_1_11_ubuf_0_top_SMT.csv"
    in_file_name = ""
    out_file_name = ""
    return config_path, stream_path, in_file_name, out_file_name
