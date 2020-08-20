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
from lake.utils.util import get_configs_dict, set_configs_sv, extract_formal_annotation


def test_lake(config_path,
              stream_path,
              in_file_name="input",
              out_file_name="output",
              in_ports=2,
              out_ports=2):

    lt_dut = LakeTop(interconnect_input_ports=in_ports,
                     interconnect_output_ports=out_ports)

    configs = get_static_bitstream(config_path, in_file_name, out_file_name)
    set_configs_sv(lt_dut, "configs.sv", get_configs_dict(configs))

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1
    tester.eval()

    # args are input ports, output ports
    in_data, out_data, valids = generate_data_lists(stream_path, in_ports, out_ports)
    print(valids)
    for (f1, f2) in configs:
        setattr(tester.circuit, f1, f2)

    for i in range(len(out_data[0])):
    # for i in range(1000):
        for j in range(len(in_data)):
            if i < len(in_data[j]):
                setattr(tester.circuit, f"data_in_{j}", in_data[j][i])
        # tester.circuit.data_in_0 = i

        tester.eval()

#        for j in range(len(out_data)):
#            if i < len(out_data[j]) and valids[i] == 1:
#                getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":

    lake_controller_path = os.getenv("LAKE_CONTROLLERS")
    lake_stream_path = os.getenv("LAKE_STREAM")

    assert lake_controller_path is not None and lake_stream_path is not None,\
        f"Please check env vars:\nLAKE_CONTROLLERS: {lake_controller_path}\nLAKE_STREAM: {lake_stream_path}"

    # conv_3_3
    # config_path = lake_controller_path + "conv_3_3_new"
    # stream_path = lake_stream_path + "buf.csv"
    # test_lake(config_path, stream_path)

    # cascade_1
    config_path = lake_controller_path + "cascade/buf1_input_10_to_buf1_conv_15_ubuf"
    stream_path = lake_stream_path + "buf1.csv"
    test_lake(config_path, stream_path, out_file_name="conv")

    # cascade_2
    # config_path = lake_controller_path + "cascade/buf2_conv_12_to_buf2_output_3_ubuf"
    # stream_path = lake_stream_path + "buf2.csv"
    # test_lake(config_path, stream_path, in_file_name="conv")

    # harris_1
    # config_path = lake_controller_path + "harris/padded16_global_wrapper_stencil_op_hcompute_padded16_global_wrapper_stencil_0_to_padded16_global_wrapper_stencil_op_hcompute_grad_x_stencil_49_ubuf"
    # stream_path = lake_stream_path + "buf.csv"
    # test_lake(config_path,
    #           stream_path,
    #           in_file_name="op_hcompute_padded16_global_wrapper_stencil",
    #           out_file_name="op_hcompute_grad_x_stencil")

    # harris_2
    # config_path = lake_controller_path + "harris/cim_stencil_op_hcompute_cim_stencil_55_to_cim_stencil_op_hcompute_cim_output_stencil_63_ubuf"
    # stream_path = lake_stream_path + "buf.csv"
    # test_lake(config_path,
    #           stream_path,
    #           in_file_name="op_hcompute_cim_stencil",
    #           out_file_name="op_hcompute_cim_output_stencil")

    # harris_3
    # config_path = lake_controller_path + "harris/lxx_stencil_op_hcompute_lxx_stencil_7_to_lxx_stencil_op_hcompute_lgxx_stencil_1_32_ubuf"
    # stream_path = lake_stream_path + "buf.csv"
    # test_lake(config_path,
    #           stream_path,
    #           in_file_name="op_hcompute_lxx_stencil",
    #           out_file_name="op_hcompute_lgxx_stencil_1")

    # harris_4
    # config_path = lake_controller_path + "harris/lxy_stencil_op_hcompute_lxy_stencil_4_to_lxy_stencil_op_hcompute_lgxy_stencil_1_22_ubuf"
    # stream_path = lake_stream_path + "buf.csv"
    # test_lake(config_path,
    #           stream_path,
    #           in_file_name="op_hcompute_lxy_stencil",
    #           out_file_name="op_hcompute_lgxy_stencil_1")

    # harris_5
    # config_path = lake_controller_path + "harris/lyy_stencil_op_hcompute_lyy_stencil_2_to_lyy_stencil_op_hcompute_lgyy_stencil_1_12_ubuf"
    # stream_path = lake_stream_path + "buf.csv"
    # test_lake(config_path,
    #           stream_path,
    #           in_file_name="op_hcompute_lyy_stencil",
    #           out_file_name="op_hcompute_lgyy_stencil_1")
