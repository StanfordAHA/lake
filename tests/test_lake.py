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
from lake.utils.util import check_env


def gen_test_lake(config_path,
                  stream_path,
                  in_file_name="input",
                  out_file_name="output",
                  in_ports=2,
                  out_ports=2):

    lt_dut = LakeTop(interconnect_input_ports=in_ports,
                     interconnect_output_ports=out_ports,
                     stencil_valid=False)

    configs = lt_dut.get_static_bitstream(config_path, in_file_name, out_file_name)
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

    for (f1, f2) in configs:
        setattr(tester.circuit, f1, f2)

    for i in range(len(out_data[0])):
        for j in range(len(in_data)):
            if i < len(in_data[j]):
                setattr(tester.circuit, f"data_in_{j}", in_data[j][i])
        # tester.circuit.data_in_0 = i

        tester.eval()

        for j in range(len(out_data)):
            if i < len(out_data[j]):
                if len(valids) != 0 and valids[i] == 1:
                    getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])
                if len(valids) == 0:
                    getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal", "--trace"])


# Need to add the paths and clockwork to CI
def test_conv_3_3():
    lc, ls = check_env()
    config_path = lc + "conv_3_3_new"
    stream_path = ls + "buf.csv"
    gen_test_lake(config_path=config_path,
                  stream_path=stream_path)


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


if __name__ == "__main__":

    # conv_3_3
    test_conv_3_3()

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
