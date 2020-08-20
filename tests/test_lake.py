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
    in_data, out_data = generate_data_lists(stream_path, in_ports, out_ports)

    for (f1, f2) in configs:
        setattr(tester.circuit, f1, f2)

    for i in range(len(out_data[0])):
        for j in range(len(in_data)):
            if i < len(in_data[j]):
                setattr(tester.circuit, f"data_in_{j}", in_data[j][i])

        tester.eval()

        # for j in range(len(out_data)):
        #     if i < len(out_data[j]):
        #         getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])

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
    # config_path = lake_controller_path + "cascade/buf1_input_10_to_buf1_conv_15_ubuf"
    # stream_path = lake_stream_path + "buf1.csv"
    # test_lake(config_path, stream_path, out_file_name="conv")

    # cascade_2
    config_path = lake_controller_path + "cascade/buf2_conv_12_to_buf2_output_3_ubuf"
    stream_path = lake_stream_path + "buf2.csv"
    test_lake(config_path, stream_path, in_file_name="conv")
