import kratos as kts
import fault
import tempfile
import pytest

from lake.utils.sram_macro import SRAMMacroInfo
# input and output data
from lake.utils.parse_clkwork_csv import generate_data_lists
# configurations
from lake.utils.parse_clkwork_config import *
from lake.utils.util import check_env
from lake.dsl.dsl_examples.memtile import *


def base_lake_tester(config_path,
                     in_file_name,
                     out_file_name,
                     in_ports,
                     out_ports,
                     lt_dut,
                     stencil_valid=False):

    configs = lt_dut.get_static_bitstream(config_path, in_file_name, out_file_name)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
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

    tester.circuit.clk_en = 1
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
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
                if len(valids) == 0:
                    getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])


@pytest.mark.skip
def test_conv_3_3():
    lc, ls = check_env()
    config_path = lc + "conv_3_3_recipe_dsl"
    stream_path = ls + "conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf_0_top_SMT.csv"

    lt_dut = tile.test_magma_lake()

    gen_test_lake(config_path=config_path,
                  stream_path=stream_path,
                  lt_dut=lt_dut)


if __name__ == "__main__":
    test_conv_3_3()
