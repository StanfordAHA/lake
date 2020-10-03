from lake.models.addr_gen_model import AddrGenModel
from lake.modules.addr_gen import AddrGen
import magma as m
from magma import *
import fault
import tempfile
from kratos import *
import kratos as k
import pytest


@pytest.mark.skip
def test_addr_gen_basic(depth=512,
                        addr_width=16,
                        iterator_support=6):
    model_ag = AddrGenModel(iterator_support=iterator_support,
                            address_width=addr_width)

    config_dict = {}
    config_dict["starting_addr"] = 0
    config_dict["dimensionality"] = 3
    config_dict["strides_0"] = 1
    config_dict["strides_1"] = 3
    config_dict["strides_2"] = 9
    config_dict["ranges_0"] = 3
    config_dict["ranges_1"] = 3
    config_dict["ranges_2"] = 3

    model_ag.set_config(config_dict)

    word_width = 1
    fetch_width = 4
    stencil_height = 3
    max_range_value = 5
    img_height = 4
    dut = AddrGen(iterator_support=iterator_support,
                  address_width=addr_width)

    magma_dut = k.util.to_magma(dut, flatten_array=True,
                                check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.dimensionality = 4
    tester.circuit.starting_addr = 0
    # tester.circuit.strides_0 = 1
    # tester.circuit.strides_1 = 3
    # tester.circuit.strides_2 = 9
    # tester.circuit.ranges_0 = 3
    # tester.circuit.ranges_1 = 3
    # tester.circuit.ranges_2 = 3
    tester.circuit.strides_0 = 1
    tester.circuit.strides_1 = 1
    tester.circuit.strides_2 = 1
    tester.circuit.strides_3 = -26
    tester.circuit.ranges_0 = 1
    tester.circuit.ranges_1 = 1
    tester.circuit.ranges_2 = 1
    tester.circuit.ranges_3 = 10000

    tester.circuit.clk = 0
    tester.circuit.clk_en = 1
    tester.circuit.rst_n = 0
    tester.eval()
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.eval()
    tester.step(2)

    for i in range(1000):
        tester.circuit.step = 1
        tester.circuit.addr_out.expect(model_ag.get_address())
        model_ag.step()
        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_addr_gen_basic()
