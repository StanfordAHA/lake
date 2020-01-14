from lake.models.addr_gen_model import AddrGenModel
from lake.modules.addr_gen import AddrGen
import magma as m
from magma import *
import fault
import tempfile
from kratos import *
import kratos as k
# from kratos import Simulator

def test_addr_gen_basic():
    model_ag = AddrGenModel(512, 6, 16)

    config_dict = {}
    config_dict["starting_addr"] = 0
    config_dict["dimensionality"] = 3
    config_dict["stride_0"] = 1
    config_dict["stride_1"] = 3
    config_dict["stride_2"] = 9
    config_dict["range_0"] = 3
    config_dict["range_1"] = 3
    config_dict["range_2"] = 3

    model_ag.set_config(config_dict)

    #print(str(model_ag.get_address()))
    #for i in range(26):
    #    model_ag.step()

    word_width = 1
    fetch_width = 4
    stencil_height = 3
    max_range_value = 5
    img_height = 4
    dut = AddrGen(512, 6, 16)
    

    #TransposeBuffer(word_width, fetch_width, stencil_height, max_range_value, img_height)

    magma_dut = k.util.to_magma(dut)
    tester = fault.Tester(magma_dut(), magma_dut.clk)

    print(str(tester.circuit.dimensionality))
    #tester.poke(tester.circuit.dimensionality, 3)
    tester.circuit.dimensionality[0] = 3
   # tester.circuit.strides

    #tester.circuit.clk = 0
    tester.poke(tester.circuit.clk, 0)
    tester.circuit.clk_en = 1
    tester.circuit.rst_n = 0
    tester.eval()
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.eval()
    tester.step(2)

    #for i in range(5):
    #    tester.circuit.input_data = i
    #    tester.circuit.range_outer = 5
    #    tester.circuit.range_inner = 3
    #    tester.stride = 2
    #    tester.indices = [0,0,0,0,0]
    for i in range(100):
        tester.eval()
        tester.step(2)



    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                            directory=tempdir,
                            flags=["-Wno-fatal", "--trace"])

test_addr_gen_basic()