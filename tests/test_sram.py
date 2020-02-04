from lake.models.sram_model import SRAMModel
from lake.modules.sram_stub import SRAMStub
import magma as m
from magma import *
import fault
import tempfile
import kratos as kts
import random as rand
import pytest


@pytest.mark.parametrize("width", [16, 32])
@pytest.mark.parametrize("depth", [512, 1024])
def test_sram_basic(width,
                    depth):

    # Set up model...
    model_sram = SRAMModel(width=width,
                           depth=depth)
    new_config = {}
    model_sram.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = SRAMStub(width=width,
                   depth=depth)

    magma_dut = kts.util.to_magma(dut, flatten_array=True)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    rand.seed(0)

    for z in range(1000):
        # Generate new input
        wen = rand.randint(0, 1)
        cen = rand.randint(0, 1)
        addr = rand.randint(0, depth - 1)
        data = rand.randint(0, 2 ** width - 1)

        tester.circuit.wen = wen
        tester.circuit.cen = cen
        tester.circuit.addr = addr
        tester.circuit.data_in = data

        model_dat_out = model_sram.interact(wen, cen, addr, data)

        tester.eval()

        tester.circuit.data_out.expect(model_dat_out)

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
