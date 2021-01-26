from lake.models.sram_model import SRAMModel
from lake.modules.sram_stub import SRAMStub
import magma as m
from magma import *
import fault
import tempfile
import kratos as kts
import random as rand
import pytest


# this test tests sram_stub as well as part of sram_wrapper in the kratos code
@pytest.mark.parametrize("data_width", [16, 32])
@pytest.mark.parametrize("depth", [512, 1024])
@pytest.mark.parametrize("width_mult", [1, 2])
def test_sram_basic(data_width,
                    depth,
                    width_mult,
                    num_tiles=1):

    # Set up model...
    model_sram = SRAMModel(data_width=data_width,
                           width_mult=width_mult,
                           depth=depth,
                           num_tiles=num_tiles)
    new_config = {}
    model_sram.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = SRAMStub(data_width=data_width,
                   width_mult=width_mult,
                   depth=depth)

    magma_dut = kts.util.to_magma(dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    rand.seed(0)

    data = [0 for i in range(width_mult)]

    for z in range(1000):
        # Generate new input
        wen = rand.randint(0, 1)
        cen = rand.randint(0, 1)
        addr = rand.randint(0, depth - 1)
        for i in range(width_mult):
            data[i] = rand.randint(0, 2 ** data_width - 1)

        tester.circuit.wen = wen
        tester.circuit.cen = cen
        tester.circuit.addr = addr

        if width_mult == 1:
            tester.circuit.data_in = data[0]
        else:
            for i in range(width_mult):
                setattr(tester.circuit, f"data_in_{i}", data[i])

        model_dat_out = model_sram.interact(wen, cen, addr, data)

        tester.eval()

        if width_mult == 1:
            tester.circuit.data_out.expect(model_dat_out[0])
        else:
            for i in range(width_mult):
                getattr(tester.circuit, f"data_out_{i}").expect(model_dat_out[i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_sram_basic(data_width=16,
                    depth=512,
                    width_mult=4,
                    num_tiles=1)
