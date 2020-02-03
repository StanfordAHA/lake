from lake.models.reg_fifo_model import RegFIFOModel
from lake.modules.reg_fifo import RegFIFO
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand


def test_reg_fifo_basic(data_width=32,
                        depth=64):

    # Set up model...
    model_rf = RegFIFOModel(data_width=data_width,
                            depth=depth)
    new_config = {}
    model_rf.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = RegFIFO(data_width=data_width,
                  depth=depth)

    magma_dut = k.util.to_magma(dut, flatten_array=True)
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

    data_in = 0

    for z in range(1000):
        # Generate new input
        push = rand.randint(0, 1)
        pop = rand.randint(0, 1)
        empty = model_rf.get_empty(push, pop)
        full = model_rf.get_full(push, pop)

        tester.circuit.empty.expect(empty)
        tester.circuit.full.expect(full)

        (model_out, model_val) = model_rf.interact(push, pop, data_in)

        tester.circuit.push = push
        tester.circuit.pop = pop
        tester.circuit.data_in = data_in

        tester.eval()

        tester.circuit.valid.expect(model_val)
        if(model_val):
            tester.circuit.data_out.expect(model_out)

        tester.step(2)

        data_in += 1

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
