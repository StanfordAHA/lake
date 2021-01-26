from lake.models.reg_fifo_model import RegFIFOModel
from lake.modules.reg_fifo import RegFIFO
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.parametrize("width_mult", [1, 2])
def test_reg_fifo_basic(width_mult,
                        data_width=16,
                        depth=64):

    # Set up model...
    model_rf = RegFIFOModel(data_width=data_width,
                            width_mult=width_mult,
                            depth=depth)
    new_config = {}
    model_rf.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = RegFIFO(data_width=data_width,
                  width_mult=width_mult,
                  depth=depth)

    magma_dut = k.util.to_magma(dut, flatten_array=True,
                                check_flip_flop_always_ff=False)
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

    data_in = []
    for i in range(width_mult):
        data_in.append(0)

    num_iters = 1000
    for z in range(num_iters):
        # Generate new input
        push = rand.randint(0, 1)
        pop = rand.randint(0, 1)
        empty = model_rf.get_empty(push, pop)
        full = model_rf.get_full(push, pop)
        for i in range(width_mult):
            data_in[i] = rand.randint(0, 2 ** data_width - 1)

        tester.circuit.empty.expect(empty)
        tester.circuit.full.expect(full)

        mem_valid_data = rand.randint(0, 1)
        # tester.circuit.mem_valid_data = mem_valid_data

        (model_out, model_val, model_empty, model_full, model_mem_valid) = \
            model_rf.interact(push, pop, data_in, mem_valid_data)

        tester.circuit.push = push
        tester.circuit.pop = pop
        if width_mult == 1:
            tester.circuit.data_in = data_in[0]
        else:
            for i in range(width_mult):
                setattr(tester.circuit, f"data_in_{i}", data_in[i])

        tester.eval()

        tester.circuit.valid.expect(model_val)
        if model_val:
            # I'm not sure what this is for?
            # tester.circuit.mem_valid_data_out.expect(model_mem_valid)
            if width_mult == 1:
                tester.circuit.data_out.expect(model_out[0])
            else:
                for i in range(width_mult):
                    getattr(tester.circuit, f"data_out_{i}").expect(model_out[i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_reg_fifo_basic(width_mult=1,
                        data_width=16,
                        depth=64)
