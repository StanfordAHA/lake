from lake.modules.aggregator import *
from lake.models.agg_model import AggModel
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


def test_aggregator_basic(word_width=16, mem_word_width=4):

    model_agg = AggModel(num_elts=mem_word_width)
    # No actual configuration to be set
    model_agg.set_config()

    # get verilog file that needs to be copied to agg_dump directory
    # before running verilator
    dut = Aggregator(word_width=word_width, mem_word_width=mem_word_width)
    magma_dut = k.util.to_magma(dut, flatten_array=True,
                                check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    num_per_piece = int(mem_word_width / word_width)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    rand.seed(0)

    data_in = 0
    valid_in = 0

    for i in range(2500):

        valid_in = rand.randint(0, 1)
        data_in = rand.randint(0, 2 ** 16 - 1)
        align = rand.randint(0, 1)
        # Circuit
        tester.circuit.valid_in = valid_in
        tester.circuit.in_pixels = data_in
        tester.circuit.align = align
        # Model
        (model_dat, model_val, model_nf) = model_agg.interact(data_in, valid_in, align)

        tester.eval()

        tester.circuit.valid_out.expect(model_val)
        if model_val == 1:
            for i in range(mem_word_width):
                getattr(tester.circuit, f"agg_out_{i}").expect(model_dat[i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_aggregator_basic()
