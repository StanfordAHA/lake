from lake.models.agg_aligner_model import AggAlignerModel
from lake.modules.agg_aligner import AggAligner
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand


def test_agg_aligner_basic(data_width=16,
                           max_line_length=2048,
                           line_length=64):

    # Set up model...
    model_al = AggAlignerModel(data_width=data_width, max_line_length=max_line_length)
    new_config = {}
    new_config['line_length'] = line_length
    model_al.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = AggAligner(data_width=data_width,
                     max_line_length=max_line_length)

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

    data_in = 0

    for i in range(1000):
        new_val = rand.randint(0, 1)

        (model_dat, model_val, model_align) = model_al.interact(data_in, new_val)
        tester.circuit.in_valid = new_val
        tester.circuit.in_dat = data_in
        tester.eval()

        tester.circuit.out_valid.expect(model_val)
        tester.circuit.align.expect(model_align)
        tester.circuit.out_dat.expect(model_dat)

        tester.step(2)

        data_in += 1

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
