from lake.models.pipe_reg_model import PipeRegModel
from lake.modules.pipe_reg import PipeReg
from lake.passes.passes import lift_config_reg
import _kratos
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.parametrize("stages", [1, 2, 4, 8])
def test_pipe_reg_basic(stages,
                        data_width=16):

    # Set up model..
    model_pr = PipeRegModel(data_width=data_width,
                            stages=stages)
    new_config = {}
    model_pr.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = PipeReg(data_width=data_width,
                  stages=stages)
    lift_config_reg(dut.internal_generator)
    magma_dut = k.util.to_magma(dut, flatten_array=True,
                                check_multiple_driver=False,
                                check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.circuit.clk_en = 1
    tester.circuit.data_in = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    # Seed for posterity
    rand.seed(0)

    data_in = 0

    for i in range(1000):

        data_in = rand.randint(0, 2 ** data_width - 1)
        tester.circuit.data_in = data_in
        data_out_exp = model_pr.update_data(data_in)
        tester.eval()

        if(stages == 0):
            tester.circuit.data_out.expect(data_out_exp)

        tester.step(2)

        if(stages > 0):
            tester.circuit.data_out.expect(data_out_exp)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
