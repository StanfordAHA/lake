from lake.models.passthru_model import PassthruModel
from lake.modules.passthru import PassThroughMod
import magma as m
from magma import *
import fault
import tempfile
from kratos import *
import random as rand
import kratos as k
import pytest


def test_passthru():

    model_pt = PassthruModel()
    config_dict = {}
    model_pt.set_config(config_dict)

    dut = PassThroughMod()
    magma_dut = k.util.to_magma(dut,
                                flatten_array=True,
                                check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.clk = 0
    tester.circuit.data_in = 0
    tester.eval()
    tester.step(2)
    tester.eval()
    tester.step(2)

    rand.seed(0)

    data_in = 0

    for i in range(1000):
        data_in = rand.randint(0, 1)
        tester.circuit.data_in = data_in
        data_out = model_pt.set_in(data_in)
        tester.eval()
        tester.circuit.data_out.expect(data_out)
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
