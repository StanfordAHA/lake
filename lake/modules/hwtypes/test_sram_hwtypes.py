from lake.modules.hwtypes.sram_stub import sram_stub
from peak import family_closure, family
from math import log2
from lake.modules.hwtypes.utils import *
from hwtypes import Bit, BitVector

import magma as m
import fault
import tempfile
import kratos as kts
import random as rand
import pytest


# This doesn't work
@pytest.mark.skip
# this test tests sram_stub as well as part of sram_wrapper in the kratos code
@pytest.mark.parametrize("data_width", [16, 32])
@pytest.mark.parametrize("mem_depth", [512, 1024])
@pytest.mark.parametrize("fetch_width", [1, 2])
def test_sram_basic(data_width,
                    mem_depth,
                    fetch_width):

    # Set up model...
    # sram_py = sram_stub(depth, data_width, width_mult, family.PyFamily())
    # model_sram = sram_py(family=family.PyFamily())()

    sram_magma = sram_stub(mem_depth, data_width, fetch_width, family=family.MagmaFamily())
    sram_magma_defn = sram_magma(family=family.MagmaFamily())
    tester = fault.Tester(sram_magma_defn, sram_magma_defn.CLK)

    data = 0
    for i in range(100):
        tester.circuit.wen = i % 2
        tester.circuit.cen = 1
        tester.circuit.data_in = data
        tester.circuit.addr = data
        data = data + 1
        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_sram_basic(data_width=16,
                    mem_depth=512,
                    fetch_width=4)
