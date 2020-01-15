from lake.modules.aggregator import *
import tempfile
import pytest

@pytest.mark.parametrize("word_width", [1,16,64])
@pytest.mark.parametrize("memory_width", [1,2,8])
def test_aggregator(word_width: int, memory_width: int):

    _Aggregator = Aggregator(word_width, memory_width)
    tester = fault.Tester(_Aggregator, _Aggregator.CLK)
    tester.circuit.CLK = 0
    for i in range(2*memory_width + 1):
        tester.circuit.INPUT_PIXELS = i
        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])
