import fault
import magma as m
import fault
import tempfile
from math import ceil, log
import pytest


def DefineAggregator(word_width, mem_word_width):
    from mantle import Register, CounterModM, DFF

    class _Aggregator(m.Circuit):
        name = 'Aggregator_' + str(word_width) + '_' + str(mem_word_width)
        IO = ['INPUT_PIXELS', m.In(m.Bits[word_width]),
              'AGGREGATED_OUTPUT', m.Out(m.Array[mem_word_width, m.Bits[word_width]]),
              'VALID', m.Out(m.Bit)] + m.ClockInterface()

        @classmethod
        def definition(agg):
            # stores input pixels from each cycle
            regs = [Register(word_width) for i in range(mem_word_width)]
            regs[0].I <= agg.INPUT_PIXELS
            if mem_word_width == 1:
                agg.VALID <= 1
            else:
                # keep track of number of input pixels so far
                counter = CounterModM(mem_word_width, ceil(log(mem_word_width, 2)))
                # output VALID  on same clock when data is in AGGREGATED_OUTPUT
                valid_dff = DFF()
                # valid when number of input pixels is same as memory_width
                valid_dff.I <= (counter.O == mem_word_width - 1)
                agg.VALID <= valid_dff.O
            # output all memory_width INPUT_PIXELS so far
            a = m.scan(regs, scanargs={'I': 'O'})
            agg.AGGREGATED_OUTPUT <= a.O

    return _Aggregator


def Aggregator(word_width, mem_word_width):
    return DefineAggregator(word_width, mem_word_width)


@pytest.mark.skip
def test_agg_magma(word_width=16, mem_word_width=4):

    dut = Aggregator(word_width=word_width, mem_word_width=mem_word_width)
    tester = fault.Tester(dut, dut.CLK)
    tester.circuit.CLK = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    for i in range(15):
        tester.circuit.INPUT_PIXELS = i % 5
        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])
