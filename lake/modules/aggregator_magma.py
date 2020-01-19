import magma as m
import mantle
import fault
import math
from math import ceil, log
from mantle import Register, CounterModM, DFF


def DefineAggregator(word_width, memory_width):
    class _Aggregator(m.Circuit):
        name = 'Aggregator_' + str(word_width) + '_' + str(memory_width)
        IO = ['INPUT_PIXELS', m.In(m.Bits[word_width]),
              'AGGREGATED_OUTPUT', m.Out(m.Array[memory_width, m.Bits[word_width]]),
              'VALID', m.Out(m.Bit)] + m.ClockInterface()

        @classmethod
        def definition(agg):
            # stores input pixels from each cycle
            regs = [Register(word_width) for i in range(memory_width)]
            regs[0].I <= agg.INPUT_PIXELS
            if memory_width == 1:
                agg.VALID <= 1
            else:
                # keep track of number of input pixels so far
                counter = CounterModM(memory_width, ceil(log(memory_width, 2)))
                # output VALID  on same clock when data is in AGGREGATED_OUTPUT
                valid_dff = DFF()
                # valid when number of input pixels is same as memory_width
                valid_dff.I <= (counter.O == memory_width - 1)
                agg.VALID <= valid_dff.O
            # output all memory_width INPUT_PIXELS so far
            a = m.scan(regs, scanargs={'I': 'O'})
            agg.AGGREGATED_OUTPUT <= a.O

    return _Aggregator


def Aggregator(word_width, memory_width):
    return DefineAggregator(word_width, memory_width)
