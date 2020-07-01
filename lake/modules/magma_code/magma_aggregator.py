import magma as m
import mantle
import fault
import math
from math import log2
from mantle import Register, CounterModM, DFF


def DefineAggregator(word_width, mem_word_width):
    class Aggregator(m.Circuit):
        name = 'Aggregator_' + str(word_width) + '_' + str(mem_word_width)
        IO = ['in_pixels', m.In(m.Bits[word_width]),
              'valid_in', m.In(m.Bit),
              'agg_out', m.Out(m.Array[mem_word_width, m.Bits[word_width]]),
              'valid_out', m.Out(m.Bit),
              'next_full', m.Out(m.Bit)] + m.ClockInterface()

        @m.circuit.combinational
        def check_valid_in(valid_in: m.Bit, check: m.Bit) -> m.Bit:
            return valid_in & check

        @classmethod
        def definition(agg):
            # stores input pixels from each cycle
            regs = [Register(word_width) for i in range(mem_word_width)]
            regs[0].I <= agg.in_pixels
            mem_word_width_bits = int(log2(mem_word_width))
            if mem_word_width == 1:
                agg.valid_out <= 1
                m.wire(agg.next_full, agg.valid_in)
            else:
                # keep track of number of input pixels so far
                counter = CounterModM(mem_word_width, mem_word_width_bits)
                # output VALID  on same clock when data is in AGGREGATED_OUTPUT
                valid_dff = DFF()
                # valid when number of input pixels is same as mem_word_width
                valid_dff.I <= (counter.O == mem_word_width - 2)
                m.wire(agg.valid_out, agg.check_valid_in(agg.valid_in, valid_dff.O))
#                agg.valid_out = agg.check_valid_in(agg.valid_in, valid_dff.O)
                m.wire(agg.next_full, agg.check_valid_in(agg.valid_in, (counter.O == mem_word_width - 2)))
#                agg.next_full = agg.check_valid_in(agg.valid_in, (counter.O == mem_word_width - 2))
#                m.wire(agg.next_full, agg.valid_in)
            # output all mem_word_width in_pixels so far
            a = m.scan(regs, scanargs={'I': 'O'})
            agg.agg_out <= a.O

    return Aggregator
