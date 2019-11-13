<<<<<<< HEAD
import magma as m
import mantle
import fault
import math

from math import ceil, log
from mantle import Register, CounterModM, DFF

def DefineAggregator(word_width, memory_width):
    class _Aggregator(m.Circuit):
        name = 'Aggregator_' + str(word_width) + '_' + str(memory_width)
        IO = [  'INPUT_PIXELS', m.In(m.Bits[word_width]), \
                'AGGREGATED_OUTPUT', m.Out(m.Array[memory_width // word_width, m.Bits[word_width]]), \
                'VALID', m.Out(m.Bit)] \
                + m.ClockInterface()

        @classmethod
        def definition(agg):
            mem_word_num = memory_width // word_width
            # stores input pixels from each cycle
            regs = [Register(word_width) for i in range(mem_word_num)]
            regs[0].I <= agg.INPUT_PIXELS
            if mem_word_num == 1:
                agg.VALID <= 1
            else:
                # keep track of number of input pixels so far
                counter = CounterModM(mem_word_num, ceil(log(mem_word_num, 2)))
                # output VALID  on same clock when data is in AGGREGATED_OUTPUT
                valid_dff = DFF()
                # valid when number of input pixels is same as mem_word_num
                valid_dff.I <= (counter.O == mem_word_num - 1)
                agg.VALID <= valid_dff.O
            # output all mem_word_num INPUT_PIXELS so far
            a = m.scan(regs, scanargs={'I':'O'})
            agg.AGGREGATED_OUTPUT <= a.O

    return _Aggregator

def Aggregator(word_width, memory_width):
    return DefineAggregator(word_width, memory_width)
=======
from kratos import *
from math import log

class Aggregator(Generator):
    def __init__(self, 
                word_width, 
                mem_word_width):

        super().__init__("aggregator")

        # inputs
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)
        self.in_pixels = self.input("in_pixels", word_width)

        # outputs
        self.agg_out = self.output("agg_out", word_width, size=mem_word_width, packed=True)
        self.valid_out = self.output("valid_out", 1)

        # local variables
        self.word_count = self.var("word_count", clog2(mem_word_width))
        self.shift_reg = self.var("shift_reg", width=word_width, size=mem_word_width, packed=True)

        # add sequential blocks
        self.add_code(self.update_counter_valid)
        self.add_code(self.update_shift_reg)

        # add combinational blocks
        self.add_code(self.output_data)

    # setting valid signal and word_count index
    @always((posedge, "clk"), (negedge, "rst_n"))
    def update_counter_valid(self):
        if (self.rst_n == 0):
            self.valid_out = 0
            if (mem_word_width > 1):
                self.word_count = 0
        # no self.word_count in this case
        elif (mem_word_width == 1):
            self.valid_out = 1
        elif (self.word_count == mem_word_width - 1):
            self.valid_out = 1
            self.word_count = 0
        else:
            self.valid_out = 0
            self.word_count = self.word_count + const(1, clog2(mem_word_width))

    @always((posedge, "clk"))
    def update_shift_reg(self):
        if mem_word_width == 1:
            self.shift_reg = self.in_pixels
        else:
            self.shift_reg[self.word_count] = self.in_pixels

    def output_data(self):
        self.agg_out = self.shift_reg
>>>>>>> beginning aggregation buffer

