import kratos
from kratos import *
from math import log

class Aggregator(Generator):
    def __init__(self, word_width, mem_word_width):
        super().__init__("aggregator")

        # inputs
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst = self.reset("rst", 1)
        self.in_pixels = self.input("in_pixels", word_width)

        # outputs
        self.agg_out = self.output("agg_out", width=word_width, size=mem_word_width)
        self.valid = self.output("valid", 1)

        # local variables
        self.word_count = self.var("word_count", clog2(mem_word_width))
        self.shift_reg = self.var("shift_reg", width=word_width, size=mem_word_width)

        # add sequential blocks
        self.add_code(self.update_counter_valid)
        self.add_code(self.update_shift_reg)

        # add combinational blocks
        self.add_code(self.output_data)

    # setting valid signal
    @always((posedge, "clk"))
    def update_counter_valid(self):
        if (self.rst == 0):
            self.valid = 0
            if (mem_word_width > 1):
                self.word_count = 0
        elif (mem_word_width == 1):
            self.valid = 1
        elif (self.word_count == mem_word_width - 1):
            self.valid = 1
            self.word_count = 0
        else:
            self.valid = 0
            self.word_count = self.word_count + const(1, clog2(mem_word_width))

    @always((posedge, "clk"))
    def update_shift_reg(self):
        self.shift_reg[self.word_count] = self.in_pixels

    def output_data(self):
        self.agg_out = self.shift_reg

