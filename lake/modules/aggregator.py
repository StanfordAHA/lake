from kratos import *
from math import log

class Aggregator(Generator):
    def __init__(self, 
                word_width,
                mem_word_width):

        super().__init__("aggregator", debug=True)
        self.mem_word_width = mem_word_width

        # inputs
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)
        self.in_pixels = self.input("in_pixels", word_width)
        self._valid_in = self.input("valid_in", 1)

        # outputs
        self.agg_out = self.output("agg_out", word_width, size=mem_word_width, packed=True, explicit_array=True)
        self.valid_out = self.output("valid_out", 1)
        self._next_full = self.output("next_full", 1)

        # local variables
        self.word_count = self.var("word_count", clog2(mem_word_width))
        self.shift_reg = self.var("shift_reg", width=word_width, size=mem_word_width, explicit_array=True, packed=True)

        # add sequential blocks
        self.add_code(self.update_counter_valid)
        self.add_code(self.update_shift_reg)

        # add combinational blocks
        self.add_code(self.output_data)

        self.add_code(self.set_next_full)

    @always_comb
    def set_next_full(self):
        self._next_full = self._valid_in & (self.word_count == const(self.mem_word_width - 1, self.word_count.width))

    # setting valid signal and word_count index
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_counter_valid(self):
        if ~self.rst_n:
            self.valid_out = 0
            #if (mem_word_width > 1):
            self.word_count = const(0, self.word_count.width)
        # no self.word_count in this case
        elif self._valid_in:
            if (self.word_count == const(self.mem_word_width - 1, self.word_count.width)):
                self.valid_out = const(1, 1)
                self.word_count = const(0, self.word_count.width)
            else:
                self.valid_out = const(0, 1)
                self.word_count = self.word_count + const(1, clog2(mem_word_width))

    @always_ff((posedge, "clk"))
    def update_shift_reg(self):
        if self._valid_in:
            self.shift_reg[self.word_count] = self.in_pixels

    @always_comb
    def output_data(self):
        self.agg_out = self.shift_reg

if __name__ == "__main__":
    db_dut = Aggregator(word_width=16, mem_word_width=4)
    verilog(db_dut, filename="aggregator.sv", check_active_high=False, check_multiple_driver=False)

