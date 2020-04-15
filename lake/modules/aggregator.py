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
        self.in_pixels = self.input("in_pixels",
                                    word_width)
        self._valid_in = self.input("valid_in", 1)
        self._align = self.input("align", 1)

        # outputs
        self._agg_out = self.output("agg_out",
                                    word_width,
                                    size=mem_word_width,
                                    packed=True,
                                    explicit_array=True)
        self._valid_out = self.output("valid_out", 1)
        self._next_full = self.output("next_full", 1)

        self.shift_reg = self.var("shift_reg",
                                  width=word_width,
                                  size=mem_word_width,
                                  explicit_array=True,
                                  packed=True)

        # local variables
        if(mem_word_width > 1):
            self._word_count = self.var("word_count", clog2(mem_word_width))
            self.add_code(self.update_counter_valid)
            self.add_code(self.set_next_full)
            self.add_code(self.update_shift_reg)
            # add combinational blocks
        else:
            self.wire(self._valid_out, const(1, 1))
            self.wire(self._next_full, self._valid_in)
            # only add the update counter/valid
            self.wire(self.shift_reg[0], self.in_pixels)

        self.add_code(self.output_data)

    @always_comb
    def set_next_full(self):
        self._next_full = (self._valid_in & (self._word_count == const(self.mem_word_width - 1,
                                                                       self._word_count.width))) | self._align

    # setting valid signal and word_count index
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_counter_valid(self):
        if ~self.rst_n:
            self._valid_out = 0
            self._word_count = const(0, self._word_count.width)
        # no self._word_count in this case
        elif self._valid_in:
            if (self._word_count == const(self.mem_word_width - 1, self._word_count.width)) | self._align:
                self._valid_out = const(1, 1)
                self._word_count = const(0, self._word_count.width)
            else:
                self._valid_out = const(0, 1)
                self._word_count = self._word_count + const(1, clog2(mem_word_width))
        else:
            self._valid_out = 0

    @always_ff((posedge, "clk"))
    def update_shift_reg(self):
        if (self._valid_in):
            self.shift_reg[self._word_count] = self.in_pixels

    @always_comb
    def output_data(self):
        self._agg_out = self.shift_reg


if __name__ == "__main__":
    db_dut = Aggregator(word_width=16,
                        mem_word_width=4)
    verilog(db_dut, filename="aggregator.sv")
