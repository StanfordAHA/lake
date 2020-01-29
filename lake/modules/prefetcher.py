from kratos import *
from utils.util import increment, decrement
from lake.attributes.config_reg_attr import ConfigRegAttr


class Prefetcher(Generator):
    '''
    This module will handle prefetching from SRAM to the tb.
    Since there is a variable latency for a ready cycle to
    provide the next valid set of data to the tbs, we need to
    buffer some amount so that we can satisfy these requests with immediacy
    '''
    def __init__(self,
                 fetch_width,
                 max_prefetch):
        super().__init__("Prefetcher", debug=True)
        # Capture to the object
        self.fetch_width = fetch_width
        self.max_prefetch = max_prefetch

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        self._in_dat = self.input("in_dat", self.data_width)
        self._in_valid = self.input("in_valid", 1)
        self._line_length = self.input("line_length", self.counter_width)
        self._line_length.add_attribute(ConfigRegAttr())

        # Outputs
        self._out_dat = self.output("out_dat", self.data_width)
        self._out_valid = self.output("out_valid", 1)
        self._out_align = self.output("align", 1)

        # Local Signals
        self._cnt = self.var("cnt", self.counter_width)

        # Generate
        self.add_code(self.update_cnt)
        self.add_code(self.set_align)

        self.wire(self._out_dat, self._in_dat)
        self.wire(self._out_valid, self._in_valid)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_cnt(self):
        if ~self._rst_n:
            self._cnt = 0
        elif(self._in_valid):
            if(self._cnt == decrement(self._line_length, 1)):
                self._cnt = 0
            else:
                self._cnt = increment(self._cnt, 1)

    @always_comb
    def set_align(self):
        self._out_align = self._in_valid & (self._cnt == decrement(self._line_length, 1))


if __name__ == "__main__":
    align_dut = AggAligner(data_width=16,
                           max_line_length=64)
    verilog(align_dut, filename="agg_aligner.sv")
