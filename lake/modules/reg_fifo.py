from kratos import *
from utils.util import increment, decrement
from lake.attributes.config_reg_attr import ConfigRegAttr


class RegFIFO(Generator):
    '''
    This module generates register-based FIFOs. These are useful
    when we only need a few entries with no prefetching needed
    '''
    def __init__(self,
                 data_width,
                 depth):
        super().__init__("RegFIFO")

        self.data_width = data_width
        self.depth = depth

        assert not (depth & (depth - 1)), "FIFO depth needs to be a power of 2"

        # CLK and RST
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._clk_en = self.input("clk_en", 1)

        # INPUTS
        self._data_in = self.input("data_in", self.data_width)
        self._data_out = self.output("data_out", self.data_width)

        self._push = self.input("push", 1)
        self._pop = self.input("pop", 1)

        self._empty = self.output("empty", 1)
        self._full = self.output("full", 1)
        self._valid = self.output("valid", 1)

        ptr_width = clog2(self.depth)

        self._rd_ptr = self.var("rd_ptr", ptr_width)
        self._wr_ptr = self.var("wr_ptr", ptr_width)
        self._read = self.var("read", 1)
        self._write = self.var("write", 1)
        self._reg_array = self.var("reg_array",
                                   self.data_width,
                                   size=self.depth,
                                   packed=True,
                                   explicit_array=True)

        self._passthru = self.var("passthru", 1)
        self.wire(self._passthru, self._pop & self._push & self._empty)

        self.wire(self._empty, self._wr_ptr == self._rd_ptr)
        self.wire(self._full, (self._wr_ptr + 1) == self._rd_ptr)
        self.wire(self._read, self._pop & ~self._passthru & ~self._empty)
        self.wire(self._write, self._push & ~self._passthru & ~self._full)

        # Boilerplate Add always @(posedge clk, ...) blocks
        self.add_code(self.rd_ptr_ff)
        self.add_code(self.wr_ptr_ff)
        self.add_code(self.reg_array_ff)
        self.add_code(self.data_out_ff)
        self.add_code(self.valid_ff)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def rd_ptr_ff(self):
        if ~self._rst_n:
            self._rd_ptr = 0
        elif self._read:
            self._rd_ptr = self._rd_ptr + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def wr_ptr_ff(self):
        if ~self._rst_n:
            self._wr_ptr = 0
        elif self._write:
            self._wr_ptr = self._wr_ptr + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def reg_array_ff(self):
        if ~self._rst_n:
            self._reg_array = 0
        elif self._write:
            self._reg_array[self._wr_ptr] = self._data_in

    # @always_ff((posedge, "clk"), (negedge, "rst_n"))
    @always_comb
    def data_out_ff(self):
        # if ~self._rst_n:
        #     self._data_out = 0
        # elif self._read:
        #     self._data_out = self._reg_array[self._rd_ptr]
        if(self._passthru):
            self._data_out = self._data_in
        else:
            self._data_out = self._reg_array[self._rd_ptr]

    # @always_ff((posedge, "clk"), (negedge, "rst_n"))
    @always_comb
    def valid_ff(self):
        # if ~self._rst_n:
        #     self._valid = 0
        # elif self._pop:
        #     self._valid = ~self._empty
        self._valid = self._pop & ((~self._empty) | self._passthru)


if __name__ == "__main__":
    dut = RegFIFO(data_width=16,
                  depth=64)
    verilog(dut, filename="reg_fifo.sv")
