from kratos import *
from lake.utils.util import increment, decrement
from lake.attributes.config_reg_attr import ConfigRegAttr


class RegFIFO(Generator):
    '''
    This module generates register-based FIFOs. These are useful
    when we only need a few entries with no prefetching needed
    '''
    def __init__(self,
                 data_width,
                 width_mult,
                 depth,
                 parallel=False,
                 break_out_rd_ptr=False):

        super().__init__(f"reg_fifo_d_{depth}_w_{width_mult}", debug=True)

        self.data_width = self.parameter("data_width", 16)
        self.data_width.value = data_width
        self.depth = depth
        self.width_mult = width_mult
        self.parallel = parallel
        self.break_out_rd_ptr = break_out_rd_ptr

        assert not (depth & (depth - 1)), "FIFO depth needs to be a power of 2"

        # CLK and RST
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._clk_en = self.input("clk_en", 1)

        # INPUTS
        self._data_in = self.input("data_in",
                                   self.data_width,
                                   size=self.width_mult,
                                   explicit_array=True,
                                   packed=True)
        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=self.width_mult,
                                     explicit_array=True,
                                     packed=True)

        if self.parallel:
            self._parallel_load = self.input("parallel_load", 1)
            self._parallel_read = self.input("parallel_read", 1)
            self._num_load = self.input("num_load", clog2(self.depth) + 1)
            self._parallel_in = self.input("parallel_in",
                                           self.data_width,
                                           size=(self.depth,
                                                 self.width_mult),
                                           explicit_array=True,
                                           packed=True)

            self._parallel_out = self.output("parallel_out",
                                             self.data_width,
                                             size=(self.depth,
                                                   self.width_mult),
                                             explicit_array=True,
                                             packed=True)

        self._push = self.input("push", 1)
        self._pop = self.input("pop", 1)

        self._valid = self.output("valid", 1)

        ptr_width = max(1, clog2(self.depth))

        self._rd_ptr = self.var("rd_ptr", ptr_width)
        if self.break_out_rd_ptr:
            self._rd_ptr_out = self.output("rd_ptr_out", ptr_width)
            self.wire(self._rd_ptr_out, self._rd_ptr)
        self._wr_ptr = self.var("wr_ptr", ptr_width)
        self._read = self.var("read", 1)
        self._write = self.var("write", 1)
        self._reg_array = self.var("reg_array",
                                   self.data_width,
                                   size=(self.depth,
                                         self.width_mult),
                                   packed=True,
                                   explicit_array=True)

        self._passthru = self.var("passthru", 1)
        self._empty = self.output("empty", 1)
        self._full = self.output("full", 1)

        self._num_items = self.var("num_items", clog2(self.depth) + 1)
        # self.wire(self._full, (self._wr_ptr + 1) == self._rd_ptr)
        self.wire(self._full, self._num_items == self.depth)
        # self.wire(self._empty, self._wr_ptr == self._rd_ptr)
        self.wire(self._empty, self._num_items == 0)

        self.wire(self._read, self._pop & ~self._passthru & ~self._empty)

        self.wire(self._passthru, self._pop & self._push & self._empty)

        # Should only write

        # Boilerplate Add always @(posedge clk, ...) blocks
        if self.parallel:
            self.add_code(self.set_num_items_parallel)
            self.add_code(self.reg_array_ff_parallel)
            self.add_code(self.wr_ptr_ff_parallel)
            self.add_code(self.rd_ptr_ff_parallel)
            self.wire(self._parallel_out, self._reg_array)
            self.wire(self._write,
                      self._push & ~self._passthru & (~self._full | (self._pop | self._parallel_read)))
        else:
            self.wire(self._write, self._push & ~self._passthru & (~self._full | self._pop))
            self.add_code(self.set_num_items)
            self.add_code(self.reg_array_ff)
            self.add_code(self.wr_ptr_ff)
            self.add_code(self.rd_ptr_ff)
        self.add_code(self.data_out_ff)
        self.add_code(self.valid_ff)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def rd_ptr_ff(self):
        if ~self._rst_n:
            self._rd_ptr = 0
        elif self._read:
            self._rd_ptr = self._rd_ptr + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def rd_ptr_ff_parallel(self):
        if ~self._rst_n:
            self._rd_ptr = 0
        elif self._parallel_load | self._parallel_read:
            self._rd_ptr = 0
        elif self._read:
            self._rd_ptr = self._rd_ptr + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def wr_ptr_ff(self):
        if ~self._rst_n:
            self._wr_ptr = 0
        elif self._write:
            if self._wr_ptr == (self.depth - 1):
                self._wr_ptr = 0
            else:
                self._wr_ptr = self._wr_ptr + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def wr_ptr_ff_parallel(self):
        if ~self._rst_n:
            self._wr_ptr = 0
        elif self._parallel_load:
            self._wr_ptr = self._num_load[max(1, clog2(self.depth)) - 1, 0]
        elif self._parallel_read:
            if self._push:
                self._wr_ptr = 1
            else:
                self._wr_ptr = 0
        elif self._write:
            self._wr_ptr = self._wr_ptr + 1
            # if self._wr_ptr == (self.depth - 1):
            #     self._wr_ptr = 0
            # else:
            #     self._wr_ptr = self._wr_ptr + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def reg_array_ff(self):
        if ~self._rst_n:
            self._reg_array = 0
        elif self._write:
            self._reg_array[self._wr_ptr] = self._data_in

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def reg_array_ff_parallel(self):
        if ~self._rst_n:
            self._reg_array = 0
        elif self._parallel_load:
            self._reg_array = self._parallel_in
        elif self._write:
            if self._parallel_read:
                self._reg_array[0] = self._data_in
            else:
                self._reg_array[self._wr_ptr] = self._data_in

    @always_comb
    def data_out_ff(self):
        if(self._passthru):
            self._data_out = self._data_in
        else:
            self._data_out = self._reg_array[self._rd_ptr]

    @always_comb
    def valid_ff(self):
        self._valid = self._pop & ((~self._empty) | self._passthru)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_num_items(self):
        if ~self._rst_n:
            self._num_items = 0
        elif self._write & ~self._read:
            self._num_items = self._num_items + 1
        elif ~self._write & self._read:
            self._num_items = self._num_items - 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_num_items_parallel(self):
        if ~self._rst_n:
            self._num_items = 0
        elif self._parallel_load:
            # When fetch width > 1, by definition we cannot load
            # 0 (only fw or fw - 1), but we need to handle this immediate
            # pass through in these cases
            if self._num_load == 0:
                self._num_items = self._push.extend(self._num_items.width)
            else:
                self._num_items = self._num_load
        # One can technically push while a parallel
        # read is happening...
        elif self._parallel_read:
            if self._push:
                self._num_items = 1
            else:
                self._num_items = 0
        elif self._write & ~self._read:
            self._num_items = self._num_items + 1
        elif ~self._write & self._read:
            self._num_items = self._num_items - 1


if __name__ == "__main__":
    dut = RegFIFO(data_width=16,
                  depth=64,
                  width_mult=4)
    verilog(dut, filename="reg_fifo.sv")
