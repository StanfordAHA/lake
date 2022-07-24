from kratos import *
from lake.utils.util import increment, decrement
from lake.attributes.config_reg_attr import ConfigRegAttr


class ReservationFIFO(Generator):
    '''
    This module generates register-based reservation FIFOs. These are useful
    when we need to keep a pipeline filled but have alternate paths
    '''
    def __init__(self,
                 data_width,
                 depth=8,
                 defer_hrdwr_gen=False,
                 num_per=2,
                 min_depth=0):

        super().__init__(f"reservation_fifo_depth_{depth}_w_{data_width}", debug=True)

        # self.data_width = self.parameter("data_width", 16)
        # self.data_width.value = data_width
        self.data_width = data_width
        self.depth = depth
        self.defer_hrdwr_gen = defer_hrdwr_gen
        self.min_depth = min_depth
        self.hardware_genned = False
        self.num_per = num_per

        # Allow fifo to get depth 0
        assert not (depth & (depth - 1)) or depth == 0, "FIFO depth needs to be a power of 2"

        # CLK and RST
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._clk_en = self.input("clk_en", 1)

        # INPUTS
        # self._data_in = [self.input(f"data_in_{i}", self.data_width, packed=True) for i in range(self.num_per)]
        self._data_in = [self.input(f"data_in_{i}", self.data_width, packed=True) for i in range(self.num_per)]
        self._fill_data_in = self.input(f"fill_data_in", self.data_width, packed=True)
        self._data_out = [self.output(f"data_out_{i}", self.data_width, packed=True) for i in range(self.num_per)]

        self._push_alloc = self.input("push_alloc", 1)
        self._push_reserve = self.input("push_reserve", 1)
        self._push_fill = self.input("pushfull", 1)
        self._pop = self.input("pop", 1)
        self._valid = self.output("valid", 1)
        self._empty = self.output("empty", 1)
        self._full = self.output("full", 1)
        # self._almost_full = self.output("almost_full", 1)

        self.ptr_width = max(1, clog2(self.depth))

        if self.defer_hrdwr_gen is False:
            self.generate_hardware()

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
    def data_out_comb(self):
        if(self._passthru):
            self._data_out = self._data_in
        else:
            self._data_out = self._reg_array[self._rd_ptr]

    @always_comb
    def valid_comb(self):
        self._valid = ((~self._empty) | self._passthru)
        # self._valid = self._pop & ((~self._empty) | self._passthru)

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

    def set_min_depth(self):
        self.set_depth(self.min_depth)

    def set_depth(self, new_depth):
        assert not (new_depth & (new_depth - 1)) or new_depth == 0, "Unsupported depth"
        assert new_depth >= self.min_depth, f"Minimum allowed depth {self.min_depth}: Tried to use {new_depth}"
        self.depth = new_depth
        self.ptr_width = max(1, clog2(self.depth))
        self.name = f"reg_fifo_depth_{self.depth}_w_{self.data_width}"

    def generate_hardware(self):
        # Routine to do the real hardware generation once the
        # parameters have been finalized
        self.name = f"reg_fifo_depth_{self.depth}_w_{self.data_width}"

        # Depth of 0 is basically passthru...
        if self.depth == 0:
            self.wire(self._data_out, self._data_in)
            # Data is valid if you are pushing
            self.wire(self._valid, self._push)
            # If you are not pushing, it is empty
            self.wire(self._empty, ~self._push)
            # It is full if you are not popping
            self.wire(self._full, ~self._pop)
            # Almost full cannot be inferred just on the ready/valid interface
            self.wire(self._almost_full, ~self._pop)
        else:
            self._rd_ptr = self.var("rd_ptr", self.ptr_width)
            self._wr_ptr = self.var("wr_ptr", self.ptr_width)
            self._read = self.var("read", 1)
            self._write = self.var("write", 1)
            self._reg_array = self.var("reg_array",
                                       self.data_width,
                                       size=(self.depth,
                                             self.width_mult),
                                       packed=True,
                                       explicit_array=True)

            self._passthru = self.var("passthru", 1)

            self._num_items = self.var("num_items", clog2(self.depth) + 1)
            # self.wire(self._full, (self._wr_ptr + 1) == self._rd_ptr)
            self.wire(self._full, self._num_items == self.depth)
            # Experiment to cover latency
            # self.wire(self._almost_full, self._num_items >= (self.depth - self.almost_full_diff))
            # self.wire(self._empty, self._wr_ptr == self._rd_ptr)
            self.wire(self._empty, self._num_items == 0)

            self.wire(self._read, self._pop & ~self._passthru & ~self._empty)

            # Disallow passthru for now to prevent combinational loops
            self.wire(self._passthru, const(0, 1))
            # self.wire(self._passthru, self._pop & self._push & self._empty)

            # Should only write

            # Boilerplate Add always @(posedge clk, ...) blocks
            if self.parallel:
                self.add_code(self.set_num_items_parallel)
                self.add_code(self.reg_array_ff_parallel)
                self.add_code(self.wr_ptr_ff_parallel)
                self.add_code(self.rd_ptr_ff_parallel)
                self.wire(self._parallel_out, self._reg_array)
                self.wire(self._write,
                          self._push & ~self._passthru & (~self._full | (self._parallel_read)))
            else:
                # self.wire(self._write, self._push & ~self._passthru & (~self._full | self._pop))
                # Don't want to write when full at all for decoupling
                self.wire(self._write, self._push & ~self._passthru & (~self._full))
                self.add_code(self.set_num_items)
                self.add_code(self.reg_array_ff)
                self.add_code(self.wr_ptr_ff)
                self.add_code(self.rd_ptr_ff)
            self.add_code(self.data_out_comb)
            self.add_code(self.valid_comb)

        self.hardware_genned = True

    def get_hardware_genned(self):
        return self.hardware_genned


if __name__ == "__main__":
    # dut = RegFIFO(data_width=16,
    #               depth=64,
    #               width_mult=4)
    # verilog(dut, filename="reg_fifo_no_defer.sv")

    dut2 = ReservationFIFO(data_width=16,
                           depth=64,
                           width_mult=4,
                           defer_hrdwr_gen=True)
    dut2.set_depth(8)
    dut2.generate_hardware()

    verilog(dut2, filename="reg_fifo_with_defer.sv")
