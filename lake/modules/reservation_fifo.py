from kratos import *
from lake.utils.util import add_counter, register
from lake.attributes.config_reg_attr import ConfigRegAttr
import kratos as kts


class ReservationFIFO(kts.Generator):
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

        super().__init__(f"reservation_fifo_depth_{depth}_w_{data_width}_num_per_{num_per}", debug=True)

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
        self._data_in_pre = [self.input(f"data_in_{i}", self.data_width, packed=True) for i in range(self.num_per)]
        self._data_in = self.var("data_in_packed", self.data_width, size=self.num_per, packed=True, explicit_array=True)
        for i_ in range(self.num_per):
            self.wire(self._data_in[i_], self._data_in_pre[i_])
        self._fill_data_in = self.input(f"fill_data_in", self.data_width, packed=True)
        self._data_out_post = [self.output(f"data_out_{i}", self.data_width, packed=True) for i in range(self.num_per)]
        # self._data_out = [self.output(f"data_out_{i}", self.data_width, packed=True) for i in range(self.num_per)]
        self._data_out = self.var(f"data_out", self.data_width, size=self.num_per, packed=True, explicit_array=True)
        for i_ in range(self.num_per):
            self.wire(self._data_out_post[i_], self._data_out[i_])

        self._push_alloc = self.input("push_alloc", 1)
        self._push_reserve = self.input("push_reserve", 1)
        self._push_fill = self.input("push_fill", 1)
        self._pop = self.input("pop", 1)
        self._valid = self.output("valid", 1)
        self._empty = self.output("empty", 1)
        self._full = self.output("full", 1)
        # self._almost_full = self.output("almost_full", 1)

        self.ptr_width = max(1, kts.clog2(self.depth))

        if self.defer_hrdwr_gen is False:
            self.generate_hardware()

    # @always_ff((posedge, "clk"), (negedge, "rst_n"))
    # def rd_ptr_ff(self):
    #     if ~self._rst_n:
    #         self._rd_ptr = 0
    #     elif self._read:
    #         self._rd_ptr = self._rd_ptr + 1

    # @always_ff((posedge, "clk"), (negedge, "rst_n"))
    # def wr_ptr_ff(self):
    #     if ~self._rst_n:
    #         self._wr_ptr = 0
    #     elif self._write:
    #         if self._wr_ptr == (self.depth - 1):
    #             self._wr_ptr = 0
    #         else:
    #             self._wr_ptr = self._wr_ptr + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def valid_mask_ff(self):
        if ~self._rst_n:
            self._valid_mask = 0
        # elif self._write_fill & self._write_reserve_final & self.:
        else:
            if self._write_fill:
                self._valid_mask[self._write_ptr] = 1
            if self._write_reserve_final:
                self._valid_mask[self._reserve_ptr] = 1
            if self._read:
                self._valid_mask[self._read_ptr] = 0

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def reg_array_ff(self):
        if ~self._rst_n:
            self._reg_array = 0
        else:
            if self._write_fill:
                self._reg_array[self._write_ptr] = self._fill_data_in
            if self._write_reserve:
                self._reg_array[self._reserve_ptr] = self._data_in

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def reg_array_ff_wide(self):
        if ~self._rst_n:
            self._reg_array = 0
        else:
            if self._write_fill:
                self._reg_array[self._write_ptr][0] = self._fill_data_in
            if self._write_reserve:
                self._reg_array[self._reserve_ptr][self._item_ptr] = self._data_in[self._item_ptr]

    @always_comb
    def data_out_comb(self):
        self._data_out = self._reg_array[self._read_ptr]

    @always_comb
    def valid_comb(self):
        # self._valid = ((~self._empty) | self._passthru)
        self._valid = self._valid_mask[self._read_ptr]
        # self._valid = self._pop & ((~self._empty) | self._passthru)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_num_items(self):
        if ~self._rst_n:
            self._num_items = 0
        elif self._write_alloc & ~self._read:
            self._num_items = self._num_items + 1
        elif ~self._write_alloc & self._read:
            self._num_items = self._num_items - 1

    @always_comb
    def find_next_reserve_low(self):
        self._next_0_valid_low_found = 0
        self._next_0_valid_low = 0
        self._next_0_valid_low_done = 0
        for idx_ in range(self.depth):
            if ~self._next_0_valid_low_done:
                if (idx_ < self._reserve_ptr):
                    if (self._valid_mask[idx_] == kts.const(0, 1)):
                        self._next_0_valid_low_found = 1
                        self._next_0_valid_low = idx_
                        self._next_0_valid_low_done = 1

    @always_comb
    def find_next_reserve_high(self):
        self._next_0_valid_high_found = 0
        self._next_0_valid_high = 0
        self._next_0_valid_high_done = 0
        for idx_ in range(self.depth):
            if ~self._next_0_valid_high_done:
                if (idx_ > self._reserve_ptr):
                    if (self._valid_mask[idx_] == kts.const(0, 1)):
                        self._next_0_valid_high_found = 1
                        self._next_0_valid_high = idx_
                        self._next_0_valid_high_done = 1

    def set_min_depth(self):
        self.set_depth(self.min_depth)

    def set_depth(self, new_depth):
        assert not (new_depth & (new_depth - 1)) or new_depth == 0, "Unsupported depth"
        assert new_depth >= self.min_depth, f"Minimum allowed depth {self.min_depth}: Tried to use {new_depth}"
        self.depth = new_depth
        self.ptr_width = max(1, clog2(self.depth))
        self.name = f"reservation_fifo_depth_{self.depth}_w_{self.data_width}_num_per_{self.num_per}"

    def generate_hardware(self):

        assert self.depth > 0, f"Depth of 0 not supported..."
        # Routine to do the real hardware generation once the
        # parameters have been finalized
        self.name = f"reservation_fifo_depth_{self.depth}_w_{self.data_width}_num_per_{self.num_per}"

        # Simultaneously alloc + fill at once (normal fifo behavior) - set valid mask to high
        self._write_fill = self.var("write_fill", 1)
        # Allocate a block but don't give it data - don't fill valid mask yet...
        self._write_alloc = self.var("write_alloc", 1)
        self._write_reserve = self.var("write_reserve", 1)
        self._write_reserve_final = self.var("write_reserve_final", 1)
        self._read = self.var("read", 1)

        # self._filled_mask = self.var("filled_mask", self.depth)
        self._valid_mask = self.var("valid_mask", self.depth)

        self.item_ptr_width = kts.clog2(self.num_per)
        if self.num_per == 1:
            self.item_ptr_width = 1
            self._item_ptr = self.var("item_ptr", 1)
            self.wire(self._item_ptr, kts.const(0, 1))
            self._inc_item_ptr = self.var("inc_item_ptr", 1)
            self._clr_item_ptr = self.var("clr_item_ptr", 1)
            self.wire(self._inc_item_ptr, self._push_reserve)
            self.wire(self._clr_item_ptr, self._push_reserve & (self._item_ptr == (self.num_per - 1)))
        else:

            self._inc_item_ptr = self.var("inc_item_ptr", 1)
            self._clr_item_ptr = self.var("clr_item_ptr", 1)
            self._item_ptr = add_counter(self, name="item_ptr_addr",
                                         bitwidth=self.item_ptr_width,
                                         increment=self._inc_item_ptr,
                                         clear=self._clr_item_ptr)

            # This is based on protocol, but it is assumed that the push reserve
            # will only be called on the pre allocated blocks, so we can increment it freely
            # every time it is asserted as we already made room for it.
            self.wire(self._inc_item_ptr, self._push_reserve)
            self.wire(self._clr_item_ptr, self._push_reserve & (self._item_ptr == (self.num_per - 1)))

        self._inc_read_ptr = self.var("inc_read_ptr", 1)
        self._clr_read_ptr = self.var("clr_read_ptr", 1)
        self._read_ptr = add_counter(self, name="read_ptr_addr",
                                     bitwidth=self.ptr_width,
                                     increment=self._inc_read_ptr,
                                     clear=self._clr_read_ptr)

        # Increment the read ptr whenever there is a read
        # It will automatically flow back over by power-of-2 depth assertions
        self.wire(self._inc_read_ptr, self._read)
        self.wire(self._clr_read_ptr, kts.const(0, 1))

        self._inc_write_ptr = self.var("inc_write_ptr", 1)
        self._clr_write_ptr = self.var("clr_write_ptr", 1)
        self._write_ptr = add_counter(self, name="write_ptr_addr",
                                      bitwidth=self.ptr_width,
                                      increment=self._inc_write_ptr,
                                      clear=self._clr_write_ptr)
        # Increment the write ptr whenever there is an allocation, so this
        # just follows normal behavior like a normal fifo write pointer
        self.wire(self._inc_write_ptr, self._write_alloc | self._write_fill)
        self.wire(self._clr_write_ptr, kts.const(0, 1))

        # Reserve pointer needs a more interesting set of logic...
        # Needs to jump to the next location without the valid bit set
        # By virtue of full/write pointer it won't ever jump past
        # self._reserve_ptr = self.var("reserve_ptr", self.ptr_width)
        self._next_0_valid_low = self.var("next_0_valid_low", self.ptr_width)
        self._next_0_valid_low_found = self.var("next_0_valid_low_found", 1)
        self._next_0_valid_low_done = self.var("next_0_valid_low_done", 1)
        self._next_0_valid_high = self.var("next_0_valid_high", self.ptr_width)
        self._next_0_valid_high_found = self.var("next_0_valid_high_found", 1)
        self._next_0_valid_high_done = self.var("next_0_valid_high_done", 1)
        self._next_0_valid = self.var("next_0_valid", self.ptr_width)
        # Actually is a next 0
        self._jump_next_0 = self.var("jump_next_0", 1)
        self.wire(self._jump_next_0, self._next_0_valid_high_found | self._next_0_valid_low_found)

        # Is a next 0 and should actually be jumping
        self._enable_reserve_ptr = self.var("enable_reserve_ptr", 1)
        self._reserve_ptr_val = self.var("reserve_ptr_val", self.ptr_width)
        # self.wire(self._enable_reserve_ptr, self._jump_next_0 & self._write_reserve_final)
        self.wire(self._enable_reserve_ptr, self._write_reserve_final | (self._write_fill & (self._reserve_ptr_val == self._write_ptr)))
        # self.wire(self._enable_reserve_ptr, self._write_reserve_final | (self._write_fill & ((self._reserve_ptr_val == self._write_ptr) |
        #                                           (~self._next_0_valid_high_found & ~self._next_0_valid_low_found) |
        #                                           (kts.ternary(self._next_0_valid_high_found,
        #                                                        self._next_0_valid_high == self._write_ptr,
        #                                                        self._next_0_valid_low == self._write_ptr)))))
        self._reserve_ptr = register(self, self._next_0_valid, enable=self._enable_reserve_ptr)
        self.wire(self._reserve_ptr_val, self._reserve_ptr)

        # Complicated.
        # Firstly, if the reserve pointer is where a fill is happening, just move it along with it
        # Secondly, if they don't match and there's no place to go, go to the current write_ptr
        self.wire(self._next_0_valid, kts.ternary(self._write_fill & ((self._reserve_ptr == self._write_ptr) |
                                                  (~self._next_0_valid_high_found & ~self._next_0_valid_low_found) |
                                                  (kts.ternary(self._next_0_valid_high_found,
                                                               self._next_0_valid_high == self._write_ptr,
                                                               self._next_0_valid_low == self._write_ptr))),
                                                  self._write_ptr + 1,
                                                  kts.ternary(~self._next_0_valid_high_found & ~self._next_0_valid_low_found,
                                                              self._write_ptr,
                                                              kts.ternary(self._next_0_valid_high_found,
                                                                          self._next_0_valid_high,
                                                                          self._next_0_valid_low))))

        # self._rd_ptr = self.var("rd_ptr", self.ptr_width)
        # self._wr_ptr = self.var("wr_ptr", self.ptr_width)
        # self._read = self.var("read", 1)
        # self._write = self.var("write", 1)
        self._reg_array = self.var("reg_array",
                                   self.data_width,
                                   size=(self.depth,
                                         self.num_per),
                                   packed=True,
                                   explicit_array=True)

        # self.wire(self._full, (self._wr_ptr + 1) == self._rd_ptr)
        # Experiment to cover latency
        # self.wire(self._almost_full, self._num_items >= (self.depth - self.almost_full_diff))
        # self.wire(self._empty, self._wr_ptr == self._rd_ptr)
        self._num_items = self.var("num_items", clog2(self.depth) + 1)
        self.wire(self._full, self._num_items == self.depth)
        self.wire(self._empty, self._num_items == 0)

        # Should only write
        # self.wire(self._write, self._push & ~self._passthru & (~self._full | self._pop))
        # Don't want to write when full at all for decoupling
        self.wire(self._write_fill, self._push_fill & self._push_alloc & ~self._full)
        self.wire(self._write_alloc, self._push_alloc & ~self._full)

        # if self.num_per == 1:
        #     # Clearing the item pointer is the same thing as the final write_reserve
        #     self.wire(self._write_reserve, self._inc_item_ptr)
        #     self.wire(self._write_reserve_final, self._clr_item_ptr)
        # else:
        # Clearing the item pointer is the same thing as the final write_reserve
        self.wire(self._write_reserve, self._inc_item_ptr)
        self.wire(self._write_reserve_final, self._clr_item_ptr)
        self.wire(self._read, self._pop & self._valid_mask[self._read_ptr])

        # self.wire(self._read, self._pop & ~self._empty)
        # self.wire(self._write, self._push & (~self._full))
        self.add_code(self.set_num_items)
        if self.num_per == 1:
            self.add_code(self.reg_array_ff)
        else:
            self.add_code(self.reg_array_ff_wide)
        # self.add_code(self.wr_ptr_ff)
        # self.add_code(self.rd_ptr_ff)
        self.add_code(self.data_out_comb)
        self.add_code(self.valid_comb)
        self.add_code(self.valid_mask_ff)
        self.add_code(self.find_next_reserve_high)
        self.add_code(self.find_next_reserve_low)

        self._inc_reserve_count = self.var("inc_reserve_count", 1)
        self.wire(self._inc_reserve_count, self._write_alloc & ~self._write_fill)
        self._reserve_count = add_counter(self, "reserve_count", 16, increment=self._inc_reserve_count)

        self.hardware_genned = True

    def get_hardware_genned(self):
        return self.hardware_genned


if __name__ == "__main__":

    num_per = 1

    dut = ReservationFIFO(data_width=16,
                          depth=64,
                          num_per=num_per,
                          defer_hrdwr_gen=False)

    verilog(dut, filename=f"reservation_fifo_num_per_{num_per}.sv")
