from kratos import *
from functools import reduce
import operator


class DoubleBufferControl(Generator):
    def __init__(self,
                 data_width,
                 mem_depth,
                 banks,
                 iterator_support):
        super().__init__("doublebuffer_control")

        self.data_width = data_width
        self.banks = banks
        self.mem_depth = mem_depth
        self.mem_addr_width = clog2(self.mem_depth)
        self.bank_addr_width = clog2(self.banks)
        self.full_addr = self.mem_addr_width + self.bank_addr_width
        self.iterator_support = iterator_support

        # PORT DEFS: begin
        self._clk = self.clock("clk")
        self._clk_en = self.input("clk_en", 1)
        self._reset = self.reset("reset")
        self._flush = self.input("flush", 1)

        self._ren = self.input("ren", 1)
        self._wen = self.input("wen", 1)
        self._data_in = self.input("data_in", self.data_width)
        self._data_out = self.output("data_out", self.data_width)
        self._addr_in = self.input("addr_in", self.data_width)

        self._doublebuffer_data_in = self.output("doublebuffer_data_in",
                                                 self.data_width, size=self.banks,
                                                 explicit_array=True,
                                                 packed=True)
        self._doublebuffer_cen_mem = self.output("doublebuffer_cen_mem", self.banks)
        self._doublebuffer_wen_mem = self.output("doublebuffer_wen_mem", self.banks)
        self._doublebuffer_addr_mem = self.output("doublebuffer_addr_mem",
                                                  self.mem_addr_width,
                                                  size=self.banks,
                                                  explicit_array=True,
                                                  packed=True)
        self._doublebuffer_data_out = self.input("doublebuffer_data_out",
                                                 self.data_width,
                                                 size=self.banks,
                                                 explicit_array=True,
                                                 packed=True)

        self._depth = self.input("depth", 16)
        self._valid = self.output("valid", 1)
        self._switch = self.input("switch", 1)

        self._chain_idx = self.input("chain_idx", 4)

        self._arbitrary_addr = self.input("arbitrary_addr", 1)
        self._starting_addr = self.input("starting_addr", 16)
        self._iter_cnt = self.input("iter_cnt", 32)
        self._dimensionality = self.input("dimensionality", 4)

        self._stride = self.input("stride", 16,
                                  size=self.iterator_support,
                                  packed=True,
                                  explicit_array=True)
        self._range = self.input("range", 32,
                                 size=self.iterator_support,
                                 packed=True,
                                 explicit_array=True)

        self._rate_matched = self.input("rate_matched", 1)
        self._stencil_width = self.input("stencil_width", 32)
        # PORT DEFS: end

        # LOCAL VARIABLES: begin
        self._current_loc = self.var("current_loc", 16,
                                     size=self.iterator_support,
                                     packed=True,
                                     explicit_array=True)
        self._valid_arb = self.var("valid_arb", 1)
        self._init_state = self.var("init_state", 1)
        self._addr = self.var("addr", self.mem_addr_width)
        self._ping_npong = self.var("ping_npong", 1)
        self._read_addr = self.var("read_addr", 16)
        self._write_addr = self.var("write_addr", 16)
        self._dim_counter = self.var("dim_counter", 32,
                                     size=self.iterator_support,
                                     packed=True,
                                     explicit_array=True)

        self._update = self.var("update", self.iterator_support)
        self._strt_addr = self.var("strt_addr", 16)
        self._read_cnt = self.var("read_cnt", 32)
        self._firstn = self.var("firstn", self.data_width,
                                size=self.banks,
                                explicit_array=True,
                                packed=True)
        self._take_the_flop = self.var("take_the_flop", 1)
        self._autoswitch = self.var("autoswitch", 1)
        self._read_done = self.var("read_done", 1)
        self._read_done_thresh = self.var("read_done_thresh", 1)
        self._write_done = self.var("write_done", 1)
        self._write_done_thresh = self.var("write_done_thresh", 1)

        self._last_line_gate = self.var("last_line_gate", 1)
        self._read_first = self.var("read_first", 1)
        self._next_take_the_flop = self.var("next_take_the_flop", 1)
        self._write_in_range = self.var("write_in_range", 1)
        self._read_in_range = self.var("read_in_range", 1)
        self._read_in_range_d1 = self.var("read_in_range_d1", 1)
        self._read_mux = self.var("read_mux", 1)
        self._counter_update = self.var("counter_update", 1)
        self._calc_addr = self.var("calc_addr", 16)
        # LOCAL VARIABLES: end

        # GENERATION LOGIC: begin
        self.wire(self._counter_update,
                  ((self._init_state | (self._depth == 0)) &
                   self._read_mux & ~self._read_done &
                   ~self._take_the_flop & ~self._read_done_thresh) |
                  (self._init_state & self._read_mux & self._take_the_flop))

        self.wire(self._read_mux,
                  ternary(self._rate_matched,
                          self._wen,
                          self._ren))
        self.wire(self._autoswitch,
                  ~self._arbitrary_addr &
                  (self._write_done | self._write_done_thresh) &
                  (self._read_done | self._read_done_thresh | ~self._init_state) &
                  ~(self._depth == const(0, self._depth.width)))

        self.wire(self._strt_addr, self._starting_addr[self.data_width - 1, 0])
        self.wire(self._addr, self._addr_in[self.mem_addr_width - 1, 0])
        self.wire(self._last_line_gate,
                  ternary(self._stencil_width == 0,
                          const(1, 1),
                          (self._read_cnt >= (self._stencil_width - 1))))

        # Figure out when read iterations are done
        self.wire(self._read_done, (self._read_cnt == (self._iter_cnt - 1)) & self._read_mux)
        self.add_code(self.read_done_thresh_update)

        # Figure out when write iterations are done
        self.wire(self._write_done, (self._write_addr == (self._depth - 1)) & self._wen)
        self.add_code(self.write_done_thresh_update)

        # Check that reads/writes are in the range of the modules
        self.wire(self._write_in_range,
                  self._write_addr[self.mem_addr_width + 4 - 1,
                                   self.mem_addr_width] == self._chain_idx)
        self.wire(self._read_in_range,
                  self._read_addr[self.mem_addr_width + 4 - 1,
                                  self.mem_addr_width] == self._chain_idx)

        # Set valid to be this junk of logic
        self.wire(self._valid,
                  ternary(self._arbitrary_addr,
                          self._valid_arb,
                          self._last_line_gate &
                          self._read_mux &
                          (self._init_state | (self._depth == 0)) &
                          self._read_in_range_d1 & ~self._read_done_thresh))

        self.add_code(self.valid_arb_update)
        self.add_code(self.read_in_range_d1_update)

        for i in range(self.banks):
            self.wire(self._doublebuffer_data_in[i], self._data_in)

            self.wire(self._doublebuffer_cen_mem[i],
                      (self._wen & (self._ping_npong == i)) |
                      self._flush |
                      self._switch |
                      self._autoswitch |
                      (self._read_mux & ~(self._ping_npong == i)))

            self.wire(self._doublebuffer_wen_mem[i],
                      (self._ping_npong == i) &
                      (self._wen & ~self._write_done_thresh) &
                      self._write_in_range &
                      ~(self._depth == 0))

            self.wire(self._doublebuffer_addr_mem[i],
                      ternary(self._ping_npong == i,
                              self._write_addr[self.mem_addr_width - 1, 0],
                              self._read_addr[self.mem_addr_width - 1, 0]))

        self.wire(self._data_out,
                  ternary(self._take_the_flop,
                          self._firstn[~self._ping_npong],
                          self._doublebuffer_data_out[~self._ping_npong]))

        # Set read address
        self.wire(self._read_addr,
                  ternary(self._arbitrary_addr,
                          concat(const(0, 7), self._addr),
                          self._calc_addr))

        self.add_code(self.calc_addr_comb)

        # Set update vector
        self.wire(self._update[0], self._init_state | (self._depth == 0))
        for i in range(self.iterator_support - 1):
            self.wire(self._update[i + 1],
                      (self._dim_counter[i] == (self._range[i] - 1)) & self._update[i])

        # Handle the prebuffer on the first element to a bank
        self.add_code(self.firstn_update)

        self.wire(self._next_take_the_flop,
                  ternary(self._autoswitch,
                          const(1, 1),
                          ternary(self._take_the_flop & ~self._read_mux,
                                  const(1, 1),
                                  const(0, 1))))

        self.add_code(self.read_first_update)
        self.add_code(self.take_the_flop_update)
        self.add_code(self.init_state_update)

        self.add_code(self.dim_counter_update)
        self.add_code(self.current_loc_update)
        self.add_code(self.ping_npong_update)

        self.add_code(self.write_addr_update)
        self.add_code(self.read_cnt_update)

        # GENERATION LOGIC: end

    def calc_addr_comb(self):
        self._calc_addr = reduce(operator.add,
                                 [(ternary(const(i, self._dimensionality.width) <
                                           self._dimensionality,
                                           self._current_loc[i],
                                           const(0, self._calc_addr.width)))
                                  for i in range(self.iterator_support)] + [self._strt_addr])

    @always((posedge, "clk"), (posedge, "reset"))
    def read_done_thresh_update(self):
        if self._reset:
            self._read_done_thresh = 0
        elif self._clk_en:
            if self._flush:
                self._read_done_thresh = 0
            else:
                if self._autoswitch | self._switch:
                    self._read_done_thresh = 0
                elif self._read_done:
                    self._read_done_thresh = 1

    @always((posedge, "clk"), (posedge, "reset"))
    def write_done_thresh_update(self):
        if self._reset:
            self._write_done_thresh = 0
        elif self._clk_en:
            if self._flush:
                self._write_done_thresh = 0
            else:
                if self._autoswitch | self._switch:
                    self._write_done_thresh = 0
                elif self._write_done:
                    self._write_done_thresh = 1

    @always((posedge, "clk"), (posedge, "reset"))
    def valid_arb_update(self):
        if self._reset:
            self._valid_arb = 0
        elif self._clk_en:
            if self._flush | self._switch:
                self._valid_arb = 0
            else:
                self._valid_arb = self._read_in_range & self._read_mux & self._init_state

    @always((posedge, "clk"), (posedge, "reset"))
    def read_in_range_d1_update(self):
        if self._reset:
            self._read_in_range_d1 = 0
        elif self._clk_en:
            # Handle some weird zero-depth stuff
            if self._flush:
                self._read_in_range_d1 = self._strt_addr[self.mem_addr_width + 4 - 1,
                                                         self.mem_addr_width] == self._chain_idx
            else:
                self._read_in_range_d1 = self._read_in_range

    def read_addr_comb(self):
        return 0

    def update_comb(self):
        return 0

    @always((posedge, "clk"), (posedge, "reset"))
    def firstn_update(self):
        if self._reset:
            self._firstn = 0
        elif self._clk_en:
            if self._flush:
                self._firstn = 0
            elif self._wen & (self._write_addr == self._strt_addr):
                # TODO: MAKE THE LOGIC GENERAL
                self._firstn[0] = ternary(~self._ping_npong, self._data_in, self._firstn[0])
                self._firstn[1] = ternary(self._ping_npong, self._data_in, self._firstn[1])

    @always((posedge, "clk"), (posedge, "reset"))
    def read_first_update(self):
        if self._reset:
            self._read_first = 1
        elif self._clk_en:
            if self._flush:
                self._read_first = 1
            elif self._take_the_flop & self._read_mux:
                self._read_first = 0

    @always((posedge, "clk"), (posedge, "reset"))
    def take_the_flop_update(self):
        if self._reset:
            self._take_the_flop = 0
        elif self._clk_en:
            if self._flush:
                self._take_the_flop = 0
            else:
                self._take_the_flop = self._next_take_the_flop

    @always((posedge, "clk"), (posedge, "reset"))
    def init_state_update(self):
        if self._reset:
            self._init_state = 0
        elif self._clk_en:
            if self._flush:
                self._init_state = 0
            elif self._autoswitch | self._switch:
                self._init_state = 1

    @always((posedge, "clk"), (posedge, "reset"))
    def dim_counter_update(self):
        if self._reset:
            self._dim_counter = 0
        elif self._clk_en:
            if self._flush:
                self._dim_counter[0] = ternary(self._depth == 0,
                                               concat(const(0, 31), self._range[0] > 1),
                                               const(0, self._dim_counter[0].width))
                for i in range(self.iterator_support - 1):
                    self._dim_counter[i + 1] = 0
            elif self._autoswitch | self._switch:
                self._dim_counter[0] = concat(const(0, 31), self._range[0] > 1)
                for i in range(self.iterator_support - 1):
                    self._dim_counter[i + 1] = 0
            elif self._counter_update & (i < self._dimensionality):
                for i in range(self.iterator_support):
                    if self._update[i]:
                        if self._dim_counter[i] == (self._range[i] - 1):
                            self._dim_counter[i] = 0
                        else:
                            self._dim_counter[i] = self._dim_counter[i] + 1

    @always((posedge, "clk"), (posedge, "reset"))
    def current_loc_update(self):
        if self._reset:
            self._current_loc = 0
        elif self._clk_en:
            if self._flush:
                self._current_loc[0] = ternary(self._depth == 0,
                                               self._stride[0],
                                               const(0, self._current_loc[0].width))
                for i in range(self.iterator_support - 1):
                    self._current_loc[i + 1] = 0
            elif self._autoswitch | self._switch:
                self._current_loc[0] = self._stride[0]
                for i in range(self.iterator_support - 1):
                    self._current_loc[i + 1] = 0
            elif self._counter_update:
                for i in range(self.iterator_support):
                    if self._update[i] & (i < self._dimensionality):
                        if self._dim_counter[i] == (self._range[i] - 1):
                            self._current_loc[i] = 0
                        else:
                            self._current_loc[i] = self._current_loc[i] + self._stride[i]

    @always((posedge, "clk"), (posedge, "reset"))
    def ping_npong_update(self):
        if self._reset:
            self._ping_npong = 0
        elif self._clk_en:
            if self._flush:
                self._ping_npong = 0
            elif self._autoswitch | self._switch:
                self._ping_npong = ~self._ping_npong

    @always((posedge, "clk"), (posedge, "reset"))
    def write_addr_update(self):
        if self._reset:
            self._write_addr = 0
        elif self._clk_en:
            if self._flush:
                self._write_addr = 0
            elif self._autoswitch | self._switch:
                self._write_addr = 0
            elif self._wen & ~self._write_done & ~self._write_done_thresh:
                self._write_addr = self._write_addr + 1

    @always((posedge, "clk"), (posedge, "reset"))
    def read_cnt_update(self):
        if self._reset:
            self._read_cnt = 0
        elif self._clk_en:
            if self._flush:
                self._read_cnt = 0
            elif self._autoswitch | self._switch:
                self._read_cnt = 0
            elif self._counter_update:
                self._read_cnt = self._read_cnt + 1


if __name__ == "__main__":
    db_dut = DoubleBufferControl(16, 512, 2, 6)
    verilog(db_dut, filename="doublebuffer_control.sv", check_active_high=False)
