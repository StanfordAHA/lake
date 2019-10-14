from kratos import *

class DoubleBufferControl(Generator):
    def __init__(self, data_width, mem_depth, banks, iterator_support):
        super().__init__("doublebuffer_control")

        self.data_width = data_width
        self.banks = banks
        self.mem_depth = mem_depth
        self.mem_addr_width = clog2(self.mem_depth)
        self.bank_addr_width = clog2(self.banks)
        self.full_addr = self.mem_addr_width + self.bank_addr_width
        self.iterator_support = iterator_support

        ##### PORT DEFS: begin
        self._clk = self.clock("clock")
        self._clk_en = self.input("clk_en", 1)
        self._reset = self.reset("reset")
        self._flush = self.input("flush", 1)

        self._ren = self.input("ren", 1)
        self._wen = self.input("wen", 1)
        self._data_in = self.input("data_in", self.data_width)
        self._data_out = self.output("data_out", self.data_width)
        self._addr_in = self.input("addr_in", self.data_width)

        self._doublebuffer_data_in = self.output("doublebuffer_data_in", self.data_width, size=self.banks, explicit_array=True, packed=True)
        self._doublebuffer_cen_mem = self.output("doubelbuffer_cen_mem", self.banks)
        self._doublebuffer_wen_mem = self.output("doublebuffer_wen_mem", self.banks)
        self._doublebuffer_addr_mem = self.output("doublebuffer_addr_mem", self.mem_addr_width, size=self.banks, explicit_array=True, packed=True)
        self._doublebuffer_data_out = self.input("doublebuffer_data_out", self.data_width, size=self.banks, explicit_array=True, packed=True)

        self._depth = self.input("depth", 16)
        self._valid = self.output("valid", 1)
        self._switch = self.input("switch", 1)
        
        self._chain_idx = self.input("chain_idx", 4)

        self._arbitrary_addr = self.input("arbitrary_addr", 1)
        self._starting_addr = self.input("starting_addr", 16)
        self._iter_cnt = self.input("iter_cnt", 32)
        self._dimensionality = self.input("dimensionality", 4)
        
        self._stride = self.input("stride", 16, size=self.iterator_support, packed=True, explicit_array=True)
        self._range = self.input("range", 32, size=self.iterator_support, packed=True, explicit_array=True)

        self._rate_matched = self.input("rate_matched", 1)
        self._stencil_width = self.input("stencil_width", 16)
        ##### PORT DEFS: end

        ##### LOCAL VARIABLES: begin
        self._current_loc = self.var("current_loc", 16, size=self.iterator_support, packed=True, explicit_array=True)
        self._valid_arb = self.var("valid_arb", 1)
        self._init_state = self.var("init_state", 1)
        self._addr = self.var("addr", self.mem_addr_width)
        self._ping_npong = self.var("ping_npong", 1)
        self._read_addr = self.var("read_addr", 16)
        self._write_addr = self.var("write_addr", 16)
        self._dim_counter = self.var("dim_counter", 32, size=self.iterator_support, packed=True, explicit_array=True)

        self._update = self.var("update", self.iterator_support)
        self._strt_addr = self.var("strt_addr", 16)
        self._read_cnt = self.var("read_cnt", 32)
        self._firstn = self.var("firstn", self.data_width, size=self.banks, explicit_array=True, packed=True)
        self._take_the_flop = self.var("take_the_flop", 1)
        self._autoswitch = self.var("autoswitch", 1)
        self._read_done = self.var("read_done", 1)
        self._read_done_thresh = self.var("read_done_thresh", 1)
        self._write_done = self.var("write_done", 1)
        self._write_done_thresh = self.var("write_done_thresh", 1)

        self._last_line_gate = self.var("last_line_gate", 1)
        self._
        ##### LOCAL VARIABLES: end

        ##### GENERATION LOGIC: begin

        ##### GENERATION LOGIC: end
