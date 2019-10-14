from kratos import *

class FIFOControl(Generator):
    def __init__(self, data_width, banks, mem_depth):
        super().__init__("fifo_control")

        self.data_width = data_width
        self.banks = banks
        self.mem_depth = mem_depth
        self.mem_addr_width = clog2(self.mem_depth)
        self.bank_addr_width = clog2(self.banks)
        self.full_addr = self.mem_addr_width + self.bank_addr_width

        ##### PORT DEFS: begin
        self._clk = self.clock("clk")
        self._clk_en = self.input("clk_en", 1)
        self._reset = self.reset("reset")
        self._flush = self.input("flush", 1)

        self._ren = self.input("ren", 1)
        self._wen = self.input("wen", 1)
        self._data_in = self.input("data_in", self.data_width)
        self._data_out = self.output("data_out", self.data_width)

        self._almost_empty = self.output("almost_empty", 1)
        self._almost_full = self.output("almost_full", 1)
        self._empty = self.output("empty", 1)
        self._full = self.output("full", 1)
        self._valid = self.output("valid", 1)

        self._fifo_to_mem_data = self.output("fifo_to_mem_data", self.data_width, size=self.banks, packed=True, explicit_array=True)
        self._fifo_to_mem_cen = self.output("fifo_to_mem_cen", self.banks)
        self._fifo_to_mem_wen = self.output("fifo_to_mem_wen", self.banks)
        self._fifo_to_mem_addr = self.output("fifo_to_mem_addr", self.mem_addr_width, size=self.banks, explicit_array=True, packed=True)
        self._mem_to_fifo_data = self.input("mem_to_fifo_data", self.data_width, size=self.banks, explicit_array=True, packed=True)

        self._num_words_mem = self.output("num_words_mem", 16)

        self._almost_count = self.input("almost_count", 4)
        self._circular_en = self.input("circular_en", 1)
        self._depth = self.input("depth", 16)
        ##### PORT DEFS: end

        ##### LOCAL VARIABLES: begin
        self._almost_count_extended = self.var("almost_count_extended", 16)
        self._read_addr = self.var("read_addr", self.full_addr)
        self._write_addr = self.var("write_addr", self.full_addr)
        
        self._read_addr_mem = self.var("read_addr_mem", self.mem_addr_width)
        self._write_addr_mem = self.var("write_addr_mem", self.mem_addr_width)

        self._ren_mem = self.var("ren_mem", self.banks)
        self._wen_mem = self.var("wen_mem", self.banks)
        self._cen_mem = self.var("cen_mem", self.banks)

        self._write_buffed = self.var("write_buffed", self.banks)
        self._write_buff = self.var("write_buff", self.data_width, size=self.banks, explicit_array=True, packed=True)
        self._write_buff_addr = self.var("write_buff_addr", self.mem_addr_width, size=self.banks, explicit_array=True, packed=True)

        self._data_out_sel = self.var("data_out_sel", self.data_width, size=self.banks, explicit_array=True, packed=True)
        self._data_addr = self.var("data_addr", self.mem_addr_width, size=self.banks, explicit_array=True, packed=True)

        self._next_num_words_mem = self.var("next_num_words_mem", 16)
        self._init_stage = self.var("init_stage", 1)
        self._read_to_write = self.var("read_to_write", 1)
        self._passthru = self.var("passthru", 1)

        self._passthru_reg = self.var("passthru_reg", self.data_width)
        self._wen_mem_en = self.var("wen_mem_en", self.banks)
        self._data_out_reg = self.var("data_out_reg", self.data_width)
        self._ren_mem_reg = self.var("ren_mem_reg", self.banks)
        self._same_bank = self.var("same_bank", self.banks)

        self._empty_d1 = self.var("empty_d1", 1)
        self._write_d1 = self.var("write_d1", 1)
        ##### LOCAL VARIABLES: end

        ##### GENERATION LOGIC: begin
        self.wire(self._read_addr_mem, self._read_addr[self.full_addr - 1, self.bank_addr_width])
        self.wire(self._write_addr_mem, self._write_addr[self.full_addr - 1, self.bank_addr_width])
        self.wire(self._almost_count_extended, concat(const(0, 12), self._almost_count))
        self.wire(self._almost_empty, self._num_words_mem <= self._almost_count_extended)
        self.wire(self._almost_full, self._num_words_mem >= (self._depth - self._almost_count_extended))

        self.wire(self._empty, self._num_words_mem == 0)
        self.wire(self._full, self._num_words_mem == self._depth)

        self.wire(self._same_bank, self._ren_mem & self._wen_mem)
        self.wire(self._cen_mem, self._ren_mem | self._wen_mem_en)

        self.wire(self._ren_mem[0], self._ren & ((self._read_addr[self.bank_addr_width - 1, 0] == 0) | self._init_stage))
        for i in range(self.banks-1):
            self.wire(self._ren_mem[i+1], self._ren & (self._read_addr[self.bank_addr_width - 1, 0] == (i+1)))

        self.wire(self._wen_mem[0], self._wen & ((self._write_addr[self.bank_addr_width - 1, 0] == 0) | self._init_stage))
        for i in range(self.banks-1):
            self.wire(self._wen_mem[i+1], self._wen & (self._write_addr[self.bank_addr_width - 1, 0] == (i+1)))

        for i in range(self.banks):
            self.wire(self._wen_mem_en[i], (self._wen_mem[i] & ~self._same_bank[i]) | self._write_buffed[i])

        for i in range(self.banks):
            self.wire(self._fifo_to_mem_data[i], ternary(self._write_buffed[i], self._write_buff[i], self._data_in))

        self.wire(self._fifo_to_mem_cen, self._cen_mem)
        self.wire(self._fifo_to_mem_wen, self._wen_mem_en)
        self.wire(self._fifo_to_mem_addr, self._data_addr)
        self.wire(self._data_out_sel, self._mem_to_fifo_data)


        self.add_code(self.empty_d1_reg)
        self.add_code(self.write_d1_reg)
        self.add_code(self.valid_update)
        self.add_code(self.num_words_update)
        ##### GENERATION LOGIC: end

    @always((posedge, "clk"), (posedge, "reset"))
    def empty_d1_reg(self):
        if self._reset:
            self._empty_d1 = 0
        elif self._clk_en:
            self._empty_d1 = ~self._flush & self._empty

    @always((posedge, "clk"), (posedge, "reset"))
    def write_d1_reg(self):
        if self._reset:
            self._write_d1 = 0
        elif self._clk_en:
            self._write_d1 = ~self._flush & self._wen

    @always((posedge, "clk"), (posedge, "reset"))
    def valid_update(self):
        if self._reset:
            self._valid = 0
        elif self._clk_en:
            self._valid = ~self._flush & (self._ren & (~self._empty | self._wen))

    @always((posedge, "clk"), (posedge, "reset"))
    def num_words_update(self):
        if self._reset:
            self._num_words_mem = 0
        elif self._clk_en:
            if self._flush:
                self._num_words_mem = 0
            else:
                self._num_words_mem = self._num_words_mem + self._next_num_words_mem

    def next_num_words_comb(self):
        if self._ren & self._wen:
            self._next_num_words_mem = 0
        elif self._ren & ~self._empty:
            self._next_num_words_mem = -1
        elif self._wen & ~self._full:
            self._next_num_words_mem = 1
        else:
            self._next_num_words_mem = 0


if __name__ == "__main__":
    fifo_dut = FIFOControl()
    verilog(fifo_dut, filename="fifo_control.sv", check_active_high=False)