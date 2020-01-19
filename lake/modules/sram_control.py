from kratos import *


class SRAMControl(Generator):
    def __init__(self,
                 data_width,
                 banks,
                 mem_depth):
        super().__init__("sram_control")

        # PORT DEFS: begin
        self.data_width = data_width
        self.banks = banks
        self.mem_depth = mem_depth
        self.mem_addr_width = clog2(self.mem_depth)

        self._clk = self.clock("clk")
        self._clk_en = self.input("clk_en", 1)
        self._reset = self.reset("reset")
        self._flush = self.input("flush", 1)

        self._data_in = self.input("data_in", data_width)
        self._wen = self.input("wen", 1)
        self._data_out = self.output("data_out", data_width)
        self._ren = self.input("ren", 1)
        self._addr_in = self.input("addr_in", 16)

        self._sram_to_mem_data = self.output("sram_to_mem_data",
                                             self.data_width,
                                             size=self.banks,
                                             explicit_array=True,
                                             packed=True)
        self._sram_to_mem_cen = self.output("sram_to_mem_cen",
                                            self.banks)
        self._sram_to_mem_wen = self.output("sram_to_mem_wen",
                                            self.banks)
        self._sram_to_mem_addr = self.output("sram_to_mem_addr",
                                             self.mem_addr_width,
                                             size=self.banks,
                                             explicit_array=True,
                                             packed=True)
        self._mem_to_sram_data = self.input("mem_to_sram_data",
                                            self.data_width,
                                            size=self.banks,
                                            explicit_array=True,
                                            packed=True)
        # PORT DEFS: end

        self.bank_addr_width = clog2(self.banks)
        self.full_addr = self.mem_addr_width + self.bank_addr_width

        # LOCAL SIGNALS: begin
        self._addr = self.var("addr", self.full_addr)
        self._bank_seld = self.var("bank_seld", self.banks)
        self._sram_to_mem_ren_reg = self.var("sram_to_mem_ren_reg", banks)
        self._data_out_reg = self.var("data_out_reg", self.data_width)
        # LOCAL SIGNALS: end

        # GENERATION LOGIC: begin
        self.wire(self._addr, self._addr_in[self.full_addr - 1, 0])
        for i in range(self.banks):
            self.wire(self._bank_seld[i],
                      self._addr[self.full_addr - 1,
                                 self.full_addr - self.bank_addr_width] == i)
            self.wire(self._sram_to_mem_data[i], self._data_in)
            self.wire(self._sram_to_mem_cen[i], self._bank_seld[i] & (self._wen | self._ren))
            self.wire(self._sram_to_mem_wen[i], self._bank_seld[i] & self._wen)
            self.wire(self._sram_to_mem_addr[i], self._addr[self.mem_addr_width - 1, 0])

        self.bank_sel_comb = CombinationalCodeBlock(self)
        out_switch = SwitchStmt(self._sram_to_mem_ren_reg)
        for i in range(self.banks):
            out_switch.case_(2 ** i, self._data_out.assign(self._mem_to_sram_data[i]))
        out_switch.case_(None, self._data_out.assign(self._data_out_reg))
        self.bank_sel_comb.add_stmt(out_switch)
        self.add_code(self.data_out_reg_update)
        self.add_code(self.ren_reg_update)
        # GENERATION LOGIC: end

    @always((posedge, "clk"), (posedge, "reset"))
    def data_out_reg_update(self):
        if self._reset:
            self._data_out_reg = 0
        elif self._clk_en:
            if self._flush:
                self._data_out_reg = 0
            else:
                self._data_out_reg = self._data_out

    @always((posedge, "clk"), (posedge, "reset"))
    def ren_reg_update(self):
        if self._reset:
            self._sram_to_mem_ren_reg = 0
        elif self._clk_en:
            if self._flush:
                self._sram_to_mem_ren_reg = 0
            else:
                self._sram_to_mem_ren_reg = self._bank_seld & concat(*[self._ren
                                                                       for i in range(self.banks)])


# Python main guard
if __name__ == "__main__":
    dut = SRAMControl(16, 2, 512)
    verilog(dut, filename="sram_control.sv", check_active_high=False)
