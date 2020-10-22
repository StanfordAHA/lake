from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.modules.reg_fifo import RegFIFO
import kratos as kts


class StrgRAM(Generator):
    '''
    Storage RAM
        Does ROM/RAM from storage
    '''
    def __init__(self,
                 data_width=16,
                 banks=1,
                 memory_width=64,
                 memory_depth=512,
                 num_tiles=1,
                 rw_same_cycle=False,  # Same as separate addresses
                 read_delay=1,
                 addr_width=16,
                 prioritize_write=True):
        super().__init__("strg_ram", debug=True)

        # Generation parameters
        self.banks = banks
        self.data_width = data_width
        self.memory_width = memory_width
        self.memory_depth = memory_depth
        self.num_tiles = num_tiles
        self.rw_same_cycle = rw_same_cycle
        self.read_delay = read_delay
        self.addr_width = addr_width
        self.fw_int = int(self.memory_width / self.data_width)
        self.prioritize_write = prioritize_write
        self.bank_width = clog2(self.banks)
        self.word_width = max(1, clog2(self.fw_int))
        self.mem_addr_width = clog2(self.num_tiles * self.memory_depth)
        self.b_a_off = clog2(self.fw_int) + clog2(self.num_tiles * self.memory_depth)

        # assert banks > 1 or rw_same_cycle is True or self.fw_int > 1, \
        #     "Can't sustain throughput with this setup. Need potential bandwidth for " + \
        #     "1 write and 1 read in a cycle - try using more banks or a macro that supports 1R1W"

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs + Outputs
        self._wen = self.input("wen", 1)
        self._ren = self.input("ren", 1)
        self._data_in = self.input("data_in", self.data_width)
        self._wr_addr_in = self.input("wr_addr_in", self.addr_width)
        self._rd_addr_in = self.input("rd_addr_in", self.addr_width)

        self._wr_addr = self.var("wr_addr", self.addr_width)
        self._rd_addr = self.var("rd_addr", self.addr_width)

        # Separate addressing...
        if self.rw_same_cycle:
            self.wire(self._wr_addr, self._wr_addr_in)
            self.wire(self._rd_addr, self._rd_addr_in)
        # Use the wr addr for both in this case...
        else:
            self.wire(self._wr_addr, self._wr_addr_in)
            self.wire(self._rd_addr, self._wr_addr_in)

        self._data_out = self.output("data_out", self.data_width)
        self._valid_out = self.output("valid_out", 1)

        # get relevant signals from the storage banks
        self._data_from_strg = self.input("data_from_strg", self.data_width,
                                          size=(self.banks,
                                                self.fw_int),
                                          explicit_array=True,
                                          packed=True)

        self._wen_addr = self.var("wen_addr", self.addr_width,
                                  size=self.banks,
                                  explicit_array=True,
                                  packed=True)

        self._ren_addr = self.var("ren_addr", self.addr_width,
                                  size=self.banks,
                                  explicit_array=True,
                                  packed=True)

        self._data_to_strg = self.output("data_to_strg", self.data_width,
                                         size=(self.banks,
                                               self.fw_int),
                                         explicit_array=True,
                                         packed=True)

        self._wen_to_strg = self.output("wen_to_strg", self.banks)
        self._ren_to_strg = self.output("ren_to_strg", self.banks)

        self._addr_out = self.output("addr_out", self.mem_addr_width,
                                     size=self.banks,
                                     explicit_array=True,
                                     packed=True)

        self._rd_bank = self.var("rd_bank", max(1, clog2(self.banks)))
        self.set_read_bank()
        self._rd_valid = self.var("rd_valid", 1)
        self.set_read_valid()
        if self.fw_int == 1:
            self.wire(self._valid_out, self._rd_valid)

        # Fetch width of 1 is simpler...
        if self.fw_int == 1:
            # Set data to storage
            if self.banks == 1:
                self.wire(self._wen_to_strg, self._wen)
                self.wire(self._ren_to_strg, self._ren)
                self.wire(self._data_to_strg[0], self._data_in)
                self.wire(self._addr_out[0],
                          kts.ternary(self._wen_to_strg[0],
                                      self._wr_addr[self.mem_addr_width - 1, 0],
                                      self._rd_addr[self.mem_addr_width - 1, 0]))
            else:
                for i in range(self.banks):
                    self.wire(self._data_to_strg[i], self._data_in)
                    self.add_code(self.decode_wen, idx=i)
                    self.add_code(self.decode_ren, idx=i)
                    self.wire(self._addr_out[i],
                              kts.ternary(self._wen_to_strg[i],
                                          self._wr_addr[self.mem_addr_width - 1, 0],
                                          self._rd_addr[self.mem_addr_width - 1, 0]))
            self.wire(self._data_out, self._data_from_strg[self._rd_bank])
        elif self.read_delay == 1:

            self._data_to_write = self.var("data_to_write", self.data_width)
            self._addr_to_write = self.var("addr_to_write", self.addr_width)
            self.add_code(self.set_dat_to_write)
            self.add_code(self.set_addr_to_write)

            self._write_gate = self.var("write_gate", 1)
            self._read_gate = self.var("read_gate", 1)

            self._data_combined = self.var("data_combined", self.data_width,
                                           size=self.fw_int,
                                           explicit_array=True,
                                           packed=True)

            for i in range(self.banks):
                self.wire(self._data_to_strg[i], self._data_combined)

            # read-modify-write implies we need to stall upstream
            self._ready = self.output("ready", 1)
            # Otherwise implement the state machine for read-modify-write
            self.rmw_fsm = self.add_fsm("r_w_seq", reset_high=False)
            IDLE = self.rmw_fsm.add_state("IDLE")
            READ = self.rmw_fsm.add_state("READ")
            MODIFY = self.rmw_fsm.add_state("MODIFY")
            DEFAULT = self.rmw_fsm.add_state("_DEFAULT")
            self.rmw_fsm.output(self._ready)
            self.rmw_fsm.output(self._valid_out)
            self.rmw_fsm.output(self._data_out)
            self.rmw_fsm.output(self._write_gate)
            self.rmw_fsm.output(self._read_gate)

            # In IDLE we go to a read state if reading, and modify state
            # if writing....
            IDLE.next(IDLE, ~(self._wen) & ~(self._ren))
            IDLE.next(READ, self._ren & ~self._wen)
            IDLE.next(MODIFY, self._wen)
            # OUT
            IDLE.output(self._ready, 1)
            IDLE.output(self._valid_out, 0)
            IDLE.output(self._data_out, 0)
            IDLE.output(self._write_gate, 0)
            IDLE.output(self._read_gate, 1)

            # In READ, we effectively use the same transitions as IDLE
            READ.next(IDLE, ~self._wen & ~self._ren)
            READ.next(READ, self._ren & ~self._wen)
            READ.next(MODIFY, self._wen)
            # OUT
            READ.output(self._ready, 1)
            READ.output(self._valid_out, 1)
            READ.output(self._data_out,
                        self._data_from_strg[self._rd_bank][self._addr_to_write[self.word_width - 1, 0]])
            READ.output(self._write_gate, 0)
            READ.output(self._read_gate, 1)

            # In MODIFY we always go back to idle
            MODIFY.next(IDLE, const(1, 1))
            MODIFY.output(self._ready, 0)
            MODIFY.output(self._valid_out, 0)
            MODIFY.output(self._data_out, 0)
            MODIFY.output(self._write_gate, 1)
            MODIFY.output(self._read_gate, 0)

            # In DEFAULT we always stick in DEFAULT because it's over...
            DEFAULT.next(DEFAULT, const(1, 1))
            DEFAULT.output(self._ready, 0)
            DEFAULT.output(self._valid_out, 0)
            DEFAULT.output(self._data_out, 0)
            DEFAULT.output(self._write_gate, 0)
            DEFAULT.output(self._read_gate, 0)

            self.rmw_fsm.set_start_state(IDLE)

            if self.banks == 1:
                self.wire(self._ren_to_strg, (self._wen | self._ren) & self._read_gate)
                self.wire(self._wen_to_strg, self._write_gate)
            else:
                for i in range(self.banks):
                    self.add_code(self.set_wen_rmw, idx=i)
                    self.add_code(self.set_ren_rmw, idx=i)

            for i in range(self.banks):
                self.add_code(self.set_addr_rmw, idx=i)
            for i in range(self.fw_int):
                self.add_code(self.set_data_combined, idx=i)
        # If read delay is 0, we can rmw in the same cycle (TIMING?)

    def set_read_bank(self):
        if self.banks == 1:
            self.wire(self._rd_bank, kts.const(0, 1))
        else:
            # The read bank is comb if no delay, otherwise delayed
            if self.read_delay == 1:
                @always_ff((posedge, "clk"), (negedge, "rst_n"))
                def read_bank_ff(self):
                    if ~self._rst_n:
                        self._rd_bank = 0
                    else:
                        self._rd_bank = \
                            self._rd_addr[self.b_a_off + self.bank_width - 1, self.b_a_off]
                self.add_code(read_bank_ff)
            else:
                @always_comb
                def read_bank_comb(self):
                    self._rd_bank = \
                        self._rd_addr[self.b_a_off + self.bank_width - 1, self.b_a_off]
                self.add_code(read_bank_comb)

    def set_read_valid(self):
        # The read bank is comb if no delay, otherwise delayed
        if self.read_delay == 1:
            if self.rw_same_cycle:
                @always_ff((posedge, "clk"), (negedge, "rst_n"))
                def read_valid_ff(self):
                    if ~self._rst_n:
                        self._rd_valid = 0
                    else:
                        # Don't need write priority if both go at once
                        self._rd_valid = self._ren
                self.add_code(read_valid_ff)
            else:
                @always_ff((posedge, "clk"), (negedge, "rst_n"))
                def read_valid_ff(self):
                    if ~self._rst_n:
                        self._rd_valid = 0
                    else:
                        # Assumes write priority
                        self._rd_valid = self._ren & ~self._wen
                self.add_code(read_valid_ff)
        else:
            if self.rw_same_cycle:
                self.wire(self._rd_valid, self._ren)
            else:
                self.wire(self._rd_valid, self._ren & ~self._wen)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_dat_to_write(self):
        if ~self._rst_n:
            self._data_to_write = 0
        else:
            self._data_to_write = self._data_in

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_addr_to_write(self):
        if ~self._rst_n:
            self._addr_to_write = 0
        else:
            self._addr_to_write = self._wr_addr

    @always_comb
    def decode_wen(self, idx):
        self._wen_to_strg[idx] = \
            self._wen & (self._wr_addr[self.b_a_off + self.bank_width - 1, self.b_a_off] == idx)

    @always_comb
    def decode_ren(self, idx):
        self._ren_to_strg[idx] = \
            self._ren & (self._rd_addr[self.b_a_off + self.bank_width - 1, self.b_a_off] == idx)

    @always_comb
    def set_ren_rmw(self, idx):
        self._ren_to_strg[idx] = \
            ((self._wen | self._ren) & self._read_gate &
             (self._rd_addr[self.b_a_off + self.bank_width - 1, self.b_a_off] == idx))

    @always_comb
    def set_wen_rmw(self, idx):
        self._wen_to_strg[idx] = \
            self._write_gate & (self._addr_to_write[self.b_a_off + self.bank_width - 1, self.b_a_off] == idx)

    @always_comb
    def set_addr_rmw(self, idx):
        self._addr_out[idx] = self._rd_addr[self.mem_addr_width + self.word_width - 1, self.word_width]
        # If we are performing the write
        if self._wen & ~self._write_gate:
            self._addr_out[idx] = self._wr_addr[self.mem_addr_width + self.word_width - 1, self.word_width]
        elif self._write_gate:
            self._addr_out[idx] = \
                self._addr_to_write[self.mem_addr_width + self.word_width - 1, self.word_width]

    @always_comb
    def set_data_combined(self, idx):
        # If the word matches the index, use the data to write
        # (replace the word)
        if self._addr_to_write[self.word_width - 1, 0] == idx:
            self._data_combined[idx] = self._data_to_write
        # Otherwise keep the data
        else:
            self._data_combined[idx] = self._data_from_strg[self._rd_bank][idx]


if __name__ == "__main__":
    stg_dut = StrgRAM()
    verilog(stg_dut, filename="strg_ram.sv",
            additional_passes={"lift config regs": lift_config_reg})
