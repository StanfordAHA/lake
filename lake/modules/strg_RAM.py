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
                 memory_width=16,
                 memory_depth=512,
                 rw_same_cycle=False,  # Same as separate addresses
                 read_delay=1,
                 addr_width=16,
                 prioritize_write=True):
        super().__init__("strg_fifo", debug=True)

        # Generation parameters
        self.banks = banks
        self.data_width = data_width
        self.memory_width = memory_width
        self.memory_depth = memory_depth
        self.rw_same_cycle = rw_same_cycle
        self.read_delay = read_delay
        self.addr_width = addr_width
        self.fw_int = int(self.memory_width / self.data_width)
        self.prioritize_write = prioritize_write
        self.bank_addr_offset = clog2(self.fw_int) + clog2(self.memory_depth)
        self.bank_width = clog2(self.banks)

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

        # Set addresses to storage
        for i in range(self.banks):
            self.add_code(self.set_wen_addr, idx=i)
            self.add_code(self.set_ren_addr, idx=i)
        # Now deal with a shared address vs separate addresses
        if self.rw_same_cycle:
            # Separate
            self._wen_addr_out = self.output("wen_addr_out", self.addr_width,
                                             size=self.banks,
                                             explicit_array=True,
                                             packed=True)
            self._ren_addr_out = self.output("ren_addr_out", self.addr_width,
                                             size=self.banks,
                                             explicit_array=True,
                                             packed=True)
            self.wire(self._wen_addr_out, self._wen_addr)
            self.wire(self._ren_addr_out, self._ren_addr)
        else:
            self._addr_out = self.output("addr_out", self.addr_width,
                                         size=self.banks,
                                         explicit_array=True,
                                         packed=True)
            # If sharing the addresses, send read addr with priority
            for i in range(self.banks):
                self.wire(self._addr_out[i],
                          kts.ternary(self._wen_to_strg[i], self._wen_addr[i], self._ren_addr[i]))


        self._rd_bank = self.var("rd_bank", max(1, clog2(self.banks)))
        self.set_read_bank()
        self._rd_valid = self.var("rd_valid", 1)
        self.set_read_valid()
        self.wire(self._valid_out, self._rd_valid)

        # Fetch width of 1 is simpler...
        if self.fw_int == 1:
            # Set data to storage
            if self.banks == 1:
                self.wire(self._wen_to_strg, self._wen)
                self.wire(self._ren_to_strg, self._ren)
                self.wire(self._data_to_strg[0], self._data_in)
            else:
                for i in range(self.banks):
                    self.wire(self._data_to_strg[i], self._data_in)
                    self.add_code(self.decode_wen, idx=i)
                    self.add_code(self.decode_ren, idx=i)
            self.wire(self._data_out, self._data_from_strg[self._rd_bank])
        else:
            # Otherwise implement the state machine for read-modify-write
            pass


    def set_read_bank(self):
        if self.banks == 1:
            self.wire(self._rd_bank, kts.const(0, 1))
        else:
            # The read bank is comb if no delay, otherwise delayed
            if self.read_delay == 1:
                @always_ff((posedge, "clk"), (negedge, "rst_n"))
                def read_bank_ff():
                    if ~self._rst_n:
                        self._rd_bank = 0
                    else:
                        self._rd_bank = self._rd_addr[self.bank_addr_offset + self.bank_width - 1, self.bank_addr_offset]
                self.add_code(read_bank_ff)
            else:
                @always_comb
                def read_bank_comb():
                    self._rd_bank = self._rd_addr[self.bank_addr_offset + self.bank_width - 1, self.bank_addr_offset]
                self.add_code(read_bank_comb)

    def set_read_valid(self):
        # The read bank is comb if no delay, otherwise delayed
        if self.read_delay == 1:
            if self.rw_same_cycle:
                @always_ff((posedge, "clk"), (negedge, "rst_n"))
                def read_valid_ff():
                    if ~self._rst_n:
                        self._rd_valid = 0
                    else:
                        # Don't need write priority if both go at once
                        self._rd_valid = self._ren
                self.add_code(read_valid_ff)
            else:
                @always_ff((posedge, "clk"), (negedge, "rst_n"))
                def read_valid_ff():
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
                def read_valid_ff():
                    if ~self._rst_n:
                        self._rd_valid = 0
                    else:
                        # Assumes write priority
                        self._rd_valid = self._ren & ~self._wen
                self.add_code(read_valid_ff)

    @always_comb
    def decode_wen(self, idx):
        self._wen_to_strg[idx] = self._wen & (self._wr_addr[self.bank_addr_offset + self.bank_width - 1, self.bank_addr_offset] == idx)

    @always_comb
    def decode_ren(self, idx):
        self._ren_to_strg[idx] = self._ren & (self._rd_addr[self.bank_addr_offset + self.bank_width - 1, self.bank_addr_offset] == idx)





    @always_comb
    def send_writes(self, idx):
        # Send a write to a bank if the read to that bank isn't happening (unless you can do both)
        # further, we make sure there is a queued write, or current buffer is
        # full and there is an incoming push
        # and there is stuff in memory to be read
        self._wen_to_strg[idx] = ((~self._ren_to_strg[idx] | kts.const(int(self.rw_same_cycle), 1)) &
                                  (self._queued_write[idx] |  # Already queued a write...
                                  (((self._front_occ == self.fw_int) & self._push &  # Grossness
                                    (~self._front_pop)) &
                                   (self._curr_bank_wr == idx))))

    # Technically the ren should be one hot TODO: make onehot assertion
    @always_comb
    def send_reads(self, idx):
        # Send a read to a bank if there is only read_delay items left and it is being read
        self._ren_to_strg[idx] = ((((self._back_occ == self.read_delay) | self._fw_is_1)) &
                                  (self._curr_bank_rd == idx) &  # Proper bank
                                  (self._pop | ((self._back_occ == 0) & (self._back_num_load == 0))) &
                                  ((self._num_words_mem > 1) | ((self._num_words_mem == 1) & ~self._back_pl)))

    # Num load to back is 0 unless we are doing a parallel load
    # in which case we can count it out
    @always_comb
    def set_back_num_load(self):
        if self._back_pl:
            self._back_num_load = kts.ternary(self._pop,
                                              kts.const(self.fw_int - 1, self._back_num_load.width),
                                              kts.const(self.fw_int, self._back_num_load.width))
        else:
            self._back_num_load = 0

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_back_occ(self):
        if ~self._rst_n:
            self._back_occ = 0
        elif self._back_pl:
            if self._back_num_load == 0:
                self._back_occ = self._back_push.extend(self._back_occ.width)
            else:
                self._back_occ = self._back_num_load
        elif self._back_push & ~self._back_pop:
            self._back_occ = self._back_occ + 1
        elif ~self._back_push & self._back_pop:
            self._back_occ = self._back_occ - 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_front_occ(self):
        if ~self._rst_n:
            self._front_occ = 0
        elif self._front_par_read:
            if self._front_push:
                self._front_occ = 1
            else:
                self._front_occ = 0
        elif self._front_push & ~self._front_pop:
            self._front_occ = self._front_occ + 1
        elif ~self._front_push & self._front_pop:
            self._front_occ = self._front_occ - 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_parallel_ld_delay_1(self):
        if ~self._rst_n:
            self._ren_delay = 0
        else:
            self._ren_delay = self._ren_to_strg.r_or()

    @always_comb
    def set_front_par_read(self):
        # If the front is full and not being drained, need to write out to write queue or memory
        self._front_par_read = (self._front_occ == self.fw_int) & self._push & ~self._front_pop

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_num_words_mem(self):
        if ~self._rst_n:
            self._num_words_mem = 0
        elif ~self._back_pl & self._front_par_read:
            self._num_words_mem = self._num_words_mem + 1
        elif self._back_pl & ~self._front_par_read:
            self._num_words_mem = self._num_words_mem - 1

    # This signal drives the automatic passthrough to the back end
    # from the front end - bypassing the storage altogether
    # ----------------------------------------------------------
    # Basically check the nothing is in memory and
    # the back fifo isn't actually using a recent read
    # Also make sure that either the back fifo isn't full or it's being read from
    @always_comb
    def set_front_pop(self):
        self._front_pop = (((self._num_words_mem == 0) | ((self._num_words_mem == 1) & self._back_pl)) &
                           # You can pop the front if the memory read is being entirely bypassed (fw == 1)
                           (~self._back_pl | (self._back_pl & (self._back_num_load == 0))) &
                           (~(self._back_occ == self.fw_int) | self._pop) &
                           (~(self._front_occ == 0) | self._push))

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_write_queue(self, idx):
        if ~self._rst_n:
            self._write_queue[idx] = 0
            self._queued_write[idx] = 0
        # If the parallel read is happening, but the write is not
        # going through, then you must have a conflict and be queueing
        # Only write the queue for the current bank
        elif self._front_par_read & ~self._wen_to_strg[idx] & (self._curr_bank_wr == idx):
            self._write_queue[idx] = self._front_combined
            self._queued_write[idx] = 1
        elif self._wen_to_strg[idx]:
            self._queued_write[idx] = 0

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_curr_bank_rd(self):
        if ~self._rst_n:
            self._curr_bank_rd = 0
        elif self._ren_to_strg.r_or():
            if self._curr_bank_rd == self.banks - 1:
                self._curr_bank_rd = 0
            else:
                self._curr_bank_rd = self._curr_bank_rd + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_prev_bank_rd(self):
        if ~self._rst_n:
            self._prev_bank_rd = 0
        # elif self._front_par_read:
        else:
            self._prev_bank_rd = self._curr_bank_rd

    @always_comb
    def set_num_items(self):
        self._num_items = ((self._num_words_mem * self.fw_int).extend(self.data_width) +
                           self._front_occ.extend(self.data_width) +
                           self._back_occ.extend(self.data_width))

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_wen_addr(self, idx):
        if ~self._rst_n:
            self._wen_addr[idx] = 0
        elif self._wen_to_strg[idx]:
            self._wen_addr[idx] = self._wen_addr[idx] + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_ren_addr(self, idx):
        if ~self._rst_n:
            self._ren_addr[idx] = 0
        elif self._ren_to_strg[idx]:
            self._ren_addr[idx] = self._ren_addr[idx] + 1


if __name__ == "__main__":
    stg_dut = StrgRAM()
    verilog(stg_dut, filename="strg_ram.sv",
            additional_passes={"lift config regs": lift_config_reg})
