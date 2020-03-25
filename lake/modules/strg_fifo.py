from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.modules.reg_fifo import RegFIFO
import kratos as kts


class StrgFIFO(Generator):
    '''
    Storage Fifo
        This module performs FIFO using multiple banks of storage and
        some small register fifos in the core

        Note: for this module to function, we assume that there is an address
        generator on both the output and input that uses a bank interleaving pattern.
        In this way, we can prevent any conflicts from occuring
    '''
    def __init__(self,
                 data_width=16,
                 banks=2,
                 memory_width=64,
                 rw_same_cycle=False,
                 read_delay=1):
        super().__init__("strg_fifo", debug=True)

        assert banks > 1 or rw_same_cycle is True, \
            "Can't sustain throughput with this setup. Need potential bandwidth for " + \
            "1 write and 1 read in a cycle - try using more banks or a macro that supports 1R1W"

        # Generation parameters
        self.banks = banks
        self.data_width = data_width
        self.memory_width = memory_width
        self.rw_same_cycle = rw_same_cycle
        self.read_delay = read_delay
        self.fw_int = int(self.memory_width / self.data_width)

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs + Outputs
        self._push = self.input("push", 1)
        self._data_in = self.input("data_in", self.data_width)
        self._pop = self.input("pop", 1)

        self._data_out = self.output("data_out", self.data_width)
        self._valid_out = self.output("valid_out", 1)
        self._empty = self.output("empty", 1)
        self._full = self.output("full", 1)

        # get relevant signals from the storage banks
        self._data_from_strg = self.input("data_from_strg", self.data_width,
                                          size=(self.banks,
                                                self.fw_int),
                                          explicit_array=True,
                                          packed=True)

        self._front_combined = self.var("front_combined", self.data_width,
                                        size=self.fw_int,
                                        explicit_array=True,
                                        packed=True)

        self._data_to_strg = self.output("data_to_strg", self.data_width,
                                         size=(self.banks,
                                               self.fw_int),
                                         explicit_array=True,
                                         packed=True)

        self._wen_to_strg = self.output("wen_to_strg", self.banks)
        self._ren_to_strg = self.output("ren_to_strg", self.banks)

        self._num_words_mem = self.var("num_words_mem", 16)  # TODO: Change this width based on memory

        if self.banks == 1:
            self._curr_bank_wr = self.var("curr_bank_wr", 1)
            self.wire(self._curr_bank_wr, kts.const(0, 1))
            self._curr_bank_rd = self.var("curr_bank_rd", 1)
            self.wire(self._curr_bank_rd, kts.const(0, 1))
        else:
            self._curr_bank_wr = self.var("curr_bank_wr", kts.clog2(self.banks))
            self._curr_bank_rd = self.var("curr_bank_rd", kts.clog2(self.banks))

        self._write_queue = self.var("write_queue", self.data_width,
                                     size=(self.banks,
                                           self.fw_int),
                                     explicit_array=True,
                                     packed=True)

        # Lets us know if the bank has a write queued up
        self._queued_write = self.var("queued_write", self.banks)

        self._front_data_out = self.var("front_data_out", self.data_width)
        self._front_pop = self.var("front_pop", 1)
        self._front_empty = self.var("front_empty", 1)
        self._front_full = self.var("front_full", 1)
        self._front_valid = self.var("front_valid", 1)
        self._front_par_read = self.var("front_par_read", 1)
        self._front_par_out = self.var("front_par_out", self.data_width,
                                       size=(self.fw_int - 1,
                                             1),
                                       explicit_array=True,
                                       packed=True)

        self._front_rf = RegFIFO(data_width=self.data_width,
                                 width_mult=1,
                                 depth=self.fw_int - 1,
                                 parallel=True,
                                 extern_full_empty=True)

        self.add_child("front_rf", self._front_rf,
                       clk=self._clk,
                       clk_en=kts.const(1, 1),
                       rst_n=self._rst_n,
                       push=self._push,
                       pop=self._front_pop,
                       empty=self._front_empty,
                       full=self._front_full,
                       valid=self._front_valid,
                       parallel_read=self._front_par_read,
                       parallel_load=kts.const(0, 1),  # We don't need to parallel load the front
                       parallel_in=0,  # Same reason as above
                       parallel_out=self._front_par_out,
                       num_load=0)
        self.wire(self._front_rf.ports.data_in[0], self._data_in)
        self.wire(self._front_data_out, self._front_rf.ports.data_out[0])

        self._back_data_in = self.var("back_data_in", self.data_width)
        self._back_data_out = self.var("back_data_out", self.data_width)
        self._back_push = self.var("back_push", 1)
        self._back_empty = self.var("back_empty", 1)
        self._back_full = self.var("back_full", 1)
        self._back_valid = self.var("back_valid", 1)
        self._back_pl = self.var("back_pl", 1)
        self._back_par_in = self.var("back_par_in", self.data_width,
                                     size=(self.fw_int,
                                           1),
                                     explicit_array=True,
                                     packed=True)
        self._back_num_load = self.var("back_num_load", clog2(self.fw_int) + 1)

        self._back_occ = self.var("back_occ", clog2(self.fw_int) + 1)
        self._front_occ = self.var("front_occ", clog2(self.fw_int) + 1)

        self._back_rf = RegFIFO(data_width=self.data_width,
                                width_mult=1,
                                depth=self.fw_int,
                                parallel=True,
                                extern_full_empty=True)

        self.add_child("back_rf", self._back_rf,
                       clk=self._clk,
                       clk_en=kts.const(1, 1),
                       rst_n=self._rst_n,
                       push=self._back_push,
                       pop=self._pop,
                       empty=self._back_empty,
                       full=self._back_full,
                       valid=self._back_valid,
                       parallel_read=kts.const(0, 1),
                       parallel_load=self._back_pl,
                       parallel_in=self._back_par_in,
                       num_load=self._back_num_load)
        self.wire(self._back_rf.ports.data_in[0], self._back_data_in)
        self.wire(self._back_data_out, self._back_rf.ports.data_out[0])
        # send the writes through when a read isn't happening
        for i in range(self.banks):
            self.add_code(self.send_writes, idx=i)
            self.add_code(self.send_reads, idx=i)

        # Set the parallel load to back bank - if no delay it's immediate
        # if not, it's delayed :)
        if self.read_delay == 1:
            self._ren_delay = self.var("ren_delay", 1)
            self.add_code(self.set_parallel_ld_delay_1)
            self.wire(self._back_pl, self._ren_delay)
        else:
            self.wire(self._back_pl, self._ren_to_strg.r_or())

        # Combine front end data - just the items + incoming
        for i in range(self.fw_int - 1):
            self.wire(self._front_combined[i], self._front_par_out[i])
        self.wire(self._front_combined[self.fw_int - 1], self._data_in)

        # prioritize queued writes, otherwise send combined data
        for i in range(self.banks):
            self.wire(self._data_to_strg[i],
                      kts.ternary(self._queued_write[i], self._write_queue[i], self._front_combined))

        # Wire the thin output from front to thin input to back
        self.wire(self._back_data_in, self._front_data_out)
        self.wire(self._back_push, self._front_valid)
        self.add_code(self.set_front_pop)

        # Queue writes
        for i in range(self.banks):
            self.add_code(self.set_write_queue, idx=i)

        # Track number of words in memory
        # if self.read_delay == 1:
        #     self.add_code(self.set_num_words_mem_delay)
        # else:
        self.add_code(self.set_num_words_mem)

        # Track occupancy of the two small fifos
        self.add_code(self.set_front_occ)
        self.add_code(self.set_back_occ)

        if self.banks > 1:
            self.add_code(self.set_curr_bank_wr)
            self.add_code(self.set_curr_bank_rd)
            if self.read_delay == 1:
                self._prev_bank_rd = self.var("prev_bank_rd", kts.clog2(self.banks))
                self.add_code(self.set_prev_bank_rd)

        # Parallel load data to back - based on num_load
        index_into = self._curr_bank_rd
        if self.read_delay == 1:
            index_into = self._prev_bank_rd
        for i in range(self.fw_int - 1):
            # Shift data over if you bypassed from the memory output
            self.wire(self._back_par_in[i],
                      kts.ternary(self._back_num_load == self.fw_int,
                                  self._data_from_strg[index_into][i],
                                  self._data_from_strg[index_into][i + 1]))
        self.wire(self._back_par_in[self.fw_int - 1],
                  kts.ternary(self._back_num_load == self.fw_int,
                              self._data_from_strg[index_into][i],
                              kts.const(0, self.data_width)))

        # Set the parallel read to the front fifo - analogous with trying to write to the memory
        self.add_code(self.set_front_par_read)

        # Set the number being parallely loaded into the register
        self.add_code(self.set_back_num_load)

        # Data out and valid out are (in the general case) just the data and valid from the back fifo
        # In the case where we have a fresh memory read, it would be from that
        self.wire(self._data_out, kts.ternary(self._back_pl, self._back_par_in[0], self._back_data_out))
        self.wire(self._valid_out, kts.ternary(self._back_pl, self._pop, self._back_valid))

        # Do final empty/full
        self._num_items = self.var("num_items", 16)
        self.add_code(self.set_num_items)
        self._fifo_depth = self.input("fifo_depth", 16)
        self._fifo_depth.add_attribute(ConfigRegAttr("Fifo depth..."))
        self.wire(self._empty, self._num_items == 0)
        self.wire(self._full, self._num_items == (self._fifo_depth - 1))

    @always_comb
    def send_writes(self, idx):
        # Send a write to a bank if the read to that bank isn't happening (unless you can do both)
        # further, we make sure there is a queued write, or current buffer is
        # full and there is an incoming push
        # and there is stuff in memory to be read
        self._wen_to_strg[idx] = ((~self._ren_to_strg[idx] | kts.const(int(self.rw_same_cycle), 1)) &
                                  (self._queued_write[idx] |
                                  (((self._front_occ == self.fw_int - 1) & self._push &
                                    ((self._num_words_mem > 0) | ~self._front_pop)) &
                                   (self._curr_bank_wr == idx))))

    # Technically the ren should be one hot TODO: make onehot assertion
    @always_comb
    def send_reads(self, idx):
        # Send a read to a bank if there is only read_delay items left and it is being read
        self._ren_to_strg[idx] = ((self._back_occ == self.read_delay) &
                                  (self._curr_bank_rd == idx) & self._pop & (self._num_words_mem > 0))

    @always_comb
    def set_back_num_load(self):
        self._back_num_load = kts.ternary(self._pop,
                                          kts.const(self.fw_int - 1, self._back_num_load.width),
                                          kts.const(self.fw_int, self._back_num_load.width))

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_back_occ(self):
        if ~self._rst_n:
            self._back_occ = 0
        elif self._back_pl:
            self._back_occ = self._back_num_load
        elif self._back_push & ~self._pop:
            self._back_occ = self._back_occ + 1
        elif ~self._back_push & self._pop:
            self._back_occ = self._back_occ - 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_front_occ(self):
        if ~self._rst_n:
            self._front_occ = 0
        elif self._front_par_read:
            self._front_occ = 0
        elif self._push & ~self._front_pop:
            self._front_occ = self._front_occ + 1
        elif ~self._push & self._front_pop:
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
        self._front_par_read = (self._front_occ == self.fw_int - 1) & self._push & ~self._front_pop

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_num_words_mem(self):
        if ~self._rst_n:
            self._num_words_mem = 0
        # write and not read
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
        self._front_pop = ((self._num_words_mem == 0) &
                           ~self._back_pl &
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
        elif self._front_par_read & ~self._wen_to_strg[idx] & (self._curr_bank_wr):
            self._write_queue[idx] = self._front_combined
            self._queued_write[idx] = 1
        elif self._wen_to_strg[idx]:
            self._queued_write[idx] = 0

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_curr_bank_wr(self):
        if ~self._rst_n:
            self._curr_bank_wr = 0
        elif self._front_par_read:
            if self._curr_bank_wr == self.banks - 1:
                self._curr_bank_wr = 0
            else:
                self._curr_bank_wr = self._curr_bank_wr + 1

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
        elif self._front_par_read:
            self._prev_bank_rd = self._curr_bank_rd

    @always_comb
    def set_num_items(self):
        self._num_items = ((self._num_words_mem * self.fw_int).extend(16) +
                           self._front_occ.extend(16) +
                           self._back_occ.extend(16))


if __name__ == "__main__":
    stg_dut = StrgFIFO()
    verilog(stg_dut, filename="strg_fifo.sv",
            additional_passes={"lift config regs": lift_config_reg})
