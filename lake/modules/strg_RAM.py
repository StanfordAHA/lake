from lake.top.memory_interface import MemoryPort, MemoryPortType
from lake.top.memory_controller import MemoryController
from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.modules.reg_fifo import RegFIFO
from lake.attributes.shared_fifo_attr import SharedFifoAttr
import kratos as kts


class StrgRAM(MemoryController):
    '''
    Storage RAM
        Does ROM/RAM from storage
    '''
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 banks=1,
                 memory_width=64,
                 memory_depth=512,
                 num_tiles=1,
                 rw_same_cycle=False,  # Same as separate addresses
                 read_delay=1,
                 addr_width=16,
                 prioritize_write=True,
                 comply_with_17=True,
                 defer_fifos=True):
        super().__init__(f"strg_ram_{memory_width}_{memory_depth}_delay{read_delay}", debug=True)

        # Generation parameters
        self.banks = banks
        self.data_width = data_width
        self.fifo_depth = fifo_depth
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
        self.defer_fifos = defer_fifos

        self.comply_with_17 = comply_with_17

        # assert banks > 1 or rw_same_cycle is True or self.fw_int > 1, \
        #     "Can't sustain throughput with this setup. Need potential bandwidth for " + \
        #     "1 write and 1 read in a cycle - try using more banks or a macro that supports 1R1W"

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))
        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))
        # Inputs + Outputs
        self._wen = self.input("wen", 1)
        self._ren = self.input("ren", 1)
        self.add_bits = 0
        if self.comply_with_17:
            self.add_bits = 1
        # Mode indicator to tell whether we are using ROM in dense or sparse mode
        self._dense_mode = self.input("dense_mode", 1)
        self._data_in = self.input("data_in", self.data_width + self.add_bits, packed=True)
        self._wr_addr_in = self.input("wr_addr_in", self.addr_width + self.add_bits, packed=True)
        self._rd_addr_in = self.input("rd_addr_in", self.addr_width + self.add_bits, packed=True)

        self._wr_addr = self.var("wr_addr", self.addr_width)
        self._rd_addr_unbuffered = self.var("rd_addr_unbuffered", self.addr_width + self.add_bits, packed=True)
        self._rd_addr = self.var("rd_addr", self.addr_width)

        self.bit_range = (self.data_width - 1, 0)
# ==============================
# INPUT FIFO
# ==============================
        # Read address input fifo control signal 
        self._rd_addr_infifo_pop = self.var("rd_addr_infifo_pop", 1)
        # Read address input fifo output signal 
        self._rd_addr_infifo_out = self.var("rd_addr_infifo_out", self.addr_width + self.add_bits, packed=True)
        self._rd_addr_infifo = RegFIFO(data_width=self.data_width + self.add_bits,
                                       width_mult=1,
                                       depth=self.fifo_depth,
                                       defer_hrdwr_gen=self.defer_fifos)
        self._rd_addr_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self.add_child("rd_addr_infifo",
                       self._rd_addr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       # Ren is connected to up stream sparse primitive's valid
                       # signal
                       push=self._ren,
                       pop=self._rd_addr_infifo_pop,
                       data_in=self._rd_addr_unbuffered,
                       data_out=self._rd_addr_infifo_out)    
        # Unpack the fifo output 
        self._rd_addr_infifo_out_data = self.var("rd_addr_infifo_out_data", self.data_width)
        self._rd_addr_infifo_out_eos = self.var("rd_addr_infifo_out_eos", 1)
        self.wire(self._rd_addr_infifo_out_data, self._rd_addr_infifo_out[self.bit_range])
        self.wire(self._rd_addr_infifo_out_eos, self._rd_addr_infifo_out[self.addr_width])       
        # Separate addressing...
        if self.rw_same_cycle:
            self.wire(self._wr_addr, self._wr_addr_in[self.bit_range])
            self.wire(self._rd_addr_unbuffered, self._rd_addr_in)
        # Use the wr addr for both in this case...
        else:
            self.wire(self._wr_addr, self._wr_addr_in[self.bit_range])
            self.wire(self._rd_addr_unbuffered, self._wr_addr_in)
        self.wire(self._rd_addr, kts.ternary(self._dense_mode,
                                             self._rd_addr_unbuffered[self.bit_range],
                                             self._rd_addr_infifo_out_data))

        self._data_out = self.output("data_out", self.data_width + self.add_bits, packed=True)
        self._data_out_from_strg = self.var("data_out_from_strg", self.addr_width + self.add_bits, packed=True)
        self._valid_out = self.output("valid_out", 1)
        self._valid_out_from_strg = self.var("valid_out_from_strg", 1)
        self._data_out_ready_in = self.input("data_out_ready_in", 1)
# ==============================
# OUTPUT FIFO
# ==============================
        # Data output fifo control signal 
        self._data_outfifo_push = self.var("data_outfifo_push", 1)
        self._data_outfifo = RegFIFO(data_width=self.data_width + self.add_bits,
                                     width_mult=1,
                                     depth=self.fifo_depth,
                                     defer_hrdwr_gen=self.defer_fifos)
        self._data_outfifo.add_attribute(SharedFifoAttr("OUT"))
        self._data_outfifo_out = self.var("data_outfifo_out", 
                                          self.addr_width + self.add_bits,
                                          packed=True)

        self.add_child("data_outfifo",
                       self._data_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._data_outfifo_push,
                       pop=self._data_out_ready_in,
                       data_in=self._data_out_from_strg,
                       data_out=self._data_outfifo_out)

        self.wire(self._data_out, kts.ternary(self._dense_mode,
                                              self._data_out_from_strg,
                                              self._data_outfifo_out))
        self.wire(self._valid_out, kts.ternary(self._dense_mode,
                                               self._valid_out_from_strg,
                                               ~self._data_outfifo.ports.empty))

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
                                     packed=True,
                                     explicit_array=True)

        self._rd_bank = self.var("rd_bank", max(1, clog2(self.banks)))
        self.set_read_bank()
        if self.fw_int == 1:
            # Alternate creating of valid read
            self._rd_valid = self.var("rd_valid", 1)
            self.set_read_valid()
            self.wire(self._valid_out_from_strg, self._rd_valid)

        # Fetch width of 1 is simpler...
        if self.fw_int == 1:
            # Set data to storage
            self._ready = self.output("ready", 1)
            self.wire(self._ready, ~self._rd_addr_infifo.ports.full)
            if self.banks == 1:
                self.wire(self._wen_to_strg, self._wen)
                self.wire(self._ren_to_strg, self._ren)
                self.wire(self._data_to_strg[0], self._data_in[self.bit_range])
                self.wire(self._addr_out[0],
                          kts.ternary(self._wen_to_strg[0],
                                      self._wr_addr[self.mem_addr_width - 1, 0],
                                      self._rd_addr[self.mem_addr_width - 1, 0]))
            else:
                for i in range(self.banks):
                    self.wire(self._data_to_strg[i], self._data_in[self.bit_range])
                    self.add_code(self.decode_wen, idx=i)
                    self.add_code(self.decode_ren, idx=i)
                    self.wire(self._addr_out[i],
                              kts.ternary(self._wen_to_strg[i],
                                          self._wr_addr[self.mem_addr_width - 1, 0],
                                          self._rd_addr[self.mem_addr_width - 1, 0]))
            self.wire(self._data_out_from_strg[self.bit_range], self._data_from_strg[self._rd_bank])
            for i_ in range(self.add_bits):
                self.wire(self._data_out_from_strg[self.data_width + i_], kts.const(0, 1))
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
            self._ready_from_strg = self.var("ready_from_strg", 1)
            self.wire(self._ready, kts.ternary(self._dense_mode, 
                                               self._ready_from_strg,
                                               ~self._rd_addr_infifo.ports.full))
            # FSM helper logic
            self._rd_addr_infifo_out_valid = self.var("rd_addr_infifo_out_valid", 1)
            self.wire(self._rd_addr_infifo_out_valid, ~self._rd_addr_infifo.ports.empty)
            self._data_outfifo_in_ready = self.var("data_oufifo_in_ready", 1)
            self.wire(self._data_outfifo_in_ready, ~self._data_outfifo.ports.full)
            # Otherwise implement the state machine for read-modify-write
            self.rmw_fsm = self.add_fsm("r_w_seq", reset_high=False)
            IDLE = self.rmw_fsm.add_state("IDLE")
            READ = self.rmw_fsm.add_state("READ")
            MODIFY = self.rmw_fsm.add_state("MODIFY")
            DEFAULT = self.rmw_fsm.add_state("_DEFAULT")
            self.rmw_fsm.output(self._ready_from_strg)
            self.rmw_fsm.output(self._valid_out_from_strg)
            # self.rmw_fsm.output(self._data_out_from_strg)
            self.rmw_fsm.output(self._data_out_from_strg[self.bit_range])
            for i_ in range(self.add_bits):
                self.rmw_fsm.output(self._data_out_from_strg[self.data_width + i_])
            self.rmw_fsm.output(self._write_gate)
            self.rmw_fsm.output(self._read_gate)
            self.rmw_fsm.output(self._rd_addr_infifo_pop)
            self.rmw_fsm.output(self._data_outfifo_push)

            # In IDLE we go to a read state if reading, and modify state
            # if writing....
            # For sparse mode, if the input is an eos, stay in IDLE
            IDLE.next(IDLE, 
                      kts.ternary(self._dense_mode,
                                 (~(self._wen) & ~(self._ren)),
                                 ~self._wen & ~self._rd_addr_infifo_out_valid | 
                                 ~self._wen & self._rd_addr_infifo_out_valid & self._rd_addr_infifo_out_eos))
            IDLE.next(READ, 
                      kts.ternary(self._dense_mode,
                                  self._ren & ~self._wen,
                                  self._rd_addr_infifo_out_valid & ~self._wen & ~self._rd_addr_infifo_out_eos & self._data_outfifo_in_ready))
            IDLE.next(MODIFY, self._wen)
            # OUT
            self._rd_addr_infifo_pop_IDLE = self.var("rd_addr_infifo_pop_IDLE", 1)
            self._data_outfifo_push_IDLE = self.var("data_outfifo_push_IDLE", 1)
            self._data_out_from_strg_IDLE = self.var("data_out_from_strg_IDLE", self.addr_width + self.add_bits)
            IDLE.output(self._ready_from_strg, 1)
            IDLE.output(self._valid_out_from_strg, 0)
            IDLE.output(self._data_out_from_strg[self.bit_range], self._data_out_from_strg_IDLE[self.bit_range])
            for i_ in range(self.add_bits):
                IDLE.output(self._data_out_from_strg[self.data_width + i_], self._data_out_from_strg_IDLE[self.mem_addr_width + i_])
            IDLE.output(self._write_gate, 0)
            # Don't gate the read enable if the data at the head of fifo is not eos, and out fifo is not at capacity
            IDLE.output(self._read_gate, kts.ternary(self._dense_mode,
                                                     kts.const(1, 1),
                                                     self._rd_addr_infifo_out_valid & ~self._rd_addr_infifo_out_eos & self._data_outfifo_in_ready))
            IDLE.output(self._rd_addr_infifo_pop, self._rd_addr_infifo_pop_IDLE)
            IDLE.output(self._data_outfifo_push, self._data_outfifo_push_IDLE)
            @always_comb
            def IDLE_state_control_logic():
                if self._dense_mode:
                    self._rd_addr_infifo_pop_IDLE = 0
                    self._data_outfifo_push_IDLE = 0
                    self._data_out_from_strg_IDLE[self.bit_range] = 0
                    for i_ in range(self.add_bits):
                        self._data_out_from_strg_IDLE[self.data_width + i_] = kts.const(0, 1)
                else:
                    if self._rd_addr_infifo_out_valid & self._data_outfifo_in_ready:
                        # pop from fifo if there's valid data in the input fifo
                        # and the output fifo is not at capacity
                        self._rd_addr_infifo_pop_IDLE = 1
                        if self._rd_addr_infifo_out_eos:
                            # if the input is eos, we don't need to consult the 
                            # ROM. Therefore, push the results now.
                            self._data_outfifo_push_IDLE = 1
                            self._data_out_from_strg_IDLE[self.bit_range] = self._rd_addr_infifo_out_data
                            for i_ in range(self.add_bits):
                                self._data_out_from_strg_IDLE[self.data_width + i_] = self._rd_addr_infifo_out_eos
                        else:
                            # Input is not a eos token, we need to consult memory
                            self._data_outfifo_push_IDLE = 0
                            self._data_out_from_strg_IDLE[self.bit_range] = 0
                            for i_ in range(self.add_bits):
                                self._data_out_from_strg_IDLE[self.data_width + i_] = kts.const(0, 1)
                    else:
                        self._rd_addr_infifo_pop_IDLE = 0
                        self._data_outfifo_push_IDLE = 0
                        self._data_out_from_strg_IDLE[self.bit_range] = 0
                        for i_ in range(self.add_bits):
                            self._data_out_from_strg_IDLE[self.data_width + i_] = kts.const(0, 1)
            self.add_code(IDLE_state_control_logic)
                
            # In READ, we effectively use the same transitions as IDLE
            READ.next(IDLE, 
                      kts.ternary(self._dense_mode,
                                 (~(self._wen) & ~(self._ren)),
                                 ~self._wen & ~self._rd_addr_infifo_out_valid | 
                                 ~self._wen & self._rd_addr_infifo_out_valid & self._rd_addr_infifo_out_eos))
            READ.next(READ, 
                      kts.ternary(self._dense_mode,
                                  self._ren & ~self._wen,
                                  self._rd_addr_infifo_out_valid & ~self._wen & ~self._rd_addr_infifo_out_eos & self._data_outfifo_in_ready))
            READ.next(MODIFY, self._wen)
            
            # OUT
            self._rd_addr_infifo_pop_READ = self.var("rd_addr_infifo_pop_READ", 1)
            self._data_outfifo_push_READ = self.var("data_outfifo_push_READ", 1)
            READ.output(self._ready_from_strg, 1)
            READ.output(self._valid_out_from_strg, 1)
            READ.output(self._data_out_from_strg[self.bit_range], 
                        self._data_from_strg[self._rd_bank][self._addr_to_write[self.word_width - 1, 0]])
            for i_ in range(self.add_bits):
                READ.output(self._data_out_from_strg[self.data_width + i_], kts.const(0, 1))
            READ.output(self._write_gate, 0)
            # Don't gate the read enable if the data at the head of fifo is not eos, and out fifo is not at capacity
            READ.output(self._read_gate, kts.ternary(self._dense_mode,
                                                     kts.const(1, 1),
                                                     self._rd_addr_infifo_out_valid & ~self._rd_addr_infifo_out_eos & self._data_outfifo_in_ready))
            READ.output(self._rd_addr_infifo_pop, self._rd_addr_infifo_pop_READ)
            READ.output(self._data_outfifo_push, self._data_outfifo_push_READ)
            @always_comb
            def READ_state_control_logic():
                if self._dense_mode:
                    self._rd_addr_infifo_pop_READ = 0
                    self._data_outfifo_push_READ = 0
                else:
                    # push the data read out from the memory to outfifo
                    self._data_outfifo_push_READ = 1
                    if self._rd_addr_infifo_out_valid & self._data_outfifo_in_ready:
                        # Data available in input fifo and data outfifo is not at capacity
                        if self._rd_addr_infifo_out_eos:
                            # Next data is eos, don't pop it yet as it will be pushed when the FSM
                            # transition to IDLE state
                            self._rd_addr_infifo_pop_READ = 0
                        else:
                            # Next data is also a valid data, pop it and get the output from mem 
                            # in the next cycle
                            self._rd_addr_infifo_pop_READ = 1
                    else:
                        self._rd_addr_infifo_pop_READ = 0
            self.add_code(READ_state_control_logic)

            # In MODIFY we always go back to idle
            MODIFY.next(IDLE, const(1, 1))
            MODIFY.output(self._ready_from_strg, 0)
            MODIFY.output(self._valid_out_from_strg, 0)
            MODIFY.output(self._data_out_from_strg[self.bit_range], 0)
            for i_ in range(self.add_bits):
                MODIFY.output(self._data_out_from_strg[self.data_width + i_], kts.const(0, 1))
            MODIFY.output(self._write_gate, 1)
            MODIFY.output(self._read_gate, 0)
            MODIFY.output(self._rd_addr_infifo_pop, 0)
            MODIFY.output(self._data_outfifo_push, 0)

            # In DEFAULT we always stick in DEFAULT because it's over...
            DEFAULT.next(DEFAULT, const(1, 1))
            DEFAULT.output(self._ready_from_strg, 0)
            DEFAULT.output(self._valid_out_from_strg, 0)
            DEFAULT.output(self._data_out_from_strg[self.bit_range], 0)
            for i_ in range(self.add_bits):
                DEFAULT.output(self._data_out_from_strg[self.data_width + i_], kts.const(0, 1))
            DEFAULT.output(self._write_gate, 0)
            DEFAULT.output(self._read_gate, 0)
            DEFAULT.output(self._rd_addr_infifo_pop, 0)
            DEFAULT.output(self._data_outfifo_push, 0)

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
        else:
            assert self.read_delay == 0
            raise NotImplementedError

        self.base_ports = [[None]]
        rw_port = MemoryPort(MemoryPortType.READWRITE)
        rw_port_intf = rw_port.get_port_interface()
        rw_port_intf['data_in'] = self._data_to_strg
        rw_port_intf['data_out'] = self._data_from_strg
        rw_port_intf['write_addr'] = self._addr_out
        rw_port_intf['write_enable'] = self._wen_to_strg
        rw_port_intf['read_addr'] = self._addr_out
        rw_port_intf['read_enable'] = self._ren_to_strg
        rw_port.annotate_port_signals()
        self.base_ports[0][0] = rw_port

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
            self._data_to_write = self._data_in[self.bit_range]

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

    def get_memory_ports(self):
        return self.base_ports

    def get_bitstream(self, config_json):
        config = []
        return config
        # raise NotImplementedError

    def get_config_mode_str(self):
        return "ROM"


if __name__ == "__main__":
    stg_dut = StrgRAM(defer_fifos=False)
    stg_dut.add_attribute("sync-reset=flush")
    kts.passes.auto_insert_sync_reset(stg_dut.internal_generator)
    # flush_port = stg_dut.internal_generator.get_port("flush")
    verilog(stg_dut, filename="strg_ram.sv",
            additional_passes={"lift config regs": lift_config_reg})
