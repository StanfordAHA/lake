from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.top.memory_interface import MemoryPort, MemoryPortType
from lake.top.memory_controller import MemoryController
from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.passes.passes import lift_config_reg
from lake.modules.reg_fifo import RegFIFO
import kratos as kts

from lake.utils.util import register


class StrgRAMRV(MemoryController):
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
                 prioritize_write=True,
                 comply_with_17=True,
                 defer_fifos=True,
                 fifo_depth=4):
        super().__init__(f"strg_ram_{memory_width}_{memory_depth}_delay_{read_delay}_rv", debug=True)

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
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos

        self.comply_with_17 = comply_with_17

        self.add_bits = 0
        if self.comply_with_17:
            self.add_bits = 1

        # assert banks > 1 or rw_same_cycle is True or self.fw_int > 1, \
        #     "Can't sustain throughput with this setup. Need potential bandwidth for " + \
        #     "1 write and 1 read in a cycle - try using more banks or a macro that supports 1R1W"

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # =============================
        # Inputs and Outputs definitions
        # =============================

        self._ren = self.input("ren", 1, packed=True)
        self._ren.add_attribute(ControlSignalAttr(is_control=False))
        self._ren_valid_in = self.input("ren_valid", 1)
        self._ren_valid_in.add_attribute(ControlSignalAttr(is_control=True))
        self._ren_ready_out = self.output("ren_ready", 1)
        self._ren_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_addr_in = self.input("rd_addr_in", self.addr_width + self.add_bits, packed=True)
        self._rd_addr_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._rd_addr_in_valid_in = self.input("rd_addr_in_valid", 1)
        self._rd_addr_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))
        self._rd_addr_in_ready_out = self.output("rd_addr_in_ready", 1)
        self._rd_addr_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._data_out = self.output("data_out", self.data_width + self.add_bits, packed=True)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._data_out_valid_out = self.output("data_out_valid", 1)
        self._data_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))
        self._data_out_ready_in = self.input("data_out_ready", 1)
        self._data_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))
        # self._valid_out = self.output("valid_out", 1)

        # =============================
        # IN + OUT FIFOs
        # =============================

        self._infifo_pop = self.var("infifo_pop", 1)
        self._outfifo_push = self.var("outfifo_push", 1)

        # Address infifo
        self._addr_infifo = RegFIFO(data_width=self._rd_addr_in.width, width_mult=1,
                                    depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._addr_infifo.add_attribute(SharedFifoAttr(direction="IN"))

        self._addr_infifo_addr_in = self.var("addr_infifo_addr_in", self._rd_addr_in.width, packed=True)
        self._addr_infifo_valid_in = self.var("addr_infifo_valid_in", 1)

        self.add_child(f"addr_infifo",
                       self._addr_infifo,
                       clk=self._clk,
                       rst_n=self._rst_n,
                    #    clk_en=self._clk_en,
                       clk_en=kts.const(1, 1),
                       push=self._rd_addr_in_valid_in,
                       pop=self._infifo_pop,
                       data_in=self._rd_addr_in,
                       data_out=self._addr_infifo_addr_in)

        self.wire(self._rd_addr_in_ready_out, ~self._addr_infifo.ports.full)
        self.wire(self._addr_infifo_valid_in, ~self._addr_infifo.ports.empty)

        # ren infifo
        self._ren_infifo = RegFIFO(data_width=self._ren.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._ren_infifo.add_attribute(SharedFifoAttr(direction="IN"))

        self._ren_infifo_ren_in = self.var("ren_infifo_ren_in", self._ren.width, packed=True)
        self._ren_infifo_valid_in = self.var("ren_infifo_valid_in", 1)

        self.add_child(f"ren_infifo",
                       self._ren_infifo,
                       clk=self._clk,
                       rst_n=self._rst_n,
                    #    clk_en=self._clk_en,
                       clk_en=kts.const(1, 1),
                       push=self._ren_valid_in,
                       pop=self._infifo_pop,
                       data_in=self._ren,
                       data_out=self._ren_infifo_ren_in)

        self.wire(self._ren_ready_out, ~self._ren_infifo.ports.full)
        self.wire(self._ren_infifo_valid_in, ~self._ren_infifo.ports.empty)

        # data outfifo
        # Set almost_full_diff to 1 with depth to 4 to ensure we can always make the read...
        # self._data_outfifo = RegFIFO(data_width=self._data_out.width, width_mult=1, depth=4, defer_hrdwr_gen=self.defer_fifos, min_depth=4, almost_full_diff=1)
        self._data_outfifo = RegFIFO(data_width=self.data_width, width_mult=1, depth=4, defer_hrdwr_gen=self.defer_fifos, min_depth=4, almost_full_diff=1)
        self._data_outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

        # self._data_outfifo_data_in = self.var("data_outfifo_data_in", self._data_out.width, packed=True)
        self._data_outfifo_data_in = self.var("data_outfifo_data_in", self.data_width, packed=True)
        self._data_outfifo_full = self.var("data_outfifo_full", 1)

        self.add_child(f"data_outfifo",
                       self._data_outfifo,
                       clk=self._clk,
                       rst_n=self._rst_n,
                    #    clk_en=self._clk_en,
                       clk_en=kts.const(1, 1),
                       push=self._outfifo_push,
                       pop=self._data_out_ready_in,
                       data_in=self._data_outfifo_data_in,
                       data_out=self._data_out[self.data_width - 1, 0])

        # self.wire(self._data_outfifo_full, self._data_outfifo.ports.full)
        self.wire(self._data_outfifo_full, self._data_outfifo.ports.almost_full)
        self.wire(self._data_out_valid_out, ~self._data_outfifo.ports.empty)

        if self.comply_with_17:
            self.wire(self._data_out[self.data_width], 0)

        # =============================
        # Connections to memory ports
        # =============================
        # get relevant signals from the storage banks
        self._data_from_strg = self.input("data_from_strg", self.data_width,
                                          size=self.fw_int,
                                          explicit_array=True,
                                          packed=True)

        self._data_to_strg = self.output("data_to_strg", self.data_width,
                                          size=self.fw_int,
                                          explicit_array=True,
                                          packed=True)
        # self._data_from_strg = self.input("data_from_strg", self.data_width,
        #                                   size=(self.banks,
        #                                         self.fw_int),
        #                                   explicit_array=True,
        #                                   packed=True)

        self._ren_addr = self.var("ren_addr", self.addr_width,
                                  explicit_array=True,
                                  packed=True)
        # self._ren_addr = self.var("ren_addr", self.addr_width,
        #                           size=self.banks,
        #                           explicit_array=True,
        #                           packed=True)

        # self._ren_to_strg = self.output("ren_to_strg", self.banks)
        self._ren_to_strg = self.output("ren_to_strg", 1)

        self._addr_out = self.output("addr_out", self.mem_addr_width,
                                     packed=True,
                                     explicit_array=True)
        # self._addr_out = self.output("addr_out", self.mem_addr_width,
        #                              size=self.banks,
        #                              packed=True,
        #                              explicit_array=True)

        self._rd_addr = self.var("rd_addr", self.addr_width)

        # ===============================
        # Rename stuff for ease of use...
        # ===============================
        ren_in = self._ren_infifo_ren_in
        ren_valid_in = self._ren_infifo_valid_in

        rd_addr_in = self._addr_infifo_addr_in
        rd_addr_valid_in = self._addr_infifo_valid_in

        data_out = self._data_outfifo_data_in
        data_ready_in = ~self._data_outfifo_full

        all_in_valid = ren_valid_in & rd_addr_valid_in

        # Separate addressing...
        self.bit_range = (self.data_width - 1, 0)
        self.wire(self._rd_addr, rd_addr_in[self.bit_range])

        # self._rd_bank = self.var("rd_bank", max(1, clog2(self.banks)))
        # self.set_read_bank()

        self._read_last_cycle = register(self, signal=self._ren_to_strg, enable=kts.const(1, 1), name="read_last_cycle")

        # We push if we made a read last cycle...
        self.wire(self._outfifo_push, self._read_last_cycle)

        # Only make a read if there is room in the fifo and there are valids in
        self.wire(self._infifo_pop, all_in_valid & data_ready_in)
        self.wire(self._ren_to_strg, self._infifo_pop)

        if self.fw_int == 1:
            # For this, the data out is simply the data coming in...
            # self.wire(data_out, self._data_from_strg[self._rd_bank][self._rd_addr[self.word_width - 1, 0]])
            self.wire(data_out, self._data_from_strg)
            self.wire(self._addr_out, self._rd_addr)
            self.wire(self._data_to_strg, kts.const(0, self._data_to_strg.width))
        else:
            # If fw > 1, we need to send out the top portion of addr and register the sub-word addr, then use that to select
            # the proper data from the subword...
            top_bits = [self.mem_addr_width + self.word_width - 1, self.word_width]
            bottom_bits = [self.word_width - 1, 0]
            subword_addr_reg = register(self, signal=rd_addr_in[bottom_bits], enable=self._infifo_pop, name="subword_addr_reg")
            self.wire(data_out, self._data_from_strg[subword_addr_reg])
            self.wire(self._addr_out, rd_addr_in[top_bits])
            for i in range(self.fw_int):
                self.wire(self._data_to_strg[i], kts.const(0, self._data_to_strg[i].width))

        self._wen_to_strg = self.output("wen_to_strg", 1)
        self.wire(self._wen_to_strg, kts.const(0, self._wen_to_strg.width))
        self._wr_addr_to_strg = self.output("wr_addr_to_strg", self.mem_addr_width)
        self.wire(self._wr_addr_to_strg, kts.const(0, self._wr_addr_to_strg.width))


        self.base_ports = [[None]]
        rw_port = MemoryPort(MemoryPortType.READWRITE)
        rw_port_intf = rw_port.get_port_interface()
        rw_port_intf['data_in'] = self._data_to_strg
        # rw_port_intf['data_in'] = 0
        rw_port_intf['data_out'] = self._data_from_strg
        # rw_port_intf['write_addr'] = self._addr_out
        rw_port_intf['write_addr'] = self._wr_addr_to_strg
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
    stg_dut = StrgRAMRV(defer_fifos=False)
    stg_dut.add_attribute("sync-reset=flush")
    kts.passes.auto_insert_sync_reset(stg_dut.internal_generator)
    # flush_port = stg_dut.internal_generator.get_port("flush")
    verilog(stg_dut, filename="strg_ram_rv.sv",
            additional_passes={"lift config regs": lift_config_reg})
