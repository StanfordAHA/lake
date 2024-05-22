from struct import pack
import kratos as kts
from kratos import *
from lake.modules.arbiter import Arbiter
from lake.passes.passes import lift_config_reg
from lake.top.memory_controller import MemoryController
from lake.top.memory_interface import MemoryInterface, MemoryPort, MemoryPortType
from lake.top.tech_maps import TSMC_Tech_Map
from lake.utils.util import decode, register, sticky_flag, sum_bits, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO
from lake.modules.strg_RAM import StrgRAM
from lake.top.tech_maps import GF_Tech_Map, SKY_Tech_Map, TSMC_Tech_Map
from lake.attributes.shared_fifo_attr import SharedFifoAttr


class BuffetLike(MemoryController):
    def __init__(self,
                 data_width=16,
                 num_ID=2,
                 mem_width=64,
                 mem_depth=512,
                 local_memory=True,
                 physical_mem=False,
                 fifo_depth=8,
                 tech_map=TSMC_Tech_Map(depth=512, width=32),
                 defer_fifos=True,
                 optimize_wide=False,
                 add_flush=False,
                 split_mem_requests=True):

        super().__init__(f"buffet_like_{data_width}", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = add_flush
        self.num_ID = num_ID
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.local_memory = local_memory
        self.physical_mem = physical_mem
        self.fifo_depth = fifo_depth
        self.tech_map = tech_map
        self.defer_fifos = defer_fifos
        self.optimize_wide = optimize_wide
        self.split_mem_requests = split_mem_requests

        if self.split_mem_requests:
            self.num_read_ports = 2
        else:
            self.num_read_ports = 1

        self.total_sets = 0

        self.base_ports = [[None]]

        self.fw_int = self.mem_width // self.data_width
        self.subword_addr_bits = kts.clog2(self.fw_int) if self.mem_width > 1 else 0

        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # Buffet-like has a write side and a read side

        # Need an ID to identify which buffet is being accessed
        # self._ID = self.input("ID", self.data_width)
        # self._ID.add_attribute(ConfigRegAttr("Identifier for the buffet controller being addressed"))

        capacity_log = kts.clog2(self.data_width)
        self._buffet_capacity_log = self.input("buffet_capacity_log", capacity_log, size=self.num_ID, explicit_array=True, packed=True)
        self._buffet_capacity_log.add_attribute(ConfigRegAttr("Capacity of buffet..."))

        self._buffet_capacity_mask = self.var("buffet_capacity_mask", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        for i in range(self.num_ID):
            for j in range(self.data_width):
                self.wire(self._buffet_capacity_mask[i][j], (j < (self._buffet_capacity_log[i])) & (self._buffet_capacity_log[i] != 0))

        self._buffet_capacity = self.var("buffet_capacity", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        [self.wire(self._buffet_capacity[idx], kts.ternary(self._buffet_capacity_log[idx] == 0,
                                                           kts.const(0, self.data_width),
                                                           (kts.const(1, self.data_width) << (self._buffet_capacity_log[idx] + self.subword_addr_bits)))) for idx in range(self.num_ID)]

        ### WRITE SIDE
        # self._random_write = self.input("random_write")
        # self._random_write.add_attribute(ConfigRegAttr("If we are using random write or linear write..."))

        # Accept an address over the line
        self._wr_ID = self.input("wr_ID", self.data_width + 1, explicit_array=True, packed=True)
        self._wr_ID.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._wr_ID_ready = self.output("wr_ID_ready", 1)
        self._wr_ID_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._wr_ID_valid = self.input("wr_ID_valid", 1)
        self._wr_ID_valid.add_attribute(ControlSignalAttr(is_control=True))

        # Accept an address over the line
        self._wr_addr = self.input("wr_addr", self.data_width + 1, explicit_array=True, packed=True)
        self._wr_addr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._wr_addr_ready = self.output("wr_addr_ready", 1)
        self._wr_addr_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._wr_addr_valid = self.input("wr_addr_valid", 1)
        self._wr_addr_valid.add_attribute(ControlSignalAttr(is_control=True))

        # Accept data + op over the line
        self._wr_data = self.input("wr_data", self.data_width + 1, explicit_array=True, packed=True)
        self._wr_data.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # Indicates allocate/finalize or write
        # self._wr_op = self.input("wr_op", 1)
        # self._wr_op.add_attribute(ControlSignalAttr(is_control=True))

        self._wr_data_ready = self.output("wr_data_ready", 1)
        self._wr_data_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._wr_data_valid = self.input("wr_data_valid", 1)
        self._wr_data_valid.add_attribute(ControlSignalAttr(is_control=True))

        ### READ SIDE
        # On read side need both a request and response channel
        # Free or Read
        self._rd_op_op = [self.input(f"rd_op_{i}", self.data_width + 1, explicit_array=True, packed=True) for i in range(self.num_read_ports)]
        [self._rd_op_op[i].add_attribute(ControlSignalAttr(is_control=False)) for i in range(self.num_read_ports)]

        self._rd_op_ready = [self.output(f"rd_op_{i}_ready", 1) for i in range(self.num_read_ports)]
        [self._rd_op_ready[i].add_attribute(ControlSignalAttr(is_control=False)) for i in range(self.num_read_ports)]

        self._rd_op_valid = [self.input(f"rd_op_{i}_valid", 1) for i in range(self.num_read_ports)]
        [self._rd_op_valid[i].add_attribute(ControlSignalAttr(is_control=True)) for i in range(self.num_read_ports)]

        self._rd_addr = [self.input(f"rd_addr_{i}", self.data_width + 1, explicit_array=True, packed=True) for i in range(self.num_read_ports)]
        [self._rd_addr[i].add_attribute(ControlSignalAttr(is_control=False, full_bus=True)) for i in range(self.num_read_ports)]

        self._rd_addr_ready = [self.output(f"rd_addr_{i}_ready", 1) for i in range(self.num_read_ports)]
        [self._rd_addr_ready[i].add_attribute(ControlSignalAttr(is_control=False)) for i in range(self.num_read_ports)]

        self._rd_addr_valid = [self.input(f"rd_addr_{i}_valid", 1) for i in range(self.num_read_ports)]
        [self._rd_addr_valid[i].add_attribute(ControlSignalAttr(is_control=True)) for i in range(self.num_read_ports)]

        if self.num_read_ports == 1:
            # Read ID
            self._rd_ID = [self.input(f"rd_ID_{i}", self.data_width + 1, explicit_array=True, packed=True) for i in range(self.num_read_ports)]
            [self._rd_ID[i].add_attribute(ControlSignalAttr(is_control=False, full_bus=True)) for i in range(self.num_read_ports)]

            self._rd_ID_ready = [self.output(f"rd_ID_{i}_ready", 1) for i in range(self.num_read_ports)]
            [self._rd_ID_ready[i].add_attribute(ControlSignalAttr(is_control=False)) for i in range(self.num_read_ports)]

            self._rd_ID_valid = [self.input(f"rd_ID_{i}_valid", 1) for i in range(self.num_read_ports)]
            [self._rd_ID_valid[i].add_attribute(ControlSignalAttr(is_control=True)) for i in range(self.num_read_ports)]

        # Read response channel
        self._rd_rsp_data = [self.output(f"rd_rsp_data_{i}", self.data_width + 1, explicit_array=True, packed=True) for i in range(self.num_read_ports)]
        [self._rd_rsp_data[i].add_attribute(ControlSignalAttr(is_control=False, full_bus=True)) for i in range(self.num_read_ports)]

        self._rd_rsp_ready = [self.input(f"rd_rsp_data_{i}_ready", 1) for i in range(self.num_read_ports)]
        [self._rd_rsp_ready[i].add_attribute(ControlSignalAttr(is_control=True)) for i in range(self.num_read_ports)]

        self._rd_rsp_valid = [self.output(f"rd_rsp_data_{i}_valid", 1) for i in range(self.num_read_ports)]
        [self._rd_rsp_valid[i].add_attribute(ControlSignalAttr(is_control=False)) for i in range(self.num_read_ports)]

# =============================
# Miscellaneous forward declarations
# =============================

        # Handle allocating separate buffets in the same physical memory space
        base_chunk = self.mem_depth // self.num_ID
        self._buffet_base = self.var("buffet_base", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        [self.wire(self._buffet_base[i], kts.const(base_chunk * i, self._buffet_base[i].width)) for i in range(self.num_ID)]

        # Read block base and bounds...
        self._blk_base = self.var("blk_base", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        self._blk_bounds = self.var("blk_bounds", self.data_width, size=self.num_ID, explicit_array=True, packed=True)

        self._wen_full = self.var("wen_full", self.num_ID)

        # Read side address + ren
        # self._rd_addr_loc = self.var("rd_addr_loc", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        self._ren_full = self.var("ren_full", self.num_ID)

        # Need to define which side (read/write) has access to the memory port
        self._mem_acq = self.var("mem_acq", 2 * self.num_ID)

# =============================
# FIFO inputs
# =============================

        # WR OP fifo
        self._wr_data_fifo_pop = self.var("wr_data_fifo_pop", 1)
        self._wr_data_fifo_valid = self.var("wr_data_fifo_valid", 1)

        self._wr_data_fifo_in = kts.concat(self._wr_data)
        self._wr_data_infifo = RegFIFO(data_width=self._wr_data_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._wr_data_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._wr_data_fifo_out_data = self.var("wr_data_fifo_out_data", self.data_width, packed=True)
        self._wr_data_fifo_out_op = self.var("wr_data_fifo_out_op", 1)

        self.add_child(f"wr_data_fifo",
                       self._wr_data_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._wr_data_valid,
                       pop=self._wr_data_fifo_pop,
                       data_in=self._wr_data_fifo_in,
                       # TODO: Make sure this concatenation is right
                       data_out=kts.concat(self._wr_data_fifo_out_op, self._wr_data_fifo_out_data))

        self.wire(self._wr_data_ready, ~self._wr_data_infifo.ports.full)
        self.wire(self._wr_data_fifo_valid, ~self._wr_data_infifo.ports.empty)

        # WR ADDR fifo
        self._wr_addr_fifo_pop = self.var("wr_addr_fifo_pop", 1)
        self._wr_addr_fifo_valid = self.var("wr_addr_fifo_valid", 1)

        self._wr_addr_fifo_in = kts.concat(self._wr_addr[0][self.data_width - 1, 0])
        self._wr_addr_infifo = RegFIFO(data_width=self._wr_addr_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._wr_addr_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._wr_addr_fifo_out_data = self.var("wr_addr_fifo_out_data", self.data_width, packed=True)

        self.add_child(f"wr_addr_fifo",
                       self._wr_addr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._wr_addr_valid,
                       pop=self._wr_addr_fifo_pop,
                       data_in=self._wr_addr_fifo_in,
                       data_out=self._wr_addr_fifo_out_data)

        self.wire(self._wr_addr_ready, ~self._wr_addr_infifo.ports.full)
        self.wire(self._wr_addr_fifo_valid, ~self._wr_addr_infifo.ports.empty)

        # WR ID fifo
        self._wr_ID_fifo_pop = self.var("wr_ID_fifo_pop", 1)
        self._wr_ID_fifo_valid = self.var("wr_ID_fifo_valid", 1)

        self._wr_ID_fifo_in = kts.concat(self._wr_ID[0][self.data_width - 1, 0])
        self._wr_ID_infifo = RegFIFO(data_width=self._wr_ID_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._wr_ID_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._wr_ID_fifo_out_data = self.var("wr_ID_fifo_out_data", self.data_width, packed=True)

        self.add_child(f"wr_ID_fifo",
                       self._wr_ID_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._wr_ID_valid,
                       pop=self._wr_ID_fifo_pop,
                       data_in=self._wr_ID_fifo_in,
                       data_out=kts.concat(self._wr_ID_fifo_out_data))

        self.wire(self._wr_ID_ready, ~self._wr_ID_infifo.ports.full)
        self.wire(self._wr_ID_fifo_valid, ~self._wr_ID_infifo.ports.empty)

        # RD OP fifo
        self._rd_op_fifo_pop = [self.var(f"rd_op_fifo_pop_{i}", 1) for i in range(self.num_read_ports)]
        self._rd_op_fifo_valid = [self.var(f"rd_op_fifo_valid_{i}", 1) for i in range(self.num_read_ports)]

        self._rd_op_fifo_in = [self._rd_op_op[i][0, 0][self.data_width - 1, 0] for i in range(self.num_read_ports)]
        self._rd_op_infifo = [RegFIFO(data_width=self._rd_op_fifo_in[i].width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos) for i in range(self.num_read_ports)]
        [self._rd_op_infifo[i].add_attribute(SharedFifoAttr(direction="IN")) for i in range(self.num_read_ports)]
        self._rd_op_fifo_out_op = [self.var(f"rd_op_fifo_out_op_{i}", self.data_width, packed=True) for i in range(self.num_read_ports)]

        [self.add_child(f"rd_op_fifo_{i}",
                       self._rd_op_infifo[i],
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_op_valid[i],
                       pop=self._rd_op_fifo_pop[i],
                       data_in=self._rd_op_fifo_in[i],
                       data_out=kts.concat(self._rd_op_fifo_out_op[i])) for i in range(self.num_read_ports)]

        [self.wire(self._rd_op_ready[i], ~self._rd_op_infifo[i].ports.full) for i in range(self.num_read_ports)]
        [self.wire(self._rd_op_fifo_valid[i], ~self._rd_op_infifo[i].ports.empty) for i in range(self.num_read_ports)]

        # RD ADDR fifo
        self._rd_addr_fifo_pop = [self.var(f"rd_addr_fifo_pop_{i}", 1) for i in range(self.num_read_ports)]
        self._rd_addr_fifo_valid = [self.var(f"rd_addr_fifo_valid_{i}", 1) for i in range(self.num_read_ports)]

        self._rd_addr_fifo_in = [kts.concat(self._rd_addr[i][0, 0][self.data_width - 1, 0]) for i in range(self.num_read_ports)]
        self._rd_addr_infifo = [RegFIFO(data_width=self._rd_addr_fifo_in[i].width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos) for i in range(self.num_read_ports)]
        [self._rd_addr_infifo[i].add_attribute(SharedFifoAttr(direction="IN")) for i in range(self.num_read_ports)]
        self._rd_addr_fifo_out_addr = [self.var(f"rd_addr_fifo_out_addr_{i}", self.data_width, packed=True) for i in range(self.num_read_ports)]

        [self.add_child(f"rd_addr_fifo_{i}",
                       self._rd_addr_infifo[i],
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_addr_valid[i],
                       pop=self._rd_addr_fifo_pop[i],
                       data_in=self._rd_addr_fifo_in[i],
                       data_out=kts.concat(self._rd_addr_fifo_out_addr[i])) for i in range(self.num_read_ports)]

        [self.wire(self._rd_addr_ready[i], ~self._rd_addr_infifo[i].ports.full) for i in range(self.num_read_ports)]
        [self.wire(self._rd_addr_fifo_valid[i], ~self._rd_addr_infifo[i].ports.empty) for i in range(self.num_read_ports)]

        if self.num_read_ports == 1:
            # RD ID fifo
            self._rd_ID_fifo_pop = [self.var(f"rd_ID_fifo_pop_{i}", 1) for i in range(self.num_read_ports)]
            self._rd_ID_fifo_valid = [self.var(f"rd_ID_fifo_valid_{i}", 1) for i in range(self.num_read_ports)]

            self._rd_ID_fifo_in = [kts.concat(self._rd_ID[i][0, 0][self.data_width - 1, 0]) for i in range(self.num_read_ports)]
            self._rd_ID_infifo = [RegFIFO(data_width=self._rd_ID_fifo_in[i].width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos) for i in range(self.num_read_ports)]
            [self._rd_ID_infifo[i].add_attribute(SharedFifoAttr(direction="IN")) for i in range(self.num_read_ports)]
            self._rd_ID_fifo_out_data = [self.var(f"rd_ID_fifo_out_data_{i}", self.data_width, packed=True) for i in range(self.num_read_ports)]

            [self.add_child(f"rd_ID_fifo_{i}",
                        self._rd_ID_infifo[i],
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        clk_en=self._clk_en,
                        push=self._rd_ID_valid[i],
                        pop=self._rd_ID_fifo_pop[i],
                        data_in=self._rd_ID_fifo_in[i],
                        data_out=kts.concat(self._rd_ID_fifo_out_data[i])) for i in range(self.num_read_ports)]

            [self.wire(self._rd_ID_ready[i], ~self._rd_ID_infifo[i].ports.full) for i in range(self.num_read_ports)]
            [self.wire(self._rd_ID_fifo_valid[i], ~self._rd_ID_infifo[i].ports.empty) for i in range(self.num_read_ports)]

# =============================
# Miscellaneous forward declarations - 2
# =============================

        # Read fifos joined
        self._read_joined = [self.var(f"read_joined_{i}", 1) for i in range(self.num_read_ports)]

        if self.num_read_ports == 1:
            [self.wire(self._read_joined[i], self._rd_ID_fifo_valid[i] & self._rd_op_fifo_valid[i] & self._rd_addr_fifo_valid[i]) for i in range(self.num_read_ports)]
        else:
            [self.wire(self._read_joined[i], self._rd_op_fifo_valid[i] & self._rd_addr_fifo_valid[i]) for i in range(self.num_read_ports)]
        # Write fifos joined
        self._joined_in_fifo = self.var("joined_in_fifo", 1)

        self._en_curr_bounds = self.var("en_curr_bounds", self.num_ID)
        self._clr_curr_bounds = self.var("clr_curr_bounds", self.num_ID)
        self._curr_bounds = [register(self, self._wr_addr_fifo_out_data + 1, enable=self._en_curr_bounds[i], clear=self._clr_curr_bounds[i],
                                      name=f"curr_bounds_{i}", packed=True) for i in range(self.num_ID)]

        self._en_curr_base = self.var("en_curr_base", self.num_ID)
        self._first_base_set = [sticky_flag(self, self._en_curr_base[idx_], name=f"first_base_set_{idx_}", seq_only=True) for idx_ in range(self.num_ID)]
        self._curr_base_pre = [self.var(f"curr_base_pre_{i}", self.data_width) for i in range(self.num_ID)]
        self._curr_base = [register(self, self._curr_base_pre[i], enable=self._en_curr_base[i], name=f"curr_base_{i}", packed=True) for i in range(self.num_ID)]

        [self.wire(self._curr_base_pre[i], kts.ternary(self._curr_bounds[i][self.subword_addr_bits-1,0] == kts.const(0, self.subword_addr_bits),
                                                       (self._curr_bounds[i] >> self.subword_addr_bits) + self._curr_base[i],
                                                       (self._curr_bounds[i] >> self.subword_addr_bits) + 1 + self._curr_base[i])) for i in range(self.num_ID)]

        self._read_pop_full = self.var("read_pop_full", self.num_ID)
        self._read_pop = [self.var(f"read_pop_{i}", 1) for i in range(self.num_read_ports)]
        self._read_joined_d1 = [register(self, self._read_joined[idx] & self._read_pop[idx], enable=kts.const(1, 1), name=f"read_joined_d1_{idx}") for idx in range(self.num_read_ports)]

        if self.optimize_wide and self.mem_width > self.data_width:

            self.fw_int = self.mem_width // self.data_width
            self.wide_num_word_bits = kts.clog2(self.fw_int)
            self.mem_addr_bit_range_outer = (self.data_width - 1, self.wide_num_word_bits)
            self.mem_addr_bit_range_inner = (self.wide_num_word_bits - 1, 0)

            # Only use based on the mem depth
            self.mem_addr_width = kts.clog2(self.mem_depth)
            self.addr_addressing_bits = (self.mem_addr_width - 1, 0)

            self._write_to_sram = [self.var(f"write_to_sram_{idx}", 1) for idx in range(self.num_ID)]
            self._read_from_sram_write_side = [self.var(f"read_from_sram_write_side_{idx}", 1) for idx in range(self.num_ID)]
            wr_acqs = []
            rd_acqs = []
            # wr_acq = self._mem_acq[0] & self._wen_full[0]
            # wr_acqs.append(self._mem_acq[0] & self._wen_full[0])
            # wr_acq = self._mem_acq[0] & (self._write_to_sram[0] | self._read_from_sram_write_side[0])
            wr_acq_ = self._mem_acq[0] & (self._write_to_sram[0])
            wr_acqs.append(self._mem_acq[0] & (self._write_to_sram[0]))
            # wr_acqs.append(self._mem_acq[0] & (self._write_to_sram[0] | self._read_from_sram_write_side[0]))
            rd_acqs.append(self._mem_acq[1] & self._ren_full[0])
            rd_acq_ = self._mem_acq[1] & self._ren_full[0]
            for i in range(self.num_ID - 1):
                # wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & self._wen_full[i + 1])
                # wr_acqs.append(self._mem_acq[2 * (i + 1)] & self._wen_full[i + 1])
                # wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1] | self._read_from_sram_write_side[i + 1]))
                wr_acq_ = kts.concat(wr_acq_, self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1]))
                # wr_acqs.append(self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1] | self._read_from_sram_write_side[i + 1]))
                wr_acqs.append(self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1]))
                rd_acq_ = kts.concat(rd_acq_, self._mem_acq[2 * (i + 1) + 1] & self._ren_full[i + 1])
                rd_acqs.append(self._mem_acq[2 * (i + 1) + 1] & self._ren_full[i + 1])

            rd_acq = [self.var(f'rd_acq_{i}', 1) for i in range(self.num_ID)]
            wr_acq = [self.var(f'wr_acq_{i}', 1) for i in range(self.num_ID)]

            # For verilog generation
            [self.wire(rd_acq[i], rd_acqs[i]) for i in range(self.num_ID)]
            [self.wire(wr_acq[i], wr_acqs[i]) for i in range(self.num_ID)]

            # Create local memory interface...
            # self._addr_to_mem = self.var("addr_to_mem", self.data_width, packed=True, explicit_array=True)
            if self.local_memory:
                self._addr_to_mem = self.var("addr_to_mem", self.data_width, packed=True)
                self._addr_to_mem_save = self._addr_to_mem
                self._data_to_mem = self.var("data_to_mem", self.data_width, size=self.fw_int, packed=True, explicit_array=True)
                self._data_from_mem = self.var("data_from_mem", self.data_width, size=self.fw_int, packed=True, explicit_array=True)
                self._wen_to_mem = self.var("wen_to_mem", 1)
                self._ren_to_mem = [self.var("ren_to_mem", 1)]
                # self._ren_to_mem_individ = [self.var(f"ren_to_mem_individ_{i}", 1) for i in range(self.num_read_ports)]
                # self.wire(self._ren_to_mem, kts.concat(*self._ren_to_mem_individ).r_or())

            else:
                # self._addr_to_mem = self.output("addr_to_mem", self.mem_addr_width, packed=True)
                # Do a width bypassing trick...
                self._addr_to_mem = self.output("addr_to_mem", self.mem_addr_width, packed=True)
                self._addr_to_mem_save = self._addr_to_mem
                self._addr_to_mem_local = self.var("addr_to_mem_local", self.data_width, packed=True)
                self.wire(self._addr_to_mem, self._addr_to_mem_local[self.addr_addressing_bits])
                self._addr_to_mem = self._addr_to_mem_local
                self._data_to_mem = self.output("data_to_mem", self.data_width, size=self.fw_int, packed=True, explicit_array=True)
                self._data_from_mem = self.input("data_from_mem", self.data_width, size=self.fw_int, packed=True, explicit_array=True)
                self._wen_to_mem = self.output("wen_to_mem", 1)
                self._ren_to_mem = self.output("ren_to_mem", 1)

            # self.wire(self._ren_to_mem, rd_acq.r_or())
            # self.wire(self._wen_to_mem, wr_acq.r_or())

            # Represents the final addr destination for this grouped word
            self._set_wide_word_addr = [self.var(f"set_wide_word_addr_{idx}", 1) for idx in range(self.num_ID)]
            self._tmp_wr_addr = [self.var(f"tmp_addr_{idx}", 16, packed=True) for idx in range(self.num_ID)]
            # tmp_addr = [(self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] + self._curr_base[idx] + self._buffet_base[idx]) for idx in range(self.num_ID)]
            # [self.wire(self._tmp_wr_addr[idx], (self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] + self._curr_base[idx] + self._buffet_base[idx])) for idx in range(self.num_ID)]
            # [self.wire(self._tmp_wr_addr[idx], (((self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] + self._curr_base[idx]) % (self._buffet_capacity[idx] >> self.subword_addr_bits)) +
            #                                 self._buffet_base[idx])) for idx in range(self.num_ID)]
            # [self.wire(self._tmp_wr_addr[idx], (((self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] + self._curr_base[idx]) % (self._buffet_capacity[idx] >> self.subword_addr_bits)) +
            # [self.wire(self._tmp_wr_addr[idx], (((self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] + self._curr_base[idx]) >> self._buffet_capacity_log[idx]) +
            [self.wire(self._tmp_wr_addr[idx], (((self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] + self._curr_base[idx]) & self._buffet_capacity_mask[idx]) +
                                            self._buffet_base[idx])) for idx in range(self.num_ID)]
            self._write_word_addr = [register(self, self._tmp_wr_addr[idx],
                                              enable=self._set_wide_word_addr[idx],
                                              name=f"write_word_addr_reg_{idx}", packed=True) for idx in range(self.num_ID)]
            self._write_word_addr_valid = [sticky_flag(self, self._set_wide_word_addr[idx],
                                                    #    clear=self._clr_cached_read[idx],
                                                       name=f"write_word_addr_valid_sticky_{idx}",
                                                       seq_only=True) for idx in range(self.num_ID)]

            # [self.wire(self._write_to_sram[idx], self._write_full_word[idx] | self._write_rmw_word[idx])]
            # self._write_to_wide_word = [self.var(f"write_to_wide_word_{idx}", 1) for idx in range(self.num_ID)]

            # Basically aggregate a wider word. Fill in the items with a mask
            self._set_write_wide_word = [self.var(f"set_write_wide_word_{idx}", 1) for idx in range(self.num_ID)]
            self._clr_write_wide_word = [self.var(f"clr_write_wide_word_{idx}", 1) for idx in range(self.num_ID)]

            # Have the current valid mask
            self._write_wide_word_mask_comb = [self.var(f"write_wide_word_mask_comb_{idx}", self.fw_int) for idx in range(self.num_ID)]
            self._write_wide_word_mask_reg_out = [self.var(f"write_wide_word_mask_reg_out_{idx}", self.fw_int) for idx in range(self.num_ID)]
            self._write_wide_word_mask_reg_in = [self.var(f"write_wide_word_mask_reg_in_{idx}", self.fw_int) for idx in range(self.num_ID)]
            self._write_wide_word_mask_reg = [register(self, self._write_wide_word_mask_reg_in[idx],
                                                       enable=self._set_write_wide_word[idx] | self._clr_write_wide_word[idx],
                                                    #    clear=self._clr_write_wide_word[idx],
                                                       name=f"write_wide_word_mask_reg_strg_{idx}") for idx in range(self.num_ID)]

            # self._write_wide_word = [self.var(f"write_wide_word_{idx}", self.data_width, size=self.fw_int, packed=True, explicit_array=True) for idx in range(self.num_ID)]
            self._write_wide_word_comb_out = [self.var(f"write_wide_word_comb_out_{idx}", self.data_width, size=self.fw_int, packed=True, explicit_array=True) for idx in range(self.num_ID)]
            self._write_wide_word_comb_in = [self.var(f"write_wide_word_comb_in_{idx}", self.data_width, size=self.fw_int, packed=True, explicit_array=True) for idx in range(self.num_ID)]
            # Modified version of the sram read for
            self._write_wide_word_modified = [self.var(f"write_wide_word_modified_{idx}", self.data_width, size=self.fw_int, packed=True, explicit_array=True) for idx in range(self.num_ID)]
            self._write_wide_word_reg = [register(self, self._write_wide_word_comb_in[idx],
                                                  enable=self._set_write_wide_word[idx] | self._clr_write_wide_word[idx],
                                                  name=f"write_wide_word_reg_{idx}", packed=True) for idx in range(self.num_ID)]
            # For the wide word, send through the data reg if masked, otherwise the fifo
            for idx_i in range(self.num_ID):
                for idx_j in range(self.fw_int):
                    self.wire(self._write_wide_word_comb_out[idx_i][idx_j], kts.ternary(self._write_wide_word_mask_reg_out[idx_i][idx_j],
                                                                                    self._write_wide_word_reg[idx_i][idx_j],
                                                                                    self._wr_data_fifo_out_data))
            for idx_i in range(self.num_ID):
                for idx_j in range(self.fw_int):
                    self.wire(self._write_wide_word_comb_in[idx_i][idx_j], kts.ternary(self._write_wide_word_mask_reg_out[idx_i][idx_j] & ~self._clr_write_wide_word[idx_i],
                                                                                    self._write_wide_word_reg[idx_i][idx_j],
                                                                                    self._wr_data_fifo_out_data))
            # this is the same thing, but the modified word from SRAM if needed
            for idx_i in range(self.num_ID):
                for idx_j in range(self.fw_int):
                    self.wire(self._write_wide_word_modified[idx_i][idx_j], kts.ternary(self._write_wide_word_mask_reg_out[idx_i][idx_j],
                                                                                    self._write_wide_word_reg[idx_i][idx_j],
                                                                                    self._data_from_mem[idx_j]))

            # The valid mask is basically the currently written valid mask + the incoming valid
            [self.wire(self._write_wide_word_mask_reg_out[idx], self._write_wide_word_mask_reg[idx]) for idx in range(self.num_ID)]
            # Only consider the combinational 1 if the addresses match
            [self.wire(self._write_wide_word_mask_comb[idx],
                    #    self._write_wide_word_mask_reg_out[idx] | ((kts.ternary(((self._curr_base[idx] + self._buffet_base[idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) == self._write_word_addr[idx]) &
                       self._write_wide_word_mask_reg_out[idx] | ((kts.ternary((self._tmp_wr_addr[idx] == self._write_word_addr[idx]) &
                                                                                    self._joined_in_fifo &
                                                                                    (self._wr_data_fifo_out_op == kts.const(1, 1)) &
                                                                                    (self._wr_ID_fifo_out_data == kts.const(idx, 1)),
                                                                               kts.const(1, 1),
                                                                               kts.const(0, 1))) << self._wr_addr_fifo_out_data[self.mem_addr_bit_range_inner])) for idx in range(self.num_ID)]

            self._write_full_word = [self.var(f"write_full_word_{idx}", 1) for idx in range(self.num_ID)]
            '''
            This gets a full description

            We are trying to calculate the valid mask at the next cycle

            Firstly, if we are clearing it, we set up a base of 0's, otherwise we use the output of the register
            Then, we can store an additional bit if the word is being cleared but there is a new valid for another address. This prevents
            a 3+1 situation from leaving a 1 valid after clearing, as that one 1 never gets written to the mask before clearing
            Otherwise, we check if the addr matches and then can write it in
            '''
            [self.wire(self._write_wide_word_mask_reg_in[idx],
                       kts.ternary(self._clr_write_wide_word[idx],
                                   kts.const(0, self._write_wide_word_mask_reg_in[idx].width),
                                   self._write_wide_word_mask_reg_out[idx]) |
                                #   ((kts.ternary(((self._clr_write_wide_word[idx] & ((self._curr_base[idx] + self._buffet_base[idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) != self._write_word_addr[idx])) |
                                #                     (((self._curr_base[idx] + self._buffet_base[idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) == self._write_word_addr[idx]) & ~self._write_full_word[idx])) &
                                  ((kts.ternary(((self._clr_write_wide_word[idx] & (self._tmp_wr_addr[idx] != self._write_word_addr[idx])) |
                                                    # ((self._tmp_wr_addr[idx] == self._write_word_addr[idx]) & ~self._write_full_word[idx])) &
                                                    ((self._tmp_wr_addr[idx] == self._write_word_addr[idx]) &
                                                        (~self._write_full_word[idx] | (self._write_full_word[idx] & ~self._mem_acq[2 * idx])))) &
                                                        # Qualify to be a write as well.
                                                        (self._wr_data_fifo_out_op == kts.const(1, 1)) &
                                                        (self._wr_ID_fifo_out_data == kts.const(idx, 2)),
                                                #  kts.const(1, self.fw_int),
                                                 kts.concat(kts.const(0, self.fw_int - 1), self._joined_in_fifo),
                                                 kts.const(0, self.fw_int))) << self._wr_addr_fifo_out_data[self.mem_addr_bit_range_inner])) for idx in range(self.num_ID)]

            self._num_bits_valid_mask = [sum_bits(self, self._write_wide_word_mask_comb[idx], name=f"num_bits_valid_mask_{idx}") for idx in range(self.num_ID)]
            # This means we have the data to write the full word (or use the rmw option)
            # self._write_rmw_word = [self.var(f"write_rmw_word_{idx}", 1) for idx in range(self.num_ID)]
            [self.wire(self._write_full_word[idx], self._num_bits_valid_mask[idx] == kts.const(self.fw_int, self.wide_num_word_bits + 1)) for idx in range(self.num_ID)]
            # [self.wire(self._write_rmw_word[idx], self._num_bits_valid_mask[idx] < kts.const(self.fw_int, self.wide_num_word_bits + 1)) for idx in range(self.num_ID)]

            # This signal will be a product of the fill status of the wide_word

            # Set the wide word addr from later FSM

            # Storage and knowledge of previous read
            # self._read_wide_word = [self.var(f"read_wide_word_{idx}", self.data_width, size=self.fw_int, packed=True, explicit_array=True) for idx in range(self.num_ID)]
            self._set_cached_read = [self.var(f"set_cached_read_{idx}", 1) for idx in range(self.num_ID)]
            self._clr_cached_read = [self.var(f"clr_cached_read_{idx}", 1) for idx in range(self.num_ID)]
            self._read_wide_word = [register(self, self._data_from_mem, enable=self._set_cached_read[idx], name=f"read_wide_word_{idx}", packed=True) for idx in range(self.num_ID)]
            self._read_wide_word_valid = [sticky_flag(self, self._set_cached_read[idx],
                                                      clear=self._clr_cached_read[idx],
                                                      name=f"read_wide_word_valid_sticky_{idx}") for idx in range(self.num_ID)]

            # Use the addr to mem as the R-M-W sequence can clobber the data register on the SRAM
            # self._last_read_addr = register(self, kts.concat(self._addr_to_mem, self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_inner]), enable=self._ren_to_mem, name="last_read_addr", packed=True)
            if self.num_read_ports == 1:
                self._last_read_addr = [register(self, self._rd_addr_fifo_out_addr[i][self.mem_addr_bit_range_inner], enable=self._ren_to_mem, name=f"last_read_addr_{i}", packed=True) for i in range(self.num_read_ports)]
                self._last_read_addr_wide = [register(self, self._addr_to_mem, enable=self._ren_to_mem, name=f"last_read_addr_wide_{i}", packed=True) for i in range(self.num_read_ports)]
            else:
                self._last_read_addr = [register(self, self._rd_addr_fifo_out_addr[i][self.mem_addr_bit_range_inner], enable=rd_acq[i], name=f"last_read_addr_{i}", packed=True) for i in range(self.num_read_ports)]
                self._last_read_addr_wide = [register(self, self._addr_to_mem, enable=rd_acq[i], name=f"last_read_addr_wide_{i}", packed=True) for i in range(self.num_read_ports)]
                # TODO: important. This signal only works for num_read_ports == 2 and self.optimize_wide and self.mem_width > self.data_width
                self._read_addr_delayed = [register(self, self._rd_addr_fifo_out_addr[i], enable=self._rd_addr_fifo_pop[i] & self._rd_addr_fifo_valid[i],
                                                    name=f"read_addr_delayed_{i}", packed=True) for i in range(self.num_read_ports)]

            # self._last_read_addr = register(self, self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_inner], enable=self._ren_to_mem, name="last_read_addr", packed=True)
            # self._last_read_addr_wide = register(self, self._addr_to_mem, enable=self._ren_to_mem, name="last_read_addr_wide", packed=True)

            if self.num_read_ports == 1:
                self._last_read_ID = register(self, self._rd_ID_fifo_out_data[0], enable=self._ren_to_mem, name="last_read_ID")

            self._set_read_word_addr = [self.var(f"set_read_word_addr_{idx}", 1) for idx in range(self.num_ID)]
            # self._read_word_addr = [self.var(f"read_word_addr_{idx}", self.data_width) for idx in range(self.num_ID)]
            # self._read_word_addr = [register(self, self._last_read_addr,
            self._read_word_addr = [register(self, self._addr_to_mem,
                                             enable=self._set_read_word_addr[idx], name=f"cached_read_word_addr_{idx}") for idx in range(self.num_ID)]

            self._valid_from_mem = self.var("valid_from_mem", 1)
            # Ren full delay...
            # self._ren_full_d1 = [register(self, self._ren_full[idx], enable=kts.const(1, 1), name=f"ren_full_delayed_{idx}") for idx in range(self.num_ID)]
            self._ren_full_d1 = [register(self, self._ren_full[idx] & rd_acq[idx], enable=kts.const(1, 1), name=f"ren_full_delayed_{idx}") for idx in range(self.num_ID)]

            # Determine whether to use the output register of the SRAM or the cached word
            self._use_cached_read = [self.var(f"use_cached_read_{idx}", 1) for idx in range(self.num_ID)]
            self._from_cached_read = [self.var(f"from_cached_read_{idx}", 1) for idx in range(self.num_ID)]
            # First, check that the word is valid (which is set when the proper read happens)
            # Then, if the last read was to this word, we check that the id and addr match (they should)
            # Otherwise, we check the op, addr, ID and joined read valids
            # Furthermore, if we are in a cached state, we drop the use_cached_read is valid_from_mem is high
            # - this is to avoid a situation where we are serving a prior read and get a cached read to serve to the output. Forces the sequence

            self._chosen_read = [self.var(f"chosen_read_{idx}", self.data_width) for idx in range(self.num_ID)]
            # [self.wire(self._chosen_read[idx], kts.ternary(self._use_cached_read[idx] & self._read_wide_word_valid[idx] & ~self._valid_from_mem,

            if self.num_read_ports == 1:

                [self.wire(self._use_cached_read[idx], self._read_wide_word_valid[idx] &
                                                    #    (self._read_word_addr[idx][self.mem_addr_bit_range_outer] == self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer]) &
                                                    #    (self._read_word_addr[idx] == self._addr_to_mem) &
                                                    #    self._read_joined &
                                                    #    kts.ternary((self._valid_from_mem & self._ren_full_d1[idx]),
                                                    kts.ternary((self._valid_from_mem & self._ren_full_d1[idx]),
                                                                (self._last_read_ID == kts.const(idx, 1)) &
                                                                        (self._last_read_addr_wide[0] == self._read_word_addr[idx]),
                                                                (self._rd_ID_fifo_out_data[0] == kts.const(idx, 1)) &
                                                                #    ((self._rd_addr_fifo_out_addr + self._blk_base[idx] + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                                                                #    ((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[idx] + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                                                                #    ((((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[idx]) % (self._buffet_capacity[idx] >> self.subword_addr_bits)) + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                                                                #    ((((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[idx]) >> self._buffet_capacity_log[idx]) + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                                                                ((((self._rd_addr_fifo_out_addr[0][self.mem_addr_bit_range_outer] + self._blk_base[idx]) & self._buffet_capacity_mask[idx]) + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                                                                        # (self._addr_to_mem == self._read_word_addr[idx]) &
                                                                        (self._rd_op_fifo_out_op[0] == kts.const(1, 2)) &
                                                                        ~self._valid_from_mem &
                                                                        # ~self._ren_full_d1[idx] &
                                                                        self._read_joined[0])) for idx in range(self.num_ID)]

                [self.wire(self._chosen_read[idx], kts.ternary(self._use_cached_read[idx] & self._read_wide_word_valid[idx] & ~self._ren_full_d1[idx],
                                                           self._read_wide_word[idx][self._rd_addr_fifo_out_addr[0][self.mem_addr_bit_range_inner]],
                                                        #    self._data_from_mem[self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_inner]])) for idx in range(self.num_ID)]
                                                           self._data_from_mem[self._last_read_addr[0][self.mem_addr_bit_range_inner]])) for idx in range(self.num_ID)]

            else:
                # [self.wire(self._use_cached_read[idx], self._read_wide_word_valid[idx] &
                #                                     #    (self._read_word_addr[idx][self.mem_addr_bit_range_outer] == self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer]) &
                #                                     #    (self._read_word_addr[idx] == self._addr_to_mem) &
                #                                     #    self._read_joined &
                #                                     #    kts.ternary((self._valid_from_mem & self._ren_full_d1[idx]),
                #                                     kts.ternary((self._valid_from_mem & self._ren_full_d1[idx]),
                #                                                         (self._last_read_addr_wide[idx] == self._read_word_addr[idx]),
                #                                                 #    ((self._rd_addr_fifo_out_addr + self._blk_base[idx] + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                #                                                 #    ((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[idx] + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                #                                                 #    ((((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[idx]) % (self._buffet_capacity[idx] >> self.subword_addr_bits)) + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                #                                                 #    ((((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[idx]) >> self._buffet_capacity_log[idx]) + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                #                                                 ((((self._rd_addr_fifo_out_addr[idx][self.mem_addr_bit_range_outer] + self._blk_base[idx]) & self._buffet_capacity_mask[idx]) + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                #                                                         # (self._addr_to_mem == self._read_word_addr[idx]) &
                #                                                         (self._rd_op_fifo_out_op[idx] == kts.const(1, 2)) &
                #                                                         # ~self._valid_from_mem &
                #                                                         ~self._ren_full_d1[idx] &
                #                                                         self._read_joined[idx])) for idx in range(self.num_ID)]

                [self.wire(self._use_cached_read[idx], self._read_wide_word_valid[idx] &
                                                        ((((self._rd_addr_fifo_out_addr[idx][self.mem_addr_bit_range_outer] + self._blk_base[idx]) & self._buffet_capacity_mask[idx]) + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                                                                (self._rd_op_fifo_out_op[idx] == kts.const(1, 2)) &
                                                                # ~self._ren_full_d1[idx] &
                                                                self._read_joined[idx]) for idx in range(self.num_ID)]

                [self.wire(self._from_cached_read[idx], self._read_wide_word_valid[idx] &
                                                                ((((self._read_addr_delayed[idx][self.mem_addr_bit_range_outer] + self._blk_base[idx]) & self._buffet_capacity_mask[idx]) + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                                                                self._read_joined_d1[idx]) for idx in range(self.num_ID)]

                # [self.wire(self._chosen_read[idx], kts.ternary(self._use_cached_read[idx] & self._read_wide_word_valid[idx] & ~self._ren_full_d1[idx],
                #                                            self._read_wide_word[idx][self._rd_addr_fifo_out_addr[idx][self.mem_addr_bit_range_inner]],
                #                                         #    self._data_from_mem[self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_inner]])) for idx in range(self.num_ID)]
                #                                            self._data_from_mem[self._last_read_addr[idx][self.mem_addr_bit_range_inner]])) for idx in range(self.num_ID)]
                [self.wire(self._chosen_read[idx], kts.ternary(~self._ren_full_d1[idx],
                                                           self._read_wide_word[idx][self._read_addr_delayed[idx][self.mem_addr_bit_range_inner]],
                                                           self._data_from_mem[self._last_read_addr[idx][self.mem_addr_bit_range_inner]])) for idx in range(self.num_ID)]
            #  (self._rd_ID_fifo_out_data == kts.const(idx, 1)) &
            #    (self._last_read_ID == kts.const(idx, 1)) &
            #    (self._rd_op_fifo_out_op == kts.const(1, 2))) for idx in range(self.num_ID)]

            self._sram_lock = [self.var(f"sram_lock_{idx}", 1) for idx in range(self.num_ID)]
            self._any_sram_lock = self.var("any_sram_lock", 1)
            self.wire(self._any_sram_lock, (kts.concat(*self._sram_lock).r_or()))

            # Need interface to memory...

            # self._read_d1 = register(self, self._ren_to_mem, enable=kts.const(1, 1), name="read_d1")
            # Read d1 is if we get a read
            self._read_d1 = register(self, kts.concat(self._mem_acq[1], self._mem_acq[3]).r_or(), enable=kts.const(1, 1), name="read_d1")

            if self.num_read_ports == 1:
                self._read_ID_d1 = register(self, self._rd_ID_fifo_out_data[0], enable=self._ren_to_mem, name="read_ID_d1")
            self.wire(self._valid_from_mem, self._read_d1)
            # self._valid_from_mem.add_attribute(ControlSignalAttr(is_control=True))

            # self._ready_from_mem = self.input("ready_from_mem", 1)
            # self._ready_from_mem.add_attribute(ControlSignalAttr(is_control=True))

            # self.base_ports = mem_ctrl_port_interface

            # Build a simple memory
            memory_params = {
                'mem_width': self.mem_width,
                'mem_depth': self.mem_depth
            }

            self.base_ports = [[None]]
            rw_port = MemoryPort(MemoryPortType.READWRITE)
            rw_port_intf = rw_port.get_port_interface()
            rw_port_intf['data_in'] = self._data_to_mem
            rw_port_intf['data_out'] = self._data_from_mem
            rw_port_intf['write_addr'] = self._addr_to_mem_save[self.addr_addressing_bits]
            rw_port_intf['write_enable'] = self._wen_to_mem
            rw_port_intf['read_addr'] = self._addr_to_mem_save[self.addr_addressing_bits]
            rw_port_intf['read_enable'] = self._ren_to_mem
            rw_port.annotate_port_signals()
            self.base_ports[0][0] = rw_port

            if self.local_memory:
                # Create the memory interface based on different params
                mem_ports = [MemoryPort(MemoryPortType.READWRITE, delay=1, active_read=True)]

                self.mem_intf = MemoryInterface(name="memory_mod",
                                                mem_params=memory_params,
                                                ports=mem_ports,
                                                sim_macro_n=not self.physical_mem,
                                                reset_in_sim=True,
                                                tech_map=self.tech_map)
                # Realize the hardware implementation then add it as a child and wire it up...
                self.mem_intf.realize_hw()
                self.add_child('memory_stub',
                            self.mem_intf)

                actual_mem_port_interface = self.mem_intf.get_ports()[0].get_port_interface()

                self.wire(self._gclk, self.mem_intf.get_clock())
                if not self.physical_mem:
                    self.wire(self._rst_n, self.mem_intf.get_reset())

                for pname, psignal in rw_port_intf.items():
                    self.wire(psignal, actual_mem_port_interface[pname])

        elif self.local_memory is False:
            # Need interface to remote memory...
            # self._addr_to_mem = self.output("addr_to_mem", self.data_width, packed=True, explicit_array=True)
            # self._addr_to_mem.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            # self._data_to_mem = self.output("data_to_mem", self.data_width, packed=True, explicit_array=True)
            # self._data_to_mem.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            # self._wen_to_mem = self.output("wen_to_mem", 1)
            # self._wen_to_mem.add_attribute(ControlSignalAttr(is_control=False))

            # self._ren_to_mem = self.output("ren_to_mem", 1)
            # self._ren_to_mem.add_attribute(ControlSignalAttr(is_control=False))

            # self._data_from_mem = self.input("data_from_mem", self.data_width, packed=True, explicit_array=True)
            # self._data_from_mem.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            # self._valid_from_mem = self.input("valid_from_mem", 1)
            # self._valid_from_mem.add_attribute(ControlSignalAttr(is_control=True))

            # self._ready_from_mem = self.input("ready_from_mem", 1)
            # self._ready_from_mem.add_attribute(ControlSignalAttr(is_control=True))

            # Otherwise stamp it out here...
            self._addr_to_mem = self.var("addr_to_mem", self.data_width, packed=True, explicit_array=True)
            # self._addr_to_mem = self.var("addr_to_mem", self.data_width, explicit_array=True)

            self._data_to_mem = self.var("data_to_mem", self.data_width, packed=True, explicit_array=True)
            # self._data_to_mem = self.var("data_to_mem", self.data_width, explicit_array=True)

            self._wen_to_mem = self.var("wen_to_mem", 1)

            self._ren_to_mem = self.var("ren_to_mem", 1)

            self._data_from_mem = self.var("data_from_mem", self.data_width, packed=True, explicit_array=True)

            self._valid_from_mem = self.var("valid_from_mem", 1)

            self._ready_from_mem = self.var("ready_from_mem", 1)

            self.strg_ram_local = StrgRAM(data_width=self.data_width, memory_depth=self.mem_depth)

            self.add_child("memory_ctrl",
                           # Buffet interface
                           self.strg_ram_local,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           wen=self._wen_to_mem,
                           ren=self._ren_to_mem,
                           data_in=self._data_to_mem,
                           wr_addr_in=self._addr_to_mem,
                           rd_addr_in=self._addr_to_mem,
                           data_out=self._data_from_mem,
                           valid_out=self._valid_from_mem,
                           ready=self._ready_from_mem)

            # Get the memory port interface
            mem_ctrl_port_interface = self.strg_ram_local.get_memory_ports()
            # mem_ctrl_port_interface = mem_ctrl_port_interface[0][0].get_port_interface()
            self.base_ports = mem_ctrl_port_interface

        else:
            # Otherwise stamp it out here...
            self._addr_to_mem = self.var("addr_to_mem", self.data_width, packed=True, explicit_array=True)
            # self._addr_to_mem = self.var("addr_to_mem", self.data_width, explicit_array=True)

            self._data_to_mem = self.var("data_to_mem", self.data_width, packed=True, explicit_array=True)
            # self._data_to_mem = self.var("data_to_mem", self.data_width, explicit_array=True)

            self._wen_to_mem = self.var("wen_to_mem", 1)

            self._ren_to_mem = self.var("ren_to_mem", 1)

            self._data_from_mem = self.var("data_from_mem", self.data_width, packed=True, explicit_array=True)

            self._valid_from_mem = self.var("valid_from_mem", 1)

            self._ready_from_mem = self.var("ready_from_mem", 1)

            self.strg_ram_local = StrgRAM()

            self.add_child("memory_ctrl",
                           # Buffet interface
                           self.strg_ram_local,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           wen=self._wen_to_mem,
                           ren=self._ren_to_mem,
                           data_in=self._data_to_mem,
                           wr_addr_in=self._addr_to_mem,
                           rd_addr_in=self._addr_to_mem,
                           data_out=self._data_from_mem,
                           valid_out=self._valid_from_mem,
                           ready=self._ready_from_mem)

            # Get the memory port interface
            mem_ctrl_port_interface = self.strg_ram_local.get_memory_ports()
            mem_ctrl_port_interface = mem_ctrl_port_interface[0][0].get_port_interface()

            # print(mem_ctrl_port_interface)

            # Build a simple memory
            memory_params = {
                'mem_width': 64,
                'mem_depth': 512
            }

            # Create the memory interface based on different params
            mem_ports = [MemoryPort(MemoryPortType.READWRITE, delay=1, active_read=True)]

            self.mem_intf = MemoryInterface(name="memory_mod",
                                            mem_params=memory_params,
                                            ports=mem_ports,
                                            sim_macro_n=not self.physical_mem,
                                            reset_in_sim=True,
                                            tech_map=self.tech_map)
            # Realize the hardware implementation then add it as a child and wire it up...
            self.mem_intf.realize_hw()
            self.add_child('memory_stub',
                           self.mem_intf)

            actual_mem_port_interface = self.mem_intf.get_ports()[0].get_port_interface()

            # print(actual_mem_port_interface)

            self.wire(self._gclk, self.mem_intf.get_clock())
            if not self.physical_mem:
                self.wire(self._rst_n, self.mem_intf.get_reset())

            for pname, psignal in mem_ctrl_port_interface.items():
                self.wire(psignal, actual_mem_port_interface[pname])

# =============================
# FIFO outputs
# =============================

        # Size requests all
        self._size_request_full = self.var("size_request_full", self.num_ID)

        self._rd_rsp_fifo_push = [self.var(f"rd_rsp_fifo_{i}_push", 1) for i in range(self.num_read_ports)]
        self._rd_rsp_fifo_full = [self.var(f"rd_rsp_fifo_{i}_full", 1) for i in range(self.num_read_ports)]
        self._rd_rsp_fifo_almost_full = [self.var(f"rd_rsp_fifo_{i}_almost_full", 1) for i in range(self.num_read_ports)]

        self._rd_rsp_fifo_in_data = [self.var(f"rd_rsp_fifo_{i}_in_data", self.data_width + 1, packed=True) for i in range(self.num_read_ports)]
        self._rd_rsp_out_fifo = [RegFIFO(data_width=self._rd_rsp_fifo_in_data[i].width, width_mult=1,
                                        depth=self.fifo_depth, min_depth=2, defer_hrdwr_gen=self.defer_fifos,
                                        almost_full_diff=0) for i in range(self.num_read_ports)]
        [self._rd_rsp_out_fifo[i].add_attribute(SharedFifoAttr(direction="OUT")) for i in range(self.num_read_ports)]

        [self.add_child(f"rd_rsp_fifo_{i}",
                       self._rd_rsp_out_fifo[i],
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_rsp_fifo_push[i],
                       pop=self._rd_rsp_ready[i],
                       data_in=self._rd_rsp_fifo_in_data[i],
                       #    data_out=self._rd_rsp_data[0][self.data_width - 1, 0])
                       data_out=self._rd_rsp_data[i]) for i in range(self.num_read_ports)]

        # Wire the last bit to 0
        # self.wire(self._rd_rsp_data[0][self.data_width + 1 - 1], kts.const(0, 1))

        [self.wire(self._rd_rsp_fifo_full[i], self._rd_rsp_out_fifo[i].ports.full) for i in range(self.num_read_ports)]
        [self.wire(self._rd_rsp_fifo_almost_full[i], self._rd_rsp_out_fifo[i].ports.almost_full) for i in range(self.num_read_ports)]
        [self.wire(self._rd_rsp_valid[i], ~self._rd_rsp_out_fifo[i].ports.empty) for i in range(self.num_read_ports)]

        chosen_size_block = decode(self, self._size_request_full, self._blk_bounds)  # Changing the base to 0

        if self.optimize_wide and self.mem_width > self.data_width:
            # self.wire(self._rd_rsp_fifo_in_data[self.data_width - 1, 0], kts.ternary(self._use_cached_read[0] & self._read_wide_word_valid[0] & (self._rd_ID_fifo_out_data[0] == kts.const(0, 1)),
            #                                                                          self._chosen_read[0],
            #                                                                          kts.ternary(self._use_cached_read[1] & self._read_wide_word_valid[1] & (self._rd_ID_fifo_out_data[0] == kts.const(1, 1)),
            #                                                                                      self._chosen_read[1],
            #                                                                                      kts.ternary(~self._use_cached_read[0] & self._valid_from_mem & (self._read_ID_d1[0] == kts.const(0, 1)),
            #                                                                                                  self._chosen_read[0],
            #                                                                                                  kts.ternary(~self._use_cached_read[1] & self._valid_from_mem & (self._read_ID_d1[0] == kts.const(1, 1)),
            #                                                                                                              self._chosen_read[1],
            #                                                                                                              chosen_size_block + 1)))))

            # Shove back in the chosen read or the block size
            if self.num_read_ports == 2:

                # [self.wire(self._rd_rsp_fifo_in_data[i][self.data_width - 1, 0], kts.ternary(self._use_cached_read[i] & self._read_wide_word_valid[i],
                #                                                                      self._chosen_read[i], chosen_size_block)) for i in range(self.num_read_ports)]
                # [self.wire(self._rd_rsp_fifo_in_data[i][self.data_width], kts.ternary(self._use_cached_read[i],
                #                                                                   kts.const(0, 1),
                #                                                                   kts.const(1, 1))) for i in range(self.num_read_ports)]

                [self.wire(self._rd_rsp_fifo_in_data[i][self.data_width - 1, 0], kts.ternary((self._from_cached_read[i]) & self._read_wide_word_valid[i],
                                                                                     self._chosen_read[i], chosen_size_block)) for i in range(self.num_read_ports)]
                [self.wire(self._rd_rsp_fifo_in_data[i][self.data_width], kts.ternary(self._from_cached_read[i],
                                                                                  kts.const(0, 1),
                                                                                  kts.const(1, 1))) for i in range(self.num_read_ports)]
            else:
                self.wire(self._rd_rsp_fifo_in_data[0][self.data_width - 1, 0], kts.ternary(self._use_cached_read[0] & self._read_wide_word_valid[0],
                                                                                     self._chosen_read[0],
                                                                                     kts.ternary(self._use_cached_read[1] & self._read_wide_word_valid[1],
                                                                                                 self._chosen_read[1],
                                                                                                 chosen_size_block)))
                self.wire(self._rd_rsp_fifo_in_data[0][self.data_width], kts.ternary(self._use_cached_read[0],
                                                                                  kts.const(0, 1),
                                                                                  kts.const(1, 1)))

            # self.wire(self._rd_rsp_fifo_in_data[self.data_width], self._read_rsp_ID_reg)

            # self.wire(self._rd_rsp_fifo_push, self._valid_from_mem | (kts.concat(*self._use_cached_read).r_or()) | self._size_request_full.r_or())

            # self.wire(self._rd_rsp_fifo_push, self._valid_from_mem | (kts.concat(*self._use_cached_read).r_or()) | self._size_request_full.r_or())
            # [self.wire(self._rd_rsp_fifo_push[i], self._valid_from_mem | self._use_cached_read[i] | self._size_request_full[i]) for i in range(self.num_read_ports)]
            if self.num_read_ports == 2:
                # [self.wire(self._rd_rsp_fifo_push[i], self._ren_full_d1[i] | self._use_cached_read[i] | self._size_request_full[i]) for i in range(self.num_read_ports)]
                [self.wire(self._rd_rsp_fifo_push[i], self._from_cached_read[i] | self._size_request_full[i]) for i in range(self.num_read_ports)]
            else:
                [self.wire(self._rd_rsp_fifo_push[i], self._valid_from_mem | (kts.concat(*self._use_cached_read)).r_or() | self._size_request_full.r_or()) for i in range(self.num_read_ports)]

        else:

            self.wire(self._rd_rsp_fifo_push, self._valid_from_mem | self._size_request_full.r_or())
            self.wire(self._rd_rsp_fifo_in_data[self.data_width - 1, 0], kts.ternary(self._valid_from_mem, self._data_from_mem, chosen_size_block))

            self._read_rsp_ID_reg = register(self, kts.ternary(self._read_pop_full[1], kts.const(1, 1), kts.const(0, 1)), enable=self._read_pop)

            self.wire(self._rd_rsp_fifo_in_data[self.data_width], self._read_rsp_ID_reg)
# # =============================
# #  Join Logic
# # =============================

        self.wire(self._joined_in_fifo, self._wr_data_fifo_valid & self._wr_addr_fifo_valid & self._wr_ID_fifo_valid)

        # Broadcast the pop
        self._pop_in_fifos = self.var("pop_in_fifos", 1)
        self.wire(kts.concat(self._wr_addr_fifo_pop, self._wr_data_fifo_pop, self._wr_ID_fifo_pop), kts.concat(*[self._pop_in_fifos for i in range(3)]))

        # Each FSM can assert pop, need to make sure this is 1-hot though
        self._pop_in_full = self.var("pop_in_full", self.num_ID)
        self.wire(self._pop_in_fifos, self._pop_in_full.r_or())

        if self.num_read_ports == 1:
            self.wire(kts.concat(self._rd_ID_fifo_pop[0], self._rd_op_fifo_pop[0], self._rd_addr_fifo_pop[0]), kts.concat(*[self._read_pop[0] for j in range(3)]))
            self.wire(self._read_pop[i], self._read_pop_full.r_or())
        else:
            [self.wire(kts.concat(self._rd_op_fifo_pop[i], self._rd_addr_fifo_pop[i]), kts.concat(*[self._read_pop[i] for j in range(2)])) for i in range(self.num_read_ports)]
            [self.wire(self._read_pop[i], self._read_pop_full[i]) for i in range(self.num_read_ports)]

# # =============================
# #  FSM
# # =============================

        # self._en_curr_bounds = self.var("en_curr_bounds", self.num_ID)
        # # self._curr_bounds = self.var("curr_bounds", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        # self._curr_bounds = [register(self, self._wr_addr_fifo_out_data, enable=self._en_curr_bounds[i], name=f"curr_bounds_{i}", packed=True) for i in range(self.num_ID)]

        # self._en_curr_base = self.var("en_curr_base", self.num_ID)
        # # self._curr_base = [register(self, self._wr_addr, enable=self._en_curr_base[i], name=f"curr_base_{i}", packed=True) for i in range(self.num_ID)]
        # self._curr_base = [register(self, self._curr_bounds[i] + 1, enable=self._en_curr_base[i], name=f"curr_base_{i}", packed=True) for i in range(self.num_ID)]
        # self._curr_bounds = register(self, self._bounds_ctr, enable=self._en_curr_bounds)

        self._push_blk = self.var("push_blk", self.num_ID)
        self._pop_blk = self.var("pop_blk", self.num_ID)
        self._blk_valid = self.var("blk_valid", self.num_ID)
        self._blk_full = self.var("blk_full", self.num_ID)

        self._blk_count = [self.var(f"blk_count_{i}", 8) for i in range(self.num_ID)]  # Log 512, approximately the maximum num of blk storage

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def blk_lock(self, idx):
            if ~self._rst_n:
                self._blk_count[idx] = 0
            elif self._push_blk[idx]:
                self._blk_count[idx] = self._blk_count[idx] + 1
            elif self._pop_blk[idx]:
                self._blk_count[idx] = self._blk_count[idx] - 1
            else:
                self._blk_count[idx] = self._blk_count[idx]

        [self.add_code(blk_lock, idx=i) for i in range(self.num_ID)]

        self._curr_capacity_pre = self.var("curr_capacity_pre", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        # self._curr_capacity = self.var("curr_capacity", self.data_width, size=self.num_ID, explicit_array=True, packed=True)

        @always_ff((posedge, self._clk), (negedge, self._rst_n))
        def cap_reg(self, idx):
            if ~self._rst_n:
                self._curr_capacity_pre[idx] = kts.const(0, self.data_width)
            # Only update when pushed or popped
            elif self._push_blk[idx] or self._pop_blk[idx]:
                self._curr_capacity_pre[idx] = self._curr_capacity_pre[idx] + kts.ternary(self._push_blk[idx],
                                                                                        #   self._blk_bounds[idx],
                                                                                          self._curr_bounds[idx],
                                                                                          kts.const(0, width=self.data_width)) - kts.ternary(self._pop_blk[idx],
                                                                                                                                             self._blk_bounds[idx],
                                                                                                                                             kts.const(0, width=self.data_width))

        [self.add_code(cap_reg, idx=i) for i in range(self.num_ID)]

        if self.optimize_wide and self.mem_width > self.data_width:

            self._previous_op = register(self, self._wr_data_fifo_out_op, enable=kts.const(1, 1), name="PREVIOUS_WR_OP")

            # Create FSM
            self.write_fsm = [self.add_fsm(f"write_fsm_{i}", reset_high=False) for i in range(self.num_ID)]
            WR_START = [self.write_fsm[i].add_state(f"WR_START_{i}") for i in range(self.num_ID)]
            WRITING = [self.write_fsm[i].add_state(f"WRITING_{i}") for i in range(self.num_ID)]
            MODIFY = [self.write_fsm[i].add_state(f"MODIFY_{i}") for i in range(self.num_ID)]

            ####################
            # Next State Logic WRITE
            ####################
            for ID_idx in range(self.num_ID):

                self.write_fsm[ID_idx].output(self._push_blk[ID_idx])
                self.write_fsm[ID_idx].output(self._en_curr_base[ID_idx])
                self.write_fsm[ID_idx].output(self._en_curr_bounds[ID_idx])
                self.write_fsm[ID_idx].output(self._wen_full[ID_idx])
                self.write_fsm[ID_idx].output(self._pop_in_full[ID_idx])
                self.write_fsm[ID_idx].output(self._set_write_wide_word[ID_idx])
                self.write_fsm[ID_idx].output(self._clr_write_wide_word[ID_idx])
                self.write_fsm[ID_idx].output(self._write_to_sram[ID_idx])
                # self.write_fsm[ID_idx].output(self._write_to_wide_word[ID_idx])
                self.write_fsm[ID_idx].output(self._set_wide_word_addr[ID_idx])
                self.write_fsm[ID_idx].output(self._sram_lock[ID_idx])
                self.write_fsm[ID_idx].output(self._read_from_sram_write_side[ID_idx])
                self.write_fsm[ID_idx].output(self._clr_curr_bounds[ID_idx], default=kts.const(0, 1))
                # self.write_fsm.output(self._en_curr_bounds)

                ####################
                # WR_START #
                ####################
                # Start state gets an allocate command
                WR_START[ID_idx].next(WRITING[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)) & self._tile_en)
                WR_START[ID_idx].next(WR_START[ID_idx], None)

                ####################
                # WRITING #
                ####################
                # Writing until we get a finalize...

                # Additional condition that we are either writing the full word (thus we don't need to do any modify), or
                # we have no outstanding writes (0 valid bits in the mask)
                # WRITING[ID_idx].next(WR_START[ID_idx], self._joined_in_fifo &
                #                                        (self._wr_data_fifo_out_op == 0) &
                #                                        self._mem_acq[2 * ID_idx + 0] &
                #                                        ~self._blk_full[ID_idx] &
                #                                        (self._write_full_word[ID_idx] | (self._num_bits_valid_mask[ID_idx] == 0)) &
                #                                        (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                WRITING[ID_idx].next(WR_START[ID_idx], self._joined_in_fifo &
                                                       (self._wr_data_fifo_out_op == 0) &
                                                       ~self._blk_full[ID_idx] &
                                                       ((self._write_full_word[ID_idx] & self._mem_acq[2 * ID_idx + 0]) | (self._num_bits_valid_mask[ID_idx] == 0)) &
                                                       (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # Go to modify if we get a change in address on a non-full word (and we get lock access) or a finalize command
                # Furthermore, make sure there is at least some data in there.
                # Either finalize or write to a new address, with nonzero num bits and not full word
                WRITING[ID_idx].next(MODIFY[ID_idx], self._joined_in_fifo &
                                                     (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)) &
                                                     self._mem_acq[2 * ID_idx + 0] &
                                                     ((self._wr_data_fifo_out_op == kts.const(0, 1)) |
                                                        # ((((self._curr_base[ID_idx] + self._buffet_base[ID_idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) != self._write_word_addr[ID_idx]) & self._write_word_addr_valid[ID_idx]) &
                                                        (((self._tmp_wr_addr[ID_idx] != self._write_word_addr[ID_idx]) & self._write_word_addr_valid[ID_idx]) &
                                                            (self._wr_data_fifo_out_op == kts.const(1, 1)))) &
                                                        (self._num_bits_valid_mask[ID_idx] > 0) &
                                                        ~self._write_full_word[ID_idx])
                WRITING[ID_idx].next(WRITING[ID_idx], None)

                ####################
                # MODIFY #
                ####################
                # Go back to writing when we have finished the modify (which should happen immediately since we have the resource lock)
                # MODIFY[ID_idx].next(WRITING[ID_idx], kts.const(1, 1))
                # MODIFY[ID_idx].next(WRITING[ID_idx], self._wr_data_fifo_out_op == kts.const(1, 1))
                MODIFY[ID_idx].next(WRITING[ID_idx], (self._previous_op == kts.const(1, 1)))
                # MODIFY[ID_idx].next(WR_START[ID_idx], self._wr_data_fifo_out_op == kts.const(0, 1))
                MODIFY[ID_idx].next(WR_START[ID_idx], (self._previous_op == kts.const(0, 1)) & ~self._blk_full[ID_idx])
                MODIFY[ID_idx].next(MODIFY[ID_idx], None)

                ####################
                # Output Logic WRITE
                ####################

                ####################
                # WR_START #
                ####################
                WR_START[ID_idx].output(self._push_blk[ID_idx], 0)
                WR_START[ID_idx].output(self._en_curr_base[ID_idx], 0)
                WR_START[ID_idx].output(self._en_curr_bounds[ID_idx], 0)
                WR_START[ID_idx].output(self._wen_full[ID_idx], 0)
                WR_START[ID_idx].output(self._pop_in_full[ID_idx], (self._wr_data_fifo_out_op == 0) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                WR_START[ID_idx].output(self._set_write_wide_word[ID_idx], 0)
                WR_START[ID_idx].output(self._clr_write_wide_word[ID_idx], 0)
                WR_START[ID_idx].output(self._write_to_sram[ID_idx], 0)
                # WR_START[ID_idx].output(self._write_to_wide_word[ID_idx], 0)
                WR_START[ID_idx].output(self._set_wide_word_addr[ID_idx], 0)
                WR_START[ID_idx].output(self._sram_lock[ID_idx], 0)
                WR_START[ID_idx].output(self._read_from_sram_write_side[ID_idx], 0)
                WR_START[ID_idx].output(self._clr_curr_bounds[ID_idx], 1)

                ####################
                # WRITING #
                ####################
                # Increment wr addr if we get wr access
                # WRITING[ID_idx].output(self._inc_wr_addr, self._mem_acq[0] & self._wr_data_fifo_valid & (self._wr_data_fifo_out_op == 1))
                # WRITING[ID_idx].output(self._inc_bounds_ctr, self._mem_acq[0] & self._wr_data_fifo_valid & (self._wr_data_fifo_out_op == 1))
                # WRITING[ID_idx].output(self._clr_bounds_ctr, 0)
                # Only want to push the block if there is room and we are either writing a full word or there are no outstanding writes
                WRITING[ID_idx].output(self._push_blk[ID_idx], self._joined_in_fifo &
                                                               (self._wr_data_fifo_out_op == 0) &
                                                               ~self._blk_full[ID_idx] &
                                                               ((self._write_full_word[ID_idx] & self._mem_acq[2 * ID_idx + 0]) | (self._num_bits_valid_mask[ID_idx] == 0)) &
                                                               (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # WRITING[ID_idx].output(self._en_curr_base[ID_idx], 0)
                # On our way back to write start we can set the current base for the next block. it is 0 at reset, so we don't need to deal with it there
                WRITING[ID_idx].output(self._en_curr_base[ID_idx], self._joined_in_fifo &
                                                                  (self._wr_data_fifo_out_op == 0) &
                                                                  ~self._blk_full[ID_idx] &
                                                                  ((self._write_full_word[ID_idx] & self._mem_acq[2 * ID_idx + 0]) | (self._num_bits_valid_mask[ID_idx] == 0)) &
                                                                  (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))  # We do allocate a word for empty to void RW hazards

                WRITING[ID_idx].output(self._set_write_wide_word[ID_idx], ((self._tmp_wr_addr[ID_idx] == self._write_word_addr[ID_idx]) &
                                                                                self._write_word_addr_valid[ID_idx]) &
                                                                                self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                                                                                (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # Any time we make a write to the current block we can update the bounds of the current block
                # WRITING[ID_idx].output(self._en_curr_bounds[ID_idx], self._mem_acq[2 * ID_idx + 0] & self._joined_in_fifo &
                WRITING[ID_idx].output(self._en_curr_bounds[ID_idx], (self._mem_acq[2 * ID_idx + 0] | self._set_write_wide_word[ID_idx]) & self._joined_in_fifo &
                                                                        (self._wr_data_fifo_out_op == 1) &
                                                                        (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # WRITING[ID_idx].output(self._wen_full[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & ~(self._curr_capacity < self._buffet_capacity))

                # Only make the wen when there is room and the proper ID is being addressed
                WRITING[ID_idx].output(self._wen_full[ID_idx], self._joined_in_fifo &
                                                               (self._wr_data_fifo_out_op == 1) &
                                                               (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) &
                                                               (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # This is horrendous to look at, but if we are in the case where we are needing to push the blk, we need to make sure there are no outstanding
                # writes as well
                # WRITING[ID_idx].output(self._pop_in_full[ID_idx], (self._mem_acq[2 * ID_idx + 0] &
                #                                                         self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                #                                                         (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) &
                #                                                         (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width))) |
                #                                                   (self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & ~self._blk_full[ID_idx] &
                #                                                         ((self._write_full_word[ID_idx] & self._mem_acq[2 * ID_idx + 0]) | (self._num_bits_valid_mask[ID_idx] == 0)) &
                #                                                         (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width))))

                # Only clear the word
                WRITING[ID_idx].output(self._clr_write_wide_word[ID_idx], (((self._tmp_wr_addr[ID_idx] != self._write_word_addr[ID_idx]) | ~self._write_word_addr_valid[ID_idx]) |
                                                                                (((self._tmp_wr_addr[ID_idx] == self._write_word_addr[ID_idx]) & self._write_word_addr_valid[ID_idx]) &
                                                                                    self._write_full_word[ID_idx])) &
                                                                          self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                                                                          self._mem_acq[2 * ID_idx + 0] &
                                                                          (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # In the WRITING state, the only way to actually write the word to SRAM is if it is full
                WRITING[ID_idx].output(self._write_to_sram[ID_idx], self._write_full_word[ID_idx] &
                                                                          self._joined_in_fifo &
                                                                          (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) &
                                                                        #   self._mem_acq[2 * ID_idx + 0] &
                                                                          (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                WRITING[ID_idx].output(self._set_wide_word_addr[ID_idx], ((self._tmp_wr_addr[ID_idx] != self._write_word_addr[ID_idx]) | ~self._write_word_addr_valid[ID_idx]) &
                                                                          self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                                                                          # Only overwrite the wide word addr if the current full
                                                                          (kts.ternary(self._write_wide_word_mask_reg_out[ID_idx].r_or(), self._mem_acq[2 * ID_idx + 0], kts.const(1, 1))) &
                                                                          (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))

                WRITING[ID_idx].output(self._sram_lock[ID_idx], 0)
                # We are reading from the SRAM if we are transitioning to MODIFY
                WRITING[ID_idx].output(self._read_from_sram_write_side[ID_idx], self._joined_in_fifo &
                                                                                (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)) &
                                                                                # self._mem_acq[2 * ID_idx + 0] &
                                                                                ~self._any_sram_lock &
                                                                                (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) &
                                                                                ((self._wr_data_fifo_out_op == kts.const(0, 1)) |
                                                                                    (((self._tmp_wr_addr[ID_idx] != self._write_word_addr[ID_idx]) &
                                                                                        self._write_word_addr_valid[ID_idx]) &
                                                                                    (self._wr_data_fifo_out_op == kts.const(1, 1)))) &
                                                                                (self._num_bits_valid_mask[ID_idx] > 0) &
                                                                                ~self._write_full_word[ID_idx])
                # WRITING.output(self._en_curr_bounds, 0)
                WRITING[ID_idx].output(self._pop_in_full[ID_idx], ((self._mem_acq[2 * ID_idx + 0] | self._set_write_wide_word[ID_idx]) &
                                                                        self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                                                                        (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) &
                                                                        (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width))) |
                                                                  (self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & ~self._blk_full[ID_idx] &
                                                                        ((self._write_full_word[ID_idx] & self._mem_acq[2 * ID_idx + 0]) | (self._num_bits_valid_mask[ID_idx] == 0)) &
                                                                        (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width))))

                # TODO: deal with a full word write backed up by a new write - need to clear only some of the bits

                ####################
                # MODIFY #
                ####################
                # MODIFY[ID_idx].output(self._push_blk[ID_idx], 0)
                MODIFY[ID_idx].output(self._push_blk[ID_idx], (self._previous_op == kts.const(0, 1)) & ~self._blk_full[ID_idx])
                MODIFY[ID_idx].output(self._en_curr_base[ID_idx], (self._previous_op == kts.const(0, 1)) & ~self._blk_full[ID_idx])
                MODIFY[ID_idx].output(self._en_curr_bounds[ID_idx], 0)
                # Simply write with the lock here
                MODIFY[ID_idx].output(self._wen_full[ID_idx], ~self._blk_full[ID_idx])
                # MODIFY[ID_idx].output(self._pop_in_full[ID_idx], 0)
                MODIFY[ID_idx].output(self._pop_in_full[ID_idx], ~self._blk_full[ID_idx])
                MODIFY[ID_idx].output(self._set_write_wide_word[ID_idx], 0)
                MODIFY[ID_idx].output(self._clr_write_wide_word[ID_idx], ~self._blk_full[ID_idx])
                MODIFY[ID_idx].output(self._write_to_sram[ID_idx], ~self._blk_full[ID_idx])
                MODIFY[ID_idx].output(self._read_from_sram_write_side[ID_idx], 0)
                # MODIFY[ID_idx].output(self._write_to_wide_word[ID_idx], 0)
                MODIFY[ID_idx].output(self._set_wide_word_addr[ID_idx], 0)
                MODIFY[ID_idx].output(self._sram_lock[ID_idx], ~self._blk_full[ID_idx])

            ##### Create read side fsm separately.

            #####################
            # READING #
            #####################

            self.read_fsm = [self.add_fsm(f"read_fsm_{i}", reset_high=False) for i in range(self.num_ID)]
            RD_START = [self.read_fsm[i].add_state(f"RD_START_{i}") for i in range(self.num_ID)]
            RD_PAUSE = [self.read_fsm[i].add_state(f"RD_PAUSE_{i}") for i in range(self.num_ID)]
            RD_PAUSE_T = [self.read_fsm[i].add_state(f"RD_PAUSE_T_{i}") for i in range(self.num_ID)]

            for ID_idx in range(self.num_ID):

                if self.num_read_ports == 2:
                    self._rd_ID_fifo_check = kts.const(1, 1)
                    read_joined_use = self._read_joined[ID_idx]
                    op_fifo_use = self._rd_op_fifo_out_op[ID_idx]
                    addr_fifo_use = self._rd_addr_fifo_out_addr[ID_idx]
                    rd_rsp_fifo_almost_full_use = self._rd_rsp_fifo_almost_full[ID_idx]
                    read_ID_d1_proxy = kts.const(1, 1)
                    rd_rsp_fifo_full_proxy = self._rd_rsp_fifo_full[ID_idx]
                else:
                    self._rd_ID_fifo_check = (self._rd_ID_fifo_out_data[0] == kts.const(ID_idx, self._rd_ID_fifo_out_data[0].width))
                    read_joined_use = self._read_joined[0]
                    op_fifo_use = self._rd_op_fifo_out_op[0]
                    addr_fifo_use = self._rd_addr_fifo_out_addr[0]
                    rd_rsp_fifo_almost_full_use = self._rd_rsp_fifo_almost_full[0]
                    read_ID_d1_proxy = (self._read_ID_d1 == kts.const(ID_idx, 1))
                    rd_rsp_fifo_full_proxy = self._rd_rsp_fifo_full[0]

                ####################
                # RD_START
                ####################
                # Get the first block size...
                RD_START[ID_idx].next(RD_PAUSE[ID_idx], (self._blk_count[ID_idx] == 0) & (op_fifo_use == 0) &
                                                        read_joined_use & self._rd_ID_fifo_check)
                RD_START[ID_idx].next(RD_START[ID_idx], None)

                RD_PAUSE[ID_idx].next(RD_PAUSE_T[ID_idx], self._push_blk[ID_idx])
                RD_PAUSE[ID_idx].next(RD_PAUSE[ID_idx], None)

                RD_PAUSE_T[ID_idx].next(RD_START[ID_idx], None)

                self.read_fsm[ID_idx].output(self._pop_blk[ID_idx])
                # self.read_fsm[ID_idx].output(self._rd_addr_loc[ID_idx])
                self.read_fsm[ID_idx].output(self._ren_full[ID_idx])
                self.read_fsm[ID_idx].output(self._read_pop_full[ID_idx])
                self.read_fsm[ID_idx].output(self._size_request_full[ID_idx])
                self.read_fsm[ID_idx].output(self._set_cached_read[ID_idx])
                self.read_fsm[ID_idx].output(self._clr_cached_read[ID_idx])
                self.read_fsm[ID_idx].output(self._set_read_word_addr[ID_idx])

                ####################
                # RD_PAUSE
                ####################

                RD_PAUSE[ID_idx].output(self._pop_blk[ID_idx], 0)
                # RD_PAUSE[ID_idx].output(self._rd_addr_loc[ID_idx], 0)
                RD_PAUSE[ID_idx].output(self._ren_full[ID_idx], 0)
                RD_PAUSE[ID_idx].output(self._read_pop_full[ID_idx], 0)
                RD_PAUSE[ID_idx].output(self._size_request_full[ID_idx], 0)
                RD_PAUSE[ID_idx].output(self._set_cached_read[ID_idx], 0)
                RD_PAUSE[ID_idx].output(self._clr_cached_read[ID_idx], 0)
                RD_PAUSE[ID_idx].output(self._set_read_word_addr[ID_idx], 0)

                ####################
                # RD_PAUSE_T
                ####################

                RD_PAUSE_T[ID_idx].output(self._pop_blk[ID_idx], 1)
                # RD_PAUSE_T[ID_idx].output(self._rd_addr_loc[ID_idx], 0)
                RD_PAUSE_T[ID_idx].output(self._ren_full[ID_idx], 0)
                RD_PAUSE_T[ID_idx].output(self._read_pop_full[ID_idx], 0)
                RD_PAUSE_T[ID_idx].output(self._size_request_full[ID_idx], 0)
                RD_PAUSE_T[ID_idx].output(self._set_cached_read[ID_idx], 0)
                RD_PAUSE_T[ID_idx].output(self._clr_cached_read[ID_idx], 0)
                RD_PAUSE_T[ID_idx].output(self._set_read_word_addr[ID_idx], 0)

                ####################
                # RD_START
                ####################

                # if self.num_read_ports == 2:
                #     self._rd_ID_fifo_check = kts.const(1, 1)
                #     read_joined_use = self._read_joined[ID_idx]
                #     op_fifo_use = self._rd_op_fifo_out_op[ID_idx]
                #     addr_fifo_use = self._rd_addr_fifo_out_addr[ID_idx]
                #     rd_rsp_fifo_almost_full_use = self._rd_rsp_fifo_almost_full[ID_idx]
                #     read_ID_d1_proxy = kts.const(1, 1)
                #     rd_rsp_fifo_full_proxy = self._rd_rsp_fifo_full[ID_idx]
                # else:
                #     self._rd_ID_fifo_check = (self._rd_ID_fifo_out_data[0] == kts.const(ID_idx, self._rd_ID_fifo_out_data[0].width))
                #     read_joined_use = self._read_joined[0]
                #     op_fifo_use = self._rd_op_fifo_out_op[0]
                #     addr_fifo_use = self._rd_addr_fifo_out_addr[0]
                #     rd_rsp_fifo_almost_full_use = self._rd_rsp_fifo_almost_full[0]
                #     read_ID_d1_proxy = (self._read_ID_d1 == kts.const(ID_idx, 1))
                #     rd_rsp_fifo_full_proxy = self._rd_rsp_fifo_full[0]

                RD_START[ID_idx].output(self._pop_blk[ID_idx], (op_fifo_use == 0) &
                                                               read_joined_use &
                                                            #    self._rd_ID_fifo_check)
                                                               self._rd_ID_fifo_check & (self._blk_count[ID_idx] > 0))
                # Guarantee there's room for the read to land (need to use almost full, not full...)
                # RD_START[ID_idx].output(self._ren_full[ID_idx], (self._rd_op_fifo_out_op == 1) & self._read_joined & ~self._rd_rsp_fifo_full & self._blk_valid[ID_idx] & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                RD_START[ID_idx].output(self._ren_full[ID_idx], (op_fifo_use == 1) &
                                                                ~self._use_cached_read[ID_idx] &
                                                                read_joined_use &
                                                                ~rd_rsp_fifo_almost_full_use &
                                                                self._blk_valid[ID_idx] &
                                                                # ((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] != self._read_word_addr[ID_idx]) | ~self._read_wide_word_valid[ID_idx]) &
                                                                (((addr_fifo_use[self.mem_addr_bit_range_outer] + self._blk_base[ID_idx] + self._buffet_base[ID_idx]) != self._read_word_addr[ID_idx]) | ~self._read_wide_word_valid[ID_idx]) &
                                                                self._rd_ID_fifo_check)
                # Pop the op fifo if there is a read that's going through or if it's a free op
                # If it's a size request, only fulfill it if we aren't pushing a read from memory to the output fifo
                RD_START[ID_idx].output(self._read_pop_full[ID_idx], kts.ternary(op_fifo_use == 2,
                                                                                #  ~self._valid_from_mem & self._blk_valid[ID_idx],
                                                                                 ~self._ren_full_d1[ID_idx] & self._blk_valid[ID_idx],
                                                                                 kts.ternary(op_fifo_use == 1,
                                                                                            #  (self._mem_acq[2 * ID_idx + 1] | self._use_cached_read[ID_idx]) & ~self._rd_rsp_fifo_full,
                                                                                            #  (self._mem_acq[2 * ID_idx + 1] | (self._use_cached_read[ID_idx] & ~self._valid_from_mem)) & ~rd_rsp_fifo_full_proxy,
                                                                                            #  (self._mem_acq[2 * ID_idx + 1] | (self._use_cached_read[ID_idx] & ~self._ren_full_d1[ID_idx])) & ~rd_rsp_fifo_full_proxy,
                                                                                             (self._mem_acq[2 * ID_idx + 1] | (self._use_cached_read[ID_idx])) & ~rd_rsp_fifo_full_proxy,
                                                                                             kts.const(1, 1))) &
                                                                                 read_joined_use &
                                                                                 self._rd_ID_fifo_check)
                RD_START[ID_idx].output(self._size_request_full[ID_idx], self._blk_valid[ID_idx] &
                                                                         (op_fifo_use == 2) &
                                                                         read_joined_use &
                                                                         self._rd_ID_fifo_check)
                # The caching is a cycle behind because it is not activated until the read returns
                # RD_START[ID_idx].output(self._set_cached_read[ID_idx], self._valid_from_mem &
                RD_START[ID_idx].output(self._set_cached_read[ID_idx], self._ren_full_d1[ID_idx] &
                                                                       read_ID_d1_proxy)
                # We clear the cached read if we are popping the block.
                RD_START[ID_idx].output(self._clr_cached_read[ID_idx], (op_fifo_use == 0) &
                                                                       read_joined_use &
                                                                       self._rd_ID_fifo_check)
                # Set the read word's addr if we have a valid back to the same ID and the address is different
                # RD_START[ID_idx].output(self._set_read_word_addr[ID_idx], self._valid_from_mem &
                #                                                           (self._last_read_addr[self.mem_addr_bit_range_outer] != self._read_word_addr[ID_idxm1	===>	fiber_access_0		===>	X07_Y01elf.mem_addr_bit_range_outer]) &
                #                                                         #   (self._read_ID_d1[ID_idx] == kts.const(ID_idx, 1)))
                #                                                           (self._read_ID_d1 == kts.const(ID_idx, 1)))
                RD_START[ID_idx].output(self._set_read_word_addr[ID_idx], self._ren_full[ID_idx] &
                                                                          self._mem_acq[2 * ID_idx + 1] &
                                                                        #   (self._addr_to_mem[self.mem_addr_bit_range_outer] != self._read_word_addr[ID_idx][self.mem_addr_bit_range_outer]) &
                                                                          (self._addr_to_mem != self._read_word_addr[ID_idx]) &
                                                                        #   (self._read_ID_d1[ID_idx] == kts.const(ID_idx, 1)))
                                                                          self._rd_ID_fifo_check)

            for i in range(self.num_ID):
                self.write_fsm[i].set_start_state(WR_START[i])
                self.read_fsm[i].set_start_state(RD_START[i])

            for i in range(self.num_ID):
                ### Bookkeeping FIFO
                blk_fifo_in = kts.concat(self._curr_base[i], self._curr_bounds[i])
                blk_fifo = RegFIFO(data_width=blk_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=False)

                self.add_child(f"blk_fifo_{i}",
                            blk_fifo,
                            clk=self._gclk,
                            rst_n=self._rst_n,
                            clk_en=self._clk_en,
                            push=self._push_blk[i],
                            pop=self._pop_blk[i],
                            data_in=blk_fifo_in,
                            data_out=kts.concat(self._blk_base[i], self._blk_bounds[i]))  # added empty control signal

                self.wire(self._blk_full[i], blk_fifo.ports.full)
                self.wire(self._blk_valid[i], ~blk_fifo.ports.empty)

            # Force FSM realization first so that flush gets added...
            kts.passes.realize_fsm(self.internal_generator)

            # Arbitrate between the write/read side with RR arbiter
            self.port_arbiter = Arbiter(ins=2 * self.num_ID,
                                        algo="RR")

            # base_rr = kts.concat(self._ren_full[0], self._wen_full[0])
            # base_rr = kts.concat(self._ren_full[0], self._write_to_sram[0] | self._read_from_sram_write_side[0] | self._wen_full[0])
            base_rr = kts.concat(self._ren_full[0], self._write_to_sram[0] | self._read_from_sram_write_side[0])
            for i in range(self.num_ID - 1):
                # base_rr = kts.concat(self._ren_full[i + 1], self._wen_full[i + 1], base_rr)
                # base_rr = kts.concat(self._ren_full[i + 1], self._write_to_sram[i + 1] | self._read_from_sram_write_side[i + 1] | self._wen_full[i + 1], base_rr)
                base_rr = kts.concat(self._ren_full[i + 1], self._write_to_sram[i + 1] | self._read_from_sram_write_side[i + 1], base_rr)

            brr = self.var("base_rr", 2 * self.num_ID)
            self.wire(brr, base_rr)

            # The "ready_from_mem" is now based on a resource lock for the rmw stages of the write side

            self.add_child(f"rr_arbiter",
                        self.port_arbiter,
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        clk_en=self._clk_en,
                        request_in=brr,
                        grant_out=self._mem_acq,
                        # resource_ready=self._ready_from_mem)
                        resource_ready=~self._any_sram_lock)

            # wr_acqs = []
            # rd_acqs = []
            # # wr_acq = self._mem_acq[0] & self._wen_full[0]
            # # wr_acqs.append(self._mem_acq[0] & self._wen_full[0])
            # # wr_acq = self._mem_acq[0] & (self._write_to_sram[0] | self._read_from_sram_write_side[0])
            # wr_acq = self._mem_acq[0] & (self._write_to_sram[0])
            # # wr_acqs.append(self._mem_acq[0] & (self._write_to_sram[0] | self._read_from_sram_write_side[0]))
            # wr_acqs.append(self._mem_acq[0] & (self._write_to_sram[0]))
            # rd_acq = self._mem_acq[1] & self._ren_full[0]
            # rd_acqs.append(self._mem_acq[1] & self._ren_full[0])
            # for i in range(self.num_ID - 1):
            #     # wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & self._wen_full[i + 1])
            #     # wr_acqs.append(self._mem_acq[2 * (i + 1)] & self._wen_full[i + 1])
            #     # wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1] | self._read_from_sram_write_side[i + 1]))
            #     wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1]))
            #     # wr_acqs.append(self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1] | self._read_from_sram_write_side[i + 1]))
            #     wr_acqs.append(self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1]))
            #     rd_acq = kts.concat(rd_acq, self._mem_acq[2 * (i + 1) + 1] & self._ren_full[i + 1])
            #     rd_acqs.append(self._mem_acq[2 * (i + 1) + 1] & self._ren_full[i + 1])
            # Do a read from the sram if the read side gets access or if the write side gets read access
            # self.wire(self._ren_to_mem, rd_acq.r_or() | kts.concat(*self._read_from_sram_write_side).r_or())
            self.wire(self._ren_to_mem, rd_acq_.r_or() | kts.concat(*self._read_from_sram_write_side).r_or())
            # This is a little weird, but nothing is going to receive this grant, so just based on the lock held
            self.wire(self._wen_to_mem, (wr_acq_ | kts.concat(*self._sram_lock)).r_or())

            # Choose which base block...
            wr_base = kts.ternary(wr_acqs[0], self._curr_base[0] + self._buffet_base[0], kts.const(0, self._curr_base[0].width))
            # rd_base = kts.ternary(rd_acqs[0], self._blk_base[0] + self._buffet_base[0], kts.const(0, self._blk_base[0].width))
            # rd_base = kts.ternary(rd_acqs[0] | self._use_cached_read[0], self._blk_base[0] + self._buffet_base[0], kts.const(0, self._blk_base[0].width))
            rd_base = kts.ternary(rd_acqs[0], self._blk_base[0] + self._buffet_base[0], kts.const(0, self._blk_base[0].width))
            for i in range(self.num_ID - 1):
                wr_base = kts.ternary(wr_acqs[i + 1], self._curr_base[i + 1] + self._buffet_base[i + 1], wr_base)
                # rd_base = kts.ternary(rd_acqs[i + 1], self._blk_base[i + 1] + self._buffet_base[i + 1], rd_base)
                # rd_base = kts.ternary(rd_acqs[i + 1] | self._use_cached_read[i + 1], self._blk_base[i + 1] + self._buffet_base[i + 1], rd_base)
                rd_base = kts.ternary(rd_acqs[i + 1], self._blk_base[i + 1] + self._buffet_base[i + 1], rd_base)
            tmp_wr_base = self.var("tmp_wr_base", self._buffet_base[0].width)
            tmp_rd_base = self.var("tmp_rd_base", self._buffet_base[0].width)
            self.wire(tmp_wr_base, wr_base)
            self.wire(tmp_rd_base, rd_base)
            # self.wire(self._data_to_mem, self._wr_data_fifo_out_data)
            # self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem, self._wr_addr_fifo_out_data + tmp_wr_base, self._rd_addr_fifo_out_addr + tmp_rd_base))
            # TODO: Generalize, for now hack
            self.wire(self._data_to_mem, kts.ternary(wr_acqs[0],
                                                     self._write_wide_word_comb_out[0],
                                                     kts.ternary(wr_acqs[1],
                                                                 self._write_wide_word_comb_out[1],
                                                                 kts.ternary(self._sram_lock[0],
                                                                             self._write_wide_word_modified[0],
                                                                             kts.ternary(self._sram_lock[1],
                                                                                         self._write_wide_word_modified[1],
                                                                                         kts.const(0, self._data_to_mem.width))))))
            # self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem, self._wr_addr_fifo_out_data + tmp_wr_base, self._rd_addr_fifo_out_addr + tmp_rd_base))
            # self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem | ~self._ren_full.r_or(),
            # self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem | self._mem_acq[0] | self._mem_acq[2],
            #                                         #  kts.ternary(wr_acqs[0],
            #                                          kts.ternary(self._mem_acq[0] | self._sram_lock[0],
            #                                                      self._write_word_addr[0],
            #                                                     #  self._write_word_addr[1]) + tmp_wr_base,
            #                                                      self._write_word_addr[1]),
            #                                         #  self._rd_addr_fifo_out_addr + tmp_rd_base))
            #                                          self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + tmp_rd_base))

            if self.num_read_ports == 1:
                p2 = 0
            else:
                p2 = 1

            self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem | self._mem_acq[0] | self._mem_acq[2],
                                                    #  kts.ternary(wr_acqs[0],
                                                     kts.ternary(self._mem_acq[0] | self._sram_lock[0],
                                                                 self._write_word_addr[0],
                                                                #  self._write_word_addr[1]) + tmp_wr_base,
                                                                 self._write_word_addr[1]),
                                                    #  self._rd_addr_fifo_out_addr + tmp_rd_base))
                                                     kts.ternary(rd_acqs[0],
                                                                #  (self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[0]) % (self._buffet_capacity[0] >> self.subword_addr_bits) + self._buffet_base[0],
                                                                #  (self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[1]) % (self._buffet_capacity[1] >> self.subword_addr_bits) + self._buffet_base[1])))
                                                                #  ((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[0]) >> self._buffet_capacity_log[0]) + self._buffet_base[0],
                                                                #  ((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[1]) >> self._buffet_capacity_log[1]) + self._buffet_base[1])))
                                                                 ((self._rd_addr_fifo_out_addr[0][self.mem_addr_bit_range_outer] + self._blk_base[0]) & self._buffet_capacity_mask[0]) + self._buffet_base[0],
                                                                 ((self._rd_addr_fifo_out_addr[p2][self.mem_addr_bit_range_outer] + self._blk_base[1]) & self._buffet_capacity_mask[1]) + self._buffet_base[1])))

        else:
            # Create FSM
            self.write_fsm = [self.add_fsm(f"write_fsm_{i}", reset_high=False) for i in range(self.num_ID)]
            WR_START = [self.write_fsm[i].add_state(f"WR_START_{i}") for i in range(self.num_ID)]
            WRITING = [self.write_fsm[i].add_state(f"WRITING_{i}") for i in range(self.num_ID)]

            ####################
            # Next State Logic WRITE
            ####################
            for ID_idx in range(self.num_ID):

                self.write_fsm[ID_idx].output(self._push_blk[ID_idx])
                self.write_fsm[ID_idx].output(self._en_curr_base[ID_idx])
                self.write_fsm[ID_idx].output(self._en_curr_bounds[ID_idx])
                self.write_fsm[ID_idx].output(self._wen_full[ID_idx])
                self.write_fsm[ID_idx].output(self._pop_in_full[ID_idx])
                # self.write_fsm.output(self._en_curr_bounds)

                ####################
                # WR_START #
                ####################
                # Start state gets an allocate command
                WR_START[ID_idx].next(WRITING[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)) & self._tile_en)
                WR_START[ID_idx].next(WR_START[ID_idx], None)

                ####################
                # WRITING #
                ####################
                # Writing until we get a finalize...
                WRITING[ID_idx].next(WR_START[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & ~self._blk_full[ID_idx] & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                WRITING[ID_idx].next(WRITING[ID_idx], None)

                ####################
                # Output Logic WRITE
                ####################

                ####################
                # WR_START #
                ####################

                WR_START[ID_idx].output(self._push_blk[ID_idx], 0)
                # WR_START[ID_idx].output(self._en_curr_base[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                WR_START[ID_idx].output(self._en_curr_base[ID_idx], 0)
                WR_START[ID_idx].output(self._en_curr_bounds[ID_idx], 0)
                WR_START[ID_idx].output(self._wen_full[ID_idx], 0)
                WR_START[ID_idx].output(self._pop_in_full[ID_idx], (self._wr_data_fifo_out_op == 0) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # WR_START.output(self._en_curr_bounds, 0)

                ####################
                # WRITING #
                ####################
                # Increment wr addr if we get wr access
                # WRITING[ID_idx].output(self._inc_wr_addr, self._mem_acq[0] & self._wr_data_fifo_valid & (self._wr_data_fifo_out_op == 1))
                # WRITING[ID_idx].output(self._inc_bounds_ctr, self._mem_acq[0] & self._wr_data_fifo_valid & (self._wr_data_fifo_out_op == 1))
                # WRITING[ID_idx].output(self._clr_bounds_ctr, 0)
                WRITING[ID_idx].output(self._push_blk[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & ~self._blk_full[ID_idx] & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # WRITING[ID_idx].output(self._en_curr_base[ID_idx], 0)
                # On our way back to write start we can set the current base for the next block. it is 0 at reset, so we don't need to deal with it there
                WRITING[ID_idx].output(self._en_curr_base[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & ~self._blk_full[ID_idx] & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # Any time we make a write to the current block we can update the bounds of the current block
                WRITING[ID_idx].output(self._en_curr_bounds[ID_idx], self._mem_acq[2 * ID_idx + 0] & self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # WRITING[ID_idx].output(self._wen_full[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & ~(self._curr_capacity < self._buffet_capacity))

                # Only make the wen when there is room and the proper ID is being addressed
                WRITING[ID_idx].output(self._wen_full[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                WRITING[ID_idx].output(self._pop_in_full[ID_idx], (self._mem_acq[2 * ID_idx + 0] &
                                                                  self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                                                                  (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) &
                                                                  (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width))) |
                                                                  (self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & ~self._blk_full[ID_idx] &
                                                                  (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width))))
                # WRITING.output(self._en_curr_bounds, 0)

            ##### Create read side fsm separately.

            #####################
            # READING #
            #####################

            self.read_fsm = [self.add_fsm(f"read_fsm_{i}", reset_high=False) for i in range(self.num_ID)]
            RD_START = [self.read_fsm[i].add_state(f"RD_START_{i}") for i in range(self.num_ID)]

            for ID_idx in range(self.num_ID):
                ####################
                # RD_START
                ####################
                # Get the first block size...
                RD_START[ID_idx].next(RD_START[ID_idx], None)

                self.read_fsm[ID_idx].output(self._pop_blk[ID_idx])
                # self.read_fsm[ID_idx].output(self._rd_addr_loc[ID_idx])
                self.read_fsm[ID_idx].output(self._ren_full[ID_idx])
                self.read_fsm[ID_idx].output(self._read_pop_full[ID_idx])
                self.read_fsm[ID_idx].output(self._size_request_full[ID_idx])

                ####################
                # RD_START
                ####################
                RD_START[ID_idx].output(self._pop_blk[ID_idx], (self._rd_op_fifo_out_op == 0) & self._read_joined & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                # Guarantee there's room for the read to land (need to use almost full, not full...)
                # RD_START[ID_idx].output(self._ren_full[ID_idx], (self._rd_op_fifo_out_op == 1) & self._read_joined & ~self._rd_rsp_fifo_full & self._blk_valid[ID_idx] & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                RD_START[ID_idx].output(self._ren_full[ID_idx], (self._rd_op_fifo_out_op == 1) & self._read_joined & ~self._rd_rsp_fifo_almost_full & self._blk_valid[ID_idx] & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                # Pop the op fifo if there is a read that's going through or if it's a free op
                # If it's a size request, only fulfill it if we aren't pushing a read from memory to the output fifo
                RD_START[ID_idx].output(self._read_pop_full[ID_idx], kts.ternary(self._rd_op_fifo_out_op == 2,
                                                                                 ~self._valid_from_mem & self._blk_valid[ID_idx],
                                                                                 kts.ternary(self._rd_op_fifo_out_op == 1,
                                                                                             self._mem_acq[2 * ID_idx + 1] & ~self._rd_rsp_fifo_full,
                                                                                             kts.const(1, 1))) &
                                                                                 self._read_joined &
                                                                                 (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                RD_START[ID_idx].output(self._size_request_full[ID_idx], self._blk_valid[ID_idx] & (self._rd_op_fifo_out_op == 2) & self._read_joined & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))

            for i in range(self.num_ID):
                self.write_fsm[i].set_start_state(WR_START[i])
                self.read_fsm[i].set_start_state(RD_START[i])

            for i in range(self.num_ID):
                ### Bookkeeping FIFO
                blk_fifo_in = kts.concat(self._curr_base[i], self._curr_bounds[i])
                blk_fifo = RegFIFO(data_width=blk_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=False)

                self.add_child(f"blk_fifo_{i}",
                            blk_fifo,
                            clk=self._gclk,
                            rst_n=self._rst_n,
                            clk_en=self._clk_en,
                            push=self._push_blk[i],
                            pop=self._pop_blk[i],
                            data_in=blk_fifo_in,
                            data_out=kts.concat(self._blk_base[i], self._blk_bounds[i]))

                self.wire(self._blk_full[i], blk_fifo.ports.full)
                self.wire(self._blk_valid[i], ~blk_fifo.ports.empty)

            # Force FSM realization first so that flush gets added...
            kts.passes.realize_fsm(self.internal_generator)

            # Arbitrate between the write/read side with RR arbiter
            self.port_arbiter = Arbiter(ins=2 * self.num_ID,
                                        algo="RR")

            base_rr = kts.concat(self._ren_full[0], self._wen_full[0])
            for i in range(self.num_ID - 1):
                base_rr = kts.concat(self._ren_full[i + 1], self._wen_full[i + 1], base_rr)

            brr = self.var("base_rr", 2 * self.num_ID)
            self.wire(brr, base_rr)

            self.add_child(f"rr_arbiter",
                        self.port_arbiter,
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        clk_en=self._clk_en,
                        request_in=brr,
                        grant_out=self._mem_acq,
                        resource_ready=self._ready_from_mem)

            self.wire(self._data_to_mem, self._wr_data_fifo_out_data)
            wr_acqs = []
            rd_acqs = []
            wr_acq = self._mem_acq[0] & self._wen_full[0]
            wr_acqs.append(self._mem_acq[0] & self._wen_full[0])
            rd_acq = self._mem_acq[1] & self._ren_full[0]
            rd_acqs.append(self._mem_acq[1] & self._ren_full[0])
            for i in range(self.num_ID - 1):
                wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & self._wen_full[i + 1])
                wr_acqs.append(self._mem_acq[2 * (i + 1)] & self._wen_full[i + 1])
                rd_acq = kts.concat(rd_acq, self._mem_acq[2 * (i + 1) + 1] & self._ren_full[i + 1])
                rd_acqs.append(self._mem_acq[2 * (i + 1) + 1] & self._ren_full[i + 1])
            self.wire(self._ren_to_mem, rd_acq.r_or())
            self.wire(self._wen_to_mem, wr_acq.r_or())

            # Choose which base block...
            wr_base = kts.ternary(wr_acqs[0], self._curr_base[0] + self._buffet_base[0], kts.const(0, self._curr_base[0].width))
            rd_base = kts.ternary(rd_acqs[0], self._blk_base[0] + self._buffet_base[0], kts.const(0, self._blk_base[0].width))
            for i in range(self.num_ID - 1):
                wr_base = kts.ternary(wr_acqs[i + 1], self._curr_base[i + 1] + self._buffet_base[i + 1], wr_base)
                rd_base = kts.ternary(rd_acqs[i + 1], self._blk_base[i + 1] + self._buffet_base[i + 1], rd_base)
            tmp_wr_base = self.var("tmp_wr_base", self._buffet_base[0].width)
            tmp_rd_base = self.var("tmp_rd_base", self._buffet_base[0].width)
            self.wire(tmp_wr_base, wr_base)
            self.wire(tmp_rd_base, rd_base)
            self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem, self._wr_addr_fifo_out_data + tmp_wr_base, self._rd_addr_fifo_out_addr + tmp_rd_base))

        if self.add_clk_enable:
            # self.clock_en("clk_en")
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))

        if self.add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

        # Finally, lift the config regs...
        lift_config_reg(self.internal_generator)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return self.base_ports

    def get_config_mode_str(self):
        return "buffet"

    # def get_bitstream(self, capacity_0=1024, capacity_1=1024):
    def get_bitstream(self, config_kwargs):

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        capacity_0 = config_kwargs['capacity_0']
        capacity_1 = config_kwargs['capacity_1']

        # Store all configurations here
        config = [
            # ("buffet_capacity_0", capacity_0),
            # ("buffet_capacity_1", capacity_1),
            ("buffet_capacity_log_0", capacity_0),
            ("buffet_capacity_log_1", capacity_1),
            ("tile_en", 1)]

        return trim_config_list(flattened, config)


if __name__ == "__main__":
    buffet_dut = BuffetLike(data_width=16,
                            num_ID=2,
                            physical_mem=True,
                            defer_fifos=False,
                            mem_width=64,
                            local_memory=False,
                            add_flush=True,
                            optimize_wide=True,
                            split_mem_requests=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(buffet_dut, filename="buffet_like.sv",
            optimize_if=False)
