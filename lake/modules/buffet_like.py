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
                 optimize_wide=False):

        super().__init__(f"buffet_like_{data_width}", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True
        self.num_ID = num_ID
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.local_memory = local_memory
        self.physical_mem = physical_mem
        self.fifo_depth = fifo_depth
        self.tech_map = tech_map
        self.defer_fifos = defer_fifos
        self.optimize_wide = optimize_wide

        self.total_sets = 0

        self.base_ports = [[None]]

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

        self._buffet_capacity = self.input("buffet_capacity", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        self._buffet_capacity.add_attribute(ConfigRegAttr("Capacity of buffet..."))

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
        self._rd_op_op = self.input("rd_op", self.data_width + 1, explicit_array=True, packed=True)
        self._rd_op_op.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_op_ready = self.output("rd_op_ready", 1)
        self._rd_op_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_op_valid = self.input("rd_op_valid", 1)
        self._rd_op_valid.add_attribute(ControlSignalAttr(is_control=True))

        self._rd_addr = self.input("rd_addr", self.data_width + 1, explicit_array=True, packed=True)
        self._rd_addr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._rd_addr_ready = self.output("rd_addr_ready", 1)
        self._rd_addr_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_addr_valid = self.input("rd_addr_valid", 1)
        self._rd_addr_valid.add_attribute(ControlSignalAttr(is_control=True))

        # Read ID
        self._rd_ID = self.input("rd_ID", self.data_width + 1, explicit_array=True, packed=True)
        self._rd_ID.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._rd_ID_ready = self.output("rd_ID_ready", 1)
        self._rd_ID_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_ID_valid = self.input("rd_ID_valid", 1)
        self._rd_ID_valid.add_attribute(ControlSignalAttr(is_control=True))

        # Read response channel
        self._rd_rsp_data = self.output("rd_rsp_data", self.data_width + 1, explicit_array=True, packed=True)
        self._rd_rsp_data.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._rd_rsp_ready = self.input("rd_rsp_data_ready", 1)
        self._rd_rsp_ready.add_attribute(ControlSignalAttr(is_control=True))

        self._rd_rsp_valid = self.output("rd_rsp_data_valid", 1)
        self._rd_rsp_valid.add_attribute(ControlSignalAttr(is_control=False))

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
        self._rd_op_fifo_pop = self.var("rd_op_fifo_pop", 1)
        self._rd_op_fifo_valid = self.var("rd_op_fifo_valid", 1)

        self._rd_op_fifo_in = kts.concat(self._rd_op_op[0][self.data_width - 1, 0])
        self._rd_op_infifo = RegFIFO(data_width=self._rd_op_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._rd_op_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._rd_op_fifo_out_op = self.var("rd_op_fifo_out_op", self.data_width, packed=True)

        self.add_child(f"rd_op_fifo",
                       self._rd_op_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_op_valid,
                       pop=self._rd_op_fifo_pop,
                       data_in=self._rd_op_fifo_in,
                       data_out=kts.concat(self._rd_op_fifo_out_op))

        self.wire(self._rd_op_ready, ~self._rd_op_infifo.ports.full)
        self.wire(self._rd_op_fifo_valid, ~self._rd_op_infifo.ports.empty)

        # RD ADDR fifo
        self._rd_addr_fifo_pop = self.var("rd_addr_fifo_pop", 1)
        self._rd_addr_fifo_valid = self.var("rd_addr_fifo_valid", 1)

        self._rd_addr_fifo_in = kts.concat(self._rd_addr[0][self.data_width - 1, 0])
        self._rd_addr_infifo = RegFIFO(data_width=self._rd_addr_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._rd_addr_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._rd_addr_fifo_out_addr = self.var("rd_addr_fifo_out_addr", self.data_width, packed=True)

        self.add_child(f"rd_addr_fifo",
                       self._rd_addr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_addr_valid,
                       pop=self._rd_addr_fifo_pop,
                       data_in=self._rd_addr_fifo_in,
                       data_out=kts.concat(self._rd_addr_fifo_out_addr))

        self.wire(self._rd_addr_ready, ~self._rd_addr_infifo.ports.full)
        self.wire(self._rd_addr_fifo_valid, ~self._rd_addr_infifo.ports.empty)

        # RD ID fifo
        self._rd_ID_fifo_pop = self.var("rd_ID_fifo_pop", 1)
        self._rd_ID_fifo_valid = self.var("rd_ID_fifo_valid", 1)

        self._rd_ID_fifo_in = kts.concat(self._rd_ID[0][self.data_width - 1, 0])
        self._rd_ID_infifo = RegFIFO(data_width=self._rd_ID_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._rd_ID_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._rd_ID_fifo_out_data = self.var("rd_ID_fifo_out_data", self.data_width, packed=True)

        self.add_child(f"rd_ID_fifo",
                       self._rd_ID_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_ID_valid,
                       pop=self._rd_ID_fifo_pop,
                       data_in=self._rd_ID_fifo_in,
                       data_out=kts.concat(self._rd_ID_fifo_out_data))

        self.wire(self._rd_ID_ready, ~self._rd_ID_infifo.ports.full)
        self.wire(self._rd_ID_fifo_valid, ~self._rd_ID_infifo.ports.empty)

# =============================
# Miscellaneous forward declarations - 2
# =============================

        # Read fifos joined
        self._read_joined = self.var("read_joined", 1)
        self.wire(self._read_joined, self._rd_ID_fifo_valid & self._rd_op_fifo_valid & self._rd_addr_fifo_valid)
        # Write fifos joined
        self._joined_in_fifo = self.var("joined_in_fifo", 1)

        self._en_curr_bounds = self.var("en_curr_bounds", self.num_ID)
        # self._curr_bounds = self.var("curr_bounds", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        self._curr_bounds = [register(self, self._wr_addr_fifo_out_data, enable=self._en_curr_bounds[i], name=f"curr_bounds_{i}", packed=True) for i in range(self.num_ID)]

        self._en_curr_base = self.var("en_curr_base", self.num_ID)
        # self._curr_base = [register(self, self._wr_addr, enable=self._en_curr_base[i], name=f"curr_base_{i}", packed=True) for i in range(self.num_ID)]
        self._curr_base = [register(self, self._curr_bounds[i] + 1, enable=self._en_curr_base[i], name=f"curr_base_{i}", packed=True) for i in range(self.num_ID)]

        self._read_pop_full = self.var("read_pop_full", self.num_ID)
        self._read_pop = self.var("read_pop", 1)

        if self.optimize_wide and self.mem_width > self.data_width:

            self.fw_int = self.mem_width // self.data_width
            self.wide_num_word_bits = kts.clog2(self.fw_int)
            self.mem_addr_bit_range_outer = (self.data_width - 1, self.wide_num_word_bits)
            self.mem_addr_bit_range_inner = (self.wide_num_word_bits - 1, 0)

            # Create local memory interface...
            # self._addr_to_mem = self.var("addr_to_mem", self.data_width, packed=True, explicit_array=True)
            self._addr_to_mem = self.var("addr_to_mem", self.data_width, packed=True)
            self._data_to_mem = self.var("data_to_mem", self.data_width, size=self.fw_int, packed=True, explicit_array=True)
            self._data_from_mem = self.var("data_from_mem", self.data_width, size=self.fw_int, packed=True, explicit_array=True)
            self._wen_to_mem = self.var("wen_to_mem", 1)
            self._ren_to_mem = self.var("ren_to_mem", 1)

            # Represents the final addr destination for this grouped word
            self._set_wide_word_addr = [self.var(f"set_wide_word_addr_{idx}", 1) for idx in range(self.num_ID)]
            # self._write_word_addr = [self.var(f"write_word_addr_{idx}", self.data_width) for idx in range(self.num_ID)]
            # self._write_word_addr = [register(self, self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer],
            # self._write_word_addr = [register(self, self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] + self._curr_base[idx] + self._buffet_base[idx],
            tmp_addr = [self.var(f"tmp_addr_{idx}", 16, packed=True) for idx in range(self.num_ID)]
            # tmp_addr = [(self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] + self._curr_base[idx] + self._buffet_base[idx]) for idx in range(self.num_ID)]
            [self.wire(tmp_addr[idx], (self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] + self._curr_base[idx] + self._buffet_base[idx])) for idx in range(self.num_ID)]
            self._write_word_addr = [register(self, tmp_addr[idx],
                                              enable=self._set_wide_word_addr[idx],
                                              name=f"write_word_addr_reg_{idx}", packed=True) for idx in range(self.num_ID)]
            self._write_word_addr_valid = [sticky_flag(self, self._set_wide_word_addr[idx],
                                                    #    clear=self._clr_cached_read[idx],
                                                       name=f"write_word_addr_valid_sticky_{idx}",
                                                       seq_only=True) for idx in range(self.num_ID)]

            self._write_to_sram = [self.var(f"write_to_sram_{idx}", 1) for idx in range(self.num_ID)]
            self._read_from_sram_write_side = [self.var(f"read_from_sram_write_side_{idx}", 1) for idx in range(self.num_ID)]
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
            self._write_wide_word_comb = [self.var(f"write_wide_word_comb_{idx}", self.data_width, size=self.fw_int, packed=True, explicit_array=True) for idx in range(self.num_ID)]
            # Modified version of the sram read for
            self._write_wide_word_modified = [self.var(f"write_wide_word_modified_{idx}", self.data_width, size=self.fw_int, packed=True, explicit_array=True) for idx in range(self.num_ID)]
            self._write_wide_word_reg = [register(self, self._write_wide_word_comb[idx],
                                                  enable=self._set_write_wide_word[idx] | self._clr_write_wide_word[idx],
                                                  name=f"write_wide_word_reg_{idx}", packed=True) for idx in range(self.num_ID)]
            # For the wide word, send through the data reg if masked, otherwise the fifo
            for idx_i in range(self.num_ID):
                for idx_j in range(self.fw_int):
                    self.wire(self._write_wide_word_comb[idx_i][idx_j], kts.ternary(self._write_wide_word_mask_reg_out[idx_i][idx_j],
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
                    #    self._write_wide_word_mask_reg_out[idx] | ((kts.ternary((self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] == self._write_word_addr[idx]) &
                       self._write_wide_word_mask_reg_out[idx] | ((kts.ternary(((self._curr_base[idx] + self._buffet_base[idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) == self._write_word_addr[idx]) &
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
                                #   ((kts.ternary(((self._clr_write_wide_word[idx] & (self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] != self._write_word_addr[idx])) |
                                  ((kts.ternary(((self._clr_write_wide_word[idx] & ((self._curr_base[idx] + self._buffet_base[idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) != self._write_word_addr[idx])) |
                                                    # ((self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] == self._write_word_addr[idx]) & ~self._write_full_word[idx])) &
                                                    (((self._curr_base[idx] + self._buffet_base[idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) == self._write_word_addr[idx]) & ~self._write_full_word[idx])) &
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
            self._last_read_addr = register(self, self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_inner], enable=self._ren_to_mem, name="last_read_addr", packed=True)
            self._last_read_addr_wide = register(self, self._addr_to_mem, enable=self._ren_to_mem, name="last_read_addr_wide", packed=True)
            self._last_read_ID = register(self, self._rd_ID_fifo_out_data, enable=self._ren_to_mem, name="last_read_ID")

            self._set_read_word_addr = [self.var(f"set_read_word_addr_{idx}", 1) for idx in range(self.num_ID)]
            # self._read_word_addr = [self.var(f"read_word_addr_{idx}", self.data_width) for idx in range(self.num_ID)]
            # self._read_word_addr = [register(self, self._last_read_addr,
            self._read_word_addr = [register(self, self._addr_to_mem,
                                             enable=self._set_read_word_addr[idx], name=f"cached_read_word_addr_{idx}") for idx in range(self.num_ID)]

            self._valid_from_mem = self.var("valid_from_mem", 1)
            # Ren full delay...
            self._ren_full_d1 = [register(self, self._ren_full[idx], enable=kts.const(1, 1), name=f"ren_full_delayed_{idx}") for idx in range(self.num_ID)]

            # Determine whether to use the output register of the SRAM or the cached word
            self._use_cached_read = [self.var(f"use_cached_read_{idx}", 1) for idx in range(self.num_ID)]
            # First, check that the word is valid (which is set when the proper read happens)
            # Then, if the last read was to this word, we check that the id and addr match (they should)
            # Otherwise, we check the op, addr, ID and joined read valids
            # Furthermore, if we are in a cached state, we drop the use_cached_read is valid_from_mem is high
            # - this is to avoid a situation where we are serving a prior read and get a cached read to serve to the output. Forces the sequence
            [self.wire(self._use_cached_read[idx], self._read_wide_word_valid[idx] &
                                                #    (self._read_word_addr[idx][self.mem_addr_bit_range_outer] == self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer]) &
                                                #    (self._read_word_addr[idx] == self._addr_to_mem) &
                                                #    self._read_joined &
                                                #    kts.ternary((self._valid_from_mem & self._ren_full_d1[idx]),
                                                   kts.ternary((self._valid_from_mem & self._ren_full_d1[idx]),
                                                               (self._last_read_ID == kts.const(idx, 1)) &
                                                                    (self._last_read_addr_wide == self._read_word_addr[idx]),
                                                               (self._rd_ID_fifo_out_data == kts.const(idx, 1)) &
                                                            #    ((self._rd_addr_fifo_out_addr + self._blk_base[idx] + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                                                               ((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + self._blk_base[idx] + self._buffet_base[idx]) == self._read_word_addr[idx]) &
                                                                    # (self._addr_to_mem == self._read_word_addr[idx]) &
                                                                    (self._rd_op_fifo_out_op == kts.const(1, 2)) &
                                                                    ~self._valid_from_mem &
                                                                    self._read_joined)) for idx in range(self.num_ID)]
            #  (self._rd_ID_fifo_out_data == kts.const(idx, 1)) &
            #    (self._last_read_ID == kts.const(idx, 1)) &
            #    (self._rd_op_fifo_out_op == kts.const(1, 2))) for idx in range(self.num_ID)]

            self._chosen_read = [self.var(f"chosen_read_{idx}", self.data_width) for idx in range(self.num_ID)]
            [self.wire(self._chosen_read[idx], kts.ternary(self._use_cached_read[idx] & self._read_wide_word_valid[idx] & ~self._valid_from_mem,
                                                           self._read_wide_word[idx][self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_inner]],
                                                        #    self._data_from_mem[self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_inner]])) for idx in range(self.num_ID)]
                                                           self._data_from_mem[self._last_read_addr[self.mem_addr_bit_range_inner]])) for idx in range(self.num_ID)]

            self._sram_lock = [self.var(f"sram_lock_{idx}", 1) for idx in range(self.num_ID)]
            self._any_sram_lock = self.var("any_sram_lock", 1)
            self.wire(self._any_sram_lock, (kts.concat(*self._sram_lock).r_or()))

            # Need interface to memory...

            # self._read_d1 = register(self, self._ren_to_mem, enable=kts.const(1, 1), name="read_d1")
            # Read d1 is if we get a read
            self._read_d1 = register(self, kts.concat(self._mem_acq[1], self._mem_acq[3]).r_or(), enable=kts.const(1, 1), name="read_d1")
            self._read_ID_d1 = register(self, self._rd_ID_fifo_out_data, enable=self._ren_to_mem, name="read_ID_d1")
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

            self.addr_addressing_bits = (kts.clog2(self.mem_depth) - 1, 0)

            self.base_ports = [[None]]
            rw_port = MemoryPort(MemoryPortType.READWRITE)
            rw_port_intf = rw_port.get_port_interface()
            rw_port_intf['data_in'] = self._data_to_mem
            rw_port_intf['data_out'] = self._data_from_mem
            rw_port_intf['write_addr'] = self._addr_to_mem[self.addr_addressing_bits]
            rw_port_intf['write_enable'] = self._wen_to_mem
            rw_port_intf['read_addr'] = self._addr_to_mem[self.addr_addressing_bits]
            rw_port_intf['read_enable'] = self._ren_to_mem
            rw_port.annotate_port_signals()
            self.base_ports[0][0] = rw_port
            # print(actual_mem_port_interface)

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

        # Size requests al
        self._size_request_full = self.var("size_request_full", self.num_ID)

        self._rd_rsp_fifo_push = self.var("rd_rsp_fifo_push", 1)
        self._rd_rsp_fifo_full = self.var("rd_rsp_fifo_full", 1)
        self._rd_rsp_fifo_almost_full = self.var("rd_rsp_fifo_almost_full", 1)

        self._rd_rsp_fifo_in_data = self.var("rd_rsp_fifo_in_data", self.data_width + 1, packed=True)
        self._rd_rsp_out_fifo = RegFIFO(data_width=self._rd_rsp_fifo_in_data.width, width_mult=1,
                                        depth=self.fifo_depth, min_depth=2, defer_hrdwr_gen=self.defer_fifos,
                                        almost_full_diff=1)
        self._rd_rsp_out_fifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self.add_child(f"rd_rsp_fifo",
                       self._rd_rsp_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_rsp_fifo_push,
                       pop=self._rd_rsp_ready,
                       data_in=self._rd_rsp_fifo_in_data,
                       #    data_out=self._rd_rsp_data[0][self.data_width - 1, 0])
                       data_out=self._rd_rsp_data[0])

        # Wire the last bit to 0
        # self.wire(self._rd_rsp_data[0][self.data_width + 1 - 1], kts.const(0, 1))

        self.wire(self._rd_rsp_fifo_full, self._rd_rsp_out_fifo.ports.full)
        self.wire(self._rd_rsp_fifo_almost_full, self._rd_rsp_out_fifo.ports.almost_full)
        self.wire(self._rd_rsp_valid, ~self._rd_rsp_out_fifo.ports.empty)

        chosen_size_block = decode(self, self._size_request_full, self._blk_bounds)

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
            self.wire(self._rd_rsp_fifo_in_data[self.data_width - 1, 0], kts.ternary(self._use_cached_read[0] & self._read_wide_word_valid[0],
                                                                                     self._chosen_read[0],
                                                                                     kts.ternary(self._use_cached_read[1] & self._read_wide_word_valid[1],
                                                                                                 self._chosen_read[1],
                                                                                                 chosen_size_block + 1)))

            # self.wire(self._rd_rsp_fifo_in_data[self.data_width], self._read_rsp_ID_reg)
            self.wire(self._rd_rsp_fifo_in_data[self.data_width], kts.ternary(self._use_cached_read[0],
                                                                              kts.const(0, 1),
                                                                              kts.const(1, 1)))

            # self.wire(self._rd_rsp_fifo_push, self._valid_from_mem | (kts.concat(*self._use_cached_read).r_or()) | self._size_request_full.r_or())
            self.wire(self._rd_rsp_fifo_push, self._valid_from_mem | (kts.concat(*self._use_cached_read).r_or()) | self._size_request_full.r_or())
        else:
            self.wire(self._rd_rsp_fifo_push, self._valid_from_mem | self._size_request_full.r_or())
            self.wire(self._rd_rsp_fifo_in_data[self.data_width - 1, 0], kts.ternary(self._valid_from_mem, self._data_from_mem, chosen_size_block + 1))

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

        self.wire(kts.concat(self._rd_ID_fifo_pop, self._rd_op_fifo_pop, self._rd_addr_fifo_pop), kts.concat(*[self._read_pop for i in range(3)]))

        self.wire(self._read_pop, self._read_pop_full.r_or())

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

        self._curr_capacity_pre = self.var("curr_capacity_pre", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        # self._curr_capacity = self.var("curr_capacity", self.data_width, size=self.num_ID, explicit_array=True, packed=True)

        @always_ff((posedge, self._clk), (negedge, self._rst_n))
        def cap_reg(self, idx):
            if ~self._rst_n:
                self._curr_capacity_pre[idx] = kts.const(0, self.data_width)
            # Only update when pushed or popped
            elif self._push_blk[idx] or self._pop_blk[idx]:
                self._curr_capacity_pre[idx] = self._curr_capacity_pre[idx] + kts.ternary(self._push_blk[idx], self._blk_bounds[idx], kts.const(0, width=self.data_width)) - kts.ternary(self._pop_blk[idx], self._blk_bounds[idx], kts.const(0, width=self.data_width))

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
                WRITING[ID_idx].next(WR_START[ID_idx], self._joined_in_fifo &
                                                       (self._wr_data_fifo_out_op == 0) &
                                                       self._mem_acq[2 * ID_idx + 0] &
                                                       ~self._blk_full[ID_idx] &
                                                       (self._write_full_word[ID_idx] | (self._num_bits_valid_mask[ID_idx] == 0)) &
                                                       (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # Go to modify if we get a change in address on a non-full word (and we get lock access) or a finalize command
                # Furthermore, make sure there is at least some data in there.
                # Either finalize or write to a new address, with nonzero num bits and not full word
                WRITING[ID_idx].next(MODIFY[ID_idx], self._joined_in_fifo &
                                                     (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)) &
                                                     self._mem_acq[2 * ID_idx + 0] &
                                                     ((self._wr_data_fifo_out_op == kts.const(0, 1)) |
                                                        # ((self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] != self._write_word_addr[ID_idx]) &
                                                        # (((self._curr_base[ID_idx] + self._buffet_base[ID_idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) != self._write_word_addr[ID_idx]) &
                                                        ((((self._curr_base[ID_idx] + self._buffet_base[ID_idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) != self._write_word_addr[ID_idx]) & self._write_word_addr_valid[ID_idx]) &
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
                                                                  (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # Any time we make a write to the current block we can update the bounds of the current block
                WRITING[ID_idx].output(self._en_curr_bounds[ID_idx], self._mem_acq[2 * ID_idx + 0] & self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # WRITING[ID_idx].output(self._wen_full[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & ~(self._curr_capacity < self._buffet_capacity))

                # Only make the wen when there is room and the proper ID is being addressed
                WRITING[ID_idx].output(self._wen_full[ID_idx], self._joined_in_fifo &
                                                               (self._wr_data_fifo_out_op == 1) &
                                                               (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) &
                                                               (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                WRITING[ID_idx].output(self._pop_in_full[ID_idx], (self._mem_acq[2 * ID_idx + 0] &
                                                                  self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                                                                  (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) &
                                                                  (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width))) |
                                                                  (self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & ~self._blk_full[ID_idx] &
                                                                  (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width))))
                # WRITING[ID_idx].output(self._set_write_wide_word[ID_idx], (self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] == self._write_word_addr[ID_idx]) &
                WRITING[ID_idx].output(self._set_write_wide_word[ID_idx], (((self._curr_base[ID_idx] + self._buffet_base[ID_idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) == self._write_word_addr[ID_idx]) & self._write_word_addr_valid[ID_idx]) &
                                                                          self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                                                                          (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # Only clear the word
                WRITING[ID_idx].output(self._clr_write_wide_word[ID_idx], ((((self._curr_base[ID_idx] + self._buffet_base[ID_idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) != self._write_word_addr[ID_idx]) | ~self._write_word_addr_valid[ID_idx]) |
                                                                                ((((self._curr_base[ID_idx] + self._buffet_base[ID_idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) == self._write_word_addr[ID_idx]) & self._write_word_addr_valid[ID_idx]) &
                                                                                    self._write_full_word[ID_idx])) &
                                                                          self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                                                                          self._mem_acq[2 * ID_idx + 0] &
                                                                          (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # WRITING[ID_idx].output(self._write_to_sram[ID_idx], ((((self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] != self._write_word_addr[ID_idx]) | self._write_full_word[ID_idx]) &
                # WRITING[ID_idx].output(self._write_to_sram[ID_idx], (((self._write_full_word[ID_idx]) &
                # In the WRITING state, the only way to actually write the word to SRAM is if it is full
                WRITING[ID_idx].output(self._write_to_sram[ID_idx], self._write_full_word[ID_idx] &
                                                                          self._joined_in_fifo &
                                                                        #   (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) &
                                                                        #   self._mem_acq[2 * ID_idx + 0] &
                                                                          (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                # WRITING[ID_idx].output(self._write_to_wide_word[ID_idx], (self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] == self._write_word_addr[ID_idx]) &
                #                                                           self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                #                                                           (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                WRITING[ID_idx].output(self._set_wide_word_addr[ID_idx], (((self._curr_base[ID_idx] + self._buffet_base[ID_idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) != self._write_word_addr[ID_idx]) | ~self._write_word_addr_valid[ID_idx]) &
                                                                          self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) &
                                                                          (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
                WRITING[ID_idx].output(self._sram_lock[ID_idx], 0)
                # We are reading from the SRAM if we are transitioning to MODIFY
                WRITING[ID_idx].output(self._read_from_sram_write_side[ID_idx], self._joined_in_fifo &
                                                                                (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)) &
                                                                                # self._mem_acq[2 * ID_idx + 0] &
                                                                                ~self._any_sram_lock &
                                                                                ((self._wr_data_fifo_out_op == kts.const(0, 1)) |
                                                                                    # ((self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer] != self._write_word_addr[ID_idx]) &
                                                                                    ((((self._curr_base[ID_idx] + self._buffet_base[ID_idx] + self._wr_addr_fifo_out_data[self.mem_addr_bit_range_outer]) != self._write_word_addr[ID_idx]) &
                                                                                        self._write_word_addr_valid[ID_idx]) &
                                                                                    (self._wr_data_fifo_out_op == kts.const(1, 1)))) &
                                                                                (self._num_bits_valid_mask[ID_idx] > 0) &
                                                                                ~self._write_full_word[ID_idx])
                # WRITING.output(self._en_curr_bounds, 0)

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
                MODIFY[ID_idx].output(self._pop_in_full[ID_idx], 0)
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
                self.read_fsm[ID_idx].output(self._set_cached_read[ID_idx])
                self.read_fsm[ID_idx].output(self._clr_cached_read[ID_idx])
                self.read_fsm[ID_idx].output(self._set_read_word_addr[ID_idx])

                ####################
                # RD_START
                ####################
                RD_START[ID_idx].output(self._pop_blk[ID_idx], (self._rd_op_fifo_out_op == 0) & self._read_joined & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                # Guarantee there's room for the read to land (need to use almost full, not full...)
                # RD_START[ID_idx].output(self._ren_full[ID_idx], (self._rd_op_fifo_out_op == 1) & self._read_joined & ~self._rd_rsp_fifo_full & self._blk_valid[ID_idx] & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                RD_START[ID_idx].output(self._ren_full[ID_idx], (self._rd_op_fifo_out_op == 1) &
                                                                ~self._use_cached_read[ID_idx] &
                                                                self._read_joined & ~self._rd_rsp_fifo_almost_full &
                                                                self._blk_valid[ID_idx] &
                                                                ((self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] != self._read_word_addr[ID_idx]) | ~self._read_wide_word_valid[ID_idx]) &
                                                                (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                # Pop the op fifo if there is a read that's going through or if it's a free op
                # If it's a size request, only fulfill it if we aren't pushing a read from memory to the output fifo
                RD_START[ID_idx].output(self._read_pop_full[ID_idx], kts.ternary(self._rd_op_fifo_out_op == 2,
                                                                                 ~self._valid_from_mem & self._blk_valid[ID_idx],
                                                                                 kts.ternary(self._rd_op_fifo_out_op == 1,
                                                                                            #  (self._mem_acq[2 * ID_idx + 1] | self._use_cached_read[ID_idx]) & ~self._rd_rsp_fifo_full,
                                                                                             (self._mem_acq[2 * ID_idx + 1] | (self._use_cached_read[ID_idx] & ~self._valid_from_mem)) & ~self._rd_rsp_fifo_full,
                                                                                             kts.const(1, 1))) &
                                                                                 self._read_joined &
                                                                                 (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                RD_START[ID_idx].output(self._size_request_full[ID_idx], self._blk_valid[ID_idx] &
                                                                         (self._rd_op_fifo_out_op == 2) &
                                                                         self._read_joined &
                                                                         (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
                # The caching is a cycle behind because it is not activated until the read returns
                RD_START[ID_idx].output(self._set_cached_read[ID_idx], self._valid_from_mem &
                                                                       (self._read_ID_d1 == kts.const(ID_idx, 1)))
                RD_START[ID_idx].output(self._clr_cached_read[ID_idx], 0)
                # Set the read word's addr if we have a valid back to the same ID and the address is different
                # RD_START[ID_idx].output(self._set_read_word_addr[ID_idx], self._valid_from_mem &
                #                                                           (self._last_read_addr[self.mem_addr_bit_range_outer] != self._read_word_addr[ID_idx][self.mem_addr_bit_range_outer]) &
                #                                                         #   (self._read_ID_d1[ID_idx] == kts.const(ID_idx, 1)))
                #                                                           (self._read_ID_d1 == kts.const(ID_idx, 1)))
                RD_START[ID_idx].output(self._set_read_word_addr[ID_idx], self._ren_full[ID_idx] &
                                                                        #   (self._addr_to_mem[self.mem_addr_bit_range_outer] != self._read_word_addr[ID_idx][self.mem_addr_bit_range_outer]) &
                                                                          (self._addr_to_mem != self._read_word_addr[ID_idx]) &
                                                                        #   (self._read_ID_d1[ID_idx] == kts.const(ID_idx, 1)))
                                                                          (self._rd_ID_fifo_out_data == kts.const(ID_idx, 1)))

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

            # base_rr = kts.concat(self._ren_full[0], self._wen_full[0])
            base_rr = kts.concat(self._ren_full[0], self._write_to_sram[0] | self._read_from_sram_write_side[0] | self._wen_full[0])
            for i in range(self.num_ID - 1):
                # base_rr = kts.concat(self._ren_full[i + 1], self._wen_full[i + 1], base_rr)
                base_rr = kts.concat(self._ren_full[i + 1], self._write_to_sram[i + 1] | self._read_from_sram_write_side[i + 1] | self._wen_full[i + 1], base_rr)

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

            wr_acqs = []
            rd_acqs = []
            # wr_acq = self._mem_acq[0] & self._wen_full[0]
            # wr_acqs.append(self._mem_acq[0] & self._wen_full[0])
            # wr_acq = self._mem_acq[0] & (self._write_to_sram[0] | self._read_from_sram_write_side[0])
            wr_acq = self._mem_acq[0] & (self._write_to_sram[0])
            # wr_acqs.append(self._mem_acq[0] & (self._write_to_sram[0] | self._read_from_sram_write_side[0]))
            wr_acqs.append(self._mem_acq[0] & (self._write_to_sram[0]))
            rd_acq = self._mem_acq[1] & self._ren_full[0]
            rd_acqs.append(self._mem_acq[1] & self._ren_full[0])
            for i in range(self.num_ID - 1):
                # wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & self._wen_full[i + 1])
                # wr_acqs.append(self._mem_acq[2 * (i + 1)] & self._wen_full[i + 1])
                # wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1] | self._read_from_sram_write_side[i + 1]))
                wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1]))
                # wr_acqs.append(self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1] | self._read_from_sram_write_side[i + 1]))
                wr_acqs.append(self._mem_acq[2 * (i + 1)] & (self._write_to_sram[i + 1]))
                rd_acq = kts.concat(rd_acq, self._mem_acq[2 * (i + 1) + 1] & self._ren_full[i + 1])
                rd_acqs.append(self._mem_acq[2 * (i + 1) + 1] & self._ren_full[i + 1])
            # Do a read from the sram if the read side gets access or if the write side gets read access
            self.wire(self._ren_to_mem, rd_acq.r_or() | kts.concat(*self._read_from_sram_write_side).r_or())
            # This is a little weird, but nothing is going to receive this grant, so just based on the lock held
            self.wire(self._wen_to_mem, (wr_acq | kts.concat(*self._sram_lock)).r_or())

            # Choose which base block...
            wr_base = kts.ternary(wr_acqs[0], self._curr_base[0] + self._buffet_base[0], kts.const(0, self._curr_base[0].width))
            # rd_base = kts.ternary(rd_acqs[0], self._blk_base[0] + self._buffet_base[0], kts.const(0, self._blk_base[0].width))
            rd_base = kts.ternary(rd_acqs[0] | self._use_cached_read[0], self._blk_base[0] + self._buffet_base[0], kts.const(0, self._blk_base[0].width))
            for i in range(self.num_ID - 1):
                wr_base = kts.ternary(wr_acqs[i + 1], self._curr_base[i + 1] + self._buffet_base[i + 1], wr_base)
                # rd_base = kts.ternary(rd_acqs[i + 1], self._blk_base[i + 1] + self._buffet_base[i + 1], rd_base)
                rd_base = kts.ternary(rd_acqs[i + 1] | self._use_cached_read[i + 1], self._blk_base[i + 1] + self._buffet_base[i + 1], rd_base)
            tmp_wr_base = self.var("tmp_wr_base", self._buffet_base[0].width)
            tmp_rd_base = self.var("tmp_rd_base", self._buffet_base[0].width)
            self.wire(tmp_wr_base, wr_base)
            self.wire(tmp_rd_base, rd_base)
            # self.wire(self._data_to_mem, self._wr_data_fifo_out_data)
            # self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem, self._wr_addr_fifo_out_data + tmp_wr_base, self._rd_addr_fifo_out_addr + tmp_rd_base))
            # TODO: Generalize, for now hack
            self.wire(self._data_to_mem, kts.ternary(wr_acqs[0],
                                                     self._write_wide_word_comb[0],
                                                     kts.ternary(wr_acqs[1],
                                                                 self._write_wide_word_comb[1],
                                                                 kts.ternary(self._sram_lock[0],
                                                                             self._write_wide_word_modified[0],
                                                                             kts.ternary(self._sram_lock[1],
                                                                                         self._write_wide_word_modified[1],
                                                                                         kts.const(0, self._data_to_mem.width))))))
            # self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem, self._wr_addr_fifo_out_data + tmp_wr_base, self._rd_addr_fifo_out_addr + tmp_rd_base))
            # self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem | ~self._ren_full.r_or(),
            self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem | self._mem_acq[0] | self._mem_acq[2],
                                                    #  kts.ternary(wr_acqs[0],
                                                     kts.ternary(self._mem_acq[0] | self._sram_lock[0],
                                                                 self._write_word_addr[0],
                                                                #  self._write_word_addr[1]) + tmp_wr_base,
                                                                 self._write_word_addr[1]),
                                                    #  self._rd_addr_fifo_out_addr + tmp_rd_base))
                                                     self._rd_addr_fifo_out_addr[self.mem_addr_bit_range_outer] + tmp_rd_base))

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

    def get_bitstream(self, capacity_0=1024, capacity_1=1024):

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Store all configurations here
        config = [
            ("buffet_capacity_0", capacity_0),
            ("buffet_capacity_1", capacity_1),
            ("tile_en", 1)]

        return trim_config_list(flattened, config)


if __name__ == "__main__":
    buffet_dut = BuffetLike(data_width=16,
                            num_ID=2,
                            physical_mem=True,
                            defer_fifos=False,
                            mem_width=64,
                            optimize_wide=True)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(buffet_dut, filename="buffet_like.sv",
            optimize_if=False)
