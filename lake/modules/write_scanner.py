from bz2 import compress
import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import add_counter, safe_wire, register, sticky_flag, transform_strides_and_ranges, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class WriteScanner(Generator):
    def __init__(self,
                 data_width=16):

        super().__init__("write_scanner", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True

        self.total_sets = 0

        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        # Compressed data structure output
        self._compressed = self.input("compressed", 1)
        self._compressed.add_attribute(ConfigRegAttr("Only matters for a lowest-level write scanner - use address of go linearly"))

        self._lowest_level = self.input("lowest_level", 1)
        self._lowest_level.add_attribute(ConfigRegAttr("Only matters for a lowest-level write scanner - use address of go linearly"))

        # Set the stop token level to act as barrier in compressed data structure formation - (default 0 + 1 for root)
        self._stop_lvl = self.input("stop_lvl", 16)
        self._stop_lvl.add_attribute(ConfigRegAttr("What level stop tokens are used as dedup barrier"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # Scanner interface will need
        # input data, input valid
        # output address, output valid
        # There is a data in (for lowest level values, other level coordinates) and for address on lowest level when doing dense
        self._data_in = self.input("data_in", self.data_width, size=2, explicit_array=True, packed=True)
        self._data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_in = self.input("valid_in", 2)
        self._valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._eos_in = self.input("eos_in", 2)
        self._eos_in.add_attribute(ControlSignalAttr(is_control=True))

        # Ready in from the memory
        self._ready_in = self.input("ready_in", 1)
        self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ready_out = self.output("ready_out", 2)
        self._ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # Outputs
        # Data to write to memory (whether its val/coord/metadata)
        self._data_out = self.output("data_out", self.data_width, packed=True)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._op_out = self.output("op_out", 1)
        self._op_out.add_attribute(ControlSignalAttr(is_control=False))

        self._data_out_ready_in = self.input("data_out_ready_in", 1)
        self._data_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._data_out_valid_out = self.output("data_out_valid_out", 1)
        self._data_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._addr_out = self.output("addr_out", self.data_width, packed=True)
        self._addr_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._addr_out_ready_in = self.input("addr_out_ready_in", 1)
        self._addr_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._addr_out_valid_out = self.output("addr_out_valid_out", 1)
        self._addr_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._ID_out = self.output("ID_out", self.data_width, packed=True)
        self._ID_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._ID_out_ready_in = self.input("ID_out_ready_in", 1)
        self._ID_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ID_out_valid_out = self.output("ID_out_valid_out", 1)
        self._ID_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._outer_coord_out = self.output("outer_coord_out", self.data_width)
        # self._outer_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._pos_out = self.output("pos_out", self.data_width)
        # self._pos_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # Valid out for both streams
        # self._valid_out = self.output("valid_out", 2)
        # self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # Address to write to memory

        # Eos for both streams...
        # self._eos_out = self.output("eos_out", 2)
        # self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Memory write enable signal

        # Point to the row in storage for data recovery
        # self._payload_ptr = self.output("payload_ptr", 16)
        # self._payload_ptr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._inner_dim_offset = self.input("inner_dim_offset", 16)
        self._inner_dim_offset.add_attribute(ConfigRegAttr("Memory address of the inner level..."))

        self._block_mode = self.input("block_mode", 1)
        self._block_mode.add_attribute(ConfigRegAttr("Block Writes or Not"))

# =============================
# Input FIFO
#
# To leverage hierarchical intersection, we need to be
# able to accept an incoming stream from the output of a previous
# level. Root-level scanners drive the processing, whereas non-root-level
# scanners are driven by upper levels
# =============================

        self._data_infifo = RegFIFO(data_width=1 * self.data_width + 1, width_mult=1, depth=8)
        self._addr_infifo = RegFIFO(data_width=1 * self.data_width + 1, width_mult=1, depth=8)
        self._infifo_pop = self.var("infifo_pop", 2)

        # For input streams, need coord_in, valid_in, eos_in
        self._data_infifo_data_in = self.var("data_infifo_data_in", self.data_width)
        # self._infifo_coord_in = self.var("infifo_coord_in", self.data_width)
        self._data_infifo_valid_in = self.var("data_infifo_valid_in", 1)
        self._data_infifo_eos_in = self.var("data_infifo_eos_in", 1)

        # Stupid convert
        self._data_infifo_in_packed = self.var("data_infifo_in_packed", 1 * self.data_width + 1, packed=True)
        self.wire(self._data_infifo_in_packed[self.data_width], self._eos_in[0])
        self.wire(self._data_infifo_in_packed[self.data_width - 1, 0], self._data_in[0])
        self._data_infifo_out_packed = self.var("data_infifo_out_packed", 1 * self.data_width + 1, packed=True)
        self.wire(self._data_infifo_eos_in, self._data_infifo_out_packed[self.data_width])
        self.wire(self._data_infifo_data_in, self._data_infifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"data_input_fifo",
                       self._data_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._valid_in[0],
                       pop=self._infifo_pop[0],
                       data_in=self._data_infifo_in_packed,
                       data_out=self._data_infifo_out_packed)

        self.wire(self._ready_out[0], ~self._data_infifo.ports.full)
        self.wire(self._data_infifo_valid_in, ~self._data_infifo.ports.empty)

        # For input streams, need coord_in, valid_in, eos_in
        self._addr_infifo_data_in = self.var("addr_infifo_data_in", self.data_width)
        # self._infifo_coord_in = self.var("infifo_coord_in", self.data_width)
        self._addr_infifo_valid_in = self.var("addr_infifo_valid_in", 1)
        self._addr_infifo_eos_in = self.var("addr_infifo_eos_in", 1)

        self._addr_infifo_in_packed = self.var("addr_infifo_in_packed", 1 * self.data_width + 1, packed=True)
        self.wire(self._addr_infifo_in_packed[self.data_width], self._eos_in[1])
        self.wire(self._addr_infifo_in_packed[self.data_width - 1, 0], self._data_in[1])
        self._addr_infifo_out_packed = self.var("addr_infifo_out_packed", 1 * self.data_width + 1, packed=True)
        self.wire(self._addr_infifo_eos_in, self._addr_infifo_out_packed[self.data_width])
        self.wire(self._addr_infifo_data_in, self._addr_infifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"addr_input_fifo",
                       self._addr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._valid_in[1],
                       pop=self._infifo_pop[1],
                       data_in=self._addr_infifo_in_packed,
                       data_out=self._addr_infifo_out_packed)

        self.wire(self._ready_out[1], ~self._addr_infifo.ports.full)
        self.wire(self._addr_infifo_valid_in, ~self._addr_infifo.ports.empty)

        # State for block writes
        self._set_block_size = self.var("set_block_size", 1)
        self._block_size = register(self, self._data_infifo_data_in, self._set_block_size, name="block_size")

        self._inc_block_write = self.var("inc_block_write", 1)
        self._clr_block_write = self.var("clr_block_write", 1)
        self._block_writes = add_counter(self, "block_write_count", 16, increment=self._inc_block_write, clear=self._clr_block_write)

# =============================
# Output FIFO
# =============================

        self._data_to_fifo = self.var("data_to_fifo", self.data_width, packed=True)
        self._op_to_fifo = self.var("op_to_fifo", 1)
        self._addr_to_fifo = self.var("addr_to_fifo", self.data_width, packed=True)
        self._ID_to_fifo = self.var("ID_to_fifo", self.data_width, packed=True)

        self._push_to_outs = self.var("push_to_outs", 1)

        all_outputs_ready = []
        out_pushes = []

        ########################
        # DATA OUT + OP OUT
        ########################
        self._data_out_fifo_push = self.var("data_out_fifo_push", 1)
        self._data_out_fifo_full = self.var("data_out_fifo_full", 1)

        out_pushes.append(self._data_out_fifo_push)

        # Output FIFOs to the buffet-like
        data_out_fifo_in = kts.concat(self._data_to_fifo, self._op_to_fifo)
        data_out_outfifo = RegFIFO(data_width=data_out_fifo_in.width, width_mult=1, depth=8)

        self.add_child(f"data_out_fifo",
                       data_out_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._data_out_fifo_push,
                       pop=self._data_out_ready_in,
                       data_in=data_out_fifo_in,
                       data_out=kts.concat(self._data_out, self._op_out))
        # RegFIFO(data_width=1 * self.data_width + 1, width_mult=1, depth=8)
        self.wire(self._data_out_fifo_full, data_out_outfifo.ports.full)
        self.wire(self._data_out_valid_out, ~data_out_outfifo.ports.empty)

        all_outputs_ready.append(self._data_out_fifo_full)

        ########################
        # ADDR OUT
        ########################
        self._addr_out_fifo_push = self.var("addr_out_fifo_push", 1)
        self._addr_out_fifo_full = self.var("addr_out_fifo_full", 1)

        out_pushes.append(self._addr_out_fifo_push)

        # Output FIFOs to the buffet-like
        addr_out_fifo_in = kts.concat(self._addr_to_fifo)
        addr_out_outfifo = RegFIFO(data_width=addr_out_fifo_in.width, width_mult=1, depth=8)

        self.add_child(f"addr_out_fifo",
                       addr_out_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._addr_out_fifo_push,
                       pop=self._addr_out_ready_in,
                       data_in=addr_out_fifo_in,
                       data_out=kts.concat(self._addr_out))
        # RegFIFO(data_width=1 * self.data_width + 1, width_mult=1, depth=8)
        self.wire(self._addr_out_fifo_full, addr_out_outfifo.ports.full)
        self.wire(self._addr_out_valid_out, ~addr_out_outfifo.ports.empty)

        all_outputs_ready.append(self._addr_out_fifo_full)

        ########################
        # ID OUT
        ########################
        self._ID_out_fifo_push = self.var("ID_out_fifo_push", 1)
        self._ID_out_fifo_full = self.var("ID_out_fifo_full", 1)

        out_pushes.append(self._ID_out_fifo_push)

        # Output FIFOs to the buffet-like
        ID_out_fifo_in = kts.concat(self._ID_to_fifo)
        ID_out_outfifo = RegFIFO(data_width=ID_out_fifo_in.width, width_mult=1, depth=8)

        self.add_child(f"ID_out_fifo",
                       ID_out_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._ID_out_fifo_push,
                       pop=self._ID_out_ready_in,
                       data_in=ID_out_fifo_in,
                       data_out=kts.concat(self._ID_out))
        # RegFIFO(data_width=1 * self.data_width + 1, width_mult=1, depth=8)
        self.wire(self._ID_out_fifo_full, ID_out_outfifo.ports.full)
        self.wire(self._ID_out_valid_out, ~ID_out_outfifo.ports.empty)

        all_outputs_ready.append(self._ID_out_fifo_full)

        catted = kts.concat(*[*all_outputs_ready])
        # only downstream ready if none of the fifos are full
        self._join_out_ready = (~catted).r_and()
        # Broadcast the single push to all three
        self.wire(kts.concat(*[*out_pushes]), kts.concat(*[self._push_to_outs for i in range(len(out_pushes))]))

# =============================
# SCAN FSM
# =============================

        # Address for writing segment
        self._inc_seg_addr = self.var("inc_seg_addr", 1)
        self._clr_seg_addr = self.var("clr_seg_addr", 1)
        self._seg_addr = add_counter(self, "segment_addr", 16, increment=self._inc_seg_addr, clear=self._clr_seg_addr)

        self._inc_coord_addr = self.var("inc_coord_addr", 1)
        self._clr_coord_addr = self.var("clr_coord_addr", 1)
        self._coord_addr = add_counter(self, "coord_addr", 16, increment=self._inc_coord_addr, clear=self._clr_coord_addr)

        # Value to go to segment
        self._inc_seg_ctr = self.var("inc_seg_ctr", 1)
        self._clr_seg_ctr = self.var("clr_seg_ctr", 1)
        self._seg_ctr = add_counter(self, "segment_counter", 16, increment=self._inc_seg_ctr, clear=self._clr_seg_ctr)

        self._set_curr_coord = self.var("set_curr_coord", 1)
        self._clr_curr_coord = self.var("clr_curr_coord", 1)
        self._curr_coord = register(self, self._data_infifo_data_in, enable=self._set_curr_coord)
        self._curr_coord_valid = sticky_flag(self, self._set_curr_coord, clear=self._clr_curr_coord, name="valid_coord_sticky", seq_only=True)

        # Indicates if we are seeing a new coordinate
        self._new_coord = self.var("new_coord", 1)
        # We have a new coord if the new coord input is valid and the curr_coord is not valid, or the data is different.
        self.wire(self._new_coord, (self._data_infifo_valid_in & ~self._data_infifo_eos_in) & (~self._curr_coord_valid | (self._data_infifo_data_in != self._curr_coord)))

        self._stop_in = self.var("stop_in", 1)
        self.wire(self._stop_in, self._data_infifo_valid_in & self._data_infifo_eos_in)

        self._full_stop = self.var("full_stop", 1)
        self.wire(self._full_stop, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in == 0))

        self._matching_stop = self.var("matching_stop", 1)
        self.wire(self._matching_stop, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in == self._stop_lvl))

        self._clr_wen_made = self.var("clr_wen_made", 1)
        self._wen_made = sticky_flag(self, self._push_to_outs, clear=self._clr_wen_made, name="wen_made", seq_only=True)

        self._ID_curr = self.var("ID_curr", self.data_width)

        # Create FSM
        self.scan_fsm = self.add_fsm("scan_seq", reset_high=False)
        START = self.scan_fsm.add_state("START")
        ALLOCATE1 = self.scan_fsm.add_state("ALLOCATE1")
        ALLOCATE2 = self.scan_fsm.add_state("ALLOCATE2")
        BLOCK_1_SZ = self.scan_fsm.add_state("BLOCK_1_SZ")
        BLOCK_1_WR = self.scan_fsm.add_state("BLOCK_1_WR")
        BLOCK_2_SZ = self.scan_fsm.add_state("BLOCK_2_SZ")
        BLOCK_2_WR = self.scan_fsm.add_state("BLOCK_2_WR")
        # Lowest level
        LL = self.scan_fsm.add_state("LL")
        # Lowest level uncompressed (use address)
        UnLL = self.scan_fsm.add_state("UnLL")
        # Lowest level compressed
        ComLL = self.scan_fsm.add_state("ComLL")
        UL_WZ = self.scan_fsm.add_state("UL_WZ")
        UL = self.scan_fsm.add_state("UL")
        FINALIZE1 = self.scan_fsm.add_state("FINALIZE1")
        FINALIZE2 = self.scan_fsm.add_state("FINALIZE2")
        UL_EMIT_COORD = self.scan_fsm.add_state("UL_EMIT_COORD")
        UL_EMIT_SEG = self.scan_fsm.add_state("UL_EMIT_SEG")
        DONE = self.scan_fsm.add_state("DONE")

        ####################
        # Next State Logic
        ####################

        ####################
        # START #
        ####################
        # Start state goes to either lowest level or upper level
        START.next(ALLOCATE1, None)

        ####################
        # ALLOCATE1 #
        ####################
        # ALLOCATE1 allocates the lower ID buffet
        ALLOCATE1.next(ALLOCATE1, ~self._join_out_ready)
        ALLOCATE1.next(ALLOCATE2, ~self._lowest_level & self._join_out_ready)
        ALLOCATE1.next(BLOCK_1_SZ, self._lowest_level & self._block_mode & self._join_out_ready)
        ALLOCATE1.next(LL, self._lowest_level & ~self._block_mode & self._join_out_ready)
        # ALLOCATE1.next(UL_WZ, ~self._lowest_level & self._join_out_ready)

        ####################
        # ALLOCATE2 #
        ####################
        # lowest level is true here
        ALLOCATE2.next(ALLOCATE2, ~self._join_out_ready)
        ALLOCATE2.next(BLOCK_1_SZ, self._block_mode & self._join_out_ready)
        ALLOCATE2.next(UL_WZ, ~self._block_mode & self._join_out_ready)

        ####################
        # BLOCK_1_SZ
        ####################
        # Get the first block size...
        BLOCK_1_SZ.next(BLOCK_1_WR, self._data_infifo_valid_in)
        BLOCK_1_SZ.next(BLOCK_1_SZ, None)

        ####################
        # BLOCK_1_WR
        ####################
        # Write the first block...
        # If this is just writing a single structure, end after that
        BLOCK_1_WR.next(BLOCK_2_SZ, (self._block_writes == self._block_size) & ~self._lowest_level)
        # BLOCK_1_WR.next(DONE, (self._block_writes == self._block_size) & self._lowest_level)
        BLOCK_1_WR.next(FINALIZE2, (self._block_writes == self._block_size) & self._lowest_level)
        BLOCK_1_WR.next(BLOCK_1_WR, None)

        ####################
        # BLOCK_2_SZ
        ####################
        # Get the second block size...
        BLOCK_2_SZ.next(BLOCK_2_WR, self._data_infifo_valid_in)
        BLOCK_2_SZ.next(BLOCK_2_SZ, None)

        ####################
        # BLOCK_2_WR
        ####################
        # Get the first block size...
        # BLOCK_2_WR.next(DONE, (self._block_writes == self._block_size))
        BLOCK_2_WR.next(FINALIZE1, (self._block_writes == self._block_size))
        BLOCK_2_WR.next(BLOCK_2_WR, None)

        ####################
        # LL #
        ####################
        # Redundant state but helpful in my head
        # Go to compressed or uncompressed from here
        LL.next(ComLL, self._compressed)
        LL.next(UnLL, ~self._compressed)

        ####################
        # ComLL
        ####################
        # In the compressed state of lowest level, we only need to write the
        # data values in order...just watching for the stop 0 token
        # ComLL.next(DONE, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in == 0))
        ComLL.next(FINALIZE2, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in == 0))
        ComLL.next(ComLL, None)

        ####################
        # UnLL
        ####################
        # In the uncompressed lowest level, we are writing the data at the specified address, so we are similarly looking
        # for stop 0 token
        # UnLL.next(DONE, self._data_infifo_valid_in & self._addr_infifo_valid_in & self._data_infifo_eos_in & self._addr_infifo_eos_in &
        UnLL.next(FINALIZE2, self._data_infifo_valid_in & self._addr_infifo_valid_in & self._data_infifo_eos_in & self._addr_infifo_eos_in &
                  (self._data_infifo_data_in == 0) & (self._addr_infifo_data_in == 0))
        UnLL.next(UnLL, None)

        ####################
        # UL_WZ
        ####################
        # Need to write a 0 to the segment array first...
        UL_WZ.next(UL, self._join_out_ready)
        UL_WZ.next(UL_WZ, ~self._join_out_ready)

        ####################
        # UL #
        ####################
        # ASSUMED TO BE COMPRESSED - OTHERWISE DFG LOOKS DIFFERENT - PERFORMS MATH ON COORDINATES
        # In the upper level, we will emit new coordinates linearly as we see new ones, reset tracking at stop_lvl
        UL.next(UL_EMIT_COORD, self._new_coord)
        UL.next(UL_EMIT_SEG, self._matching_stop)
        UL.next(UL, None)

        ####################
        # UL_EMIT_COORD #
        ####################
        # From the emit coord, we will send a write out as long the memory is ready for a write
        # Then go back to UL once we see new data or a stop in
        UL_EMIT_COORD.next(UL, self._new_coord | self._stop_in)
        UL_EMIT_COORD.next(UL_EMIT_COORD, None)

        ####################
        # UL_EMIT_SEG #
        ####################
        # From the emit seg, we will send out the writes to the segment array, will clear all the state
        # Should go to done if we see a stop 0
        # Should only move on once we have drained the subsequent stops and see valid data coming in
        UL_EMIT_SEG.next(UL, self._data_infifo_valid_in & ~self._data_infifo_eos_in)
        # UL_EMIT_SEG.next(DONE, self._full_stop)
        UL_EMIT_SEG.next(FINALIZE1, self._full_stop)
        UL_EMIT_SEG.next(UL_EMIT_SEG, None)

        ####################
        # FINALIZE1
        ####################
        FINALIZE1.next(FINALIZE2, self._join_out_ready)
        FINALIZE1.next(FINALIZE1, None)

        ####################
        # FINALIZE2
        ####################
        FINALIZE2.next(DONE, self._join_out_ready)
        FINALIZE2.next(FINALIZE2, None)

        ####################
        # DONE
        ####################
        # We are done...
        # TODO: Accept multiple blocks
        DONE.next(START, None)

        ####################
        # FSM Output Logic
        ####################

        self.scan_fsm.output(self._data_to_fifo)
        self.scan_fsm.output(self._op_to_fifo)
        self.scan_fsm.output(self._addr_to_fifo)
        self.scan_fsm.output(self._ID_to_fifo)
        self.scan_fsm.output(self._push_to_outs)
        self.scan_fsm.output(self._inc_seg_addr)
        self.scan_fsm.output(self._clr_seg_addr)
        self.scan_fsm.output(self._inc_coord_addr)
        self.scan_fsm.output(self._clr_coord_addr)
        self.scan_fsm.output(self._inc_seg_ctr)
        self.scan_fsm.output(self._clr_seg_ctr)
        self.scan_fsm.output(self._set_curr_coord)
        self.scan_fsm.output(self._clr_curr_coord)
        self.scan_fsm.output(self._infifo_pop[0])
        self.scan_fsm.output(self._infifo_pop[1])
        self.scan_fsm.output(self._clr_wen_made)
        self.scan_fsm.output(self._set_block_size)
        self.scan_fsm.output(self._inc_block_write)
        self.scan_fsm.output(self._clr_block_write)

        #######
        # START - TODO - Generate general hardware...
        #######
        START.output(self._data_to_fifo, kts.const(0, 16))
        START.output(self._op_to_fifo, 0)
        START.output(self._addr_to_fifo, kts.const(0, 16))
        START.output(self._ID_to_fifo, kts.const(0, 16))
        START.output(self._push_to_outs, 0)
        START.output(self._inc_seg_addr, 0)
        START.output(self._clr_seg_addr, 1)
        START.output(self._inc_coord_addr, 0)
        START.output(self._clr_coord_addr, 1)
        START.output(self._inc_seg_ctr, 0)
        START.output(self._clr_seg_ctr, 1)
        START.output(self._set_curr_coord, 0)
        START.output(self._clr_curr_coord, 1)
        START.output(self._infifo_pop[0], 0)
        START.output(self._infifo_pop[1], 0)
        START.output(self._clr_wen_made, 1)
        START.output(self._set_block_size, 0)
        START.output(self._inc_block_write, 0)
        START.output(self._clr_block_write, 1)

        #######
        # ALLOCATE1 - TODO - Generate general hardware...
        #######
        ALLOCATE1.output(self._data_to_fifo, kts.const(0, 16))
        ALLOCATE1.output(self._op_to_fifo, 0)
        ALLOCATE1.output(self._addr_to_fifo, kts.const(0, 16))
        ALLOCATE1.output(self._ID_to_fifo, kts.const(0, 16))
        ALLOCATE1.output(self._push_to_outs, 1)
        ALLOCATE1.output(self._inc_seg_addr, 0)
        ALLOCATE1.output(self._clr_seg_addr, 0)
        ALLOCATE1.output(self._inc_coord_addr, 0)
        ALLOCATE1.output(self._clr_coord_addr, 0)
        ALLOCATE1.output(self._inc_seg_ctr, 0)
        ALLOCATE1.output(self._clr_seg_ctr, 0)
        ALLOCATE1.output(self._set_curr_coord, 0)
        ALLOCATE1.output(self._clr_curr_coord, 0)
        ALLOCATE1.output(self._infifo_pop[0], 0)
        ALLOCATE1.output(self._infifo_pop[1], 0)
        ALLOCATE1.output(self._clr_wen_made, 0)
        ALLOCATE1.output(self._set_block_size, 0)
        ALLOCATE1.output(self._inc_block_write, 0)
        ALLOCATE1.output(self._clr_block_write, 0)

        #######
        # ALLOCATE2 - TODO - Generate general hardware...
        #######
        ALLOCATE2.output(self._data_to_fifo, kts.const(0, 16))
        ALLOCATE2.output(self._op_to_fifo, 0)
        ALLOCATE2.output(self._addr_to_fifo, kts.const(0, 16))
        ALLOCATE2.output(self._ID_to_fifo, kts.const(1, 16))
        ALLOCATE2.output(self._push_to_outs, 1)
        ALLOCATE2.output(self._inc_seg_addr, 0)
        ALLOCATE2.output(self._clr_seg_addr, 0)
        ALLOCATE2.output(self._inc_coord_addr, 0)
        ALLOCATE2.output(self._clr_coord_addr, 0)
        ALLOCATE2.output(self._inc_seg_ctr, 0)
        ALLOCATE2.output(self._clr_seg_ctr, 0)
        ALLOCATE2.output(self._set_curr_coord, 0)
        ALLOCATE2.output(self._clr_curr_coord, 0)
        ALLOCATE2.output(self._infifo_pop[0], 0)
        ALLOCATE2.output(self._infifo_pop[1], 0)
        ALLOCATE2.output(self._clr_wen_made, 0)
        ALLOCATE2.output(self._set_block_size, 0)
        ALLOCATE2.output(self._inc_block_write, 0)
        ALLOCATE2.output(self._clr_block_write, 0)

        #######
        # BLOCK_1_SZ
        #######
        BLOCK_1_SZ.output(self._data_to_fifo, kts.const(0, 16))
        BLOCK_1_SZ.output(self._op_to_fifo, 0)
        BLOCK_1_SZ.output(self._addr_to_fifo, kts.const(0, 16))
        BLOCK_1_SZ.output(self._ID_to_fifo, kts.const(1, 16))
        BLOCK_1_SZ.output(self._push_to_outs, 0)
        BLOCK_1_SZ.output(self._inc_seg_addr, 0)
        BLOCK_1_SZ.output(self._clr_seg_addr, 0)
        BLOCK_1_SZ.output(self._inc_coord_addr, 0)
        BLOCK_1_SZ.output(self._clr_coord_addr, 0)
        BLOCK_1_SZ.output(self._inc_seg_ctr, 0)
        BLOCK_1_SZ.output(self._clr_seg_ctr, 0)
        BLOCK_1_SZ.output(self._set_curr_coord, 0)
        BLOCK_1_SZ.output(self._clr_curr_coord, 0)
        BLOCK_1_SZ.output(self._infifo_pop[0], self._data_infifo_valid_in)
        BLOCK_1_SZ.output(self._infifo_pop[1], 0)
        BLOCK_1_SZ.output(self._clr_wen_made, 0)
        BLOCK_1_SZ.output(self._set_block_size, self._data_infifo_valid_in)
        BLOCK_1_SZ.output(self._inc_block_write, 0)
        BLOCK_1_SZ.output(self._clr_block_write, 1)

        #######
        # BLOCK_1_WR
        #######
        BLOCK_1_WR.output(self._data_to_fifo, self._data_infifo_data_in)
        BLOCK_1_WR.output(self._op_to_fifo, 1)
        BLOCK_1_WR.output(self._addr_to_fifo, self._block_writes)
        BLOCK_1_WR.output(self._ID_to_fifo, kts.const(0, 16))
        BLOCK_1_WR.output(self._push_to_outs, self._data_infifo_valid_in & (self._block_writes < self._block_size))
        # BLOCK_1_WR.output(self._addr_out, self._block_writes)
        # BLOCK_1_WR.output(self._wen, self._data_infifo_valid_in & (self._block_writes < self._block_size))
        # BLOCK_1_WR.output(self._data_out, self._data_infifo_data_in)
        BLOCK_1_WR.output(self._inc_seg_addr, 0)
        BLOCK_1_WR.output(self._clr_seg_addr, 0)
        BLOCK_1_WR.output(self._inc_coord_addr, 0)
        BLOCK_1_WR.output(self._clr_coord_addr, 0)
        BLOCK_1_WR.output(self._inc_seg_ctr, 0)
        BLOCK_1_WR.output(self._clr_seg_ctr, 0)
        BLOCK_1_WR.output(self._set_curr_coord, 0)
        BLOCK_1_WR.output(self._clr_curr_coord, 0)
        BLOCK_1_WR.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_1_WR.output(self._infifo_pop[1], 0)
        BLOCK_1_WR.output(self._clr_wen_made, 0)
        BLOCK_1_WR.output(self._set_block_size, 0)
        BLOCK_1_WR.output(self._inc_block_write, self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_1_WR.output(self._clr_block_write, 0)

        #######
        # BLOCK_2_SZ
        #######
        BLOCK_2_SZ.output(self._data_to_fifo, kts.const(0, 16))
        BLOCK_2_SZ.output(self._op_to_fifo, 0)
        BLOCK_2_SZ.output(self._addr_to_fifo, kts.const(0, 16))
        BLOCK_2_SZ.output(self._ID_to_fifo, kts.const(0, 16))
        BLOCK_2_SZ.output(self._push_to_outs, 0)
        # BLOCK_2_SZ.output(self._addr_out, kts.const(0, 16))
        # BLOCK_2_SZ.output(self._wen, 0)
        # BLOCK_2_SZ.output(self._data_out, kts.const(0, 16))
        BLOCK_2_SZ.output(self._inc_seg_addr, 0)
        BLOCK_2_SZ.output(self._clr_seg_addr, 0)
        BLOCK_2_SZ.output(self._inc_coord_addr, 0)
        BLOCK_2_SZ.output(self._clr_coord_addr, 0)
        BLOCK_2_SZ.output(self._inc_seg_ctr, 0)
        BLOCK_2_SZ.output(self._clr_seg_ctr, 0)
        BLOCK_2_SZ.output(self._set_curr_coord, 0)
        BLOCK_2_SZ.output(self._clr_curr_coord, 0)
        BLOCK_2_SZ.output(self._infifo_pop[0], self._data_infifo_valid_in)
        BLOCK_2_SZ.output(self._infifo_pop[1], 0)
        BLOCK_2_SZ.output(self._clr_wen_made, 0)
        BLOCK_2_SZ.output(self._set_block_size, self._data_infifo_valid_in)
        BLOCK_2_SZ.output(self._inc_block_write, 0)
        BLOCK_2_SZ.output(self._clr_block_write, 1)

        #######
        # BLOCK_2_WR
        #######
        BLOCK_2_WR.output(self._data_to_fifo, self._data_infifo_data_in)
        BLOCK_2_WR.output(self._op_to_fifo, 1)
        BLOCK_2_WR.output(self._addr_to_fifo, self._block_writes)
        BLOCK_2_WR.output(self._ID_to_fifo, kts.const(1, 16))
        BLOCK_2_WR.output(self._push_to_outs, self._data_infifo_valid_in & (self._block_writes < self._block_size))
        # BLOCK_2_WR.output(self._addr_out, self._block_writes + self._inner_dim_offset)
        # BLOCK_2_WR.output(self._wen, self._data_infifo_valid_in & (self._block_writes < self._block_size))
        # BLOCK_2_WR.output(self._data_out, self._data_infifo_data_in)
        BLOCK_2_WR.output(self._inc_seg_addr, 0)
        BLOCK_2_WR.output(self._clr_seg_addr, 0)
        BLOCK_2_WR.output(self._inc_coord_addr, 0)
        BLOCK_2_WR.output(self._clr_coord_addr, 0)
        BLOCK_2_WR.output(self._inc_seg_ctr, 0)
        BLOCK_2_WR.output(self._clr_seg_ctr, 0)
        BLOCK_2_WR.output(self._set_curr_coord, 0)
        BLOCK_2_WR.output(self._clr_curr_coord, 0)
        BLOCK_2_WR.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_2_WR.output(self._infifo_pop[1], 0)
        BLOCK_2_WR.output(self._clr_wen_made, 0)
        BLOCK_2_WR.output(self._set_block_size, 0)
        BLOCK_2_WR.output(self._inc_block_write, self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_2_WR.output(self._clr_block_write, 0)

        #######
        # LL
        #######
        LL.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        LL.output(self._op_to_fifo, 0)
        LL.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        LL.output(self._ID_to_fifo, kts.const(0, 16))
        LL.output(self._push_to_outs, 0)
        # LL.output(self._addr_out, kts.const(0, 16))
        # LL.output(self._wen, 0)
        # LL.output(self._data_out, kts.const(0, 16))
        LL.output(self._inc_seg_addr, 0)
        LL.output(self._clr_seg_addr, 0)
        LL.output(self._inc_coord_addr, 0)
        LL.output(self._clr_coord_addr, 0)
        LL.output(self._inc_seg_ctr, 0)
        LL.output(self._clr_seg_ctr, 0)
        LL.output(self._set_curr_coord, 0)
        LL.output(self._clr_curr_coord, 0)
        LL.output(self._infifo_pop[0], 0)
        LL.output(self._infifo_pop[1], 0)
        LL.output(self._clr_wen_made, 0)
        LL.output(self._set_block_size, 0)
        LL.output(self._inc_block_write, 0)
        LL.output(self._clr_block_write, 0)

        #######
        # UnLL
        #######
        UnLL.output(self._data_to_fifo, self._data_infifo_data_in)
        UnLL.output(self._op_to_fifo, 1)
        UnLL.output(self._addr_to_fifo, self._addr_infifo_data_in)
        UnLL.output(self._ID_to_fifo, kts.const(0, 16))
        UnLL.output(self._push_to_outs, (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ~(self._data_infifo_eos_in | self._addr_infifo_eos_in))
        # UnLL.output(self._addr_out, self._addr_infifo_data_in)
        # Only write the values
        # UnLL.output(self._wen, (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ~(self._data_infifo_eos_in | self._addr_infifo_eos_in))
        # UnLL.output(self._data_out, self._data_infifo_data_in)
        UnLL.output(self._inc_seg_addr, 0)
        UnLL.output(self._clr_seg_addr, 0)
        UnLL.output(self._inc_coord_addr, 0)
        UnLL.output(self._clr_coord_addr, 0)
        UnLL.output(self._inc_seg_ctr, 0)
        UnLL.output(self._clr_seg_ctr, 0)
        UnLL.output(self._set_curr_coord, 0)
        UnLL.output(self._clr_curr_coord, 0)
        # Pop if the memory is ready for a write, or its eos
        UnLL.output(self._infifo_pop[0], (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ((self._data_infifo_eos_in & self._addr_infifo_eos_in) | self._join_out_ready))
        UnLL.output(self._infifo_pop[1], (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ((self._data_infifo_eos_in & self._addr_infifo_eos_in) | self._join_out_ready))
        UnLL.output(self._clr_wen_made, 0)
        UnLL.output(self._set_block_size, 0)
        UnLL.output(self._inc_block_write, 0)
        UnLL.output(self._clr_block_write, 0)

        #######
        # ComLL
        #######
        ComLL.output(self._data_to_fifo, self._data_infifo_data_in)
        ComLL.output(self._op_to_fifo, 1)
        ComLL.output(self._addr_to_fifo, self._seg_addr)
        ComLL.output(self._ID_to_fifo, kts.const(0, 16))
        ComLL.output(self._push_to_outs, self._data_infifo_valid_in & ~self._data_infifo_eos_in)
        # Use the seg addr
        # ComLL.output(self._addr_out, self._seg_addr)
        # Only write if its data
        # ComLL.output(self._wen, self._data_infifo_valid_in & ~self._data_infifo_eos_in)
        # ComLL.output(self._data_out, self._data_infifo_data_in)
        # Increase the seg addr only if we are actually writing
        ComLL.output(self._inc_seg_addr, self._data_infifo_valid_in & ~self._data_infifo_eos_in & self._join_out_ready)
        ComLL.output(self._clr_seg_addr, 0)
        ComLL.output(self._inc_coord_addr, 0)
        ComLL.output(self._clr_coord_addr, 0)
        ComLL.output(self._inc_seg_ctr, 0)
        ComLL.output(self._clr_seg_ctr, 0)
        ComLL.output(self._set_curr_coord, 0)
        ComLL.output(self._clr_curr_coord, 0)
        # Only pop if its eos or the memory is ready for the write
        ComLL.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._data_infifo_eos_in | self._join_out_ready))
        ComLL.output(self._infifo_pop[1], 0)
        ComLL.output(self._clr_wen_made, 0)
        ComLL.output(self._set_block_size, 0)
        ComLL.output(self._inc_block_write, 0)
        ComLL.output(self._clr_block_write, 0)

        #######
        # UL_WZ
        #######
        UL_WZ.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        UL_WZ.output(self._op_to_fifo, 1)
        UL_WZ.output(self._addr_to_fifo, self._seg_addr)
        UL_WZ.output(self._ID_to_fifo, kts.const(0, 16))
        UL_WZ.output(self._push_to_outs, 1)

        # Write a 0 to the segment array
        # UL_WZ.output(self._addr_out, self._seg_addr)
        # UL_WZ.output(self._wen, 1)
        # UL_WZ.output(self._data_out, kts.const(0, 16))
        UL_WZ.output(self._inc_seg_addr, self._join_out_ready)
        UL_WZ.output(self._clr_seg_addr, 0)
        UL_WZ.output(self._inc_coord_addr, 0)
        UL_WZ.output(self._clr_coord_addr, 0)
        UL_WZ.output(self._inc_seg_ctr, 0)
        UL_WZ.output(self._clr_seg_ctr, 0)
        UL_WZ.output(self._set_curr_coord, 0)
        UL_WZ.output(self._clr_curr_coord, 0)
        UL_WZ.output(self._infifo_pop[0], 0)
        UL_WZ.output(self._infifo_pop[1], 0)
        UL_WZ.output(self._clr_wen_made, 0)
        UL_WZ.output(self._set_block_size, 0)
        UL_WZ.output(self._inc_block_write, 0)
        UL_WZ.output(self._clr_block_write, 0)

        #######
        # UL
        #######
        UL.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        UL.output(self._op_to_fifo, 0)
        UL.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        UL.output(self._ID_to_fifo, kts.const(0, 16))
        UL.output(self._push_to_outs, 0)

        # UL.output(self._addr_out, kts.const(0, 16))
        # UL.output(self._wen, 0)
        # UL.output(self._data_out, kts.const(0, 16))
        UL.output(self._inc_seg_addr, 0)
        UL.output(self._clr_seg_addr, 0)
        UL.output(self._inc_coord_addr, 0)
        UL.output(self._clr_coord_addr, 0)
        UL.output(self._inc_seg_ctr, 0)
        UL.output(self._clr_seg_ctr, 0)
        UL.output(self._set_curr_coord, self._new_coord)
        UL.output(self._clr_curr_coord, 0)
        # Pop below the stop level
        UL.output(self._infifo_pop[0], self._stop_in & (self._data_infifo_data_in > self._stop_lvl))
        # UL.output(self._infifo_pop[0], 0)
        UL.output(self._infifo_pop[1], 0)
        UL.output(self._clr_wen_made, 1)
        UL.output(self._set_block_size, 0)
        UL.output(self._inc_block_write, 0)
        UL.output(self._clr_block_write, 0)

        #######
        # UL_EMIT_COORD
        #######
        UL_EMIT_COORD.output(self._data_to_fifo, self._curr_coord)
        UL_EMIT_COORD.output(self._op_to_fifo, 1)
        UL_EMIT_COORD.output(self._addr_to_fifo, self._coord_addr)
        UL_EMIT_COORD.output(self._ID_to_fifo, kts.const(1, 16))
        UL_EMIT_COORD.output(self._push_to_outs, ~self._wen_made & self._join_out_ready)

        # UL_EMIT_COORD.output(self._addr_out, self._coord_addr + self._inner_dim_offset)
        # UL_EMIT_COORD.output(self._wen, ~self._wen_made & self._ready_in)
        # UL_EMIT_COORD.output(self._data_out, self._curr_coord)
        UL_EMIT_COORD.output(self._inc_seg_addr, 0)
        UL_EMIT_COORD.output(self._clr_seg_addr, 0)
        UL_EMIT_COORD.output(self._inc_coord_addr, ~self._wen_made & self._join_out_ready)
        UL_EMIT_COORD.output(self._clr_coord_addr, 0)
        UL_EMIT_COORD.output(self._inc_seg_ctr, ~self._wen_made & self._join_out_ready)
        UL_EMIT_COORD.output(self._clr_seg_ctr, 0)
        UL_EMIT_COORD.output(self._set_curr_coord, 0)
        UL_EMIT_COORD.output(self._clr_curr_coord, 0)
        # Pop until stop in or new coordinate
        UL_EMIT_COORD.output(self._infifo_pop[0], ~self._new_coord & ~self._stop_in)
        UL_EMIT_COORD.output(self._infifo_pop[1], 0)
        UL_EMIT_COORD.output(self._clr_wen_made, 0)
        UL_EMIT_COORD.output(self._set_block_size, 0)
        UL_EMIT_COORD.output(self._inc_block_write, 0)
        UL_EMIT_COORD.output(self._clr_block_write, 0)

        #######
        # UL_EMIT_SEG
        #######
        UL_EMIT_SEG.output(self._data_to_fifo, self._seg_ctr)
        UL_EMIT_SEG.output(self._op_to_fifo, 1)
        UL_EMIT_SEG.output(self._addr_to_fifo, self._seg_addr)
        UL_EMIT_SEG.output(self._ID_to_fifo, kts.const(0, 16))
        UL_EMIT_SEG.output(self._push_to_outs, ~self._wen_made & self._join_out_ready)

        # UL_EMIT_SEG.output(self._addr_out, self._seg_addr)
        # UL_EMIT_SEG.output(self._wen, ~self._wen_made & self._ready_in)
        # UL_EMIT_SEG.output(self._data_out, self._seg_ctr)
        UL_EMIT_SEG.output(self._inc_seg_addr, ~self._wen_made & self._join_out_ready)
        UL_EMIT_SEG.output(self._clr_seg_addr, 0)
        UL_EMIT_SEG.output(self._inc_coord_addr, 0)
        UL_EMIT_SEG.output(self._clr_coord_addr, 0)
        UL_EMIT_SEG.output(self._inc_seg_ctr, 0)
        UL_EMIT_SEG.output(self._clr_seg_ctr, 0)
        UL_EMIT_SEG.output(self._set_curr_coord, 0)
        # Make sure to clear the coord on segment emissions so it doesn't get reused
        UL_EMIT_SEG.output(self._clr_curr_coord, 1)
        # Assumption is that valid sets of coordinates are always passed here so I should be able to hit new data
        # Pop until we have data in thats not a stop (or we fall through to DONE)
        UL_EMIT_SEG.output(self._infifo_pop[0], self._data_infifo_valid_in & self._data_infifo_eos_in)
        UL_EMIT_SEG.output(self._infifo_pop[1], 0)
        UL_EMIT_SEG.output(self._clr_wen_made, 0)
        UL_EMIT_SEG.output(self._set_block_size, 0)
        UL_EMIT_SEG.output(self._inc_block_write, 0)
        UL_EMIT_SEG.output(self._clr_block_write, 0)

        #############
        # FINALIZE1
        #############
        FINALIZE1.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        FINALIZE1.output(self._op_to_fifo, 0)
        FINALIZE1.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        FINALIZE1.output(self._ID_to_fifo, kts.const(1, 16))
        FINALIZE1.output(self._push_to_outs, 1)

        # DONE.output(self._addr_out, kts.const(0, 16))
        # DONE.output(self._wen, 0)
        # DONE.output(self._data_out, kts.const(0, 16))
        FINALIZE1.output(self._inc_seg_addr, 0)
        FINALIZE1.output(self._clr_seg_addr, 0)
        FINALIZE1.output(self._inc_coord_addr, 0)
        FINALIZE1.output(self._clr_coord_addr, 0)
        FINALIZE1.output(self._inc_seg_ctr, 0)
        FINALIZE1.output(self._clr_seg_ctr, 0)
        FINALIZE1.output(self._set_curr_coord, 0)
        FINALIZE1.output(self._clr_curr_coord, 0)
        FINALIZE1.output(self._infifo_pop[0], 0)
        FINALIZE1.output(self._infifo_pop[1], 0)
        FINALIZE1.output(self._clr_wen_made, 0)
        FINALIZE1.output(self._set_block_size, 0)
        FINALIZE1.output(self._inc_block_write, 0)
        FINALIZE1.output(self._clr_block_write, 0)

        #############
        # FINALIZE2
        #############
        FINALIZE2.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        FINALIZE2.output(self._op_to_fifo, 0)
        FINALIZE2.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        FINALIZE2.output(self._ID_to_fifo, kts.const(0, 16))
        FINALIZE2.output(self._push_to_outs, 1)

        # DONE.output(self._addr_out, kts.const(0, 16))
        # DONE.output(self._wen, 0)
        # DONE.output(self._data_out, kts.const(0, 16))
        FINALIZE2.output(self._inc_seg_addr, 0)
        FINALIZE2.output(self._clr_seg_addr, 0)
        FINALIZE2.output(self._inc_coord_addr, 0)
        FINALIZE2.output(self._clr_coord_addr, 0)
        FINALIZE2.output(self._inc_seg_ctr, 0)
        FINALIZE2.output(self._clr_seg_ctr, 0)
        FINALIZE2.output(self._set_curr_coord, 0)
        FINALIZE2.output(self._clr_curr_coord, 0)
        FINALIZE2.output(self._infifo_pop[0], 0)
        FINALIZE2.output(self._infifo_pop[1], 0)
        FINALIZE2.output(self._clr_wen_made, 0)
        FINALIZE2.output(self._set_block_size, 0)
        FINALIZE2.output(self._inc_block_write, 0)
        FINALIZE2.output(self._clr_block_write, 0)

        #############
        # DONE
        #############
        DONE.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        DONE.output(self._op_to_fifo, 0)
        DONE.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        DONE.output(self._ID_to_fifo, kts.const(0, 16))
        DONE.output(self._push_to_outs, 0)

        # DONE.output(self._addr_out, kts.const(0, 16))
        # DONE.output(self._wen, 0)
        # DONE.output(self._data_out, kts.const(0, 16))
        DONE.output(self._inc_seg_addr, 0)
        DONE.output(self._clr_seg_addr, 0)
        DONE.output(self._inc_coord_addr, 0)
        DONE.output(self._clr_coord_addr, 0)
        DONE.output(self._inc_seg_ctr, 0)
        DONE.output(self._clr_seg_ctr, 0)
        DONE.output(self._set_curr_coord, 0)
        DONE.output(self._clr_curr_coord, 0)
        DONE.output(self._infifo_pop[0], 0)
        DONE.output(self._infifo_pop[1], 0)
        DONE.output(self._clr_wen_made, 0)
        DONE.output(self._set_block_size, 0)
        DONE.output(self._inc_block_write, 0)
        DONE.output(self._clr_block_write, 0)

        self.scan_fsm.set_start_state(START)

        # Force FSM realization first so that flush gets added...
        kts.passes.realize_fsm(self.internal_generator)

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

    def get_bitstream(self, inner_offset, compressed=0, lowest_level=0, stop_lvl=0, block_mode=0):

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Store all configurations here
        config = [
            ("inner_dim_offset", inner_offset),
            ("compressed", compressed),
            ("lowest_level", lowest_level),
            ("stop_lvl", stop_lvl),
            ("block_mode", block_mode),
            ("tile_en", 1)]

        return trim_config_list(flattened, config)


if __name__ == "__main__":
    scanner_dut = WriteScanner(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(scanner_dut, filename="write_scanner.sv",
            optimize_if=False)
