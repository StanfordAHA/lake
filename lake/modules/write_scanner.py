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

        # Data to write to memory (whether its val/coord/metadata)
        self._data_out = self.output("data_out", self.data_width)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._outer_coord_out = self.output("outer_coord_out", self.data_width)
        # self._outer_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._pos_out = self.output("pos_out", self.data_width)
        # self._pos_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # Valid out for both streams
        # self._valid_out = self.output("valid_out", 2)
        # self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # Address to write to memory
        self._addr_out = self.output("addr_out", self.data_width)
        self._addr_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # Eos for both streams...
        # self._eos_out = self.output("eos_out", 2)
        # self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Memory write enable signal
        self._wen = self.output("wen", 1)
        self._wen.add_attribute(ControlSignalAttr(is_control=False))

        # Point to the row in storage for data recovery
        # self._payload_ptr = self.output("payload_ptr", 16)
        # self._payload_ptr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._inner_dim_offset = self.input("inner_dim_offset", 16)
        self._inner_dim_offset.add_attribute(ConfigRegAttr("Memory address of the inner level..."))

# ==========================================
# Generate addresses to scan over fiber...
# ==========================================

        self._inc_fiber_addr = self.var("inc_fiber_addr", 1)
        self._clr_fiber_addr = self.var("clr_fiber_addr", 1)
        self._fiber_addr_pre = add_counter(self, "fiber_addr_pre", 16, self._inc_fiber_addr, clear=self._clr_fiber_addr)

        self._inc_rep = self.var("inc_rep", 1)
        self._clr_rep = self.var("clr_rep", 1)
        self._num_reps = add_counter(self, "num_reps", 16, self._inc_rep, self._clr_rep)
        # self._step_agen = self.var("step_agen", 1)

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


# =============================
# SCAN FSM
# =============================

        # Address for writing segment
        self._inc_seg_addr = self.var("inc_seg_addr", 1)
        self._clr_seg_addr = self.var("clr_seg_addr", 1)
        self._seg_addr = add_counter(self, "segment_addr", 16, increment=self._inc_seg_addr, clear=self._clr_seg_addr)

        # Value to go to segment
        self._inc_seg_ctr = self.var("inc_seg_ctr", 1)
        self._clr_seg_ctr = self.var("clr_seg_ctr", 1)
        self._seg_ctr = add_counter(self, "segment_counter", 16, increment=self._inc_seg_ctr, clear=self._clr_seg_ctr)

        self._set_curr_coord = self.var("set_curr_coord", 1)
        self._curr_coord = register(self, self._data_infifo_data_in, enable=self._set_curr_coord)

        # Create FSM
        self.scan_fsm = self.add_fsm("scan_seq", reset_high=False)
        START = self.scan_fsm.add_state("START")
        # Lowest level
        LL = self.scan_fsm.add_state("LL")
        # Lowest level uncompressed (use address)
        UnLL = self.scan_fsm.add_state("UnLL")
        # Lowest level compressed 
        ComLL = self.scan_fsm.add_state("ComLL")

        UL = self.scan_fsm.add_state("UL")

        DONE = self.scan_fsm.add_state("DONE")

        self.scan_fsm.output(self._valid_inc)
        self.scan_fsm.output(self._valid_rst)
        self.scan_fsm.output(self._ren)
        self.scan_fsm.output(self._fifo_push)
        self.scan_fsm.output(self._tag_eos)
        self.scan_fsm.output(self._addr_out)
        self.scan_fsm.output(self._next_seq_length)
        self.scan_fsm.output(self._update_seq_state)
        self.scan_fsm.output(self._last_valid_accepting)
        self.scan_fsm.output(self._pop_infifo)
        self.scan_fsm.output(self._inc_fiber_addr)
        self.scan_fsm.output(self._clr_fiber_addr)
        self.scan_fsm.output(self._inc_rep)
        self.scan_fsm.output(self._clr_rep)
        self.scan_fsm.output(self._data_to_fifo)
        self.scan_fsm.output(self._en_reg_data_in)
        self.scan_fsm.output(self._pos_out_to_fifo)

        ####################
        # Next State Logic
        ####################

        # Dummy state for eventual filling block.
        START.next(ISSUE_STRM, self._root)
        START.next(ISSUE_STRM_NR, ~self._root)

        # Completely done at this point
        # ISSUE_STRM.next(DONE, self._out_dim_x == self._max_outer_dim)
        # If not done, we have to issue more streams
        # ISSUE_STRM.next(SEQ_START, (self._outer_addr == self._previous_outer) & self._previous_outer_valid)
        # ISSUE_STRM.next(READ_0, ~((self._outer_addr == self._previous_outer) & self._previous_outer_valid))
        ISSUE_STRM.next(READ_0, None)

        # If we are seeing the eos_in this state we need to pass them along to downstream modules
        # If we have valid data in (the input fifo is not empty), then we should issue the corresponding stream
        ISSUE_STRM_NR.next(PASS_STOP, self._infifo_eos_in & self._infifo_valid_in)
        ISSUE_STRM_NR.next(READ_0, ~self._infifo_eos_in & self._infifo_valid_in)
        ISSUE_STRM_NR.next(ISSUE_STRM_NR, None)

        # In this state, we are passing through stop tokens into
        # the downstream
        PASS_STOP.next(PASS_STOP, self._infifo_eos_in)
        PASS_STOP.next(ISSUE_STRM_NR, ~self._infifo_eos_in & ~self._seen_root_eos)
        PASS_STOP.next(DONE, ~self._infifo_eos_in & self._seen_root_eos)

        # Can continue on our way in root mode, otherwise we need to
        # bring the pointer up to locate the coordinate
        READ_0.next(READ_1, None)

        # If you're not at the right coordinate yet, go straight to SEQ_START and emit length 0 sequence
        # READ_1.next(SEQ_START, (self._coord_in > self._outer_addr) & self._root)
        # # Otherwise, proceed
        # READ_1.next(READ_2, (self._coord_in <= self._outer_addr) & self._root)
        # READ_1.next(READ_2, self._root)

        # # If not root, we should go back to READ_0 while the input coordinate is smaller than the fifo coord
        # READ_1.next(READ_0, (self._coord_in < self._infifo_coord_in) & ~self._root)
        # # Otherwise, proceed
        # READ_1.next(READ_2, (self._coord_in >= self._infifo_coord_in) & ~self._root)
        READ_1.next(READ_2, None)

        # Go to this state to see the length, will also have EOS?
        READ_2.next(SEQ_START, None)

        # At Start, we can either immediately end the stream or go to the iteration phase
        SEQ_START.next(SEQ_DONE, self._seq_length == kts.const(2 ** 16 - 1, 16))
        SEQ_START.next(SEQ_ITER, None)

        # In ITER, we go back to idle when the fifo is full to avoid
        # complexity, or if we are looking at one of the eos since we can make the last
        # move for the intersection now...
        # If we have eos and can push to the fifo, we are done
        # SEQ_ITER.next(SEQ_DONE, self._last_valid_accepting)
        SEQ_ITER.next(REP_INNER_PRE, self._root & self._do_repeat & ~self._repeat_outer_inner_n)
        SEQ_ITER.next(REP_OUTER, (self._root & self._do_repeat & self._repeat_outer_inner_n) & self._iter_finish)
        SEQ_ITER.next(SEQ_DONE, self._iter_finish)
        SEQ_ITER.next(SEQ_ITER, None)

        # Once done, we need another flush
        # SEQ_DONE.next(DONE, ((self._outer_restart | self._outer_length_one) & self._root) | (self._eos_in_seen))
        # SEQ_DONE.next(ISSUE_STRM, ~self._outer_restart & self._root)
        SEQ_DONE.next(SEQ_DONE, self._fifo_full)
        SEQ_DONE.next(DONE, self._root & ~self._fifo_full)
        # SEQ_DONE.next(ISSUE_STRM_NR, ~self._outer_restart & ~self._root)
        # It might be the case that you should always set up another issue if not at EOS
        SEQ_DONE.next(ISSUE_STRM_NR, ~self._root & ~self._fifo_full)

        # Now handle the repetition states...

        # Rep inner pre is used to register the read from memory...can probably optimize this away
        # TODO: Optimize this state away...
        REP_INNER_PRE.next(REP_INNER, None)

        # From rep inner, keep emitting the same data until the reps are finished
        REP_INNER.next(REP_STOP, self._rep_finish)
        REP_INNER.next(REP_INNER, None)

        # This state might be unnecessary, but from rep outer, we should inject another STOP
        REP_OUTER.next(REP_STOP, None)

        # From the stop injection we can either 1. go back to the iterator for the repeat inner case
        # if there is more to do (iter has not finished yet) or go to seq done if it is or...
        # 2. go to the start of the sequence again, clearing state similar to the transition from R2 to seq start
        # Fall through to stay here until the token gets pushed into the FIFO
        # Can only move on if the fifo is not full...
        REP_STOP.next(SEQ_ITER, ~self._iter_finish & ~self._repeat_outer_inner_n & ~self._fifo_full)
        REP_STOP.next(SEQ_DONE, self._iter_finish & ~self._repeat_outer_inner_n & ~self._fifo_full)
        # TODO: Add assertion the iter_finish is high if in outer repeat
        REP_STOP.next(SEQ_START, self._repeat_outer_inner_n & ~self._rep_finish & ~self._fifo_full)
        REP_STOP.next(SEQ_DONE, self._repeat_outer_inner_n & self._rep_finish & ~self._fifo_full)
        REP_STOP.next(REP_STOP, None)

        # DONE
        DONE.next(DONE, None)

        ####################
        # FSM Output Logic
        ####################

        #######
        # START - TODO - Generate general hardware...
        #######
        START.output(self._valid_inc, 0)
        START.output(self._valid_rst, 0)
        START.output(self._ren, 0)
        START.output(self._fifo_push, 0)
        START.output(self._tag_eos, 0)
        START.output(self._addr_out, kts.const(0, 16))
        START.output(self._next_seq_length, kts.const(0, 16))
        START.output(self._update_seq_state, 0)
        START.output(self._last_valid_accepting, 0)
        START.output(self._pop_infifo, 0)
        START.output(self._inc_fiber_addr, 0)
        START.output(self._clr_fiber_addr, 0)
        START.output(self._inc_rep, 0)
        START.output(self._clr_rep, 0)
        START.output(self._data_to_fifo, kts.const(0, 16))
        START.output(self._en_reg_data_in, 0)
        START.output(self._pos_out_to_fifo, kts.const(0, 16))

        #######
        # ISSUE_STRM - TODO - Generate general hardware...
        #######
        ISSUE_STRM.output(self._valid_inc, 0)
        ISSUE_STRM.output(self._valid_rst, 0)
        ISSUE_STRM.output(self._ren, 0)
        ISSUE_STRM.output(self._fifo_push, 0)
        ISSUE_STRM.output(self._tag_eos, 0)
        # Only increment if we are seeing a new address and the most recent stream wasn't 0 length
        ISSUE_STRM.output(self._addr_out, kts.const(0, 16))
        ISSUE_STRM.output(self._next_seq_length, kts.const(0, 16))
        ISSUE_STRM.output(self._update_seq_state, 0)
        ISSUE_STRM.output(self._last_valid_accepting, 0)
        ISSUE_STRM.output(self._pop_infifo, 0)
        ISSUE_STRM.output(self._inc_fiber_addr, 0)
        ISSUE_STRM.output(self._clr_fiber_addr, 0)
        ISSUE_STRM.output(self._inc_rep, 0)
        ISSUE_STRM.output(self._clr_rep, 0)
        ISSUE_STRM.output(self._data_to_fifo, kts.const(0, 16))
        ISSUE_STRM.output(self._en_reg_data_in, 0)
        ISSUE_STRM.output(self._pos_out_to_fifo, kts.const(0, 16))

        #######
        # ISSUE_STRM_NR
        #######
        ISSUE_STRM_NR.output(self._valid_inc, 0)
        ISSUE_STRM_NR.output(self._valid_rst, 0)
        ISSUE_STRM_NR.output(self._ren, 0)
        ISSUE_STRM_NR.output(self._fifo_push, 0)
        ISSUE_STRM_NR.output(self._tag_eos, 0)
        # Only increment if we are seeing a new address and the most recent stream wasn't 0 length
        ISSUE_STRM_NR.output(self._addr_out, kts.const(0, 16))
        ISSUE_STRM_NR.output(self._next_seq_length, kts.const(0, 16))
        ISSUE_STRM_NR.output(self._update_seq_state, 0)
        ISSUE_STRM_NR.output(self._last_valid_accepting, 0)
        ISSUE_STRM_NR.output(self._pop_infifo, 0)
        ISSUE_STRM_NR.output(self._inc_fiber_addr, 0)
        ISSUE_STRM_NR.output(self._clr_fiber_addr, 0)
        ISSUE_STRM_NR.output(self._inc_rep, 0)
        ISSUE_STRM_NR.output(self._clr_rep, 0)
        ISSUE_STRM_NR.output(self._data_to_fifo, kts.const(0, 16))
        ISSUE_STRM_NR.output(self._en_reg_data_in, 0)
        ISSUE_STRM_NR.output(self._pos_out_to_fifo, kts.const(0, 16))

        #######
        # PASS_STOP
        #######
        PASS_STOP.output(self._valid_inc, 0)
        PASS_STOP.output(self._valid_rst, 0)
        PASS_STOP.output(self._ren, 0)
        PASS_STOP.output(self._fifo_push, self._infifo_eos_in)
        PASS_STOP.output(self._tag_eos, 1)
        # Only increment if we are seeing a new address and the most recent stream wasn't 0 length
        PASS_STOP.output(self._addr_out, kts.const(0, 16))
        PASS_STOP.output(self._next_seq_length, kts.const(0, 16))
        PASS_STOP.output(self._update_seq_state, 0)
        PASS_STOP.output(self._last_valid_accepting, 0)
        PASS_STOP.output(self._pop_infifo, ~self._fifo_full & self._infifo_eos_in)
        PASS_STOP.output(self._inc_fiber_addr, 0)
        PASS_STOP.output(self._clr_fiber_addr, 0)
        PASS_STOP.output(self._inc_rep, 0)
        PASS_STOP.output(self._clr_rep, 0)
        PASS_STOP.output(self._data_to_fifo, self._infifo_pos_in)
        PASS_STOP.output(self._en_reg_data_in, 0)
        PASS_STOP.output(self._pos_out_to_fifo, self._infifo_pos_in)

        #######
        # READ_0 - TODO - Generate general hardware...
        #######
        READ_0.output(self._valid_inc, 0)
        READ_0.output(self._valid_rst, 0)
        READ_0.output(self._ren, 1)
        READ_0.output(self._fifo_push, 0)
        READ_0.output(self._tag_eos, 0)
        # READ_0.output(self._addr_out, self._out_dim_addr)
        READ_0.output(self._addr_out, self._pos_addr)
        READ_0.output(self._next_seq_length, kts.const(0, 16))
        READ_0.output(self._update_seq_state, 0)
        READ_0.output(self._last_valid_accepting, 0)
        READ_0.output(self._pop_infifo, 0)
        READ_0.output(self._inc_fiber_addr, 0)
        READ_0.output(self._clr_fiber_addr, 0)
        READ_0.output(self._inc_rep, 0)
        READ_0.output(self._clr_rep, 0)
        READ_0.output(self._data_to_fifo, kts.const(0, 16))
        READ_0.output(self._en_reg_data_in, 0)
        READ_0.output(self._pos_out_to_fifo, kts.const(0, 16))

        #######
        # READ_1 - TODO - Generate general hardware...
        #######
        READ_1.output(self._valid_inc, 0)
        READ_1.output(self._valid_rst, 0)
        READ_1.output(self._ren, 1)
        READ_1.output(self._fifo_push, 0)
        READ_1.output(self._tag_eos, 0)
        READ_1.output(self._addr_out, self._pos_addr + 1)
        READ_1.output(self._next_seq_length, kts.const(2 ** 16 - 1, 16))
        READ_1.output(self._update_seq_state, 0)
        READ_1.output(self._last_valid_accepting, 0)
        READ_1.output(self._pop_infifo, 0)
        READ_1.output(self._inc_fiber_addr, 0)
        READ_1.output(self._clr_fiber_addr, 0)
        READ_1.output(self._inc_rep, 0)
        READ_1.output(self._clr_rep, 0)
        READ_1.output(self._data_to_fifo, kts.const(0, 16))
        READ_1.output(self._en_reg_data_in, 0)
        READ_1.output(self._pos_out_to_fifo, kts.const(0, 16))

        #######
        # READ_2 - TODO - Generate general hardware...
        #######
        READ_2.output(self._valid_inc, 0)
        READ_2.output(self._valid_rst, 0)
        READ_2.output(self._ren, 0)
        READ_2.output(self._fifo_push, 0)
        READ_2.output(self._tag_eos, 0)
        # Don't increment here - only increment after seeing the second one
        # READ_2.output(self._inc_out_dim_addr, 0)
        READ_2.output(self._addr_out, kts.const(0, 16))
        READ_2.output(self._next_seq_length, self._seq_length_ptr_math)
        READ_2.output(self._update_seq_state, 1)
        READ_2.output(self._last_valid_accepting, 0)
        READ_2.output(self._pop_infifo, 0)
        READ_2.output(self._inc_fiber_addr, 0)
        READ_2.output(self._clr_fiber_addr, 0)
        READ_2.output(self._inc_rep, 0)
        READ_2.output(self._clr_rep, 0)
        READ_2.output(self._data_to_fifo, kts.const(0, 16))
        READ_2.output(self._en_reg_data_in, 0)
        READ_2.output(self._pos_out_to_fifo, kts.const(0, 16))

        #######
        # SEQ_START - TODO - Generate general hardware...
        #######
        SEQ_START.output(self._valid_rst, 0)
        SEQ_START.output(self._valid_inc, 0)
        SEQ_START.output(self._ren, 0)
        SEQ_START.output(self._fifo_push, self._seq_length == kts.const(2 ** 16 - 1, 16))
        SEQ_START.output(self._tag_eos, self._seq_length == kts.const(2 ** 16 - 1, 16))
        SEQ_START.output(self._addr_out, kts.const(0, 16))
        SEQ_START.output(self._next_seq_length, kts.const(0, 16))
        SEQ_START.output(self._update_seq_state, 0)
        SEQ_START.output(self._last_valid_accepting, 0)
        SEQ_START.output(self._pop_infifo, 0)
        SEQ_START.output(self._inc_fiber_addr, 0)
        SEQ_START.output(self._clr_fiber_addr, 0)
        SEQ_START.output(self._inc_rep, 0)
        SEQ_START.output(self._clr_rep, 0)
        SEQ_START.output(self._data_to_fifo, kts.const(0, 16))
        SEQ_START.output(self._en_reg_data_in, 0)
        SEQ_START.output(self._pos_out_to_fifo, kts.const(0, 16))

        #############
        # SEQ_ITER
        #############
        SEQ_ITER.output(self._valid_inc, self._valid_in & (~self._fifo_full))
        SEQ_ITER.output(self._valid_rst, 0)
        SEQ_ITER.output(self._ren, ~self._join_almost_full)
        SEQ_ITER.output(self._fifo_push, self._valid_in & (~self._fifo_full))
        SEQ_ITER.output(self._tag_eos, 0)
        SEQ_ITER.output(self._addr_out, self._fiber_addr)
        SEQ_ITER.output(self._next_seq_length, kts.const(0, 16))
        SEQ_ITER.output(self._update_seq_state, 0)
        SEQ_ITER.output(self._last_valid_accepting, (self._valid_cnt == self._seq_length) & (self._valid_in))
        SEQ_ITER.output(self._pop_infifo, 0)
        SEQ_ITER.output(self._inc_fiber_addr, ~self._join_almost_full)
        SEQ_ITER.output(self._clr_fiber_addr, 0)
        SEQ_ITER.output(self._inc_rep, 0)
        SEQ_ITER.output(self._clr_rep, 0)
        SEQ_ITER.output(self._data_to_fifo, self._data_in)
        SEQ_ITER.output(self._en_reg_data_in, 0)
        # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe
        SEQ_ITER.output(self._pos_out_to_fifo, self._agen_addr_d1)

        #############
        # SEQ_DONE
        #############
        SEQ_DONE.output(self._valid_inc, 0)
        SEQ_DONE.output(self._valid_rst, 1)
        SEQ_DONE.output(self._ren, 0)
        SEQ_DONE.output(self._fifo_push, 1)
        SEQ_DONE.output(self._tag_eos, 1)
        SEQ_DONE.output(self._addr_out, kts.const(0, 16))
        SEQ_DONE.output(self._next_seq_length, kts.const(0, 16))
        SEQ_DONE.output(self._update_seq_state, 0)
        # SEQ_DONE.output(self._step_agen, 0)
        SEQ_DONE.output(self._last_valid_accepting, 0)
        SEQ_DONE.output(self._pop_infifo, ~self._root)
        SEQ_DONE.output(self._inc_fiber_addr, 0)
        # Make sure to clear the fiber addr
        SEQ_DONE.output(self._clr_fiber_addr, 1)
        SEQ_DONE.output(self._inc_rep, 0)
        SEQ_DONE.output(self._clr_rep, 0)
        SEQ_DONE.output(self._data_to_fifo, self._stop_lvl)
        SEQ_DONE.output(self._en_reg_data_in, 0)
        SEQ_DONE.output(self._pos_out_to_fifo, self._stop_lvl)
        # SEQ_DONE.output(self._step_outer, 1)
        # SEQ_DONE.output(self._update_previous_outer, 0)

        #############
        # REP_INNER_PRE
        #############
        REP_INNER_PRE.output(self._valid_inc, 0)
        REP_INNER_PRE.output(self._valid_rst, 0)
        REP_INNER_PRE.output(self._ren, 0)
        REP_INNER_PRE.output(self._fifo_push, 0)
        REP_INNER_PRE.output(self._tag_eos, 0)
        REP_INNER_PRE.output(self._addr_out, kts.const(0, 16))
        REP_INNER_PRE.output(self._next_seq_length, kts.const(0, 16))
        REP_INNER_PRE.output(self._update_seq_state, 0)
        REP_INNER_PRE.output(self._last_valid_accepting, 0)
        REP_INNER_PRE.output(self._pop_infifo, 0)
        REP_INNER_PRE.output(self._inc_fiber_addr, 0)
        REP_INNER_PRE.output(self._clr_fiber_addr, 0)
        REP_INNER_PRE.output(self._inc_rep, 0)
        REP_INNER_PRE.output(self._clr_rep, 0)
        REP_INNER_PRE.output(self._data_to_fifo, kts.const(0, 16))
        REP_INNER_PRE.output(self._en_reg_data_in, 1)
        REP_INNER_PRE.output(self._pos_out_to_fifo, kts.const(0, 16))

        #############
        # REP_INNER
        #############
        REP_INNER.output(self._valid_inc, 0)
        REP_INNER.output(self._valid_rst, 0)
        REP_INNER.output(self._ren, 0)
        REP_INNER.output(self._fifo_push, 1)
        REP_INNER.output(self._tag_eos, 0)
        REP_INNER.output(self._addr_out, kts.const(0, 16))
        REP_INNER.output(self._next_seq_length, kts.const(0, 16))
        REP_INNER.output(self._update_seq_state, 0)
        REP_INNER.output(self._last_valid_accepting, 0)
        REP_INNER.output(self._pop_infifo, 0)
        REP_INNER.output(self._inc_fiber_addr, 0)
        REP_INNER.output(self._clr_fiber_addr, 0)
        REP_INNER.output(self._inc_rep, ~self._fifo_full)
        REP_INNER.output(self._clr_rep, 0)
        REP_INNER.output(self._data_to_fifo, self._data_in_d1)
        REP_INNER.output(self._en_reg_data_in, 0)
        # Capture the address used to read as the position
        REP_INNER.output(self._pos_out_to_fifo, self._agen_addr_d1_cap)

        #############
        # REP_OUTER
        #############
        REP_OUTER.output(self._valid_inc, 0)
        REP_OUTER.output(self._valid_rst, 1)
        REP_OUTER.output(self._ren, 0)
        REP_OUTER.output(self._fifo_push, 0)
        REP_OUTER.output(self._tag_eos, 0)
        REP_OUTER.output(self._addr_out, kts.const(0, 16))
        REP_OUTER.output(self._next_seq_length, kts.const(0, 16))
        REP_OUTER.output(self._update_seq_state, 0)
        REP_OUTER.output(self._last_valid_accepting, 0)
        REP_OUTER.output(self._pop_infifo, 0)
        REP_OUTER.output(self._inc_fiber_addr, 0)
        REP_OUTER.output(self._clr_fiber_addr, 1)
        REP_OUTER.output(self._inc_rep, 1)
        REP_OUTER.output(self._clr_rep, 0)
        REP_OUTER.output(self._data_to_fifo, kts.const(0, 16))
        REP_OUTER.output(self._en_reg_data_in, 0)
        REP_OUTER.output(self._pos_out_to_fifo, kts.const(0, 16))

        #############
        # REP_STOP
        #############
        # Since we will escape SEQ_ITER every time there is a read, we will need to increment it here
        REP_STOP.output(self._valid_inc, ~self._repeat_outer_inner_n & ~self._fifo_full)
        REP_STOP.output(self._valid_rst, self._repeat_outer_inner_n)
        REP_STOP.output(self._ren, 0)
        REP_STOP.output(self._fifo_push, 1)
        REP_STOP.output(self._tag_eos, 1)
        REP_STOP.output(self._addr_out, kts.const(0, 16))
        REP_STOP.output(self._next_seq_length, kts.const(0, 16))
        REP_STOP.output(self._update_seq_state, 0)
        # If we are on the last one, this gets us to the end
        REP_STOP.output(self._last_valid_accepting, (self._valid_cnt == self._seq_length) & ~self._repeat_outer_inner_n)
        REP_STOP.output(self._pop_infifo, 0)
        REP_STOP.output(self._inc_fiber_addr, 0)
        REP_STOP.output(self._clr_fiber_addr, 0)
        REP_STOP.output(self._inc_rep, 0)
        REP_STOP.output(self._clr_rep, ~self._repeat_outer_inner_n)
        REP_STOP.output(self._data_to_fifo, kts.const(1, 16))
        REP_STOP.output(self._en_reg_data_in, 0)
        REP_STOP.output(self._pos_out_to_fifo, kts.const(1, 16))

        #############
        # DONE
        #############
        DONE.output(self._valid_inc, 0)
        DONE.output(self._valid_rst, 0)
        DONE.output(self._ren, 0)
        DONE.output(self._fifo_push, 0)
        DONE.output(self._tag_eos, 0)
        DONE.output(self._addr_out, kts.const(0, 16))
        DONE.output(self._next_seq_length, kts.const(0, 16))
        DONE.output(self._update_seq_state, 0)
        DONE.output(self._last_valid_accepting, 0)
        DONE.output(self._pop_infifo, 0)
        DONE.output(self._inc_fiber_addr, 0)
        DONE.output(self._clr_fiber_addr, 0)
        DONE.output(self._inc_rep, 0)
        DONE.output(self._clr_rep, 0)
        DONE.output(self._data_to_fifo, kts.const(0, 16))
        DONE.output(self._en_reg_data_in, 0)
        DONE.output(self._pos_out_to_fifo, kts.const(0, 16))

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

    def get_bitstream(self, inner_offset, max_out, ranges, strides, root, do_repeat=0, repeat_outer=0, repeat_factor=0, stop_lvl=0):

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Store all configurations here
        config = [
            ("inner_dim_offset", inner_offset),
            # ("max_outer_dim", max_out),
            ("do_repeat", do_repeat),
            ("repeat_outer_inner_n", repeat_outer),
            ("repeat_factor", repeat_factor),
            ("stop_lvl", stop_lvl)]

        if root:
            dim = len(ranges)
            tform_ranges, tform_strides = transform_strides_and_ranges(ranges=ranges,
                                                                       strides=strides,
                                                                       dimensionality=dim)
            # for i in range(dim):
            #     config += [("fiber_outer_iter_dimensionality", dim)]
            #     config += [(f"fiber_outer_iter_ranges_{i}", tform_ranges[i])]
            #     config += [(f"fiber_outer_addr_strides_{i}", tform_strides[i])]
            #     config += [("fiber_outer_addr_starting_addr", 0)]

        return trim_config_list(flattened, config)


if __name__ == "__main__":
    scanner_dut = WriteScanner(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(scanner_dut, filename="write_scanner.sv",
            optimize_if=False)
