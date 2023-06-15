import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import add_counter, safe_wire, register, intercept_cfg, observe_cfg, transform_strides_and_ranges
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class SIPO(Generator):
    def __init__(self,
                 data_width=16,
                 fetch_width=4):

        super().__init__("sipo", debug=True)

        self.data_width = data_width
        self.fetch_width = fetch_width
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

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # Scanner interface will need
        # input data, input valid
        # output address, output valid
        self._data_in = self.input("data_in", self.data_width)
        self._data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_in = self.input("valid_in", 1)
        self._valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ready_in = self.input("ready_in", 1)
        self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ready_out = self.output("ready_out", 1)
        self._ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._coord_out = self.output("coord_out", self.data_width)
        self._coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._outer_coord_out = self.output("outer_coord_out", self.data_width)
        self._outer_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._pos_out = self.output("pos_out", self.data_width)
        self._pos_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_out = self.output("valid_out", 1)
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._addr_out = self.output("addr_out", self.data_width)
        self._addr_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._eos_out = self.output("eos_out", 1)
        self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Intermediate for typing...
        self._ren = self.var("ren", 1)

        # Point to the row in storage for data recovery
        self._payload_ptr = self.output("payload_ptr", 16)
        self._payload_ptr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

# ==========================================
# Generate addresses to scan over fiber...
# ==========================================

        self._step_agen = self.var("step_agen", 1)

        # Create read address generator
        self.FIBER_READ_ITER = ForLoop(iterator_support=2,
                                       config_width=16)
        self.FIBER_READ_ADDR = AddrGen(iterator_support=2,
                                       config_width=16)

        self.add_child(f"fiber_read_iter",
                       self.FIBER_READ_ITER,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       step=self._step_agen)

        # Whatever comes through here should hopefully just pipe through seamlessly
        # addressor modules

        self.add_child(f"fiber_read_addr",
                       self.FIBER_READ_ADDR,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       step=self._step_agen,
                       mux_sel=self.FIBER_READ_ITER.ports.mux_sel_out,
                       restart=self.FIBER_READ_ITER.ports.restart)
        self._agen_addr = self.var("agen_addr", 16)
        safe_wire(self, self._agen_addr, self.FIBER_READ_ADDR.ports.addr_out)

        self._iter_restart = self.var("iter_restart", 1)
        self.wire(self._iter_restart, self.FIBER_READ_ITER.ports.restart)

# ==========================================
# Generate addresses to scan over outer positions...
# ==========================================

        self._step_outer = self.var("step_outer", 1)

        # Create read address generator
        self.FIBER_OUTER_ITER = ForLoop(iterator_support=2,
                                        config_width=16)
        self.FIBER_OUTER_ADDR = AddrGen(iterator_support=2,
                                        config_width=16)

        self.add_child(f"fiber_outer_iter",
                       self.FIBER_OUTER_ITER,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       step=self._step_outer)

        # Whatever comes through here should hopefully just pipe through seamlessly
        # addressor modules

        self.add_child(f"fiber_outer_addr",
                       self.FIBER_OUTER_ADDR,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       step=self._step_outer,
                       mux_sel=self.FIBER_OUTER_ITER.ports.mux_sel_out,
                       restart=self.FIBER_OUTER_ITER.ports.restart)
        self._outer_addr = self.var("outer_addr", 16)
        safe_wire(self, self._outer_addr, self.FIBER_OUTER_ADDR.ports.addr_out)

        self._outer_restart = self.var("outer_restart", 1)
        self.wire(self._outer_restart, self.FIBER_OUTER_ITER.ports.restart)

        self._update_previous_outer = self.var("update_previous_outer", 1)
        self._previous_outer = self.var("previous_outer", 16)
        self._previous_outer_valid = self.var("previous_outer_valid", 1)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def prev_outer_ff():
            if ~self._rst_n:
                self._previous_outer = 0
                self._previous_outer_valid = 0
            elif self._update_previous_outer:
                self._previous_outer = self._outer_addr
                self._previous_outer_valid = 1
        self.add_code(prev_outer_ff)


# =============================
# Input FIFO
#
# To leverage hierarchical intersection, we need to be
# able to accept an incoming stream from the output of a previous
# level. Root-level scanners drive the processing, whereas non-root-level
# scanners are driven by upper levels
# =============================

        self._infifo = RegFIFO(data_width=2 * self.data_width + 2, width_mult=1, depth=8)

        # Need to know if we've seen eos_in
        self._eos_in_seen = self.var("eos_in_seen", 1)

        self._fifo_push = self.var("fifo_push", 1)
        self._tag_valid_data = self.var("tag_valid_data", 1)
        self._tag_eos = self.var("tag_eos", 1)
        self._last_valid_accepting = self.var("last_valid_accepting", 1)
        self._fifo_full = self.var("fifo_full", 1)
        # Gate ready after last read in the stream
        self._ready_gate = self.var("ready_gate", 1)

        # For input streams, need coord_in, valid_in, eos_in
        self._upstream_pos_in = self.input("us_pos_in", self.data_width)
        self._upstream_pos_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._upstream_coord_in = self.output("us_coord_in", self.data_width)
        self._upstream_coord_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._upstream_valid_in = self.input("us_valid_in", 1)
        self._upstream_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._upstream_eos_in = self.input("us_eos_in", 1)
        self._upstream_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._upstream_ready_out = self.output("us_ready_out", 1)
        self._upstream_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # For input streams, need coord_in, valid_in, eos_in
        self._infifo_pos_in = self.var("infifo_pos_in", self.data_width)
        self._infifo_coord_in = self.var("infifo_coord_in", self.data_width)
        self._infifo_valid_in = self.var("infifo_valid_in", 1)
        self._infifo_eos_in = self.var("infifo_eos_in", 1)

        # Stupid convert
        self._pos_in_us_packed = self.var("fifo_us_in_packed", 2 * self.data_width + 2, packed=True)

        # indicate valid data as well
        self.wire(self._pos_in_us_packed[2 * self.data_width + 2 - 1, self.data_width + 2], self._upstream_coord_in)
        self.wire(self._pos_in_us_packed[self.data_width + 1], self._upstream_valid_in)
        # The EOS tags on the last valid in the stream
        self.wire(self._pos_in_us_packed[self.data_width], self._upstream_eos_in)
        self.wire(self._pos_in_us_packed[self.data_width - 1, 0], self._upstream_pos_in)

        self._data_out_us_packed = self.var("fifo_out_us_packed", 2 * self.data_width + 2, packed=True)
        self.wire(self._infifo_coord_in, self._data_out_us_packed[2 * self.data_width + 2 - 1, self.data_width + 2])
        self.wire(self._infifo_valid_in, self._data_out_us_packed[self.data_width + 1])
        self.wire(self._infifo_eos_in, self._data_out_us_packed[self.data_width])
        self.wire(self._infifo_pos_in, self._data_out_us_packed[self.data_width - 1, 0])

        # self._input_fifo_pop = self.var("input_fifo_pop", 1)
        # self._fifo_us_valid_entry = self.var("fifo_us_valid_entry", 1)
        self._fifo_us_full = self.var("fifo_us_full", 1)

        self.add_child(f"input_fifo",
                       self._infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=(self._upstream_valid_in | self._upstream_eos_in) & ~self._fifo_us_full,
                       pop=self._step_outer,
                       data_in=self._pos_in_us_packed,
                       data_out=self._data_out_us_packed)

        # self.wire(self._ren, (~self._rfifo.ports.almost_full & ~self._ready_gate) )
        self.wire(self._fifo_us_full, self._infifo.ports.full)
        self.wire(self._upstream_ready_out, ~self._fifo_us_full)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def eos_seen_ff():
            if ~self._rst_n:
                self._eos_in_seen = 0
            elif self._infifo_eos_in:
                self._eos_in_seen = 1
        self.add_code(eos_seen_ff)

# =============================
# SCAN FSM
# =============================

        # Contains the address as the position for the ptr array
        self._pos_addr = self.var("pos_addr", self.data_width)

        self._iter_dim = intercept_cfg(self.FIBER_READ_ITER, "dimensionality")
        self._iter_ranges = intercept_cfg(self.FIBER_READ_ITER, "ranges")
        self._addr_strides = intercept_cfg(self.FIBER_READ_ADDR, "strides")
        self._addr_offset = intercept_cfg(self.FIBER_READ_ADDR, "starting_addr")

        self._valid_inc = self.var("valid_inc", 1)
        self._valid_rst = self.var("valid_rst", 1)
        self._valid_cnt = self.var("valid_cnt", 16)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def valid_cnt():
            if ~self._rst_n:
                self._valid_cnt = 0
            elif self._valid_rst:
                self._valid_cnt = 0
            elif self._valid_inc:
                self._valid_cnt = self._valid_cnt + 1
        self.add_code(valid_cnt)

        self._inner_dim_offset = self.input("inner_dim_offset", 16)
        self._inner_dim_offset.add_attribute(ConfigRegAttr("Memory address of the offset..."))
        # self._coord_in = self.var("coord_in", 8)
        self._ptr_in = self.var("ptr_in", self.data_width)
        self.wire(self._ptr_in, self._data_in)
        # self.wire(self._ptr_in, self._data_in[15, 8])

        # Need to hook up the current and next seqence_length
        # Also need to intercept the configs and set them internally...
        self._ptr_reg = register(self, self._ptr_in)
        self._agen_addr_d1 = register(self, self._agen_addr)
        self._pos_out_to_fifo = self.var("pos_out_to_fifo", self.data_width)
        # In this way, we can save the relative position of each value in the ptr array for downstream
        self.wire(self._pos_out_to_fifo, (self._agen_addr_d1 - self._inner_dim_offset))

        self._update_seq_state = self.var("update_seq_state", 1)
        self._seq_length = self.var("seq_length", 16)
        self._seq_addr = self.var("seq_addr", 16)
        self._next_seq_length = self.var("next_seq_length", 16)
        self._next_seq_addr = self.var("next_seq_addr", 16)

        # State of the address gens
        self.wire(self._iter_dim, 1)
        self.wire(self._iter_ranges[0], self._seq_length - 1)
        self.wire(self._iter_ranges[1], 0)
        self.wire(self._addr_strides[0], 1)
        self.wire(self._addr_strides[1], 0)
        # This offset is needed to get info into the next structure
        self.wire(self._addr_offset, self._seq_addr)

        self._seq_length_ptr_math = self.var("seq_length_ptr_math", self.data_width)
        # self.wire(self._seq_length_ptr_math[7, 0], self._ptr_in - self._ptr_reg - 1)
        # self.wire(self._seq_length_ptr_math[15, 8], 0)
        self.wire(self._seq_length_ptr_math, self._ptr_in - self._ptr_reg - 1)

        self.wire(self._pos_addr, kts.ternary(self._root, self._outer_addr, self._infifo_pos_in))

        # On the first read, we locate the base offset addr, then on the
        # second we get the length as the subtraction of the pointers
        # self.wire(self._next_seq_length, self._ptr_in - self._ptr_reg - 1)
        # The memory address is the pointer offset + base register
        self.wire(self._next_seq_addr, self._ptr_reg + self._inner_dim_offset)

        self._outer_length_one = self.var("outer_length_one", 1)

        self._outer_dim = self.var("outer_dim", self.FIBER_OUTER_ITER.ports.dimensionality.width)
        self._outer_range = self.var("outer_range", self.FIBER_OUTER_ITER.ports.ranges[0].width,
                                     size=2,
                                     explicit_array=True,
                                     packed=True)

        observe_cfg(self, self._outer_dim.name, self.FIBER_OUTER_ITER, "dimensionality")
        observe_cfg(self, self._outer_range.name, self.FIBER_OUTER_ITER, "ranges")

        self.wire(self._outer_length_one,
                  ((self._outer_dim == 1) &
                   (self._outer_range[0] == kts.const(2 ** 16 - 1, 16))))

        # Hold state for iterator - just length
        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def update_seq_state_ff():
            if ~self._rst_n:
                self._seq_length = 0
                self._seq_addr = 0
                self._payload_ptr = 0
            elif self._update_seq_state:
                self._seq_length = self._next_seq_length
                self._seq_addr = self._next_seq_addr
                # Output this for use in the intersection engine
                self._payload_ptr = self._ptr_reg
        self.add_code(update_seq_state_ff)

        self._inc_out_dim_addr = self.var("inc_out_dim_addr", 1)
        self._out_dim_addr = add_counter(self, "out_dim_addr", 16, self._inc_out_dim_addr)

        self._inc_out_dim_x = self.var("inc_out_dim_x", 1)
        self._out_dim_x = add_counter(self, "out_dim_x", 16, self._inc_out_dim_x)
        self._max_outer_dim = self.input("max_outer_dim", 16)
        self._max_outer_dim.add_attribute(ConfigRegAttr("How long is the matrix..."))

        self._rfifo = RegFIFO(data_width=3 * self.data_width + 2, width_mult=1, depth=8)

        self._fifo_push = self.var("fifo_push", 1)
        self._tag_valid_data = self.var("tag_valid_data", 1)
        self._tag_eos = self.var("tag_eos", 1)
        self._last_valid_accepting = self.var("last_valid_accepting", 1)
        self._fifo_full = self.var("fifo_full", 1)
        # Gate ready after last read in the stream
        self._ready_gate = self.var("ready_gate", 1)

        # Create FSM
        self.scan_fsm = self.add_fsm("scan_seq", reset_high=False)
        START = self.scan_fsm.add_state("START")
        ISSUE_STRM = self.scan_fsm.add_state("ISSUE_STRM")
        # Non-root dispatch state for separation
        ISSUE_STRM_NR = self.scan_fsm.add_state("ISSUE_STRM_NR")
        READ_0 = self.scan_fsm.add_state("READ_0")
        READ_1 = self.scan_fsm.add_state("READ_1")
        READ_2 = self.scan_fsm.add_state("READ_2")

        SEQ_START = self.scan_fsm.add_state("SEQ_START")
        SEQ_ITER = self.scan_fsm.add_state("SEQ_ITER")
        SEQ_DONE = self.scan_fsm.add_state("SEQ_DONE")

        DONE = self.scan_fsm.add_state("DONE")

        self.scan_fsm.output(self._valid_inc)
        self.scan_fsm.output(self._valid_rst)
        self.scan_fsm.output(self._ren)
        self.scan_fsm.output(self._fifo_push)
        self.scan_fsm.output(self._tag_valid_data)
        self.scan_fsm.output(self._tag_eos)
        self.scan_fsm.output(self._inc_out_dim_x)
        self.scan_fsm.output(self._inc_out_dim_addr)
        self.scan_fsm.output(self._addr_out)
        self.scan_fsm.output(self._next_seq_length)
        self.scan_fsm.output(self._update_seq_state)
        self.scan_fsm.output(self._step_agen)
        self.scan_fsm.output(self._last_valid_accepting)
        self.scan_fsm.output(self._step_outer)
        self.scan_fsm.output(self._update_previous_outer)
        # self.scan_fsm.output(self._input_fifo_pop)

        ####################
        # Next State Logic
        ####################

        # Dummy state for eventual filling block.
        START.next(ISSUE_STRM, self._root)
        START.next(ISSUE_STRM_NR, ~self._root)

        # Completely done at this point
        # ISSUE_STRM.next(DONE, self._out_dim_x == self._max_outer_dim)
        # If not done, we have to issue more streams
        ISSUE_STRM.next(SEQ_START, (self._outer_addr == self._previous_outer) & self._previous_outer_valid)
        ISSUE_STRM.next(READ_0, ~((self._outer_addr == self._previous_outer) & self._previous_outer_valid))

        # If we are seeing the eos_in this state we are done
        ISSUE_STRM_NR.next(DONE, self._infifo_eos_in & ~self._infifo_valid_in)
        ISSUE_STRM_NR.next(READ_0, self._infifo_valid_in)

        # Can continue on our way in root mode, otherwise we need to
        # bring the pointer up to locate the coordinate
        READ_0.next(READ_1, kts.const(1, 1))

        # If you're not at the right coordinate yet, go straight to SEQ_START and emit length 0 sequence
        # READ_1.next(SEQ_START, (self._coord_in > self._outer_addr) & self._root)
        # # Otherwise, proceed
        # READ_1.next(READ_2, (self._coord_in <= self._outer_addr) & self._root)
        # READ_1.next(READ_2, self._root)

        # # If not root, we should go back to READ_0 while the input coordinate is smaller than the fifo coord
        # READ_1.next(READ_0, (self._coord_in < self._infifo_coord_in) & ~self._root)
        # # Otherwise, proceed
        # READ_1.next(READ_2, (self._coord_in >= self._infifo_coord_in) & ~self._root)
        READ_1.next(READ_2, kts.const(1, 1))

        # Go to this state to see the length, will also have EOS?
        READ_2.next(SEQ_START, kts.const(1, 1))

        # At Start, we can either immediately end the stream or go to the iteration phase
        SEQ_START.next(SEQ_DONE, self._seq_length == kts.const(2 ** 16 - 1, 16))
        SEQ_START.next(SEQ_ITER, const(1, 1))

        # In ITER, we go back to idle when the fifo is full to avoid
        # complexity, or if we are looking at one of the eos since we can make the last
        # move for the intersection now...
        # If we have eos and can push to the fifo, we are done
        SEQ_ITER.next(SEQ_DONE, self._last_valid_accepting)
        SEQ_ITER.next(SEQ_ITER, kts.const(1, 1))

        # Once done, we need another flush
        SEQ_DONE.next(DONE, ((self._outer_restart | self._outer_length_one) & self._root) | (self._eos_in_seen))
        SEQ_DONE.next(ISSUE_STRM, ~self._outer_restart & self._root)
        SEQ_DONE.next(ISSUE_STRM_NR, ~self._outer_restart & ~self._root)

        # DONE
        DONE.next(DONE, kts.const(1, 1))

        ####################
        # FSM Output Logic
        ####################

        #######
        # START - TODO - Generate general hardware...
        #######
        START.output(self._valid_inc, 0)
        START.output(self._valid_rst, 0)
        START.output(self._ren, 0)
        START.output(self._step_agen, 0)
        START.output(self._fifo_push, 0)
        START.output(self._tag_valid_data, 0)
        START.output(self._tag_eos, 0)
        START.output(self._inc_out_dim_x, 0)
        START.output(self._inc_out_dim_addr, 0)
        START.output(self._addr_out, kts.const(0, 16))
        START.output(self._next_seq_length, kts.const(0, 16))
        START.output(self._update_seq_state, 0)
        START.output(self._last_valid_accepting, 0)
        START.output(self._step_outer, 0)
        START.output(self._update_previous_outer, 0)

        #######
        # ISSUE_STRM - TODO - Generate general hardware...
        #######
        ISSUE_STRM.output(self._valid_inc, 0)
        ISSUE_STRM.output(self._valid_rst, 0)
        ISSUE_STRM.output(self._ren, 0)
        ISSUE_STRM.output(self._fifo_push, 0)
        ISSUE_STRM.output(self._tag_valid_data, 0)
        ISSUE_STRM.output(self._tag_eos, 0)
        ISSUE_STRM.output(self._inc_out_dim_x, 0)
        # Only increment if we are seeing a new address and the most recent stream wasn't 0 length
        ISSUE_STRM.output(self._inc_out_dim_addr, ((self._outer_addr != self._previous_outer) & self._previous_outer_valid) & ~(self._seq_length == kts.const(2 ** 16 - 1, 16)))
        ISSUE_STRM.output(self._addr_out, kts.const(0, 16))
        ISSUE_STRM.output(self._next_seq_length, kts.const(0, 16))
        ISSUE_STRM.output(self._update_seq_state, 0)
        ISSUE_STRM.output(self._step_agen, 0)
        ISSUE_STRM.output(self._last_valid_accepting, 0)
        ISSUE_STRM.output(self._step_outer, 0)
        ISSUE_STRM.output(self._update_previous_outer, 1)

        #######
        # ISSUE_STRM_NR
        #######
        ISSUE_STRM_NR.output(self._valid_inc, 0)
        ISSUE_STRM_NR.output(self._valid_rst, 0)
        ISSUE_STRM_NR.output(self._ren, 0)
        ISSUE_STRM_NR.output(self._fifo_push, 0)
        ISSUE_STRM_NR.output(self._tag_valid_data, 0)
        ISSUE_STRM_NR.output(self._tag_eos, 0)
        ISSUE_STRM_NR.output(self._inc_out_dim_x, 0)
        # Only increment if we are seeing a new address and the most recent stream wasn't 0 length
        ISSUE_STRM_NR.output(self._inc_out_dim_addr, 0)
        ISSUE_STRM_NR.output(self._addr_out, kts.const(0, 16))
        ISSUE_STRM_NR.output(self._next_seq_length, kts.const(0, 16))
        ISSUE_STRM_NR.output(self._update_seq_state, 0)
        ISSUE_STRM_NR.output(self._step_agen, 0)
        ISSUE_STRM_NR.output(self._last_valid_accepting, 0)
        ISSUE_STRM_NR.output(self._step_outer, self._infifo_eos_in & ~self._infifo_valid_in)
        ISSUE_STRM_NR.output(self._update_previous_outer, 0)

        #######
        # READ_0 - TODO - Generate general hardware...
        #######
        READ_0.output(self._valid_inc, 0)
        READ_0.output(self._valid_rst, 0)
        READ_0.output(self._ren, 1)
        READ_0.output(self._fifo_push, 0)
        READ_0.output(self._tag_valid_data, 0)
        READ_0.output(self._tag_eos, 0)
        READ_0.output(self._inc_out_dim_x, 0)
        READ_0.output(self._inc_out_dim_addr, 0)
        # READ_0.output(self._addr_out, self._out_dim_addr)
        READ_0.output(self._addr_out, self._pos_addr)
        READ_0.output(self._next_seq_length, kts.const(0, 16))
        READ_0.output(self._update_seq_state, 0)
        READ_0.output(self._step_agen, 0)
        READ_0.output(self._last_valid_accepting, 0)
        READ_0.output(self._step_outer, 0)
        READ_0.output(self._update_previous_outer, 0)

        #######
        # READ_1 - TODO - Generate general hardware...
        #######
        READ_1.output(self._valid_inc, 0)
        READ_1.output(self._valid_rst, 0)
        READ_1.output(self._ren, 1)
        READ_1.output(self._fifo_push, 0)
        READ_1.output(self._tag_valid_data, 0)
        READ_1.output(self._tag_eos, 0)
        READ_1.output(self._inc_out_dim_x, 0)
        READ_1.output(self._inc_out_dim_addr, 0)
        READ_1.output(self._addr_out, self._pos_addr + 1)
        READ_1.output(self._next_seq_length, kts.const(2 ** 16 - 1, 16))
        READ_1.output(self._update_seq_state, 0)
        READ_1.output(self._step_agen, 0)
        READ_1.output(self._last_valid_accepting, 0)
        READ_1.output(self._step_outer, 0)
        READ_1.output(self._update_previous_outer, 0)

        #######
        # READ_2 - TODO - Generate general hardware...
        #######
        READ_2.output(self._valid_inc, 0)
        READ_2.output(self._valid_rst, 0)
        READ_2.output(self._ren, 0)
        READ_2.output(self._fifo_push, 0)
        READ_2.output(self._tag_valid_data, 0)
        READ_2.output(self._tag_eos, 0)
        READ_2.output(self._inc_out_dim_x, 0)
        # Don't increment here - only increment after seeing the second one
        READ_2.output(self._inc_out_dim_addr, 0)
        READ_2.output(self._addr_out, kts.const(0, 16))
        READ_2.output(self._next_seq_length, self._seq_length_ptr_math)
        READ_2.output(self._update_seq_state, 1)
        READ_2.output(self._step_agen, 0)
        READ_2.output(self._last_valid_accepting, 0)
        READ_2.output(self._step_outer, 0)
        READ_2.output(self._update_previous_outer, 0)

        #######
        # SEQ_START - TODO - Generate general hardware...
        #######
        SEQ_START.output(self._valid_rst, 0)
        SEQ_START.output(self._valid_inc, 0)
        SEQ_START.output(self._ren, 0)
        SEQ_START.output(self._fifo_push, self._seq_length == kts.const(2 ** 16 - 1, 16))
        SEQ_START.output(self._tag_valid_data, 0)
        SEQ_START.output(self._tag_eos, self._seq_length == kts.const(2 ** 16 - 1, 16))
        SEQ_START.output(self._inc_out_dim_x, 0)
        SEQ_START.output(self._inc_out_dim_addr, 0)
        SEQ_START.output(self._addr_out, kts.const(0, 16))
        SEQ_START.output(self._next_seq_length, kts.const(0, 16))
        SEQ_START.output(self._update_seq_state, 0)
        SEQ_START.output(self._step_agen, 0)
        SEQ_START.output(self._last_valid_accepting, 0)
        SEQ_START.output(self._step_outer, 0)
        SEQ_START.output(self._update_previous_outer, 0)

        #############
        # SEQ_ITER
        #############
        SEQ_ITER.output(self._valid_inc, self._valid_in & (~self._fifo_full))
        SEQ_ITER.output(self._valid_rst, 0)
        SEQ_ITER.output(self._ren, ~self._rfifo.ports.almost_full & ~self._ready_gate)
        SEQ_ITER.output(self._fifo_push, self._valid_in & (~self._fifo_full))
        SEQ_ITER.output(self._tag_valid_data, self._valid_in)
        SEQ_ITER.output(self._tag_eos, self._last_valid_accepting)
        SEQ_ITER.output(self._inc_out_dim_x, 0)
        SEQ_ITER.output(self._inc_out_dim_addr, 0)
        SEQ_ITER.output(self._addr_out, self._agen_addr)
        SEQ_ITER.output(self._next_seq_length, kts.const(0, 16))
        SEQ_ITER.output(self._update_seq_state, 0)
        SEQ_ITER.output(self._step_agen, ~self._rfifo.ports.almost_full & ~self._ready_gate)
        SEQ_ITER.output(self._last_valid_accepting, (self._valid_cnt == self._seq_length) & (self._valid_in))
        SEQ_ITER.output(self._step_outer, 0)
        SEQ_ITER.output(self._update_previous_outer, 0)

        # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe

        #############
        # SEQ_DONE
        #############
        SEQ_DONE.output(self._valid_inc, 0)
        SEQ_DONE.output(self._valid_rst, 1)
        SEQ_DONE.output(self._ren, 0)
        SEQ_DONE.output(self._fifo_push, 0)
        SEQ_DONE.output(self._tag_valid_data, 0)
        SEQ_DONE.output(self._tag_eos, 0)
        SEQ_DONE.output(self._inc_out_dim_x, 1)
        SEQ_DONE.output(self._inc_out_dim_addr, 0)
        SEQ_DONE.output(self._addr_out, kts.const(0, 16))
        SEQ_DONE.output(self._next_seq_length, kts.const(0, 16))
        SEQ_DONE.output(self._update_seq_state, 0)
        SEQ_DONE.output(self._step_agen, 0)
        SEQ_DONE.output(self._last_valid_accepting, 0)
        SEQ_DONE.output(self._step_outer, 1)
        SEQ_DONE.output(self._update_previous_outer, 0)

        #############
        # DONE
        #############
        DONE.output(self._valid_inc, 0)
        DONE.output(self._valid_rst, 0)
        DONE.output(self._ren, 0)
        DONE.output(self._fifo_push, 0)
        DONE.output(self._tag_valid_data, 0)
        DONE.output(self._tag_eos, 0)
        DONE.output(self._inc_out_dim_x, 0)
        DONE.output(self._inc_out_dim_addr, 0)
        DONE.output(self._addr_out, kts.const(0, 16))
        DONE.output(self._next_seq_length, kts.const(0, 16))
        DONE.output(self._update_seq_state, 0)
        DONE.output(self._step_agen, 0)
        DONE.output(self._last_valid_accepting, 0)
        DONE.output(self._step_outer, 0)
        DONE.output(self._update_previous_outer, 0)

        self.scan_fsm.set_start_state(START)

# ===================================
# Dump metadata into fifo
# ===================================

        # Include the outer coord as well
        self._oc_to_fifo = self.var("outer_coord_to_fifo", self.data_width)
        self.wire(self._oc_to_fifo, kts.ternary(self._root, kts.const(0, self.data_width), self._infifo_coord_in))

        # Stupid convert
        self._data_in_packed = self.var("fifo_in_packed", 3 * self.data_width + 2, packed=True)

        # indicate valid data as well
        self.wire(self._data_in_packed[(3 * self.data_width) - 1 + 2, 2 * self.data_width + 2], self._oc_to_fifo)
        self.wire(self._data_in_packed[(2 * self.data_width) - 1 + 2, self.data_width + 2], self._pos_out_to_fifo)
        self.wire(self._data_in_packed[self.data_width + 1], self._tag_valid_data)
        # The EOS tags on the last valid in the stream
        self.wire(self._data_in_packed[self.data_width], self._tag_eos)
        self.wire(self._data_in_packed[self.data_width - 1, 0], self._data_in)

        self._data_out_packed = self.var("fifo_out_packed", 3 * self.data_width + 2, packed=True)
        # self.wire(self._data_out_packed, kts.concat(self._pos_out, self._valid_out, self._eos_out, self._coord_out))
        self.wire(self._outer_coord_out, self._data_out_packed[3 * self.data_width - 1 + 2, 2 * self.data_width + 2])
        self.wire(self._pos_out, self._data_out_packed[2 * self.data_width - 1 + 2, self.data_width + 2])
        self.wire(self._valid_out, self._data_out_packed[self.data_width + 1])
        self.wire(self._eos_out, self._data_out_packed[self.data_width])
        self.wire(self._coord_out, self._data_out_packed[self.data_width - 1, 0])

        self._fifo_valid_entry = self.var("fifo_valid_entry", 1)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def ready_gate_ff():
            if ~self._rst_n:
                self._ready_gate = 0
            elif self._iter_restart:
                self._ready_gate = 1
            elif self._inc_out_dim_x:
                self._ready_gate = 0
        self.add_code(ready_gate_ff)

        self.add_child(f"coordinate_fifo",
                       self._rfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._fifo_push,
                       pop=(self._fifo_valid_entry & self._ready_in),
                       data_in=self._data_in_packed,
                       data_out=self._data_out_packed,
                       valid=self._fifo_valid_entry)

        # self.wire(self._ren, (~self._rfifo.ports.almost_full & ~self._ready_gate) )
        self.wire(self._ready_out, self._ren)
        self.wire(self._fifo_full, self._rfifo.ports.full)
        # Increment valid count when we receive a valid we can actually push in the FIFO

        # @always_comb
        # def eos_comparison():
        #     self._last_valid_accepting = (self._valid_cnt == self._seq_length) & (self._valid_in)
        #     self._last_valid_accepting = (self._valid_cnt == self._seq_length) & (self._valid_in)
        # self.add_code(eos_comparison)

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

    def get_bitstream(self, inner_offset, max_out, ranges, strides, root):

        # Store all configurations here
        config = [
            ("inner_dim_offset", inner_offset),
            ("max_outer_dim", max_out)]

        if root:
            dim = len(ranges)
            tform_ranges, tform_strides = transform_strides_and_ranges(ranges=ranges,
                                                                       strides=strides,
                                                                       dimensionality=dim)
            for i in range(dim):
                config += [("fiber_outer_iter_dimensionality", dim)]
                config += [(f"fiber_outer_iter_ranges_{i}", tform_ranges[i])]
                config += [(f"fiber_outer_addr_strides_{i}", tform_strides[i])]
                config += [("fiber_outer_addr_starting_addr", 0)]

        return config


if __name__ == "__main__":
    scanner_dut = Scanner(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(scanner_dut, filename="scanner.sv",
            optimize_if=False)
