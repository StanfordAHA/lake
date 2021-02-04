import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import add_counter, safe_wire, register, intercept_cfg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class Scanner(Generator):
    def __init__(self,
                 data_width=16):

        super().__init__("scanner", debug=True)

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

        self._data_out = self.output("data_out", self.data_width)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_out = self.output("valid_out", 1)
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._addr_out = self.output("addr_out", self.data_width)
        self._addr_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._eos_out = self.output("eos_out", 1)
        self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Intermediate for typing...
        self._ren = self.var("ren", 1)

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
# =============================
# SCAN FSM
# =============================

        self._iter_dim = intercept_cfg(self.FIBER_READ_ITER, "dimensionality")
        self._iter_ranges = intercept_cfg(self.FIBER_READ_ITER, "ranges")
        self._addr_strides = intercept_cfg(self.FIBER_READ_ADDR, "strides")
        self._addr_offset = intercept_cfg(self.FIBER_READ_ADDR, "starting_addr")

        self._valid_inc = self.var("valid_inc", 1)
        self._valid_cnt = add_counter(self, "valid_count", 16, self._valid_inc)

        self._inner_dim_offset = self.input("inner_dim_offset", 16)
        self._inner_dim_offset.add_attribute(ConfigRegAttr("Memory address of the offset..."))
        self._coord_in = self.var("coord_in", 8)
        self._ptr_in = self.var("ptr_in", 8)
        self.wire(self._coord_in, self._data_in[7, 0])
        self.wire(self._ptr_in, self._data_in[15, 8])
        self._coord_in = self.var("coord_in", 8)

        # Need to hook up the current and next seqence_length
        # Also need to intercept the configs and set them internally...
        self._ptr_reg = register(self, self._ptr_in)

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
        self.wire(self._addr_offset, self._seq_addr)

        self._seq_length_ptr_math = self.var("seq_length_ptr_math", 16)
        self.wire(self._seq_length_ptr_math[7, 0], self._ptr_in - self._ptr_reg - 1)
        self.wire(self._seq_length_ptr_math[15, 8], 0)

        # On the first read, we locate the base offset addr, then on the
        # second we get the length as the subtraction of the pointers
        # self.wire(self._next_seq_length, self._ptr_in - self._ptr_reg - 1)
        # The memory address is the pointer offset + base register
        self.wire(self._next_seq_addr, self._ptr_reg + self._inner_dim_offset)

        # Hold state for iterator - just length
        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def update_seq_state_ff():
            if ~self._rst_n:
                self._seq_length = 0
                self._seq_addr = 0
            elif self._update_seq_state:
                self._seq_length = self._next_seq_length
                self._seq_addr = self._next_seq_addr
        self.add_code(update_seq_state_ff)

        self._inc_out_dim_addr = self.var("inc_out_dim_addr", 1)
        self._out_dim_addr = add_counter(self, "out_dim_addr", 16, self._inc_out_dim_addr)

        self._inc_out_dim_x = self.var("inc_out_dim_x", 1)
        self._out_dim_x = add_counter(self, "out_dim_x", 16, self._inc_out_dim_x)
        self._max_outer_dim = self.input("max_outer_dim", 16)
        # self._out_dim_ptr = add_counter(self, "out_dim_ptr", 16, self._inc_out_dim_x)
        self._max_outer_dim.add_attribute(ConfigRegAttr("How long is the matrix..."))
        # self._inc_out_dim_x = self.var(self, "inc_out_dim_x", 1)
        # self._out_dim_ptr = add_counter(self, )

        self._rfifo = RegFIFO(data_width=self.data_width + 2, width_mult=1, depth=8)

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
        READ_0 = self.scan_fsm.add_state("READ_0")
        READ_1 = self.scan_fsm.add_state("READ_1")
        READ_2 = self.scan_fsm.add_state("READ_2")

        SEQ_START = self.scan_fsm.add_state("SEQ_START")
        SEQ_ITER = self.scan_fsm.add_state("SEQ_ITER")
        SEQ_DONE = self.scan_fsm.add_state("SEQ_DONE")

        DONE = self.scan_fsm.add_state("DONE")

        self.scan_fsm.output(self._valid_inc)
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

        ####################
        # Next State Logic
        ####################

        # Dummy state for eventual filling block.
        START.next(ISSUE_STRM, kts.const(1, 1))

        # Completely done at this point
        ISSUE_STRM.next(DONE, self._out_dim_x == self._max_outer_dim)
        # If not done, we have to issue more streams
        ISSUE_STRM.next(READ_0, kts.const(1, 1))

        READ_0.next(READ_1, kts.const(1, 1))

        # If you're not at the right coordinate yet, go straight to SEQ_START and emit length 0 sequence
        READ_1.next(SEQ_START, self._coord_in > self._out_dim_x)
        # Otherwise, proceed
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
        SEQ_DONE.next(ISSUE_STRM, kts.const(1, 1))

        # DONE
        DONE.next(DONE, kts.const(1, 1))

        ####################
        # FSM Output Logic
        ####################

        #######
        # START - TODO - Generate general hardware...
        #######
        START.output(self._valid_inc, 0)
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

        #######
        # ISSUE_STRM - TODO - Generate general hardware...
        #######
        ISSUE_STRM.output(self._valid_inc, 0)
        ISSUE_STRM.output(self._ren, 0)
        ISSUE_STRM.output(self._fifo_push, 0)
        ISSUE_STRM.output(self._tag_valid_data, 0)
        ISSUE_STRM.output(self._tag_eos, 0)
        ISSUE_STRM.output(self._inc_out_dim_x, 0)
        ISSUE_STRM.output(self._inc_out_dim_addr, 0)
        ISSUE_STRM.output(self._addr_out, kts.const(0, 16))
        ISSUE_STRM.output(self._next_seq_length, kts.const(0, 16))
        ISSUE_STRM.output(self._update_seq_state, 0)
        ISSUE_STRM.output(self._step_agen, 0)
        ISSUE_STRM.output(self._last_valid_accepting, 0)

        #######
        # READ_0 - TODO - Generate general hardware...
        #######
        READ_0.output(self._valid_inc, 0)
        READ_0.output(self._ren, 1)
        READ_0.output(self._fifo_push, 0)
        READ_0.output(self._tag_valid_data, 0)
        READ_0.output(self._tag_eos, 0)
        READ_0.output(self._inc_out_dim_x, 0)
        READ_0.output(self._inc_out_dim_addr, 0)
        READ_0.output(self._addr_out, self._out_dim_addr)
        READ_0.output(self._next_seq_length, kts.const(0, 16))
        READ_0.output(self._update_seq_state, 0)
        READ_0.output(self._step_agen, 0)
        READ_0.output(self._last_valid_accepting, 0)

        #######
        # READ_1 - TODO - Generate general hardware...
        #######
        READ_1.output(self._valid_inc, 0)
        READ_1.output(self._ren, self._coord_in <= self._out_dim_x)
        READ_1.output(self._fifo_push, 0)
        READ_1.output(self._tag_valid_data, 0)
        READ_1.output(self._tag_eos, 0)
        READ_1.output(self._inc_out_dim_x, 0)
        READ_1.output(self._inc_out_dim_addr, 0)
        READ_1.output(self._addr_out, self._out_dim_addr + 1)
        READ_1.output(self._next_seq_length, kts.const(2 ** 16 - 1, 16))
        READ_1.output(self._update_seq_state, (self._coord_in > self._out_dim_x)[0])
        READ_1.output(self._step_agen, 0)
        READ_1.output(self._last_valid_accepting, 0)

        #######
        # READ_2 - TODO - Generate general hardware...
        #######
        READ_2.output(self._valid_inc, 0)
        READ_2.output(self._ren, 0)
        READ_2.output(self._fifo_push, 0)
        READ_2.output(self._tag_valid_data, 0)
        READ_2.output(self._tag_eos, 0)
        READ_2.output(self._inc_out_dim_x, 0)
        READ_2.output(self._inc_out_dim_addr, 1)
        READ_2.output(self._addr_out, kts.const(0, 16))
        READ_2.output(self._next_seq_length, self._seq_length_ptr_math)
        READ_2.output(self._update_seq_state, 1)
        READ_2.output(self._step_agen, 0)
        READ_2.output(self._last_valid_accepting, 0)

        #######
        # SEQ_START - TODO - Generate general hardware...
        #######
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
        # Clear the agen on the way in to start fresh

        #############
        # SEQ_ITER
        #############
        SEQ_ITER.output(self._valid_inc, self._valid_in & (~self._fifo_full))
        SEQ_ITER.output(self._ren, (~self._rfifo.ports.almost_full & ~self._ready_gate))
        SEQ_ITER.output(self._fifo_push, self._valid_in & (~self._fifo_full))
        SEQ_ITER.output(self._tag_valid_data, self._valid_in)
        SEQ_ITER.output(self._tag_eos, self._last_valid_accepting)
        SEQ_ITER.output(self._inc_out_dim_x, 0)
        SEQ_ITER.output(self._inc_out_dim_addr, 0)
        SEQ_ITER.output(self._addr_out, self._agen_addr)
        SEQ_ITER.output(self._next_seq_length, kts.const(0, 16))
        SEQ_ITER.output(self._update_seq_state, 0)
        SEQ_ITER.output(self._step_agen, (~self._rfifo.ports.almost_full & ~self._ready_gate))
        SEQ_ITER.output(self._last_valid_accepting, (self._valid_cnt == self._seq_length) & (self._valid_in))

        # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe

        #############
        # SEQ_DONE
        #############
        SEQ_DONE.output(self._valid_inc, 0)
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

        #############
        # DONE
        #############
        DONE.output(self._valid_inc, 0)
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

        self.scan_fsm.set_start_state(START)

# ===================================
# Dump metadata into fifo
# ===================================

        # Stupid convert
        self._data_in_packed = self.var("fifo_in_packed", self.data_width + 2, packed=True)

        # indicate valid data as well
        self.wire(self._data_in_packed[self.data_width + 1], self._tag_valid_data)
        # The EOS tags on the last valid in the stream
        self.wire(self._data_in_packed[self.data_width], self._tag_eos)
        self.wire(self._data_in_packed[self.data_width - 1, 0], self._data_in)

        self._data_out_packed = self.var("fifo_out_packed", self.data_width + 2, packed=True)
        self.wire(self._valid_out, self._data_out_packed[self.data_width + 1])
        self.wire(self._eos_out, self._data_out_packed[self.data_width])
        self.wire(self._data_out, self._data_out_packed[self.data_width - 1, 0])

        self._fifo_valid_entry = self.var("fifo_valid_entry", 1)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def ready_gate_ff():
            if ~self._rst_n:
                self._ready_gate = 0
            elif self._iter_restart:
                self._ready_gate = 1
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

    def get_bitstream(self, inner_offset, max_out):

        # Store all configurations here
        config = [("inner_dim_offset", inner_offset),
                  ("max_outer_dim", max_out)]
        return config


if __name__ == "__main__":
    scanner_dut = Scanner(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(scanner_dut, filename="scanner.sv",
            optimize_if=False)
