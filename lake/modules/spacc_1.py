from math import e
from struct import pack
import kratos as kts
from kratos import *
from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.passes.passes import lift_config_reg
from lake.utils.util import sticky_flag, trim_config_list, add_counter
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_controller import MemoryController
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO
from enum import Enum, unique
from lake.utils.util import add_counter


class SpAcc_1(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 add_clk_enable=True,
                 add_flush=False,
                 lift_config=False,
                 defer_fifos=True,
                 perf_debug=True):

        super().__init__(name="SpAcc_1", debug=True)

        self.data_width = data_width
        self.add_clk_enable = add_clk_enable
        self.add_flush = add_flush
        self.lift_config = lift_config  # what is this for?
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos
        self.perf_debug = perf_debug

        # For compatibility with tile integration...
        self.total_sets = 0

        self.num_streams = 2

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

        # input: outer_crd, inner_crd, inner_value
        self._outer_crd_in = self.input("outer_crd_in", self.data_width + 1, packed=True)
        self._outer_crd_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._outer_crd_valid = self.input("outer_crd_valid", 1)
        self._outer_crd_valid.add_attribute(ControlSignalAttr(is_control=True))

        self._outer_crd_ready = self.output("outer_crd_ready", 1)
        self._outer_crd_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._inner_crd_in = self.input("inner_crd_in", self.data_width + 1, packed=True)
        self._inner_crd_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._inner_crd_valid = self.input("inner_crd_valid", 1)
        self._inner_crd_valid.add_attribute(ControlSignalAttr(is_control=True))

        self._inner_crd_ready = self.output("inner_crd_ready", 1)
        self._inner_crd_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._inner_val_in = self.input("inner_val_in", self.data_width + 1, packed=True)
        self._inner_val_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._inner_val_valid = self.input("inner_val_valid", 1)
        self._inner_val_valid.add_attribute(ControlSignalAttr(is_control=True))

        self._inner_val_ready = self.output("inner_val_ready", 1)
        self._inner_val_ready.add_attribute(ControlSignalAttr(is_control=False))

        # output: inner_crd_parse, inner_value_parse, read_signal
        self._inner_crd_parse = self.output("inner_crd_parse", self.data_width + 1, packed=True)
        self._inner_crd_parse.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._inner_crd_parse_valid = self.output("inner_crd_parse_valid", 1)
        self._inner_crd_parse_valid.add_attribute(ControlSignalAttr(is_control=False))

        self._inner_crd_parse_ready = self.input("inner_crd_parse_ready", 1)
        self._inner_crd_parse_ready.add_attribute(ControlSignalAttr(is_control=True))

        self._inner_val_parse = self.output("inner_val_parse", self.data_width + 1, packed=True)
        self._inner_val_parse.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._inner_val_parse_valid = self.output("inner_val_parse_valid", 1)
        self._inner_val_parse_valid.add_attribute(ControlSignalAttr(is_control=False))

        self._inner_val_parse_ready = self.input("inner_val_parse_ready", 1)
        self._inner_val_parse_ready.add_attribute(ControlSignalAttr(is_control=True))

        self._read_signal = self.output("read_signal", self.data_width + 1, packed=True)
        self._read_signal.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._read_signal_valid = self.output("read_signal_valid", 1)
        self._read_signal_valid.add_attribute(ControlSignalAttr(is_control=False))

        self._read_signal_ready = self.input("read_signal_ready", 1)
        self._read_signal_ready.add_attribute(ControlSignalAttr(is_control=True))

# ==========================================
# FIFO inputs
# ==========================================
        self._outer_crd_fifo_pop = self.var("outer_crd_fifo_pop", 1)
        self._outer_crd_fifo_valid = self.var("outer_crd_fifo_valid", 1)
        self._outer_crd_fifo_in = kts.concat(self._outer_crd_in)
        self._outer_crd_in_fifo = RegFIFO(data_width=self._outer_crd_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._outer_crd_in_fifo.add_attribute(SharedFifoAttr(direction='IN'))
        self._outer_crd_fifo_out_data = self.var("outer_crd_fifo_out_data", self.data_width, packed=True)
        self._outer_crd_fifo_out_eos = self.var("outer_crd_fifo_out_eos", 1)

        self.add_child(f"outer_crd_in_fifo",
                        self._outer_crd_in_fifo,
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        clk_en=self._clk_en,
                        push=self._outer_crd_valid,
                        pop=self._outer_crd_fifo_pop,
                        data_in=self._outer_crd_fifo_in,
                        data_out=kts.concat(self._outer_crd_fifo_out_eos, self._outer_crd_fifo_out_data))
        self.wire(self._outer_crd_ready, ~self._outer_crd_in_fifo.full)
        self.wire(self._outer_crd_fifo_valid, ~self._outer_crd_in_fifo.empty)

        self._inner_crd_fifo_pop = self.var("inner_crd_fifo_pop", 1)
        self._inner_crd_fifo_valid = self.var("inner_crd_fifo_valid", 1)
        self._inner_crd_fifo_in = kts.concat(self._inner_crd_in)
        self._inner_crd_in_fifo = RegFIFO(data_width=self._inner_crd_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._inner_crd_in_fifo.add_attribute(SharedFifoAttr(direction='IN'))
        self._inner_crd_fifo_out_data = self.var("inner_crd_fifo_out_data", self.data_width, packed=True)
        self._inner_crd_fifo_out_eos = self.var("inner_crd_fifo_out_eos", 1)

        self.add_child(f"inner_crd_in_fifo",
                        self._inner_crd_in_fifo,
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        clk_en=self._clk_en,
                        push=self._inner_crd_valid,
                        pop=self._inner_crd_fifo_pop,
                        data_in=self._inner_crd_fifo_in,
                        data_out=kts.concat(self._inner_crd_fifo_out_eos, self._inner_crd_fifo_out_data))
        self.wire(self._inner_crd_ready, ~self._inner_crd_in_fifo.full)
        self.wire(self._inner_crd_fifo_valid, ~self._inner_crd_in_fifo.empty)

        self._inner_val_fifo_pop = self.var("inner_val_fifo_pop", 1)
        self._inner_val_fifo_valid = self.var("inner_val_fifo_valid", 1)
        self._inner_val_fifo_in = kts.concat(self._inner_val_in)
        self._inner_val_in_fifo = RegFIFO(data_width=self._inner_val_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._inner_val_in_fifo.add_attribute(SharedFifoAttr(direction='IN'))
        self._inner_val_fifo_out_data = self.var("inner_val_fifo_out_data", self.data_width, packed=True)
        self._inner_val_fifo_out_eos = self.var("inner_val_fifo_out_eos", 1)

        self.add_child(f"inner_val_in_fifo",
                        self._inner_val_in_fifo,
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        clk_en=self._clk_en,
                        push=self._inner_val_valid,
                        pop=self._inner_val_fifo_pop,
                        data_in=self._inner_val_fifo_in,
                        data_out=kts.concat(self._inner_val_fifo_out_eos, self._inner_val_fifo_out_data))
        self.wire(self._inner_val_ready, ~self._inner_val_in_fifo.full)
        self.wire(self._inner_val_fifo_valid, ~self._inner_val_in_fifo.empty)

        # Create sticky bits for seeing EOS on either side...
        self._eos_in_sticky = self.var("eos_in_sticky", self.num_streams)
        self._clr_eos_sticky = self.var("clr_eos_sticky", self.num_streams)
        for i in range(self.num_streams):
            # tmp_sticky = sticky_flag(self, self._coord_in_fifo_eos_in[i], clear=self._clr_eos_sticky[i], name=f"eos_sticky_{i}")
            # tmp_sticky = sticky_flag(self, self._coord_in_fifo_eos_in[i] & self._coord_in_fifo_valid_in[i], clear=self._clr_eos_sticky[i], name=f"eos_sticky_{i}")
            # Need to join the coord and pos
            tmp_sticky = sticky_flag(self, self._coord_in_fifo_eos_in[i] & self._coord_in_fifo_valid_in[i] & self._pos_in_fifo_eos_in[i] & self._pos_in_fifo_valid_in[i],
                                    clear=self._clr_eos_sticky[i], name=f"eos_sticky_{i}")
            self.wire(self._eos_in_sticky[i], tmp_sticky)

        if self.perf_debug:

            cyc_count = add_counter(self, "clock_cycle_count", 64, increment=self._clk & self._clk_en)

            # Start when any of the coord inputs is valid
            # self._start_signal = sticky_flag(self, kts.concat((*[self._coord_in_fifo_valid_in[i] for i in range(self.num_streams)])).r_or(),
            self._start_signal = sticky_flag(self, self._coord_in_fifo_valid_in[0],
                                             name='start_indicator')
            self.add_performance_indicator(self._start_signal, edge='posedge', label='start', cycle_count=cyc_count)

            # End when we see DONE on the output ref signal
            self._done_signal = sticky_flag(self, (self._coord_out == MemoryController.DONE_PROXY) &
                                                    self._coord_out[MemoryController.EOS_BIT] & self._coord_out_valid_out,
                                                    name='done_indicator')
            self.add_performance_indicator(self._done_signal, edge='posedge', label='done', cycle_count=cyc_count)

        # Intermediates
        self._pos_cnt = self.var("pos_cnt", self.data_width,
                                 size=self.num_streams,
                                 explicit_array=True,
                                 packed=True)

# ==========================================
# Generate FSM for Intersecting these streams...
# ==========================================

        self._all_valid = self.var("all_valid", 1)
        self._all_valid_join = self.var("all_valid_join", 1)

        self._any_eos = self.var("any_eos", 1)

        # Join valids
        all_in_valids = kts.concat(*self._coord_in_fifo_valid_in, *self._pos_in_fifo_valid_in)
        self.wire(self._all_valid, all_in_valids.r_and() & ~self._any_eos)
        self.wire(self._all_valid_join, all_in_valids.r_and())

        all_eos = kts.concat(*self._coord_in_fifo_eos_in, *self._pos_in_fifo_eos_in) & kts.concat(*self._coord_in_fifo_valid_in, *self._pos_in_fifo_valid_in)

        all_eos_alt = self.var("all_eos_alt", 2)
        self.wire(all_eos_alt[0], self._coord_in_fifo_eos_in[0] & self._pos_in_fifo_eos_in[0] & self._coord_in_fifo_valid_in[0] & self._pos_in_fifo_valid_in[0])
        self.wire(all_eos_alt[1], self._coord_in_fifo_eos_in[1] & self._pos_in_fifo_eos_in[1] & self._coord_in_fifo_valid_in[1] & self._pos_in_fifo_valid_in[1])

        self.wire(self._any_eos, all_eos.r_or())

        self._maybe = self.var("maybe", self.data_width)
        self.wire(self._maybe, kts.concat(kts.const(0, 6), kts.const(2, 2), kts.const(0, 8)))

        for i in range(self.num_streams):
            @always_ff((posedge, "clk"), (negedge, "rst_n"))
            def pos_cnt_ff():
                if ~self._rst_n:
                    self._pos_cnt[i] = 0
                elif self._rst_pos_cnt[i]:
                    self._pos_cnt[i] = 0
                elif self._inc_pos_cnt[i]:
                    self._pos_cnt[i] = self._pos_cnt[i] + 1
            self.add_code(pos_cnt_ff)

        # self._ready_out - already declared but lets us pop
        self._fifo_push = self.var("fifo_push", 1)
        self._fifo_full = self.var("fifo_full", 3)

        # Swap ins for FIFO
        self._coord_to_fifo = self.var("coord_to_fifo", 16)
        self._pos_to_fifo = self.var("pos_to_fifo", 16, size=self.num_streams, explicit_array=True, packed=True)

        self._coord_to_fifo_eos = self.var("coord_to_fifo_eos", 1)
        self._pos_to_fifo_eos = self.var("pos_to_fifo_eos", self.num_streams)

        # Create FSM
        self.intersect_fsm = self.add_fsm("intersect_seq", reset_high=False)
        IDLE = self.intersect_fsm.add_state("IDLE")
        ITER = self.intersect_fsm.add_state("ITER")
        UNION = self.intersect_fsm.add_state("UNION")
        DRAIN = self.intersect_fsm.add_state("DRAIN")
        ALIGN = self.intersect_fsm.add_state("ALIGN")
        # ALIGN_UNION = self.intersect_fsm.add_state("ALIGN_UNION")
        DONE = self.intersect_fsm.add_state("DONE")

        self.intersect_fsm.output(self._inc_pos_cnt[0])
        self.intersect_fsm.output(self._inc_pos_cnt[1])
        self.intersect_fsm.output(self._rst_pos_cnt[0])
        self.intersect_fsm.output(self._rst_pos_cnt[1])
        # self.intersect_fsm.output(self._ready_out)
        self.intersect_fsm.output(self._fifo_push)
        # self.intersect_fsm.output(self._eos_seen_set[0])
        # self.intersect_fsm.output(self._eos_seen_set[1])
        # self.intersect_fsm.output(self._eos_seen_clr[0])
        # self.intersect_fsm.output(self._eos_seen_clr[1])
        self.intersect_fsm.output(self._clr_eos_sticky[0])
        self.intersect_fsm.output(self._clr_eos_sticky[1])
        self.intersect_fsm.output(self._coord_to_fifo)
        self.intersect_fsm.output(self._coord_to_fifo_eos)
        self.intersect_fsm.output(self._pos_to_fifo[0])
        self.intersect_fsm.output(self._pos_to_fifo_eos[0])
        self.intersect_fsm.output(self._pos_to_fifo[1])
        self.intersect_fsm.output(self._pos_to_fifo_eos[1])

        ####################
        # Next State Logic
        ####################

        # In IDLE we stay if the fifo is full, otherwise wait
        # until we have two valids...
        IDLE.next(UNION, self._all_valid_join & (self._joiner_op == kts.const(JoinerOp.UNION.value, op_bits)) & self._tile_en)
        # If either stream is empty, we can skip to drain right away
        IDLE.next(ALIGN, self._any_eos & (self._joiner_op == kts.const(JoinerOp.INTERSECT.value, op_bits)) & self._tile_en)
        IDLE.next(ITER, self._all_valid & (self._joiner_op == kts.const(JoinerOp.INTERSECT.value, op_bits)) & self._tile_en)
        IDLE.next(IDLE, None)
        # IDLE.next(UNION, self._all_valid & (self._joiner_op == kts.const(JoinerOp.UNION.value, op_bits)))
        # IDLE.next(IDLE, self._fifo_full.r_or() | (~self._all_valid))

        # In ITER, we go back to idle when the fifo is full to avoid
        # complexity, or if we are looking at one of the eos since we can make the last
        # move for the intersection now...
        # If we have eos and can push it to the fifo, we are done with this stream
        ITER.next(ALIGN, self._any_eos & ~all_eos.r_and())
        ITER.next(ITER, None)

        # First we align the streams to both stop tokens
        # ALIGN.next(DRAIN, self._eos_in_sticky.r_and())
        # ALIGN.next(ITER, self._eos_in_sticky.r_and())
        ALIGN.next(ITER, all_eos.r_and())
        ALIGN.next(ALIGN, None)

        # For Union, there is no real early stop, we just can go until both streams hit stop tokens
        UNION.next(DRAIN, self._eos_in_sticky.r_and())
        UNION.next(UNION, None)

        # Then in DRAIN, we pass thru the stop tokens
        # The only way to leave DRAIN is to get new data
        # where both streams are valid but not both streams are eos
        # DRAIN.next(DONE, ~self._any_eos & self._all_valid)
        DRAIN.next(DONE, ~all_eos.r_and() & all_in_valids.r_and())
        DRAIN.next(DRAIN, None)

        # Once done, we need another flush
        # Just go back to beginning
        DONE.next(IDLE, None)

        ####################
        # FSM Output Logic
        ####################

        #######
        # IDLE - TODO - Generate general hardware...
        #######
        # Can detect empty here
        IDLE.output(self._inc_pos_cnt[0], 0)
        IDLE.output(self._inc_pos_cnt[1], 0)
        IDLE.output(self._rst_pos_cnt[0], 0)
        IDLE.output(self._rst_pos_cnt[1], 0)
        IDLE.output(self._fifo_push, 0)
        IDLE.output(self._clr_eos_sticky[0], 0)
        IDLE.output(self._clr_eos_sticky[1], 0)
        IDLE.output(self._coord_to_fifo, kts.const(0, 16))
        IDLE.output(self._pos_to_fifo[0], kts.const(0, 16))
        IDLE.output(self._pos_to_fifo[1], kts.const(0, 16))
        IDLE.output(self._coord_to_fifo_eos, 0)
        IDLE.output(self._pos_to_fifo_eos[0], 0)
        IDLE.output(self._pos_to_fifo_eos[1], 0)

        #######
        # ITER
        #######
        # ITER.output(self._inc_pos_cnt[0], (self._all_valid & (self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1])) & ~self._fifo_full.r_or())
        # ITER.output(self._inc_pos_cnt[1], (self._all_valid & (self._coord_in_fifo_in[0] >= self._coord_in_fifo_in[1])) & ~self._fifo_full.r_or())
        ITER.output(self._inc_pos_cnt[0], (((self._all_valid | (self._all_valid_join & all_eos.r_and())) & (self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1])) & ~self._fifo_full.r_or()) |
                    (self._all_valid_join & ~all_eos.r_and() & all_eos_alt[1]))
        ITER.output(self._inc_pos_cnt[1], ((self._all_valid | (self._all_valid_join & all_eos.r_and())) & (self._coord_in_fifo_in[0] >= self._coord_in_fifo_in[1])) & ~self._fifo_full.r_or() |
                    (self._all_valid_join & ~all_eos.r_and() & all_eos_alt[0]))
        # ITER.output(self._inc_pos_cnt[0], ((self._all_valid | (self._all_valid_join & all_eos.r_and())) & (self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1])) & ~self._fifo_full.r_or())
        # ITER.output(self._inc_pos_cnt[1], ((self._all_valid | (self._all_valid_join & all_eos.r_and())) & (self._coord_in_fifo_in[0] >= self._coord_in_fifo_in[1])) & ~self._fifo_full.r_or())
        ITER.output(self._rst_pos_cnt[0], self._any_eos & ~self._fifo_full.r_or())
        ITER.output(self._rst_pos_cnt[1], self._any_eos & ~self._fifo_full.r_or())
        # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe
        ITER.output(self._fifo_push, self._all_valid_join & (((self._coord_in_fifo_in[0] == self._coord_in_fifo_in[1]) & ~self._any_eos) | (all_eos.r_and())) & ~self._fifo_full.r_or())
        ITER.output(self._clr_eos_sticky[0], (all_eos.r_and() & ~self._fifo_full.r_or()))
        ITER.output(self._clr_eos_sticky[1], (all_eos.r_and() & ~self._fifo_full.r_or()))
        ITER.output(self._coord_to_fifo, self._coord_in_fifo_in[0][15, 0])
        # ITER.output(self._pos_to_fifo[0], self._pos_cnt[0] + self._payload_ptr[0])
        # ITER.output(self._pos_to_fifo[1], self._pos_cnt[1] + self._payload_ptr[1])
        ITER.output(self._pos_to_fifo[0], self._pos_in_fifo_in[0][15, 0])
        ITER.output(self._pos_to_fifo[1], self._pos_in_fifo_in[1][15, 0])
        ITER.output(self._coord_to_fifo_eos, all_eos.r_and())
        ITER.output(self._pos_to_fifo_eos[0], all_eos.r_and())
        ITER.output(self._pos_to_fifo_eos[1], all_eos.r_and())
        # ITER.output(self._coord_to_fifo_eos, 0)
        # ITER.output(self._pos_to_fifo_eos[0], 0)
        # ITER.output(self._pos_to_fifo_eos[1], 0)

        #######
        # ALIGN
        #######
        # Need to align the inputs as well
        ALIGN.output(self._inc_pos_cnt[0], (~self._eos_in_sticky[0] & self._coord_in_fifo_valid_in[0] & self._pos_in_fifo_valid_in[0]) | (all_eos.r_and() & ~self._fifo_full.r_or()))
        ALIGN.output(self._inc_pos_cnt[1], (~self._eos_in_sticky[1] & self._coord_in_fifo_valid_in[1] & self._pos_in_fifo_valid_in[1]) | (all_eos.r_and() & ~self._fifo_full.r_or()))
        ALIGN.output(self._rst_pos_cnt[0], 0)
        ALIGN.output(self._rst_pos_cnt[1], 0)
        # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe
        ALIGN.output(self._fifo_push, (all_eos.r_and() & ~self._fifo_full.r_or()))
        ALIGN.output(self._clr_eos_sticky[0], (all_eos.r_and() & ~self._fifo_full.r_or()))
        ALIGN.output(self._clr_eos_sticky[1], (all_eos.r_and() & ~self._fifo_full.r_or()))
        ALIGN.output(self._coord_to_fifo, self._coord_in_fifo_in[0][15, 0])
        ALIGN.output(self._pos_to_fifo[0], self._pos_in_fifo_in[0][15, 0])
        ALIGN.output(self._pos_to_fifo[1], self._pos_in_fifo_in[1][15, 0])
        ALIGN.output(self._coord_to_fifo_eos, 1)
        ALIGN.output(self._pos_to_fifo_eos[0], 1)
        ALIGN.output(self._pos_to_fifo_eos[1], 1)

        #######
        # UNION
        #######
        # Pop if the lesser coord or the other stream is at eos
        UNION.output(self._inc_pos_cnt[0], self._all_valid_join & ((self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1]) | self._coord_in_fifo_eos_in[1]) & ~self._fifo_full.r_or() & ~self._coord_in_fifo_eos_in[0])
        UNION.output(self._inc_pos_cnt[1], self._all_valid_join & ((self._coord_in_fifo_in[0] >= self._coord_in_fifo_in[1]) | self._coord_in_fifo_eos_in[0]) & ~self._fifo_full.r_or() & ~self._coord_in_fifo_eos_in[1])
        UNION.output(self._rst_pos_cnt[0], self._any_eos & ~self._fifo_full.r_or())
        UNION.output(self._rst_pos_cnt[1], self._any_eos & ~self._fifo_full.r_or())
        # We need to push any coordinate period as long as there is room and they are not all eos
        # UNION.output(self._fifo_push, self._all_valid & ~self._fifo_full.r_or() & ~all_eos.r_and())
        UNION.output(self._fifo_push, self._all_valid_join & ~self._fifo_full.r_or() & ~all_eos.r_and())
        UNION.output(self._clr_eos_sticky[0], 0)
        UNION.output(self._clr_eos_sticky[1], 0)
        # Need to pick which FIFO to pass through
        # UNION.output(self._coord_to_fifo, self._coord_in_fifo_in[0][15, 0])
        UNION.output(self._coord_to_fifo, kts.ternary(self._coord_in_fifo_eos_in[0],
                                                      self._coord_in_fifo_in[1][15, 0], kts.ternary(self._coord_in_fifo_eos_in[1],
                                                                                                    self._coord_in_fifo_in[0][15, 0], kts.ternary((self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1]),
                                                                                                                                                  self._coord_in_fifo_in[0][15, 0], self._coord_in_fifo_in[1][15, 0]))))
        # UNION.output(self._pos_to_fifo[0], self._pos_cnt[0] + self._payload_ptr[0])
        # UNION.output(self._pos_to_fifo[1], self._pos_cnt[1] + self._payload_ptr[1])
        # UNION.output(self._pos_to_fifo[0], self._pos_in_fifo_in[0][15, 0])
        # Difference in union is we are capable of passing the MAYBE token - only pass the reference if the out coordinate matches the first coordinate
        # UNION.output(self._pos_to_fifo[0], kts.ternary(self._coord_in_fifo_in[0] == self._coord_to_fifo,
        #                                                self._pos_in_fifo_in[0][15, 0], kts.concat(kts.const(0, 6), kts.const(2, 2), kts.const(0, 8))))
        # UNION.output(self._pos_to_fifo[0], kts.ternary(self._coord_in_fifo_eos_in[0],
        #                                                self._maybe, kts.ternary(self._coord_in_fifo_eos_in[1],
        #                                                                         self._pos_in_fifo_in[0][15, 0], kts.ternary((self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1]),
        #                                                                                                                     self._pos_in_fifo_in[0][15, 0], self._maybe))))
        UNION.output(self._pos_to_fifo[0], kts.ternary(self._value_mode,
                                                kts.ternary(self._coord_in_fifo_eos_in[0] | (~self._any_eos & (self._coord_in_fifo_in[0] > self._coord_in_fifo_in[1])),
                                                    self._pos_in_fifo_in[1][15, 0],
                                                    kts.ternary(self._coord_in_fifo_eos_in[1] | (~self._any_eos & (self._coord_in_fifo_in[0] < self._coord_in_fifo_in[1])),
                                                        self._pos_in_fifo_in[0][15, 0],
                                                        self._pos_in_fifo_in[0][15, 0] + self._pos_in_fifo_in[1][15, 0])),
                                                kts.ternary(self._coord_in_fifo_eos_in[0],
                                                       self._maybe, kts.ternary(self._coord_in_fifo_eos_in[1],
                                                                                self._pos_in_fifo_in[0][15, 0], kts.ternary((self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1]),
                                                                                                                            self._pos_in_fifo_in[0][15, 0], self._maybe)))))
        # UNION.output(self._pos_to_fifo[1], self._pos_in_fifo_in[1][15, 0])
        # UNION.output(self._pos_to_fifo[1], kts.ternary(self._coord_in_fifo_in[1] == self._coord_to_fifo,
        #                                                self._pos_in_fifo_in[1][15, 0], kts.concat(kts.const(0, 6), kts.const(2, 2), kts.const(0, 8))))
        # UNION.output(self._pos_to_fifo[1], kts.ternary(self._coord_in_fifo_eos_in[1],
        #                                                self._maybe, kts.ternary(self._coord_in_fifo_eos_in[0],
        #                                                                         self._pos_in_fifo_in[1][15, 0], kts.ternary((self._coord_in_fifo_in[1] <= self._coord_in_fifo_in[0]),
        #                                                                                                                     self._pos_in_fifo_in[1][15, 0], self._maybe))))
        UNION.output(self._pos_to_fifo[1], kts.ternary(self._value_mode,
                                                kts.ternary(self._coord_in_fifo_eos_in[0] | (~self._any_eos & (self._coord_in_fifo_in[0] > self._coord_in_fifo_in[1])),
                                                    self._pos_in_fifo_in[1][15, 0],
                                                    kts.ternary(self._coord_in_fifo_eos_in[1] | (~self._any_eos & (self._coord_in_fifo_in[0] < self._coord_in_fifo_in[1])),
                                                        self._pos_in_fifo_in[0][15, 0],
                                                        self._pos_in_fifo_in[0][15, 0] + self._pos_in_fifo_in[1][15, 0])),
                                                kts.ternary(self._coord_in_fifo_eos_in[1],
                                                       self._maybe, kts.ternary(self._coord_in_fifo_eos_in[0],
                                                                                self._pos_in_fifo_in[1][15, 0], kts.ternary((self._coord_in_fifo_in[1] <= self._coord_in_fifo_in[0]),
                                                                                                                            self._pos_in_fifo_in[1][15, 0], self._maybe)))))
        UNION.output(self._coord_to_fifo_eos, 0)
        # UNION.output(self._pos_to_fifo_eos[0], (self._pos_in_fifo_eos_in[0] & ~self._coord_in_fifo_eos_in[0]) | kts.ternary(self._coord_in_fifo_eos_in[0],
        #                                                    kts.const(1, 1), kts.ternary(self._coord_in_fifo_eos_in[1],
        #                                                                                 kts.const(0, 1), kts.ternary((self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1]),
        #                                                                                                              kts.const(0, 1), kts.const(1, 1)))))
        # UNION.output(self._pos_to_fifo_eos[1], (self._pos_in_fifo_eos_in[1] & ~self._coord_in_fifo_eos_in[1]) | kts.ternary(self._coord_in_fifo_eos_in[1],
        #                                                    kts.const(1, 1), kts.ternary(self._coord_in_fifo_eos_in[0],
        #                                                                                 kts.const(0, 1), kts.ternary((self._coord_in_fifo_in[1] <= self._coord_in_fifo_in[0]),
        #                                                                                                              kts.const(0, 1), kts.const(1, 1)))))
        UNION.output(self._pos_to_fifo_eos[0], kts.ternary(self._value_mode,
                                                0,
                                                (self._pos_in_fifo_eos_in[0] & ~self._coord_in_fifo_eos_in[0]) | kts.ternary(self._coord_in_fifo_eos_in[0],
                                                           kts.const(1, 1), kts.ternary(self._coord_in_fifo_eos_in[1],
                                                                                        kts.const(0, 1), kts.ternary((self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1]),
                                                                                                                     kts.const(0, 1), kts.const(1, 1))))))
        UNION.output(self._pos_to_fifo_eos[1], kts.ternary(self._value_mode,
                                                0,
                                                (self._pos_in_fifo_eos_in[1] & ~self._coord_in_fifo_eos_in[1]) | kts.ternary(self._coord_in_fifo_eos_in[1],
                                                           kts.const(1, 1), kts.ternary(self._coord_in_fifo_eos_in[0],
                                                                                        kts.const(0, 1), kts.ternary((self._coord_in_fifo_in[1] <= self._coord_in_fifo_in[0]),
                                                                                                                     kts.const(0, 1), kts.const(1, 1))))))
        # UNION.output(self._pos_to_fifo_eos[0], (self._coord_in_fifo_in[0][15, 0] != self._coord_to_fifo[15, 0]))
        # UNION.output(self._pos_to_fifo_eos[1], (self._coord_in_fifo_in[1][15, 0] != self._coord_to_fifo[15, 0]))

        #######
        # DRAIN
        #######
        DRAIN.output(self._inc_pos_cnt[0], ~self._fifo_full.r_or() & all_eos.r_and() & all_in_valids.r_and())
        DRAIN.output(self._inc_pos_cnt[1], ~self._fifo_full.r_or() & all_eos.r_and() & all_in_valids.r_and())
        # DRAIN.output(self._inc_pos_cnt[0], ~self._fifo_full.r_or() & self._coord_in_fifo_eos_in[0] & all_in_valids.r_and())
        # DRAIN.output(self._inc_pos_cnt[1], ~self._fifo_full.r_or() & self._coord_in_fifo_eos_in[0] & all_in_valids.r_and())
        DRAIN.output(self._rst_pos_cnt[0], 0)
        DRAIN.output(self._rst_pos_cnt[1], 0)
        # Keep draining while we have eos in...should be aligned
        # DRAIN.output(self._fifo_push, ~self._fifo_full.r_or() & self._coord_in_fifo_eos_in[0] & all_in_valids.r_and())
        DRAIN.output(self._fifo_push, ~self._fifo_full.r_or() & all_eos.r_and() & all_in_valids.r_and())
        DRAIN.output(self._clr_eos_sticky[0], 0)
        DRAIN.output(self._clr_eos_sticky[1], 0)
        # TODO
        DRAIN.output(self._coord_to_fifo, self._coord_in_fifo_in[0][15, 0])
        DRAIN.output(self._pos_to_fifo[0], self._coord_in_fifo_in[0][15, 0])
        DRAIN.output(self._pos_to_fifo[1], self._coord_in_fifo_in[0][15, 0])
        DRAIN.output(self._coord_to_fifo_eos, self._any_eos)
        DRAIN.output(self._pos_to_fifo_eos[0], self._any_eos)
        DRAIN.output(self._pos_to_fifo_eos[1], self._any_eos)

        #######
        # DONE
        #######
        DONE.output(self._inc_pos_cnt[0], 0)
        DONE.output(self._inc_pos_cnt[1], 0)
        DONE.output(self._rst_pos_cnt[0], 1)
        DONE.output(self._rst_pos_cnt[1], 1)
        DONE.output(self._fifo_push, 0)
        DONE.output(self._clr_eos_sticky[0], 1)
        DONE.output(self._clr_eos_sticky[1], 1)
        DONE.output(self._coord_to_fifo, kts.const(0, 16))
        DONE.output(self._pos_to_fifo[0], kts.const(0, 16))
        DONE.output(self._pos_to_fifo[1], kts.const(0, 16))
        DONE.output(self._coord_to_fifo_eos, 0)
        DONE.output(self._pos_to_fifo_eos[0], 0)
        DONE.output(self._pos_to_fifo_eos[1], 0)

        self.intersect_fsm.set_start_state(IDLE)

# ===================================
# Dump metadata into fifo
# ===================================
        self._coord_fifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._coord_fifo.add_attribute(SharedFifoAttr(direction="OUT"))
        self._pos0_fifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._pos0_fifo.add_attribute(SharedFifoAttr(direction="OUT"))
        self._pos1_fifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._pos1_fifo.add_attribute(SharedFifoAttr(direction="OUT"))

        # Stupid convert -
        self._coord_data_in_packed = self.var("coord_fifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._coord_data_in_packed[self.data_width], self._coord_to_fifo_eos)
        self.wire(self._coord_data_in_packed[self.data_width - 1, 0 * self.data_width], self._coord_to_fifo)

        self._coord_data_out_packed = self.var("coord_fifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._coord_out[self.data_width], self._coord_data_out_packed[self.data_width])
        self.wire(self._coord_out[self.data_width - 1, 0], self._coord_data_out_packed[self.data_width - 1, 0 * self.data_width])

        self._pos0_data_in_packed = self.var("pos0_fifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._pos0_data_in_packed[self.data_width], self._pos_to_fifo_eos[0])
        self.wire(self._pos0_data_in_packed[self.data_width - 1, 0 * self.data_width], self._pos_to_fifo[0])

        self._pos0_data_out_packed = self.var("pos0_fifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._pos_out[0][self.data_width], self._pos0_data_out_packed[self.data_width])
        self.wire(self._pos_out[0][self.data_width - 1, 0], self._pos0_data_out_packed[self.data_width - 1, 0 * self.data_width])

        self._pos1_data_in_packed = self.var("pos1_fifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._pos1_data_in_packed[self.data_width], self._pos_to_fifo_eos[1])
        self.wire(self._pos1_data_in_packed[self.data_width - 1, 0 * self.data_width], self._pos_to_fifo[1])

        self._pos1_data_out_packed = self.var("pos1_fifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._pos_out[1][self.data_width], self._pos1_data_out_packed[self.data_width])
        self.wire(self._pos_out[1][self.data_width - 1, 0], self._pos1_data_out_packed[self.data_width - 1, 0 * self.data_width])

        self.add_child(f"coordinate_fifo",
                       self._coord_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._fifo_push,
                       pop=self._coord_out_ready_in,
                       data_in=self._coord_data_in_packed,
                       data_out=self._coord_data_out_packed,
                       full=self._fifo_full[0])

        self.add_child(f"pos0_fifo",
                       self._pos0_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._fifo_push,
                       pop=self._pos_out_ready_in[0],
                       data_in=self._pos0_data_in_packed,
                       data_out=self._pos0_data_out_packed,
                       full=self._fifo_full[1])

        self.add_child(f"pos1_fifo",
                       self._pos1_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._fifo_push,
                       pop=self._pos_out_ready_in[1],
                       data_in=self._pos1_data_in_packed,
                       data_out=self._pos1_data_out_packed,
                       full=self._fifo_full[2])

        self.wire(self._coord_out_valid_out, ~self._coord_fifo.ports.empty)
        self.wire(self._pos_out_valid_out[0], ~self._pos0_fifo.ports.empty)
        self.wire(self._pos_out_valid_out[1], ~self._pos1_fifo.ports.empty)
        # Build in the merging logic.

        # Force FSM realization first so that flush gets added...
        kts.passes.realize_fsm(self.internal_generator)

        if self.defer_fifos is False:
            all_fifos = self.get_fifos()
            for child_fifo in all_fifos:
                child_fifo.generate_hardware()

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

        if self.lift_config:
            # Finally, lift the config regs...
            lift_config_reg(self.internal_generator)

    # def get_bitstream(self, cmrg_enable=0, cmrg_stop_lvl=0, op=JoinerOp.INTERSECT.value):
    def get_bitstream(self, config_kwargs):

        op = config_kwargs['op']

        # Store all configurations here
        # TODO: add value mode config later
        config = [("tile_en", 1),
                  ("joiner_op", op)]

        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Trim the list
        return trim_config_list(flattened, config)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "intersect"


if __name__ == "__main__":
    intersect_dut = Intersect(data_width=16,
                              use_merger=False,
                              defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(intersect_dut, filename="intersect.sv",
            optimize_if=False)
