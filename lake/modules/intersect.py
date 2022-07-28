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


@unique
class JoinerOp(Enum):
    INTERSECT = 0
    UNION = 1


class Intersect(MemoryController):
    def __init__(self,
                 data_width=16,
                 use_merger=False,
                 fifo_depth=8,
                 add_clk_enable=True,
                 add_flush=False,
                 lift_config=False,
                 defer_fifos=True):

        name_str = f"intersect_unit{'_w_crddrop' if use_merger else ''}"
        super().__init__(name=name_str, debug=True)

        self.data_width = data_width
        self.add_clk_enable = add_clk_enable
        self.add_flush = add_flush
        self.lift_config = lift_config
        self.use_merger = use_merger
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos

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

        if len(JoinerOp) == 1:
            op_bits = 1
        else:
            op_bits = kts.clog2(len(JoinerOp))

        self._joiner_op = self.input("joiner_op", op_bits)
        self._joiner_op.add_attribute(ConfigRegAttr("Operation to perform"))

        # Scanner interface will need
        # input data, input valid
        # output address, output valid
        self._coord_in = []
        self._coord_valid_in = []
        self._coord_ready_out = []
        self._pos_in = []
        self._pos_valid_in = []
        self._pos_ready_out = []

        self._pos_out = []
        self._pos_out_valid_out = []
        self._pos_out_ready_in = []
        # self._pos_out_eos_out = []
        for i in range(self.num_streams):

            tmp_coord_in = self.input(f"coord_in_{i}", self.data_width + 1, packed=True)
            tmp_coord_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_coord_valid = self.input(f"coord_in_{i}_valid", 1)
            tmp_coord_valid.add_attribute(ControlSignalAttr(is_control=True))

            tmp_coord_ready = self.output(f"coord_in_{i}_ready", 1)
            tmp_coord_ready.add_attribute(ControlSignalAttr(is_control=False))

            self._coord_in.append(tmp_coord_in)
            self._coord_valid_in.append(tmp_coord_valid)
            self._coord_ready_out.append(tmp_coord_ready)

            tmp_pos_in = self.input(f"pos_in_{i}", self.data_width + 1, packed=True)
            tmp_pos_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_pos_valid = self.input(f"pos_in_{i}_valid", 1)
            tmp_pos_valid.add_attribute(ControlSignalAttr(is_control=True))

            tmp_pos_ready = self.output(f"pos_in_{i}_ready", 1)
            tmp_pos_ready.add_attribute(ControlSignalAttr(is_control=False))

            self._pos_in.append(tmp_pos_in)
            self._pos_valid_in.append(tmp_pos_valid)
            self._pos_ready_out.append(tmp_pos_ready)

            # POS OUT
            tmp_pos_out = self.output(f"pos_out_{i}", self.data_width + 1, packed=True)
            tmp_pos_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_pos_out_valid = self.output(f"pos_out_{i}_valid", 1)
            tmp_pos_out_valid.add_attribute(ControlSignalAttr(is_control=False))

            tmp_pos_out_ready = self.input(f"pos_out_{i}_ready", 1)
            tmp_pos_out_ready.add_attribute(ControlSignalAttr(is_control=True))

            self._pos_out.append(tmp_pos_out)
            self._pos_out_valid_out.append(tmp_pos_out_valid)
            self._pos_out_ready_in.append(tmp_pos_out_ready)

        # only send one common coord out for now.
        self._coord_out = self.output("coord_out", self.data_width + 1, packed=True)
        self._coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._coord_out_ready_in = self.input("coord_out_ready", 1)
        self._coord_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._coord_out_valid_out = self.output("coord_out_valid", 1)
        self._coord_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

# ==========================================
# Main logic begin - FIFO inputs
# ==========================================

        self._coord_in_fifo_in = []
        self._coord_in_fifo_valid_in = []
        self._coord_in_fifo_eos_in = []
        self._pos_in_fifo_in = []
        self._pos_in_fifo_valid_in = []
        self._pos_in_fifo_eos_in = []

        # Control Vars from FSM
        self._rst_pos_cnt = self.var("rst_pos_cnt", self.num_streams)
        self._inc_pos_cnt = self.var("inc_pos_cnt", self.num_streams)

        for i in range(self.num_streams):

            # COORD IN FIFOS
            # COORD IN FIFOS
            tmp_coord_fifo = RegFIFO(data_width=self._coord_in[i].width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
            tmp_coord_fifo.add_attribute(SharedFifoAttr(direction="IN"))

            tmp_coord_in_valid_in = self.var(f"coord_in_{i}_fifo_valid_in", 1)

            tmp_coord_in = self.var(f"coord_in_{i}_fifo_in", self.data_width + 1, packed=True)
            tmp_coord_in_eos_in = self.var(f"coord_in_{i}_fifo_eos_in", 1, packed=True)

            self.wire(tmp_coord_in_eos_in, tmp_coord_in[self.data_width])

            self.add_child(f"coord_in_fifo_{i}",
                           tmp_coord_fifo,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           clk_en=self._clk_en,
                           push=self._coord_valid_in[i],
                           pop=self._inc_pos_cnt[i],
                           data_in=self._coord_in[i],
                           data_out=tmp_coord_in)

            self.wire(self._coord_ready_out[i], ~tmp_coord_fifo.ports.full)
            self.wire(tmp_coord_in_valid_in, ~tmp_coord_fifo.ports.empty)

            self._coord_in_fifo_in.append(tmp_coord_in)
            self._coord_in_fifo_valid_in.append(tmp_coord_in_valid_in)
            self._coord_in_fifo_eos_in.append(tmp_coord_in_eos_in)

            # POS IN FIFOS
            tmp_pos_fifo = RegFIFO(data_width=self._pos_in[i].width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
            tmp_pos_fifo.add_attribute(SharedFifoAttr(direction="IN"))

            tmp_pos_in_valid_in = self.var(f"pos_in_{i}_fifo_valid_in", 1)

            tmp_pos_in = self.var(f"pos_in_{i}_fifo_in", self.data_width + 1, packed=True)
            tmp_pos_in_eos_in = self.var(f"pos_in_{i}_fifo_eos_in", 1, packed=True)

            self.wire(tmp_pos_in_eos_in, tmp_pos_in[self.data_width])

            self.add_child(f"pos_in_fifo_{i}",
                           tmp_pos_fifo,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           clk_en=self._clk_en,
                           push=self._pos_valid_in[i],
                           pop=self._inc_pos_cnt[i],
                           data_in=self._pos_in[i],
                           data_out=tmp_pos_in)

            self.wire(self._pos_ready_out[i], ~tmp_pos_fifo.ports.full)
            self.wire(tmp_pos_in_valid_in, ~tmp_pos_fifo.ports.empty)

            self._pos_in_fifo_in.append(tmp_pos_in)
            self._pos_in_fifo_valid_in.append(tmp_pos_in_valid_in)
            self._pos_in_fifo_eos_in.append(tmp_pos_in_eos_in)

        # Create sticky bits for seeing EOS on either side...
        self._eos_in_sticky = self.var("eos_in_sticky", self.num_streams)
        self._clr_eos_sticky = self.var("clr_eos_sticky", self.num_streams)
        for i in range(self.num_streams):
            # tmp_sticky = sticky_flag(self, self._coord_in_fifo_eos_in[i], clear=self._clr_eos_sticky[i], name=f"eos_sticky_{i}")
            tmp_sticky = sticky_flag(self, self._coord_in_fifo_eos_in[i] & self._coord_in_fifo_valid_in[i], clear=self._clr_eos_sticky[i], name=f"eos_sticky_{i}")
            self.wire(self._eos_in_sticky[i], tmp_sticky)

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
        ITER.next(ALIGN, self._any_eos)
        ITER.next(ITER, None)

        # First we align the streams to both stop tokens
        ALIGN.next(DRAIN, self._eos_in_sticky.r_and())
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
        ITER.output(self._inc_pos_cnt[0], (self._all_valid & (self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1])) & ~self._fifo_full.r_or())
        ITER.output(self._inc_pos_cnt[1], (self._all_valid & (self._coord_in_fifo_in[0] >= self._coord_in_fifo_in[1])) & ~self._fifo_full.r_or())
        ITER.output(self._rst_pos_cnt[0], self._any_eos & ~self._fifo_full.r_or())
        ITER.output(self._rst_pos_cnt[1], self._any_eos & ~self._fifo_full.r_or())
        # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe
        ITER.output(self._fifo_push, self._all_valid & (self._coord_in_fifo_in[0] == self._coord_in_fifo_in[1]) & ~self._fifo_full.r_or() & ~self._any_eos)
        ITER.output(self._clr_eos_sticky[0], 0)
        ITER.output(self._clr_eos_sticky[1], 0)
        ITER.output(self._coord_to_fifo, self._coord_in_fifo_in[0][15, 0])
        # ITER.output(self._pos_to_fifo[0], self._pos_cnt[0] + self._payload_ptr[0])
        # ITER.output(self._pos_to_fifo[1], self._pos_cnt[1] + self._payload_ptr[1])
        ITER.output(self._pos_to_fifo[0], self._pos_in_fifo_in[0][15, 0])
        ITER.output(self._pos_to_fifo[1], self._pos_in_fifo_in[1][15, 0])
        ITER.output(self._coord_to_fifo_eos, 0)
        ITER.output(self._pos_to_fifo_eos[0], 0)
        ITER.output(self._pos_to_fifo_eos[1], 0)

        #######
        # ALIGN
        #######
        ALIGN.output(self._inc_pos_cnt[0], ~self._eos_in_sticky[0])
        ALIGN.output(self._inc_pos_cnt[1], ~self._eos_in_sticky[1])
        ALIGN.output(self._rst_pos_cnt[0], 0)
        ALIGN.output(self._rst_pos_cnt[1], 0)
        # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe
        ALIGN.output(self._fifo_push, 0)
        ALIGN.output(self._clr_eos_sticky[0], 0)
        ALIGN.output(self._clr_eos_sticky[1], 0)
        ALIGN.output(self._coord_to_fifo, kts.const(0, 16))
        ALIGN.output(self._pos_to_fifo[0], kts.const(0, 16))
        ALIGN.output(self._pos_to_fifo[1], kts.const(0, 16))
        ALIGN.output(self._coord_to_fifo_eos, 0)
        ALIGN.output(self._pos_to_fifo_eos[0], 0)
        ALIGN.output(self._pos_to_fifo_eos[1], 0)

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
        UNION.output(self._pos_to_fifo[0], kts.ternary(self._coord_in_fifo_eos_in[0],
                                                       self._maybe, kts.ternary(self._coord_in_fifo_eos_in[1],
                                                                                self._pos_in_fifo_in[0][15, 0], kts.ternary((self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1]),
                                                                                                                            self._pos_in_fifo_in[0][15, 0], self._maybe))))
        # UNION.output(self._pos_to_fifo[1], self._pos_in_fifo_in[1][15, 0])
        # UNION.output(self._pos_to_fifo[1], kts.ternary(self._coord_in_fifo_in[1] == self._coord_to_fifo,
        #                                                self._pos_in_fifo_in[1][15, 0], kts.concat(kts.const(0, 6), kts.const(2, 2), kts.const(0, 8))))
        UNION.output(self._pos_to_fifo[1], kts.ternary(self._coord_in_fifo_eos_in[1],
                                                       self._maybe, kts.ternary(self._coord_in_fifo_eos_in[0],
                                                                                self._pos_in_fifo_in[1][15, 0], kts.ternary((self._coord_in_fifo_in[1] <= self._coord_in_fifo_in[0]),
                                                                                                                            self._pos_in_fifo_in[1][15, 0], self._maybe))))
        UNION.output(self._coord_to_fifo_eos, 0)
        UNION.output(self._pos_to_fifo_eos[0], kts.ternary(self._coord_in_fifo_eos_in[0],
                                                           kts.const(1, 1), kts.ternary(self._coord_in_fifo_eos_in[1],
                                                                                        kts.const(0, 1), kts.ternary((self._coord_in_fifo_in[0] <= self._coord_in_fifo_in[1]),
                                                                                                                     kts.const(0, 1), kts.const(1, 1)))))
        UNION.output(self._pos_to_fifo_eos[1], kts.ternary(self._coord_in_fifo_eos_in[1],
                                                           kts.const(1, 1), kts.ternary(self._coord_in_fifo_eos_in[0],
                                                                                        kts.const(0, 1), kts.ternary((self._coord_in_fifo_in[1] <= self._coord_in_fifo_in[0]),
                                                                                                                     kts.const(0, 1), kts.const(1, 1)))))
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
        if use_merger:
            self.add_merging_logic()

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

    def get_bitstream(self, cmrg_enable=0, cmrg_stop_lvl=0, op=JoinerOp.INTERSECT.value):

        # Store all configurations here
        config = [("tile_en", 1),
                  ("joiner_op", op)]

        if self.use_merger:
            config += [("cmrg_enable", cmrg_enable)]
            config += [("cmrg_stop_lvl", cmrg_stop_lvl)]

        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Trim the list
        return trim_config_list(flattened, config)

    def add_merging_logic(self):

        # TODO: Gate merging logic when unused...
        # Enable/Disable tile
        self._cmrg_stop_lvl = self.input("cmrg_stop_lvl", 16)
        self._cmrg_stop_lvl.add_attribute(ConfigRegAttr("Strip level for cmrg"))

        self._cmrg_enable = self.input("cmrg_enable", 1)
        self._cmrg_enable.add_attribute(ConfigRegAttr("Enable cmrger"))

        # Interface to downstream
        self._cmrg_coord_out = []
        self._cmrg_coord_out_ready_in = []
        self._cmrg_coord_out_valid_out = []

        self._cmrg_coord_in = []
        self._cmrg_coord_in_ready_out = []
        self._cmrg_coord_in_valid_in = []
        self._cmrg_coord_in_eos_in = []

        for i in range(2):

            tmp_cmrg_coord_out = self.output(f"cmrg_coord_out_{i}", self.data_width + 1, packed=True)
            tmp_cmrg_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_cmrg_coord_out_valid_out = self.output(f"cmrg_coord_out_{i}_valid", 1)
            tmp_cmrg_coord_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

            tmp_cmrg_coord_out_ready_in = self.input(f"cmrg_coord_out_{i}_ready", 1)
            tmp_cmrg_coord_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

            # Add to lists
            self._cmrg_coord_out.append(tmp_cmrg_coord_out)
            self._cmrg_coord_out_ready_in.append(tmp_cmrg_coord_out_ready_in)
            self._cmrg_coord_out_valid_out.append(tmp_cmrg_coord_out_valid_out)

            tmp_cmrg_coord_in = self.input(f"cmrg_coord_in_{i}", self.data_width + 1, packed=True)
            tmp_cmrg_coord_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_cmrg_coord_in_valid_in = self.input(f"cmrg_coord_in_{i}_valid", 1)
            tmp_cmrg_coord_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))

            tmp_cmrg_coord_in_ready_out = self.output(f"cmrg_coord_in_{i}_ready", 1)
            tmp_cmrg_coord_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

            tmp_cmrg_coord_in_eos_in = self.var(f"cmrg_coord_in_{i}_eos", 1)
            self.wire(tmp_cmrg_coord_in_eos_in, tmp_cmrg_coord_in[self.data_width])

            self._cmrg_coord_in.append(tmp_cmrg_coord_in)
            self._cmrg_coord_in_ready_out.append(tmp_cmrg_coord_in_ready_out)
            self._cmrg_coord_in_valid_in.append(tmp_cmrg_coord_in_valid_in)
            self._cmrg_coord_in_eos_in.append(tmp_cmrg_coord_in_eos_in)
        # self._cmrg_ready_in = self.input("cmrg_ready_in", 2)
        # self._cmrg_ready_in.add_attribute(ControlSignalAttr(is_control=False))

        # self._cmrg_valid_out = self.output("cmrg_valid_out", 2)
        # self._cmrg_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._cmrg_eos_out = self.output("cmrg_eos_out", 2)
        # self._cmrg_eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._cmrg_ready_out = self.output("cmrg_ready_out", 2)
        # self._cmrg_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._cmrg_coord_out = self.output(f"cmrg_coord_out", self.data_width,
        #                                    size=2, packed=True, explicit_array=True)
        # self._cmrg_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # # Coords in
        # self._cmrg_coord_in = self.input(f"cmrg_coord_in", self.data_width,
        #                                  size=2, packed=True, explicit_array=True)
        # self._cmrg_coord_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._cmrg_valid_in = self.input("cmrg_valid_in", 2)
        # self._cmrg_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        # self._cmrg_eos_in = self.input("cmrg_eos_in", 2)
        # self._cmrg_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        ####
        self._cmrg_fifo_push = self.var("cmrg_fifo_push", 2)
        self._cmrg_fifo_pop = self.var("cmrg_fifo_pop", 2)

        # Create a bunch of fifos
        fifo_kwargs = {
            "data_width": self.data_width + 1,
            "width_mult": 1,
            "depth": self.fifo_depth,
            'defer_hrdwr_gen': True
        }

        base_infifo = RegFIFO(**fifo_kwargs)
        base_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        proc_infifo = RegFIFO(**fifo_kwargs)
        proc_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        base_outfifo = RegFIFO(**fifo_kwargs)
        base_outfifo.add_attribute(SharedFifoAttr(direction="OUT"))
        proc_outfifo = RegFIFO(**fifo_kwargs)
        proc_outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

        ##############
        # BASE infifo
        ##############
        # Use this stream to process the PROC stream...
        self._base_infifo_in_data = self.var("base_infifo_in_data", 16)
        self._base_infifo_in_eos = self.var("base_infifo_in_eos", 1)
        self._base_infifo_in_valid = self.var("base_infifo_in_valid", 1)
        # Stupid convert -
        self._base_infifo_in_packed = self.var(f"base_infifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._base_infifo_in_packed[self.data_width], self._cmrg_coord_in_eos_in[0])
        self.wire(self._base_infifo_in_packed[self.data_width - 1, 0], self._cmrg_coord_in[0][self.data_width - 1, 0])

        self._base_infifo_out_packed = self.var(f"base_infifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._base_infifo_in_eos, self._base_infifo_out_packed[self.data_width])
        self.wire(self._base_infifo_in_data, self._base_infifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"base_infifo",
                       base_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._cmrg_coord_in_valid_in[0],
                       pop=self._cmrg_fifo_pop[0],
                       data_in=self._base_infifo_in_packed,
                       data_out=self._base_infifo_out_packed)

        self.wire(self._base_infifo_in_valid, ~base_infifo.ports.empty)
        self.wire(self._cmrg_coord_in_ready_out[0], ~base_infifo.ports.full)

        ##############
        # PROC infifo
        ##############
        self._proc_infifo_in_data = self.var("proc_infifo_in_data", 16)
        self._proc_infifo_in_eos = self.var("proc_infifo_in_eos", 1)
        self._proc_infifo_in_valid = self.var("proc_infifo_in_valid", 1)

        self._proc_infifo_in_packed = self.var(f"proc_infifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._proc_infifo_in_packed[self.data_width], self._cmrg_coord_in_eos_in[1])
        self.wire(self._proc_infifo_in_packed[self.data_width - 1, 0], self._cmrg_coord_in[1][self.data_width - 1, 0])

        self._proc_infifo_out_packed = self.var(f"proc_infifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._proc_infifo_in_eos, self._proc_infifo_out_packed[self.data_width])
        self.wire(self._proc_infifo_in_data, self._proc_infifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"proc_infifo",
                       proc_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._cmrg_coord_in_valid_in[1],
                       pop=self._cmrg_fifo_pop[1],
                       data_in=self._proc_infifo_in_packed,
                       data_out=self._proc_infifo_out_packed)

        self.wire(self._proc_infifo_in_valid, ~proc_infifo.ports.empty)
        self.wire(self._cmrg_coord_in_ready_out[1], ~proc_infifo.ports.full)

        ####################
        # HELPER LOGIC FOR FSM
        ####################

        self._base_data_seen = self.var("base_data_seen", 1)
        self.wire(self._base_data_seen, self._base_infifo_in_valid & ~self._base_infifo_in_eos)
        self._proc_data_seen = self.var("proc_data_seen", 1)
        self.wire(self._proc_data_seen, self._proc_infifo_in_valid & ~self._proc_infifo_in_eos)

        self._eos_seen = self.var("base_eos_seen", 1)
        self.wire(self._eos_seen, self._base_infifo_in_valid & self._base_infifo_in_eos & (self._base_infifo_in_data[9, 8] == kts.const(0, 2)))

        self._done_seen = self.var("base_done_seen", 1)
        self.wire(self._done_seen, self._base_infifo_in_valid & self._base_infifo_in_eos & (self._base_infifo_in_data[9, 8] == kts.const(1, 2)))

        self._clr_pushed_proc = self.var("clr_pushed_proc", 1)
        self._pushed_proc = sticky_flag(self, self._cmrg_fifo_push[1], clear=self._clr_pushed_proc, name="pushed_proc", seq_only=True)

        self._clr_pushed_stop_lvl = self.var("clr_pushed_stop_lvl", 1)
        self._pushed_stop_lvl = sticky_flag(self, self._cmrg_fifo_push[0] & self._base_infifo_in_valid & self._base_infifo_in_eos,
                                            clear=self._clr_pushed_stop_lvl, name="pushed_stop_lvl", seq_only=True)

        self._both_done = self.var("both_done", 1)
        self.wire(self._both_done, self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & self._proc_infifo_in_eos &
                  (self._base_infifo_in_data[9, 8] == kts.const(1, 2)) & (self._proc_infifo_in_data[9, 8] == kts.const(1, 2)))

        self._pushing_s0 = self.var("pushing_done", 1)
        self.wire(self._pushing_s0, self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & self._proc_infifo_in_eos &
                  (self._base_infifo_in_data[9, 8] == kts.const(1, 2)) & (self._proc_infifo_in_data[9, 8] == kts.const(1, 2)) & ~base_infifo.ports.full & ~proc_infifo.ports.full)

        ####################
        # STATE MACHINE TO PROCESS PROC STREAM
        ####################

        # Create FSM
        self.proc_fsm = self.add_fsm("proc_seq", reset_high=False)
        START = self.proc_fsm.add_state("START")
        DATA_SEEN = self.proc_fsm.add_state("DATA_SEEN")
        STRIP = self.proc_fsm.add_state("STRIP")
        PASS_STOP = self.proc_fsm.add_state("PASS_STOP")
        PASS_DONE = self.proc_fsm.add_state("PASS_DONE")
        DONE = self.proc_fsm.add_state("DONEX")

        ####################
        # Next State Logic
        ####################

        ####################
        # START #
        ####################
        # IN the START state, we are waiting to see data in a stream
        # to know to pass on the processed stream
        # If we hit the EOS without seeing data, we should strip it
        START.next(DATA_SEEN, self._base_data_seen & self._tile_en)
        START.next(STRIP, self._eos_seen & self._tile_en)
        START.next(PASS_STOP, self._done_seen & self._tile_en)
        START.next(START, None)

        ####################
        # DATA_SEEN #
        ####################
        # In DATA SEEN, we want to pass thru any data including the
        # stop token at the correct level, then go to the pass_stop logic. Make sure we also pushed the proc stream once
        # DATA_SEEN.next(PASS_STOP, self._pushed_proc & self._pushed_stop_lvl)
        DATA_SEEN.next(START, self._pushed_proc & self._pushed_stop_lvl)
        # DATA_SEEN.next(PASS_STOP, self._pushed_proc & (self._base_infifo_in_eos & self._base_infifo_in_valid & (self._base_infifo_in_data[9, 8] == kts.const(0, 2))))
        DATA_SEEN.next(DATA_SEEN, None)

        ####################
        # STRIP #
        ####################
        # At strip, we just want to strip one data from proc and strip the stop token from base
        # Once that is done, we can move onto draining the stop tokens from both if there are any...
        # STRIP.next(PASS_STOP, self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & ~self._proc_infifo_in_eos)
        STRIP.next(START, self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & ~self._proc_infifo_in_eos)
        STRIP.next(STRIP, None)

        ####################
        # PASS_STOP #
        ####################
        # IN PASS_STOP, there are two possibilities - the proc stream either hits S0 or has data again
        # since every hierarchically issued coordinate has a chance to have a non-null payload
        # PASS_STOP.next(START, self._proc_data_seen)
        PASS_STOP.next(PASS_DONE, self._both_done)
        PASS_STOP.next(PASS_STOP, None)

        ####################
        # PASS_DONE #
        ####################
        # IN PASS_STOP, there are two possibilities - the proc stream either hits S0 or has data again
        # since every hierarchically issued coordinate has a chance to have a non-null payload
        # PASS_STOP.next(START, self._proc_data_seen)
        PASS_DONE.next(DONE, self._pushing_s0)
        PASS_DONE.next(PASS_DONE, None)

        ####################
        # DONE #
        ####################
        DONE.next(DONE, None)

        ####################
        # FSM Output Logic
        ####################

        self.proc_fsm.output(self._cmrg_fifo_pop[0])
        self.proc_fsm.output(self._cmrg_fifo_pop[1])
        self.proc_fsm.output(self._cmrg_fifo_push[0])
        self.proc_fsm.output(self._cmrg_fifo_push[1])
        self.proc_fsm.output(self._clr_pushed_proc)
        self.proc_fsm.output(self._clr_pushed_stop_lvl)

        ################
        # START
        ################
        START.output(self._cmrg_fifo_pop[0], 0)
        START.output(self._cmrg_fifo_pop[1], 0)
        START.output(self._cmrg_fifo_push[0], 0)
        START.output(self._cmrg_fifo_push[1], 0)
        # Force these to 0 (but does this consume power?)
        START.output(self._clr_pushed_proc, 1)
        START.output(self._clr_pushed_stop_lvl, 1)

        ################
        # DATA_SEEN
        ################
        DATA_SEEN.output(self._cmrg_fifo_pop[0], ~self._pushed_stop_lvl & ~base_outfifo.ports.full)
        # DATA_SEEN.output(self._cmrg_fifo_pop[0], self._base_infifo_in_valid & ~self._base_infifo_in_eos & ~base_outfifo.ports.full)
        DATA_SEEN.output(self._cmrg_fifo_pop[1], ~self._pushed_proc & ~proc_outfifo.ports.full)
        DATA_SEEN.output(self._cmrg_fifo_push[0], ~self._pushed_stop_lvl & self._base_infifo_in_valid)
        # DATA_SEEN.output(self._cmrg_fifo_push[0],  self._base_infifo_in_valid & ~self._base_infifo_in_eos)
        DATA_SEEN.output(self._cmrg_fifo_push[1], ~self._pushed_proc & self._proc_infifo_in_valid & ~self._proc_infifo_in_eos)
        DATA_SEEN.output(self._clr_pushed_proc, 0)
        DATA_SEEN.output(self._clr_pushed_stop_lvl, 0)

        ################
        # STRIP
        ################
        # Pop both fifos when they are joined
        STRIP.output(self._cmrg_fifo_pop[0], self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & ~self._proc_infifo_in_eos)
        STRIP.output(self._cmrg_fifo_pop[1], self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & ~self._proc_infifo_in_eos)
        STRIP.output(self._cmrg_fifo_push[0], 0)
        STRIP.output(self._cmrg_fifo_push[1], 0)
        STRIP.output(self._clr_pushed_proc, 0)
        STRIP.output(self._clr_pushed_stop_lvl, 0)

        ################
        # PASS_STOP
        ################
        # TODO
        # Here we have base done, need to drain to proc done
        PASS_STOP.output(self._cmrg_fifo_pop[0], 0)
        PASS_STOP.output(self._cmrg_fifo_pop[1], self._proc_infifo_in_valid & self._proc_infifo_in_eos & (self._proc_infifo_in_data[9, 8] == kts.const(0, 2)) & ~proc_outfifo.ports.full)
        PASS_STOP.output(self._cmrg_fifo_push[0], 0)
        PASS_STOP.output(self._cmrg_fifo_push[1], self._proc_infifo_in_valid & self._proc_infifo_in_eos & (self._proc_infifo_in_data[9, 8] == kts.const(0, 2)))
        # PASS_STOP.output(self._cmrg_fifo_pop[0], self._base_infifo_in_valid & self._proc_infifo_in_valid & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # PASS_STOP.output(self._cmrg_fifo_pop[1], self._base_infifo_in_valid & self._proc_infifo_in_valid & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # PASS_STOP.output(self._cmrg_fifo_push[0], self._base_infifo_in_valid & self._proc_infifo_in_valid)
        # PASS_STOP.output(self._cmrg_fifo_push[1], self._base_infifo_in_valid & self._proc_infifo_in_valid)
        PASS_STOP.output(self._clr_pushed_proc, 0)
        PASS_STOP.output(self._clr_pushed_stop_lvl, 0)

        ################
        # PASS_DONE
        ################
        # TODO
        # Here we have base done, need to drain to proc done
        PASS_DONE.output(self._cmrg_fifo_pop[0], self._pushing_s0)
        PASS_DONE.output(self._cmrg_fifo_pop[1], self._pushing_s0)
        PASS_DONE.output(self._cmrg_fifo_push[0], self._pushing_s0)
        PASS_DONE.output(self._cmrg_fifo_push[1], self._pushing_s0)
        # PASS_DONE.output(self._cmrg_fifo_pop[0], self._base_infifo_in_valid & self._proc_infifo_in_valid & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # PASS_DONE.output(self._cmrg_fifo_pop[1], self._base_infifo_in_valid & self._proc_infifo_in_valid & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # PASS_DONE.output(self._cmrg_fifo_push[0], self._base_infifo_in_valid & self._proc_infifo_in_valid)
        # PASS_DONE.output(self._cmrg_fifo_push[1], self._base_infifo_in_valid & self._proc_infifo_in_valid)
        PASS_DONE.output(self._clr_pushed_proc, 0)
        PASS_DONE.output(self._clr_pushed_stop_lvl, 0)

        ################
        # DONE
        ################
        # TODO
        DONE.output(self._cmrg_fifo_pop[0], 0)
        DONE.output(self._cmrg_fifo_pop[1], 0)
        DONE.output(self._cmrg_fifo_push[0], 0)
        DONE.output(self._cmrg_fifo_push[1], 0)
        DONE.output(self._clr_pushed_proc, 0)
        DONE.output(self._clr_pushed_stop_lvl, 0)

        self.proc_fsm.set_start_state(START)

        ################
        # OUTPUT FIFOS #
        ################

        ##############
        # BASE outfifo
        ##############
        # Use this stream to process the PROC stream...
        # self._base_outfifo_in_data = self.var("base_outfifo_in_data", 16)
        # self._base_outfifo_in_eos = self.var("base_outfifo_in_eos", 1)
        # Stupid convert -
        self._base_outfifo_in_packed = self.var(f"base_outfifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._base_outfifo_in_packed[self.data_width], self._base_infifo_in_eos)
        self.wire(self._base_outfifo_in_packed[self.data_width - 1, 0], self._base_infifo_in_data)

        self._base_outfifo_out_packed = self.var(f"base_outfifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._cmrg_coord_out[0][self.data_width], self._base_outfifo_out_packed[self.data_width])
        self.wire(self._cmrg_coord_out[0][self.data_width - 1, 0], self._base_outfifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"base_outfifo",
                       base_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._cmrg_fifo_push[0],
                       pop=self._cmrg_coord_out_ready_in[0],
                       data_in=self._base_outfifo_in_packed,
                       data_out=self._base_outfifo_out_packed)

        self.wire(self._cmrg_coord_out_valid_out[0], ~base_outfifo.ports.empty)

        ##############
        # PROC outfifo
        ##############
        # self._proc_outfifo_in_data = self.var("proc_outfifo_in_data", 16)
        # self._proc_outfifo_in_eos = self.var("proc_outfifo_in_eos", 1)

        self._proc_outfifo_in_packed = self.var(f"proc_outfifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._proc_outfifo_in_packed[self.data_width], self._proc_infifo_in_eos)
        self.wire(self._proc_outfifo_in_packed[self.data_width - 1, 0], self._proc_infifo_in_data)

        self._proc_outfifo_out_packed = self.var(f"proc_outfifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._cmrg_coord_out[1][self.data_width], self._proc_outfifo_out_packed[self.data_width])
        self.wire(self._cmrg_coord_out[1][self.data_width - 1, 0], self._proc_outfifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"proc_outfifo",
                       proc_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._cmrg_fifo_push[1],
                       pop=self._cmrg_coord_out_ready_in[1],
                       data_in=self._proc_outfifo_in_packed,
                       data_out=self._proc_outfifo_out_packed)

        self.wire(self._cmrg_coord_out_valid_out[1], ~proc_outfifo.ports.empty)

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
