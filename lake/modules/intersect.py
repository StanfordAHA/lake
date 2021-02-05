from math import e
import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import trim_config_list, add_counter
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class Intersect(Generator):
    def __init__(self,
                 data_width=16):

        super().__init__("intersect_unit", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True

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

        # Scanner interface will need
        # input data, input valid
        # output address, output valid
        self._coord_in = self.input("coord_in", self.data_width,
                                    size=self.num_streams,
                                    packed=True,
                                    explicit_array=True)
        self._coord_in.add_attribute(ControlSignalAttr(is_control=False))

        # Are incoming guys valid/eos?
        self._valid_in = self.input("valid_in", self.num_streams)
        self._valid_in.add_attribute(ControlSignalAttr(is_control=True))
        self._eos_in = self.input("eos_in", self.num_streams)
        self._eos_in.add_attribute(ControlSignalAttr(is_control=True))

        # Pop the incoming guys
        self._ready_out = self.output("ready_out", self.num_streams)
        self._ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # BackPRESH
        self._ready_in = self.input("ready_in", 1)
        self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        # only send one common coord out for now.
        self._coord_out = self.output("coord_out", self.data_width)
        self._coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        # but need to send two positions...
        self._pos_out = self.output("pos_out", self.data_width,
                                    size=self.num_streams,
                                    explicit_array=True,
                                    packed=True)
        self._pos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Can broadcast the valid and eos out
        self._valid_out = self.output("valid_out", 1)
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._eos_out = self.output("eos_out", 1)
        self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Intermediates
        self._pos_cnt = self.var("pos_cnt", self.data_width,
                                 size=self.num_streams,
                                 explicit_array=True,
                                 packed=True)

# ==========================================
# Generate FSM for Intersecting these streams...
# ==========================================

        self._all_valid = self.var("all_valid", 1)
        self.wire(self._all_valid, self._valid_in.r_and())

        self._any_eos = self.var("any_eos", 1)
        self._gate_eos = self.var("gate_eos", 2)
        # for i in range(self._gate_eos.width):
        # TODO: Deal with stream of length 0
        self.wire(self._gate_eos[0], self._eos_in[0] & ((self._coord_in[0] <= self._coord_in[1])))
        self.wire(self._gate_eos[1], self._eos_in[1] & (self._coord_in[0] >= self._coord_in[1]))
        self.wire(self._any_eos, self._gate_eos.r_or())

        # Check if we have seen both eos to drain both inputs and start again
        self._eos_seen = self.var("eos_seen", self.num_streams)
        self._eos_seen_set = self.var("eos_seen_set", self.num_streams)
        self._eos_seen_clr = self.var("eos_seen_clr", self.num_streams)
        for i in range(self.num_streams):
            @always_ff((posedge, "clk"), (negedge, "rst_n"))
            def eos_seen_ff():
                if ~self._rst_n:
                    self._eos_seen[i] = 0
                elif self._eos_seen_clr[i]:
                    self._eos_seen[i] = 0
                elif self._eos_seen_set[i]:
                    self._eos_seen[i] = 1
            self.add_code(eos_seen_ff)
        self._all_eos_seen = self.var("all_eos_seen", 1)
        self.wire(self._all_eos_seen, self._eos_seen.r_and())

        # Control Vars from FSM
        self._inc_pos_cnt = self.var("inc_pos_cnt", self.num_streams)
        self._rst_pos_cnt = self.var("rst_pos_cnt", self.num_streams)

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
        self._fifo_full = self.var("fifo_full", 1)

        # Create FSM
        self.intersect_fsm = self.add_fsm("intersect_seq", reset_high=False)
        IDLE = self.intersect_fsm.add_state("IDLE")
        ITER = self.intersect_fsm.add_state("ITER")
        DRAIN = self.intersect_fsm.add_state("DRAIN")
        DONE = self.intersect_fsm.add_state("DONE")

        self.intersect_fsm.output(self._inc_pos_cnt[0])
        self.intersect_fsm.output(self._inc_pos_cnt[1])
        self.intersect_fsm.output(self._rst_pos_cnt[0])
        self.intersect_fsm.output(self._rst_pos_cnt[1])
        # self.intersect_fsm.output(self._ready_out)
        self.intersect_fsm.output(self._fifo_push)
        self.intersect_fsm.output(self._eos_seen_set[0])
        self.intersect_fsm.output(self._eos_seen_set[1])
        self.intersect_fsm.output(self._eos_seen_clr[0])
        self.intersect_fsm.output(self._eos_seen_clr[1])

        ####################
        # Next State Logic
        ####################

        # In IDLE we stay if the fifo is full, otherwise wait
        # until we have two valids...
        IDLE.next(IDLE, self._fifo_full | (~self._all_valid))
        IDLE.next(ITER, self._all_valid)

        # In ITER, we go back to idle when the fifo is full to avoid
        # complexity, or if we are looking at one of the eos since we can make the last
        # move for the intersection now...
        # If we have eos and can push it to the fifo, we are done with this stream
        ITER.next(DRAIN, self._any_eos & ~self._fifo_full)
        # ITER.next(IDLE, self._fifo_full)
        ITER.next(ITER, kts.const(1, 1))

        DRAIN.next(DONE, self._all_eos_seen)
        DRAIN.next(DRAIN, ~self._all_eos_seen)

        # Once done, we need another flush
        # Just go back to beginning
        DONE.next(IDLE, kts.const(1, 1))

        ####################
        # FSM Output Logic
        ####################

        #######
        # IDLE - TODO - Generate general hardware...
        #######
        IDLE.output(self._inc_pos_cnt[0], 0)
        IDLE.output(self._inc_pos_cnt[1], 0)
        IDLE.output(self._rst_pos_cnt[0], 0)
        IDLE.output(self._rst_pos_cnt[1], 0)
        IDLE.output(self._fifo_push, 0)
        IDLE.output(self._eos_seen_set[0], 0)
        IDLE.output(self._eos_seen_set[1], 0)
        IDLE.output(self._eos_seen_clr[0], 0)
        IDLE.output(self._eos_seen_clr[1], 0)

        #######
        # ITER
        #######
        ITER.output(self._inc_pos_cnt[0], (self._all_valid & (self._coord_in[0] <= self._coord_in[1])) & ~self._fifo_full)
        ITER.output(self._inc_pos_cnt[1], (self._all_valid & (self._coord_in[0] >= self._coord_in[1])) & ~self._fifo_full)
        ITER.output(self._rst_pos_cnt[0], self._any_eos & ~self._fifo_full)
        ITER.output(self._rst_pos_cnt[1], self._any_eos & ~self._fifo_full)
        # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe
        ITER.output(self._fifo_push, (self._any_eos | (self._all_valid & (self._coord_in[0] == self._coord_in[1]))) & ~self._fifo_full)
        ITER.output(self._eos_seen_set[0], self._gate_eos[0])
        ITER.output(self._eos_seen_set[1], self._gate_eos[1])
        ITER.output(self._eos_seen_clr[0], 0)
        ITER.output(self._eos_seen_clr[1], 0)

        #######
        # DRAIN
        #######
        DRAIN.output(self._inc_pos_cnt[0], ~self._eos_seen[0])
        DRAIN.output(self._inc_pos_cnt[1], ~self._eos_seen[1])
        DRAIN.output(self._rst_pos_cnt[0], self._all_eos_seen)
        DRAIN.output(self._rst_pos_cnt[1], self._all_eos_seen)
        DRAIN.output(self._fifo_push, 0)
        DRAIN.output(self._eos_seen_set[0], self._eos_in[0])
        DRAIN.output(self._eos_seen_set[1], self._eos_in[1])
        DRAIN.output(self._eos_seen_clr[0], self._all_eos_seen)
        DRAIN.output(self._eos_seen_clr[1], self._all_eos_seen)

        #######
        # DONE
        #######
        DONE.output(self._inc_pos_cnt[0], 0)
        DONE.output(self._inc_pos_cnt[1], 0)
        DONE.output(self._rst_pos_cnt[0], 0)
        DONE.output(self._rst_pos_cnt[1], 0)
        DONE.output(self._fifo_push, 0)
        DONE.output(self._eos_seen_set[0], 0)
        DONE.output(self._eos_seen_set[1], 0)
        DONE.output(self._eos_seen_clr[0], 0)
        DONE.output(self._eos_seen_clr[1], 0)

        self.intersect_fsm.set_start_state(IDLE)

        # Incrementing the pos cnt == popping the incoming stream
        self.wire(self._ready_out, self._inc_pos_cnt)
# ===================================
# Dump metadata into fifo
# ===================================
        self._rfifo = RegFIFO(data_width=3 * self.data_width + 2, width_mult=1, depth=16)

        # Stupid convert -
        self._data_in_packed = self.var("fifo_in_packed", 3 * self.data_width + 2, packed=True)
        self.wire(self._data_in_packed[3 * self.data_width + 1], (self._all_valid & (self._coord_in[0] == self._coord_in[1])))
        self.wire(self._data_in_packed[3 * self.data_width], self._any_eos)
        self.wire(self._data_in_packed[3 * self.data_width - 1, 2 * self.data_width], self._pos_cnt[1])
        self.wire(self._data_in_packed[2 * self.data_width - 1, 1 * self.data_width], self._pos_cnt[0])
        self.wire(self._data_in_packed[1 * self.data_width - 1, 0 * self.data_width], self._coord_in[0])

        self._data_out_packed = self.var("fifo_out_packed", 3 * self.data_width + 2, packed=True)
        self.wire(self._valid_out, self._data_out_packed[3 * self.data_width + 1] & (~self._rfifo.ports.empty))
        self.wire(self._eos_out, self._data_out_packed[3 * self.data_width])
        self.wire(self._pos_out[1], self._data_out_packed[3 * self.data_width - 1, 2 * self.data_width])
        self.wire(self._pos_out[0], self._data_out_packed[2 * self.data_width - 1, 1 * self.data_width])
        self.wire(self._coord_out, self._data_out_packed[1 * self.data_width - 1, 0 * self.data_width])

        self._fifo_valid_entry = self.var("fifo_valid_entry", 1)

        self.add_child(f"coordinate_fifo",
                       self._rfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._fifo_push,
                       pop=(self._fifo_valid_entry & self._ready_in),
                       data_in=self._data_in_packed,
                       data_out=self._data_out_packed,
                       valid=self._fifo_valid_entry,
                       full=self._fifo_full)

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

    def get_bitstream(self):

        # Store all configurations here
        config = []
        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Trim the list
        return trim_config_list(flattened, config)


if __name__ == "__main__":
    intersect_dut = Intersect(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(intersect_dut, filename="intersect.sv",
            optimize_if=False)
