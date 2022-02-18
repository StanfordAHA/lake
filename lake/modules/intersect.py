from math import e
import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import sticky_flag, trim_config_list, add_counter
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class Intersect(kts.Generator):
    def __init__(self,
                 data_width=16,
                 use_merger=False):

        name_str = f"intersect_unit{'_w_merger' if use_merger else ''}"
        super().__init__(name=name_str, debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True
        self.use_merger = use_merger

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

        self._pos_in = self.input("pos_in", self.data_width,
                                  size=self.num_streams,
                                  packed=True,
                                  explicit_array=True)
        self._pos_in.add_attribute(ControlSignalAttr(is_control=False))

        # self._o_coord_in = self.input("o_coord_in", self.data_width,
        #                               size=self.num_streams,
        #                               packed=True,
        #                               explicit_array=True)
        # self._o_coord_in.add_attribute(ControlSignalAttr(is_control=False))

        # Need offsets for memory access
        # self._payload_ptr = self.input("payload_ptr", 16,
        #                                size=self.num_streams,
        #                                packed=True,
        #                                explicit_array=True)
        # self._payload_ptr.add_attribute(ControlSignalAttr(is_control=False))

        # Are incoming guys valid/eos?
        self._valid_in = self.input("valid_in", self.num_streams * 2)
        self._valid_in.add_attribute(ControlSignalAttr(is_control=True))
        self._eos_in = self.input("eos_in", self.num_streams * 2)
        self._eos_in.add_attribute(ControlSignalAttr(is_control=True))

        # Pop the incoming guys
        self._ready_out = self.output("ready_out", self.num_streams * 2)
        self._ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # BackPRESH, 3 different channels
        self._ready_in = self.input("ready_in", 3)
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

        # self._o_coord_out = self.output("o_coord_out", self.data_width,
        #                                 size=self.num_streams,
        #                                 explicit_array=True,
        #                                 packed=True)
        # self._o_coord_out.add_attribute(ControlSignalAttr(is_control=False))

        # Can broadcast the valid and eos out
        self._valid_out = self.output("valid_out", 3)
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._eos_out = self.output("eos_out", 3)
        self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Create sticky bits for seeing EOS on either side...
        self._eos_in_sticky = self.var("eos_in_sticky", self.num_streams)
        self._clr_eos_sticky = self.var("clr_eos_sticky", self.num_streams)
        for i in range(self.num_streams):
            tmp_sticky = sticky_flag(self, self._eos_in[i], clear=self._clr_eos_sticky[i], name=f"eos_sticky_{i}")
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

        self._any_eos = self.var("any_eos", 1)

        self.wire(self._all_valid, self._valid_in.r_and() & ~self._any_eos)
        # self._gate_eos = self.var("gate_eos", 2)
        # for i in range(self._gate_eos.width):
        # TODO: Deal with stream of length 0
        # self.wire(self._gate_eos[0], self._eos_in[0] & ((self._coord_in[0] <= self._coord_in[1]) | ~self._valid_in[0]))
        # self.wire(self._gate_eos[1], self._eos_in[1] & ((self._coord_in[0] >= self._coord_in[1]) | ~self._valid_in[1]))
        # self.wire(self._any_eos, self._gate_eos.r_or())

        self.wire(self._any_eos, self._eos_in.r_or())

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
        self._fifo_full = self.var("fifo_full", 3)

        # Swap ins for FIFO
        self._coord_to_fifo = self.var("coord_to_fifo", 16)
        self._pos_to_fifo = self.var("pos_to_fifo", 16, size=self.num_streams, explicit_array=True, packed=True)

        # Create FSM
        self.intersect_fsm = self.add_fsm("intersect_seq", reset_high=False)
        IDLE = self.intersect_fsm.add_state("IDLE")
        ITER = self.intersect_fsm.add_state("ITER")
        DRAIN = self.intersect_fsm.add_state("DRAIN")
        ALIGN = self.intersect_fsm.add_state("ALIGN")
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
        self.intersect_fsm.output(self._pos_to_fifo[0])
        self.intersect_fsm.output(self._pos_to_fifo[1])

        ####################
        # Next State Logic
        ####################

        # In IDLE we stay if the fifo is full, otherwise wait
        # until we have two valids...
        IDLE.next(IDLE, self._fifo_full.r_or() | (~self._all_valid))
        # If either stream is empty, we can skip to drain right away
        IDLE.next(ALIGN, self._any_eos)
        IDLE.next(ITER, self._all_valid)

        # In ITER, we go back to idle when the fifo is full to avoid
        # complexity, or if we are looking at one of the eos since we can make the last
        # move for the intersection now...
        # If we have eos and can push it to the fifo, we are done with this stream
        ITER.next(ALIGN, self._any_eos)
        ITER.next(ITER, None)

        # First we align the streams to both stop tokens
        ALIGN.next(DRAIN, self._eos_in_sticky.r_and())
        ALIGN.next(ALIGN, None)

        # Then in DRAIN, we pass thru the stop tokens
        # The only way to leave DRAIN is to get new data
        # where both streams are valid but not both streams are eos
        # DRAIN.next(DONE, ~self._any_eos & self._all_valid)
        DRAIN.next(DONE, ~self._eos_in.r_and() & self._valid_in.r_and())
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

        #######
        # ITER
        #######
        ITER.output(self._inc_pos_cnt[0], (self._all_valid & (self._coord_in[0] <= self._coord_in[1])) & ~self._fifo_full.r_or())
        ITER.output(self._inc_pos_cnt[1], (self._all_valid & (self._coord_in[0] >= self._coord_in[1])) & ~self._fifo_full.r_or())
        ITER.output(self._rst_pos_cnt[0], self._any_eos & ~self._fifo_full.r_or())
        ITER.output(self._rst_pos_cnt[1], self._any_eos & ~self._fifo_full.r_or())
        # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe
        ITER.output(self._fifo_push, self._all_valid & (self._coord_in[0] == self._coord_in[1]) & ~self._fifo_full.r_or() & ~self._any_eos)
        ITER.output(self._clr_eos_sticky[0], 0)
        ITER.output(self._clr_eos_sticky[1], 0)
        ITER.output(self._coord_to_fifo, self._coord_in[0])
        # ITER.output(self._pos_to_fifo[0], self._pos_cnt[0] + self._payload_ptr[0])
        # ITER.output(self._pos_to_fifo[1], self._pos_cnt[1] + self._payload_ptr[1])
        ITER.output(self._pos_to_fifo[0], self._pos_in[0])
        ITER.output(self._pos_to_fifo[1], self._pos_in[1])

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

        #######
        # DRAIN
        #######
        DRAIN.output(self._inc_pos_cnt[0], ~self._fifo_full.r_or() & self._eos_in[0] & self._valid_in.r_and())
        DRAIN.output(self._inc_pos_cnt[1], ~self._fifo_full.r_or() & self._eos_in[0] & self._valid_in.r_and())
        DRAIN.output(self._rst_pos_cnt[0], 0)
        DRAIN.output(self._rst_pos_cnt[1], 0)
        # Keep draining while we have eos in...should be aligned
        DRAIN.output(self._fifo_push, ~self._fifo_full.r_or() & self._eos_in[0] & self._valid_in.r_and())
        DRAIN.output(self._clr_eos_sticky[0], 0)
        DRAIN.output(self._clr_eos_sticky[1], 0)
        # TODO
        DRAIN.output(self._coord_to_fifo, self._coord_in[0])
        DRAIN.output(self._pos_to_fifo[0], self._coord_in[0])
        DRAIN.output(self._pos_to_fifo[1], self._coord_in[0])

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

        self.intersect_fsm.set_start_state(IDLE)

        # Incrementing the pos cnt == popping the incoming stream
        self.wire(self._ready_out[0], self._inc_pos_cnt[0])
        self.wire(self._ready_out[1], self._inc_pos_cnt[1])
        self.wire(self._ready_out[2], self._inc_pos_cnt[0])
        self.wire(self._ready_out[3], self._inc_pos_cnt[1])
# ===================================
# Dump metadata into fifo
# ===================================
        self._coord_fifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=16)
        self._pos0_fifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=16)
        self._pos1_fifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=16)

        # Stupid convert -
        self._coord_data_in_packed = self.var("coord_fifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._coord_data_in_packed[self.data_width], self._any_eos)
        self.wire(self._coord_data_in_packed[self.data_width - 1, 0 * self.data_width], self._coord_to_fifo)

        self._coord_data_out_packed = self.var("coord_fifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._eos_out[0], self._coord_data_out_packed[self.data_width])
        self.wire(self._coord_out, self._coord_data_out_packed[self.data_width - 1, 0 * self.data_width])

        self._pos0_data_in_packed = self.var("pos0_fifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._pos0_data_in_packed[self.data_width], self._any_eos)
        self.wire(self._pos0_data_in_packed[self.data_width - 1, 0 * self.data_width], self._pos_to_fifo[0])

        self._pos0_data_out_packed = self.var("pos0_fifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._eos_out[1], self._pos0_data_out_packed[self.data_width])
        self.wire(self._pos_out[0], self._pos0_data_out_packed[self.data_width - 1, 0 * self.data_width])

        self._pos1_data_in_packed = self.var("pos1_fifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._pos1_data_in_packed[self.data_width], self._any_eos)
        self.wire(self._pos1_data_in_packed[self.data_width - 1, 0 * self.data_width], self._pos_to_fifo[1])

        self._pos1_data_out_packed = self.var("pos1_fifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._eos_out[2], self._pos1_data_out_packed[self.data_width])
        self.wire(self._pos_out[1], self._pos1_data_out_packed[self.data_width - 1, 0 * self.data_width])

        self.add_child(f"coordinate_fifo",
                       self._coord_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._fifo_push,
                       pop=self._ready_in[0],
                       data_in=self._coord_data_in_packed,
                       data_out=self._coord_data_out_packed,
                       full=self._fifo_full[0])

        self.add_child(f"pos0_fifo",
                       self._pos0_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._fifo_push,
                       pop=self._ready_in[1],
                       data_in=self._pos0_data_in_packed,
                       data_out=self._pos0_data_out_packed,
                       full=self._fifo_full[1])

        self.add_child(f"pos1_fifo",
                       self._pos1_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._fifo_push,
                       pop=self._ready_in[2],
                       data_in=self._pos1_data_in_packed,
                       data_out=self._pos1_data_out_packed,
                       full=self._fifo_full[2])

        self.wire(self._valid_out[0], ~self._coord_fifo.ports.empty)
        self.wire(self._valid_out[1], ~self._pos0_fifo.ports.empty)
        self.wire(self._valid_out[2], ~self._pos1_fifo.ports.empty)
        # Build in the merging logic.
        if use_merger:
            self.add_merging_logic()

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

    def get_bitstream(self, cmrg_enable=0, cmrg_stop_lvl=0):

        # Store all configurations here
        config = [("tile_en", 1)]

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
        self._cmrg_ready_in = self.input("cmrg_ready_in", 2)
        self._cmrg_ready_in.add_attribute(ControlSignalAttr(is_control=False))

        self._cmrg_valid_out = self.output("cmrg_valid_out", 2)
        self._cmrg_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._cmrg_eos_out = self.output("cmrg_eos_out", 2)
        self._cmrg_eos_out.add_attribute(ControlSignalAttr(is_control=False))

        self._cmrg_ready_out = self.output("cmrg_ready_out", 2)
        self._cmrg_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._cmrg_coord_out = self.output(f"cmrg_coord_out", self.data_width,
                                           size=2, packed=True, explicit_array=True)
        self._cmrg_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # Coords in
        self._cmrg_coord_in = self.input(f"cmrg_coord_in", self.data_width,
                                         size=2, packed=True, explicit_array=True)
        self._cmrg_coord_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._cmrg_valid_in = self.input("cmrg_valid_in", 2)
        self._cmrg_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._cmrg_eos_in = self.input("cmrg_eos_in", 2)
        self._cmrg_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        ####
        self._cmrg_fifo_push = self.var("cmrg_fifo_push", 2)
        self._cmrg_fifo_pop = self.var("cmrg_fifo_pop", 2)

        # Create a bunch of fifos
        fifo_kwargs = {
            "data_width": self.data_width + 1,
            "width_mult": 1,
            "depth": 8
        }

        base_infifo = RegFIFO(**fifo_kwargs)
        proc_infifo = RegFIFO(**fifo_kwargs)
        base_outfifo = RegFIFO(**fifo_kwargs)
        proc_outfifo = RegFIFO(**fifo_kwargs)

        ##############
        # BASE infifo
        ##############
        # Use this stream to process the PROC stream...
        self._base_infifo_in_data = self.var("base_infifo_in_data", 16)
        self._base_infifo_in_eos = self.var("base_infifo_in_eos", 1)
        self._base_infifo_in_valid = self.var("base_infifo_in_valid", 1)
        # Stupid convert -
        self._base_infifo_in_packed = self.var(f"base_infifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._base_infifo_in_packed[self.data_width], self._cmrg_eos_in[0])
        self.wire(self._base_infifo_in_packed[self.data_width - 1, 0], self._cmrg_coord_in[0])

        self._base_infifo_out_packed = self.var(f"base_infifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._base_infifo_in_eos, self._base_infifo_out_packed[self.data_width])
        self.wire(self._base_infifo_in_data, self._base_infifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"base_infifo",
                       base_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._cmrg_valid_in[0],
                       pop=self._cmrg_fifo_pop[0],
                       data_in=self._base_infifo_in_packed,
                       data_out=self._base_infifo_out_packed)

        self.wire(self._base_infifo_in_valid, ~base_infifo.ports.empty)
        self.wire(self._cmrg_ready_out[0], ~base_infifo.ports.full)

        ##############
        # PROC infifo
        ##############
        self._proc_infifo_in_data = self.var("proc_infifo_in_data", 16)
        self._proc_infifo_in_eos = self.var("proc_infifo_in_eos", 1)
        self._proc_infifo_in_valid = self.var("proc_infifo_in_valid", 1)

        self._proc_infifo_in_packed = self.var(f"proc_infifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._proc_infifo_in_packed[self.data_width], self._cmrg_eos_in[1])
        self.wire(self._proc_infifo_in_packed[self.data_width - 1, 0], self._cmrg_coord_in[1])

        self._proc_infifo_out_packed = self.var(f"proc_infifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._proc_infifo_in_eos, self._proc_infifo_out_packed[self.data_width])
        self.wire(self._proc_infifo_in_data, self._proc_infifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"proc_infifo",
                       proc_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._cmrg_valid_in[1],
                       pop=self._cmrg_fifo_pop[1],
                       data_in=self._proc_infifo_in_packed,
                       data_out=self._proc_infifo_out_packed)

        self.wire(self._proc_infifo_in_valid, ~proc_infifo.ports.empty)
        self.wire(self._cmrg_ready_out[1], ~proc_infifo.ports.full)

        ####################
        # HELPER LOGIC FOR FSM
        ####################

        self._base_data_seen = self.var("base_data_seen", 1)
        self.wire(self._base_data_seen, self._base_infifo_in_valid & ~self._base_infifo_in_eos)
        self._proc_data_seen = self.var("proc_data_seen", 1)
        self.wire(self._proc_data_seen, self._proc_infifo_in_valid & ~self._proc_infifo_in_eos)

        self._eos_seen = self.var("eos_seen", 1)
        self.wire(self._eos_seen, self._base_infifo_in_valid & self._base_infifo_in_eos)

        self._clr_pushed_proc = self.var("clr_pushed_proc", 1)
        self._pushed_proc = sticky_flag(self, self._cmrg_fifo_push[1], clear=self._clr_pushed_proc, name="pushed_proc", seq_only=True)

        self._clr_pushed_stop_lvl = self.var("clr_pushed_stop_lvl", 1)
        self._pushed_stop_lvl = sticky_flag(self, self._cmrg_fifo_push[0] & self._base_infifo_in_valid & self._base_infifo_in_eos,
                                            clear=self._clr_pushed_stop_lvl, name="pushed_stop_lvl", seq_only=True)

        self._pushing_s0 = self.var("pushing_s0", 1)
        self.wire(self._pushing_s0, self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & self._proc_infifo_in_eos &
                  (self._base_infifo_in_data == 0) & (self._proc_infifo_in_data == 0) & ~base_infifo.ports.full & ~proc_infifo.ports.full)

        ####################
        # STATE MACHINE TO PROCESS PROC STREAM
        ####################

        # Create FSM
        self.proc_fsm = self.add_fsm("proc_seq", reset_high=False)
        START = self.proc_fsm.add_state("START")
        DATA_SEEN = self.proc_fsm.add_state("DATA_SEEN")
        STRIP = self.proc_fsm.add_state("STRIP")
        PASS_STOP = self.proc_fsm.add_state("PASS_STOP")
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
        START.next(DATA_SEEN, self._base_data_seen)
        START.next(STRIP, self._eos_seen)
        START.next(START, None)

        ####################
        # DATA_SEEN #
        ####################
        # In DATA SEEN, we want to pass thru any data including the
        # stop token at the correct level, then go to the pass_stop logic. Make sure we also pushed the proc stream once
        DATA_SEEN.next(PASS_STOP, self._pushed_proc & self._pushed_stop_lvl)
        DATA_SEEN.next(DATA_SEEN, None)

        ####################
        # STRIP #
        ####################
        # At strip, we just want to strip one data from proc and strip the stop token from base
        # Once that is done, we can move onto draining the stop tokens from both if there are any...
        STRIP.next(PASS_STOP, self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & self._proc_infifo_in_eos)
        STRIP.next(STRIP, None)

        ####################
        # PASS_STOP #
        ####################
        # IN PASS_STOP, there are two possibilities - the proc stream either hits S0 or has data again
        # since every hierarchically issued coordinate has a chance to have a non-null payload
        PASS_STOP.next(START, self._proc_data_seen)
        PASS_STOP.next(DONE, self._pushing_s0)
        PASS_STOP.next(PASS_STOP, None)

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
        DATA_SEEN.output(self._cmrg_fifo_pop[1], ~self._pushed_proc & ~proc_outfifo.ports.full)
        DATA_SEEN.output(self._cmrg_fifo_push[0], ~self._pushed_stop_lvl & self._base_infifo_in_valid)
        DATA_SEEN.output(self._cmrg_fifo_push[1], ~self._pushed_proc & self._proc_infifo_in_valid)
        DATA_SEEN.output(self._clr_pushed_proc, 0)
        DATA_SEEN.output(self._clr_pushed_stop_lvl, 0)

        ################
        # STRIP
        ################
        # Pop both fifos when they are joined
        STRIP.output(self._cmrg_fifo_pop[0], self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & self._proc_infifo_in_eos)
        STRIP.output(self._cmrg_fifo_pop[1], self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & self._proc_infifo_in_eos)
        STRIP.output(self._cmrg_fifo_push[0], 0)
        STRIP.output(self._cmrg_fifo_push[1], 0)
        STRIP.output(self._clr_pushed_proc, 0)
        STRIP.output(self._clr_pushed_stop_lvl, 0)

        ################
        # PASS_STOP
        ################
        # TODO
        PASS_STOP.output(self._cmrg_fifo_pop[0], self._base_infifo_in_valid & self._proc_infifo_in_valid & ~base_infifo.ports.full & ~proc_infifo.ports.full)
        PASS_STOP.output(self._cmrg_fifo_pop[1], self._base_infifo_in_valid & self._proc_infifo_in_valid & ~base_infifo.ports.full & ~proc_infifo.ports.full)
        PASS_STOP.output(self._cmrg_fifo_push[0], self._base_infifo_in_valid & self._proc_infifo_in_valid)
        PASS_STOP.output(self._cmrg_fifo_push[1], self._base_infifo_in_valid & self._proc_infifo_in_valid)
        PASS_STOP.output(self._clr_pushed_proc, 0)
        PASS_STOP.output(self._clr_pushed_stop_lvl, 0)

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
        self.wire(self._cmrg_eos_out[0], self._base_outfifo_out_packed[self.data_width])
        self.wire(self._cmrg_coord_out[0], self._base_outfifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"base_outfifo",
                       base_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._cmrg_fifo_push[0],
                       pop=self._ready_in[0],
                       data_in=self._base_outfifo_in_packed,
                       data_out=self._base_outfifo_out_packed)

        self.wire(self._cmrg_valid_out[0], ~base_outfifo.ports.empty)

        ##############
        # PROC outfifo
        ##############
        # self._proc_outfifo_in_data = self.var("proc_outfifo_in_data", 16)
        # self._proc_outfifo_in_eos = self.var("proc_outfifo_in_eos", 1)

        self._proc_outfifo_in_packed = self.var(f"proc_outfifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._proc_outfifo_in_packed[self.data_width], self._proc_infifo_in_eos)
        self.wire(self._proc_outfifo_in_packed[self.data_width - 1, 0], self._proc_infifo_in_data)

        self._proc_outfifo_out_packed = self.var(f"proc_outfifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._cmrg_eos_out[1], self._proc_outfifo_out_packed[self.data_width])
        self.wire(self._cmrg_coord_out[1], self._proc_outfifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"proc_outfifo",
                       proc_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._cmrg_fifo_push[1],
                       pop=self._ready_in[1],
                       data_in=self._proc_outfifo_in_packed,
                       data_out=self._proc_outfifo_out_packed)

        self.wire(self._cmrg_valid_out[1], ~proc_outfifo.ports.empty)


if __name__ == "__main__":
    intersect_dut = Intersect(data_width=16,
                              use_merger=True)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(intersect_dut, filename="intersect.sv",
            optimize_if=False)
