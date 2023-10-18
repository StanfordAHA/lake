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


class CrdDrop(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 add_clk_enable=True,
                 add_flush=False,
                 lift_config=False,
                 defer_fifos=True,
                 perf_debug=False):

        name_str = f"crddrop"
        super().__init__(name=name_str, debug=True)

        self.data_width = data_width
        self.add_clk_enable = add_clk_enable
        self.add_flush = add_flush
        self.lift_config = lift_config
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

        # Build in the merging logic.
        # if use_merger:
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

        if self.lift_config:
            # Finally, lift the config regs...
            lift_config_reg(self.internal_generator)

    # def get_bitstream(self, cmrg_enable=0, cmrg_stop_lvl=0):
    def get_bitstream(self, config_kwargs):

        cmrg_enable = config_kwargs['cmrg_enable']
        cmrg_stop_lvl = config_kwargs['cmrg_stop_lvl']
        cmrg_mode = config_kwargs['cmrg_mode']

        # Store all configurations here
        config = [("tile_en", 1)]

        config += [("cmrg_enable", cmrg_enable)]
        config += [("cmrg_stop_lvl", cmrg_stop_lvl)]
        config += [("cmrg_mode", cmrg_mode)]

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

        # mode 0: dropping crd stream according to the value stream
        # mode 1: dropping crd streams according to the inner level crd stream
        self._cmrg_mode = self.input("cmrg_mode", 1)
        self._cmrg_mode.add_attribute(ConfigRegAttr("Merge mode"))

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

        if self.perf_debug:

            cyc_count = add_counter(self, "clock_cycle_count", 64, increment=self._clk & self._clk_en)

            # Start when any of the coord inputs is valid
            self._start_signal = sticky_flag(self, kts.concat((*[self._cmrg_coord_in_valid_in[i] for i in range(2)])).r_or(),
                                             name='start_indicator')
            self.add_performance_indicator(self._start_signal, edge='posedge', label='start', cycle_count=cyc_count)

            # End when we see DONE on the output coord
            self._done_signal = sticky_flag(self, (self._cmrg_coord_out[0] == MemoryController.DONE_PROXY) &
                                                    self._cmrg_coord_out[0][MemoryController.EOS_BIT] & self._cmrg_coord_out_valid_out[0],
                                                    name='done_indicator')
            self.add_performance_indicator(self._done_signal, edge='posedge', label='done', cycle_count=cyc_count)

        ####
        # Delayed base value
        self._base_delay = self.var("base_delay", self.data_width + 1)
        self._base_valid_delay = self.var("base_valid_delay", 1)

        self._delay_eos = self.var("delay_eos", 1)
        self.wire(self._delay_eos, self._base_valid_delay & self._base_delay[self.data_width])
        self._delay_data = self.var("delay_data", 1)
        self.wire(self._delay_data, self._base_valid_delay & ~self._delay_eos)
        self._delay_done = self.var("delay_done", 1)
        self.wire(self._delay_done, self._delay_eos & (self._base_delay[9, 8] == kts.const(1, 2)))
        self._delay_stop = self.var("delay_stop", 1)
        self.wire(self._delay_stop, self._delay_eos & (self._base_delay[9, 8] == kts.const(0, 2)))

        self._cmrg_fifo_push = self.var("cmrg_fifo_push", 2)
        self._cmrg_fifo_pop = self.var("cmrg_fifo_pop", 2)

        # Create a bunch of fifos
        fifo_kwargs = {
            "data_width": self.data_width + 1,
            "width_mult": 1,
            "depth": self.fifo_depth,
            'defer_hrdwr_gen': self.defer_fifos
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
        # Create a fake pop to deal with the state after done token
        self._base_infifo_true_pop = self.var("base_infifo_true_pop", 1)
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
                       pop=self._base_infifo_true_pop,
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

        self._set_pushed_data_lower = self.var("set_pushed_data_lower", 1)
        self._clr_pushed_data_lower = self.var("clr_pushed_data_lower", 1)
        self._pushed_data_lower = sticky_flag(self, self._set_pushed_data_lower,
                                              clear=self._clr_pushed_data_lower, name="pushed_data_sticky",
                                              seq_only=True)

        self._eos_seen = self.var("base_eos_seen", 1)
        self.wire(self._eos_seen, self._base_infifo_in_valid & self._base_infifo_in_eos & (self._base_infifo_in_data[9, 8] == kts.const(0, 2)))

        self._done_seen = self.var("base_done_seen", 1)
        self.wire(self._done_seen, self._base_infifo_in_valid & self._base_infifo_in_eos & (self._base_infifo_in_data[9, 8] == kts.const(1, 2)))

        self._base_done = self.var("base_done", 1)
        self.wire(self._base_done, self._base_infifo_in_valid & self._base_infifo_in_eos & (self._base_infifo_in_data[9, 8] == kts.const(1, 2)))

        self._proc_done = self.var("proc_done", 1)
        self.wire(self._proc_done, self._proc_infifo_in_valid & self._proc_infifo_in_eos & (self._proc_infifo_in_data[9, 8] == kts.const(1, 2)))

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
                  (self._base_infifo_in_data[9, 8] == kts.const(1, 2)) & (self._proc_infifo_in_data[9, 8] == kts.const(1, 2)) & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)

        # Fake Pop
        # Useful if we want to process consecutive streams, only used for crddrop and not zerodrop
        self.wire(self._base_infifo_true_pop, kts.ternary(self._cmrg_mode, self._cmrg_fifo_pop[0] & ~(self._delay_stop & self._done_seen), self._cmrg_fifo_pop[0]))

        ####################
        # STATE MACHINE TO PROCESS PROC STREAM
        ####################

        # Create FSM
        self.proc_fsm = self.add_fsm("proc_seq", reset_high=False)
        START = self.proc_fsm.add_state("START")
        PROCESS = self.proc_fsm.add_state("PROCESS")
        DROPZERO = self.proc_fsm.add_state("DROPZERO")

        ####################
        # Next State Logic
        ####################

        ####################
        # START #
        ####################
        # IN the START state, we are waiting to see data in a stream
        # to know to pass on the processed stream
        # If we hit the EOS without seeing data, we should strip it
        # START.next(DATA_SEEN, self._base_data_seen & self._tile_en)
        START.next(PROCESS, self._tile_en & self._cmrg_mode)
        START.next(DROPZERO, self._tile_en & ~self._cmrg_mode)
        START.next(START, None)

        ####################
        # PROCESS #
        ####################
        # In DATA SEEN, we want to pass thru any data including the
        # stop token at the correct level, then go to the pass_stop logic. Make sure we also pushed the proc stream once
        # PROCESS.next(PROCESS, self._pushed_proc & self._pushed_stop_lvl)
        # TODO: when all the tokens are processed, should we go back to START state?
        PROCESS.next(PROCESS, None)

        ####################
        # DROPZERO #
        ####################
        # In DROPZERO, one input stream is the value stream, and the other
        # is its corresponding coordinate stream. The goal is to drop all
        # coordinates where its corresponding value entries are zero
        # value stream will be pushed through through input fifo[0]
        # crd stream will be pushed through via input fifo[1]
        # TODO: when all the tokens are processed, should we go back to START state?
        DROPZERO.next(DROPZERO, None)

        ####################
        # FSM Output Logic
        ####################

        self.proc_fsm.output(self._cmrg_fifo_pop[0])
        self.proc_fsm.output(self._cmrg_fifo_pop[1])
        self.proc_fsm.output(self._cmrg_fifo_push[0])
        self.proc_fsm.output(self._cmrg_fifo_push[1])
        self.proc_fsm.output(self._clr_pushed_proc)
        self.proc_fsm.output(self._clr_pushed_stop_lvl)
        self.proc_fsm.output(self._set_pushed_data_lower)
        self.proc_fsm.output(self._clr_pushed_data_lower)

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
        START.output(self._set_pushed_data_lower, 0)
        START.output(self._clr_pushed_data_lower, 1)

        ################
        # PROCESS
        ################
        # To pop the lower one, we want to make sure there is data on it - free to push
        # if stop on it, need to make sure the upper has valid data on it (or done)
        PROCESS.output(self._cmrg_fifo_pop[0], ~self._base_valid_delay | kts.ternary(self._delay_done,
                                                          self._proc_done & ~base_outfifo.ports.full & ~proc_outfifo.ports.full,
                                                          kts.ternary(self._delay_data,
                                                                        ~base_outfifo.ports.full & self._base_infifo_in_valid &
                                                                        (self._base_data_seen | (self._eos_seen & self._proc_infifo_in_valid & ~self._proc_infifo_in_eos & ~proc_outfifo.ports.full)),
                                                                        kts.ternary(self._delay_eos,
                                                                                    self._base_infifo_in_valid & ((self._proc_infifo_in_valid & ~self._proc_infifo_in_eos) | self._proc_done) &
                                                                                    (((self._base_data_seen | self._base_done) & ((self._pushed_data_lower & ~base_outfifo.ports.full) | ~self._pushed_data_lower)) | self._eos_seen),
                                                                                    0))))

        # Only pop the proc fifo if the base level has a stop token and upper has valid, or the upper has a stop token by itself
        PROCESS.output(self._cmrg_fifo_pop[1], kts.ternary(self._proc_done,
                                                            self._delay_done & ~base_outfifo.ports.full & ~proc_outfifo.ports.full,
                                                            kts.ternary(self._proc_infifo_in_valid & ~self._proc_infifo_in_eos,
                                                                        self._eos_seen & ((~proc_outfifo.ports.full & self._delay_data & ~base_outfifo.ports.full) | self._delay_eos | ~self._base_valid_delay),
                                                                        kts.ternary(self._proc_infifo_in_valid & self._proc_infifo_in_eos,
                                                                                    ~proc_outfifo.ports.full,
                                                                                    0))))

        # The push is basically the same as the pop
        # But we only want to push the STOP token if we have previously pushed data on the line, otherwise we are dropping the fiber from both
        # levels of the hierarchy
        PROCESS.output(self._cmrg_fifo_push[0], kts.ternary(self._delay_done,
                                                            self._proc_done & ~base_outfifo.ports.full & ~proc_outfifo.ports.full,
                                                            kts.ternary(self._delay_data,
                                                                        ~base_outfifo.ports.full & self._base_infifo_in_valid &
                                                                        (self._base_data_seen | (self._eos_seen & self._proc_infifo_in_valid & ~self._proc_infifo_in_eos & ~proc_outfifo.ports.full)),
                                                                        kts.ternary(self._delay_eos,
                                                                                    self._base_infifo_in_valid & ((self._proc_infifo_in_valid & ~self._proc_infifo_in_eos) | self._proc_done) &
                                                                                    (self._base_data_seen | self._base_done) & self._pushed_data_lower & ~base_outfifo.ports.full,
                                                                                    0))))

        # Push is similar to pop, but in the case of a real data on proc, we only push it if we pushed a data on the base level along with the stop token
        PROCESS.output(self._cmrg_fifo_push[1], kts.ternary(self._proc_done,
                                                            self._delay_done & ~base_outfifo.ports.full & ~proc_outfifo.ports.full,
                                                            kts.ternary(self._proc_infifo_in_valid & ~self._proc_infifo_in_eos,
                                                                        self._eos_seen & ~proc_outfifo.ports.full & self._delay_data & ~base_outfifo.ports.full,
                                                                        kts.ternary(self._proc_infifo_in_valid & self._proc_infifo_in_eos,
                                                                                    ~proc_outfifo.ports.full,
                                                                                    0))))

        # Force these to 0 (but does this consume power?)
        PROCESS.output(self._clr_pushed_proc, 0)
        PROCESS.output(self._clr_pushed_stop_lvl, 0)
        # Set that data is pushed when you're pushing data...
        PROCESS.output(self._set_pushed_data_lower, self._delay_data & ~base_outfifo.ports.full & self._base_infifo_in_valid &
                                                    (self._base_data_seen | (self._eos_seen & self._proc_infifo_in_valid & ~self._proc_infifo_in_eos & ~proc_outfifo.ports.full)))
        # Clear that data has been pushed when you are pushing the stop token of the base line
        PROCESS.output(self._clr_pushed_data_lower, self._delay_done | (self._delay_eos & self._base_infifo_in_valid & ((self._proc_infifo_in_valid & ~self._proc_infifo_in_eos) | self._proc_done) &
                                                                        (self._base_data_seen | self._base_done) & self._pushed_data_lower & ~base_outfifo.ports.full))

        ################
        # DROPZERO
        ################
        self._cmrg_base_fifo_pop = self.var("cmrg_base_fifo_pop", 1)
        self._cmrg_proc_fifo_pop = self.var("cmrg_proc_fifo_pop", 1)
        self._cmrg_base_fifo_push = self.var("cmrg_base_fifo_push", 1)
        self._cmrg_proc_fifo_push = self.var("cmrg_proc_fifo_push", 1)
        DROPZERO.output(self._cmrg_fifo_pop[0], self._cmrg_base_fifo_pop)
        DROPZERO.output(self._cmrg_fifo_pop[1], self._cmrg_proc_fifo_pop)
        DROPZERO.output(self._cmrg_fifo_push[0], self._cmrg_base_fifo_push)
        DROPZERO.output(self._cmrg_fifo_push[1], self._cmrg_proc_fifo_push)

        # TODO: check whether we can simply force these signals to fixed values
        DROPZERO.output(self._clr_pushed_proc, 1)
        DROPZERO.output(self._clr_pushed_stop_lvl, 1)
        DROPZERO.output(self._set_pushed_data_lower, 0)
        DROPZERO.output(self._clr_pushed_data_lower, 1)

        self.proc_fsm.set_start_state(START)

        self.add_code(self.base_delay)

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
        # Select input from the dalay if delay is valid for crddrop mode
        # For dropzero mode, simply grab value from infifo
        self.wire(self._base_outfifo_in_packed[self.data_width], kts.ternary(self._cmrg_mode, self._base_delay[self.data_width], self._base_infifo_in_eos))
        self.wire(self._base_outfifo_in_packed[self.data_width - 1, 0], kts.ternary(self._cmrg_mode, self._base_delay[self.data_width - 1, 0], self._base_infifo_in_data))

        self._base_outfifo_out_packed = self.var(f"base_outfifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._cmrg_coord_out[0][self.data_width], self._base_outfifo_out_packed[self.data_width])
        self.wire(self._cmrg_coord_out[0][self.data_width - 1, 0], self._base_outfifo_out_packed[self.data_width - 1, 0])
        self._base_outfifo_in_ready = self.var(f"base_outfifo_in_ready", 1)

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
        self.wire(self._base_outfifo_in_ready, ~base_outfifo.ports.full)

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

        self._proc_outfifo_in_ready = self.var(f"proc_outfifo_in_ready", 1)

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
        self.wire(self._proc_outfifo_in_ready, ~proc_outfifo.ports.full)

        self.add_code(self.cmrg_fifo_control_logic_dropzero)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "crddrop"

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def base_delay(self):
        if ~self._rst_n:
            self._base_delay = 0
            self._base_valid_delay = 0
        elif (self._cmrg_fifo_pop[0] & ~(self._delay_done & self._done_seen)):
            self._base_delay = kts.ternary(~self._base_valid_delay | self._base_data_seen | self._done_seen | self._delay_data,
                                            self._base_infifo_out_packed,
                                            kts.ternary(self._base_infifo_out_packed < self._base_delay,  # Only keeps the maximum
                                                    self._base_delay,
                                                    self._base_infifo_out_packed))
            self._base_valid_delay = self._base_infifo_in_valid
        elif (self._cmrg_fifo_pop[0] & self._delay_done & self._done_seen):
            self._base_delay = 0
            self._base_valid_delay = 0
        else:
            self._base_delay = self._base_delay
            self._base_valid_delay = self._base_valid_delay

    @always_comb
    def cmrg_fifo_control_logic_dropzero(self):
        if (self._base_infifo_in_valid & self._proc_infifo_in_valid):
            # only pop from fifo when data are present in both fifos to keep them aligned
            if ((self._base_infifo_in_data == kts.const(0, self.data_width)) & ~self._eos_seen & ~self._base_done):
                # the value data is zero and is not a eos or done token, get rid both value and crd data by popping them and make outfifo ignore them
                self._cmrg_base_fifo_pop = 1
                self._cmrg_proc_fifo_pop = 1
                self._cmrg_base_fifo_push = 0
                self._cmrg_proc_fifo_push = 0
            else:
                # if the data value on the value stream is not zero, a eos, or a done token, we need to push keep the value and crd by pushing them into the out fifos
                if (self._base_outfifo_in_ready & self._proc_outfifo_in_ready):
                    # check for available space in both output fifos
                    self._cmrg_base_fifo_pop = 1
                    self._cmrg_proc_fifo_pop = 1
                    self._cmrg_base_fifo_push = 1
                    self._cmrg_proc_fifo_push = 1
                else:
                    self._cmrg_base_fifo_pop = 0
                    self._cmrg_proc_fifo_pop = 0
                    self._cmrg_base_fifo_push = 0
                    self._cmrg_proc_fifo_push = 0
        else:
            self._cmrg_base_fifo_pop = 0
            self._cmrg_proc_fifo_pop = 0
            self._cmrg_base_fifo_push = 0
            self._cmrg_proc_fifo_push = 0


if __name__ == "__main__":
    crddrop_dut = CrdDrop(data_width=16, defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    kts.verilog(crddrop_dut, filename="crd_drop.sv",
            optimize_if=False)
