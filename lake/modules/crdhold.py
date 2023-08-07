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


class CrdHold(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 add_clk_enable=True,
                 add_flush=False,
                 lift_config=False,
                 defer_fifos=True,
                 perf_debug=False):

        name_str = f"crdhold"
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

        # Store all configurations here
        config = [("tile_en", 1)]

        cmrg_enable = config_kwargs['cmrg_enable']
        cmrg_stop_lvl = config_kwargs['cmrg_stop_lvl']

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

        self._data_to_procfifo = self.var("data_to_fifo", self.data_width)
        self._eos_to_procfifo = self.var("eos_to_fifo", 1)
        self._hold_reg = self.var("hold_reg", self.data_width)

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
                                                    self._cmrg_coord_out_valid_out[0],
                                                    name='done_indicator')
            self.add_performance_indicator(self._done_signal, edge='posedge', label='done', cycle_count=cyc_count)

        ####
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

        self._base_eos_seen = self.var("base_eos_seen", 1)
        self.wire(self._base_eos_seen, self._base_infifo_in_valid & self._base_infifo_in_eos & (self._base_infifo_in_data[9, 8] == kts.const(0, 2)))

        self._proc_eos_seen = self.var("proc_eos_seen", 1)
        self.wire(self._proc_eos_seen, self._proc_infifo_in_valid & self._proc_infifo_in_eos &
                  (self._proc_infifo_in_data[9, 8] == kts.const(0, 2)))

        self._base_done_seen = self.var("base_done_seen", 1)
        self.wire(self._base_done_seen, self._base_infifo_in_valid & self._base_infifo_in_eos & (self._base_infifo_in_data[9, 8] == kts.const(1, 2)))

        self._proc_done_seen = self.var("proc_done_seen", 1)
        self.wire(self._proc_done_seen, self._proc_infifo_in_valid & self._proc_infifo_in_eos & (self._proc_infifo_in_data[9, 8] == kts.const(1, 2)))

        self._clr_pushed_proc = self.var("clr_pushed_proc", 1)
        self._pushed_proc = sticky_flag(self, self._cmrg_fifo_push[1], clear=self._clr_pushed_proc, name="pushed_proc", seq_only=True)

        self._clr_pushed_base = self.var("clr_pushed_base", 1)
        self._pushed_base = sticky_flag(self, self._cmrg_fifo_push[0], clear=self._clr_pushed_base, name="pushed_base",
                                        seq_only=True)

        # self._clr_pushed_stop_lvl = self.var("clr_pushed_stop_lvl", 1)
        # self._pushed_stop_lvl = sticky_flag(self, self._cmrg_fifo_push[0] & self._base_infifo_in_valid & self._base_infifo_in_eos,
        #                                     clear=self._clr_pushed_stop_lvl, name="pushed_stop_lvl", seq_only=True)

        self._both_done = self.var("both_done", 1)
        self.wire(self._both_done, self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & self._proc_infifo_in_eos &
                  (self._base_infifo_in_data[9, 8] == kts.const(1, 2)) & (self._proc_infifo_in_data[9, 8] == kts.const(1, 2)))

        self._pushing_done = self.var("pushing_done", 1)
        self.wire(self._pushing_done, self._base_infifo_in_valid & self._base_infifo_in_eos & self._proc_infifo_in_valid & self._proc_infifo_in_eos &
                  (self._base_infifo_in_data[9, 8] == kts.const(1, 2)) & (self._proc_infifo_in_data[9, 8] == kts.const(1, 2)) & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)

        self._reg_clr = self.var("reg_clr", 1)
        self._reg_hold = self.var("reg_hold", 1)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def hold_reg_ff():
            if ~self._rst_n:
                self._hold_reg = 0
            elif self._reg_clr:
                self._hold_reg = 0
            elif self._reg_hold:
                self._hold_reg = self._proc_infifo_in_data
        self.add_code(hold_reg_ff)

        ####################
        # STATE MACHINE TO PROCESS PROC STREAM
        ####################

        # Create FSM
        self.proc_fsm = self.add_fsm("proc_seq", reset_high=False)
        START = self.proc_fsm.add_state("START")
        DATA_SEEN = self.proc_fsm.add_state("DATA_SEEN")
        # HOLD = self.proc_fsm.add_state("HOLD")
        # PASS_STOP = self.proc_fsm.add_state("PASS_STOP")
        # PASS_DONE = self.proc_fsm.add_state("PASS_DONE")
        DONE = self.proc_fsm.add_state("DONE")

        ####################
        # Next State Logic
        ####################

        ####################
        # START #
        ####################
        # IN the START state, we are waiting to see data in a stream
        # to know to pass on the processed stream
        # START.next(PASS_DONE, self._done_seen)
        # START.next(PASS_STOP, self._base_eos_seen)
        # START.next(DATA_SEEN, self._base_infifo_in_valid & ~self._base_infifo_in_eos)
        START.next(DATA_SEEN, self._tile_en)
        # START.next(DONE, self._done_seen)
        START.next(START, None)

        ####################
        # DATA_SEEN #
        ####################
        # In DATA SEEN, we want to trigger the hold register and pass through any base data.
        # DATA_SEEN.next(HOLD, self._base_infifo_in_valid & ~self._proc_infifo_in_eos & ~self._base_infifo_in_eos & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # DATA_SEEN.next(PASS_STOP, self._base_eos_seen & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        DATA_SEEN.next(DONE, self._both_done)
        DATA_SEEN.next(DATA_SEEN, None)

        # HOLD.next(PASS_STOP, self._base_eos_seen & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # HOLD.next(HOLD, None)
        # ####################
        # # PASS_STOP #
        # ####################
        # # IN PASS_STOP, we pass the base stop to the proc stream then go to done
        # PASS_STOP.next(PASS_DONE, self._both_done)
        # # PASS_STOP.next(START, self._pushed_proc)
        # PASS_STOP.next(START, self._pushed_proc & ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        # PASS_STOP.next(PASS_STOP, None)

        # ####################
        # # PASS_DONE #
        # ####################
        # PASS_DONE.next(DONE, self._pushing_done)
        # PASS_DONE.next(PASS_DONE, None)

        ####################
        # DONE #
        ####################
        DONE.next(START, ~base_outfifo.ports.full & ~proc_outfifo.ports.full)

        ####################
        # FSM Output Logic
        ####################

        self.proc_fsm.output(self._cmrg_fifo_pop[0])
        self.proc_fsm.output(self._cmrg_fifo_pop[1])
        self.proc_fsm.output(self._cmrg_fifo_push[0])
        self.proc_fsm.output(self._cmrg_fifo_push[1])
        self.proc_fsm.output(self._data_to_procfifo)
        self.proc_fsm.output(self._eos_to_procfifo)
        self.proc_fsm.output(self._clr_pushed_proc)
        self.proc_fsm.output(self._clr_pushed_base)
        self.proc_fsm.output(self._reg_hold)
        self.proc_fsm.output(self._reg_clr)

        ################
        # START
        ################
        START.output(self._cmrg_fifo_pop[0], 0)
        START.output(self._cmrg_fifo_pop[1], 0)
        START.output(self._cmrg_fifo_push[0], 0)
        START.output(self._cmrg_fifo_push[1], 0)
        START.output(self._data_to_procfifo, kts.const(0, 16))
        START.output(self._eos_to_procfifo, 0)
        # Force these to 0 (but does this consume power?)
        START.output(self._clr_pushed_proc, 1)
        START.output(self._clr_pushed_base, 1)
        START.output(self._reg_clr, 0)
        START.output(self._reg_hold, 0)

        ################
        # DATA_SEEN
        ################
        # DATA_SEEN.output(self._cmrg_fifo_pop[0], self._base_infifo_in_valid & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # DATA_SEEN.output(self._cmrg_fifo_pop[1], self._proc_infifo_in_valid & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # DATA_SEEN.output(self._cmrg_fifo_push[0], self._base_infifo_in_valid & ~self._base_infifo_in_eos)
        # DATA_SEEN.output(self._cmrg_fifo_push[1], self._proc_infifo_in_valid & ~self._base_infifo_in_eos)
        # DATA_SEEN.output(self._data_to_procfifo, self._proc_infifo_in_data)
        # DATA_SEEN.output(self._eos_to_procfifo, 0)
        # DATA_SEEN.output(self._clr_pushed_proc, 0)
        # DATA_SEEN.output(self._clr_pushed_base, 0)
        # DATA_SEEN.output(self._reg_clr, 0)
        # DATA_SEEN.output(self._reg_hold, 1)
        # Pop the base if there's room in the output and the proc isn't eos
        # DATA_SEEN.output(self._cmrg_fifo_pop[0], ~self._proc_eos_seen & ~base_outfifo.ports.full & ~proc_outfifo.ports.full & ~self._base_done_seen)
        DATA_SEEN.output(self._cmrg_fifo_pop[0], kts.ternary(self._base_eos_seen,
                                                             kts.const(1, 1) & (self._proc_data_seen | self._proc_done_seen),
                                                             self._base_infifo_in_valid & ~self._base_infifo_in_eos &
                                                                self._proc_infifo_in_valid & ~self._proc_infifo_in_eos) &
                                                                    ~base_outfifo.ports.full & ~proc_outfifo.ports.full & ~self._base_done_seen)
        # Pop proc if it is eos itself (dump it) or if the base has eos on it
        DATA_SEEN.output(self._cmrg_fifo_pop[1], kts.ternary(self._proc_eos_seen,
                                                             kts.const(1, 1),
                                                             self._base_eos_seen & ~base_outfifo.ports.full & ~proc_outfifo.ports.full) & ~self._proc_done_seen)
        # Push the bottom if there is room and the bottom is valid
        DATA_SEEN.output(self._cmrg_fifo_push[0], kts.ternary(self._base_eos_seen,
                                                              kts.const(1, 1) & (self._proc_data_seen | self._proc_done_seen),
                                                              self._base_infifo_in_valid & ~self._base_infifo_in_eos &
                                                                self._proc_infifo_in_valid & ~self._proc_infifo_in_eos) &
                                                                    ~base_outfifo.ports.full & ~proc_outfifo.ports.full & ~self._base_done_seen)
        # If the bottom is eos, push, if the bottom is not eos, push if the top also not eos
        DATA_SEEN.output(self._cmrg_fifo_push[1], kts.ternary(self._base_eos_seen,
                                                              kts.const(1, 1) & (self._proc_data_seen | self._proc_done_seen),
                                                              self._base_infifo_in_valid & ~self._base_infifo_in_eos &
                                                                self._proc_infifo_in_valid & ~self._proc_infifo_in_eos) &
                                                                    ~base_outfifo.ports.full & ~proc_outfifo.ports.full & ~self._base_done_seen)
        # If base is eos, send through base, otherwise safe to send through proc
        DATA_SEEN.output(self._data_to_procfifo, kts.ternary(self._base_infifo_in_eos, self._base_infifo_in_data, self._proc_infifo_in_data))
        DATA_SEEN.output(self._eos_to_procfifo, self._base_infifo_in_eos)
        DATA_SEEN.output(self._clr_pushed_proc, 1)
        DATA_SEEN.output(self._clr_pushed_base, 1)
        DATA_SEEN.output(self._reg_clr, 1)
        DATA_SEEN.output(self._reg_hold, 0)

        # ################
        # # HOLD
        # ################
        # HOLD.output(self._cmrg_fifo_pop[0], self._base_infifo_in_valid & ~self._base_infifo_in_eos & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # HOLD.output(self._cmrg_fifo_pop[1], 0)
        # HOLD.output(self._cmrg_fifo_push[0], self._base_infifo_in_valid & ~self._base_infifo_in_eos)
        # HOLD.output(self._cmrg_fifo_push[1], self._base_infifo_in_valid & ~self._base_infifo_in_eos)
        # HOLD.output(self._data_to_procfifo, self._hold_reg)
        # HOLD.output(self._eos_to_procfifo, 0)
        # HOLD.output(self._clr_pushed_proc, 1)
        # HOLD.output(self._clr_pushed_base, 0)
        # HOLD.output(self._reg_clr, 0)
        # HOLD.output(self._reg_hold, 0)

        # ################
        # # PASS_STOP
        # ################
        # # Here we have to pass base eos to both base and proc
        # PASS_STOP.output(self._cmrg_fifo_pop[0], self._base_eos_seen & ~base_outfifo.ports.full & ~proc_outfifo.ports.full)
        # # PASS_STOP.output(self._cmrg_fifo_pop[1], self._proc_eos_seen & ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        # PASS_STOP.output(self._cmrg_fifo_pop[1], self._proc_eos_seen & ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        # PASS_STOP.output(self._cmrg_fifo_push[0], self._base_eos_seen)
        # PASS_STOP.output(self._cmrg_fifo_push[1], self._base_eos_seen)
        # PASS_STOP.output(self._data_to_procfifo, self._base_infifo_in_data)
        # PASS_STOP.output(self._eos_to_procfifo, self._base_infifo_in_eos)
        # PASS_STOP.output(self._clr_pushed_proc, 0)
        # PASS_STOP.output(self._clr_pushed_base, 0)
        # PASS_STOP.output(self._reg_clr, 1)
        # PASS_STOP.output(self._reg_hold, 0)

        # ################
        # # PASS_DONE
        # ################
        # # TODO
        # PASS_DONE.output(self._cmrg_fifo_pop[0], self._pushing_done)
        # PASS_DONE.output(self._cmrg_fifo_pop[1], self._pushing_done)
        # PASS_DONE.output(self._cmrg_fifo_push[0], self._pushing_done)
        # PASS_DONE.output(self._cmrg_fifo_push[1], self._pushing_done)
        # PASS_DONE.output(self._data_to_procfifo, self._base_infifo_in_data)
        # PASS_DONE.output(self._eos_to_procfifo, self._base_infifo_in_eos)
        # PASS_DONE.output(self._clr_pushed_proc, 0)
        # PASS_DONE.output(self._clr_pushed_base, 0)
        # PASS_DONE.output(self._reg_clr, 0)
        # PASS_DONE.output(self._reg_hold, 0)

        ################
        # DONE
        ################
        # TODO
        DONE.output(self._cmrg_fifo_pop[0], ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        DONE.output(self._cmrg_fifo_pop[1], ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        DONE.output(self._cmrg_fifo_push[0], ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        DONE.output(self._cmrg_fifo_push[1], ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        DONE.output(self._data_to_procfifo, self._base_infifo_in_data)
        DONE.output(self._eos_to_procfifo, 1)
        DONE.output(self._clr_pushed_proc, 1)
        DONE.output(self._clr_pushed_base, 1)
        DONE.output(self._reg_clr, 1)
        DONE.output(self._reg_hold, 0)
        # # TODO
        # DONE.output(self._cmrg_fifo_pop[0], ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        # DONE.output(self._cmrg_fifo_pop[1], ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        # DONE.output(self._cmrg_fifo_push[0], ~proc_outfifo.ports.full & ~base_outfifo.ports.full)
        # DONE.output(self._cmrg_fifo_push[1], 0)
        # DONE.output(self._data_to_procfifo, kts.const(0, 16))
        # DONE.output(self._eos_to_procfifo, 0)
        # DONE.output(self._clr_pushed_proc, 0)
        # DONE.output(self._clr_pushed_base, 0)
        # DONE.output(self._reg_clr, 0)
        # DONE.output(self._reg_hold, 0)

        self.proc_fsm.set_start_state(START)

        ################
        # OUTPUT FIFOS #
        ################

        ##############
        # BASE outfifo
        ##############
        # Use this stream to process the PROC stream...

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

        self._proc_outfifo_in_packed = self.var(f"proc_outfifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._proc_outfifo_in_packed[self.data_width], self._eos_to_procfifo)
        self.wire(self._proc_outfifo_in_packed[self.data_width - 1, 0], self._data_to_procfifo)

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
        return "crdhold"


if __name__ == "__main__":
    crdhold_dut = CrdHold(data_width=16, defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(crdhold_dut, filename="crd_drop.sv",
            optimize_if=False)
