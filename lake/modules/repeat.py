import kratos as kts
from kratos import *
from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.top.memory_controller import MemoryController
from lake.utils.util import add_counter, sticky_flag
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class Repeat(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 defer_fifos=True,
                 add_flush=False,
                 perf_debug=True):

        super().__init__("Repeat", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = add_flush
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos
        self.perf_debug = perf_debug

        # For consistency with Core wrapper in garnet...
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

        # Take in a data stream to do repeats (modifier) and a data stream to process

        # REPSIG IN
        self._repsig_data_in = self.input("repsig_data_in", self.data_width + 1, packed=True)
        self._repsig_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._repsig_eos_in = self.var("repsig_eos_in", 1)
        # self.wire(self._repsig_eos_in, self._repsig_data_in[self.data_width])
        # self._repsig_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._repsig_ready_out = self.output("repsig_data_in_ready", 1)
        self._repsig_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._repsig_valid_in = self.input("repsig_data_in_valid", 1)
        self._repsig_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        # PROC IN
        self._proc_data_in = self.input("proc_data_in", self.data_width + 1, packed=True)
        self._proc_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._proc_eos_in = self.input("proc_eos_in", 1)
        # self._proc_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._proc_ready_out = self.output("proc_data_in_ready", 1)
        self._proc_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._proc_valid_in = self.input("proc_data_in_valid", 1)
        self._proc_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        # Data out (right now its a ref...)
        self._ref_data_out = self.output("ref_data_out", self.data_width + 1, packed=True)
        self._ref_data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._ref_ready_in = self.input("ref_data_out_ready", 1)
        self._ref_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ref_valid_out = self.output("ref_data_out_valid", 1)
        self._ref_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._ref_eos_out = self.output("ref_eos_out", 1)
        # self._ref_eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Config regs

        # Set the stop token injection level
        self._stop_lvl = self.input("stop_lvl", self.data_width)
        self._stop_lvl.add_attribute(ConfigRegAttr("What level stop tokens should this scanner inject"))

        # Root mode
        self._root = self.input("root", 1)
        self._root.add_attribute(ConfigRegAttr("Is this a root repeater"))

        self._spacc_mode = self.input("spacc_mode", 1)
        self._spacc_mode.add_attribute(ConfigRegAttr("Is this in spacc mode?"))

        if self.perf_debug:

            cyc_count = add_counter(self, "clock_cycle_count", 64, increment=self._clk & self._clk_en)

            # Start when any of the coord inputs is valid
            self._start_signal = sticky_flag(self, self._proc_valid_in,
                                             name='start_indicator')
            self.add_performance_indicator(self._start_signal, edge='posedge', label='start', cycle_count=cyc_count)

            # End when we see DONE on the output coord
            self._done_signal = sticky_flag(self, (self._ref_data_out == MemoryController.DONE_PROXY) &
                                                    self._ref_valid_out,
                                                    name='done_indicator')
            self.add_performance_indicator(self._done_signal, edge='posedge', label='done', cycle_count=cyc_count)

# ==============================
# INPUT FIFO
# ==============================

        self._set_last_pushed_data = self.var("set_last_pushed_data", 1)
        self._clr_last_pushed_data = self.var("clr_last_pushed_data", 1)
        self._last_pushed_data = sticky_flag(self, self._set_last_pushed_data, clear=self._clr_last_pushed_data, name="pushed_data_sticky", seq_only=True)

        # Repsig fifo
        self._repsig_fifo_pop = self.var("repsig_fifo_pop", 1)
        self._repsig_fifo_valid = self.var("repsig_fifo_valid", 1)

        self._repsig_fifo_in = kts.concat(self._repsig_data_in)
        self._repsig_in_fifo = RegFIFO(data_width=self._repsig_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._repsig_in_fifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._repsig_fifo_out_data = self.var("repsig_fifo_out_data", self.data_width, packed=True)
        self._repsig_fifo_out_eos = self.var("repsig_fifo_out_eos", 1)

        self.add_child(f"repsig_in_fifo",
                       self._repsig_in_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._repsig_valid_in,
                       pop=self._repsig_fifo_pop,
                       data_in=self._repsig_fifo_in,
                       data_out=kts.concat(self._repsig_fifo_out_eos, self._repsig_fifo_out_data))

        self.wire(self._repsig_ready_out, ~self._repsig_in_fifo.ports.full)
        self.wire(self._repsig_fifo_valid, ~self._repsig_in_fifo.ports.empty)

        # Proc fifo
        self._proc_fifo_pop = self.var("proc_fifo_pop", 1)
        self._proc_fifo_valid = self.var("proc_fifo_valid", 1)

        self._proc_fifo_push = self.var("proc_fifo_push", 1)
        self._proc_fifo_full = self.var("proc_fifo_full", 1)

        # Need a path to inject into the input fifo for root mode
        self._proc_fifo_inject_push = self.var("proc_fifo_inject_push", 1)
        self._proc_fifo_inject_data = self.var("proc_fifo_inject_data", 16)
        self._proc_fifo_inject_eos = self.var("proc_fifo_inject_eos", 1)

        self.wire(self._proc_fifo_push, kts.ternary(self._root, self._proc_fifo_inject_push, self._proc_valid_in))

        self._proc_fifo_in = kts.ternary(self._root, kts.concat(self._proc_fifo_inject_eos, self._proc_fifo_inject_data), kts.concat(self._proc_data_in))
        self._proc_in_fifo = RegFIFO(data_width=self._proc_fifo_in.width,
                                     width_mult=1,
                                     depth=self.fifo_depth,
                                     min_depth=2,
                                     defer_hrdwr_gen=self.defer_fifos)
        self._proc_in_fifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._proc_fifo_out_data = self.var("proc_fifo_out_data", self.data_width, packed=True)
        self._proc_fifo_out_eos = self.var("proc_fifo_out_eos", 1)

        self.add_child(f"proc_in_fifo",
                       self._proc_in_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._proc_fifo_push,
                       pop=self._proc_fifo_pop,
                       data_in=self._proc_fifo_in,
                       data_out=kts.concat(self._proc_fifo_out_eos, self._proc_fifo_out_data))

        self.wire(self._proc_ready_out, ~self._proc_in_fifo.ports.full)
        self.wire(self._proc_fifo_full, self._proc_in_fifo.ports.full)
        self.wire(self._proc_fifo_valid, ~self._proc_in_fifo.ports.empty)

# ==============================
# OUTPUT FIFO
# ==============================

        self._ref_fifo_push = self.var("ref_fifo_push", 1)
        self._ref_fifo_full = self.var("ref_fifo_full", 1)

        self._ref_fifo_in_data = self.var("ref_fifo_in_data", self.data_width)
        self._ref_fifo_in_eos = self.var("ref_fifo_in_eos", 1)

        self._ref_fifo_in = kts.concat(self._ref_fifo_in_eos, self._ref_fifo_in_data)
        self._ref_out_fifo = RegFIFO(data_width=self._ref_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._ref_out_fifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self.add_child(f"ref_out_fifo",
                       self._ref_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._ref_fifo_push,
                       pop=self._ref_ready_in,
                       data_in=self._ref_fifo_in,
                       data_out=kts.concat(self._ref_data_out))

        self.wire(self._ref_fifo_full, self._ref_out_fifo.ports.full)
        self.wire(self._ref_valid_out, ~self._ref_out_fifo.ports.empty)

# =============================
# Various Logic
# =============================
        self._seen_root_eos = sticky_flag(self, (self._proc_fifo_out_data == 0) & self._proc_fifo_out_eos & self._proc_fifo_valid, name="seen_root_eos")

        self._ref_maybe = self.var("ref_maybe", 1)
        self.wire(self._ref_maybe, self._proc_fifo_valid & self._proc_fifo_out_eos & (self._proc_fifo_out_data[9, 8] == kts.const(2, 2)))
        self._proc_data_seen = self.var("proc_data", 1)
        self.wire(self._proc_data_seen, (~self._proc_fifo_out_eos | self._ref_maybe) & self._proc_fifo_valid)
        self._proc_stop = self.var("proc_stop", 1)
        self.wire(self._proc_stop, (self._proc_fifo_out_data[9, 8] == kts.const(0, 2)) & self._proc_fifo_out_eos & self._proc_fifo_valid)
        self._proc_done = self.var("proc_done", 1)
        self.wire(self._proc_done, (self._proc_fifo_out_data[9, 8] == kts.const(1, 2)) & self._proc_fifo_out_eos & self._proc_fifo_valid)
        self._repsig_stop = self.var("repsig_stop", 1)
        self.wire(self._repsig_stop, (self._repsig_fifo_out_data[9, 8] == kts.const(0, 2)) & self._repsig_fifo_out_eos & self._repsig_fifo_valid)
        self._repsig_sig = self.var("repsig_sig", 1)
        self.wire(self._repsig_sig, ~self._repsig_fifo_out_eos & self._repsig_fifo_valid)
        self._repsig_done = self.var("repsig_done", 1)
        self.wire(self._repsig_done, (self._repsig_fifo_out_data[9, 8] == kts.const(1, 2)) & self._repsig_fifo_out_eos & self._repsig_fifo_valid)

        # The following two signals are used to indicate repeat over an empty fiber (R in repsig and SX in proc)
        # We use these signals to indicate staying in the PASS_REPEAT state as to not lose a cycle for a state transition
        self._blank_repeat = self.var("blank_repeat", 1)
        self.wire(self._blank_repeat, self._proc_stop & ~self._repsig_stop & self._repsig_fifo_valid)

        self._blank_repeat_stop = self.var("blank_repeat_stop", 1)
        self.wire(self._blank_repeat_stop, self._proc_stop & self._repsig_stop & self._last_pushed_data)
# =============================
# Instantiate FSM
# =============================

        self.repeat_fsm = self.add_fsm("repeat_fsm", reset_high=False)

        START = self.repeat_fsm.add_state("START")
        INJECT0 = self.repeat_fsm.add_state("INJECT0")
        INJECT1 = self.repeat_fsm.add_state("INJECT1")
        PASS_REPEAT = self.repeat_fsm.add_state("PASS_REPEAT")

# =============================
# FSM Transitions
# =============================

        #####################
        # START
        #####################
        START.next(INJECT0, self._root & self._tile_en)
        START.next(PASS_REPEAT, ~self._root & self._tile_en)
        START.next(START, None)

        #####################
        # INJECT0
        #####################
        INJECT0.next(INJECT1, ~self._proc_fifo_full)
        INJECT0.next(INJECT0, None)

        #####################
        # INJECT1
        #####################
        INJECT1.next(PASS_REPEAT, ~self._proc_fifo_full)
        INJECT1.next(INJECT1, None)

        #####################
        # PASS_REPEAT
        #####################
        # We can go to the PASS_STOP state when the stream being repeated has an actual stop token

        # We go to pass stop when we have a stop token on the repsig line - either to pass through or coalesce with proc stop
        PASS_REPEAT.next(START, self._proc_done & self._repsig_done & ~self._ref_fifo_full)
        # PASS_REPEAT.next(PASS_STOP, self._proc_fifo_valid & self._repsig_fifo_out_eos & self._repsig_fifo_valid &
        #                             (self._repsig_fifo_out_data[9, 8] == kts.const(0, 2)) & ~self._blank_repeat & ~self._blank_repeat_stop)
        PASS_REPEAT.next(PASS_REPEAT, None)

# =============================
# FSM Output Declaration
# =============================

        self.repeat_fsm.output(self._ref_fifo_in_data)
        self.repeat_fsm.output(self._ref_fifo_in_eos)
        self.repeat_fsm.output(self._ref_fifo_push)
        self.repeat_fsm.output(self._proc_fifo_pop)
        self.repeat_fsm.output(self._repsig_fifo_pop)
        self.repeat_fsm.output(self._proc_fifo_inject_push)
        self.repeat_fsm.output(self._proc_fifo_inject_data)
        self.repeat_fsm.output(self._proc_fifo_inject_eos)
        self.repeat_fsm.output(self._set_last_pushed_data)
        self.repeat_fsm.output(self._clr_last_pushed_data)

# =============================
# FSM Output Implementation
# =============================

        #####################
        # START
        #####################
        START.output(self._ref_fifo_in_data, 0)
        START.output(self._ref_fifo_in_eos, 0)
        START.output(self._ref_fifo_push, 0)
        START.output(self._proc_fifo_pop, 0)
        START.output(self._repsig_fifo_pop, 0)
        START.output(self._proc_fifo_inject_push, 0)
        START.output(self._proc_fifo_inject_data, 0)
        START.output(self._proc_fifo_inject_eos, 0)
        START.output(self._set_last_pushed_data, 0)
        START.output(self._clr_last_pushed_data, 0)

        #####################
        # INJECT0
        #####################
        INJECT0.output(self._ref_fifo_in_data, 0)
        INJECT0.output(self._ref_fifo_in_eos, 0)
        INJECT0.output(self._ref_fifo_push, 0)
        INJECT0.output(self._proc_fifo_pop, 0)
        INJECT0.output(self._repsig_fifo_pop, 0)
        INJECT0.output(self._proc_fifo_inject_push, 1)
        INJECT0.output(self._proc_fifo_inject_data, 0)
        INJECT0.output(self._proc_fifo_inject_eos, 0)
        INJECT0.output(self._set_last_pushed_data, 0)
        INJECT0.output(self._clr_last_pushed_data, 0)

        #####################
        # INJECT1
        #####################
        INJECT1.output(self._ref_fifo_in_data, 0)
        INJECT1.output(self._ref_fifo_in_eos, 0)
        INJECT1.output(self._ref_fifo_push, 0)
        INJECT1.output(self._proc_fifo_pop, 0)
        INJECT1.output(self._repsig_fifo_pop, 0)
        INJECT1.output(self._proc_fifo_inject_push, 1)
        INJECT1.output(self._proc_fifo_inject_data, kts.const(2**8, 16))
        INJECT1.output(self._proc_fifo_inject_eos, 1)
        INJECT1.output(self._set_last_pushed_data, 0)
        INJECT1.output(self._clr_last_pushed_data, 0)

        #####################
        # PASS_REPEAT
        #####################
        # Either injecting the original data or the stop token if the repsig is giving an eos
        # PASS_REPEAT.output(self._ref_fifo_in_data, kts.ternary(self._blank_repeat_stop, self._proc_fifo_out_data + 1, self._proc_fifo_out_data))
        PASS_REPEAT.output(self._ref_fifo_in_data, kts.ternary(self._repsig_stop, self._repsig_fifo_out_data, self._proc_fifo_out_data))
        # Pass eos as 1 if there is a maybe
        # PASS_REPEAT.output(self._ref_fifo_in_eos, self._ref_maybe | self._blank_repeat_stop)
        PASS_REPEAT.output(self._ref_fifo_in_eos, self._ref_maybe | self._repsig_done | self._repsig_stop)
        # We need both inputs to be valid to push out
        # PASS_REPEAT.output(self._ref_fifo_push, (((self._repsig_fifo_valid & self._proc_fifo_valid) & ~self._repsig_fifo_out_eos) | self._blank_repeat_stop) & ~self._proc_done & ~self._ref_fifo_full & ~self._blank_repeat)
        PASS_REPEAT.output(self._ref_fifo_push, ~self._ref_fifo_full & ((self._proc_done & self._repsig_done) |
                                                (self._proc_data_seen & self._repsig_fifo_valid) | (self._proc_stop & ~self._last_pushed_data & self._repsig_stop)))
        # If we are injecting the repsig stop token, then we should simultaneously pop the proc fifo as we are moving to the next data
        # I believe it is guaranteed by construction that the proc fifo HAS to have data on its line, it could never be a stop token on the proc fifo at this point
        # Just rip the data off once the stop token on the repsig line is hit
        # PASS_REPEAT.output(self._proc_fifo_pop, ((self._repsig_fifo_valid & self._repsig_fifo_out_eos & ~self._spacc_mode &
        #                                           (kts.ternary(self._proc_fifo_valid,
        #                                                         ~self._proc_fifo_out_eos | self._ref_maybe, kts.const(0, 1)))) | (self._spacc_mode & self._repsig_done) | (self._blank_repeat_stop & ~self._ref_fifo_full)) & ~self._proc_done)
        PASS_REPEAT.output(self._proc_fifo_pop, kts.ternary(self._proc_done,
                                                           self._repsig_done & ~self._ref_fifo_full,
                                                           kts.ternary(self._proc_stop,
                                                                      self._last_pushed_data | (self._repsig_stop & ~self._ref_fifo_full & ~self._last_pushed_data),
                                                                      self._repsig_stop & ~self._ref_fifo_full)))
        # Only pop the repsig fifo if there's room in the output fifo and join of input fifos (and not EOS)
        # PASS_REPEAT.output(self._repsig_fifo_pop, ~self._ref_fifo_full & (((self._repsig_fifo_valid & ~self._repsig_fifo_out_eos) & self._proc_fifo_valid & ~self._proc_done) | self._blank_repeat_stop))
        PASS_REPEAT.output(self._repsig_fifo_pop, kts.ternary(self._repsig_done,
                                                             self._proc_done & ~self._ref_fifo_full,
                                                             kts.ternary(self._repsig_stop,
                                                                        (self._proc_data_seen | (self._proc_stop & ~self._last_pushed_data)) & ~self._ref_fifo_full,
                                                                        (self._proc_data_seen & ~self._ref_fifo_full) | (self._proc_stop & ~self._last_pushed_data))))

        PASS_REPEAT.output(self._proc_fifo_inject_push, 0)
        PASS_REPEAT.output(self._proc_fifo_inject_data, 0)
        PASS_REPEAT.output(self._proc_fifo_inject_eos, 0)
        PASS_REPEAT.output(self._set_last_pushed_data, self._proc_data_seen)
        PASS_REPEAT.output(self._clr_last_pushed_data, self._proc_stop | (self._proc_done & self._repsig_done & ~self._ref_fifo_full))

        self.repeat_fsm.set_start_state(START)

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

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "repeat"

#     def get_bitstream(self, stop_lvl=0, root=0):
    def get_bitstream(self, config_kwargs):

        stop_lvl = config_kwargs['stop_lvl']
        root = config_kwargs['root']
        if 'spacc_mode' in config_kwargs:
            spacc_mode = config_kwargs['spacc_mode']

        # Store all configurations here
        config = [("tile_en", 1),
                  ("stop_lvl", stop_lvl),
                  ("root", root),
                  ("spacc_mode", spacc_mode)
                  ]
        return config


if __name__ == "__main__":

    repeat_dut = Repeat(data_width=16,
                        defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")
    verilog(repeat_dut, filename="repeat.sv",
            optimize_if=False)
