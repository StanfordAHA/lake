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


class RepeatSignalGenerator(MemoryController):
    def __init__(self,
                 data_width=16,
                 passthru=True,
                 fifo_depth=8,
                 defer_fifos=True,
                 add_flush=False,
                 perf_debug=True):

        super().__init__("RepeatSignalGenerator", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = add_flush
        self.passthru = passthru
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

        # BASE IN
        self._base_data_in = self.input("base_data_in", self.data_width + 1, packed=True)
        self._base_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._base_eos_in = self.input("base_eos_in", 1)
        # self._base_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._base_ready_out = self.output("base_data_in_ready", 1)
        self._base_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._base_valid_in = self.input("base_data_in_valid", 1)
        self._base_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        # Data out (right now its a repsig...)
        self._repsig_data_out = self.output("repsig_data_out", self.data_width + 1, packed=True)
        self._repsig_data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._repsig_ready_in = self.input("repsig_data_out_ready", 1)
        self._repsig_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._repsig_valid_out = self.output("repsig_data_out_valid", 1)
        self._repsig_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._repsig_eos_out = self.output("repsig_eos_out", 1)
        # self._repsig_eos_out.add_attribute(ControlSignalAttr(is_control=False))

        if self.passthru:
            # Passthru out
            self._passthru_data_out = self.output("passthru_data_out", self.data_width + 1, packed=True)
            self._passthru_data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            self._passthru_ready_in = self.input("passthru_data_out_ready", 1)
            self._passthru_ready_in.add_attribute(ControlSignalAttr(is_control=True))

            self._passthru_valid_out = self.output("passthru_data_out_valid", 1)
            self._passthru_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._passthru_eos_out = self.output("passthru_eos_out", 1)
        # self._passthru_eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Config regs

        # Set the stop token injection level
        self._stop_lvl = self.input("stop_lvl", self.data_width)
        self._stop_lvl.add_attribute(ConfigRegAttr("What level stop tokens should this scanner inject"))

        if self.perf_debug:

            cyc_count = add_counter(self, "clock_cycle_count", 64, increment=self._clk & self._clk_en)

            # Start when any of the coord inputs is valid
            self._start_signal = sticky_flag(self, self._base_valid_in,
                                             name='start_indicator')
            self.add_performance_indicator(self._start_signal, edge='posedge', label='start', cycle_count=cyc_count)

            # End when we see DONE on the output coord
            self._done_signal = sticky_flag(self, (self._repsig_data_out == MemoryController.DONE_PROXY) &
                                                    self._repsig_valid_out,
                                                    name='done_indicator')
            self.add_performance_indicator(self._done_signal, edge='posedge', label='done', cycle_count=cyc_count)

# ==============================
# INPUT FIFO
# ==============================

        # BASE fifo
        self._base_fifo_pop = self.var("base_fifo_pop", 1)
        self._base_fifo_valid = self.var("base_fifo_valid", 1)

        self._base_fifo_in = kts.concat(self._base_data_in)
        self._base_in_fifo = RegFIFO(data_width=self._base_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._base_in_fifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._base_fifo_out_data = self.var("base_fifo_out_data", self.data_width, packed=True)
        self._base_fifo_out_eos = self.var("base_fifo_out_eos", 1)

        self.add_child(f"base_in_fifo",
                       self._base_in_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._base_valid_in,
                       pop=self._base_fifo_pop,
                       data_in=self._base_fifo_in,
                       data_out=kts.concat(self._base_fifo_out_eos, self._base_fifo_out_data))

        self.wire(self._base_ready_out, ~self._base_in_fifo.ports.full)
        self.wire(self._base_fifo_valid, ~self._base_in_fifo.ports.empty)

# ==============================
# OUTPUT FIFO
# ==============================

        # REPSIG FIFO
        self._repsig_fifo_push = self.var("repsig_fifo_push", 1)
        self._repsig_fifo_full = self.var("repsig_fifo_full", 1)

        self._repsig_fifo_in_data = self.var("repsig_fifo_in_data", self.data_width)
        self._repsig_fifo_in_eos = self.var("repsig_fifo_in_eos", 1)

        self._repsig_fifo_in = kts.concat(self._repsig_fifo_in_eos, self._repsig_fifo_in_data)
        self._repsig_out_fifo = RegFIFO(data_width=self._repsig_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._repsig_out_fifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self.add_child(f"repsig_out_fifo",
                       self._repsig_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._repsig_fifo_push,
                       pop=self._repsig_ready_in,
                       data_in=self._repsig_fifo_in,
                       data_out=kts.concat(self._repsig_data_out))

        self.wire(self._repsig_fifo_full, self._repsig_out_fifo.ports.full)
        self.wire(self._repsig_valid_out, ~self._repsig_out_fifo.ports.empty)

        if self.passthru:
            # PASSTHRU FIFO
            self._passthru_fifo_push = self.var("passthru_fifo_push", 1)
            self._passthru_fifo_full = self.var("passthru_fifo_full", 1)

            self._passthru_fifo_in_data = self.var("passthru_fifo_in_data", self.data_width)
            self._passthru_fifo_in_eos = self.var("passthru_fifo_in_eos", 1)

            self._passthru_fifo_in = kts.concat(self._passthru_fifo_in_eos, self._passthru_fifo_in_data)
            self._passthru_out_fifo = RegFIFO(data_width=self._passthru_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
            self._passthru_out_fifo.add_attribute(SharedFifoAttr(direction="OUT"))

            self.add_child(f"passthru_out_fifo",
                           self._passthru_out_fifo,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           clk_en=self._clk_en,
                           push=self._passthru_fifo_push,
                           pop=self._passthru_ready_in,
                           data_in=self._passthru_fifo_in,
                           data_out=kts.concat(self._passthru_data_out))

            self.wire(self._passthru_fifo_full, self._passthru_out_fifo.ports.full)
            self.wire(self._passthru_valid_out, ~self._passthru_out_fifo.ports.empty)

# =============================
# Various Logic
# =============================
        self._seen_root_eos = sticky_flag(self, (self._base_fifo_out_data[9, 8] == kts.const(1, 2)) & self._base_fifo_out_eos & self._base_fifo_valid, name="seen_root_eos")

        # Passthru is trivial, just push it when the other one is being pushed
        if self.passthru:
            self.wire(self._passthru_fifo_in_data, self._base_fifo_out_data)
            self.wire(self._passthru_fifo_in_eos, self._base_fifo_out_eos)

        self._clr_already_pushed_repsig_eos = self.var("clr_already_pushed_repsig_eos", 1)
        self._already_pushed_repsig_eos = sticky_flag(self, self._repsig_fifo_push & ~self._repsig_fifo_full, clear=self._clr_already_pushed_repsig_eos, name="already_pushed_repsig_eos", seq_only=True)

# =============================
# Instantiate FSM
# =============================

        self.rsg_fsm = self.add_fsm("rsg_fsm", reset_high=False)

        START = self.rsg_fsm.add_state("START")
        PASS_REPEAT = self.rsg_fsm.add_state("PASS_REPEAT")
        PASS_STOP = self.rsg_fsm.add_state("PASS_STOP")
        DONE = self.rsg_fsm.add_state("DONE")

# =============================
# FSM Transitions
# =============================

        #####################
        # START
        #####################
        START.next(PASS_REPEAT, self._tile_en)
        START.next(START, None)

        #####################
        # PASS_REPEAT
        #####################
        PASS_REPEAT.next(PASS_STOP, self._base_fifo_out_eos & self._base_fifo_valid)
        PASS_REPEAT.next(PASS_REPEAT, None)

        #####################
        # PASS_STOP
        #####################
        # PASS_STOP.next(DONE, self._seen_root_eos & ~self._repsig_fifo_full)
        # Can be done once we are pushing DONE
        PASS_STOP.next(DONE, (self._base_fifo_valid & self._base_fifo_out_eos & (self._base_fifo_out_data[9, 8] == kts.const(1, 2))) & ~self._repsig_fifo_full)
        # If we hit more data, we have to go back to passing repeats
        PASS_STOP.next(PASS_REPEAT, self._base_fifo_valid & ~self._base_fifo_out_eos)
        PASS_STOP.next(PASS_STOP, None)

        #####################
        # DONE
        #####################
        DONE.next(START, None)

# =============================
# FSM Output Declaration
# =============================
        self.rsg_fsm.output(self._repsig_fifo_in_data)
        self.rsg_fsm.output(self._repsig_fifo_in_eos)
        self.rsg_fsm.output(self._repsig_fifo_push)
        self.rsg_fsm.output(self._base_fifo_pop)
        self.rsg_fsm.output(self._clr_already_pushed_repsig_eos)
        if self.passthru:
            self.rsg_fsm.output(self._passthru_fifo_push)

# =============================
# FSM Output Implementation
# =============================
        #####################
        # START
        #####################
        START.output(self._repsig_fifo_in_data, 0)
        START.output(self._repsig_fifo_in_eos, 0)
        START.output(self._repsig_fifo_push, 0)
        START.output(self._base_fifo_pop, 0)
        START.output(self._clr_already_pushed_repsig_eos, 0)
        if self.passthru:
            START.output(self._passthru_fifo_push, 0)

        #####################
        # PASS_REPEAT
        #####################
        PASS_REPEAT.output(self._repsig_fifo_in_data, kts.const(1, 16))
        PASS_REPEAT.output(self._repsig_fifo_in_eos, 0)
        # Push the data on if it is data
        PASS_REPEAT.output(self._repsig_fifo_push, ~self._base_fifo_out_eos & self._base_fifo_valid)
        # Pop the incoming if it is data and there's room in the output fifo
        PASS_REPEAT.output(self._clr_already_pushed_repsig_eos, 1)
        if self.passthru:
            PASS_REPEAT.output(self._passthru_fifo_push, ~self._base_fifo_out_eos & self._base_fifo_valid)
            PASS_REPEAT.output(self._base_fifo_pop, ~self._base_fifo_out_eos & self._base_fifo_valid & ~self._repsig_fifo_full & ~self._passthru_fifo_full)
        else:
            PASS_REPEAT.output(self._base_fifo_pop, ~self._base_fifo_out_eos & self._base_fifo_valid & ~self._repsig_fifo_full)

        #####################
        # PASS_STOP
        #####################
        # We are passing along 0 unless it is a done token, in which case we pass it along
        PASS_STOP.output(self._repsig_fifo_in_data, kts.ternary(self._base_fifo_out_data[9, 8] == kts.const(1, 2),
                                                                # self._base_fifo_out_data, kts.const(0, self.data_width)))
                                                                self._base_fifo_out_data, self._base_fifo_out_data))
        PASS_STOP.output(self._repsig_fifo_in_eos, 1)
        # Only pass a single stop token to the repsig line for compliance, by construction the other
        # stop tokens will reappear on the proc lines of other repeat blocks
        # PASS_STOP.output(self._repsig_fifo_push, self._base_fifo_out_eos & self._base_fifo_valid & ~self._already_pushed_repsig_eos)
        # In the revised form, stop tokens are coalesced, so we only will emit one per input stop token anyway
        PASS_STOP.output(self._repsig_fifo_push, self._base_fifo_out_eos & self._base_fifo_valid)
        # Pass all stops to the passthru line
        PASS_STOP.output(self._clr_already_pushed_repsig_eos, 0)
        if self.passthru:
            PASS_STOP.output(self._passthru_fifo_push, self._base_fifo_out_eos & self._base_fifo_valid)
            PASS_STOP.output(self._base_fifo_pop, self._base_fifo_out_eos & self._base_fifo_valid & ~self._repsig_fifo_full & ~self._passthru_fifo_full)
        else:
            PASS_STOP.output(self._base_fifo_pop, self._base_fifo_out_eos & self._base_fifo_valid & ~self._repsig_fifo_full)

        #####################
        # DONE
        #####################
        DONE.output(self._repsig_fifo_in_data, 0)
        DONE.output(self._repsig_fifo_in_eos, 0)
        DONE.output(self._repsig_fifo_push, 0)
        DONE.output(self._base_fifo_pop, 0)
        DONE.output(self._clr_already_pushed_repsig_eos, 0)
        if self.passthru:
            DONE.output(self._passthru_fifo_push, 0)

        self.rsg_fsm.set_start_state(START)

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
        return "rsg"

    # def get_bitstream(self, stop_lvl=0):
    def get_bitstream(self, config_kwargs):

        stop_lvl = config_kwargs['stop_lvl']

        # Store all configurations here
        config = [("tile_en", 1),
                  ("stop_lvl", stop_lvl)
                  ]
        return config


if __name__ == "__main__":

    rsg_dut = RepeatSignalGenerator(data_width=16, passthru=True, defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")
    verilog(rsg_dut, filename="repeatsignalgenerator.sv",
            optimize_if=False)
