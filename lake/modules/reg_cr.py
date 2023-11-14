import kratos as kts
from kratos import *
from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.top.memory_controller import MemoryController
from lake.utils.util import add_counter, safe_wire, sticky_flag, add_counter
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class Reg(MemoryController):
    def __init__(self,
                 data_width=16,
                 add_dispatcher=False,
                 dispatcher_size=2,
                 fifo_depth=8,
                 defer_fifos=True,
                 add_flush=False,
                 perf_debug=True):

        super().__init__("reg_cr", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = add_flush
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos
        self.perf_debug = perf_debug

        self.add_dispatcher = add_dispatcher
        self.dispatcher_size = dispatcher_size

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
        self._data_in = self.input("data_in", self.data_width + 1, packed=True)
        self._data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_in = self.input("data_in_valid", 1)
        self._valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ready_out = self.output("data_in_ready", 1)
        self._ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._eos_in = self.input("eos_in", 1)
        # self._eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._data_out = self.output("data_out", self.data_width + 1, packed=True)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._ready_in = self.input("data_out_ready", 1)
        self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._valid_out = self.output("data_out_valid", 1)
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._eos_out = self.output("eos_out", 1)
        # self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Store the default value...
        self._default_value = self.input("default_value", self.data_width)
        self._default_value.add_attribute(ConfigRegAttr("Default value for accumulation"))

        # Set the reduction stop level - crush at and below, pass above
        self._stop_lvl = self.input("stop_lvl", 16)
        self._stop_lvl.add_attribute(ConfigRegAttr("What level stop tokens should this reduction block crush"))

        # Interface with the PE
        self._data_to_pe0 = self.output("data_to_pe0", self.data_width + 1, packed=True)
        self._data_to_pe1 = self.output("data_to_pe1", self.data_width + 1, packed=True)
        self._data_from_pe = self.input("data_from_pe", self.data_width + 1, packed=True)

        # Declare the accum reg
        self._accum_reg = self.var("accum_reg", self.data_width)
        self._data_to_fifo = self.var("data_to_fifo", self.data_width)

# ==============================
# INPUT FIFO
# ==============================
        self._infifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._infifo.add_attribute(SharedFifoAttr(direction="IN"))

        # Ready is just a function of having room in the FIFO
        self.wire(self._ready_out, ~self._infifo.ports.full)

        # Convert to packed
        self._infifo_in_packed = self.var("infifo_in_packed", self.data_width + 1, packed=True)
        self._infifo_out_packed = self.var("infifo_out_packed", self.data_width + 1, packed=True)

        self._infifo_out_eos = self.var("infifo_out_eos", 1)
        self._infifo_out_valid = self.var("infifo_out_valid", 1)
        self._infifo_out_data = self.var("infifo_out_data", self.data_width)

        # indicate valid data as well
        # self.wire(self._infifo_in_packed[self.data_width], self._eos_in)
        # self.wire(self._infifo_in_packed[self.data_width], self._valid_in)
        self.wire(self._infifo_in_packed[self.data_width, 0], self._data_in)

        self.wire(self._infifo_out_eos, self._infifo_out_packed[self.data_width])
        # self.wire(self._infifo_out_valid, self._infifo_out_packed[self.data_width])
        self.wire(self._infifo_out_data, self._infifo_out_packed[self.data_width - 1, 0])

        # self._infifo_valid_entry = self.var("infifo_valid_entry", 1)

        # Push when there's incoming transaction and room to accept it
        self._infifo_push = self.var("infifo_push", 1)
        # self.wire(self._infifo_push, (self._valid_in | self._eos_in) & (~self._infifo.ports.full))
        self.wire(self._infifo_push, self._valid_in)

        # Pop when ready to accum more streams
        self._infifo_pop = self.var("infifo_pop", 1)

        self.add_child(f"input_fifo",
                       self._infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._infifo_push,
                       pop=self._infifo_pop,
                       data_in=self._infifo_in_packed,
                       data_out=self._infifo_out_packed)

        self.wire(self._infifo_out_valid, ~self._infifo.ports.empty)
        self.wire(self._data_to_pe0, self._infifo_out_packed)
        # Loop for accumulation
        self.wire(self._data_to_pe1[self.data_width - 1, 0], self._accum_reg)
        self.wire(self._data_to_pe1[self.data_width], kts.const(0, 1))

# ==============================
# OUTPUT FIFO
# ==============================
        self._outfifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

        # Convert to packed
        self._outfifo_in_packed = self.var("outfifo_in_packed", self.data_width + 1, packed=True)
        self._outfifo_out_packed = self.var("outfifo_out_packed", self.data_width + 1, packed=True)

        self._outfifo_in_eos = self.var("outfifo_in_eos", 1)
        # self._outfifo_in_valid = self.var("outfifo_in_valid", 1)
        # self._outfifo_in_data = self.var("outfifo_in_data", self.data_width)

        # indicate valid data as well
        # self.wire(self._outfifo_in_packed[self.data_width + 1], self._outfifo_in_eos)
        # self.wire(self._outfifo_in_packed[self.data_width], self._outfifo_in_valid)
        # self.wire(self._outfifo_in_packed[self.data_width], kts.const(1, 1))
        self.wire(self._outfifo_in_packed[self.data_width], self._outfifo_in_eos)
        self.wire(self._outfifo_in_packed[self.data_width - 1, 0], self._data_to_fifo)

        # self.wire(self._outfifo_out_valid, self._outfifo_out_packed[self.data_width + 1])
        # self.wire(self._eos_out, self._outfifo_out_packed[self.data_width])
        self.wire(self._data_out, self._outfifo_out_packed[self.data_width, 0])

        self._outfifo_valid_entry = self.var("outfifo_valid_entry", 1)

        # Push when there's incoming transaction and room to accept it
        self._outfifo_push = self.var("outfifo_push", 1)

        # Pop when ready to accum more streams
        self._outfifo_pop = self.var("outfifo_pop", 1)
        self._outfifo_full = self.var("outfifo_full", 1)
        # self._outfifo_empty = self.var("outfifo_empty", 1)

        self.add_child(f"output_fifo",
                       self._outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._outfifo_push,
                       pop=self._outfifo_pop,
                       data_in=self._outfifo_in_packed,
                       data_out=self._outfifo_out_packed)

        self.wire(self._valid_out, ~self._outfifo.ports.empty)

        self.wire(self._outfifo_pop, self._ready_in)
        self.wire(self._outfifo_full, self._outfifo.ports.full)

# =============================
# ACCUM FSM
# =============================

        # self._set_accum_happened = self.var("set_accum_happened", 1)
        # self._clr_accum_happened = self.var("clr_accum_happened", 1)
        # self._accum_happened = sticky_flag(self, self._set_accum_happened, clear=self._clr_accum_happened, name='accum_happened_reg')

        self._reg_clr = self.var("reg_clr", 1)
        self._update_accum_reg = self.var("update_accum_reg", 1)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def accum_reg_ff():
            if ~self._rst_n:
                self._accum_reg = 0
            elif self._reg_clr:
                self._accum_reg = self._default_value
            elif self._update_accum_reg:
                self._accum_reg = self._data_from_pe[self.data_width - 1, 0]
        self.add_code(accum_reg_ff)

        # Create FSM
        self.accum_fsm = self.add_fsm("accum_seq", reset_high=False)
        START = self.accum_fsm.add_state("START")
        self.accum_fsm.set_start_state(START)
        ACCUM = self.accum_fsm.add_state("ACCUM")
        OUTPUT = self.accum_fsm.add_state("OUTPUT")
        STOP_PASS = self.accum_fsm.add_state("STOP_PASS")
        DONE = self.accum_fsm.add_state("DONE")

        self.accum_fsm.output(self._infifo_pop)
        self.accum_fsm.output(self._outfifo_push)
        self.accum_fsm.output(self._data_to_fifo)
        self.accum_fsm.output(self._outfifo_in_eos)
        self.accum_fsm.output(self._reg_clr)
        self.accum_fsm.output(self._update_accum_reg)
        # State Transitions

        # In START, we are looking for some valid data and checking if the PE is ready
        # If we see EOS, we know we can consume the stop token
        # If we don't see EOS, we can start accumulating
        START.next(ACCUM, self._infifo_out_valid & ~self._infifo_out_eos)
        START.next(DONE, self._infifo_out_valid & self._infifo_out_eos & (self._infifo_out_data[9, 8] == kts.const(1, 2)))
        START.next(OUTPUT, self._infifo_out_valid & self._infifo_out_eos & (self._infifo_out_data[9, 8] == kts.const(0, 2)))
        START.next(START, None)
        # START.next(OUTPUT, self._infifo_out_valid & self._infifo_out_eos)

        # In ACCUM, we are just accumulating data as long as we have valids
        # Need to crush lower level stops then move on when we see the appropriate level
        # ACCUM.next(ACCUM, self._infifo_out_valid & self._infifo_out_eos & (self._infifo_out_data > self._stop_lvl))
        # ACCUM.next(OUTPUT, self._infifo_out_valid & self._infifo_out_eos & (self._infifo_out_data == self._stop_lvl))
        ACCUM.next(OUTPUT, self._infifo_out_valid & self._infifo_out_eos)
        ACCUM.next(ACCUM, None)

        OUTPUT.next(STOP_PASS, ~self._outfifo_full)
        # OUTPUT.next(STOP_PASS, ~self._outfifo_full | ~self._accum_happened)
        OUTPUT.next(OUTPUT, None)

        # Basically pass through until we get a new valid data...otherwise we are technically done
        # Or until we hit a new empty stream which would be indicated by a crushable stop
        # STOP_PASS.next(START, self._infifo_out_valid & (~self._infifo_out_eos | (self._infifo_out_eos & (self._infifo_out_data >= self._stop_lvl))))

        # STOP PASS is basically going to crush stop levels until we see new data or the DONE token
        # STOP_PASS.next(START, self._infifo_out_valid & (~self._infifo_out_eos | (self._infifo_out_eos & (self._infifo_out_data[9, 8] == kts.const(1, 2)))))
        # Just crush a single stop
        STOP_PASS.next(START, ~self._outfifo_full)
        STOP_PASS.next(STOP_PASS, None)

        # In DONE, we just pass the DONE token
        DONE.next(START, ~self._outfifo_full)
        DONE.next(DONE, None)

        #############
        # START
        #############
        # When in START, if we see just eos but not valid, then that was a blank stream and we should pop it...
        # START.output(self._infifo_pop, self._infifo_out_eos & self._infifo_out_valid)
        START.output(self._infifo_pop, 0)
        START.output(self._outfifo_push, 0)
        START.output(self._data_to_fifo, kts.const(0, 16))
        START.output(self._outfifo_in_eos, 0)
        START.output(self._reg_clr, 0)
        START.output(self._update_accum_reg, 0)

        #############
        # ACCUM
        #############
        # Pop if we have a valid data or an eos above the stop level to crush
        # The data pushed in previously is coming back, ready to push another one
        ACCUM.output(self._infifo_pop, self._infifo_out_valid & ~self._infifo_out_eos)
        ACCUM.output(self._outfifo_push, 0)
        ACCUM.output(self._data_to_fifo, kts.const(0, 16))
        ACCUM.output(self._outfifo_in_eos, 0)
        ACCUM.output(self._reg_clr, 0)
        ACCUM.output(self._update_accum_reg, self._infifo_out_valid & ~self._infifo_out_eos)

        #############
        # OUTPUT
        #############
        # Don't pop the fifo, just use this state to output a value to the output fifo
        # TODO: Merge states with ACCUM
        OUTPUT.output(self._infifo_pop, 0)
        OUTPUT.output(self._outfifo_push, ~self._outfifo_full)
        # OUTPUT.output(self._outfifo_push, ~self._outfifo_full & self._accum_happened)
        OUTPUT.output(self._reg_clr, 0)
        OUTPUT.output(self._data_to_fifo, self._accum_reg)
        OUTPUT.output(self._outfifo_in_eos, 0)
        OUTPUT.output(self._update_accum_reg, 0)

        #############
        # STOP_PASS - Deal with full output...
        #############
        # Only rip the stop off if its below the stop level
        # STOP_PASS.output(self._infifo_pop, ~self._outfifo_full & self._infifo_out_valid & self._infifo_out_eos & (self._infifo_out_data < self._stop_lvl))
        # Only pop off stop tokens
        STOP_PASS.output(self._infifo_pop, ~self._outfifo_full & self._infifo_out_valid & self._infifo_out_eos & (self._infifo_out_data[9, 8] == kts.const(0, 2)))
        # STOP_PASS.output(self._outfifo_push, ~self._outfifo_full & self._infifo_out_valid & self._infifo_out_eos & (self._infifo_out_data < self._stop_lvl))
        # Only push stop tokens that aren't being completely squashed
        STOP_PASS.output(self._outfifo_push, ~self._outfifo_full & self._infifo_out_valid & self._infifo_out_eos &
                         (self._infifo_out_data[9, 8] == 0) & (self._infifo_out_data[7, 0] > 0))
        STOP_PASS.output(self._reg_clr, 1)
        # Reduce the stop level by 1 - if it's 0, the stop token gets squashed
        STOP_PASS.output(self._data_to_fifo, self._infifo_out_data - 1)
        # STOP_PASS.output(self._outfifo_in_eos, self._infifo_out_valid & self._infifo_out_eos)
        STOP_PASS.output(self._outfifo_in_eos, 1)
        STOP_PASS.output(self._update_accum_reg, 0)
        # STOP_PASS.output(self._set_accum_happened, 0)
        # STOP_PASS.output(self._clr_accum_happened, 1)

        #############
        # DONE
        #############
        '''
        When in DONE, we are just passing on the DONE token
        '''
        DONE.output(self._infifo_pop, ~self._outfifo_full)
        DONE.output(self._outfifo_push, ~self._outfifo_full)
        DONE.output(self._reg_clr, 1)
        DONE.output(self._data_to_fifo, self._infifo_out_data)
        DONE.output(self._outfifo_in_eos, self._infifo_out_eos)
        DONE.output(self._update_accum_reg, 0)

        if self.add_dispatcher:
            self.add_dispatch_logic()

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

        if self.perf_debug:

            cyc_count = add_counter(self, "clock_cycle_count", 64, increment=self._clk & self._clk_en)

            # Start when any of the coord inputs is valid
            self._start_signal = sticky_flag(self, kts.concat((*[self._valid_in[i] for i in range(1)])).r_or(),
                                             name='start_indicator')
            self.add_performance_indicator(self._start_signal, edge='posedge', label='start', cycle_count=cyc_count)

            # End when we see DONE on the output coord
            self._done_signal = sticky_flag(self, (self._data_out == MemoryController.DONE_PROXY) &
                                                    self._valid_out,
                                                    name='done_indicator')
            self.add_performance_indicator(self._done_signal, edge='posedge', label='done', cycle_count=cyc_count)

        # Finally, lift the config regs...
        lift_config_reg(self.internal_generator)

    def add_dispatch_logic(self):

        self.f_iters = []
        self.f_coords = []
        self.f_sms = []

        # Need an addr out + read enable, then data incoming
        addr_out = []
        dsp_data_in = []
        read_out = []
        for i in range(self.dispatcher_size):
            new_addr_out = self.output(f"addr_out_{i}", self.data_width)
            new_addr_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            addr_out.append(new_addr_out)

            new_data_in = self.input(f"dsp_data_in_{i}", self.data_width)
            new_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            dsp_data_in.append(new_data_in)

            new_ren = self.output(f"dsp_ren_out_{i}", 1)
            new_ren.add_attribute(ControlSignalAttr(is_control=False))
            read_out.append(new_ren)

        # Basically want a set of iterators for each dispatcher
        for i in range(self.dispatcher_size):
            # Create read address generator
            FIBER_ITER = ForLoop(iterator_support=2,
                                 config_width=16)
            FIBER_COORD = AddrGen(iterator_support=2,
                                  config_width=16)

            self.f_iters.append(FIBER_ITER)
            self.f_coords.append(FIBER_COORD)

            self.add_child(f"fiber_iter_{i}",
                           FIBER_ITER,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           step=self._step_outer)

            # Whatever comes through here should hopefully just pipe through seamlessly
            # addressor modules

            self.add_child(f"fiber_coord_{i}",
                           FIBER_COORD,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           step=self._step_outer,
                           mux_sel=FIBER_ITER.ports.mux_sel_out,
                           restart=FIBER_ITER.ports.restart)

            self._outer_addr = self.var("outer_addr", 16)
            safe_wire(self, self._outer_addr, self.FIBER_OUTER_ADDR.ports.addr_out)

            self._outer_restart = self.var("outer_restart", 1)
            self.wire(self._outer_restart, self.FIBER_OUTER_ITER.ports.restart)

            # Create the SM
            DISPATCH_FSM = self.add_fsm(f"dispatch_fsm_{i}", reset_high=False)
            START = DISPATCH_FSM.add_state("START")
            DISPATCH = DISPATCH_FSM.add_state("DISPATCH")
            SRCH_LKUP_R = DISPATCH_FSM.add_state("SRCH_LKUP_R")
            SRCH_LKUP_CHK = DISPATCH_FSM.add_state("SRCH_LKUP_CHK")
            SYNC_RESULT = DISPATCH_FSM.add_state("SYNC_RESULT")
            DONE = DISPATCH_FSM.add_state("DONE")

            # Set restart state
            DISPATCH_FSM.set_start_state(START)

            # Define state outputs
            DISPATCH_FSM.output(addr_out[i])
            DISPATCH_FSM.output(read_out[i])

            ####################
            # Next State Logic
            ####################

            # Dummy state for eventual filling block.
            START.next(ISSUE_STRM, self._root)
            START.next(ISSUE_STRM_NR, ~self._root)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "reduce"

    # No actual config at this level
    # def get_bitstream(self, stop_lvl):
    def get_bitstream(self, config_kwargs):

        stop_lvl = config_kwargs['stop_lvl']

        # Store all configurations here
        config = [("tile_en", 1),
                  ("default_value", 0),
                  ("stop_lvl", stop_lvl)]
        return config


if __name__ == "__main__":

    reg_dut = Reg(data_width=16, defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(reg_dut, filename="reg.sv",
            optimize_if=False)
