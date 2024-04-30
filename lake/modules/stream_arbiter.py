from math import e
from struct import pack
import kratos as kts
from kratos import *
from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.passes.passes import lift_config_reg
from lake.utils.util import sticky_flag, trim_config_list, add_counter, register
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_controller import MemoryController
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO
from lake.modules.arbiter import Arbiter
from enum import Enum, unique


class StreamArbiter(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 add_clk_enable=True,
                 add_flush=False,
                 lift_config=False,
                 defer_fifos=True,
                 perf_debug=True):

        name_str = f"stream_arbiter"
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

        self.num_streams = 4

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

        self._num_requests = self.input("num_requests", kts.clog2(self.num_streams))  # 0 to 3
        self._num_requests.add_attribute(ConfigRegAttr("Number of potential requests to arbitrate"))

        self._seg_mode = self.input("seg_mode", 1)
        self._seg_mode.add_attribute(ConfigRegAttr("Valid mode: 1 = seg and crd, 0 = value"))

        self._stream_in = []
        self._stream_in_valid_in = []
        self._stream_in_ready_out = []

        for i in range(self.num_streams):

            tmp_stream_in = self.input(f"stream_in_{i}", self.data_width + 1, packed=True)
            tmp_stream_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_stream_in_valid = self.input(f"stream_in_{i}_valid", 1)
            tmp_stream_in_valid.add_attribute(ControlSignalAttr(is_control=True))

            tmp_stream_in_ready = self.output(f"stream_in_{i}_ready", 1)
            tmp_stream_in_ready.add_attribute(ControlSignalAttr(is_control=False))

            self._stream_in.append(tmp_stream_in)
            self._stream_in_valid_in.append(tmp_stream_in_valid)
            self._stream_in_ready_out.append(tmp_stream_in_ready)

        # STRAM OUT.
        self._stream_out = self.output("stream_out", self.data_width + 1, packed=True)
        self._stream_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._stream_out_ready_in = self.input("stream_out_ready", 1)
        self._stream_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._stream_out_valid_out = self.output("stream_out_valid", 1)
        self._stream_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

# ==========================================
# FIFO inputs
# ==========================================

        self._stream_in_fifo_in = []
        self._stream_in_fifo_valid_in = []

        self._pop_fifo = self.var("pop_fifo", self.num_streams)

        for i in range(self.num_streams):

            # STREAM IN FIFOS
            tmp_stream_in_fifo = RegFIFO(data_width=self._stream_in[i].width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
            tmp_stream_in_fifo.add_attribute(SharedFifoAttr(direction="IN"))

            tmp_stream_in_valid_in = self.var(f"stream_in_{i}_fifo_valid_in", 1)

            tmp_stream_in = self.var(f"stream_in_{i}_fifo_in", self.data_width + 1, packed=True)

            self.add_child(f"stream_in_fifo_{i}",
                           tmp_stream_in_fifo,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           clk_en=self._clk_en,
                           push=self._stream_in_valid_in[i],
                           pop=self._pop_fifo[i],
                           data_in=self._stream_in[i],
                           data_out=tmp_stream_in)

            self.wire(self._stream_in_ready_out[i], ~tmp_stream_in_fifo.ports.full)
            self.wire(tmp_stream_in_valid_in, ~tmp_stream_in_fifo.ports.empty & (self._num_requests >= i))  # Filter out unconfigured channels

            self._stream_in_fifo_in.append(tmp_stream_in)
            self._stream_in_fifo_valid_in.append(tmp_stream_in_valid_in)

        # NO perf_debug in RTL


# ==========================================
# FIFO output
# ==========================================

        self._stream_out_fifo = RegFIFO(data_width=self._stream_out.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._stream_out_fifo.add_attribute(SharedFifoAttr(direction="OUT"))
        self._stream_out_fifo_ready = self.var("stream_out_fifo_ready", 1)

# ==========================================
# Generate FSM for Intersecting these streams...
# ==========================================

        self._fifo_push = self.var("fifo_push", 1)
        self._fifo_full = self.var("fifo_full", 1)

        self._stream_to_fifo = self.var("stream_to_fifo", self.data_width + 1)
        self._stream_to_fifo_packed = self.var("stream_to_fifo_packed", self.data_width + 1, packed=True)
        self.wire(self._stream_to_fifo_packed, self._stream_to_fifo)

        self._set_stream_length = self.var("set_stream_length", 1)
        self._clr_stream_length = self.var("clr_stream_length", 1)
        self._max_stream_length = self.var("max_stream_length", self.data_width)
        # by seq_only = True, we have 1 cycle delay. Function as a valid signal here, for case of str length = 0
        self._stream_max_sticky = sticky_flag(self, self._set_stream_length, self._clr_stream_length, "max_stream_length_sticky", seq_only=True)

        self._inc_stream_pass = self.var("inc_stream_pass", 1)
        self._clr_stream_pass = self.var("clr_stream_pass", 1)
        self._num_stream_pass = add_counter(self, name="num_stream_pass", bitwidth=self.data_width, increment=self._inc_stream_pass, clear=self._clr_stream_pass)

        self._stream_done = self.var("stream_done", 1)
        self.wire(self._stream_done, (self._num_stream_pass == self._max_stream_length) & self._stream_max_sticky)

        self._arb_req = self.var("arb_req", self.num_streams)
        self._arb_grant = self.var("arb_grant", self.num_streams)
        self._rr_arbiter = Arbiter(ins=self.num_streams, algo="RR_STREAM")

        self._any_grant = self.var("any_grant", 1)
        self.wire(self._any_grant, self._arb_grant.r_or())
        self._any_pop = self.var("any_pop", 1)
        self.wire(self._any_pop, self._pop_fifo.r_or())

        _stream_in_fifo_valid_in_reversed = self._stream_in_fifo_valid_in[::-1]
        self._ori_arb_req = kts.concat(*_stream_in_fifo_valid_in_reversed)

        self._clear_grant = self.var("clear_grant", 1)
        self._lock_grant = register(self, self._arb_grant, enable=self._any_grant, clear=self._clear_grant, name="lock_grant", packed=True)

        self.add_code(self.lock_max_stream_length)
        self.add_code(self.stream_out_mux)

        self.add_child(f"rr_arbiter",
                       self._rr_arbiter,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       request_in=self._arb_req,
                       grant_out=self._arb_grant,
                       resource_ready=self._stream_out_fifo_ready,
                       num_req=self._num_requests)

        # Create FSM
        self.stream_arbiter_fsm = self.add_fsm("stream_arbiter_seq", reset_high=False)
        WAIT = self.stream_arbiter_fsm.add_state("WAIT")
        PASS1 = self.stream_arbiter_fsm.add_state("PASS1")  # for seg or val
        PASS2 = self.stream_arbiter_fsm.add_state("PASS2")  # for crd if seg_mode = 1

        for i in range(self.num_streams):
            self.stream_arbiter_fsm.output(self._pop_fifo[i])
        self.stream_arbiter_fsm.output(self._fifo_push)
        self.stream_arbiter_fsm.output(self._inc_stream_pass)
        self.stream_arbiter_fsm.output(self._clr_stream_pass)
        self.stream_arbiter_fsm.output(self._set_stream_length)
        self.stream_arbiter_fsm.output(self._clr_stream_length)
        self.stream_arbiter_fsm.output(self._arb_req)
        self.stream_arbiter_fsm.output(self._clear_grant)

        ####################
        # Next State Logic
        ####################
        WAIT.next(PASS1, self._any_grant)
        WAIT.next(WAIT, None)

        PASS1.next(PASS2, self._seg_mode & self._stream_done)
        PASS1.next(WAIT, ~self._seg_mode & self._stream_done)
        PASS1.next(PASS1, None)

        PASS2.next(WAIT, self._stream_done)
        PASS2.next(PASS2, None)

        ####################
        # FSM Output Logic
        ####################

        #######
        # WAIT
        #######
        for i in range(self.num_streams):
            WAIT.output(self._pop_fifo[i], self._arb_grant[i])
        WAIT.output(self._fifo_push, self._any_pop)
        WAIT.output(self._inc_stream_pass, 0)
        WAIT.output(self._clr_stream_pass, 0)
        WAIT.output(self._set_stream_length, 0)
        WAIT.output(self._clr_stream_length, 0)
        WAIT.output(self._arb_req, self._ori_arb_req)
        WAIT.output(self._clear_grant, 0)

        #######
        # PASS1
        #######
        for i in range(self.num_streams):
            PASS1.output(self._pop_fifo[i], self._lock_grant[i] & self._stream_out_fifo_ready & ~self._stream_done)
        PASS1.output(self._fifo_push, self._any_pop)
        # don't increment for the first cycle
        PASS1.output(self._inc_stream_pass, self._any_pop & self._stream_max_sticky & ~self._stream_done)  # TODO: check for the special case with 0 length
        PASS1.output(self._clr_stream_pass, self._stream_done)
        PASS1.output(self._set_stream_length, self._any_pop & ~self._stream_max_sticky)
        PASS1.output(self._clr_stream_length, self._stream_done)
        PASS1.output(self._arb_req, 0)
        PASS1.output(self._clear_grant, self._stream_done & ~self._seg_mode)

        #######
        # PASS2
        #######
        for i in range(self.num_streams):
            PASS2.output(self._pop_fifo[i], self._lock_grant[i] & self._stream_out_fifo_ready & ~self._stream_done)
        PASS2.output(self._fifo_push, self._any_pop)
        PASS2.output(self._inc_stream_pass, self._any_pop & self._stream_max_sticky & ~self._stream_done)
        PASS2.output(self._clr_stream_pass, self._stream_done)
        PASS2.output(self._set_stream_length, self._any_pop & ~self._stream_max_sticky)
        PASS2.output(self._clr_stream_length, self._stream_done)
        PASS2.output(self._arb_req, 0)
        PASS2.output(self._clear_grant, self._stream_done)

        self.stream_arbiter_fsm.set_start_state(WAIT)

# ===================================
# FIFO output instantiate
# ===================================

        self.add_child(f"stream_out_fifo",
                       self._stream_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._fifo_push,
                       pop=self._stream_out_ready_in,
                       data_in=self._stream_to_fifo_packed,
                       data_out=self._stream_out,
                       full=self._fifo_full)

        self.wire(self._stream_out_valid_out, ~self._stream_out_fifo.ports.empty)
        self.wire(self._stream_out_fifo_ready, ~self._stream_out_fifo.ports.full)

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

    def get_bitstream(self, config_kwargs):

        num_requests = config_kwargs['num_requests']
        seg_mode = config_kwargs['seg_mode']

        # Store all configurations here
        config = [("tile_en", 1),
                  ("num_requests", num_requests),
                  ("seg_mode", seg_mode)]

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
        return "stream_arbiter"

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def lock_max_stream_length(self):
        if ~self._rst_n:
            self._max_stream_length = 0
        elif self._set_stream_length:
            self._max_stream_length = self._stream_to_fifo[self.data_width-1, 0]
        elif self._clr_stream_length:
            self._max_stream_length = 0
        else:
            self._max_stream_length = self._max_stream_length

    @always_comb
    def stream_out_mux(self):  # hardcoded to support input of 4
        if self._arb_grant[0] | self._lock_grant[0]:
            self._stream_to_fifo = self._stream_in_fifo_in[0]
        elif self._arb_grant[1] | self._lock_grant[1]:
            self._stream_to_fifo = self._stream_in_fifo_in[1]
        elif self._arb_grant[2] | self._lock_grant[2]:
            self._stream_to_fifo = self._stream_in_fifo_in[2]
        elif self._arb_grant[3] | self._lock_grant[3]:
            self._stream_to_fifo = self._stream_in_fifo_in[3]
        else:
            self._stream_to_fifo = 0


if __name__ == "__main__":
    stream_arbiter_dut = StreamArbiter(data_width=16,
                              use_merger=False,
                              defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(stream_arbiter_dut, filename="stream_arbiter.sv",
            optimize_if=False)
