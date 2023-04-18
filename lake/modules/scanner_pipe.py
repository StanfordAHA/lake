import kratos as kts
from kratos import *
from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.modules.arbiter import Arbiter
from lake.modules.reservation_fifo import ReservationFIFO
from lake.passes.passes import lift_config_reg
from lake.top.memory_controller import MemoryController
from lake.utils.util import add_counter, register, sticky_flag, transform_strides_and_ranges, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class ScannerPipe(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 add_clk_enable=False,
                 add_flush=False,
                 lift_config=False,
                 defer_fifos=True,
                 perf_debug=True):

        self.data_width = data_width
        self.add_clk_enable = add_clk_enable
        self.add_flush = add_flush
        self.fifo_depth = fifo_depth
        self.lift_config = lift_config
        self.defer_fifos = defer_fifos
        self.perf_debug = perf_debug

        name_base = "scanner_pipe"
        if self.add_clk_enable:
            name_base = f"{name_base}_w_clk_enable"
        if self.add_flush:
            name_base = f"{name_base}_w_flush"

        super().__init__(name_base, debug=True)

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

        # Root-level scanner
        self._root = self.input("root", 1)
        self._root.add_attribute(ConfigRegAttr("If this scanner is at the root node, it will dispatch on its own"))

        # Dense scanner
        self._dense = self.input("dense", 1)
        self._dense.add_attribute(ConfigRegAttr("This scanner is 'scanning' a dense data structure..."))

        # Dimension Size
        self._dim_size = self.input("dim_size", 16)
        self._dim_size.add_attribute(ConfigRegAttr("This scanner is 'scanning' a dense data structure of this size..."))

        # Repeat number
        self._do_repeat = self.input("do_repeat", 1)
        self._do_repeat.add_attribute(ConfigRegAttr("If this scanner should do a repeat for creating outer coords"))

        # Repeat inner or outer
        self._repeat_outer_inner_n = self.input("repeat_outer_inner_n", 1)
        self._repeat_outer_inner_n.add_attribute(ConfigRegAttr("If this scanner should repeat inside or outside"))

        # Repeat inner or outer
        self._repeat_factor = self.input("repeat_factor", 16)
        self._repeat_factor.add_attribute(ConfigRegAttr("How many times this scanner should repeat inside or outside"))

        self._block_mode = self.input("block_mode", 1)
        self._block_mode.add_attribute(ConfigRegAttr("Performing block reads"))

        self._lookup_mode = self.input("lookup", 1)
        self._lookup_mode.add_attribute(ConfigRegAttr("Random access/lookup mode...."))

        # Set the stop token injection level - (default 0 + 1 for root)
        self._stop_lvl = self.input("stop_lvl", 16)
        self._stop_lvl.add_attribute(ConfigRegAttr("What level stop tokens should this scanner inject/In sparse accum mode, when to pop the data blocks"))

        # self._stop_lvl_spill = self.input("stop_lvl_spill", 16)
        # self._stop_lvl_spill.add_attribute(ConfigRegAttr("What level stop tokens should this scanner inject/In sparse accum mode, when to pop the data blocks"))

        self._spacc_mode = self.input("spacc_mode", 1)
        self._spacc_mode.add_attribute(ConfigRegAttr("Sparse accum mode"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # OUTPUT STREAMS
        self._coord_out = self.output("coord_out", self.data_width + 1, packed=True)
        self._coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._coord_out_valid_out = self.output("coord_out_valid", 1)
        self._coord_out_valid_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._coord_out_ready_in = self.input("coord_out_ready", 1)
        self._coord_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._pos_out = self.output("pos_out", self.data_width + 1, packed=True)
        self._pos_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._pos_out_valid_out = self.output("pos_out_valid", 1)
        self._pos_out_valid_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._pos_out_ready_in = self.input("pos_out_ready", 1)
        self._pos_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._block_rd_out = self.output("block_rd_out", self.data_width + 1, packed=True)
        self._block_rd_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._block_rd_out_valid_out = self.output("block_rd_out_valid", 1)
        self._block_rd_out_valid_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._block_rd_out_ready_in = self.input("block_rd_out_ready", 1)
        self._block_rd_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))
        # Eos for both streams...
        # self._eos_out = self.output("eos_out", 2)
        # self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Valid out for both streams
        # self._valid_out = self.output("valid_out", 2)
        # self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # Ready in for pos and coord
        # self._ready_in = self.input("ready_in", 2)
        # self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        ################################################################################
        # TO BUFFET
        ################################################################################
        # Addr out r/v
        self._addr_out = self.output("addr_out", self.data_width + 1, explicit_array=True, packed=True)
        self._addr_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._addr_out_ready_in = self.input("addr_out_ready", 1)
        self._addr_out_ready_in.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self._addr_out_valid_out = self.output("addr_out_valid", 1)
        self._addr_out_valid_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        # OP out r/v
        self._op_out = self.output("op_out", self.data_width + 1, explicit_array=True, packed=True)
        self._op_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._op_out_ready_in = self.input("op_out_ready", 1)
        self._op_out_ready_in.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self._op_out_valid_out = self.output("op_out_valid", 1)
        self._op_out_valid_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        # ID out r/v
        self._ID_out = self.output("ID_out", self.data_width + 1, explicit_array=True, packed=True)
        self._ID_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._ID_out_ready_in = self.input("ID_out_ready", 1)
        self._ID_out_ready_in.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self._ID_out_valid_out = self.output("ID_out_valid", 1)
        self._ID_out_valid_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        # Response channel from buffet
        self._rd_rsp_data_in = self.input("rd_rsp_data_in", self.data_width + 1, explicit_array=True, packed=True)
        self._rd_rsp_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._rd_rsp_valid_in = self.input("rd_rsp_data_in_valid", 1)
        self._rd_rsp_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._rd_rsp_ready_out = self.output("rd_rsp_data_in_ready", 1)
        self._rd_rsp_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # Intermediate for typing...
        # self._ren = self.var("ren", 1)

        # Point to the row in storage for data recovery
        self._payload_ptr = self.var("payload_ptr", self.data_width)
        # self._payload_ptr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self.wire(self._addr_out[0][self.data_width + 1 - 1], kts.const(0, 1))
        # self.wire(self._op_out[0][self.data_width + 1 - 1], kts.const(0, 1))
        # self.wire(self._ID_out[0][self.data_width + 1 - 1], kts.const(0, 1))

# ==========================================
# Generate addresses to scan over fiber...
# ==========================================

        self._inc_fiber_addr = self.var("inc_fiber_addr", 1)
        self._clr_fiber_addr = self.var("clr_fiber_addr", 1)
        self._fiber_addr_pre = add_counter(self, "fiber_addr_pre", 16, self._inc_fiber_addr, clear=self._clr_fiber_addr)

        self._inc_rep = self.var("inc_rep", 1)
        self._clr_rep = self.var("clr_rep", 1)
        self._num_reps = add_counter(self, "num_reps", 16, self._inc_rep, self._clr_rep)
        # self._step_agen = self.var("step_agen", 1)

# =============================
# Input FIFO
#
# To leverage hierarchical intersection, we need to be
# able to accept an incoming stream from the output of a previous
# level. Root-level scanners drive the processing, whereas non-root-level
# scanners are driven by upper levels
# =============================

        # Give this thing a min depth of 2 so we can still run the injection routine
        self._infifo = RegFIFO(data_width=1 * self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True, min_depth=2)
        self._infifo.add_attribute(SharedFifoAttr(direction="IN"))

        # Need to know if we've seen eos_in
        # self._eos_in_seen = self.var("eos_in_seen", 1)
        # Gate ready after last read in the stream
        # self._ready_gate = self.var("ready_gate", 1)

        # For input streams, need coord_in, valid_in, eos_in
        self._upstream_pos_in = self.input("us_pos_in", self.data_width + 1, packed=True)
        self._upstream_pos_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._upstream_coord_in = self.input("us_coord_in", self.data_width)
        # self._upstream_coord_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._upstream_valid_in = self.input("us_pos_in_valid", 1)
        self._upstream_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        # self._upstream_eos_in = self.input("us_eos_in", 1)
        # self._upstream_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._upstream_ready_out = self.output("us_pos_in_ready", 1)
        self._upstream_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # For input streams, need coord_in, valid_in, eos_in
        self._infifo_pos_in = self.var("infifo_pos_in", self.data_width, packed=True)
        # self._infifo_coord_in = self.var("infifo_coord_in", self.data_width)
        self._infifo_valid_in = self.var("infifo_valid_in", 1)
        self._infifo_eos_in = self.var("infifo_eos_in", 1)

        # Stupid convert
        self._pos_in_us_packed = self.var("fifo_us_in_packed", 1 * self.data_width + 1, packed=True)

        # Need a path to inject into the input fifo for root mode
        self._us_fifo_inject_push = self.var("us_fifo_inject_push", 1)
        self._us_fifo_inject_data = self.var("us_fifo_inject_data", 16)
        self._us_fifo_inject_eos = self.var("us_fifo_inject_eos", 1)

        self._us_fifo_push = self.var("us_fifo_push", 1)
        self.wire(self._us_fifo_push, kts.ternary(self._root, self._us_fifo_inject_push, self._upstream_valid_in))

        # indicate valid data as well
        # self.wire(self._pos_in_us_packed[2 * self.data_width + 2 - 1, self.data_width + 2], self._upstream_coord_in)
        # self.wire(self._pos_in_us_packed[self.data_width + 1], self._upstream_valid_in)
        # The EOS tags on the last valid in the stream
        self.wire(self._pos_in_us_packed[self.data_width], kts.ternary(self._root, self._us_fifo_inject_eos, self._upstream_pos_in[self.data_width]))
        self.wire(self._pos_in_us_packed[self.data_width - 1, 0], kts.ternary(self._root, self._us_fifo_inject_data, self._upstream_pos_in[self.data_width - 1, 0]))

        self._data_out_us_packed = self.var("fifo_out_us_packed", 1 * self.data_width + 1, packed=True)
        # self.wire(self._infifo_coord_in, self._data_out_us_packed[2 * self.data_width + 2 - 1, self.data_width + 2])
        # self.wire(self._infifo_valid_in, self._data_out_us_packed[self.data_width + 1])
        self.wire(self._infifo_eos_in, self._data_out_us_packed[self.data_width])
        self.wire(self._infifo_pos_in, self._data_out_us_packed[self.data_width - 1, 0])

        # self._input_fifo_pop = self.var("input_fifo_pop", 1)
        # self._fifo_us_valid_entry = self.var("fifo_us_valid_entry", 1)
        self._fifo_us_full = self.var("fifo_us_full", 1)
        self._pop_infifo = self.var("pop_infifo", 1)
        self._seg_pop_infifo = self.var("seg_pop_infifo", 1)
        self._crd_pop_infifo = self.var("crd_pop_infifo", 1)

        self.wire(self._pop_infifo, self._seg_pop_infifo | self._crd_pop_infifo)

        self._clr_pop_infifo_sticky = self.var("clr_pop_infifo_sticky", 1)
        self._pop_infifo_sticky = sticky_flag(self, self._pop_infifo, self._clr_pop_infifo_sticky, 'pop_infifo_sticky', seq_only=True)

        self.add_child(f"input_fifo",
                       self._infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._us_fifo_push,
                       pop=self._pop_infifo,
                       data_in=self._pos_in_us_packed,
                       data_out=self._data_out_us_packed)

        self.wire(self._fifo_us_full, self._infifo.ports.full)
        self.wire(self._upstream_ready_out, ~self._fifo_us_full)

        self.wire(self._infifo_valid_in, ~self._infifo.ports.empty)

        # @always_ff((posedge, "clk"), (negedge, "rst_n"))
        # def eos_seen_ff():
        #     if ~self._rst_n:
        #         self._eos_in_seen = 0
        #     elif self._infifo_eos_in:
        #         self._eos_in_seen = 1
        # self.add_code(eos_seen_ff)

        # Read Response FIFO
        self._rd_rsp_fifo_pop = self.var("rd_rsp_fifo_pop", 1)
        self._seg_rd_rsp_fifo_pop = self.var("seg_rd_rsp_fifo_pop", 1)
        self._crd_rd_rsp_fifo_pop = self.var("crd_rd_rsp_fifo_pop", 1)

        # Every response has preallocated room - should be able to wire to 1
        # self.wire(self._rd_rsp_fifo_pop, self._seg_rd_rsp_fifo_pop | self._crd_rd_rsp_fifo_pop)
        self.wire(self._rd_rsp_fifo_pop, kts.const(1, 1))

        self._rd_rsp_fifo_valid = self.var("rd_rsp_fifo_valid", 1)

        # self._rd_rsp_fifo_in = kts.concat(self._rd_rsp_data_in[0][self.data_width - 1, 0])
        # Use all 17b - Last bit tells us destination prealloc fifo
        self._rd_rsp_fifo_in = kts.concat(self._rd_rsp_data_in[0])
        self._rd_rsp_infifo = RegFIFO(data_width=self._rd_rsp_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._rd_rsp_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._rd_rsp_fifo_out_data = self.var("rd_rsp_fifo_out_data", self.data_width + 1, packed=True)

        self.add_child(f"rd_rsp_fifo",
                       self._rd_rsp_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_rsp_valid_in,
                       pop=self._rd_rsp_fifo_pop,
                       data_in=self._rd_rsp_fifo_in,
                       data_out=kts.concat(self._rd_rsp_fifo_out_data))

        self.wire(self._rd_rsp_ready_out, ~self._rd_rsp_infifo.ports.full)
        self.wire(self._rd_rsp_fifo_valid, ~self._rd_rsp_infifo.ports.empty)

# =============================
# Output FIFO to BUFFET
# =============================

        # These should all get muxed based on a port arbiter from the seg + crd FSM

        self._seg_req_push = self.var("seg_req_push", 1)
        self._crd_req_push = self.var("crd_req_push", 1)

        self._seg_grant_push = self.var("seg_grant_push", 1)
        self._crd_grant_push = self.var("crd_grant_push", 1)

        # algo = 'RR'
        algo = 'PRIO'
        self.port_arbiter = Arbiter(ins=2,
                                    algo=algo)

        brr = self.var("base_rr", 2)
        # self.wire(brr[0], self._seg_req_push)
        # self.wire(brr[1], self._crd_req_push)
        self.wire(brr, kts.concat(self._crd_req_push, self._seg_req_push))

        self._no_outfifo_full = self.var("no_outfifo_full", 1)

        self.add_child(f"rr_arbiter",
                       self.port_arbiter,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       request_in=brr,
                       grant_out=kts.concat(self._crd_grant_push, self._seg_grant_push),
                       resource_ready=self._no_outfifo_full)

        self._seg_addr_out_to_fifo = self.var("seg_addr_out_to_fifo", self.data_width, explicit_array=True, packed=True)
        self._seg_op_out_to_fifo = self.var("seg_op_out_to_fifo", self.data_width, explicit_array=True, packed=True)
        self._seg_ID_out_to_fifo = self.var("seg_ID_out_to_fifo", self.data_width, explicit_array=True, packed=True)

        self._crd_addr_out_to_fifo = self.var("crd_addr_out_to_fifo", self.data_width, explicit_array=True, packed=True)
        self._crd_op_out_to_fifo = self.var("crd_op_out_to_fifo", self.data_width, explicit_array=True, packed=True)
        self._crd_ID_out_to_fifo = self.var("crd_ID_out_to_fifo", self.data_width, explicit_array=True, packed=True)

        self._addr_out_to_fifo = self.var("addr_out_to_fifo", self.data_width, explicit_array=True, packed=True)
        self._op_out_to_fifo = self.var("op_out_to_fifo", self.data_width, explicit_array=True, packed=True)
        self._ID_out_to_fifo = self.var("ID_out_to_fifo", self.data_width, explicit_array=True, packed=True)

        self.wire(self._addr_out_to_fifo, kts.ternary(self._crd_grant_push, self._crd_addr_out_to_fifo, self._seg_addr_out_to_fifo))
        self.wire(self._op_out_to_fifo, kts.ternary(self._crd_grant_push, self._crd_op_out_to_fifo, self._seg_op_out_to_fifo))
        self.wire(self._ID_out_to_fifo, kts.ternary(self._crd_grant_push, self._crd_ID_out_to_fifo, self._seg_ID_out_to_fifo))

        # ADDR Outfifo
        self._addr_out_fifo_push = self.var("addr_out_fifo_push", 1)
        # self._seg_addr_out_fifo_push = self.var("seg_addr_out_fifo_push", 1)
        # self._crd_addr_out_fifo_push = self.var("crd_addr_out_fifo_push", 1)
        self.wire(self._addr_out_fifo_push, self._seg_grant_push | self._crd_grant_push)
        self._addr_out_fifo_full = self.var("addr_out_fifo_full", 1)
        self._addr_out_fifo_in = kts.concat(kts.const(0, 1), self._addr_out_to_fifo)
        self._addr_out_fifo = RegFIFO(data_width=self._addr_out_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._addr_out_fifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self.add_child(f"addr_out_fifo",
                       self._addr_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._addr_out_fifo_push,
                       pop=self._addr_out_ready_in,
                       data_in=self._addr_out_fifo_in,
                       data_out=self._addr_out)

        self.wire(self._addr_out_fifo_full, self._addr_out_fifo.ports.full)
        self.wire(self._addr_out_valid_out, ~self._addr_out_fifo.ports.empty)

        # OP Outfifo
        self._op_out_fifo_push = self.var("op_out_fifo_push", 1)
        self.wire(self._op_out_fifo_push, self._seg_grant_push | self._crd_grant_push)
        self._op_out_fifo_full = self.var("op_out_fifo_full", 1)
        self._op_out_fifo_in = kts.concat(kts.const(0, 1), self._op_out_to_fifo)
        self._op_out_fifo = RegFIFO(data_width=self._op_out_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._op_out_fifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self.add_child(f"op_out_fifo",
                       self._op_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._op_out_fifo_push,
                       pop=self._op_out_ready_in,
                       data_in=self._op_out_fifo_in,
                       data_out=self._op_out)

        self.wire(self._op_out_fifo_full, self._op_out_fifo.ports.full)
        self.wire(self._op_out_valid_out, ~self._op_out_fifo.ports.empty)

        # ID Outfifo
        self._ID_out_fifo_push = self.var("ID_out_fifo_push", 1)
        # self._seg_ID_out_fifo_push = self.var("seg_ID_out_fifo_push", 1)
        # self._crd_ID_out_fifo_push = self.var("crd_ID_out_fifo_push", 1)
        self.wire(self._ID_out_fifo_push, self._seg_grant_push | self._crd_grant_push)
        self._ID_out_fifo_full = self.var("ID_out_fifo_full", 1)
        self._ID_out_fifo_in = kts.concat(kts.const(0, 1), self._ID_out_to_fifo)
        self._ID_out_fifo = RegFIFO(data_width=self._ID_out_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._ID_out_fifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self.add_child(f"ID_out_fifo",
                       self._ID_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._ID_out_fifo_push,
                       pop=self._ID_out_ready_in,
                       data_in=self._ID_out_fifo_in,
                       data_out=self._ID_out)

        self.wire(self._ID_out_fifo_full, self._ID_out_fifo.ports.full)
        self.wire(self._ID_out_valid_out, ~self._ID_out_fifo.ports.empty)

        # Calculate ready for the outgoing channel
        self.wire(self._no_outfifo_full, ~(self._ID_out_fifo_full | self._op_out_fifo_full | self._addr_out_fifo_full))

# =============================
# JOIN Logic
# =============================
        # out_pushes = []
        # out_fulls = []

        # # Broadcast the single push to the buffet out push
        # out_pushes.append(self._addr_out_fifo_push)
        # out_pushes.append(self._op_out_fifo_push)
        # out_pushes.append(self._ID_out_fifo_push)
        # self._buffet_push = self.var("buffet_push", 1)
        # self.wire(kts.concat(*[*out_pushes]), kts.concat(*[self._buffet_push for i in range(len(out_pushes))]))

        # self._buffet_joined = self.var("buffet_joined", 1)

        # # Join the fulls to a joined ready
        # out_fulls.append(self._addr_out_fifo_full)
        # out_fulls.append(self._op_out_fifo_full)
        # out_fulls.append(self._ID_out_fifo_full)

        # self.wire(self._buffet_joined, (~kts.concat(*out_fulls)).r_and())

        self._set_pushed_done_seg = self.var("set_pushed_done_seg", 1)
        self._clr_pushed_done_seg = self.var("clr_pushed_done_seg", 1)

        self._set_pushed_done_crd = self.var("set_pushed_done_crd", 1)
        self._clr_pushed_done_crd = self.var("clr_pushed_done_crd", 1)
        # self._pushed_done = self.var("pushed_done", 1)
        self._pushed_done = sticky_flag(self, self._set_pushed_done_seg | self._set_pushed_done_crd,
                                        clear=self._clr_pushed_done_seg | self._clr_pushed_done_crd,
                                        name='pushed_done_sticky', seq_only=True)

        self._set_readout_loop_seg = self.var("set_readout_loop_seg", 1)
        self._clr_readout_loop_seg = self.var("clr_readout_loop_seg", 1)

        self._set_readout_loop_crd = self.var("set_readout_loop_crd", 1)
        self._clr_readout_loop_crd = self.var("clr_readout_loop_crd", 1)

        self._readout_loop = sticky_flag(self, self._set_readout_loop_seg | self._set_readout_loop_crd,
                                         clear=self._clr_readout_loop_seg | self._clr_readout_loop_crd, name="readout_loop_sticky",
                                         seq_only=True)

# =================================
# Midpoint Reservation FIFOs
# =================================

        self._seg_res_fifo = ReservationFIFO(depth=8, data_width=self.data_width + 1, num_per=2)

        # self._seg_res_fifo_data_out = self.var("seg_res_fifo_data_out", (self.data_width + 1) * 2, packed=True)
        self._seg_res_fifo_data_out = [self.var(f"seg_res_fifo_data_out_{i_}", (self.data_width + 1), packed=True) for i_ in range(2)]
        self._seg_res_fifo_push_alloc = self.var("seg_res_fifo_push_alloc", 1)
        self._seg_res_fifo_push_reserve = self.var("seg_res_fifo_push_reserve", 1)
        self._seg_res_fifo_push_fill = self.var("seg_res_fifo_push_fill", 1)
        self._seg_res_fifo_pop = self.var("seg_res_fifo_pop", 1)
        self._seg_res_fifo_valid = self.var("seg_res_fifo_valid", 1)
        self._seg_res_fifo_full = self.var("seg_res_fifo_full", 1)

        self._seg_res_fifo_fill_data_in = self.var("seg_res_fifo_fill_data_in", self.data_width + 1)

        # Technically if you reserved something it will always be ready to receive
        # data for the reserved spot by design
        self.add_child("seg_res_fifo",
                       self._seg_res_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       data_in_0=self._rd_rsp_fifo_out_data,
                       data_in_1=self._rd_rsp_fifo_out_data,
                       fill_data_in=self._seg_res_fifo_fill_data_in,
                       data_out_0=self._seg_res_fifo_data_out[0],
                       data_out_1=self._seg_res_fifo_data_out[1],
                       push_alloc=self._seg_res_fifo_push_alloc & ~self._lookup_mode,
                       push_reserve=self._rd_rsp_fifo_valid & ((self._rd_rsp_fifo_out_data[self.data_width] == kts.const(0, 1)) & ~self._block_mode & ~self._lookup_mode),
                       push_fill=self._seg_res_fifo_push_fill & ~self._lookup_mode,
                       pop=self._seg_res_fifo_pop & ~self._lookup_mode,
                       valid=self._seg_res_fifo_valid,
                       full=self._seg_res_fifo_full)

        # self._crd_res_fifo = ReservationFIFO(depth=8, data_width=self.data_width + 1 + 1, num_per=1)
        self._crd_res_fifo = ReservationFIFO(depth=8, data_width=self.data_width + 1, num_per=1)

        self._crd_res_fifo_data_out = self.var("crd_res_fifo_data_out", (self.data_width + 1) * 1, packed=True)
        self._crd_res_fifo_push_alloc = self.var("crd_res_fifo_push_alloc", 1)
        self._crd_res_fifo_push_reserve = self.var("crd_res_fifo_push_reserve", 1)
        self._crd_res_fifo_push_fill = self.var("crd_res_fifo_push_fill", 1)
        self._crd_res_fifo_pop = self.var("crd_res_fifo_pop", 1)
        self._crd_res_fifo_valid = self.var("crd_res_fifo_valid", 1)
        self._crd_res_fifo_full = self.var("crd_res_fifo_full", 1)

        self._pos_out_to_fifo = self.var("pos_out_to_fifo", self.data_width + 1)
        self._crd_out_to_fifo = self.var("crd_out_to_fifo", self.data_width + 1)
        # self._block_rd_out_to_fifo = self.var("block_rd_out_to_fifo", self.data_width + 1)

        self._readout_dst_seg = self.var("readout_dst_seg", 1)
        self._readout_dst_crd = self.var("readout_dst_crd", 1)
        self._readout_dst_out = self.var("readout_dst_out", 1)
        # Technically if you reserved something it will always be ready to receive
        # data for the reserved spot by design
        self.add_child("crd_res_fifo",
                       self._crd_res_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       data_in_0=kts.concat(kts.const(0, 1), self._rd_rsp_fifo_out_data[self.data_width - 1, 0]),
                    #    data_in_0=kts.concat(kts.ternary(self._lookup_mode, self._readout_dst_seg, self._readout_dst_crd), kts.const(0, 1), self._rd_rsp_fifo_out_data[self.data_width - 1, 0]),
                    #    fill_data_in=kts.ternary(self._lookup_mode, self._seg_res_fifo_fill_data_in, self._seg_res_fifo_data_out[0]),
                       fill_data_in=kts.ternary(self._lookup_mode,
                    #    fill_data_in=kts.concat(kts.ternary(self._lookup_mode, self._readout_dst_seg, self._readout_dst_crd), kts.ternary(self._lookup_mode,
                                                self._seg_res_fifo_fill_data_in,
                                                kts.ternary(self._dense,
                                                            self._crd_out_to_fifo,
                                                            self._seg_res_fifo_data_out[0])),
                    #    data_out_0=kts.concat(self._readout_dst_out, self._crd_res_fifo_data_out),
                       data_out_0=self._crd_res_fifo_data_out,
                       push_alloc=kts.ternary(self._lookup_mode, self._seg_res_fifo_push_alloc, self._crd_res_fifo_push_alloc),
                       push_reserve=self._rd_rsp_fifo_valid & ((self._rd_rsp_fifo_out_data[self.data_width] == kts.const(1, 1)) | self._block_mode | self._lookup_mode),
                    #    push_reserve=self._rd_rsp_fifo_valid & ((self._rd_rsp_fifo_out_data[self.data_width] == kts.const(1, 1)) | self._block_mode | self._lookup_mode | (self._spacc_mode & self._readout_loop)),
                       push_fill=kts.ternary(self._lookup_mode, self._seg_res_fifo_push_fill, self._crd_res_fifo_push_fill),
                       pop=self._crd_res_fifo_pop,
                       valid=self._crd_res_fifo_valid,
                       full=self._crd_res_fifo_full)

        self._seg_in_done_state = self.var("seg_in_done_state", 1)
        self._seg_in_start_state = self.var("seg_in_start_state", 1)

        # Capture the last pushed stop token as the fill data from the seg side of the scanner FSM
        # only capture it when a stop token is being pushed
        self._last_stop_token = register(self, kts.ternary(self._seg_in_start_state, self._infifo.ports.data_out + 1, self._seg_res_fifo_fill_data_in),
                                         enable=kts.ternary(self._seg_in_start_state,
                                                            # self._seg_pop_infifo,
                                                            kts.const(0, 1),
                                                            (self._seg_res_fifo_push_fill & self._seg_res_fifo_push_alloc) & kts.ternary(self._lookup_mode,
                                                                                                                                        #  ~self._seg_res_fifo_full,
                                                                                                                                        #  ~self._crd_res_fifo_full) &
                                                                                                                                         ~self._crd_res_fifo_full,
                                                                                                                                         ~self._seg_res_fifo_full) &
                                                                self._seg_res_fifo_fill_data_in[self.EOS_BIT] &
                                                                (self._seg_res_fifo_fill_data_in[self.OPCODE_BT] == self.STOP_CODE)),
                                                                name="last_stop_token")

        # Marker to determine if the last stop token pushed was the true done
        self._last_stop_done = self.var("last_stop_done", 1)
        self.wire(self._last_stop_done, self._last_stop_token[self.data_width - 1, 0] == (self._stop_lvl + 2))

        self._clr_used_data = self.var("clr_used_data", 1)
        self._used_data = sticky_flag(self, self._infifo_valid_in & ~self._infifo_eos_in, clear=self._clr_used_data, name="use_data_sticky", seq_only=True)

        self.wire(self._clr_used_data, self._readout_loop & self._spacc_mode)

# =============================
# SCAN FSM
# =============================

        # Contains the address as the position for the ptr array
        self._pos_addr = self.var("pos_addr", self.data_width, packed=True)

        self._valid_inc = self.var("valid_inc", 1)
        self._valid_rst = self.var("valid_rst", 1)
        self._valid_cnt = self.var("valid_cnt", 16)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def valid_cnt():
            if ~self._rst_n:
                self._valid_cnt = 0
            elif self._valid_rst:
                self._valid_cnt = 0
            elif self._valid_inc:
                self._valid_cnt = self._valid_cnt + 1
        self.add_code(valid_cnt)

        self._inner_dim_offset = self.input("inner_dim_offset", 16)
        self._inner_dim_offset.add_attribute(ConfigRegAttr("Memory address of the offset..."))
        # self._coord_in = self.var("coord_in", 8)
        self._ptr_in = self.var("ptr_in", self.data_width, packed=True)
        # self.wire(self._ptr_in, self._rd_rsp_fifo_out_data)
        self.wire(self._ptr_in, self._rd_rsp_fifo_out_data[self.data_width - 1, 0])
        # self.wire(self._ptr_in, self._data_in[15, 8])

        # Need to hook up the current and next seqence_length
        # Also need to intercept the configs and set them internally...
        self._ptr_reg_en = self.var("ptr_reg_en", 1)
        self._ptr_reg = register(self, self._ptr_in, enable=self._ptr_reg_en)
        self._agen_addr_d1 = register(self, self._fiber_addr_pre)

        # In this way, we can save the relative position of each value in the ptr array for downstream
        # self.wire(self._pos_out_to_fifo, self._agen_addr_d1)
        # self.wire(self._pos_out_to_fifo, (self._agen_addr_d1 - self._inner_dim_offset))

        self._update_seq_state = self.var("update_seq_state", 1)
        self._seq_length = self.var("seq_length", 16)
        self._seq_addr = self.var("seq_addr", 16)
        self._next_seq_length = self.var("next_seq_length", 16)
        self._next_seq_addr = self.var("next_seq_addr", 16)

        self._seq_length_ptr_math = self.var("seq_length_ptr_math", self.data_width)
        # self.wire(self._seq_length_ptr_math[7, 0], self._ptr_in - self._ptr_reg - 1)
        # self.wire(self._seq_length_ptr_math[15, 8], 0)
        # self.wire(self._seq_length_ptr_math, self._ptr_in - self._ptr_reg - 1)
        # self.wire(self._seq_length_ptr_math, self._ptr_in - self._ptr_reg)
        self.wire(self._seq_length_ptr_math, (self._seg_res_fifo_data_out[1] - self._seg_res_fifo_data_out[0])[self.data_width - 1, 0])

        self.wire(self._pos_addr, kts.ternary(self._root, kts.const(0, self._pos_addr.width), self._infifo_pos_in))

        # On the first read, we locate the base offset addr, then on the
        # second we get the length as the subtraction of the pointers
        # self.wire(self._next_seq_length, self._ptr_in - self._ptr_reg - 1)
        # The memory address is the pointer offset + base register
        self.wire(self._next_seq_addr, self._ptr_reg + self._inner_dim_offset)

        # Hold state for iterator - just length
        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def update_seq_state_ff():
            if ~self._rst_n:
                self._seq_length = 0
                self._seq_addr = 0
                self._payload_ptr = 0
            elif self._update_seq_state:
                self._seq_length = self._next_seq_length
                self._seq_addr = self._next_seq_addr
                # Output this for use in the intersection engine
                self._payload_ptr = self._ptr_reg
        self.add_code(update_seq_state_ff)

        # Set up fiber addr
        self._fiber_addr = self.var("fiber_addr", self.data_width, packed=True)
        self.wire(self._fiber_addr, self._fiber_addr_pre + self._seq_addr)

        self._coord_fifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._coord_fifo.add_attribute(SharedFifoAttr(direction="OUT"))
        self._pos_fifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._pos_fifo.add_attribute(SharedFifoAttr(direction="OUT"))
        self._block_rd_fifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._block_rd_fifo.add_attribute(SharedFifoAttr(direction="OUT"))

        # self._fifo_push = self.var("fifo_push", 1)
        self._coord_out_fifo_push = self.var("coord_out_fifo_push", 1)
        self._pos_out_fifo_push = self.var("pos_out_fifo_push", 1)
        # self._block_rd_out_fifo_push = self.var("block_rd_out_fifo_push", 1)

        self._tag_eos = self.var("tag_eos", 1)
        self._last_valid_accepting = self.var("last_valid_accepting", 1)

        # Join the individual fifo fulls
        self._fifo_full_pre = self.var("fifo_full_pre", 2)
        self._fifo_full = self.var("fifo_full", 1)
        self.wire(self._fifo_full, self._fifo_full_pre.r_or())

        self._data_to_fifo = self.var("data_to_fifo", self.data_width)
        # Gate ready after last read in the stream
        # self._ready_gate = self.var("ready_gate", 1)

        # Define logic for iter_finish + rep_finish
        self._iter_finish = sticky_flag(self, self._last_valid_accepting, clear=self._clr_fiber_addr, name="iter_finish")
        self._rep_finish = sticky_flag(self, (self._num_reps == (self._repeat_factor - 1)) & self._inc_rep, clear=self._clr_rep, name="rep_finish")
        self._clr_seen_root_eos = self.var("clr_seen_root_eos", 1)
        self._seen_root_eos = sticky_flag(self, self._infifo_eos_in & (self._infifo_pos_in == 0), name="seen_root_eos", clear=self._clr_seen_root_eos)

        self._en_reg_data_in = self.var("en_reg_data_in", 1)
        self._data_in_d1 = register(self, self._rd_rsp_fifo_out_data, enable=self._en_reg_data_in)
        self._agen_addr_d1_cap = register(self, self._agen_addr_d1, enable=self._en_reg_data_in)

        self._done_in = self.var("done_in", 1)
        self.wire(self._done_in, self._infifo_eos_in & self._infifo_valid_in & (self._infifo_pos_in[9, 8] == kts.const(1, 2)))
        self._eos_in = self.var("eos_in", 1)
        self.wire(self._eos_in, self._infifo_eos_in & self._infifo_valid_in & (self._infifo_pos_in[9, 8] == kts.const(0, 2)))
        self._maybe_in = self.var("maybe_in", 1)
        self.wire(self._maybe_in, self._infifo_eos_in & self._infifo_valid_in & (self._infifo_pos_in[9, 8] == kts.const(2, 2)))

        self._seg_stop_lvl_geq = self.var("seg_stop_lvl_geq", 1)
        self.wire(self._seg_stop_lvl_geq, self._seg_res_fifo_fill_data_in[16] & (self._seg_res_fifo_fill_data_in[self.OPCODE_BT] == self.STOP_CODE) &
                                      (self._seg_res_fifo_fill_data_in[self.STOP_BT] >= self._stop_lvl[self.STOP_BT]))

        # This indicates we should go into the readout loop
        self._seg_stop_lvl_geq_p1 = self.var("seg_stop_lvl_geq_p1", 1)
        self.wire(self._seg_stop_lvl_geq_p1, self._seg_res_fifo_fill_data_in[16] & (self._seg_res_fifo_fill_data_in[self.OPCODE_BT] == self.STOP_CODE) &
                                      (self._seg_res_fifo_fill_data_in[self.STOP_BT] >= (self._stop_lvl[self.STOP_BT] + 1)))

        self._seg_stop_lvl_geq_p1_sticky = sticky_flag(self, self._seg_stop_lvl_geq_p1 & self._seg_res_fifo_push_alloc & self._seg_res_fifo_push_fill,
                                                       clear=self._clr_readout_loop_seg, name="go_to_readout_sticky")

        ####### Logic for block reads
        self._inc_req_made_seg = self.var("inc_req_made_seg", 1)
        self._clr_req_made_seg = self.var("clr_req_made_seg", 1)
        self._num_req_made_seg = add_counter(self, name="num_req_made_seg", bitwidth=16, increment=self._inc_req_made_seg, clear=self._clr_req_made_seg)
        self._inc_req_rec_seg = self.var("inc_req_rec_seg", 1)
        self._clr_req_rec_seg = self.var("clr_req_rec_seg", 1)
        self._num_req_rec_seg = add_counter(self, name="num_req_rec_seg", bitwidth=16, increment=self._inc_req_rec_seg, clear=self._clr_req_rec_seg)

        self._inc_req_made_crd = self.var("inc_req_made_crd", 1)
        self._clr_req_made_crd = self.var("clr_req_made_crd", 1)
        self._num_req_made_crd = add_counter(self, name="num_req_made_crd", bitwidth=16, increment=self._inc_req_made_crd, clear=self._clr_req_made_crd)
        self._inc_req_rec_crd = self.var("inc_req_rec_crd", 1)
        self._clr_req_rec_crd = self.var("clr_req_rec_crd", 1)
        self._num_req_rec_crd = add_counter(self, name="num_req_rec_crd", bitwidth=16, increment=self._inc_req_rec_crd, clear=self._clr_req_rec_crd)

        # Create FSM
        self.scan_fsm_seg = self.add_fsm("scan_seq_seg", reset_high=False)
        START_SEG = self.scan_fsm_seg.add_state("START_SEG")
        INJECT_ROUTING = self.scan_fsm_seg.add_state("INJECT_ROUTING")
        INJECT_0 = self.scan_fsm_seg.add_state("INJECT_0")
        INJECT_DONE = self.scan_fsm_seg.add_state("INJECT_DONE")
        READ = self.scan_fsm_seg.add_state("READ")
        READ_ALT = self.scan_fsm_seg.add_state("READ_ALT")
        LOOKUP = self.scan_fsm_seg.add_state("LOOKUP")
        PASS_STOP = self.scan_fsm_seg.add_state("PASS_STOP_SEG")
        PASS_DONE = self.scan_fsm_seg.add_state("PASS_DONE_SEG")
        FREE_SEG = self.scan_fsm_seg.add_state("FREE_SEG")
        # Squashing logic
        # SEQ_DONE_SEG = self.scan_fsm_seg.add_state("SEQ_DONE_SEG")
        DONE_SEG = self.scan_fsm_seg.add_state("DONE_SEG")

        self.scan_fsm_seg.output(self._seg_addr_out_to_fifo)
        self.scan_fsm_seg.output(self._seg_op_out_to_fifo)
        self.scan_fsm_seg.output(self._seg_ID_out_to_fifo)
        self.scan_fsm_seg.output(self._seg_req_push)
        self.scan_fsm_seg.output(self._seg_rd_rsp_fifo_pop)
        self.scan_fsm_seg.output(self._seg_pop_infifo)
        self.scan_fsm_seg.output(self._inc_req_made_seg)
        self.scan_fsm_seg.output(self._clr_req_made_seg)
        self.scan_fsm_seg.output(self._inc_req_rec_seg)
        self.scan_fsm_seg.output(self._clr_req_rec_seg)
        self.scan_fsm_seg.output(self._us_fifo_inject_data, default=kts.const(0, self.data_width))
        self.scan_fsm_seg.output(self._us_fifo_inject_eos, default=kts.const(0, 1))
        self.scan_fsm_seg.output(self._us_fifo_inject_push, default=kts.const(0, 1))
        self.scan_fsm_seg.output(self._seg_res_fifo_push_alloc)
        # self.scan_fsm_seg.output(self._seg_res_fifo_push_reserve)
        self.scan_fsm_seg.output(self._seg_res_fifo_push_fill)
        self.scan_fsm_seg.output(self._seg_res_fifo_fill_data_in)
        self.scan_fsm_seg.output(self._set_pushed_done_seg, default=kts.const(0, 1))
        self.scan_fsm_seg.output(self._clr_pushed_done_seg, default=kts.const(0, 1))
        self.scan_fsm_seg.output(self._set_readout_loop_seg, default=kts.const(0, 1))
        self.scan_fsm_seg.output(self._clr_readout_loop_seg, default=kts.const(0, 1))
        self.scan_fsm_seg.output(self._seg_in_done_state, default=kts.const(0, 1))
        self.scan_fsm_seg.output(self._seg_in_start_state, default=kts.const(0, 1))

    # Dummy state for eventual filling block.
        # Sit in start if in block mode - everything else handled by CRD FSM
        START_SEG.next(START_SEG, self._block_mode)
        # START_SEG.next(READ, ~self._root & ~self._lookup_mode & ~self._block_mode & (~self._spacc_mode | ~self._pushed_done))
        # Can return to the read state for a readout_loop
        # START_SEG.next(READ, (~self._root | (self._spacc_mode & self._readout_loop)) & ~self._lookup_mode & ~self._block_mode)
        # START_SEG.next(READ, ~self._root & ~self._lookup_mode & ~self._block_mode & self._tile_en)
        # START_SEG.next(READ, ~self._root & ~self._lookup_mode & ~self._block_mode & self._infifo_valid_in & self._tile_en)
        # START_SEG.next(READ, ~self._root & ~self._lookup_mode & ~self._block_mode & self._infifo_valid_in & self._tile_en & ~self._spacc_mode)
        START_SEG.next(READ, ~self._root & ~self._lookup_mode & ~self._block_mode & ~self._spacc_mode & self._tile_en)
        START_SEG.next(INJECT_ROUTING, ~self._root & ~self._lookup_mode & ~self._block_mode & (self._infifo_valid_in | self._readout_loop) & self._spacc_mode & self._tile_en)
        START_SEG.next(INJECT_0, self._root & ~self._lookup_mode & ~self._block_mode & self._tile_en)
        # START_SEG.next(LOOKUP, ~self._root & self._lookup_mode & ~self._block_mode & (~self._spacc_mode | ~self._pushed_done))
        START_SEG.next(LOOKUP, ~self._root & self._lookup_mode & ~self._block_mode & self._tile_en)

        # Inject a single value into the fifo
        INJECT_0.next(INJECT_DONE, None)

        # Inject a single done into the fifo
        INJECT_DONE.next(READ, None)

        INJECT_ROUTING.next(READ, ~self._seg_res_fifo_full)

        # From READ - we have one option
        # 1. Issue read to first address in seg pair and reserve room in the reservation FIFO and
        # Pop this thing off so we can have the next data or the EOS token as soon as possible
        # READ.next(PASS_STOP, self._maybe_in | ~self._seg_res_fifo_full)
        # Additionally, if we are in dense mode, we will ping pong between these states of PASS_STOP and read
        # Can go to pass stop if we are in dense mode and we don't have a done in
        # READ.next(PASS_STOP, self._maybe_in | (self._dense & ~self._done_in & ~self._seg_res_fifo_full & self._infifo_valid_in))
        # READ.next(PASS_STOP, self._maybe_in | ((self._dense & ~self._done_in & ~self._seg_res_fifo_full & self._infifo_valid_in) | (~self._dense & ~self._spacc_mode & self._eos_in)))
        READ.next(PASS_STOP, self._maybe_in | ((self._dense & ~self._done_in & ~self._seg_res_fifo_full & self._infifo_valid_in) | ((~self._spacc_mode | ~self._readout_loop) & ~self._dense & self._eos_in & (~self._spacc_mode | ~self._used_data))))
        READ.next(READ_ALT, self._seg_grant_push & ~self._seg_res_fifo_full)
        # For now, just handle PASS_STOP here...
        # READ.next(PASS_STOP, self._infifo_valid_in & self._infifo_eos_in & ~self._done_in)
        READ.next(FREE_SEG, self._done_in & ~self._seg_res_fifo_full)
        READ.next(READ, None)

        # In READ_ALT - we are sending the second read request and popping at the end
        # Always need to emit a stop token after a pair of reads
        READ_ALT.next(PASS_STOP, self._seg_grant_push & ~self._seg_res_fifo_full)
        # READ_ALT.next(READ, self._seg_grant_push & ~self._seg_res_fifo_full)
        READ_ALT.next(READ_ALT, None)

        # In LOOKUP mode, we need to direct everything to the crd reservation fifo
        # LOOKUP.next(FREE_SEG, self._done_in & ~self._crd_res_fifo_full)
        # Go to free seg if we have a done token, or we are pushing an appropriate stop level
        # In spacc mode, we don't want to free the seg on done, we actually just want to wait for the readout loop to end
        # LOOKUP.next(FREE_SEG, (self._done_in | (self._spacc_mode & self._seg_stop_lvl_geq)) & ~self._crd_res_fifo_full)
        LOOKUP.next(FREE_SEG, ((self._done_in & ~self._spacc_mode) |
                                    (self._spacc_mode & self._seg_stop_lvl_geq & self._infifo_valid_in & ~self._pushed_done)) &
                               ~self._crd_res_fifo_full)
        LOOKUP.next(LOOKUP, None)

        # In this state, we are passing through stop tokens into
        # the downstream
        # Go to free if we see DONE since we are application done
        # We need to make sure we only transition if there was somewhere to put an item
        # PASS_STOP.next(FREE_SEG, self._done_in & kts.ternary(self._lookup_mode, ~self._coord_fifo.ports.full, ~self._seg_res_fifo_full))
        # PASS_STOP.next(LOOKUP, (~self._infifo_eos_in & self._infifo_valid_in) & ~self._done_in & self._lookup_mode)
        # Go to lookup if there's room in the final output fifo
        # PASS_STOP.next(LOOKUP, (~self._infifo_eos_in & self._infifo_valid_in) & ~self._done_in & self._lookup_mode & ~self._coord_fifo.ports.full)
        # PASS_STOP.next(READ, (~self._infifo_eos_in & self._infifo_valid_in) & ~self._done_in & ~self._lookup_mode & ~self._seg_res_fifo_full)
        # Go back to read if there's now valid data on the input and we shoved the new stop token somewhere
        # PASS_STOP.next(LOOKUP, self._infifo_valid_in & self._lookup_mode & ~self._coord_fifo.ports.full)
        # PASS_STOP.next(FREE_SEG, ((self._infifo_valid_in & ~self._lookup_mode & self._spacc_mode & self._seg_stop_lvl_geq) | (self._readout_loop & ~self._pushed_done)) & ~self._seg_res_fifo_full)
        # PASS_STOP.next(PASS_DONE, ((self._infifo_valid_in & ~self._lookup_mode & self._spacc_mode & self._seg_stop_lvl_geq) | (self._readout_loop & self._pushed_done)) & ~self._seg_res_fifo_full)
        PASS_STOP.next(PASS_DONE, ((self._infifo_valid_in & ~self._lookup_mode & self._spacc_mode & self._seg_stop_lvl_geq) |
                                        # (self._readout_loop & self._pushed_done)) & ~self._seg_res_fifo_full)
                                        (self._readout_loop)) & ~self._seg_res_fifo_full)
        # PASS_STOP.next(FREE_SEG, ((self._infifo_valid_in & ~self._lookup_mode & self._spacc_mode & self._seg_stop_lvl_geq) |
        #                                 # (self._readout_loop & self._pushed_done)) & ~self._seg_res_fifo_full)
        #                                 (self._readout_loop)) & ~self._seg_res_fifo_full)
        # PASS_STOP.next(READ, self._infifo_valid_in & ~self._lookup_mode & ~self._seg_res_fifo_full & ~self._spacc_mode)
        PASS_STOP.next(READ, self._infifo_valid_in & ~self._lookup_mode & ~self._seg_res_fifo_full)
        PASS_STOP.next(PASS_STOP, None)
        # PASS_STOP.next(FREE_SEG, self._done_in & ~self._fifo_full)
        # PASS_STOP.next(LOOKUP, (~self._infifo_eos_in & self._infifo_valid_in) & ~self._done_in & self._lookup_mode)
        # PASS_STOP.next(PASS_STOP, None)

        # Use pass done for the readout loop
        # PASS_DONE.next(FREE_SEG, ~self._seg_res_fifo_full)
        # PASS_DONE.next(FREE_SEG, (self._readout_loop & (~self._pushed_done | ~self._seg_res_fifo_full)) | (self._infifo_valid_in & (~self._done_in | ~self._seg_res_fifo_full)))
        # PASS_DONE.next(FREE_SEG, (self._readout_loop & (~self._pushed_done | ~self._seg_res_fifo_full)) | ((self._infifo_valid_in | ~self._last_stop_done) & (~self._done_in | ~self._seg_res_fifo_full)))
        PASS_DONE.next(FREE_SEG, (self._readout_loop & (~self._pushed_done | ~self._seg_res_fifo_full)) | (~self._last_stop_done | ~self._seg_res_fifo_full))
        # If we are in the readout loop and we have pushed done, we should push another done
        # if not in the readout loop, we should check the next input token and pass it if it is done
        # PASS_DONE.next(FREE_SEG, kts.ternary(self._readout_loop,
        #                                      ~self._seg_res_fifo_full,
        #                                      kts.ternary(self._done_in,
        #                                                  ~self._seg_res_fifo_full)))

        # Free the ID 0 data structure
        FREE_SEG.next(DONE_SEG, self._seg_grant_push)

        self._crd_in_done_state = self.var("crd_in_done_state", 1)
        # DONE_SEG.next(START_SEG, None)
        # In the case of non lookup mode, need to wait for the coordinate side to
        # finish before we can get the next data
        # DONE_SEG.next(START_SEG, (~self._dense & ~self._lookup_mode & self._crd_in_done_state) | self._lookup_mode)
        DONE_SEG.next(START_SEG, kts.ternary(self._lookup_mode,
                                             kts.const(1, 1),
                                            #  ~self._dense & self._crd_in_done_state))
                                             ~self._dense & self._crd_in_done_state))
        # DONE_SEG.next(DONE_SEG, None)

        #######
        # START_SEG
        #######
        START_SEG.output(self._seg_addr_out_to_fifo, 0)
        START_SEG.output(self._seg_op_out_to_fifo, 0)
        START_SEG.output(self._seg_ID_out_to_fifo, 0)
        START_SEG.output(self._seg_req_push, 0)
        START_SEG.output(self._seg_rd_rsp_fifo_pop, 0)
        # START_SEG.output(self._seg_pop_infifo, 0)
        # START_SEG.output(self._seg_pop_infifo, self._eos_in & self._spacc_mode & ~self._root & ~self._lookup_mode & ~self._block_mode & ~self._readout_loop)
        START_SEG.output(self._seg_pop_infifo, 0)
        START_SEG.output(self._inc_req_made_seg, 0)
        START_SEG.output(self._clr_req_made_seg, 0)
        START_SEG.output(self._inc_req_rec_seg, 0)
        START_SEG.output(self._clr_req_rec_seg, 0)
        START_SEG.output(self._us_fifo_inject_data, 0)
        START_SEG.output(self._us_fifo_inject_eos, 0)
        START_SEG.output(self._us_fifo_inject_push, 0)
        START_SEG.output(self._seg_res_fifo_push_alloc, 0)
        # START_SEG.output(self._seg_res_fifo_push_reserve, 0)
        START_SEG.output(self._seg_res_fifo_push_fill, 0)
        START_SEG.output(self._seg_res_fifo_fill_data_in, 0)
        # Special case where there is no accum and we go directly do readout mode...need to pop the incoming fifo
        # START_SEG.output(self._set_readout_loop_seg, self._eos_in & self._spacc_mode & ~self._root & ~self._lookup_mode & ~self._block_mode & ~self._readout_loop)
        START_SEG.output(self._set_readout_loop_seg, 0)
        START_SEG.output(self._seg_in_start_state, 1)

        #######
        # INJECT_0
        #######
        INJECT_0.output(self._seg_addr_out_to_fifo, 0)
        INJECT_0.output(self._seg_op_out_to_fifo, 0)
        INJECT_0.output(self._seg_ID_out_to_fifo, 0)
        INJECT_0.output(self._seg_req_push, 0)
        INJECT_0.output(self._seg_rd_rsp_fifo_pop, 0)
        INJECT_0.output(self._seg_pop_infifo, 0)
        INJECT_0.output(self._inc_req_made_seg, 0)
        INJECT_0.output(self._clr_req_made_seg, 0)
        INJECT_0.output(self._inc_req_rec_seg, 0)
        INJECT_0.output(self._clr_req_rec_seg, 0)
        INJECT_0.output(self._us_fifo_inject_data, 0)
        INJECT_0.output(self._us_fifo_inject_eos, 0)
        INJECT_0.output(self._us_fifo_inject_push, 1)
        INJECT_0.output(self._seg_res_fifo_push_alloc, 0)
        # INJECT_0.output(self._seg_res_fifo_push_reserve, 0)
        INJECT_0.output(self._seg_res_fifo_push_fill, 0)
        INJECT_0.output(self._seg_res_fifo_fill_data_in, 0)

        #######
        # INJECT_DONE
        #######
        INJECT_DONE.output(self._seg_addr_out_to_fifo, 0)
        INJECT_DONE.output(self._seg_op_out_to_fifo, 0)
        INJECT_DONE.output(self._seg_ID_out_to_fifo, 0)
        INJECT_DONE.output(self._seg_req_push, 0)
        INJECT_DONE.output(self._seg_rd_rsp_fifo_pop, 0)
        INJECT_DONE.output(self._seg_pop_infifo, 0)
        INJECT_DONE.output(self._inc_req_made_seg, 0)
        INJECT_DONE.output(self._clr_req_made_seg, 0)
        INJECT_DONE.output(self._inc_req_rec_seg, 0)
        INJECT_DONE.output(self._clr_req_rec_seg, 0)
        INJECT_DONE.output(self._us_fifo_inject_data, kts.const(2**8, 16))
        INJECT_DONE.output(self._us_fifo_inject_eos, 1)
        INJECT_DONE.output(self._us_fifo_inject_push, 1)
        INJECT_DONE.output(self._seg_res_fifo_push_alloc, 0)
        # INJECT_DONE.output(self._seg_res_fifo_push_reserve, 0)
        INJECT_DONE.output(self._seg_res_fifo_push_fill, 0)
        INJECT_DONE.output(self._seg_res_fifo_fill_data_in, 0)

        #######
        # INJECT_ROUTING
        #######
        INJECT_ROUTING.output(self._seg_addr_out_to_fifo, 0)
        INJECT_ROUTING.output(self._seg_op_out_to_fifo, 0)
        INJECT_ROUTING.output(self._seg_ID_out_to_fifo, 0)
        INJECT_ROUTING.output(self._seg_req_push, 0)
        INJECT_ROUTING.output(self._seg_rd_rsp_fifo_pop, 0)
        INJECT_ROUTING.output(self._seg_pop_infifo, 0)
        INJECT_ROUTING.output(self._inc_req_made_seg, 0)
        INJECT_ROUTING.output(self._clr_req_made_seg, 0)
        INJECT_ROUTING.output(self._inc_req_rec_seg, 0)
        INJECT_ROUTING.output(self._clr_req_rec_seg, 0)
        INJECT_ROUTING.output(self._us_fifo_inject_data, 0)
        INJECT_ROUTING.output(self._us_fifo_inject_eos, 0)
        INJECT_ROUTING.output(self._us_fifo_inject_push, 1)
        INJECT_ROUTING.output(self._seg_res_fifo_push_alloc, ~self._seg_res_fifo_full)
        INJECT_ROUTING.output(self._seg_res_fifo_push_fill, ~self._seg_res_fifo_full)
        # INJECT_0.output(self._seg_res_fifo_push_reserve, 0)
        INJECT_ROUTING.output(self._seg_res_fifo_fill_data_in, kts.concat(kts.const(1, 1), kts.const(0, 6), kts.const(3, 2), kts.const(0, 7), self._readout_loop))

        #######
        # READ
        #######
        READ.output(self._seg_addr_out_to_fifo, kts.ternary(self._readout_loop, 0, self._infifo_pos_in))
        READ.output(self._seg_op_out_to_fifo, 1)
        READ.output(self._seg_ID_out_to_fifo, 0)
        # Only request a push when there's valid, non-eos data on the fifo
        # READ.output(self._seg_req_push, self._infifo_valid_in & ~self._infifo_eos_in & ~self._seg_res_fifo_full & ~self._dense)
        READ.output(self._seg_req_push, ((self._infifo_valid_in & ~self._infifo_eos_in & ~self._dense) | self._readout_loop) & ~self._seg_res_fifo_full)
        READ.output(self._seg_rd_rsp_fifo_pop, 1)
        # Can pop the infifo if we have done or are in dense mode
        READ.output(self._seg_pop_infifo, (((self._done_in | (self._dense & self._infifo_valid_in)) & ~self._seg_res_fifo_full) | self._maybe_in) & ~self._readout_loop)
        READ.output(self._inc_req_made_seg, 0)
        READ.output(self._clr_req_made_seg, 0)
        READ.output(self._inc_req_rec_seg, 0)
        READ.output(self._clr_req_rec_seg, 0)
        READ.output(self._us_fifo_inject_data, 0)
        READ.output(self._us_fifo_inject_eos, 0)
        READ.output(self._us_fifo_inject_push, 0)
        # Only push through the done in READ, in conjunction with the fill pulse
        READ.output(self._seg_res_fifo_push_alloc, kts.ternary(self._done_in | self._dense,
                                                               ~self._seg_res_fifo_full & self._infifo_valid_in,
                                                               (~self._seg_res_fifo_full & self._seg_grant_push) & ~self._maybe_in))
        # Only fill if we have done_in
        READ.output(self._seg_res_fifo_push_fill, (self._done_in | (self._dense & self._infifo_valid_in)) & ~self._seg_res_fifo_full)
        READ.output(self._seg_res_fifo_fill_data_in, kts.concat(self._infifo_eos_in, self._infifo_pos_in))
        # READ.output(self._set_pushed_done_seg, self._done_in & ~self._seg_res_fifo_full & self._spacc_mode & self._infifo_valid_in)
        # READ.output(self._seg_rd_rsp_fifo_pop, self._rd_rsp_fifo_valid & (self._rd_rsp_fifo_out_data[self.data_width] == kts.const(0, 1)))

        #######
        # READ_ALT
        #######
        # READ_ALT.output(self._seg_addr_out_to_fifo, self._infifo_pos_in + 1)
        READ_ALT.output(self._seg_addr_out_to_fifo, kts.ternary(self._readout_loop, 1, self._infifo_pos_in + 1))
        READ_ALT.output(self._seg_op_out_to_fifo, 1)
        READ_ALT.output(self._seg_ID_out_to_fifo, 0)
        # READ_ALT.output(self._seg_req_push, self._infifo_valid_in & ~self._infifo_eos_in & ~self._seg_res_fifo_full)
        READ_ALT.output(self._seg_req_push, ~self._seg_res_fifo_full)
        READ_ALT.output(self._seg_rd_rsp_fifo_pop, 1)
        # READ_ALT.output(self._seg_pop_infifo, self._seg_grant_push & ~self._seg_res_fifo_full)
        READ_ALT.output(self._seg_pop_infifo, (self._seg_grant_push & ~self._seg_res_fifo_full) & ~self._readout_loop)
        READ_ALT.output(self._inc_req_made_seg, 0)
        READ_ALT.output(self._clr_req_made_seg, 0)
        READ_ALT.output(self._inc_req_rec_seg, 0)
        READ_ALT.output(self._clr_req_rec_seg, 0)
        READ_ALT.output(self._us_fifo_inject_data, 0)
        READ_ALT.output(self._us_fifo_inject_eos, 0)
        READ_ALT.output(self._us_fifo_inject_push, 0)
        # READ_ALT.output(self._seg_res_fifo_push_alloc, ~self._seg_res_fifo_full & self._seg_grant_push)
        # Don't do another alloc in this phase
        READ_ALT.output(self._seg_res_fifo_push_alloc, 0)
        READ_ALT.output(self._seg_res_fifo_push_fill, 0)
        READ_ALT.output(self._seg_res_fifo_fill_data_in, 0)

        #######
        # LOOKUP
        #######
        LOOKUP.output(self._seg_addr_out_to_fifo, self._infifo_pos_in)
        LOOKUP.output(self._seg_op_out_to_fifo, 1)
        LOOKUP.output(self._seg_ID_out_to_fifo, 0)
        # LOOKUP.output(self._seg_req_push, self._infifo_valid_in & ~self._infifo_eos_in)
        LOOKUP.output(self._seg_req_push, self._infifo_valid_in & ~self._infifo_eos_in & ~self._crd_res_fifo_full)
        LOOKUP.output(self._seg_rd_rsp_fifo_pop, 1)
        # LOOKUP.output(self._seg_pop_infifo, self._infifo_valid_in & ~self._infifo_eos_in & ~self._seg_res_fifo_full & self._seg_grant_push)
        # Pop the infifo if there's room and it's valid
        # If the input is a stop token, we just need room and valid, otherwise we need to make
        # sure that this controller is getting granted read request
        # LOOKUP.output(self._seg_pop_infifo, self._infifo_valid_in & ~self._crd_res_fifo_full)
        LOOKUP.output(self._seg_pop_infifo, self._infifo_valid_in & ~self._crd_res_fifo_full & kts.ternary(self._infifo_eos_in,
                                                                                                           kts.const(1, 1),
                                                                                                           self._seg_grant_push))
        LOOKUP.output(self._inc_req_made_seg, 0)
        LOOKUP.output(self._clr_req_made_seg, 1)
        LOOKUP.output(self._inc_req_rec_seg, 0)
        LOOKUP.output(self._clr_req_rec_seg, 1)
        LOOKUP.output(self._us_fifo_inject_data, 0)
        LOOKUP.output(self._us_fifo_inject_eos, 0)
        LOOKUP.output(self._us_fifo_inject_push, 0)
        LOOKUP.output(self._seg_res_fifo_push_alloc, ~self._crd_res_fifo_full & (self._seg_grant_push | (self._infifo_valid_in & self._infifo_eos_in)))
        # LOOKUP.output(self._seg_res_fifo_push_fill, 0)
        LOOKUP.output(self._seg_res_fifo_push_fill, self._infifo_valid_in & self._infifo_eos_in & ~self._crd_res_fifo_full)
        # LOOKUP.output(self._seg_res_fifo_fill_data_in, kts.concat(self._infifo_eos_in, self._infifo_pos_in))
        # LOOKUP.output(self._seg_res_fifo_fill_data_in, kts.ternary(self._infifo_eos_in & (self._infifo_pos_in[self.OPCODE_BT] == kts.const(2, 2)),
        LOOKUP.output(self._seg_res_fifo_fill_data_in, kts.ternary(self._infifo_eos_in & (self._infifo_pos_in[self.OPCODE_BT] == self.MAYBE_CODE),
                                                                   kts.const(0, self._seg_res_fifo_fill_data_in.width),
                                                                   kts.concat(self._infifo_eos_in, self._infifo_pos_in)))
        LOOKUP.output(self._set_pushed_done_seg, self._done_in & ~self._crd_res_fifo_full & self._spacc_mode)
        LOOKUP.output(self._set_readout_loop_seg, self._done_in & ~self._crd_res_fifo_full & self._spacc_mode)

        #######
        # PASS_STOP
        #######
        PASS_STOP.output(self._seg_addr_out_to_fifo, 0)
        PASS_STOP.output(self._seg_op_out_to_fifo, 0)
        PASS_STOP.output(self._seg_ID_out_to_fifo, 0)
        PASS_STOP.output(self._seg_req_push, 0)
        PASS_STOP.output(self._seg_rd_rsp_fifo_pop, 1)
        # PASS_STOP.output(self._seg_pop_infifo, ~self._seg_res_fifo_full & self._eos_in)
        PASS_STOP.output(self._seg_pop_infifo, (~self._seg_res_fifo_full & self._eos_in) & ~self._readout_loop)
        PASS_STOP.output(self._inc_req_made_seg, 0)
        PASS_STOP.output(self._clr_req_made_seg, 1)
        PASS_STOP.output(self._inc_req_rec_seg, 0)
        PASS_STOP.output(self._clr_req_rec_seg, 1)
        PASS_STOP.output(self._us_fifo_inject_data, 0)
        PASS_STOP.output(self._us_fifo_inject_eos, 0)
        PASS_STOP.output(self._us_fifo_inject_push, 0)
        # PASS_STOP.output(self._seg_res_fifo_push_alloc, self._infifo_valid_in & self._infifo_eos_in)
        PASS_STOP.output(self._seg_res_fifo_push_alloc, (self._infifo_valid_in | self._readout_loop) & ~self._seg_res_fifo_full)
        # PASS_STOP.output(self._seg_res_fifo_push_fill, self._infifo_valid_in & self._infifo_eos_in)
        PASS_STOP.output(self._seg_res_fifo_push_fill, (self._infifo_valid_in | self._readout_loop) & ~self._seg_res_fifo_full)
        # In the pass stop state, we either shove the eos in or add to it
        PASS_STOP.output(self._seg_res_fifo_fill_data_in, kts.ternary(self._readout_loop,
                                                                    #   self._last_stop_token,
                                                                      self._last_stop_token - 1,
                                                                      kts.ternary(self._eos_in,
                                                                                  kts.concat(kts.const(1, 1), (self._infifo_pos_in + 1)),
                                                                                  kts.concat(kts.const(1, 1), kts.const(0, self.data_width)))))
        # PASS_STOP.output(self._set_readout_loop_seg, self._seg_stop_lvl_geq_p1 & )

        #######
        # PASS_DONE
        #######
        PASS_DONE.output(self._seg_addr_out_to_fifo, 0)
        PASS_DONE.output(self._seg_op_out_to_fifo, 0)
        PASS_DONE.output(self._seg_ID_out_to_fifo, 0)
        # Only request a push when there's valid, non-eos data on the fifo
        # READ.output(self._seg_req_push, self._infifo_valid_in & ~self._infifo_eos_in & ~self._seg_res_fifo_full & ~self._dense)
        PASS_DONE.output(self._seg_req_push, 0)
        PASS_DONE.output(self._seg_rd_rsp_fifo_pop, 1)
        # Can pop the infifo if we are not in readout loop and we actually have a done in
        # PASS_DONE.output(self._seg_pop_infifo, ~self._readout_loop & self._done_in & ~self._seg_res_fifo_full)
        PASS_DONE.output(self._seg_pop_infifo, ~self._readout_loop & self._done_in & ~self._seg_res_fifo_full)
        PASS_DONE.output(self._inc_req_made_seg, 0)
        PASS_DONE.output(self._clr_req_made_seg, 0)
        PASS_DONE.output(self._inc_req_rec_seg, 0)
        PASS_DONE.output(self._clr_req_rec_seg, 0)
        PASS_DONE.output(self._us_fifo_inject_data, 0)
        PASS_DONE.output(self._us_fifo_inject_eos, 0)
        PASS_DONE.output(self._us_fifo_inject_push, 0)
        # Only going to be pushing something if we are not in the readout loop and have done, or in the readout loop and have pushed done
        # PASS_DONE.output(self._seg_res_fifo_push_alloc, ((self._readout_loop & self._pushed_done) | (~self._readout_loop & self._done_in)) & ~self._seg_res_fifo_full)
        PASS_DONE.output(self._seg_res_fifo_push_alloc, ((self._readout_loop & self._pushed_done) | (~self._readout_loop & self._last_stop_done)) & ~self._seg_res_fifo_full)
        # Only fill if we have done_in
        # PASS_DONE.output(self._seg_res_fifo_push_fill, ((self._readout_loop & self._pushed_done) | (~self._readout_loop & self._done_in)) & ~self._seg_res_fifo_full)
        PASS_DONE.output(self._seg_res_fifo_push_fill, ((self._readout_loop & self._pushed_done) | (~self._readout_loop & self._last_stop_done)) & ~self._seg_res_fifo_full)
        PASS_DONE.output(self._seg_res_fifo_fill_data_in, self.DONE_PROXY)
        # PASS_DONE.output(self._set_pushed_done_seg, self._done_in & ~self._seg_res_fifo_full & self._spacc_mode & self._infifo_valid_in)
        PASS_DONE.output(self._set_pushed_done_seg, self._last_stop_done & ~self._seg_res_fifo_full & self._spacc_mode)

        #######
        # FREE_SEG
        #######
        FREE_SEG.output(self._seg_addr_out_to_fifo, 0)
        FREE_SEG.output(self._seg_op_out_to_fifo, 0)
        FREE_SEG.output(self._seg_ID_out_to_fifo, 0)
        FREE_SEG.output(self._seg_req_push, 1)
        FREE_SEG.output(self._seg_rd_rsp_fifo_pop, 1)
        FREE_SEG.output(self._seg_pop_infifo, 0)
        FREE_SEG.output(self._inc_req_made_seg, 0)
        FREE_SEG.output(self._clr_req_made_seg, 0)
        FREE_SEG.output(self._inc_req_rec_seg, 0)
        FREE_SEG.output(self._clr_req_rec_seg, 0)
        FREE_SEG.output(self._us_fifo_inject_data, 0)
        FREE_SEG.output(self._us_fifo_inject_eos, 0)
        FREE_SEG.output(self._us_fifo_inject_push, 0)
        FREE_SEG.output(self._seg_res_fifo_push_alloc, 0)
        FREE_SEG.output(self._seg_res_fifo_push_fill, 0)
        FREE_SEG.output(self._seg_res_fifo_fill_data_in, 0)

        #######
        # DONE_SEG
        #######
        DONE_SEG.output(self._seg_addr_out_to_fifo, 0)
        DONE_SEG.output(self._seg_op_out_to_fifo, 0)
        DONE_SEG.output(self._seg_ID_out_to_fifo, 0)
        DONE_SEG.output(self._seg_req_push, 0)
        DONE_SEG.output(self._seg_rd_rsp_fifo_pop, 1)
        DONE_SEG.output(self._seg_pop_infifo, 0)
        DONE_SEG.output(self._inc_req_made_seg, 0)
        DONE_SEG.output(self._clr_req_made_seg, 0)
        DONE_SEG.output(self._inc_req_rec_seg, 0)
        DONE_SEG.output(self._clr_req_rec_seg, 0)
        DONE_SEG.output(self._us_fifo_inject_data, 0)
        DONE_SEG.output(self._us_fifo_inject_eos, 0)
        DONE_SEG.output(self._us_fifo_inject_push, 0)
        DONE_SEG.output(self._seg_res_fifo_push_alloc, 0)
        DONE_SEG.output(self._seg_res_fifo_push_fill, 0)
        DONE_SEG.output(self._seg_res_fifo_fill_data_in, 0)
        DONE_SEG.output(self._set_readout_loop_seg, self._seg_stop_lvl_geq_p1_sticky & ~self._readout_loop & self._spacc_mode & (self._crd_in_done_state | self._lookup_mode))
        DONE_SEG.output(self._clr_readout_loop_seg, self._readout_loop & self._spacc_mode & (self._crd_in_done_state | self._lookup_mode))
        DONE_SEG.output(self._seg_in_done_state, 1)

        self.scan_fsm_seg.set_start_state(START_SEG)

# ================================
# CRD/Optional FSM
# ================================
# This FSM is basically used to read coordinates in compressed mode,
# do dense streaming, and do the block reads

        self.scan_fsm_crd = self.add_fsm("scan_seq_crd", reset_high=False)

        START_CRD = self.scan_fsm_crd.add_state("START_CRD")
        DENSE_STRM = self.scan_fsm_crd.add_state("DENSE_STRM")
        SEQ_STRM = self.scan_fsm_crd.add_state("SEQ_STRM")
        # SEQ_ITER = self.scan_fsm.add_state("SEQ_ITER")
        # SEQ_DONE = self.scan_fsm.add_state("SEQ_DONE")
        # SEQ_STOP = self.scan_fsm.add_state("SEQ_STOP")

        FREE_CRD = self.scan_fsm_crd.add_state("FREE_CRD")
        # If performing the block reads, need to free both data structures :shrug:
        FREE_CRD2 = self.scan_fsm_crd.add_state("FREE_CRD2")

        # Block readout
        BLOCK_1_SIZE_REQ = self.scan_fsm_crd.add_state("BLOCK_1_SIZE_REQ")
        BLOCK_1_SIZE_REC = self.scan_fsm_crd.add_state("BLOCK_1_SIZE_REC")
        BLOCK_1_RD = self.scan_fsm_crd.add_state("BLOCK_1_RD")
        BLOCK_2_SIZE_REQ = self.scan_fsm_crd.add_state("BLOCK_2_SIZE_REQ")
        BLOCK_2_SIZE_REC = self.scan_fsm_crd.add_state("BLOCK_2_SIZE_REC")
        BLOCK_2_RD = self.scan_fsm_crd.add_state("BLOCK_2_RD")
        READOUT_SYNC_LOCK = self.scan_fsm_crd.add_state("READOUT_SYNC_LOCK")
        PASS_DONE_CRD = self.scan_fsm_crd.add_state("PASS_DONE_CRD")

        DONE_CRD = self.scan_fsm_crd.add_state("DONE_CRD")

        self.scan_fsm_crd.output(self._crd_addr_out_to_fifo)
        self.scan_fsm_crd.output(self._crd_op_out_to_fifo)
        self.scan_fsm_crd.output(self._crd_ID_out_to_fifo)
        self.scan_fsm_crd.output(self._crd_req_push)
        self.scan_fsm_crd.output(self._crd_rd_rsp_fifo_pop)
        self.scan_fsm_crd.output(self._pos_out_fifo_push)
        self.scan_fsm_crd.output(self._crd_pop_infifo)
        self.scan_fsm_crd.output(self._en_reg_data_in)
        self.scan_fsm_crd.output(self._pos_out_to_fifo)
        # Only use for DENSE_STRM
        self.scan_fsm_crd.output(self._crd_out_to_fifo)
        self.scan_fsm_crd.output(self._inc_req_made_crd)
        self.scan_fsm_crd.output(self._clr_req_made_crd)
        self.scan_fsm_crd.output(self._inc_req_rec_crd)
        self.scan_fsm_crd.output(self._clr_req_rec_crd)
        self.scan_fsm_crd.output(self._crd_res_fifo_push_alloc)
        self.scan_fsm_crd.output(self._crd_res_fifo_push_fill)
        self.scan_fsm_crd.output(self._ptr_reg_en)
        self.scan_fsm_crd.output(self._seg_res_fifo_pop)
        self.scan_fsm_crd.output(self._crd_in_done_state, default=kts.const(0, 1))
        self.scan_fsm_crd.output(self._set_pushed_done_crd, default=kts.const(0, 1))
        self.scan_fsm_crd.output(self._clr_pushed_done_crd, default=kts.const(0, 1))
        self.scan_fsm_crd.output(self._set_readout_loop_crd, default=kts.const(0, 1))
        self.scan_fsm_crd.output(self._clr_readout_loop_crd, default=kts.const(0, 1))
        # self.scan_fsm.output(self._ren)
        # self.scan_fsm.output(self._fifo_push)
        # self.scan_fsm.output(self._coord_out_fifo_push)
        # self.scan_fsm.output(self._last_valid_accepting)
        # self.scan_fsm.output(self._tag_eos)
        # self.scan_fsm.output(self._next_seq_length)
        # self.scan_fsm.output(self._update_seq_state)
        # self.scan_fsm.output(self._data_to_fifo)
        # self.scan_fsm.output(self._clr_pop_infifo_sticky, default=kts.const(0, 1))
        # self.scan_fsm.output(self._clr_seen_root_eos, default=kts.const(0, 1))
        # self.scan_fsm.output(self._inc_fiber_addr)
        # self.scan_fsm.output(self._clr_fiber_addr)
        # self.scan_fsm.output(self._inc_rep)
        # self.scan_fsm.output(self._clr_rep)
        # self.scan_fsm.output(self._us_fifo_inject_data, default=kts.const(0, self.data_width))
        # self.scan_fsm.output(self._us_fifo_inject_eos, default=kts.const(0, 1))
        # self.scan_fsm.output(self._us_fifo_inject_push, default=kts.const(0, 1))

        ####################
        # Next State Logic
        ####################
        # START_CRD.next(BLOCK_1_SIZE_REQ, self._block_mode)
        # START_CRD.next(BLOCK_1_SIZE_REQ, self._block_mode | (self._spacc_mode & self._readout_loop))
        START_CRD.next(BLOCK_1_SIZE_REQ, self._block_mode & self._tile_en)
        START_CRD.next(DENSE_STRM, self._dense & ~self._lookup_mode & self._tile_en)
        START_CRD.next(SEQ_STRM, ~self._dense & ~self._lookup_mode & self._tile_en)

        self._seg_res_fifo_done_out = self.var("seg_res_fifo_done_out", 1)
        self.wire(self._seg_res_fifo_done_out, self._seg_res_fifo_valid & self._seg_res_fifo_data_out[0][self.data_width] &
                      (self._seg_res_fifo_data_out[0][9, 8] == kts.const(1, 2)))

        self._crd_stop_lvl_geq = self.var("crd_stop_lvl_geq", 1)
        self.wire(self._crd_stop_lvl_geq, self._seg_res_fifo_valid & self._seg_res_fifo_data_out[0][16] & (self._seg_res_fifo_data_out[0][self.OPCODE_BT] == self.STOP_CODE) &
                                      (self._seg_res_fifo_data_out[0][self.STOP_BT] >= self._stop_lvl[self.STOP_BT]))

        # DENSE_STRM = self.scan_fsm_crd.add_state("DENSE_STRM")
        # DENSE_STRM.next(SEQ_STRM, self._num_req_made_crd == self._dim_size)
        # DENSE_STRM.next(FREE_CRD, self._num_req_made_crd == self._dim_size)
        DENSE_STRM.next(DENSE_STRM, None)

        # SEQ_STRM.next(FREE_CRD, self._done_in & ~self._fifo_full)
        # only leave SEQ_STRM if we see done
        # SEQ_STRM.next(FREE_CRD, self._seg_res_fifo_done_out & ~self._crd_res_fifo_full & ~self._pos_fifo.ports.full)
        # SEQ_STRM.next(FREE_CRD, ((self._seg_res_fifo_done_out | (self._spacc_mode & self._crd_stop_lvl_geq)) & ~self._crd_res_fifo_full & ~self._pos_fifo.ports.full))
        SEQ_STRM.next(FREE_CRD, ((self._seg_res_fifo_done_out | (self._spacc_mode & self._crd_stop_lvl_geq)) & ~self._crd_res_fifo_full & ~self._pos_fifo.ports.full))

        # Block readout
        # BLOCK_1_SIZE_REQ
        # BLOCK_1_SIZE_REQ.next(BLOCK_1_SIZE_REC, self._buffet_joined)
        BLOCK_1_SIZE_REQ.next(BLOCK_1_SIZE_REC, self._crd_grant_push)
        BLOCK_1_SIZE_REQ.next(BLOCK_1_SIZE_REQ, None)

        # BLOCK_1_SIZE_REC
        # Push the size on the line...
        # BLOCK_1_SIZE_REC.next(BLOCK_1_RD, self._buffet_joined & ~self._fifo_full & self._rd_rsp_fifo_valid)
        # BLOCK_1_SIZE_REC.next(BLOCK_1_RD, self._crd_grant_push & ~self._fifo_full & self._rd_rsp_fifo_valid)
        # BLOCK_1_SIZE_REC.next(BLOCK_1_RD, self._crd_grant_push & ~self._crd_res_fifo_full & self._rd_rsp_fifo_valid)
        BLOCK_1_SIZE_REC.next(BLOCK_1_RD, self._rd_rsp_fifo_valid)
        BLOCK_1_SIZE_REC.next(BLOCK_1_SIZE_REC, None)

        BLOCK_1_RD.next(BLOCK_2_SIZE_REQ, (self._num_req_rec_crd == self._ptr_reg) & ~self._lookup_mode)
        BLOCK_1_RD.next(FREE_CRD, (self._num_req_rec_crd == self._ptr_reg) & self._lookup_mode)
        BLOCK_1_RD.next(BLOCK_1_RD, None)

        # BLOCK_2_SIZE_REQ
        # BLOCK_2_SIZE_REQ.next(BLOCK_2_SIZE_REC, self._buffet_joined)
        BLOCK_2_SIZE_REQ.next(BLOCK_2_SIZE_REC, self._crd_grant_push)
        BLOCK_2_SIZE_REQ.next(BLOCK_2_SIZE_REQ, None)

        # BLOCK_2_SIZE_REC
        # Push the size on the line...
        # BLOCK_2_SIZE_REC.next(BLOCK_2_RD, self._buffet_joined & ~self._fifo_full & self._rd_rsp_fifo_valid)
        # BLOCK_2_SIZE_REC.next(BLOCK_2_RD, self._crd_grant_push & ~self._fifo_full & self._rd_rsp_fifo_valid)
        # BLOCK_2_SIZE_REC.next(BLOCK_2_RD, self._crd_grant_push & ~self._crd_res_fifo_full & self._rd_rsp_fifo_valid)
        BLOCK_2_SIZE_REC.next(BLOCK_2_RD, self._rd_rsp_fifo_valid)
        BLOCK_2_SIZE_REC.next(BLOCK_2_SIZE_REC, None)

        BLOCK_2_RD.next(FREE_CRD, self._num_req_rec_crd == self._ptr_reg)
        BLOCK_2_RD.next(BLOCK_2_RD, None)

        # DONE
        # Go to START after sending a free?
        # FREE_CRD.next(FREE_CRD2, sel  READ = 4'h6,f._buffet_joined & ~self._lookup_mode)
        # Since the block mode is handled by the crd side - free the second data structure

        # only if in block mode and not lookup
        # FREE_CRD.next(FREE_CRD2, self._crd_grant_push & (self._block_mode | (self._spacc_mode & self._readout_loop)) & ~self._lookup_mode)
        # Move on if not in block mode (or in lookup block mode) as in the dense case we never get here, and in any non-dense,
        # non block mode, we wouldn't handle freeing more than 1 ds
        # FREE_CRD.next(DONE_CRD, self._crd_grant_push)
        # FREE_CRD.next(READOUT_SYNC_LOCK, self._crd_grant_push & self._spacc_mode & self._readout_loop)
        # If we have pushed done and are not in the readout loop yet, we need to push the DONE token through on normal channels
        # FREE_CRD.next(PASS_DONE_CRD, self._crd_grant_push & self._pushed_done & ~self._readout_loop & ~self._crd_res_fifo_full)

        # If in block mode basically need to clear both structures
        FREE_CRD.next(FREE_CRD2, self._crd_grant_push & (self._block_mode) & ~self._lookup_mode)
        # If in spacc mode and we've pushed done, we need a special way to push it out on top of what else we've sent
        FREE_CRD.next(PASS_DONE_CRD, self._crd_grant_push & self._pushed_done & self._spacc_mode)
        # If we are not in spacc more or haven't pushed a final done, we are clear to continue on normally
        # FREE_CRD.next(DONE_CRD, self._crd_grant_push & (~self._spacc_mode | self._readout_loop))
        FREE_CRD.next(DONE_CRD, self._crd_grant_push & (~self._spacc_mode | ~self._pushed_done))
        # FREE_CRD.next(DONE_CRD, self._buffet_joined & self._lookup_mode)
        FREE_CRD.next(FREE_CRD, None)

        # FREE_CRD2.next(READOUT_SYNC_LOCK, self._crd_grant_push & self._spacc_mode & self._readout_loop)
        FREE_CRD2.next(DONE_CRD, self._crd_grant_push)
        # FREE_CRD2.next(DONE_CRD, self._buffet_joined)
        # FREE2.next(START, self._buffet_joined & ~self._root)
        # FREE2.next(DONE, self._buffet_joined & self._root)
        FREE_CRD2.next(FREE_CRD2, None)

        # State inserted for sync
        READOUT_SYNC_LOCK.next(DONE_CRD, None)

        PASS_DONE_CRD.next(DONE_CRD, ~self._crd_res_fifo_full & ~self._pos_fifo.ports.full)

        # DONE_CRD.next(START_CRD, None)
        # DONE_CRD.next(START_CRD, None)
        # Stopgap to synchronize the crd fsm from going to next phase without waiting for seg fsm when it will
        # be going into readout loop
        # DONE_CRD.next(START_CRD, ~self._spacc_mode | (self._spacc_mode & ((self._seg_stop_lvl_geq_p1_sticky & self._seg_in_done_state) | ~self._seg_stop_lvl_geq_p1_sticky)))
        DONE_CRD.next(START_CRD, ~self._spacc_mode | (self._spacc_mode & ((self._seg_in_done_state))))
        # DONE_CRD.next(DONE_CRD, None)

        ################
        # STATE OUTPUTS
        ################

        ######################
        # START_CRD
        ######################
        START_CRD.output(self._crd_addr_out_to_fifo, 0)
        START_CRD.output(self._crd_op_out_to_fifo, 0)
        START_CRD.output(self._crd_ID_out_to_fifo, 0)
        START_CRD.output(self._crd_req_push, 0)
        START_CRD.output(self._crd_rd_rsp_fifo_pop, 0)
        START_CRD.output(self._pos_out_fifo_push, 0)
        START_CRD.output(self._crd_pop_infifo, 0)
        START_CRD.output(self._en_reg_data_in, 0)
        START_CRD.output(self._pos_out_to_fifo, 0)
        # Only used in DENSE_STRM mode
        START_CRD.output(self._crd_out_to_fifo, 0)
        START_CRD.output(self._inc_req_made_crd, 0)
        START_CRD.output(self._clr_req_made_crd, 0)
        START_CRD.output(self._inc_req_rec_crd, 0)
        START_CRD.output(self._clr_req_rec_crd, 0)
        START_CRD.output(self._crd_res_fifo_push_alloc, 0)
        START_CRD.output(self._crd_res_fifo_push_fill, 0)
        START_CRD.output(self._ptr_reg_en, 0)
        START_CRD.output(self._seg_res_fifo_pop, 0)

        ######################
        # DENSE_STRM
        ######################
        DENSE_STRM.output(self._crd_addr_out_to_fifo, 0)
        DENSE_STRM.output(self._crd_op_out_to_fifo, 0)
        DENSE_STRM.output(self._crd_ID_out_to_fifo, 0)
        DENSE_STRM.output(self._crd_req_push, 0)
        # DENSE_STRM.output(self._crd_rd_rsp_fifo_pop, 1)
        DENSE_STRM.output(self._crd_rd_rsp_fifo_pop, 0)
        # Push out to the pos fifo if the current input is valid - if it's a stop token then push it
        # if there is room, otherwise push it if there have been few enough requests made
        DENSE_STRM.output(self._pos_out_fifo_push, self._seg_res_fifo_valid & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full &
                                                    kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                                kts.const(1, 1),
                                                                (self._num_req_made_crd < self._dim_size)))
        DENSE_STRM.output(self._crd_pop_infifo, 0)
        DENSE_STRM.output(self._en_reg_data_in, 0)
        # DENSE_STRM.output(self._pos_out_to_fifo, 0)
        # If it's a stop token, pass it through, otherwise do the math
        DENSE_STRM.output(self._pos_out_to_fifo, kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                             self._seg_res_fifo_data_out[0],
                                                             (self._seg_res_fifo_data_out[0][self.data_width - 1, 0] * self._dim_size) + self._num_req_made_crd))
        DENSE_STRM.output(self._crd_out_to_fifo, kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                             self._seg_res_fifo_data_out[0],
                                                             self._num_req_made_crd))
        DENSE_STRM.output(self._inc_req_made_crd, self._seg_res_fifo_valid & (self._num_req_made_crd < self._dim_size) & ~self._seg_res_fifo_data_out[0][self.data_width] &
                                                    ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full)
        # Can simply clear this once there is a stop token on the input
        # DENSE_STRM.output(self._clr_req_made_crd, (self._num_req_made_crd == (self._dim_size - 1)) & self._inc_req_made_crd)
        DENSE_STRM.output(self._clr_req_made_crd, self._seg_res_fifo_valid & self._seg_res_fifo_data_out[0][self.data_width])
        DENSE_STRM.output(self._inc_req_rec_crd, 0)
        DENSE_STRM.output(self._clr_req_rec_crd, 0)
        # Push the data
        DENSE_STRM.output(self._crd_res_fifo_push_alloc, self._seg_res_fifo_valid & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full &
                                                            kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                                        kts.const(1, 1),
                                                                        (self._num_req_made_crd < self._dim_size)))
        # DENSE_STRM.output(self._crd_res_fifo_push_fill, self._seg_res_fifo_valid & (self._num_req_made_crd < self._dim_size) & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full)
        DENSE_STRM.output(self._crd_res_fifo_push_fill, self._seg_res_fifo_valid & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full &
                                                            kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                                        kts.const(1, 1),
                                                                        (self._num_req_made_crd < self._dim_size)))
        DENSE_STRM.output(self._ptr_reg_en, 0)
        # DENSE_STRM.output(self._seg_res_fifo_pop, self._clr_req_made_crd)
        # Pop once we are either finished emitting the dense stream, or we have a stop token on the input (and there's output space)
        DENSE_STRM.output(self._seg_res_fifo_pop, self._seg_res_fifo_valid & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full &
                                                    kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                                kts.const(1, 1),
                                                                (self._num_req_made_crd == (self._dim_size - 1)) & self._inc_req_made_crd))
        # DENSE_STRM.output(self._set_pushed_done_crd, ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_done_out & self._spacc_mode)

        ######################
        # SEQ_STRM
        ######################
        SEQ_STRM.output(self._crd_addr_out_to_fifo, self._num_req_made_crd + self._seg_res_fifo_data_out[0][self.data_width - 1, 0])
        SEQ_STRM.output(self._crd_op_out_to_fifo, 1)
        SEQ_STRM.output(self._crd_ID_out_to_fifo, 1)
        SEQ_STRM.output(self._crd_req_push, self._seg_res_fifo_valid & ~self._seg_res_fifo_data_out[0][self.data_width] &
                        ~self._crd_res_fifo_full & (self._num_req_made_crd < self._seq_length_ptr_math) & ~self._pos_fifo.ports.full)
        SEQ_STRM.output(self._crd_rd_rsp_fifo_pop, 1)
        SEQ_STRM.output(self._pos_out_fifo_push, kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                             ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid,
                                                             self._crd_grant_push & (self._num_req_made_crd < self._seq_length_ptr_math) &
                                                             ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid))
        SEQ_STRM.output(self._crd_pop_infifo, 0)
        SEQ_STRM.output(self._en_reg_data_in, 0)
        SEQ_STRM.output(self._pos_out_to_fifo, kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                           self._seg_res_fifo_data_out[0],
                                                           kts.concat(kts.const(0, 1),
                                                                      self._num_req_made_crd + self._seg_res_fifo_data_out[0][self.data_width - 1, 0])))
        SEQ_STRM.output(self._crd_out_to_fifo, 0)
        SEQ_STRM.output(self._inc_req_made_crd, self._crd_grant_push & (self._num_req_made_crd < self._seq_length_ptr_math) & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid)
        SEQ_STRM.output(self._clr_req_made_crd, ((self._crd_grant_push & (self._num_req_made_crd == (self._seq_length_ptr_math - 1))) | (self._seq_length_ptr_math == 0)) &
                                                ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid)
        SEQ_STRM.output(self._inc_req_rec_crd, 0)
        SEQ_STRM.output(self._clr_req_rec_crd, 0)
        SEQ_STRM.output(self._crd_res_fifo_push_alloc, kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                                   ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid,
                                                                   self._crd_grant_push & (self._num_req_made_crd < self._seq_length_ptr_math) &
                                                                   ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid))
        SEQ_STRM.output(self._crd_res_fifo_push_fill, self._seg_res_fifo_valid & self._seg_res_fifo_data_out[0][self.data_width] & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full)
        SEQ_STRM.output(self._ptr_reg_en, 0)
        SEQ_STRM.output(self._seg_res_fifo_pop, self._clr_req_made_crd | (self._seg_res_fifo_valid & self._seg_res_fifo_data_out[0][self.data_width] &
                                                                          ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full))
        # Indicate that we have pushed done to move into readout mode for spacc
        # SEQ_STRM.output(self._set_pushed_done_crd, ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_done_out & self._spacc_mode)

        ######################
        # FREE_CRD
        ######################
        FREE_CRD.output(self._crd_addr_out_to_fifo, 0)
        FREE_CRD.output(self._crd_op_out_to_fifo, 0)
        # FREE_CRD.output(self._crd_ID_out_to_fifo, 1)
        # In block mode, we are always freeing the 0 id here
        # else, we are just handling the crd structure
        FREE_CRD.output(self._crd_ID_out_to_fifo, kts.ternary(self._block_mode,
                                                              kts.const(0, self._crd_ID_out_to_fifo.width),
                                                              kts.const(1, self._crd_ID_out_to_fifo.width)))
        FREE_CRD.output(self._crd_req_push, 1)
        FREE_CRD.output(self._crd_rd_rsp_fifo_pop, 0)
        FREE_CRD.output(self._pos_out_fifo_push, 0)
        FREE_CRD.output(self._crd_pop_infifo, 0)
        FREE_CRD.output(self._en_reg_data_in, 0)
        FREE_CRD.output(self._pos_out_to_fifo, 0)
        FREE_CRD.output(self._crd_out_to_fifo, 0)
        FREE_CRD.output(self._inc_req_made_crd, 0)
        FREE_CRD.output(self._clr_req_made_crd, 1)
        FREE_CRD.output(self._inc_req_rec_crd, 0)
        FREE_CRD.output(self._clr_req_rec_crd, 1)
        FREE_CRD.output(self._crd_res_fifo_push_alloc, 0)
        FREE_CRD.output(self._crd_res_fifo_push_fill, 0)
        FREE_CRD.output(self._ptr_reg_en, 0)
        FREE_CRD.output(self._seg_res_fifo_pop, 0)

        ######################
        # FREE_CRD2
        ######################
        # Always free the ID=1 data structure here, so we can handle freeing 0 or 1 in the first state
        FREE_CRD2.output(self._crd_addr_out_to_fifo, 0)
        FREE_CRD2.output(self._crd_op_out_to_fifo, 0)
        FREE_CRD2.output(self._crd_ID_out_to_fifo, 1)
        FREE_CRD2.output(self._crd_req_push, 1)
        FREE_CRD2.output(self._crd_rd_rsp_fifo_pop, 0)
        FREE_CRD2.output(self._pos_out_fifo_push, 0)
        FREE_CRD2.output(self._crd_pop_infifo, 0)
        FREE_CRD2.output(self._en_reg_data_in, 0)
        FREE_CRD2.output(self._pos_out_to_fifo, 0)
        FREE_CRD2.output(self._crd_out_to_fifo, 0)
        FREE_CRD2.output(self._inc_req_made_crd, 0)
        FREE_CRD2.output(self._clr_req_made_crd, 1)
        FREE_CRD2.output(self._inc_req_rec_crd, 0)
        FREE_CRD2.output(self._clr_req_rec_crd, 1)
        FREE_CRD2.output(self._crd_res_fifo_push_alloc, 0)
        FREE_CRD2.output(self._crd_res_fifo_push_fill, 0)
        FREE_CRD2.output(self._ptr_reg_en, 0)
        FREE_CRD2.output(self._seg_res_fifo_pop, 0)

        ######################
        # BLOCK_1_SIZE_REQ
        ######################
        BLOCK_1_SIZE_REQ.output(self._crd_addr_out_to_fifo, 0)
        BLOCK_1_SIZE_REQ.output(self._crd_op_out_to_fifo, 2)
        BLOCK_1_SIZE_REQ.output(self._crd_ID_out_to_fifo, 0)
        BLOCK_1_SIZE_REQ.output(self._crd_req_push, ~self._crd_res_fifo_full)
        BLOCK_1_SIZE_REQ.output(self._crd_rd_rsp_fifo_pop, 0)
        BLOCK_1_SIZE_REQ.output(self._pos_out_fifo_push, 0)
        BLOCK_1_SIZE_REQ.output(self._crd_pop_infifo, 0)
        BLOCK_1_SIZE_REQ.output(self._en_reg_data_in, 0)
        BLOCK_1_SIZE_REQ.output(self._pos_out_to_fifo, 0)
        BLOCK_1_SIZE_REQ.output(self._crd_out_to_fifo, 0)
        BLOCK_1_SIZE_REQ.output(self._inc_req_made_crd, 0)
        BLOCK_1_SIZE_REQ.output(self._clr_req_made_crd, 0)
        BLOCK_1_SIZE_REQ.output(self._inc_req_rec_crd, 0)
        BLOCK_1_SIZE_REQ.output(self._clr_req_rec_crd, 0)
        BLOCK_1_SIZE_REQ.output(self._crd_res_fifo_push_alloc, ~self._crd_res_fifo_full & self._crd_grant_push)
        BLOCK_1_SIZE_REQ.output(self._crd_res_fifo_push_fill, 0)
        BLOCK_1_SIZE_REQ.output(self._ptr_reg_en, 0)
        BLOCK_1_SIZE_REQ.output(self._seg_res_fifo_pop, 0)
        # If we are in the readout phase of sparse accumulate, then we can clear the pushed done flag for the next go around
        # since we won't deal with any more DONE tokens coming in until the next execution

        ######################
        # BLOCK_1_SIZE_REC
        ######################
        BLOCK_1_SIZE_REC.output(self._crd_addr_out_to_fifo, 0)
        BLOCK_1_SIZE_REC.output(self._crd_op_out_to_fifo, 0)
        BLOCK_1_SIZE_REC.output(self._crd_ID_out_to_fifo, 0)
        BLOCK_1_SIZE_REC.output(self._crd_req_push, 0)
        BLOCK_1_SIZE_REC.output(self._crd_rd_rsp_fifo_pop, 1)
        BLOCK_1_SIZE_REC.output(self._pos_out_fifo_push, 0)
        BLOCK_1_SIZE_REC.output(self._crd_pop_infifo, 0)
        BLOCK_1_SIZE_REC.output(self._en_reg_data_in, 0)
        BLOCK_1_SIZE_REC.output(self._pos_out_to_fifo, 0)
        BLOCK_1_SIZE_REC.output(self._crd_out_to_fifo, 0)
        BLOCK_1_SIZE_REC.output(self._inc_req_made_crd, 0)
        BLOCK_1_SIZE_REC.output(self._clr_req_made_crd, 1)
        BLOCK_1_SIZE_REC.output(self._inc_req_rec_crd, 0)
        BLOCK_1_SIZE_REC.output(self._clr_req_rec_crd, 0)
        BLOCK_1_SIZE_REC.output(self._crd_res_fifo_push_alloc, 0)
        BLOCK_1_SIZE_REC.output(self._crd_res_fifo_push_fill, 0)
        BLOCK_1_SIZE_REC.output(self._ptr_reg_en, self._rd_rsp_fifo_valid)
        BLOCK_1_SIZE_REC.output(self._seg_res_fifo_pop, 0)

        ######################
        # BLOCK_1_RD
        ######################
        BLOCK_1_RD.output(self._crd_addr_out_to_fifo, self._num_req_made_crd)
        BLOCK_1_RD.output(self._crd_op_out_to_fifo, 1)
        BLOCK_1_RD.output(self._crd_ID_out_to_fifo, 0)
        # BLOCK_1_RD.output(self._crd_req_push, (self._num_req_made_crd < self._ptr_reg) & self._crd_grant_push & ~self._crd_res_fifo_full)
        BLOCK_1_RD.output(self._crd_req_push, (self._num_req_made_crd < self._ptr_reg) & ~self._crd_res_fifo_full)
        BLOCK_1_RD.output(self._crd_rd_rsp_fifo_pop, (self._num_req_rec_crd < self._ptr_reg))
        BLOCK_1_RD.output(self._pos_out_fifo_push, 0)
        BLOCK_1_RD.output(self._crd_pop_infifo, 0)
        BLOCK_1_RD.output(self._en_reg_data_in, 0)
        BLOCK_1_RD.output(self._pos_out_to_fifo, 0)
        BLOCK_1_RD.output(self._crd_out_to_fifo, 0)
        BLOCK_1_RD.output(self._inc_req_made_crd, (self._num_req_made_crd < self._ptr_reg) & self._crd_grant_push & ~self._crd_res_fifo_full)
        BLOCK_1_RD.output(self._clr_req_made_crd, 0)
        BLOCK_1_RD.output(self._inc_req_rec_crd, (self._num_req_rec_crd < self._ptr_reg) & self._rd_rsp_fifo_valid)
        BLOCK_1_RD.output(self._clr_req_rec_crd, 0)
        BLOCK_1_RD.output(self._crd_res_fifo_push_alloc, (self._num_req_made_crd < self._ptr_reg) & self._crd_grant_push & ~self._crd_res_fifo_full)
        BLOCK_1_RD.output(self._crd_res_fifo_push_fill, 0)
        BLOCK_1_RD.output(self._ptr_reg_en, 0)
        BLOCK_1_RD.output(self._seg_res_fifo_pop, 0)

        ######################
        # BLOCK_2_SIZE_REQ
        ######################
        BLOCK_2_SIZE_REQ.output(self._crd_addr_out_to_fifo, 0)
        BLOCK_2_SIZE_REQ.output(self._crd_op_out_to_fifo, 2)
        BLOCK_2_SIZE_REQ.output(self._crd_ID_out_to_fifo, 1)
        BLOCK_2_SIZE_REQ.output(self._crd_req_push, ~self._crd_res_fifo_full)
        BLOCK_2_SIZE_REQ.output(self._crd_rd_rsp_fifo_pop, 0)
        BLOCK_2_SIZE_REQ.output(self._pos_out_fifo_push, 0)
        BLOCK_2_SIZE_REQ.output(self._crd_pop_infifo, 0)
        BLOCK_2_SIZE_REQ.output(self._en_reg_data_in, 0)
        BLOCK_2_SIZE_REQ.output(self._pos_out_to_fifo, 0)
        BLOCK_2_SIZE_REQ.output(self._crd_out_to_fifo, 0)
        BLOCK_2_SIZE_REQ.output(self._inc_req_made_crd, 0)
        BLOCK_2_SIZE_REQ.output(self._clr_req_made_crd, 0)
        BLOCK_2_SIZE_REQ.output(self._inc_req_rec_crd, 0)
        BLOCK_2_SIZE_REQ.output(self._clr_req_rec_crd, 0)
        BLOCK_2_SIZE_REQ.output(self._crd_res_fifo_push_alloc, ~self._crd_res_fifo_full & self._crd_grant_push)
        BLOCK_2_SIZE_REQ.output(self._crd_res_fifo_push_fill, 0)
        BLOCK_2_SIZE_REQ.output(self._ptr_reg_en, 0)
        BLOCK_2_SIZE_REQ.output(self._seg_res_fifo_pop, 0)

        ######################
        # BLOCK_2_SIZE_REC
        ######################
        BLOCK_2_SIZE_REC.output(self._crd_addr_out_to_fifo, 0)
        BLOCK_2_SIZE_REC.output(self._crd_op_out_to_fifo, 0)
        BLOCK_2_SIZE_REC.output(self._crd_ID_out_to_fifo, 0)
        BLOCK_2_SIZE_REC.output(self._crd_req_push, 0)
        BLOCK_2_SIZE_REC.output(self._crd_rd_rsp_fifo_pop, 1)
        BLOCK_2_SIZE_REC.output(self._pos_out_fifo_push, 0)
        BLOCK_2_SIZE_REC.output(self._crd_pop_infifo, 0)
        BLOCK_2_SIZE_REC.output(self._en_reg_data_in, 0)
        BLOCK_2_SIZE_REC.output(self._pos_out_to_fifo, 0)
        BLOCK_2_SIZE_REC.output(self._crd_out_to_fifo, 0)
        BLOCK_2_SIZE_REC.output(self._inc_req_made_crd, 0)
        BLOCK_2_SIZE_REC.output(self._clr_req_made_crd, 1)
        BLOCK_2_SIZE_REC.output(self._inc_req_rec_crd, 0)
        BLOCK_2_SIZE_REC.output(self._clr_req_rec_crd, 1)
        BLOCK_2_SIZE_REC.output(self._crd_res_fifo_push_alloc, 0)
        BLOCK_2_SIZE_REC.output(self._crd_res_fifo_push_fill, 0)
        BLOCK_2_SIZE_REC.output(self._ptr_reg_en, self._rd_rsp_fifo_valid)
        BLOCK_2_SIZE_REC.output(self._seg_res_fifo_pop, 0)

        ######################
        # BLOCK_2_RD
        ######################
        BLOCK_2_RD.output(self._crd_addr_out_to_fifo, self._num_req_made_crd)
        BLOCK_2_RD.output(self._crd_op_out_to_fifo, 1)
        BLOCK_2_RD.output(self._crd_ID_out_to_fifo, 1)
        BLOCK_2_RD.output(self._crd_req_push, (self._num_req_made_crd < self._ptr_reg) & ~self._crd_res_fifo_full)
        BLOCK_2_RD.output(self._crd_rd_rsp_fifo_pop, (self._num_req_rec_crd < self._ptr_reg))
        BLOCK_2_RD.output(self._pos_out_fifo_push, 0)
        BLOCK_2_RD.output(self._crd_pop_infifo, 0)
        BLOCK_2_RD.output(self._en_reg_data_in, 0)
        BLOCK_2_RD.output(self._pos_out_to_fifo, 0)
        BLOCK_2_RD.output(self._crd_out_to_fifo, 0)
        BLOCK_2_RD.output(self._inc_req_made_crd, (self._num_req_made_crd < self._ptr_reg) & self._crd_grant_push & ~self._crd_res_fifo_full)
        BLOCK_2_RD.output(self._clr_req_made_crd, 0)
        BLOCK_2_RD.output(self._inc_req_rec_crd, (self._num_req_rec_crd < self._ptr_reg) & self._rd_rsp_fifo_valid)
        BLOCK_2_RD.output(self._clr_req_rec_crd, 0)
        BLOCK_2_RD.output(self._crd_res_fifo_push_alloc, (self._num_req_made_crd < self._ptr_reg) & self._crd_grant_push & ~self._crd_res_fifo_full)
        BLOCK_2_RD.output(self._crd_res_fifo_push_fill, 0)
        BLOCK_2_RD.output(self._ptr_reg_en, 0)
        BLOCK_2_RD.output(self._seg_res_fifo_pop, 0)

        ######################
        # READOUT_SYNC_LOCK
        ######################
        READOUT_SYNC_LOCK.output(self._crd_addr_out_to_fifo, 0)
        READOUT_SYNC_LOCK.output(self._crd_op_out_to_fifo, 0)
        READOUT_SYNC_LOCK.output(self._crd_ID_out_to_fifo, 0)
        READOUT_SYNC_LOCK.output(self._crd_req_push, 0)
        READOUT_SYNC_LOCK.output(self._crd_rd_rsp_fifo_pop, 0)
        READOUT_SYNC_LOCK.output(self._pos_out_fifo_push, 0)
        READOUT_SYNC_LOCK.output(self._crd_pop_infifo, 0)
        READOUT_SYNC_LOCK.output(self._en_reg_data_in, 0)
        READOUT_SYNC_LOCK.output(self._pos_out_to_fifo, 0)
        READOUT_SYNC_LOCK.output(self._crd_out_to_fifo, 0)
        READOUT_SYNC_LOCK.output(self._inc_req_made_crd, 0)
        READOUT_SYNC_LOCK.output(self._clr_req_made_crd, 0)
        READOUT_SYNC_LOCK.output(self._inc_req_rec_crd, 0)
        READOUT_SYNC_LOCK.output(self._clr_req_rec_crd, 0)
        READOUT_SYNC_LOCK.output(self._crd_res_fifo_push_alloc, 0)
        READOUT_SYNC_LOCK.output(self._crd_res_fifo_push_fill, 0)
        READOUT_SYNC_LOCK.output(self._ptr_reg_en, 0)
        READOUT_SYNC_LOCK.output(self._seg_res_fifo_pop, 0)
        READOUT_SYNC_LOCK.output(self._clr_pushed_done_crd, 1)
        READOUT_SYNC_LOCK.output(self._clr_readout_loop_crd, 1)

        ######################
        # PASS_DONE_CRD
        ######################
        PASS_DONE_CRD.output(self._crd_addr_out_to_fifo, 0)
        PASS_DONE_CRD.output(self._crd_op_out_to_fifo, 0)
        PASS_DONE_CRD.output(self._crd_ID_out_to_fifo, 0)
        PASS_DONE_CRD.output(self._crd_req_push, 0)
        PASS_DONE_CRD.output(self._crd_rd_rsp_fifo_pop, 0)
        PASS_DONE_CRD.output(self._pos_out_fifo_push, ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_done_out & self._seg_res_fifo_valid)
        PASS_DONE_CRD.output(self._crd_pop_infifo, 0)
        PASS_DONE_CRD.output(self._en_reg_data_in, 0)
        PASS_DONE_CRD.output(self._pos_out_to_fifo, self.DONE_PROXY)
        PASS_DONE_CRD.output(self._crd_out_to_fifo, 0)
        PASS_DONE_CRD.output(self._inc_req_made_crd, 0)
        PASS_DONE_CRD.output(self._clr_req_made_crd, 0)
        PASS_DONE_CRD.output(self._inc_req_rec_crd, 0)
        PASS_DONE_CRD.output(self._clr_req_rec_crd, 0)
        PASS_DONE_CRD.output(self._crd_res_fifo_push_alloc, ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_done_out & self._seg_res_fifo_valid)
        PASS_DONE_CRD.output(self._crd_res_fifo_push_fill, ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_done_out & self._seg_res_fifo_valid)
        PASS_DONE_CRD.output(self._ptr_reg_en, 0)
        PASS_DONE_CRD.output(self._seg_res_fifo_pop, ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_done_out & self._seg_res_fifo_valid)
        PASS_DONE_CRD.output(self._clr_pushed_done_crd, 0)
        PASS_DONE_CRD.output(self._clr_readout_loop_crd, 0)

        ######################
        # DONE_CRD
        ######################
        DONE_CRD.output(self._crd_addr_out_to_fifo, 0)
        DONE_CRD.output(self._crd_op_out_to_fifo, 0)
        DONE_CRD.output(self._crd_ID_out_to_fifo, 0)
        DONE_CRD.output(self._crd_req_push, 0)
        DONE_CRD.output(self._crd_rd_rsp_fifo_pop, 0)
        DONE_CRD.output(self._pos_out_fifo_push, 0)
        DONE_CRD.output(self._crd_pop_infifo, 0)
        DONE_CRD.output(self._en_reg_data_in, 0)
        DONE_CRD.output(self._pos_out_to_fifo, 0)
        DONE_CRD.output(self._crd_out_to_fifo, 0)
        DONE_CRD.output(self._inc_req_made_crd, 0)
        DONE_CRD.output(self._clr_req_made_crd, 1)
        DONE_CRD.output(self._inc_req_rec_crd, 0)
        DONE_CRD.output(self._clr_req_rec_crd, 1)
        DONE_CRD.output(self._crd_res_fifo_push_alloc, 0)
        DONE_CRD.output(self._crd_res_fifo_push_fill, 0)
        DONE_CRD.output(self._ptr_reg_en, 0)
        DONE_CRD.output(self._seg_res_fifo_pop, 0)
        DONE_CRD.output(self._crd_in_done_state, 1)
        # Once we are done, if we should enter the readout loop state we follow this...
        # DONE_CRD.output(self._set_readout_loop_crd, self._spacc_mode & self._pushed_done)
        DONE_CRD.output(self._set_readout_loop_crd, 0)

        self.scan_fsm_crd.set_start_state(START_CRD)

# ===================================
# Dump metadata into fifo
# ===================================

        # Weird place to handle this, but :shrug:
        self.wire(self._readout_dst_crd, self._readout_loop)
        self.wire(self._readout_dst_seg, self._readout_loop)

        self._inc_requests_made = self.var("inc_requests_made_CRDDD_READ", 1)
        self.wire(self._inc_requests_made, self._crd_grant_push)
        self._reads_made = add_counter(self, "READS_MADE", 16, increment=self._inc_requests_made)

        self._inc_requests_made_ = self.var("inc_requests_REC_CRD_READ", 1)
        self.wire(self._inc_requests_made_, self._rd_rsp_fifo_valid & (self._rd_rsp_fifo_out_data[self.data_width] == kts.const(1, 1)))
        self._reads_made_ = add_counter(self, "READS_REC_CRD_READ", 16, increment=self._inc_requests_made_)

        # self.wire(self._ready_out, self._ren)
        # Include the outer coord as well
        # self._oc_to_fifo = self.var("outer_coord_to_fifo", self.data_width)
        # self.wire(self._oc_to_fifo, kts.ternary(self._root, kts.const(0, self.data_width), self._infifo_coord_in))

        ### COORD FIFO
        self._coord_data_in_packed = self.var("coord_fifo_in_packed", self.data_width + 1, packed=True)
        # self.wire(self._coord_data_in_packed[self.data_width], self._tag_eos)
        # self.wire(self._coord_data_in_packed[self.data_width - 1, 0], self._data_to_fifo)

        self.wire(self._coord_data_in_packed, self._crd_res_fifo_data_out)

        self._coord_data_out_packed = self.var("coord_fifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._coord_out[self.data_width], self._coord_data_out_packed[self.data_width])
        self.wire(self._coord_out[self.data_width - 1, 0], self._coord_data_out_packed[self.data_width - 1, 0])

        # Signal for getting pushed done
        self._set_final_pushed_done = self.var("set_final_pushed_done", 1)
        self._clr_final_pushed_done = self.var("clr_final_pushed_done", 1)
        self._final_pushed_done = sticky_flag(self, self._set_final_pushed_done, clear=self._clr_final_pushed_done,
                                              name="final_pushed_done_sticky", seq_only=True)

        # crd_res_fifo_done = self._crd_res_fifo_data_out[16] & (self._crd_res_fifo_data_out[self.OPCODE_BT] == self.DONE_CODE)
        # crd_res_fifo_done = self._crd_res_fifo_data_out[16] & (self._crd_res_fifo_data_out[self.OPCODE_BT] == self.STOP_CODE) & (self._crd_res_fifo_data_out[self.STOP_BT] >= (self._stop_lvl + 1))
        crd_res_fifo_done = kts.ternary(self._pushed_done,
                                        self._crd_res_fifo_data_out[16] & (self._crd_res_fifo_data_out[self.OPCODE_BT] == self.DONE_CODE),
                                        self._crd_res_fifo_data_out[16] & (self._crd_res_fifo_data_out[self.OPCODE_BT] == self.STOP_CODE) &
                                            # (self._crd_res_fifo_data_out[self.STOP_BT] >= (self._last_stop_token - 1)))
                                            kts.ternary(self._final_pushed_done,
                                                        (self._crd_res_fifo_data_out[self.STOP_BT] >= (self._stop_lvl)),
                                                        (self._crd_res_fifo_data_out[self.STOP_BT] >= (self._stop_lvl + 1))))

        crd_res_routing_token = self._crd_res_fifo_data_out[self.EOS_BIT] & self._crd_res_fifo_valid & (self._crd_res_fifo_data_out[self.OPCODE_BT] == 3)

        # self.wire(self._set_final_pushed_done, self._crd_res_fifo_valid & ~self._block_mode & crd_res_fifo_done & self._spacc_mode & ~self._coord_fifo.ports.full)
        self.wire(self._set_final_pushed_done, self._crd_res_fifo_valid & ~self._block_mode & crd_res_routing_token & self._spacc_mode & self._crd_res_fifo_data_out[0])
        # self.wire(self._clr_final_pushed_done, self._clr_readout_loop_crd | self._clr_readout_loop_seg)
        # Once we pushed a done to the readout block we clear it
        # self.wire(self._clr_final_pushed_done, self._crd_res_fifo_valid & ~self._block_mode & crd_res_fifo_done & self._spacc_mode & ~self._block_rd_fifo.ports.full &
        #                                             self._final_pushed_done)
        self.wire(self._clr_final_pushed_done, self._crd_res_fifo_valid & ~self._block_mode & crd_res_routing_token & self._spacc_mode & ~self._crd_res_fifo_data_out[0])

        self.add_child(f"coordinate_fifo",
                       self._coord_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                    #    push=self._crd_res_fifo_valid & ~self._block_mode & ~(self._spacc_mode & self._readout_loop & self._final_pushed_done),
                       push=self._crd_res_fifo_valid & ~self._block_mode & ~(self._spacc_mode & self._final_pushed_done) & ~crd_res_routing_token,
                    #    push=self._crd_res_fifo_valid & ~self._block_mode & (self._readout_dst_out == 0),
                       pop=self._coord_out_ready_in,
                       data_in=self._coord_data_in_packed,
                       data_out=self._coord_data_out_packed)

        self.wire(self._coord_out_valid_out, ~self._coord_fifo.ports.empty)
        self.wire(self._fifo_full_pre[0], self._coord_fifo.ports.full)

        # self.wire(self._crd_res_fifo_pop, ~self._coord_fifo.ports.full)
        # Can pop the crd res fifo if we are reading out normally, in block mode
        # if there is room in the block fifo, or
        # self.wire(self._crd_res_fifo_pop, kts.ternary(self._block_mode,
        #                                               ~self._block_rd_fifo.ports.full,
        #                                               kts.ternary(self._spacc_mode,
        #                                                           kts.ternary(self._final_pushed_done,
        #                                                                       ~self._block_rd_fifo.ports.full,
        #                                                                       ~self._coord_fifo.ports.full),
        #                                                           ~self._coord_fifo.ports.full)))
        self.wire(self._crd_res_fifo_pop, kts.ternary(self._block_mode,
                                                      ~self._block_rd_fifo.ports.full,
                                                      kts.ternary(self._spacc_mode,
                                                                #   kts.ternary(self._final_pushed_done,
                                                                  kts.ternary(crd_res_routing_token,
                                                                              kts.const(1, 1),
                                                                              kts.ternary(self._final_pushed_done,
                                                                                          ~self._block_rd_fifo.ports.full,
                                                                                          ~self._coord_fifo.ports.full)),
                                                                  ~self._coord_fifo.ports.full)))

        ### POS FIFO
        self._pos_data_in_packed = self.var("pos_fifo_in_packed", self.data_width + 1, packed=True)
        # self.wire(self._pos_data_in_packed[self.data_width], self._tag_eos)
        # self.wire(self._pos_data_in_packed[self.data_width - 1, 0], self._pos_out_to_fifo)
        self.wire(self._pos_data_in_packed, self._pos_out_to_fifo)

        self._pos_data_out_packed = self.var("pos_fifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._pos_out[self.data_width], self._pos_data_out_packed[self.data_width])
        self.wire(self._pos_out[self.data_width - 1, 0], self._pos_data_out_packed[self.data_width - 1, 0])

        self.add_child(f"pos_fifo",
                       self._pos_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._pos_out_fifo_push,
                       pop=self._pos_out_ready_in,
                       data_in=self._pos_data_in_packed,
                       data_out=self._pos_data_out_packed)

        self.wire(self._pos_out_valid_out, ~self._pos_fifo.ports.empty)
        self.wire(self._fifo_full_pre[1], self._pos_fifo.ports.full)

        ### Block Read FIFO
        self.add_child(f"block_rd_fifo",
                       self._block_rd_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                    #    push=self._crd_res_fifo_valid & (self._block_mode | (self._spacc_mode & self._readout_loop & self._final_pushed_done)),
                       push=self._crd_res_fifo_valid & (self._block_mode | (self._spacc_mode & self._final_pushed_done)) & ~crd_res_routing_token,
                    #    push=self._crd_res_fifo_valid & (self._block_mode | (self._readout_dst_out == 1)),
                       pop=self._block_rd_out_ready_in,
                       data_in=self._crd_res_fifo_data_out,
                       data_out=self._block_rd_out)

        self.wire(self._block_rd_out_valid_out, ~self._block_rd_fifo.ports.empty)
        # self.wire(self._fifo_full_pre[1], self._block_rd_fifo.ports.full)

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

        if self.perf_debug:

            cyc_count = add_counter(self, "clock_cycle_count", 64, increment=self._clk & self._clk_en)

            # Count up how many memory operations
            mem_request_ctr = add_counter(self, 'mem_request_num', 64,
                                          increment=self._clk_en & self._rd_rsp_fifo_pop & self._rd_rsp_fifo_valid)

            # Start when any of the coord inputs is valid
            self._start_signal = sticky_flag(self, self._upstream_valid_in | self._root,
                                             name='start_indicator')
            self.add_performance_indicator(self._start_signal, edge='posedge', label='start',
                                           cycle_count=cyc_count)

            # End when we see DONE on the output ref signal
            self._done_signal = sticky_flag(self, (self._coord_out == MemoryController.DONE_PROXY) &
                                                    self._coord_out[MemoryController.EOS_BIT] & self._coord_out_valid_out,
                                                    name='done_indicator')
            # self._done_signal = sticky_flag(self, (self._coord_out == MemoryController.DONE_PROXY) &
            #                                         self._coord_out[MemoryController.EOS_BIT] & self._coord_out_valid_out,
            #                                         name='done_indicator')
            self.add_performance_indicator(self._done_signal, edge='posedge', label='done',
                                           cycle_count=cyc_count)
            self.add_performance_indicator(self._done_signal, edge='posedge', label='ops',
                                           cycle_count=mem_request_ctr)

        if self.lift_config:
            # Finally, lift the config regs...
            lift_config_reg(self.internal_generator)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "read_scanner"

    # def get_bitstream(self, inner_offset, max_out, ranges, strides, root, do_repeat=0, repeat_outer=0, repeat_factor=0, stop_lvl=0, block_mode=0, lookup=0):
    def get_bitstream(self, config_kwargs):

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        inner_offset = config_kwargs['inner_offset']
        max_out = config_kwargs['max_out']
        ranges = config_kwargs['ranges']
        strides = config_kwargs['strides']
        root = config_kwargs['root']
        do_repeat = config_kwargs['do_repeat']
        repeat_outer = config_kwargs['repeat_outer']
        repeat_factor = config_kwargs['repeat_factor']
        stop_lvl = config_kwargs['stop_lvl']
        block_mode = config_kwargs['block_mode']
        lookup = config_kwargs['lookup']
        dense = config_kwargs['dense']
        dim_size = config_kwargs['dim_size']
        spacc_mode = config_kwargs['spacc_mode']

        # Store all configurations here
        config = [
            ("inner_dim_offset", inner_offset),
            # ("max_outer_dim", max_out),
            ("do_repeat", do_repeat),
            ("repeat_outer_inner_n", repeat_outer),
            ("repeat_factor", repeat_factor),
            ("stop_lvl", stop_lvl),
            ("block_mode", block_mode),
            ('lookup', lookup),
            ('root', root),
            ('dense', dense),
            ('dim_size', dim_size),
            ('spacc_mode', spacc_mode),
            ('tile_en', 1)]

        if root:
            dim = len(ranges)
            tform_ranges, tform_strides = transform_strides_and_ranges(ranges=ranges,
                                                                       strides=strides,
                                                                       dimensionality=dim)
            # for i in range(dim):
            #     config += [("fiber_outer_iter_dimensionality", dim)]
            #     config += [(f"fiber_outer_iter_ranges_{i}", tform_ranges[i])]
            #     config += [(f"fiber_outer_addr_strides_{i}", tform_strides[i])]
            #     config += [("fiber_outer_addr_starting_addr", 0)]

        return trim_config_list(flattened, config)


if __name__ == "__main__":
    scanner_dut = ScannerPipe(data_width=16,
                              defer_fifos=False,
                              lift_config=True,
                              add_flush=True,
                              add_clk_enable=True,
                              perf_debug=True)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(scanner_dut, filename="scanner_pipe.sv",
            optimize_if=False)
