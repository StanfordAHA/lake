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
                 defer_fifos=True):

        self.data_width = data_width
        self.add_clk_enable = add_clk_enable
        self.add_flush = add_flush
        self.fifo_depth = fifo_depth
        self.lift_config = lift_config
        self.defer_fifos = defer_fifos

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
        self._stop_lvl.add_attribute(ConfigRegAttr("What level stop tokens should this scanner inject"))

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

        self.wire(self._rd_rsp_fifo_pop, self._seg_rd_rsp_fifo_pop | self._crd_rd_rsp_fifo_pop)

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

        self.port_arbiter = Arbiter(ins=2,
                                    algo="RR")

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

        self._crd_res_fifo = ReservationFIFO(depth=8, data_width=self.data_width + 1, num_per=1)

        self._crd_res_fifo_data_out = self.var("crd_res_fifo_data_out", (self.data_width + 1) * 1, packed=True)
        self._crd_res_fifo_push_alloc = self.var("crd_res_fifo_push_alloc", 1)
        self._crd_res_fifo_push_reserve = self.var("crd_res_fifo_push_reserve", 1)
        self._crd_res_fifo_push_fill = self.var("crd_res_fifo_push_fill", 1)
        self._crd_res_fifo_pop = self.var("crd_res_fifo_pop", 1)
        self._crd_res_fifo_valid = self.var("crd_res_fifo_valid", 1)
        self._crd_res_fifo_full = self.var("crd_res_fifo_full", 1)

        # Technically if you reserved something it will always be ready to receive
        # data for the reserved spot by design
        self.add_child("crd_res_fifo",
                       self._crd_res_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       data_in_0=kts.concat(kts.const(0, 1), self._rd_rsp_fifo_out_data[self.data_width - 1, 0]),
                       fill_data_in=kts.ternary(self._lookup_mode, self._seg_res_fifo_fill_data_in, self._seg_res_fifo_data_out[0]),
                       data_out_0=self._crd_res_fifo_data_out,
                       push_alloc=kts.ternary(self._lookup_mode, self._seg_res_fifo_push_alloc, self._crd_res_fifo_push_alloc),
                       push_reserve=self._rd_rsp_fifo_valid & ((self._rd_rsp_fifo_out_data[self.data_width] == kts.const(1, 1)) | self._block_mode | self._lookup_mode),
                       push_fill=kts.ternary(self._lookup_mode, self._seg_res_fifo_push_fill, self._crd_res_fifo_push_fill),
                       pop=self._crd_res_fifo_pop,
                       valid=self._crd_res_fifo_valid,
                       full=self._crd_res_fifo_full)

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
        self._pos_out_to_fifo = self.var("pos_out_to_fifo", self.data_width + 1)
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

        # self._fifo_push = self.var("fifo_push", 1)
        self._coord_out_fifo_push = self.var("coord_out_fifo_push", 1)
        self._pos_out_fifo_push = self.var("pos_out_fifo_push", 1)

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
        INJECT_0 = self.scan_fsm_seg.add_state("INJECT_0")
        INJECT_DONE = self.scan_fsm_seg.add_state("INJECT_DONE")
        READ = self.scan_fsm_seg.add_state("READ")
        READ_ALT = self.scan_fsm_seg.add_state("READ_ALT")
        LOOKUP = self.scan_fsm_seg.add_state("LOOKUP")
        PASS_STOP = self.scan_fsm_seg.add_state("PASS_STOP_SEG")
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

    # Dummy state for eventual filling block.
        # Sit in start if in block mode - everything else handled by CRD FSM
        START_SEG.next(START_SEG, self._block_mode)
        START_SEG.next(READ, ~self._root & ~self._lookup_mode & ~self._block_mode)
        START_SEG.next(INJECT_0, self._root & ~self._lookup_mode & ~self._block_mode)
        START_SEG.next(LOOKUP, ~self._root & self._lookup_mode & ~self._block_mode)

        # Inject a single value into the fifo
        INJECT_0.next(INJECT_DONE, None)

        # Inject a single done into the fifo
        INJECT_DONE.next(READ, None)

        # From READ - we have one option
        # 1. Issue read to first address in seg pair and reserve room in the reservation FIFO and
        # Pop this thing off so we can have the next data or the EOS token as soon as possible
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
        LOOKUP.next(FREE_SEG, self._done_in & ~self._crd_res_fifo_full)
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
        PASS_STOP.next(READ, self._infifo_valid_in & ~self._lookup_mode & ~self._seg_res_fifo_full)
        PASS_STOP.next(PASS_STOP, None)
        # PASS_STOP.next(FREE_SEG, self._done_in & ~self._fifo_full)
        # PASS_STOP.next(LOOKUP, (~self._infifo_eos_in & self._infifo_valid_in) & ~self._done_in & self._lookup_mode)
        # PASS_STOP.next(PASS_STOP, None)

        # Free the ID 0 data structure
        FREE_SEG.next(DONE_SEG, self._seg_grant_push)

        # DONE_SEG.next(START_SEG, None)
        DONE_SEG.next(DONE_SEG, None)

        #######
        # START_SEG
        #######
        START_SEG.output(self._seg_addr_out_to_fifo, 0)
        START_SEG.output(self._seg_op_out_to_fifo, 0)
        START_SEG.output(self._seg_ID_out_to_fifo, 0)
        START_SEG.output(self._seg_req_push, 0)
        START_SEG.output(self._seg_rd_rsp_fifo_pop, 0)
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
        # READ
        #######
        READ.output(self._seg_addr_out_to_fifo, self._infifo_pos_in)
        READ.output(self._seg_op_out_to_fifo, 1)
        READ.output(self._seg_ID_out_to_fifo, 0)
        # Only request a push when there's valid, non-eos data on the fifo
        READ.output(self._seg_req_push, self._infifo_valid_in & ~self._infifo_eos_in & ~self._seg_res_fifo_full)
        READ.output(self._seg_rd_rsp_fifo_pop, 1)
        READ.output(self._seg_pop_infifo, self._done_in & ~self._seg_res_fifo_full)
        READ.output(self._inc_req_made_seg, 0)
        READ.output(self._clr_req_made_seg, 0)
        READ.output(self._inc_req_rec_seg, 0)
        READ.output(self._clr_req_rec_seg, 0)
        READ.output(self._us_fifo_inject_data, 0)
        READ.output(self._us_fifo_inject_eos, 0)
        READ.output(self._us_fifo_inject_push, 0)
        READ.output(self._seg_res_fifo_push_alloc, kts.ternary(self._done_in,
                                                               ~self._seg_res_fifo_full,
                                                               ~self._seg_res_fifo_full & self._seg_grant_push))
        READ.output(self._seg_res_fifo_push_fill, self._done_in & ~self._seg_res_fifo_full)
        READ.output(self._seg_res_fifo_fill_data_in, kts.concat(self._infifo_eos_in, self._infifo_pos_in))
        # READ.output(self._seg_rd_rsp_fifo_pop, self._rd_rsp_fifo_valid & (self._rd_rsp_fifo_out_data[self.data_width] == kts.const(0, 1)))

        #######
        # READ_ALT
        #######
        READ_ALT.output(self._seg_addr_out_to_fifo, self._infifo_pos_in + 1)
        READ_ALT.output(self._seg_op_out_to_fifo, 1)
        READ_ALT.output(self._seg_ID_out_to_fifo, 0)
        # READ_ALT.output(self._seg_req_push, self._infifo_valid_in & ~self._infifo_eos_in & ~self._seg_res_fifo_full)
        READ_ALT.output(self._seg_req_push, ~self._seg_res_fifo_full)
        READ_ALT.output(self._seg_rd_rsp_fifo_pop, 1)
        READ_ALT.output(self._seg_pop_infifo, self._seg_grant_push & ~self._seg_res_fifo_full)
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
        LOOKUP.output(self._seg_req_push, self._infifo_valid_in & ~self._infifo_eos_in)
        LOOKUP.output(self._seg_rd_rsp_fifo_pop, 1)
        # LOOKUP.output(self._seg_pop_infifo, self._infifo_valid_in & ~self._infifo_eos_in & ~self._seg_res_fifo_full & self._seg_grant_push)
        # Pop the infifo if there's room and it's valid
        LOOKUP.output(self._seg_pop_infifo, self._infifo_valid_in & ~self._crd_res_fifo_full)
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
        LOOKUP.output(self._seg_res_fifo_fill_data_in, kts.concat(self._infifo_eos_in, self._infifo_pos_in))

        #######
        # PASS_STOP
        #######
        PASS_STOP.output(self._seg_addr_out_to_fifo, 0)
        PASS_STOP.output(self._seg_op_out_to_fifo, 0)
        PASS_STOP.output(self._seg_ID_out_to_fifo, 0)
        PASS_STOP.output(self._seg_req_push, 0)
        PASS_STOP.output(self._seg_rd_rsp_fifo_pop, 1)
        PASS_STOP.output(self._seg_pop_infifo, ~self._seg_res_fifo_full & self._eos_in)
        PASS_STOP.output(self._inc_req_made_seg, 0)
        PASS_STOP.output(self._clr_req_made_seg, 1)
        PASS_STOP.output(self._inc_req_rec_seg, 0)
        PASS_STOP.output(self._clr_req_rec_seg, 1)
        PASS_STOP.output(self._us_fifo_inject_data, 0)
        PASS_STOP.output(self._us_fifo_inject_eos, 0)
        PASS_STOP.output(self._us_fifo_inject_push, 0)
        # PASS_STOP.output(self._seg_res_fifo_push_alloc, self._infifo_valid_in & self._infifo_eos_in)
        PASS_STOP.output(self._seg_res_fifo_push_alloc, self._infifo_valid_in & ~self._seg_res_fifo_full)
        # PASS_STOP.output(self._seg_res_fifo_push_fill, self._infifo_valid_in & self._infifo_eos_in)
        PASS_STOP.output(self._seg_res_fifo_push_fill, self._infifo_valid_in & ~self._seg_res_fifo_full)
        # In the pass stop state, we either shove the eos in or add to it
        PASS_STOP.output(self._seg_res_fifo_fill_data_in, kts.ternary(self._eos_in,
                                                                      kts.concat(kts.const(1, 1), (self._infifo_pos_in + 1)),
                                                                      kts.concat(kts.const(1, 1), kts.const(0, self.data_width))))

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
        self.scan_fsm_crd.output(self._inc_req_made_crd)
        self.scan_fsm_crd.output(self._clr_req_made_crd)
        self.scan_fsm_crd.output(self._inc_req_rec_crd)
        self.scan_fsm_crd.output(self._clr_req_rec_crd)
        self.scan_fsm_crd.output(self._crd_res_fifo_push_alloc)
        self.scan_fsm_crd.output(self._crd_res_fifo_push_fill)
        self.scan_fsm_crd.output(self._ptr_reg_en)
        self.scan_fsm_crd.output(self._seg_res_fifo_pop)
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

        START_CRD.next(BLOCK_1_SIZE_REQ, self._block_mode)
        START_CRD.next(DENSE_STRM, self._dense)
        START_CRD.next(SEQ_STRM, ~self._dense)

        # DENSE_STRM = self.scan_fsm_crd.add_state("DENSE_STRM")
        DENSE_STRM.next(SEQ_STRM, self._num_req_made_crd == self._dim_size)
        DENSE_STRM.next(DENSE_STRM, None)

        # SEQ_STRM.next(FREE_CRD, self._done_in & ~self._fifo_full)
        # only leave SEQ_STRM if we see done
        SEQ_STRM.next(FREE_CRD, self._seg_res_fifo_valid & self._seg_res_fifo_data_out[0][self.data_width] &
                      (self._seg_res_fifo_data_out[0][9, 8] == kts.const(1, 2)) & ~self._crd_res_fifo_full & ~self._pos_fifo.ports.full)

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
        FREE_CRD.next(FREE_CRD2, self._crd_grant_push & ~self._lookup_mode)
        # FREE_CRD.next(FREE_CRD2, sel  READ = 4'h6,f._buffet_joined & ~self._lookup_mode)
        FREE_CRD.next(DONE_CRD, self._crd_grant_push & self._lookup_mode)
        # FREE_CRD.next(DONE_CRD, self._buffet_joined & self._lookup_mode)
        FREE_CRD.next(FREE_CRD, None)

        FREE_CRD2.next(DONE_CRD, self._crd_grant_push)
        # FREE_CRD2.next(DONE_CRD, self._buffet_joined)
        # FREE2.next(START, self._buffet_joined & ~self._root)
        # FREE2.next(DONE, self._buffet_joined & self._root)
        FREE_CRD2.next(FREE_CRD2, None)

        # DONE_CRD.next(START_CRD, None)
        DONE_CRD.next(DONE_CRD, None)

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
        DENSE_STRM.output(self._crd_rd_rsp_fifo_pop, 1)
        DENSE_STRM.output(self._pos_out_fifo_push, (self._num_req_made_crd < self._dim_size) & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full)
        DENSE_STRM.output(self._crd_pop_infifo, 0)
        DENSE_STRM.output(self._en_reg_data_in, 0)
        DENSE_STRM.output(self._pos_out_to_fifo, 0)
        DENSE_STRM.output(self._inc_req_made_crd, (self._num_req_made_crd < self._dim_size) & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full)
        DENSE_STRM.output(self._clr_req_made_crd, (self._num_req_made_crd == (self._dim_size - 1) & self._inc_req_made_crd))
        DENSE_STRM.output(self._inc_req_rec_crd, 0)
        DENSE_STRM.output(self._clr_req_rec_crd, 0)
        DENSE_STRM.output(self._crd_res_fifo_push_alloc, (self._num_req_made_crd < self._dim_size) & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full)
        DENSE_STRM.output(self._crd_res_fifo_push_fill, (self._num_req_made_crd < self._dim_size) & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full)
        DENSE_STRM.output(self._ptr_reg_en, 0)
        DENSE_STRM.output(self._seg_res_fifo_pop, self._clr_req_made_crd)

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
        SEQ_STRM.output(self._inc_req_made_crd, self._crd_grant_push & (self._num_req_made_crd < self._seq_length_ptr_math) & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid)
        SEQ_STRM.output(self._clr_req_made_crd, self._crd_grant_push & (self._num_req_made_crd == (self._seq_length_ptr_math - 1)) & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid)
        SEQ_STRM.output(self._inc_req_rec_crd, 0)
        SEQ_STRM.output(self._clr_req_rec_crd, 0)
        SEQ_STRM.output(self._crd_res_fifo_push_alloc, kts.ternary(self._seg_res_fifo_data_out[0][self.data_width],
                                                                   ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid,
                                                                   self._crd_grant_push & (self._num_req_made_crd < self._seq_length_ptr_math) &
                                                                   ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full & self._seg_res_fifo_valid))
        SEQ_STRM.output(self._crd_res_fifo_push_fill, self._seg_res_fifo_valid & self._seg_res_fifo_data_out[0][self.data_width] & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full)
        SEQ_STRM.output(self._ptr_reg_en, 0)
        SEQ_STRM.output(self._seg_res_fifo_pop, self._clr_req_made_crd | (self._seg_res_fifo_valid & self._seg_res_fifo_data_out[0][self.data_width] & ~self._pos_fifo.ports.full & ~self._crd_res_fifo_full))

        ######################
        # FREE_CRD
        ######################
        FREE_CRD.output(self._crd_addr_out_to_fifo, 0)
        FREE_CRD.output(self._crd_op_out_to_fifo, 0)
        FREE_CRD.output(self._crd_ID_out_to_fifo, 1)
        FREE_CRD.output(self._crd_req_push, 1)
        FREE_CRD.output(self._crd_rd_rsp_fifo_pop, 0)
        FREE_CRD.output(self._pos_out_fifo_push, 0)
        FREE_CRD.output(self._crd_pop_infifo, 0)
        FREE_CRD.output(self._en_reg_data_in, 0)
        FREE_CRD.output(self._pos_out_to_fifo, 0)
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
        FREE_CRD2.output(self._crd_addr_out_to_fifo, 0)
        FREE_CRD2.output(self._crd_op_out_to_fifo, 0)
        FREE_CRD2.output(self._crd_ID_out_to_fifo, 0)
        FREE_CRD2.output(self._crd_req_push, 1)
        FREE_CRD2.output(self._crd_rd_rsp_fifo_pop, 0)
        FREE_CRD2.output(self._pos_out_fifo_push, 0)
        FREE_CRD2.output(self._crd_pop_infifo, 0)
        FREE_CRD2.output(self._en_reg_data_in, 0)
        FREE_CRD2.output(self._pos_out_to_fifo, 0)
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
        BLOCK_1_SIZE_REQ.output(self._inc_req_made_crd, 0)
        BLOCK_1_SIZE_REQ.output(self._clr_req_made_crd, 0)
        BLOCK_1_SIZE_REQ.output(self._inc_req_rec_crd, 0)
        BLOCK_1_SIZE_REQ.output(self._clr_req_rec_crd, 0)
        BLOCK_1_SIZE_REQ.output(self._crd_res_fifo_push_alloc, ~self._crd_res_fifo_full & self._crd_grant_push)
        BLOCK_1_SIZE_REQ.output(self._crd_res_fifo_push_fill, 0)
        BLOCK_1_SIZE_REQ.output(self._ptr_reg_en, 0)
        BLOCK_1_SIZE_REQ.output(self._seg_res_fifo_pop, 0)

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
        BLOCK_2_RD.output(self._inc_req_made_crd, (self._num_req_made_crd < self._ptr_reg) & self._crd_grant_push & ~self._crd_res_fifo_full)
        BLOCK_2_RD.output(self._clr_req_made_crd, 0)
        BLOCK_2_RD.output(self._inc_req_rec_crd, (self._num_req_rec_crd < self._ptr_reg) & self._rd_rsp_fifo_valid)
        BLOCK_2_RD.output(self._clr_req_rec_crd, 0)
        BLOCK_2_RD.output(self._crd_res_fifo_push_alloc, (self._num_req_made_crd < self._ptr_reg) & self._crd_grant_push & ~self._crd_res_fifo_full)
        BLOCK_2_RD.output(self._crd_res_fifo_push_fill, 0)
        BLOCK_2_RD.output(self._ptr_reg_en, 0)
        BLOCK_2_RD.output(self._seg_res_fifo_pop, 0)

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
        DONE_CRD.output(self._inc_req_made_crd, 0)
        DONE_CRD.output(self._clr_req_made_crd, 1)
        DONE_CRD.output(self._inc_req_rec_crd, 0)
        DONE_CRD.output(self._clr_req_rec_crd, 1)
        DONE_CRD.output(self._crd_res_fifo_push_alloc, 0)
        DONE_CRD.output(self._crd_res_fifo_push_fill, 0)
        DONE_CRD.output(self._ptr_reg_en, 0)
        DONE_CRD.output(self._seg_res_fifo_pop, 0)

        self.scan_fsm_crd.set_start_state(START_CRD)

        # # Dummy state for eventual filling block.
        # START.next(BLOCK_1_SIZE_REQ, self._block_mode & ~self._lookup_mode)
        # START.next(LOOKUP, self._lookup_mode)
        # # START.next(ISSUE_STRM, self._root & ~self._lookup_mode)
        # START.next(INJECT_0, self._root & ~self._lookup_mode)
        # START.next(ISSUE_STRM_NR, ~self._root & ~self._lookup_mode)

        # # In lookup we pass the address along to the buffet, making sure all reads complete and end up in
        # # the output buffer before passing along stop tokens...
        # LOOKUP.next(PASS_STOP, self._infifo_eos_in & self._infifo_valid_in & (self._num_req_made == self._num_req_rec))
        # LOOKUP.next(LOOKUP, None)

        # # Completely done at this point
        # # ISSUE_STRM.next(DONE, self._out_dim_x == self._max_outer_dim)
        # # If not done, we have to issue more streams
        # # ISSUE_STRM.next(SEQ_START, (self._outer_addr == self._previous_outer) & self._previous_outer_valid)
        # # ISSUE_STRM.next(READ_0, ~((self._outer_addr == self._previous_outer) & self._previous_outer_valid))
        # # ISSUE_STRM.next(READ_0, None)

        # # Inject a single value into the fifo
        # INJECT_0.next(INJECT_DONE, ~self._fifo_us_full)
        # INJECT_0.next(INJECT_0, None)

        # # Inject a single done into the fifo
        # INJECT_DONE.next(ISSUE_STRM_NR, ~self._fifo_us_full)
        # INJECT_DONE.next(INJECT_DONE, None)

        # # If we are seeing the eos_in this state we need to pass them along to downstream modules
        # # If we have valid data in (the input fifo is not empty), then we should issue the corresponding stream
        # # We should have seen all squashable STOP tokens, now we are seeing if it is
        # ISSUE_STRM_NR.next(PASS_STOP, self._done_in)
        # ISSUE_STRM_NR.next(READ_0, ~self._infifo_eos_in & self._infifo_valid_in & ~self._dense)
        # ISSUE_STRM_NR.next(DENSE_STRM, ~self._infifo_eos_in & self._infifo_valid_in & self._dense)
        # ISSUE_STRM_NR.next(SEQ_DONE, self._infifo_eos_in & self._infifo_valid_in & (self._infifo_pos_in[9, 8] == kts.const(2, 2)))
        # ISSUE_STRM_NR.next(ISSUE_STRM_NR, None)

        # # In this state, we are passing through stop tokens into
        # # the downstream
        # # Go to free if we see root eos since we are application done
        # # We need to make sure we only transition if there was somewhere to put an item
        # PASS_STOP.next(FREE1, self._done_in & ~self._fifo_full)
        # # Otherwise, we are waiting to see a valid in without eos (since we should see one eventually) (not pushing in these cases, don't care about fifo full)
        # # PASS_STOP.next(ISSUE_STRM_NR, (~self._infifo_eos_in & self._infifo_valid_in) & ~self._seen_root_eos & ~self._lookup_mode)
        # PASS_STOP.next(LOOKUP, (~self._infifo_eos_in & self._infifo_valid_in) & ~self._done_in & self._lookup_mode)
        # # PASS_STOP.next(ISSUE_STRM_NR, (~self._infifo_eos_in & self._infifo_valid_in) & ~self._seen_root_eos & ~self._lookup_mode)
        # # PASS_STOP.next(LOOKUP, (~self._infifo_eos_in & self._infifo_valid_in) & ~self._seen_root_eos & self._lookup_mode)
        # # PASS_STOP.next(FREE1, (~self._infifo_eos_in & self._infifo_valid_in) & self._seen_root_eos)
        # PASS_STOP.next(PASS_STOP, None)

        # # Can continue on our way in root mode, otherwise we need toSEQ_START
        # # bring the pointer up to locate the coordinate. Make sure the first read is emmitted
        # # Except if we get a maybe input token - then we need to skip the emission of the sequence
        # # READ_0.next(SEQ_DONE, self._infifo_valid_in & self._infifo_eos_in & (self._infifo_pos_in[9, 8] == kts.const(2, 2)))
        # READ_0.next(READ_1, self._buffet_joined)
        # READ_0.next(READ_0, None)

        # # If you're not at the right coordinate yet, go straight to SEQ_START and emit length 0 sequence
        # # READ_1.next(SEQ_START, (self._coord_in > self._outer_addr) & self._root)
        # # # Otherwise, proceed
        # # READ_1.next(READ_2, (self._coord_in <= self._outer_addr) & self._root)
        # # READ_1.next(READ_2, self._root)

        # # # If not root, we should go back to READ_0 while the input coordinate is smaller than the fifo coord
        # # READ_1.next(READ_0, (self._coord_in < self._infifo_coord_in) & ~self._root)
        # # # Otherwise, proceed
        # # READ_1.next(READ_2, (self._coord_in >= self._infifo_coord_in) & ~self._root)
        # READ_1.next(READ_2, self._buffet_joined & self._rd_rsp_fifo_valid)
        # READ_1.next(READ_1, None)

        # # Go to this state to see the length, will also have EOS?
        # READ_2.next(SEQ_START, self._rd_rsp_fifo_valid)
        # READ_2.next(READ_2, None)

        # # At Start, we can either immediately end the stream or go to the iteration phase
        # SEQ_START.next(SEQ_DONE, self._seq_length == kts.const(2 ** 16 - 1, 16))
        # SEQ_START.next(SEQ_ITER, None)

        # # In ITER, we go back to idle when the fifo is full to avoid
        # # complexity, or if we are looking at one of the eos since we can make the last
        # # move for the intersection now...
        # # If we have eos and can push to the fifo, we are done
        # # SEQ_ITER.next(SEQ_DONE, self._last_valid_accepting)
        # # SEQ_ITER.next(REP_INNER_PRE, self._root & self._do_repeat & ~self._repeat_outer_inner_n)
        # # SEQ_ITER.next(REP_OUTER, (self._root & self._do_repeat & self._repeat_outer_inner_n) & self._iter_finish)
        # # SEQ_ITER.next(REP_OUTER, (self._root & self._do_repeat & self._repeat_outer_inner_n) & (self._num_req_rec == self._seq_length))
        # # SEQ_ITER.next(SEQ_DONE, self._iter_finish)
        # SEQ_ITER.next(SEQ_DONE, (self._num_req_rec == self._seq_length))
        # SEQ_ITER.next(SEQ_ITER, None)

        # # Basically just use this state instead of sparse reads/streams - use num_req_made as number of pushes to output
        # DENSE_STRM.next(SEQ_DONE, self._num_req_made == self._dim_size)
        # DENSE_STRM.next(DENSE_STRM, None)

        # # Once done with the sequence, we should wait to find out if we need to squash together
        # # another stop token or not.
        # # SEQ_DONE.next(ISSUE_STRM_NR, ~self._fifo_full & self._infifo_valid_in & ~self._eos_in)
        # SEQ_DONE.next(SEQ_STOP, kts.const(1, 1))
        # SEQ_DONE.next(SEQ_DONE, None)

        # # Can go back once we confirm that it's either not eos, or there's room to inject the eos
        # # SEQ_STOP.next(ISSUE_STRM_NR, self._infifo_valid_in & ((self._eos_in & ~self._fifo_full) | ~self._eos_in))
        # SEQ_STOP.next(ISSUE_STRM_NR, self._infifo_valid_in & ~self._fifo_full)
        # SEQ_STOP.next(SEQ_STOP, None)

        # # Now handle the repetition states...

        # # Rep inner pre is used to register the read from memory...can probably optimize this away
        # # TODO: Optimize this state away...
        # # REP_INNER_PRE.next(REP_INNER, None)

        # # From rep inner, keep emitting the same data until the reps are finished
        # # REP_INNER.next(REP_STOP, self._rep_finish)
        # # REP_INNER.next(REP_INNER, None)

        # # This state might be unnecessary, but from rep outer, we should inject another STOP
        # # REP_OUTER.next(REP_STOP, None)

        # # From the stop injection we can either 1. go back to the iterator for the repeat inner case
        # # if there is more to do (iter has not finished yet) or go to seq done if it is or...
        # # 2. go to the start of the sequence again, clearing state similar to the transition from R2 to seq start
        # # Fall through to stay here until the token gets pushed into the FIFO
        # # Can only move on if the fifo is not full...
        # # REP_STOP.next(SEQ_ITER, ~self._iter_finish & ~self._repeat_outer_inner_n & ~self._fifo_full)
        # # REP_STOP.next(SEQ_DONE, self._iter_finish & ~self._repeat_outer_inner_n & ~self._fifo_full)
        # # TODO: Add assertion the iter_finish is high if in outer repeat
        # # REP_STOP.next(SEQ_START, self._repeat_outer_inner_n & ~self._rep_finish & ~self._fifo_full)
        # # REP_STOP.next(SEQ_DONE, self._repeat_outer_inner_n & self._rep_finish & ~self._fifo_full)
        # # REP_STOP.next(REP_STOP, None)

        # # BLOCK_1_SIZE_REQ
        # BLOCK_1_SIZE_REQ.next(BLOCK_1_SIZE_REC, self._buffet_joined)
        # BLOCK_1_SIZE_REQ.next(BLOCK_1_SIZE_REQ, None)

        # # BLOCK_1_SIZE_REC
        # # Push the size on the line...
        # BLOCK_1_SIZE_REC.next(BLOCK_1_RD, self._buffet_joined & ~self._fifo_full & self._rd_rsp_fifo_valid)
        # BLOCK_1_SIZE_REC.next(BLOCK_1_SIZE_REC, None)

        # BLOCK_1_RD.next(BLOCK_2_SIZE_REQ, (self._num_req_rec == self._ptr_reg) & ~self._lookup_mode)
        # BLOCK_1_RD.next(FREE1, (self._num_req_rec == self._ptr_reg) & self._lookup_mode)
        # BLOCK_1_RD.next(BLOCK_1_RD, None)

        # # BLOCK_2_SIZE_REQ
        # BLOCK_2_SIZE_REQ.next(BLOCK_2_SIZE_REC, self._buffet_joined)
        # BLOCK_2_SIZE_REQ.next(BLOCK_2_SIZE_REQ, None)

        # # BLOCK_2_SIZE_REC
        # # Push the size on the line...
        # BLOCK_2_SIZE_REC.next(BLOCK_2_RD, self._buffet_joined & ~self._fifo_full & self._rd_rsp_fifo_valid)
        # BLOCK_2_SIZE_REC.next(BLOCK_2_SIZE_REC, None)

        # BLOCK_2_RD.next(FREE1, self._num_req_rec == self._ptr_reg)
        # BLOCK_2_RD.next(BLOCK_2_RD, None)

        # # DONE
        # # Go to START after sending a free?
        # FREE1.next(FREE2, self._buffet_joined & ~self._lookup_mode)
        # FREE1.next(START, self._buffet_joined & self._lookup_mode)
        # FREE1.next(FREE1, None)

        # FREE2.next(START, self._buffet_joined)
        # # FREE2.next(START, self._buffet_joined & ~self._root)
        # # FREE2.next(DONE, self._buffet_joined & self._root)
        # FREE2.next(FREE2, None)

        # DONE.next(DONE, None)

        # ####################
        # # FSM Output Logic
        # ####################

        # #######
        # # START - TODO - Generate general hardware...
        # #######
        # START.output(self._addr_out_to_fifo, 0)
        # START.output(self._op_out_to_fifo, 0)
        # START.output(self._ID_out_to_fifo, 0)
        # START.output(self._buffet_push, 0)
        # START.output(self._rd_rsp_fifo_pop, 0)
        # START.output(self._ptr_reg_en, 0)

        # START.output(self._valid_inc, 0)
        # START.output(self._valid_rst, 0)
        # # START.output(self._ren, 0)
        # # START.output(self._fifo_push, 0)
        # START.output(self._coord_out_fifo_push, 0)
        # START.output(self._pos_out_fifo_push, 0)
        # START.output(self._tag_eos, 0)
        # # START.output(self._addr_out_to_fifo, kts.const(0, 16))
        # START.output(self._next_seq_length, kts.const(0, 16))
        # START.output(self._update_seq_state, 0)
        # START.output(self._last_valid_accepting, 0)
        # START.output(self._pop_infifo, 0)
        # START.output(self._inc_fiber_addr, 0)
        # START.output(self._clr_fiber_addr, 0)
        # START.output(self._inc_rep, 0)
        # START.output(self._clr_rep, 0)
        # START.output(self._data_to_fifo, kts.const(0, 16))
        # START.output(self._en_reg_data_in, 0)
        # START.output(self._pos_out_to_fifo, kts.const(0, 16))
        # START.output(self._inc_req_made, 0)
        # START.output(self._clr_req_made, 0)
        # START.output(self._inc_req_rec, 0)
        # START.output(self._clr_req_rec, 0)
        # START.output(self._clr_seen_root_eos, 1)

        # #######
        # # LOOKUP - TODO - Generate general hardware...
        # #######
        # LOOKUP.output(self._addr_out_to_fifo, self._infifo_pos_in)
        # LOOKUP.output(self._op_out_to_fifo, 1)
        # LOOKUP.output(self._ID_out_to_fifo, 0)
        # LOOKUP.output(self._buffet_push, self._infifo_valid_in & ~self._infifo_eos_in)
        # LOOKUP.output(self._rd_rsp_fifo_pop, ~self._fifo_full)
        # LOOKUP.output(self._ptr_reg_en, 0)

        # LOOKUP.output(self._valid_inc, 0)
        # LOOKUP.output(self._valid_rst, 0)
        # # Push to the output when we have a valid input
        # # LOOKUP.output(self._fifo_push, self._rd_rsp_fifo_valid)
        # LOOKUP.output(self._coord_out_fifo_push, self._rd_rsp_fifo_valid)
        # # LOOKUP.output(self._pos_out_fifo_push, self._buffet_push)
        # # Shouldn't use pos out in lookup
        # LOOKUP.output(self._pos_out_fifo_push, 0)
        # LOOKUP.output(self._tag_eos, 0)
        # # Only increment if we are seeing a new address and the most recent stream wasn't 0 length
        # # ISSUE_STRM.output(self._addr_out_to_fifo, kts.const(0, 16))
        # LOOKUP.output(self._next_seq_length, kts.const(0, 16))
        # LOOKUP.output(self._update_seq_state, 0)
        # LOOKUP.output(self._last_valid_accepting, 0)
        # # Pop the input fifo if there is a place to put it and there's no eos
        # LOOKUP.output(self._pop_infifo, self._buffet_joined & ~self._infifo_eos_in)
        # LOOKUP.output(self._inc_fiber_addr, 0)
        # LOOKUP.output(self._clr_fiber_addr, 0)
        # LOOKUP.output(self._inc_rep, 0)
        # LOOKUP.output(self._clr_rep, 0)
        # LOOKUP.output(self._data_to_fifo, self._rd_rsp_fifo_out_data)
        # LOOKUP.output(self._en_reg_data_in, 0)
        # # LOOKUP.output(self._pos_out_to_fifo, kts.const(0, 16))
        # LOOKUP.output(self._pos_out_to_fifo, self._infifo_pos_in)
        # LOOKUP.output(self._inc_req_made, self._buffet_push)
        # LOOKUP.output(self._clr_req_made, 0)
        # LOOKUP.output(self._inc_req_rec, self._rd_rsp_fifo_valid & ~self._fifo_full)
        # LOOKUP.output(self._clr_req_rec, 0)

        # #######
        # # ISSUE_STRM - TODO - Generate general hardware...
        # #######
        # # ISSUE_STRM.output(self._addr_out_to_fifo, 0)
        # # ISSUE_STRM.output(self._op_out_to_fifo, 0)
        # # ISSUE_STRM.output(self._ID_out_to_fifo, 0)
        # # ISSUE_STRM.output(self._buffet_push, 0)
        # # ISSUE_STRM.output(self._rd_rsp_fifo_pop, 0)
        # # ISSUE_STRM.output(self._ptr_reg_en, 0)

        # # ISSUE_STRM.output(self._valid_inc, 0)
        # # ISSUE_STRM.output(self._valid_rst, 0)
        # # # ISSUE_STRM.output(self._ren, 0)
        # # # ISSUE_STRM.output(self._fifo_push, 0)
        # # ISSUE_STRM.output(self._coord_out_fifo_push, 0)
        # # ISSUE_STRM.output(self._pos_out_fifo_push, 0)
        # # ISSUE_STRM.output(self._tag_eos, 0)
        # # # Only increment if we are seeing a new address and the most recent stream wasn't 0 length
        # # # ISSUE_STRM.output(self._addr_out_to_fifo, kts.const(0, 16))
        # # ISSUE_STRM.output(self._next_seq_length, kts.const(0, 16))
        # # ISSUE_STRM.output(self._update_seq_state, 0)
        # # ISSUE_STRM.output(self._last_valid_accepting, 0)
        # # ISSUE_STRM.output(self._pop_infifo, 0)
        # # ISSUE_STRM.output(self._inc_fiber_addr, 0)
        # # ISSUE_STRM.output(self._clr_fiber_addr, 0)
        # # ISSUE_STRM.output(self._inc_rep, 0)
        # # ISSUE_STRM.output(self._clr_rep, 0)
        # # ISSUE_STRM.output(self._data_to_fifo, kts.const(0, 16))
        # # ISSUE_STRM.output(self._en_reg_data_in, 0)
        # # ISSUE_STRM.output(self._pos_out_to_fifo, kts.const(0, 16))
        # # ISSUE_STRM.output(self._inc_req_made, 0)
        # # ISSUE_STRM.output(self._clr_req_made, 0)
        # # ISSUE_STRM.output(self._inc_req_rec, 0)
        # # ISSUE_STRM.output(self._clr_req_rec, 0)

        # #######
        # # ISSUE_STRM_NR
        # #######
        # ISSUE_STRM_NR.output(self._addr_out_to_fifo, 0)
        # ISSUE_STRM_NR.output(self._op_out_to_fifo, 0)
        # ISSUE_STRM_NR.output(self._ID_out_to_fifo, 0)
        # ISSUE_STRM_NR.output(self._buffet_push, 0)
        # ISSUE_STRM_NR.output(self._rd_rsp_fifo_pop, 0)
        # ISSUE_STRM_NR.output(self._ptr_reg_en, 0)

        # ISSUE_STRM_NR.output(self._valid_inc, 0)
        # ISSUE_STRM_NR.output(self._valid_rst, 0)
        # # ISSUE_STRM_NR.output(self._ren, 0)
        # # ISSUE_STRM_NR.output(self._fifo_push, 0)
        # ISSUE_STRM_NR.output(self._coord_out_fifo_push, 0)
        # ISSUE_STRM_NR.output(self._pos_out_fifo_push, 0)

        # ISSUE_STRM_NR.output(self._tag_eos, 0)
        # # Only increment if we are seeing a new address and the most recent stream wasn't 0 length
        # # ISSUE_STRM_NR.output(self._addr_out_to_fifo, kts.const(0, 16))
        # ISSUE_STRM_NR.output(self._next_seq_length, kts.const(0, 16))
        # ISSUE_STRM_NR.output(self._update_seq_state, 0)
        # ISSUE_STRM_NR.output(self._last_valid_accepting, 0)
        # ISSUE_STRM_NR.output(self._pop_infifo, 0)
        # # Need to clear this flag for popping at the end.
        # ISSUE_STRM_NR.output(self._clr_pop_infifo_sticky, 1)
        # ISSUE_STRM_NR.output(self._inc_fiber_addr, 0)
        # ISSUE_STRM_NR.output(self._clr_fiber_addr, 0)
        # ISSUE_STRM_NR.output(self._inc_rep, 0)
        # ISSUE_STRM_NR.output(self._clr_rep, 0)
        # ISSUE_STRM_NR.output(self._data_to_fifo, kts.const(0, 16))
        # ISSUE_STRM_NR.output(self._en_reg_data_in, 0)
        # ISSUE_STRM_NR.output(self._pos_out_to_fifo, kts.const(0, 16))
        # ISSUE_STRM_NR.output(self._inc_req_made, 0)
        # ISSUE_STRM_NR.output(self._clr_req_made, 0)
        # ISSUE_STRM_NR.output(self._inc_req_rec, 0)
        # ISSUE_STRM_NR.output(self._clr_req_rec, 0)

        # #######
        # # INJECT_0 - TODO - Generate general hardware...
        # #######
        # INJECT_0.output(self._addr_out_to_fifo, 0)
        # INJECT_0.output(self._op_out_to_fifo, 0)
        # INJECT_0.output(self._ID_out_to_fifo, 0)
        # INJECT_0.output(self._buffet_push, 0)
        # INJECT_0.output(self._rd_rsp_fifo_pop, 0)
        # INJECT_0.output(self._ptr_reg_en, 0)

        # INJECT_0.output(self._valid_inc, 0)
        # INJECT_0.output(self._valid_rst, 0)
        # # START.output(self._ren, 0)
        # # START.output(self._fifo_push, 0)
        # INJECT_0.output(self._coord_out_fifo_push, 0)
        # INJECT_0.output(self._pos_out_fifo_push, 0)
        # INJECT_0.output(self._tag_eos, 0)
        # # START.output(self._addr_out_to_fifo, kts.const(0, 16))
        # INJECT_0.output(self._next_seq_length, kts.const(0, 16))
        # INJECT_0.output(self._update_seq_state, 0)
        # INJECT_0.output(self._last_valid_accepting, 0)
        # INJECT_0.output(self._pop_infifo, 0)
        # INJECT_0.output(self._inc_fiber_addr, 0)
        # INJECT_0.output(self._clr_fiber_addr, 0)
        # INJECT_0.output(self._inc_rep, 0)
        # INJECT_0.output(self._clr_rep, 0)
        # INJECT_0.output(self._data_to_fifo, kts.const(0, 16))
        # INJECT_0.output(self._en_reg_data_in, 0)
        # INJECT_0.output(self._pos_out_to_fifo, kts.const(0, 16))
        # INJECT_0.output(self._inc_req_made, 0)
        # INJECT_0.output(self._clr_req_made, 0)
        # INJECT_0.output(self._inc_req_rec, 0)
        # INJECT_0.output(self._clr_req_rec, 0)
        # INJECT_0.output(self._clr_seen_root_eos, 1)
        # INJECT_0.output(self._us_fifo_inject_data, 0)
        # INJECT_0.output(self._us_fifo_inject_eos, 0)
        # INJECT_0.output(self._us_fifo_inject_push, 1)

        # #######
        # # INJECT_DONE - TODO - Generate general hardware...
        # #######
        # INJECT_DONE.output(self._addr_out_to_fifo, 0)
        # INJECT_DONE.output(self._op_out_to_fifo, 0)
        # INJECT_DONE.output(self._ID_out_to_fifo, 0)
        # INJECT_DONE.output(self._buffet_push, 0)
        # INJECT_DONE.output(self._rd_rsp_fifo_pop, 0)
        # INJECT_DONE.output(self._ptr_reg_en, 0)

        # INJECT_DONE.output(self._valid_inc, 0)
        # INJECT_DONE.output(self._valid_rst, 0)
        # # START.output(self._ren, 0)
        # # START.output(self._fifo_push, 0)
        # INJECT_DONE.output(self._coord_out_fifo_push, 0)
        # INJECT_DONE.output(self._pos_out_fifo_push, 0)
        # INJECT_DONE.output(self._tag_eos, 0)
        # # START.output(self._addr_out_to_fifo, kts.const(0, 16))
        # INJECT_DONE.output(self._next_seq_length, kts.const(0, 16))
        # INJECT_DONE.output(self._update_seq_state, 0)
        # INJECT_DONE.output(self._last_valid_accepting, 0)
        # INJECT_DONE.output(self._pop_infifo, 0)
        # INJECT_DONE.output(self._inc_fiber_addr, 0)
        # INJECT_DONE.output(self._clr_fiber_addr, 0)
        # INJECT_DONE.output(self._inc_rep, 0)
        # INJECT_DONE.output(self._clr_rep, 0)
        # INJECT_DONE.output(self._data_to_fifo, kts.const(0, 16))
        # INJECT_DONE.output(self._en_reg_data_in, 0)
        # INJECT_DONE.output(self._pos_out_to_fifo, kts.const(0, 16))
        # INJECT_DONE.output(self._inc_req_made, 0)
        # INJECT_DONE.output(self._clr_req_made, 0)
        # INJECT_DONE.output(self._inc_req_rec, 0)
        # INJECT_DONE.output(self._clr_req_rec, 0)
        # INJECT_DONE.output(self._clr_seen_root_eos, 1)
        # INJECT_DONE.output(self._us_fifo_inject_data, kts.const(2**8, 16))
        # INJECT_DONE.output(self._us_fifo_inject_eos, 1)
        # INJECT_DONE.output(self._us_fifo_inject_push, 1)

        # #######
        # # PASS_STOP
        # #######
        # PASS_STOP.output(self._addr_out_to_fifo, 0)
        # PASS_STOP.output(self._op_out_to_fifo, 0)
        # PASS_STOP.output(self._ID_out_to_fifo, 0)
        # PASS_STOP.output(self._buffet_push, 0)
        # PASS_STOP.output(self._rd_rsp_fifo_pop, 0)
        # PASS_STOP.output(self._ptr_reg_en, 0)

        # PASS_STOP.output(self._valid_inc, 0)
        # PASS_STOP.output(self._valid_rst, 0)
        # # PASS_STOP.output(self._ren, 0)
        # # PASS_STOP.output(self._fifo_push, self._infifo_eos_in)
        # PASS_STOP.output(self._coord_out_fifo_push, self._infifo_eos_in & self._infifo_valid_in & ~self._fifo_full)
        # # PASS_STOP.output(self._pos_out_fifo_push, self._infifo_eos_in)
        # # Only push it to pos fifo if not in lookup mode...
        # PASS_STOP.output(self._pos_out_fifo_push, self._infifo_eos_in & self._infifo_valid_in & ~self._lookup_mode & ~self._fifo_full)

        # # PASS_STOP.output(self._tag_eos, 1)
        # PASS_STOP.output(self._tag_eos, kts.ternary(self._infifo_pos_in[9, 8] == kts.const(2, 2),
        #                                             kts.const(0, 1), kts.const(1, 1)))
        # # Only increment if we are seeing a new address and the most recent stream wasn't 0 length
        # # PASS_STOP.output(self._addr_out_to_fifo, kts.const(0, 16))
        # PASS_STOP.output(self._next_seq_length, kts.const(0, 16))
        # PASS_STOP.output(self._update_seq_state, 0)
        # PASS_STOP.output(self._last_valid_accepting, 0)
        # PASS_STOP.output(self._pop_infifo, ~self._fifo_full & self._infifo_eos_in & self._infifo_valid_in)
        # PASS_STOP.output(self._inc_fiber_addr, 0)
        # PASS_STOP.output(self._clr_fiber_addr, 0)
        # PASS_STOP.output(self._inc_rep, 0)
        # PASS_STOP.output(self._clr_rep, 0)
        # # With the possibility of a maybe token, we need to pass 0 to the output instead
        # PASS_STOP.output(self._data_to_fifo, kts.ternary(self._infifo_pos_in[9, 8] == kts.const(2, 2),
        #                                                  kts.const(0, self.data_width), self._infifo_pos_in))
        # PASS_STOP.output(self._en_reg_data_in, 0)
        # PASS_STOP.output(self._pos_out_to_fifo, self._infifo_pos_in)
        # PASS_STOP.output(self._inc_req_made, 0)
        # PASS_STOP.output(self._clr_req_made, 0)
        # PASS_STOP.output(self._inc_req_rec, 0)
        # PASS_STOP.output(self._clr_req_rec, 0)

        # #######
        # # READ_0 - TODO - Generate general hardware...
        # #######
        # READ_0.output(self._addr_out_to_fifo, self._pos_addr)
        # READ_0.output(self._op_out_to_fifo, 1)
        # READ_0.output(self._ID_out_to_fifo, 0)
        # READ_0.output(self._buffet_push, 1)
        # READ_0.output(self._rd_rsp_fifo_pop, 0)
        # READ_0.output(self._ptr_reg_en, 0)

        # READ_0.output(self._valid_inc, 0)
        # READ_0.output(self._valid_rst, 0)
        # # READ_0.output(self._ren, 1)
        # # READ_0.output(self._fifo_push, 0)
        # READ_0.output(self._coord_out_fifo_push, 0)
        # READ_0.output(self._pos_out_fifo_push, 0)

        # READ_0.output(self._tag_eos, 0)
        # # READ_0.output(self._addr_out, self._out_dim_addr)
        # # READ_0.output(self._addr_out_to_fifo, self._pos_addr)
        # READ_0.output(self._next_seq_length, kts.const(0, 16))
        # READ_0.output(self._update_seq_state, 0)
        # READ_0.output(self._last_valid_accepting, 0)
        # READ_0.output(self._pop_infifo, 0)
        # READ_0.output(self._inc_fiber_addr, 0)
        # READ_0.output(self._clr_fiber_addr, 0)
        # READ_0.output(self._inc_rep, 0)
        # READ_0.output(self._clr_rep, 0)
        # READ_0.output(self._data_to_fifo, kts.const(0, 16))
        # READ_0.output(self._en_reg_data_in, 0)
        # READ_0.output(self._pos_out_to_fifo, kts.const(0, 16))
        # READ_0.output(self._inc_req_made, 0)
        # READ_0.output(self._clr_req_made, 0)
        # READ_0.output(self._inc_req_rec, 0)
        # READ_0.output(self._clr_req_rec, 0)

        # #######
        # # READ_1 - TODO - Generate general hardware...
        # #######
        # READ_1.output(self._addr_out_to_fifo, self._pos_addr + 1)
        # READ_1.output(self._op_out_to_fifo, 1)
        # READ_1.output(self._ID_out_to_fifo, 0)
        # READ_1.output(self._buffet_push, self._rd_rsp_fifo_valid)
        # READ_1.output(self._rd_rsp_fifo_pop, self._buffet_joined)
        # READ_1.output(self._ptr_reg_en, self._rd_rsp_fifo_valid & self._buffet_joined)

        # READ_1.output(self._valid_inc, 0)
        # READ_1.output(self._valid_rst, 0)
        # # READ_1.output(self._ren, 1)
        # # READ_1.output(self._fifo_push, 0)
        # READ_1.output(self._coord_out_fifo_push, 0)
        # READ_1.output(self._pos_out_fifo_push, 0)

        # READ_1.output(self._tag_eos, 0)
        # # READ_1.output(self._addr_out_to_fifo, self._pos_addr + 1)
        # READ_1.output(self._next_seq_length, kts.const(2 ** 16 - 1, 16))
        # READ_1.output(self._update_seq_state, 0)
        # READ_1.output(self._last_valid_accepting, 0)
        # READ_1.output(self._pop_infifo, 0)
        # READ_1.output(self._inc_fiber_addr, 0)
        # READ_1.output(self._clr_fiber_addr, 0)
        # READ_1.output(self._inc_rep, 0)
        # READ_1.output(self._clr_rep, 0)
        # READ_1.output(self._data_to_fifo, kts.const(0, 16))
        # READ_1.output(self._en_reg_data_in, 0)
        # READ_1.output(self._pos_out_to_fifo, kts.const(0, 16))
        # READ_1.output(self._inc_req_made, 0)
        # READ_1.output(self._clr_req_made, 0)
        # READ_1.output(self._inc_req_rec, 0)
        # READ_1.output(self._clr_req_rec, 0)

        # #######
        # # READ_2 - TODO - Generate general hardware...
        # #######
        # READ_2.output(self._addr_out_to_fifo, 0)
        # READ_2.output(self._op_out_to_fifo, 0)
        # READ_2.output(self._ID_out_to_fifo, 0)
        # READ_2.output(self._buffet_push, 0)
        # READ_2.output(self._rd_rsp_fifo_pop, 1)
        # READ_2.output(self._ptr_reg_en, 0)

        # READ_2.output(self._valid_inc, 0)
        # READ_2.output(self._valid_rst, 0)
        # # READ_2.output(self._ren, 0)
        # # READ_2.output(self._fifo_push, 0)
        # READ_2.output(self._coord_out_fifo_push, 0)
        # READ_2.output(self._pos_out_fifo_push, 0)

        # READ_2.output(self._tag_eos, 0)
        # # Don't increment here - only increment after seeing the second one
        # # READ_2.output(self._inc_out_dim_addr, 0)
        # # READ_2.output(self._addr_out_to_fifo, kts.const(0, 16))
        # READ_2.output(self._next_seq_length, self._seq_length_ptr_math)
        # READ_2.output(self._update_seq_state, self._rd_rsp_fifo_valid)
        # READ_2.output(self._last_valid_accepting, 0)
        # READ_2.output(self._pop_infifo, 0)
        # READ_2.output(self._inc_fiber_addr, 0)
        # READ_2.output(self._clr_fiber_addr, 0)
        # READ_2.output(self._inc_rep, 0)
        # READ_2.output(self._clr_rep, 0)
        # READ_2.output(self._data_to_fifo, kts.const(0, 16))
        # READ_2.output(self._en_reg_data_in, 0)
        # READ_2.output(self._pos_out_to_fifo, kts.const(0, 16))
        # READ_2.output(self._inc_req_made, 0)
        # READ_2.output(self._clr_req_made, 0)
        # READ_2.output(self._inc_req_rec, 0)
        # READ_2.output(self._clr_req_rec, 0)

        # #######
        # # SEQ_START - TODO - Generate general hardware...
        # #######
        # SEQ_START.output(self._addr_out_to_fifo, 0)
        # SEQ_START.output(self._op_out_to_fifo, 0)
        # SEQ_START.output(self._ID_out_to_fifo, 0)
        # SEQ_START.output(self._buffet_push, 0)
        # SEQ_START.output(self._rd_rsp_fifo_pop, 0)
        # SEQ_START.output(self._ptr_reg_en, 0)

        # SEQ_START.output(self._valid_rst, 0)
        # SEQ_START.output(self._valid_inc, 0)
        # # SEQ_START.output(self._ren, 0)
        # # SEQ_START.output(self._fifo_push, self._seq_length == kts.const(2 ** 16 - 1, 16))
        # SEQ_START.output(self._coord_out_fifo_push, self._seq_length == kts.const(2 ** 16 - 1, 16))
        # SEQ_START.output(self._pos_out_fifo_push, self._seq_length == kts.const(2 ** 16 - 1, 16))

        # SEQ_START.output(self._tag_eos, self._seq_length == kts.const(2 ** 16 - 1, 16))
        # # SEQ_START.output(self._addr_out_to_fifo, kts.const(0, 16))
        # SEQ_START.output(self._next_seq_length, kts.const(0, 16))
        # SEQ_START.output(self._update_seq_state, 0)
        # SEQ_START.output(self._last_valid_accepting, 0)
        # SEQ_START.output(self._pop_infifo, 0)
        # SEQ_START.output(self._inc_fiber_addr, 0)
        # SEQ_START.output(self._clr_fiber_addr, 0)
        # SEQ_START.output(self._inc_rep, 0)
        # SEQ_START.output(self._clr_rep, 0)
        # SEQ_START.output(self._data_to_fifo, kts.const(0, 16))
        # SEQ_START.output(self._en_reg_data_in, 0)
        # SEQ_START.output(self._pos_out_to_fifo, kts.const(0, 16))
        # SEQ_START.output(self._inc_req_made, 0)
        # SEQ_START.output(self._clr_req_made, 0)
        # SEQ_START.output(self._inc_req_rec, 0)
        # SEQ_START.output(self._clr_req_rec, 0)

        # #############
        # # SEQ_ITER
        # #############
        # SEQ_ITER.output(self._addr_out_to_fifo, self._fiber_addr)
        # SEQ_ITER.output(self._op_out_to_fifo, 1)
        # SEQ_ITER.output(self._ID_out_to_fifo, 1)
        # SEQ_ITER.output(self._buffet_push, self._buffet_joined & (self._num_req_made < self._seq_length) & ~self._fifo_full)
        # SEQ_ITER.output(self._rd_rsp_fifo_pop, ~self._fifo_full & (self._num_req_rec < self._seq_length))
        # SEQ_ITER.output(self._ptr_reg_en, 0)
        # # SEQ_ITER.output(self._valid_inc, self._rd_rsp_fifo_valid & (~self._fifo_full))
        # SEQ_ITER.output(self._valid_inc, 0)
        # SEQ_ITER.output(self._valid_rst, 0)
        # # SEQ_ITER.output(self._fifo_push, self._rd_rsp_fifo_valid & (~self._fifo_full) & (self._num_req_rec < self._seq_length))
        # SEQ_ITER.output(self._coord_out_fifo_push, self._rd_rsp_fifo_valid & (~self._fifo_full) & (self._num_req_rec < self._seq_length))
        # SEQ_ITER.output(self._pos_out_fifo_push, self._buffet_joined & (self._num_req_made < self._seq_length) & ~self._fifo_full)
        # SEQ_ITER.output(self._tag_eos, 0)
        # # SEQ_ITER.output(self._addr_out_to_fifo, self._fiber_addr)
        # SEQ_ITER.output(self._next_seq_length, kts.const(0, 16))
        # SEQ_ITER.output(self._update_seq_state, 0)
        # SEQ_ITER.output(self._last_valid_accepting, (self._valid_cnt == self._seq_length) & (self._rd_rsp_fifo_valid) & ~self._fifo_full)
        # SEQ_ITER.output(self._pop_infifo, 0)
        # SEQ_ITER.output(self._inc_fiber_addr, self._buffet_joined & (self._num_req_made < self._seq_length) & ~self._fifo_full)
        # SEQ_ITER.output(self._clr_fiber_addr, 0)
        # SEQ_ITER.output(self._inc_rep, 0)
        # SEQ_ITER.output(self._clr_rep, 0)
        # SEQ_ITER.output(self._data_to_fifo, self._rd_rsp_fifo_out_data)
        # SEQ_ITER.output(self._en_reg_data_in, 0)
        # # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe
        # SEQ_ITER.output(self._pos_out_to_fifo, self._fiber_addr)
        # # SEQ_ITER.output(self._pos_out_to_fifo, self._agen_addr_d1 + self._payload_ptr)
        # SEQ_ITER.output(self._inc_req_made, self._buffet_joined & (self._num_req_made < self._seq_length) & ~self._fifo_full)
        # SEQ_ITER.output(self._clr_req_made, 0)
        # SEQ_ITER.output(self._inc_req_rec, self._rd_rsp_fifo_valid & ~self._fifo_full & (self._num_req_rec < self._seq_length))
        # SEQ_ITER.output(self._clr_req_rec, 0)

        # #############
        # # DENSE_STRM
        # #############
        # DENSE_STRM.output(self._addr_out_to_fifo, 0)
        # DENSE_STRM.output(self._op_out_to_fifo, 0)
        # DENSE_STRM.output(self._ID_out_to_fifo, 0)
        # DENSE_STRM.output(self._buffet_push, 0)
        # DENSE_STRM.output(self._rd_rsp_fifo_pop, 0)
        # DENSE_STRM.output(self._ptr_reg_en, 0)
        # DENSE_STRM.output(self._valid_inc, 0)
        # DENSE_STRM.output(self._valid_rst, 0)
        # DENSE_STRM.output(self._coord_out_fifo_push, (self._num_req_made < self._dim_size) & ~self._fifo_full)
        # DENSE_STRM.output(self._pos_out_fifo_push, (self._num_req_made < self._dim_size) & ~self._fifo_full)
        # DENSE_STRM.output(self._tag_eos, 0)
        # DENSE_STRM.output(self._next_seq_length, kts.const(0, 16))
        # DENSE_STRM.output(self._update_seq_state, 0)
        # DENSE_STRM.output(self._last_valid_accepting, 0)
        # DENSE_STRM.output(self._pop_infifo, 0)
        # DENSE_STRM.output(self._inc_fiber_addr, (self._num_req_made < self._dim_size) & ~self._fifo_full)
        # DENSE_STRM.output(self._clr_fiber_addr, 0)
        # DENSE_STRM.output(self._inc_rep, 0)
        # DENSE_STRM.output(self._clr_rep, 0)
        # DENSE_STRM.output(self._en_reg_data_in, 0)
        # # We need to push any good coordinates, then push at EOS? Or do something so that EOS gets in the pipe
        # DENSE_STRM.output(self._data_to_fifo, self._fiber_addr)
        # # Translation to reference.
        # DENSE_STRM.output(self._pos_out_to_fifo, self._fiber_addr + (self._infifo_pos_in * self._dim_size))
        # DENSE_STRM.output(self._inc_req_made, (self._num_req_made < self._dim_size) & ~self._fifo_full)
        # DENSE_STRM.output(self._clr_req_made, 0)
        # DENSE_STRM.output(self._inc_req_rec, 0)
        # DENSE_STRM.output(self._clr_req_rec, 0)

        # #############
        # # SEQ_DONE
        # #############
        # SEQ_DONE.output(self._addr_out_to_fifo, 0)
        # SEQ_DONE.output(self._op_out_to_fifo, 0)
        # SEQ_DONE.output(self._ID_out_to_fifo, 0)
        # SEQ_DONE.output(self._buffet_push, 0)
        # SEQ_DONE.output(self._rd_rsp_fifo_pop, 0)
        # SEQ_DONE.output(self._ptr_reg_en, 0)

        # SEQ_DONE.output(self._valid_inc, 0)
        # SEQ_DONE.output(self._valid_rst, 1)
        # # SEQ_DONE.output(self._ren, 0)
        # # SEQ_DONE.output(self._fifo_push, 1)
        # # Only push once...don't push if either is full...the state transition will occur once not full, so we are sure it will
        # # only happen once...make sure we already popped the last issued coordinate
        # # SEQ_DONE.output(self._coord_out_fifo_push, ~self._fifo_full & self._infifo_valid_in & self._pop_infifo_sticky)
        # SEQ_DONE.output(self._coord_out_fifo_push, 0)
        # # SEQ_DONE.output(self._pos_out_fifo_push, ~self._fifo_full & self._infifo_valid_in & self._pop_infifo_sticky)
        # SEQ_DONE.output(self._pos_out_fifo_push, 0)

        # SEQ_DONE.output(self._tag_eos, 0)
        # # SEQ_DONE.output(self._addr_out_to_fifo, kts.const(0, 16))
        # SEQ_DONE.output(self._next_seq_length, kts.const(0, 16))
        # SEQ_DONE.output(self._update_seq_state, 0)
        # # SEQ_DONE.output(self._step_agen, 0)
        # SEQ_DONE.output(self._last_valid_accepting, 0)
        # # Just pop a single
        # # SEQ_DONE.output(self._pop_infifo, ~self._root & ~self._pop_infifo_sticky)
        # # Only pop the input fifo if you've discovered that it's eos in
        # # SEQ_DONE.output(self._pop_infifo, ~self._pop_infifo_sticky & self._eos_in & ~self._fifo_full)
        # # SEQ_DONE.output(self._pop_infifo, ~self._pop_infifo_sticky & self._infifo_valid_in & ~self._fifo_full)
        # SEQ_DONE.output(self._pop_infifo, 1)
        # SEQ_DONE.output(self._inc_fiber_addr, 0)
        # # Make sure to clear the fiber addr
        # SEQ_DONE.output(self._clr_fiber_addr, 1)
        # SEQ_DONE.output(self._inc_rep, 0)
        # SEQ_DONE.output(self._clr_rep, 0)
        # # Once we know if we are squashing or not, we can inject the stop token
        # SEQ_DONE.output(self._data_to_fifo, kts.const(0, self.data_width))
        # # SEQ_DONE.output(self._data_to_fifo, kts.ternary(self._eos_in, self._infifo_pos_in + 1, kts.const(0, self.data_width)))
        # SEQ_DONE.output(self._pos_out_to_fifo, kts.const(0, self.data_width))
        # # SEQ_DONE.output(self._pos_out_to_fifo, kts.ternary(self._eos_in, self._infifo_pos_in + 1, kts.const(0, self.data_width)))
        # SEQ_DONE.output(self._en_reg_data_in, 0)
        # # SEQ_DONE.output(self._step_outer, 1)
        # # SEQ_DONE.output(self._update_previous_outer, 0)
        # SEQ_DONE.output(self._inc_req_made, 0)
        # SEQ_DONE.output(self._clr_req_made, 1)
        # SEQ_DONE.output(self._inc_req_rec, 0)
        # SEQ_DONE.output(self._clr_req_rec, 1)

        # #############
        # # SEQ_STOP
        # #############
        # SEQ_STOP.output(self._addr_out_to_fifo, 0)
        # SEQ_STOP.output(self._op_out_to_fifo, 0)
        # SEQ_STOP.output(self._ID_out_to_fifo, 0)
        # SEQ_STOP.output(self._buffet_push, 0)
        # SEQ_STOP.output(self._rd_rsp_fifo_pop, 0)
        # SEQ_STOP.output(self._ptr_reg_en, 0)

        # SEQ_STOP.output(self._valid_inc, 0)
        # SEQ_STOP.output(self._valid_rst, 1)
        # # SEQ_DONE.output(self._ren, 0)
        # # SEQ_DONE.output(self._fifo_push, 1)
        # # Only push once...don't push if either is full...the state transition will occur once not full, so we are sure it will
        # # only happen once...make sure we already popped the last issued coordinate
        # # SEQ_STOP.output(self._coord_out_fifo_push, ~self._fifo_full & self._infifo_valid_in & self._pop_infifo_sticky)
        # SEQ_STOP.output(self._coord_out_fifo_push, ~self._fifo_full & self._infifo_valid_in)
        # # SEQ_STOP.output(self._pos_out_fifo_push, ~self._fifo_full & self._infifo_valid_in & self._pop_infifo_sticky)
        # SEQ_STOP.output(self._pos_out_fifo_push, ~self._fifo_full & self._infifo_valid_in)

        # SEQ_STOP.output(self._tag_eos, 1)
        # # SEQ_DONE.output(self._addr_out_to_fifo, kts.const(0, 16))
        # SEQ_STOP.output(self._next_seq_length, kts.const(0, 16))
        # SEQ_STOP.output(self._update_seq_state, 0)
        # # SEQ_DONE.output(self._step_agen, 0)
        # SEQ_STOP.output(self._last_valid_accepting, 0)
        # # Just pop a single
        # # SEQ_DONE.output(self._pop_infifo, ~self._root & ~self._pop_infifo_sticky)
        # # Only pop the input fifo if you've discovered that it's eos in
        # # SEQ_DONE.output(self._pop_infifo, ~self._pop_infifo_sticky & self._eos_in & ~self._fifo_full)
        # # SEQ_STOP.output(self._pop_infifo, ~self._pop_infifo_sticky & self._infifo_valid_in & ~self._fifo_full)
        # SEQ_STOP.output(self._pop_infifo, self._eos_in & ~self._fifo_full)
        # SEQ_STOP.output(self._inc_fiber_addr, 0)
        # # Make sure to clear the fiber addr
        # SEQ_STOP.output(self._clr_fiber_addr, 1)
        # SEQ_STOP.output(self._inc_rep, 0)
        # SEQ_STOP.output(self._clr_rep, 0)
        # # Once we know if we are squashing or not, we can inject the stop token
        # SEQ_STOP.output(self._data_to_fifo, kts.ternary(self._eos_in, self._infifo_pos_in + 1, kts.const(0, self.data_width)))
        # SEQ_STOP.output(self._pos_out_to_fifo, kts.ternary(self._eos_in, self._infifo_pos_in + 1, kts.const(0, self.data_width)))
        # SEQ_STOP.output(self._en_reg_data_in, 0)
        # # SEQ_DONE.output(self._step_outer, 1)
        # # SEQ_DONE.output(self._update_previous_outer, 0)
        # SEQ_STOP.output(self._inc_req_made, 0)
        # SEQ_STOP.output(self._clr_req_made, 1)
        # SEQ_STOP.output(self._inc_req_rec, 0)
        # SEQ_STOP.output(self._clr_req_rec, 1)

        # #############
        # # BLOCK_1_SIZE_REQ
        # #############
        # BLOCK_1_SIZE_REQ.output(self._addr_out_to_fifo, 0)
        # # Size request op
        # BLOCK_1_SIZE_REQ.output(self._op_out_to_fifo, 2)
        # BLOCK_1_SIZE_REQ.output(self._ID_out_to_fifo, 0)
        # BLOCK_1_SIZE_REQ.output(self._buffet_push, 1)
        # BLOCK_1_SIZE_REQ.output(self._rd_rsp_fifo_pop, 0)
        # BLOCK_1_SIZE_REQ.output(self._ptr_reg_en, 0)

        # BLOCK_1_SIZE_REQ.output(self._valid_inc, 0)
        # BLOCK_1_SIZE_REQ.output(self._valid_rst, 0)
        # # BLOCK_1_SIZE_REQ.output(self._fifo_push, 0)
        # BLOCK_1_SIZE_REQ.output(self._coord_out_fifo_push, 0)
        # BLOCK_1_SIZE_REQ.output(self._pos_out_fifo_push, 0)

        # BLOCK_1_SIZE_REQ.output(self._tag_eos, 0)
        # BLOCK_1_SIZE_REQ.output(self._next_seq_length, kts.const(0, 16))
        # BLOCK_1_SIZE_REQ.output(self._update_seq_state, 0)
        # BLOCK_1_SIZE_REQ.output(self._last_valid_accepting, 0)
        # BLOCK_1_SIZE_REQ.output(self._pop_infifo, 0)
        # BLOCK_1_SIZE_REQ.output(self._inc_fiber_addr, 0)
        # BLOCK_1_SIZE_REQ.output(self._clr_fiber_addr, 0)
        # BLOCK_1_SIZE_REQ.output(self._inc_rep, 0)
        # BLOCK_1_SIZE_REQ.output(self._clr_rep, 0)
        # BLOCK_1_SIZE_REQ.output(self._data_to_fifo, kts.const(0, 16))
        # BLOCK_1_SIZE_REQ.output(self._en_reg_data_in, 0)
        # BLOCK_1_SIZE_REQ.output(self._pos_out_to_fifo, kts.const(0, 16))
        # BLOCK_1_SIZE_REQ.output(self._inc_req_made, 0)
        # BLOCK_1_SIZE_REQ.output(self._clr_req_made, 0)
        # BLOCK_1_SIZE_REQ.output(self._inc_req_rec, 0)
        # BLOCK_1_SIZE_REQ.output(self._clr_req_rec, 0)

        # #############
        # # BLOCK_1_SIZE_REC
        # #############
        # BLOCK_1_SIZE_REC.output(self._addr_out_to_fifo, 0)
        # BLOCK_1_SIZE_REC.output(self._op_out_to_fifo, 0)
        # BLOCK_1_SIZE_REC.output(self._ID_out_to_fifo, 0)
        # BLOCK_1_SIZE_REC.output(self._buffet_push, 0)
        # BLOCK_1_SIZE_REC.output(self._rd_rsp_fifo_pop, ~self._fifo_full)
        # BLOCK_1_SIZE_REC.output(self._ptr_reg_en, self._rd_rsp_fifo_valid)

        # BLOCK_1_SIZE_REC.output(self._valid_inc, 0)
        # BLOCK_1_SIZE_REC.output(self._valid_rst, 0)
        # # BLOCK_1_SIZE_REC.output(self._fifo_push, self._rd_rsp_fifo_valid)
        # BLOCK_1_SIZE_REC.output(self._coord_out_fifo_push, self._rd_rsp_fifo_valid)
        # BLOCK_1_SIZE_REC.output(self._pos_out_fifo_push, 0)

        # BLOCK_1_SIZE_REC.output(self._tag_eos, 0)
        # BLOCK_1_SIZE_REC.output(self._next_seq_length, kts.const(0, 16))
        # BLOCK_1_SIZE_REC.output(self._update_seq_state, 0)
        # BLOCK_1_SIZE_REC.output(self._last_valid_accepting, 0)
        # BLOCK_1_SIZE_REC.output(self._pop_infifo, 0)
        # BLOCK_1_SIZE_REC.output(self._inc_fiber_addr, 0)
        # BLOCK_1_SIZE_REC.output(self._clr_fiber_addr, 0)
        # BLOCK_1_SIZE_REC.output(self._inc_rep, 0)
        # BLOCK_1_SIZE_REC.output(self._clr_rep, 0)
        # BLOCK_1_SIZE_REC.output(self._data_to_fifo, self._rd_rsp_fifo_out_data)
        # BLOCK_1_SIZE_REC.output(self._en_reg_data_in, 0)
        # BLOCK_1_SIZE_REC.output(self._pos_out_to_fifo, kts.const(0, 16))
        # BLOCK_1_SIZE_REC.output(self._inc_req_made, 0)
        # BLOCK_1_SIZE_REC.output(self._clr_req_made, 0)
        # BLOCK_1_SIZE_REC.output(self._inc_req_rec, 0)
        # BLOCK_1_SIZE_REC.output(self._clr_req_rec, 0)

        # #############
        # # BLOCK_1_RD
        # #############
        # BLOCK_1_RD.output(self._addr_out_to_fifo, self._num_req_made)
        # BLOCK_1_RD.output(self._op_out_to_fifo, 1)
        # BLOCK_1_RD.output(self._ID_out_to_fifo, 0)
        # BLOCK_1_RD.output(self._buffet_push, self._num_req_made < self._ptr_reg)
        # BLOCK_1_RD.output(self._rd_rsp_fifo_pop, ~self._fifo_full & (self._num_req_rec < self._ptr_reg))
        # BLOCK_1_RD.output(self._ptr_reg_en, 0)

        # BLOCK_1_RD.output(self._valid_inc, 0)
        # BLOCK_1_RD.output(self._valid_rst, 0)
        # # BLOCK_1_RD.output(self._fifo_push, self._rd_rsp_fifo_valid & (self._num_req_rec < self._ptr_reg))
        # BLOCK_1_RD.output(self._coord_out_fifo_push, self._rd_rsp_fifo_valid & (self._num_req_rec < self._ptr_reg))
        # BLOCK_1_RD.output(self._pos_out_fifo_push, 0)

        # BLOCK_1_RD.output(self._tag_eos, 0)
        # BLOCK_1_RD.output(self._next_seq_length, kts.const(0, 16))
        # BLOCK_1_RD.output(self._update_seq_state, 0)
        # BLOCK_1_RD.output(self._last_valid_accepting, 0)
        # BLOCK_1_RD.output(self._pop_infifo, 0)
        # BLOCK_1_RD.output(self._inc_fiber_addr, 0)
        # BLOCK_1_RD.output(self._clr_fiber_addr, 0)
        # BLOCK_1_RD.output(self._inc_rep, 0)
        # BLOCK_1_RD.output(self._clr_rep, 0)
        # BLOCK_1_RD.output(self._data_to_fifo, self._rd_rsp_fifo_out_data)
        # BLOCK_1_RD.output(self._en_reg_data_in, 0)
        # BLOCK_1_RD.output(self._pos_out_to_fifo, kts.const(0, 16))
        # BLOCK_1_RD.output(self._inc_req_made, self._buffet_push & self._buffet_joined)
        # BLOCK_1_RD.output(self._clr_req_made, 0)
        # BLOCK_1_RD.output(self._inc_req_rec, self._rd_rsp_fifo_valid & ~self._fifo_full)
        # BLOCK_1_RD.output(self._clr_req_rec, 0)

        # #############
        # # BLOCK_2_SIZE_REQ
        # #############
        # BLOCK_2_SIZE_REQ.output(self._addr_out_to_fifo, 0)
        # # Size request op
        # BLOCK_2_SIZE_REQ.output(self._op_out_to_fifo, 2)
        # BLOCK_2_SIZE_REQ.output(self._ID_out_to_fifo, 1)
        # BLOCK_2_SIZE_REQ.output(self._buffet_push, 1)
        # BLOCK_2_SIZE_REQ.output(self._rd_rsp_fifo_pop, 0)
        # BLOCK_2_SIZE_REQ.output(self._ptr_reg_en, 0)

        # BLOCK_2_SIZE_REQ.output(self._valid_inc, 0)
        # BLOCK_2_SIZE_REQ.output(self._valid_rst, 0)
        # # BLOCK_2_SIZE_REQ.output(self._fifo_push, 0)
        # BLOCK_2_SIZE_REQ.output(self._coord_out_fifo_push, 0)
        # BLOCK_2_SIZE_REQ.output(self._pos_out_fifo_push, 0)

        # BLOCK_2_SIZE_REQ.output(self._tag_eos, 0)
        # BLOCK_2_SIZE_REQ.output(self._next_seq_length, kts.const(0, 16))
        # BLOCK_2_SIZE_REQ.output(self._update_seq_state, 0)
        # BLOCK_2_SIZE_REQ.output(self._last_valid_accepting, 0)
        # BLOCK_2_SIZE_REQ.output(self._pop_infifo, 0)
        # BLOCK_2_SIZE_REQ.output(self._inc_fiber_addr, 0)
        # BLOCK_2_SIZE_REQ.output(self._clr_fiber_addr, 0)
        # BLOCK_2_SIZE_REQ.output(self._inc_rep, 0)
        # BLOCK_2_SIZE_REQ.output(self._clr_rep, 0)
        # BLOCK_2_SIZE_REQ.output(self._data_to_fifo, kts.const(0, 16))
        # BLOCK_2_SIZE_REQ.output(self._en_reg_data_in, 0)
        # BLOCK_2_SIZE_REQ.output(self._pos_out_to_fifo, kts.const(0, 16))
        # BLOCK_2_SIZE_REQ.output(self._inc_req_made, 0)
        # BLOCK_2_SIZE_REQ.output(self._clr_req_made, 1)
        # BLOCK_2_SIZE_REQ.output(self._inc_req_rec, 0)
        # BLOCK_2_SIZE_REQ.output(self._clr_req_rec, 1)

        # #############
        # # BLOCK_2_SIZE_REC
        # #############
        # BLOCK_2_SIZE_REC.output(self._addr_out_to_fifo, 0)
        # BLOCK_2_SIZE_REC.output(self._op_out_to_fifo, 0)
        # BLOCK_2_SIZE_REC.output(self._ID_out_to_fifo, 0)
        # BLOCK_2_SIZE_REC.output(self._buffet_push, 0)
        # BLOCK_2_SIZE_REC.output(self._rd_rsp_fifo_pop, ~self._fifo_full)
        # BLOCK_2_SIZE_REC.output(self._ptr_reg_en, self._rd_rsp_fifo_valid)

        # BLOCK_2_SIZE_REC.output(self._valid_inc, 0)
        # BLOCK_2_SIZE_REC.output(self._valid_rst, 0)
        # # BLOCK_2_SIZE_REC.output(self._fifo_push, self._rd_rsp_fifo_valid)
        # BLOCK_2_SIZE_REC.output(self._coord_out_fifo_push, self._rd_rsp_fifo_valid)
        # BLOCK_2_SIZE_REC.output(self._pos_out_fifo_push, 0)

        # BLOCK_2_SIZE_REC.output(self._tag_eos, 0)
        # BLOCK_2_SIZE_REC.output(self._next_seq_length, kts.const(0, 16))
        # BLOCK_2_SIZE_REC.output(self._update_seq_state, 0)
        # BLOCK_2_SIZE_REC.output(self._last_valid_accepting, 0)
        # BLOCK_2_SIZE_REC.output(self._pop_infifo, 0)
        # BLOCK_2_SIZE_REC.output(self._inc_fiber_addr, 0)
        # BLOCK_2_SIZE_REC.output(self._clr_fiber_addr, 0)
        # BLOCK_2_SIZE_REC.output(self._inc_rep, 0)
        # BLOCK_2_SIZE_REC.output(self._clr_rep, 0)
        # BLOCK_2_SIZE_REC.output(self._data_to_fifo, self._rd_rsp_fifo_out_data)
        # BLOCK_2_SIZE_REC.output(self._en_reg_data_in, 0)
        # BLOCK_2_SIZE_REC.output(self._pos_out_to_fifo, kts.const(0, 16))
        # BLOCK_2_SIZE_REC.output(self._inc_req_made, 0)
        # BLOCK_2_SIZE_REC.output(self._clr_req_made, 0)
        # BLOCK_2_SIZE_REC.output(self._inc_req_rec, 0)
        # BLOCK_2_SIZE_REC.output(self._clr_req_rec, 0)

        # #############
        # # BLOCK_2_RD
        # #############
        # BLOCK_2_RD.output(self._addr_out_to_fifo, self._num_req_made)
        # BLOCK_2_RD.output(self._op_out_to_fifo, 1)
        # BLOCK_2_RD.output(self._ID_out_to_fifo, 1)
        # BLOCK_2_RD.output(self._buffet_push, self._num_req_made < self._ptr_reg)
        # BLOCK_2_RD.output(self._rd_rsp_fifo_pop, ~self._fifo_full & (self._num_req_rec < self._ptr_reg))
        # BLOCK_2_RD.output(self._ptr_reg_en, 0)

        # BLOCK_2_RD.output(self._valid_inc, 0)
        # BLOCK_2_RD.output(self._valid_rst, 0)
        # # BLOCK_2_RD.output(self._fifo_push, self._rd_rsp_fifo_valid & (self._num_req_rec < self._ptr_reg))
        # BLOCK_2_RD.output(self._coord_out_fifo_push, self._rd_rsp_fifo_valid & (self._num_req_rec < self._ptr_reg))
        # BLOCK_2_RD.output(self._pos_out_fifo_push, 0)

        # BLOCK_2_RD.output(self._tag_eos, 0)
        # BLOCK_2_RD.output(self._next_seq_length, kts.const(0, 16))
        # BLOCK_2_RD.output(self._update_seq_state, 0)
        # BLOCK_2_RD.output(self._last_valid_accepting, 0)
        # BLOCK_2_RD.output(self._pop_infifo, 0)
        # BLOCK_2_RD.output(self._inc_fiber_addr, 0)
        # BLOCK_2_RD.output(self._clr_fiber_addr, 0)
        # BLOCK_2_RD.output(self._inc_rep, 0)
        # BLOCK_2_RD.output(self._clr_rep, 0)
        # BLOCK_2_RD.output(self._data_to_fifo, self._rd_rsp_fifo_out_data)
        # BLOCK_2_RD.output(self._en_reg_data_in, 0)
        # BLOCK_2_RD.output(self._pos_out_to_fifo, kts.const(0, 16))
        # BLOCK_2_RD.output(self._inc_req_made, self._buffet_push & self._buffet_joined)
        # BLOCK_2_RD.output(self._clr_req_made, 0)
        # BLOCK_2_RD.output(self._inc_req_rec, self._rd_rsp_fifo_valid & ~self._fifo_full)
        # BLOCK_2_RD.output(self._clr_req_rec, 0)

        # #############
        # # FREE1
        # #############
        # FREE1.output(self._addr_out_to_fifo, 0)
        # FREE1.output(self._op_out_to_fifo, 0)
        # FREE1.output(self._ID_out_to_fifo, 0)
        # FREE1.output(self._buffet_push, 1)
        # FREE1.output(self._rd_rsp_fifo_pop, 0)
        # FREE1.output(self._ptr_reg_en, 0)

        # FREE1.output(self._valid_inc, 0)
        # FREE1.output(self._valid_rst, 0)
        # # FREE1.output(self._fifo_push, 0)
        # FREE1.output(self._coord_out_fifo_push, 0)
        # FREE1.output(self._pos_out_fifo_push, 0)

        # FREE1.output(self._tag_eos, 0)
        # FREE1.output(self._next_seq_length, kts.const(0, 16))
        # FREE1.output(self._update_seq_state, 0)
        # FREE1.output(self._last_valid_accepting, 0)
        # FREE1.output(self._pop_infifo, 0)
        # FREE1.output(self._inc_fiber_addr, 0)
        # FREE1.output(self._clr_fiber_addr, 0)
        # FREE1.output(self._inc_rep, 0)
        # FREE1.output(self._clr_rep, 0)
        # FREE1.output(self._data_to_fifo, kts.const(0, 16))
        # FREE1.output(self._en_reg_data_in, 0)
        # FREE1.output(self._pos_out_to_fifo, kts.const(0, 16))
        # FREE1.output(self._inc_req_made, 0)
        # FREE1.output(self._clr_req_made, 0)
        # FREE1.output(self._inc_req_rec, 0)
        # FREE1.output(self._clr_req_rec, 0)

        # #############
        # # FREE2
        # #############
        # FREE2.output(self._addr_out_to_fifo, 0)
        # FREE2.output(self._op_out_to_fifo, 0)
        # FREE2.output(self._ID_out_to_fifo, 1)
        # FREE2.output(self._buffet_push, 1)
        # FREE2.output(self._rd_rsp_fifo_pop, 0)
        # FREE2.output(self._ptr_reg_en, 0)

        # FREE2.output(self._valid_inc, 0)
        # FREE2.output(self._valid_rst, 1)
        # # FREE2.output(self._fifo_push, 0)
        # FREE2.output(self._coord_out_fifo_push, 0)
        # FREE2.output(self._pos_out_fifo_push, 0)

        # FREE2.output(self._tag_eos, 0)
        # FREE2.output(self._next_seq_length, kts.const(0, 16))
        # FREE2.output(self._update_seq_state, 0)
        # FREE2.output(self._last_valid_accepting, 0)
        # FREE2.output(self._pop_infifo, 0)
        # FREE2.output(self._inc_fiber_addr, 0)
        # FREE2.output(self._clr_fiber_addr, 1)
        # FREE2.output(self._inc_rep, 0)
        # FREE2.output(self._clr_rep, 1)
        # FREE2.output(self._data_to_fifo, kts.const(0, 16))
        # FREE2.output(self._en_reg_data_in, 0)
        # FREE2.output(self._pos_out_to_fifo, kts.const(0, 16))
        # FREE2.output(self._inc_req_made, 0)
        # FREE2.output(self._clr_req_made, 0)
        # FREE2.output(self._inc_req_rec, 0)
        # FREE2.output(self._clr_req_rec, 0)

        # #############
        # # DONE
        # #############
        # DONE.output(self._addr_out_to_fifo, 0)
        # DONE.output(self._op_out_to_fifo, 0)
        # DONE.output(self._ID_out_to_fifo, 0)
        # DONE.output(self._buffet_push, 0)
        # DONE.output(self._rd_rsp_fifo_pop, 0)
        # DONE.output(self._ptr_reg_en, 0)
        # DONE.output(self._valid_inc, 0)
        # DONE.output(self._valid_rst, 1)
        # # DONE.output(self._fifo_push, 0)
        # DONE.output(self._coord_out_fifo_push, 0)
        # DONE.output(self._pos_out_fifo_push, 0)

        # DONE.output(self._tag_eos, 0)
        # DONE.output(self._next_seq_length, kts.const(0, 16))
        # DONE.output(self._update_seq_state, 0)
        # DONE.output(self._last_valid_accepting, 0)
        # DONE.output(self._pop_infifo, 0)
        # DONE.output(self._inc_fiber_addr, 0)
        # DONE.output(self._clr_fiber_addr, 1)
        # DONE.output(self._inc_rep, 0)
        # DONE.output(self._clr_rep, 1)
        # DONE.output(self._data_to_fifo, kts.const(0, 16))
        # DONE.output(self._en_reg_data_in, 0)
        # DONE.output(self._pos_out_to_fifo, kts.const(0, 16))
        # DONE.output(self._inc_req_made, 0)
        # DONE.output(self._clr_req_made, 0)
        # DONE.output(self._inc_req_rec, 0)
        # DONE.output(self._clr_req_rec, 0)

        # self.scan_fsm.set_start_state(START)

# ===================================
# Dump metadata into fifo
# ===================================

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

        self.add_child(f"coordinate_fifo",
                       self._coord_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._crd_res_fifo_valid,
                       pop=self._coord_out_ready_in,
                       data_in=self._coord_data_in_packed,
                       data_out=self._coord_data_out_packed)

        self.wire(self._coord_out_valid_out, ~self._coord_fifo.ports.empty)
        self.wire(self._fifo_full_pre[0], self._coord_fifo.ports.full)

        self.wire(self._crd_res_fifo_pop, ~self._coord_fifo.ports.full)

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
                              add_clk_enable=True)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(scanner_dut, filename="scanner_pipe.sv",
            optimize_if=False)