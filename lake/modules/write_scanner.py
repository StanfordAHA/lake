from bz2 import compress
import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.top.memory_controller import MemoryController
from lake.utils.util import add_counter, register, sticky_flag, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO
from lake.attributes.shared_fifo_attr import SharedFifoAttr


class WriteScanner(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 defer_fifos=True,
                 add_flush=False,
                 perf_debug=True):

        super().__init__("write_scanner", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = add_flush
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos
        self.perf_debug = perf_debug

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

        # Compressed data structure output
        self._compressed = self.input("compressed", 1)
        self._compressed.add_attribute(ConfigRegAttr("Only matters for a lowest-level write scanner - use address of go linearly"))

        self._lowest_level = self.input("lowest_level", 1)
        self._lowest_level.add_attribute(ConfigRegAttr("Only matters for a lowest-level write scanner - use address of go linearly"))

        # Set the stop token level to act as barrier in compressed data structure formation - (default 0 + 1 for root)
        self._stop_lvl = self.input("stop_lvl", 16)
        self._stop_lvl.add_attribute(ConfigRegAttr("What level stop tokens are used as dedup barrier"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # Scanner interface will need
        # input data, input valid
        # output address, output valid
        # There is a data in (for lowest level values, other level coordinates) and for address on lowest level when doing dense
        self._data_in = self.input("data_in", self.data_width + 1, packed=True)
        self._data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._data_in_valid_in = self.input("data_in_valid", 1)
        self._data_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._data_in_ready_out = self.output("data_in_ready", 1)
        self._data_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._addr_in = self.input("addr_in", self.data_width + 1, packed=True)
        self._addr_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._addr_in_valid_in = self.input("addr_in_valid", 1)
        self._addr_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._addr_in_ready_out = self.output("addr_in_ready", 1)
        self._addr_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._block_wr_in = self.input("block_wr_in", self.data_width + 1, packed=True)
        self._block_wr_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._block_wr_in_valid_in = self.input("block_wr_in_valid", 1)
        self._block_wr_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._block_wr_in_ready_out = self.output("block_wr_in_ready", 1)
        self._block_wr_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._eos_in = self.input("eos_in", 2)
        # self._eos_in.add_attribute(ControlSignalAttr(is_control=True))

        # Ready in from the memory
        # self._ready_in = self.input("ready_in", 1)
        # self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        # self._ready_out = self.output("ready_out", 2)
        # self._ready_out.add_attribute(ControlSignalAttr(is_control=False))

        # Outputs
        # Data to write to memory (whether its val/coord/metadata)
        self._data_out = self.output("data_out", self.data_width + 1, packed=True)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._op_out = self.output("op_out", 1)
        # self._op_out.add_attribute(ControlSignalAttr(is_control=False))

        self._data_out_ready_in = self.input("data_out_ready", 1)
        self._data_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._data_out_valid_out = self.output("data_out_valid", 1)
        self._data_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._addr_out = self.output("addr_out", self.data_width + 1, packed=True)
        self._addr_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._addr_out_ready_in = self.input("addr_out_ready", 1)
        self._addr_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._addr_out_valid_out = self.output("addr_out_valid", 1)
        self._addr_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._ID_out = self.output("ID_out", self.data_width + 1, packed=True)
        self._ID_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._ID_out_ready_in = self.input("ID_out_ready", 1)
        self._ID_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ID_out_valid_out = self.output("ID_out_valid", 1)
        self._ID_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._outer_coord_out = self.output("outer_coord_out", self.data_width)
        # self._outer_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._pos_out = self.output("pos_out", self.data_width)
        # self._pos_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # Valid out for both streams
        # self._valid_out = self.output("valid_out", 2)
        # self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # Address to write to memory

        # Eos for both streams...
        # self._eos_out = self.output("eos_out", 2)
        # self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Memory write enable signal

        # Point to the row in storage for data recovery
        # self._payload_ptr = self.output("payload_ptr", 16)
        # self._payload_ptr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._inner_dim_offset = self.input("inner_dim_offset", 16)
        # self._inner_dim_offset.add_attribute(ConfigRegAttr("Memory address of the inner level..."))

        self._block_mode = self.input("block_mode", 1)
        self._block_mode.add_attribute(ConfigRegAttr("Block Writes or Not"))

        # self._spacc_mode = self.input("spacc_mode", 1)
        # self._spacc_mode.add_attribute(ConfigRegAttr("Sparse Accum Mode or Not"))

        self._init_blank = self.input("init_blank", 1)
        self._init_blank.add_attribute(ConfigRegAttr("Init blank fiber (for sparse accum)"))

        # Vector Reduce Mode
        self._vector_reduce_mode = self.input("vector_reduce_mode", 1)

        self._set_blank_done = self.var("set_blank_done", 1)
        self._clr_blank_done = self.var("clr_blank_done", 1)
        self._blank_done = sticky_flag(self, self._set_blank_done,
                                       clear=self._clr_blank_done, name='blank_done_stick', seq_only=True)

# =============================
# Input FIFO
#
# To leverage hierarchical intersection, we need to be
# able to accept an incoming stream from the output of a previous
# level. Root-level scanners drive the processing, whereas non-root-level
# scanners are driven by upper levels
# =============================

        self._data_infifo = RegFIFO(data_width=1 * self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._data_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._addr_infifo = RegFIFO(data_width=1 * self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._addr_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._block_wr_infifo = RegFIFO(data_width=self.data_width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._block_wr_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        self._infifo_pop = self.var("infifo_pop", 2)

        # For input streams, need coord_in, valid_in, eos_in
        self._data_infifo_data_in = self.var("data_infifo_data_in", self.data_width)
        # self._infifo_coord_in = self.var("infifo_coord_in", self.data_width)
        self._data_infifo_valid_in = self.var("data_infifo_valid_in", 1)
        self._data_infifo_eos_in = self.var("data_infifo_eos_in", 1)

        # Stupid convert
        self._data_infifo_in_packed = self.var("data_infifo_in_packed", 1 * self.data_width + 1, packed=True)

        self._done_token = self.var("done_token", self.data_width + 1)
        self._semi_done_token = self.var("semi_done_token", self.data_width + 1)
        self.wire(self._done_token, kts.concat(kts.const(1, 1), kts.const(0, 7), kts.const(1, 1), kts.const(0, 8)))
        self.wire(self._semi_done_token, kts.concat(kts.const(1, 1), kts.const(0, 11), kts.const(1, 1), kts.const(0, 4)))

        # self.wire(self._data_infifo_in_packed[self.data_width], self._data_in[self.data_width])
        # self.wire(self._data_infifo_in_packed[self.data_width - 1, 0], self._data_in[self.data_width - 1, 0])

        self._vr_fsm_wr_scan_data_in = self.var("vr_fsm_wr_scan_data_in", self.data_width + 1)
        self.wire(self._vr_fsm_wr_scan_data_in, kts.ternary((self._data_in == self._semi_done_token), self._done_token, self._data_in))
        self.wire(self._data_infifo_in_packed, kts.ternary(self._vector_reduce_mode, self._vr_fsm_wr_scan_data_in, self._data_in))

        self._data_infifo_out_packed = self.var("data_infifo_out_packed", 1 * self.data_width + 1, packed=True)
        self.wire(self._data_infifo_eos_in, self._data_infifo_out_packed[self.data_width])
        self.wire(self._data_infifo_data_in, self._data_infifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"data_input_fifo",
                       self._data_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._data_in_valid_in,
                       pop=self._infifo_pop[0],
                       data_in=self._data_infifo_in_packed,
                       data_out=self._data_infifo_out_packed)

        self.wire(self._data_in_ready_out, ~self._data_infifo.ports.full)
        self.wire(self._data_infifo_valid_in, ~self._data_infifo.ports.empty)

        # For input streams, need coord_in, valid_in, eos_in
        self._addr_infifo_data_in = self.var("addr_infifo_data_in", self.data_width)
        # self._infifo_coord_in = self.var("infifo_coord_in", self.data_width)
        self._addr_infifo_valid_in = self.var("addr_infifo_valid_in", 1)
        self._addr_infifo_eos_in = self.var("addr_infifo_eos_in", 1)

        self._addr_infifo_in_packed = self.var("addr_infifo_in_packed", 1 * self.data_width + 1, packed=True)
        self.wire(self._addr_infifo_in_packed[self.data_width], self._addr_in[self.data_width])
        self.wire(self._addr_infifo_in_packed[self.data_width - 1, 0], self._addr_in[self.data_width - 1, 0])
        self._addr_infifo_out_packed = self.var("addr_infifo_out_packed", 1 * self.data_width + 1, packed=True)
        self.wire(self._addr_infifo_eos_in, self._addr_infifo_out_packed[self.data_width])
        self.wire(self._addr_infifo_data_in, self._addr_infifo_out_packed[self.data_width - 1, 0])

        self.add_child(f"addr_input_fifo",
                       self._addr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._addr_in_valid_in,
                       pop=self._infifo_pop[1],
                       data_in=self._addr_infifo_in_packed,
                       data_out=self._addr_infifo_out_packed)

        self.wire(self._addr_in_ready_out, ~self._addr_infifo.ports.full)
        self.wire(self._addr_infifo_valid_in, ~self._addr_infifo.ports.empty)

        # Block WR FIFO

        self._block_wr_fifo_data_out = self.var("block_wr_fifo_data_out", 1 * self.data_width, packed=True)

        self._pop_block_wr = self.var("pop_block_wr", 1)
        self._block_wr_fifo_valid = self.var("block_wr_fifo_valid", 1)
        self.add_child(f"block_wr_input_fifo",
                       self._block_wr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._block_wr_in_valid_in,
                       pop=self._pop_block_wr,
                       data_in=self._block_wr_in[self.data_width - 1, 0])

        self.wire(self._block_wr_in_ready_out, ~self._block_wr_infifo.ports.full)
        self.wire(self._block_wr_fifo_valid, ~self._block_wr_infifo.ports.empty)

        # State for block writes
        self._set_block_size = self.var("set_block_size", 1)
        # self._block_size = register(self, self._data_infifo_data_in, self._set_block_size, name="block_size")
        self._block_size = register(self, self._block_wr_infifo.ports.data_out, self._set_block_size, name="block_size", packed=True)

        self._inc_block_write = self.var("inc_block_write", 1)
        self._clr_block_write = self.var("clr_block_write", 1)
        self._block_writes = add_counter(self, "block_write_count", 16, increment=self._inc_block_write, clear=self._clr_block_write)

# =============================
# Output FIFO
# =============================

        self._data_to_fifo = self.var("data_to_fifo", self.data_width, packed=True)
        self._op_to_fifo = self.var("op_to_fifo", 1)
        self._addr_to_fifo = self.var("addr_to_fifo", self.data_width, packed=True)
        self._ID_to_fifo = self.var("ID_to_fifo", self.data_width, packed=True)

        self._push_to_outs = self.var("push_to_outs", 1)

        all_outputs_ready = []
        out_pushes = []

        ########################
        # DATA OUT + OP OUT
        ########################
        self._data_out_fifo_push = self.var("data_out_fifo_push", 1)
        self._data_out_fifo_full = self.var("data_out_fifo_full", 1)

        out_pushes.append(self._data_out_fifo_push)

        # Output FIFOs to the buffet-like
        # TODO: Make sure this works
        data_out_fifo_in = kts.concat(self._op_to_fifo, self._data_to_fifo)
        data_out_outfifo = RegFIFO(data_width=data_out_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        data_out_outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self.add_child(f"data_out_fifo",
                       data_out_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._data_out_fifo_push,
                       pop=self._data_out_ready_in,
                       data_in=data_out_fifo_in,
                       data_out=kts.concat(self._data_out))
        self.wire(self._data_out_fifo_full, data_out_outfifo.ports.full)
        self.wire(self._data_out_valid_out, ~data_out_outfifo.ports.empty)

        all_outputs_ready.append(self._data_out_fifo_full)

        ########################
        # ADDR OUT
        ########################
        self._addr_out_fifo_push = self.var("addr_out_fifo_push", 1)
        self._addr_out_fifo_full = self.var("addr_out_fifo_full", 1)

        out_pushes.append(self._addr_out_fifo_push)

        # Output FIFOs to the buffet-like
        addr_out_fifo_in = kts.concat(kts.const(0, 1), self._addr_to_fifo)
        addr_out_outfifo = RegFIFO(data_width=addr_out_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        addr_out_outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self.add_child(f"addr_out_fifo",
                       addr_out_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._addr_out_fifo_push,
                       pop=self._addr_out_ready_in,
                       data_in=addr_out_fifo_in,
                       data_out=kts.concat(self._addr_out))
        self.wire(self._addr_out_fifo_full, addr_out_outfifo.ports.full)
        self.wire(self._addr_out_valid_out, ~addr_out_outfifo.ports.empty)

        all_outputs_ready.append(self._addr_out_fifo_full)

        ########################
        # ID OUT
        ########################
        self._ID_out_fifo_push = self.var("ID_out_fifo_push", 1)
        self._ID_out_fifo_full = self.var("ID_out_fifo_full", 1)

        out_pushes.append(self._ID_out_fifo_push)

        # Output FIFOs to the buffet-like
        ID_out_fifo_in = kts.concat(kts.const(0, 1), self._ID_to_fifo)
        ID_out_outfifo = RegFIFO(data_width=ID_out_fifo_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        ID_out_outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self.add_child(f"ID_out_fifo",
                       ID_out_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._ID_out_fifo_push,
                       pop=self._ID_out_ready_in,
                       data_in=ID_out_fifo_in,
                       data_out=kts.concat(self._ID_out))
        self.wire(self._ID_out_fifo_full, ID_out_outfifo.ports.full)
        self.wire(self._ID_out_valid_out, ~ID_out_outfifo.ports.empty)

        all_outputs_ready.append(self._ID_out_fifo_full)

        catted = kts.concat(*[*all_outputs_ready])
        # only downstream ready if none of the fifos are full
        self._join_out_ready = (~catted).r_and()
        # Broadcast the single push to all three
        self.wire(kts.concat(*[*out_pushes]), kts.concat(*[self._push_to_outs for i in range(len(out_pushes))]))

# =============================
# SCAN FSM
# =============================

        # Indicate if the incoming stop token is geq than the programmed stop lvl
        # self._stop_lvl_geq = self.var("stop_lvl_geq", 1)
        # self.wire(self._stop_lvl_geq, self._data_infifo_eos_in & self._data_infifo_valid_in & (self._data_infifo_data_in[self.OPCODE_BT] == self.STOP_CODE) &
        #                               (self._data_infifo_data_in[self.STOP_BT] >= self._stop_lvl[self.STOP_BT]))

        # self._stop_lvl_geq_p1 = self.var("stop_lvl_geq_p1", 1)
        # self.wire(self._stop_lvl_geq_p1, self._data_infifo_eos_in & self._data_infifo_valid_in & (self._data_infifo_data_in[self.OPCODE_BT] == self.STOP_CODE) &
        #                               (self._data_infifo_data_in[self.STOP_BT] >= (self._stop_lvl[self.STOP_BT] + 1)))

        # self._stop_lvl_geq_p1_sticky = sticky_flag(self, self._stop_lvl_geq_p1, clear=self._clr_blank_done,
        #                                            name="stop_lvl_new_blank_sticky", seq_only=True)

        self._data_done_in = self.var("data_done_in", 1)
        self.wire(self._data_done_in, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in[self.OPCODE_BT] == self.DONE_CODE))

        # self._data_done_in = self.var("data_done_in", 1)
        # self.wire(self._data_done_in, (self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in[self.OPCODE_BT] == self.DONE_CODE)) |
        #                               (self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in[self.OPCODE_BT] == self.STOP_CODE) & (self._data_infifo_data_in[self.STOP_BT] >= (self._stop_lvl + 1))))

        self._addr_done_in = self.var("addr_done_in", 1)
        self.wire(self._addr_done_in, self._addr_infifo_valid_in & self._addr_infifo_eos_in & (self._addr_infifo_data_in[self.OPCODE_BT] == self.DONE_CODE))

        # Address for writing segment
        self._inc_seg_addr = self.var("inc_seg_addr", 1)
        self._clr_seg_addr = self.var("clr_seg_addr", 1)
        self._seg_addr = add_counter(self, "segment_addr", 16, increment=self._inc_seg_addr, clear=self._clr_seg_addr)

        self._inc_coord_addr = self.var("inc_coord_addr", 1)
        self._clr_coord_addr = self.var("clr_coord_addr", 1)
        self._coord_addr = add_counter(self, "coord_addr", 16, increment=self._inc_coord_addr, clear=self._clr_coord_addr)

        # Value to go to segment
        self._inc_seg_ctr = self.var("inc_seg_ctr", 1)
        self._clr_seg_ctr = self.var("clr_seg_ctr", 1)
        self._seg_ctr = add_counter(self, "segment_counter", 16, increment=self._inc_seg_ctr, clear=self._clr_seg_ctr)

        self._set_curr_coord = self.var("set_curr_coord", 1)
        self._clr_curr_coord = self.var("clr_curr_coord", 1)
        self._curr_coord = register(self, self._data_infifo_data_in, enable=self._set_curr_coord)
        self._curr_coord_valid = sticky_flag(self, self._set_curr_coord, clear=self._clr_curr_coord, name="valid_coord_sticky", seq_only=True)

        # Indicates if we are seeing a new coordinate
        self._new_coord = self.var("new_coord", 1)
        # We have a new coord if the new coord input is valid and the curr_coord is not valid, or the data is different.
        self.wire(self._new_coord, (self._data_infifo_valid_in & ~self._data_infifo_eos_in) & (~self._curr_coord_valid | (self._data_infifo_data_in != self._curr_coord)))

        self._stop_in = self.var("stop_in", 1)
        self.wire(self._stop_in, self._data_infifo_valid_in & self._data_infifo_eos_in)

        # self._full_stop = self.var("full_stop", 1)
        # self.wire(self._full_stop, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in == 0))

        self._clr_wen_made = self.var("clr_wen_made", 1)
        self._wen_made = sticky_flag(self, self._push_to_outs, clear=self._clr_wen_made, name="wen_made", seq_only=True)

        self._ID_curr = self.var("ID_curr", self.data_width)

        self._in_done = self.var("IN_DONE", 1)

        # Create FSM
        self.scan_fsm = self.add_fsm("scan_seq", reset_high=False)
        START = self.scan_fsm.add_state("START")
        ALLOCATE1 = self.scan_fsm.add_state("ALLOCATE1")
        ALLOCATE2 = self.scan_fsm.add_state("ALLOCATE2")
        BLOCK_1_SZ = self.scan_fsm.add_state("BLOCK_1_SZ")
        BLOCK_1_WR = self.scan_fsm.add_state("BLOCK_1_WR")
        BLOCK_2_SZ = self.scan_fsm.add_state("BLOCK_2_SZ")
        BLOCK_2_WR = self.scan_fsm.add_state("BLOCK_2_WR")
        # Lowest level
        LL = self.scan_fsm.add_state("LL")
        # Lowest level uncompressed (use address)
        UnLL = self.scan_fsm.add_state("UnLL")
        # Lowest level compressed
        ComLL = self.scan_fsm.add_state("ComLL")
        UL_WZ = self.scan_fsm.add_state("UL_WZ")
        UL = self.scan_fsm.add_state("UL")
        FINALIZE1 = self.scan_fsm.add_state("FINALIZE1")
        FINALIZE2 = self.scan_fsm.add_state("FINALIZE2")
        UL_EMIT = self.scan_fsm.add_state("UL_EMIT")
        # UL_EMIT_COORD = self.scan_fsm.add_state("UL_EMIT_COORD")
        # UL_EMIT_SEG = self.scan_fsm.add_state("UL_EMIT_SEG")
        DONE = self.scan_fsm.add_state("DONE")

        ####################
        # Next State Logic
        ####################

        ####################
        # START #
        ####################
        # Start state goes to either lowest level or upper level
        START.next(ALLOCATE1, self._tile_en)
        START.next(START, None)

        ####################
        # ALLOCATE1 #
        ####################
        # ALLOCATE1 allocates the lower ID buffet
        ALLOCATE1.next(ALLOCATE1, ~self._join_out_ready)
        ALLOCATE1.next(ALLOCATE2, ~self._lowest_level & self._join_out_ready)
        ALLOCATE1.next(BLOCK_1_SZ, self._lowest_level & self._block_mode & self._join_out_ready)
        ALLOCATE1.next(LL, self._lowest_level & ~self._block_mode & self._join_out_ready)
        # ALLOCATE1.next(UL_WZ, ~self._lowest_level & self._join_out_ready)

        ####################
        # ALLOCATE2 #
        ####################
        # lowest level is true here
        ALLOCATE2.next(ALLOCATE2, ~self._join_out_ready)
        ALLOCATE2.next(BLOCK_1_SZ, self._block_mode & self._join_out_ready)
        ALLOCATE2.next(UL_WZ, ~self._block_mode & self._join_out_ready)

        ####################
        # BLOCK_1_SZ
        ####################
        # Get the first block size...
        # BLOCK_1_SZ.next(BLOCK_1_WR, self._data_infifo_valid_in)
        BLOCK_1_SZ.next(BLOCK_1_WR, self._block_wr_fifo_valid)
        BLOCK_1_SZ.next(BLOCK_1_SZ, None)

        ####################
        # BLOCK_1_WR
        ####################
        # Write the first block...
        # If this is just writing a single structure, end after that
        BLOCK_1_WR.next(BLOCK_2_SZ, (self._block_writes == self._block_size) & ~self._lowest_level)
        # BLOCK_1_WR.next(DONE, (self._block_writes == self._block_size) & self._lowest_level)
        BLOCK_1_WR.next(FINALIZE2, (self._block_writes == self._block_size) & self._lowest_level)
        BLOCK_1_WR.next(BLOCK_1_WR, None)

        ####################
        # BLOCK_2_SZ
        ####################
        # Get the second block size...
        # BLOCK_2_SZ.next(BLOCK_2_WR, self._data_infifo_valid_in)
        BLOCK_2_SZ.next(BLOCK_2_WR, self._block_wr_fifo_valid)
        BLOCK_2_SZ.next(BLOCK_2_SZ, None)

        ####################
        # BLOCK_2_WR
        ####################
        # Get the first block size...
        # BLOCK_2_WR.next(DONE, (self._block_writes == self._block_size))
        BLOCK_2_WR.next(FINALIZE1, (self._block_writes == self._block_size))
        BLOCK_2_WR.next(BLOCK_2_WR, None)

        ####################
        # LL #
        ####################
        # Redundant state but helpful in my head
        # Go to compressed or uncompressed from here
        # Unless we are doing spacc mode - want to go straight to finalize
        # if we haven't already done so...
        LL.next(FINALIZE2, (self._init_blank & ~self._blank_done))
        LL.next(ComLL, self._compressed & (~self._init_blank | self._blank_done))
        LL.next(UnLL, ~self._compressed & (~self._init_blank | self._blank_done))

        ####################
        # ComLL
        ####################
        # In the compressed state of lowest level, we only need to write the
        # data values in order...just watching for the stop 0 token
        # ComLL.next(DONE, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in == 0))
        # ComLL.next(FINALIZE2, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in[9, 8] == kts.const(1, 2)))
        # ComLL.next(FINALIZE2, self._data_done_in)
        # ComLL.next(FINALIZE2, self._data_done_in | (self._spacc_mode & self._stop_lvl_geq))
        ComLL.next(FINALIZE2, self._data_done_in)
        # ComLL.next(FINALIZE2, (self._data_done_in & ~self._spacc_mode) | (self._spacc_mode & self._stop_lvl_geq))
        ComLL.next(ComLL, None)

        ####################
        # UnLL
        ####################
        # In the uncompressed lowest level, we are writing the data at the specified address, so we are similarly looking
        # for stop 0 token
        # UnLL.next(DONE, self._data_infifo_valid_in & self._addr_infifo_valid_in & self._data_infifo_eos_in & self._addr_infifo_eos_in &
        # UnLL.next(FINALIZE2, self._data_infifo_valid_in & self._addr_infifo_valid_in & self._data_infifo_eos_in & self._addr_infifo_eos_in &
        #           (self._data_infifo_data_in[9, 8] == kts.const(1, 2)) & (self._addr_infifo_data_in[9, 8] == kts.const(1, 2)))
        # UnLL.next(FINALIZE2, (self._data_done_in & self._addr_done_in) | (self._spacc_mode & self._stop_lvl_geq))
        UnLL.next(FINALIZE2, (self._data_done_in & self._addr_done_in))
        # UnLL.next(FINALIZE2, (self._data_done_in & self._addr_done_in & ~self._spacc_mode) | (self._spacc_mode & self._stop_lvl_geq))
        UnLL.next(UnLL, None)

        ####################
        # UL_WZ
        ####################
        # Need to write a 0 to the segment array first...
        UL_WZ.next(UL, self._join_out_ready)
        UL_WZ.next(UL_WZ, ~self._join_out_ready)

        ####################
        # UL #
        ####################
        # ASSUMED TO BE COMPRESSED - OTHERWISE DFG LOOKS DIFFERENT - PERFORMS MATH ON COORDINATES
        # In the upper level, we will emit new coordinates linearly as we see new ones, reset tracking at stop_lvl
        UL.next(UL_EMIT, self._data_infifo_valid_in)
        UL.next(UL, None)

        ####################
        # UL_EMIT #
        ####################
        # Combines the stage of writing segment and writing value. Only transits to Finalize1 when _data_done_in
        UL_EMIT.next(FINALIZE1, self._data_done_in)
        UL_EMIT.next(UL_EMIT, None)

        ####################
        # FINALIZE1
        ####################
        FINALIZE1.next(FINALIZE2, self._join_out_ready)
        FINALIZE1.next(FINALIZE1, None)

        ####################
        # FINALIZE2
        ####################
        FINALIZE2.next(DONE, self._join_out_ready)
        FINALIZE2.next(FINALIZE2, None)

        ####################
        # DONE
        ####################
        # We are done...
        # TODO: Accept multiple blocks
        # Could probably drop the DONE here.
        DONE.next(START, None)

        ####################
        # FSM Output Logic
        ####################

        self.scan_fsm.output(self._data_to_fifo)
        self.scan_fsm.output(self._op_to_fifo)
        self.scan_fsm.output(self._addr_to_fifo)
        self.scan_fsm.output(self._ID_to_fifo)
        self.scan_fsm.output(self._push_to_outs)
        self.scan_fsm.output(self._inc_seg_addr)
        self.scan_fsm.output(self._clr_seg_addr)
        self.scan_fsm.output(self._inc_coord_addr)
        self.scan_fsm.output(self._clr_coord_addr)
        self.scan_fsm.output(self._inc_seg_ctr)
        self.scan_fsm.output(self._clr_seg_ctr)
        self.scan_fsm.output(self._set_curr_coord)
        self.scan_fsm.output(self._clr_curr_coord)
        self.scan_fsm.output(self._infifo_pop[0])
        self.scan_fsm.output(self._infifo_pop[1])
        self.scan_fsm.output(self._clr_wen_made)
        self.scan_fsm.output(self._set_block_size)
        self.scan_fsm.output(self._inc_block_write)
        self.scan_fsm.output(self._clr_block_write)
        self.scan_fsm.output(self._set_blank_done, default=kts.const(0, 1))
        self.scan_fsm.output(self._clr_blank_done, default=kts.const(0, 1))
        self.scan_fsm.output(self._pop_block_wr, default=kts.const(0, 1))
        self.scan_fsm.output(self._in_done, default=kts.const(0, 1))

        #######
        # START - TODO - Generate general hardware...
        #######
        START.output(self._data_to_fifo, kts.const(0, 16))
        START.output(self._op_to_fifo, 0)
        START.output(self._addr_to_fifo, kts.const(0, 16))
        START.output(self._ID_to_fifo, kts.const(0, 16))
        START.output(self._push_to_outs, 0)
        START.output(self._inc_seg_addr, 0)
        START.output(self._clr_seg_addr, 1)
        START.output(self._inc_coord_addr, 0)
        START.output(self._clr_coord_addr, 1)
        START.output(self._inc_seg_ctr, 0)
        START.output(self._clr_seg_ctr, 1)
        START.output(self._set_curr_coord, 0)
        START.output(self._clr_curr_coord, 1)
        START.output(self._infifo_pop[0], 0)
        START.output(self._infifo_pop[1], 0)
        START.output(self._clr_wen_made, 1)
        START.output(self._set_block_size, 0)
        START.output(self._inc_block_write, 0)
        START.output(self._clr_block_write, 1)

        #######
        # ALLOCATE1 - TODO - Generate general hardware...
        #######
        ALLOCATE1.output(self._data_to_fifo, kts.const(0, 16))
        ALLOCATE1.output(self._op_to_fifo, 0)
        ALLOCATE1.output(self._addr_to_fifo, kts.const(0, 16))
        ALLOCATE1.output(self._ID_to_fifo, kts.const(0, 16))
        ALLOCATE1.output(self._push_to_outs, 1)
        ALLOCATE1.output(self._inc_seg_addr, 0)
        ALLOCATE1.output(self._clr_seg_addr, 0)
        ALLOCATE1.output(self._inc_coord_addr, 0)
        ALLOCATE1.output(self._clr_coord_addr, 0)
        ALLOCATE1.output(self._inc_seg_ctr, 0)
        ALLOCATE1.output(self._clr_seg_ctr, 0)
        ALLOCATE1.output(self._set_curr_coord, 0)
        ALLOCATE1.output(self._clr_curr_coord, 0)
        ALLOCATE1.output(self._infifo_pop[0], 0)
        ALLOCATE1.output(self._infifo_pop[1], 0)
        ALLOCATE1.output(self._clr_wen_made, 0)
        ALLOCATE1.output(self._set_block_size, 0)
        ALLOCATE1.output(self._inc_block_write, 0)
        ALLOCATE1.output(self._clr_block_write, 0)

        #######
        # ALLOCATE2 - TODO - Generate general hardware...
        #######
        ALLOCATE2.output(self._data_to_fifo, kts.const(0, 16))
        ALLOCATE2.output(self._op_to_fifo, 0)
        ALLOCATE2.output(self._addr_to_fifo, kts.const(0, 16))
        ALLOCATE2.output(self._ID_to_fifo, kts.const(1, 16))
        ALLOCATE2.output(self._push_to_outs, 1)
        ALLOCATE2.output(self._inc_seg_addr, 0)
        ALLOCATE2.output(self._clr_seg_addr, 0)
        ALLOCATE2.output(self._inc_coord_addr, 0)
        ALLOCATE2.output(self._clr_coord_addr, 0)
        ALLOCATE2.output(self._inc_seg_ctr, 0)
        ALLOCATE2.output(self._clr_seg_ctr, 0)
        ALLOCATE2.output(self._set_curr_coord, 0)
        ALLOCATE2.output(self._clr_curr_coord, 0)
        ALLOCATE2.output(self._infifo_pop[0], 0)
        ALLOCATE2.output(self._infifo_pop[1], 0)
        ALLOCATE2.output(self._clr_wen_made, 0)
        ALLOCATE2.output(self._set_block_size, 0)
        ALLOCATE2.output(self._inc_block_write, 0)
        ALLOCATE2.output(self._clr_block_write, 0)

        #######
        # BLOCK_1_SZ
        #######
        BLOCK_1_SZ.output(self._data_to_fifo, kts.const(0, 16))
        BLOCK_1_SZ.output(self._op_to_fifo, 0)
        BLOCK_1_SZ.output(self._addr_to_fifo, kts.const(0, 16))
        BLOCK_1_SZ.output(self._ID_to_fifo, kts.const(1, 16))
        BLOCK_1_SZ.output(self._push_to_outs, 0)
        BLOCK_1_SZ.output(self._inc_seg_addr, 0)
        BLOCK_1_SZ.output(self._clr_seg_addr, 0)
        BLOCK_1_SZ.output(self._inc_coord_addr, 0)
        BLOCK_1_SZ.output(self._clr_coord_addr, 0)
        BLOCK_1_SZ.output(self._inc_seg_ctr, 0)
        BLOCK_1_SZ.output(self._clr_seg_ctr, 0)
        BLOCK_1_SZ.output(self._set_curr_coord, 0)
        BLOCK_1_SZ.output(self._clr_curr_coord, 0)
        # BLOCK_1_SZ.output(self._infifo_pop[0], self._data_infifo_valid_in)
        BLOCK_1_SZ.output(self._infifo_pop[0], 0)
        BLOCK_1_SZ.output(self._infifo_pop[1], 0)
        BLOCK_1_SZ.output(self._clr_wen_made, 0)
        # BLOCK_1_SZ.output(self._set_block_size, self._data_infifo_valid_in)
        BLOCK_1_SZ.output(self._set_block_size, self._block_wr_fifo_valid)
        BLOCK_1_SZ.output(self._inc_block_write, 0)
        BLOCK_1_SZ.output(self._clr_block_write, 1)
        BLOCK_1_SZ.output(self._pop_block_wr, self._block_wr_fifo_valid)

        #######
        # BLOCK_1_WR
        #######
        # BLOCK_1_WR.output(self._data_to_fifo, self._data_infifo_data_in)
        BLOCK_1_WR.output(self._data_to_fifo, self._block_wr_infifo.ports.data_out)
        BLOCK_1_WR.output(self._op_to_fifo, 1)
        BLOCK_1_WR.output(self._addr_to_fifo, self._block_writes)
        BLOCK_1_WR.output(self._ID_to_fifo, kts.const(0, 16))
        # BLOCK_1_WR.output(self._push_to_outs, self._data_infifo_valid_in & (self._block_writes < self._block_size))
        BLOCK_1_WR.output(self._push_to_outs, self._block_wr_fifo_valid & (self._block_writes < self._block_size))
        # BLOCK_1_WR.output(self._addr_out, self._block_writes)
        # BLOCK_1_WR.output(self._wen, self._data_infifo_valid_in & (self._block_writes < self._block_size))
        # BLOCK_1_WR.output(self._data_out, self._data_infifo_data_in)
        BLOCK_1_WR.output(self._inc_seg_addr, 0)
        BLOCK_1_WR.output(self._clr_seg_addr, 0)
        BLOCK_1_WR.output(self._inc_coord_addr, 0)
        BLOCK_1_WR.output(self._clr_coord_addr, 0)
        BLOCK_1_WR.output(self._inc_seg_ctr, 0)
        BLOCK_1_WR.output(self._clr_seg_ctr, 0)
        BLOCK_1_WR.output(self._set_curr_coord, 0)
        BLOCK_1_WR.output(self._clr_curr_coord, 0)
        # BLOCK_1_WR.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_1_WR.output(self._infifo_pop[0], 0)
        BLOCK_1_WR.output(self._infifo_pop[1], 0)
        BLOCK_1_WR.output(self._clr_wen_made, 0)
        BLOCK_1_WR.output(self._set_block_size, 0)
        # BLOCK_1_WR.output(self._inc_block_write, self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_1_WR.output(self._inc_block_write, self._block_wr_fifo_valid & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_1_WR.output(self._clr_block_write, 0)
        BLOCK_1_WR.output(self._pop_block_wr, self._block_wr_fifo_valid & (self._block_writes < self._block_size) & self._join_out_ready)

        #######
        # BLOCK_2_SZ
        #######
        BLOCK_2_SZ.output(self._data_to_fifo, kts.const(0, 16))
        BLOCK_2_SZ.output(self._op_to_fifo, 0)
        BLOCK_2_SZ.output(self._addr_to_fifo, kts.const(0, 16))
        BLOCK_2_SZ.output(self._ID_to_fifo, kts.const(0, 16))
        BLOCK_2_SZ.output(self._push_to_outs, 0)
        # BLOCK_2_SZ.output(self._addr_out, kts.const(0, 16))
        # BLOCK_2_SZ.output(self._wen, 0)
        # BLOCK_2_SZ.output(self._data_out, kts.const(0, 16))
        BLOCK_2_SZ.output(self._inc_seg_addr, 0)
        BLOCK_2_SZ.output(self._clr_seg_addr, 0)
        BLOCK_2_SZ.output(self._inc_coord_addr, 0)
        BLOCK_2_SZ.output(self._clr_coord_addr, 0)
        BLOCK_2_SZ.output(self._inc_seg_ctr, 0)
        BLOCK_2_SZ.output(self._clr_seg_ctr, 0)
        BLOCK_2_SZ.output(self._set_curr_coord, 0)
        BLOCK_2_SZ.output(self._clr_curr_coord, 0)
        # BLOCK_2_SZ.output(self._infifo_pop[0], self._data_infifo_valid_in)
        BLOCK_2_SZ.output(self._infifo_pop[0], 0)
        BLOCK_2_SZ.output(self._infifo_pop[1], 0)
        BLOCK_2_SZ.output(self._clr_wen_made, 0)
        # BLOCK_2_SZ.output(self._set_block_size, self._data_infifo_valid_in)
        BLOCK_2_SZ.output(self._set_block_size, self._block_wr_fifo_valid)
        BLOCK_2_SZ.output(self._inc_block_write, 0)
        BLOCK_2_SZ.output(self._clr_block_write, 1)
        BLOCK_2_SZ.output(self._pop_block_wr, self._block_wr_fifo_valid)

        #######
        # BLOCK_2_WR
        #######
        # BLOCK_2_WR.output(self._data_to_fifo, self._data_infifo_data_in)
        BLOCK_2_WR.output(self._data_to_fifo, self._block_wr_infifo.ports.data_out)
        BLOCK_2_WR.output(self._op_to_fifo, 1)
        BLOCK_2_WR.output(self._addr_to_fifo, self._block_writes)
        BLOCK_2_WR.output(self._ID_to_fifo, kts.const(1, 16))
        # BLOCK_2_WR.output(self._push_to_outs, self._data_infifo_valid_in & (self._block_writes < self._block_size))
        BLOCK_2_WR.output(self._push_to_outs, self._block_wr_fifo_valid & (self._block_writes < self._block_size))
        # BLOCK_2_WR.output(self._addr_out, self._block_writes + self._inner_dim_offset)
        # BLOCK_2_WR.output(self._wen, self._data_infifo_valid_in & (self._block_writes < self._block_size))
        # BLOCK_2_WR.output(self._data_out, self._data_infifo_data_in)
        BLOCK_2_WR.output(self._inc_seg_addr, 0)
        BLOCK_2_WR.output(self._clr_seg_addr, 0)
        BLOCK_2_WR.output(self._inc_coord_addr, 0)
        BLOCK_2_WR.output(self._clr_coord_addr, 0)
        BLOCK_2_WR.output(self._inc_seg_ctr, 0)
        BLOCK_2_WR.output(self._clr_seg_ctr, 0)
        BLOCK_2_WR.output(self._set_curr_coord, 0)
        BLOCK_2_WR.output(self._clr_curr_coord, 0)
        # BLOCK_2_WR.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_2_WR.output(self._infifo_pop[0], 0)
        BLOCK_2_WR.output(self._infifo_pop[1], 0)
        BLOCK_2_WR.output(self._clr_wen_made, 0)
        BLOCK_2_WR.output(self._set_block_size, 0)
        # BLOCK_2_WR.output(self._inc_block_write, self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_2_WR.output(self._inc_block_write, self._block_wr_fifo_valid & (self._block_writes < self._block_size) & self._join_out_ready)
        BLOCK_2_WR.output(self._clr_block_write, 0)
        BLOCK_2_WR.output(self._pop_block_wr, self._block_wr_fifo_valid & (self._block_writes < self._block_size) & self._join_out_ready)

        #######
        # LL
        #######
        LL.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        LL.output(self._op_to_fifo, 0)
        LL.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        LL.output(self._ID_to_fifo, kts.const(0, 16))
        LL.output(self._push_to_outs, 0)
        # LL.output(self._addr_out, kts.const(0, 16))
        # LL.output(self._wen, 0)
        # LL.output(self._data_out, kts.const(0, 16))
        LL.output(self._inc_seg_addr, 0)
        LL.output(self._clr_seg_addr, 0)
        LL.output(self._inc_coord_addr, 0)
        LL.output(self._clr_coord_addr, 0)
        LL.output(self._inc_seg_ctr, 0)
        LL.output(self._clr_seg_ctr, 0)
        LL.output(self._set_curr_coord, 0)
        LL.output(self._clr_curr_coord, 0)
        LL.output(self._infifo_pop[0], 0)
        LL.output(self._infifo_pop[1], 0)
        LL.output(self._clr_wen_made, 0)
        LL.output(self._set_block_size, 0)
        LL.output(self._inc_block_write, 0)
        LL.output(self._clr_block_write, 0)

        #######
        # UnLL
        #######
        UnLL.output(self._data_to_fifo, self._data_infifo_data_in)
        UnLL.output(self._op_to_fifo, 1)
        UnLL.output(self._addr_to_fifo, self._addr_infifo_data_in)
        UnLL.output(self._ID_to_fifo, kts.const(0, 16))
        UnLL.output(self._push_to_outs, (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ~(self._data_infifo_eos_in | self._addr_infifo_eos_in))
        # UnLL.output(self._addr_out, self._addr_infifo_data_in)
        # Only write the values
        # UnLL.output(self._wen, (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ~(self._data_infifo_eos_in | self._addr_infifo_eos_in))
        # UnLL.output(self._data_out, self._data_infifo_data_in)
        UnLL.output(self._inc_seg_addr, 0)
        UnLL.output(self._clr_seg_addr, 0)
        UnLL.output(self._inc_coord_addr, 0)
        UnLL.output(self._clr_coord_addr, 0)
        UnLL.output(self._inc_seg_ctr, 0)
        UnLL.output(self._clr_seg_ctr, 0)
        UnLL.output(self._set_curr_coord, 0)
        UnLL.output(self._clr_curr_coord, 0)
        # Pop if the memory is ready for a write, or its eos
        UnLL.output(self._infifo_pop[0], (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ((self._data_infifo_eos_in & self._addr_infifo_eos_in) | self._join_out_ready))
        UnLL.output(self._infifo_pop[1], (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ((self._data_infifo_eos_in & self._addr_infifo_eos_in) | self._join_out_ready))
        UnLL.output(self._clr_wen_made, 0)
        UnLL.output(self._set_block_size, 0)
        UnLL.output(self._inc_block_write, 0)
        UnLL.output(self._clr_block_write, 0)

        #######
        # ComLL
        #######
        ComLL.output(self._data_to_fifo, self._data_infifo_data_in)
        ComLL.output(self._op_to_fifo, 1)
        ComLL.output(self._addr_to_fifo, self._seg_addr)
        ComLL.output(self._ID_to_fifo, kts.const(0, 16))
        ComLL.output(self._push_to_outs, self._data_infifo_valid_in & ~self._data_infifo_eos_in)
        # Use the seg addr
        # ComLL.output(self._addr_out, self._seg_addr)
        # Only write if its data
        # ComLL.output(self._wen, self._data_infifo_valid_in & ~self._data_infifo_eos_in)
        # ComLL.output(self._data_out, self._data_infifo_data_in)
        # Increase the seg addr only if we are actually writing
        ComLL.output(self._inc_seg_addr, self._data_infifo_valid_in & ~self._data_infifo_eos_in & self._join_out_ready)
        ComLL.output(self._clr_seg_addr, 0)
        ComLL.output(self._inc_coord_addr, 0)
        ComLL.output(self._clr_coord_addr, 0)
        ComLL.output(self._inc_seg_ctr, 0)
        ComLL.output(self._clr_seg_ctr, 0)
        ComLL.output(self._set_curr_coord, 0)
        ComLL.output(self._clr_curr_coord, 0)
        # Only pop if its eos or the memory is ready for the write
        ComLL.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._data_infifo_eos_in | self._join_out_ready))
        ComLL.output(self._infifo_pop[1], 0)
        ComLL.output(self._clr_wen_made, 0)
        ComLL.output(self._set_block_size, 0)
        ComLL.output(self._inc_block_write, 0)
        ComLL.output(self._clr_block_write, 0)

        #######
        # UL_WZ
        #######
        UL_WZ.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        UL_WZ.output(self._op_to_fifo, 1)
        UL_WZ.output(self._addr_to_fifo, self._seg_addr)
        UL_WZ.output(self._ID_to_fifo, kts.const(0, 16))
        UL_WZ.output(self._push_to_outs, 1)

        # Write a 0 to the segment array
        # UL_WZ.output(self._addr_out, self._seg_addr)
        # UL_WZ.output(self._wen, 1)
        # UL_WZ.output(self._data_out, kts.const(0, 16))
        UL_WZ.output(self._inc_seg_addr, self._join_out_ready)
        UL_WZ.output(self._clr_seg_addr, 0)
        UL_WZ.output(self._inc_coord_addr, 0)
        UL_WZ.output(self._clr_coord_addr, 0)
        UL_WZ.output(self._inc_seg_ctr, 0)
        UL_WZ.output(self._clr_seg_ctr, 0)
        UL_WZ.output(self._set_curr_coord, 0)
        UL_WZ.output(self._clr_curr_coord, 0)
        UL_WZ.output(self._infifo_pop[0], 0)
        UL_WZ.output(self._infifo_pop[1], 0)
        UL_WZ.output(self._clr_wen_made, 0)
        UL_WZ.output(self._set_block_size, 0)
        UL_WZ.output(self._inc_block_write, 0)
        UL_WZ.output(self._clr_block_write, 0)

        #######
        # UL
        #######
        UL.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        UL.output(self._op_to_fifo, 1)
        UL.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        UL.output(self._ID_to_fifo, kts.const(0, 16))
        UL.output(self._push_to_outs, 0)

        # UL.output(self._addr_out, kts.const(0, 16))
        # UL.output(self._wen, 0)
        # UL.output(self._data_out, kts.const(0, 16))
        UL.output(self._inc_seg_addr, 0)
        UL.output(self._clr_seg_addr, 0)
        UL.output(self._inc_coord_addr, 0)
        UL.output(self._clr_coord_addr, 0)
        UL.output(self._inc_seg_ctr, 0)
        UL.output(self._clr_seg_ctr, 0)
        UL.output(self._set_curr_coord, self._new_coord)
        UL.output(self._clr_curr_coord, 0)
        # Pop below the stop level
        # UL.output(self._infifo_pop[0], self._stop_in & (self._data_infifo_data_in > self._stop_lvl))

        # Don't actually pop
        UL.output(self._infifo_pop[0], 0)
        # UL.output(self._infifo_pop[0], 0)
        UL.output(self._infifo_pop[1], 0)
        UL.output(self._clr_wen_made, 1)
        UL.output(self._set_block_size, 0)
        UL.output(self._inc_block_write, 0)
        UL.output(self._clr_block_write, 0)

        #######
        # UL_EMIT
        #######
        UL_EMIT.output(self._data_to_fifo, kts.ternary(self._stop_in,
                                                            self._seg_ctr,
                                                            self._data_infifo_data_in))
        UL_EMIT.output(self._op_to_fifo, 1)
        UL_EMIT.output(self._addr_to_fifo, kts.ternary(self._stop_in,
                                                            self._seg_addr,
                                                            self._coord_addr))
        UL_EMIT.output(self._ID_to_fifo, kts.ternary(self._stop_in,
                                                        kts.const(0, 16),
                                                        kts.const(1, 16)))
        UL_EMIT.output(self._push_to_outs, self._data_infifo_valid_in & self._join_out_ready & ~self._data_done_in)
        UL_EMIT.output(self._inc_seg_addr, self._stop_in & self._join_out_ready & ~self._data_done_in)
        UL_EMIT.output(self._clr_seg_addr, 0)
        UL_EMIT.output(self._inc_coord_addr, ~self._data_infifo_eos_in & self._data_infifo_valid_in & self._join_out_ready)
        UL_EMIT.output(self._clr_coord_addr, 0)
        UL_EMIT.output(self._inc_seg_ctr, ~self._data_infifo_eos_in & self._data_infifo_valid_in & self._join_out_ready)
        UL_EMIT.output(self._clr_seg_ctr, 0)
        UL_EMIT.output(self._set_curr_coord, self._new_coord)
        # Make sure to clear the coord on segment emissions so it doesn't get reused
        UL_EMIT.output(self._clr_curr_coord, ~self._wen_made)
        # Assumption is that valid sets of coordinates are always passed here so I should be able to hit new data
        # Pop until we have a DONE
        UL_EMIT.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._join_out_ready | self._data_done_in))
        UL_EMIT.output(self._infifo_pop[1], 0)
        # UL_EMIT.output(self._clr_wen_made, 0)
        UL_EMIT.output(self._clr_wen_made, self._wen_made & self._data_infifo_valid_in)
        UL_EMIT.output(self._set_block_size, 0)
        UL_EMIT.output(self._inc_block_write, 0)
        UL_EMIT.output(self._clr_block_write, 0)

        #############
        # FINALIZE1
        #############
        FINALIZE1.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        FINALIZE1.output(self._op_to_fifo, 0)
        FINALIZE1.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        FINALIZE1.output(self._ID_to_fifo, kts.const(1, 16))
        FINALIZE1.output(self._push_to_outs, 1)

        # DONE.output(self._addr_out, kts.const(0, 16))
        # DONE.output(self._wen, 0)
        # DONE.output(self._data_out, kts.const(0, 16))
        FINALIZE1.output(self._inc_seg_addr, 0)
        FINALIZE1.output(self._clr_seg_addr, 0)
        FINALIZE1.output(self._inc_coord_addr, 0)
        FINALIZE1.output(self._clr_coord_addr, 0)
        FINALIZE1.output(self._inc_seg_ctr, 0)
        FINALIZE1.output(self._clr_seg_ctr, 0)
        FINALIZE1.output(self._set_curr_coord, 0)
        FINALIZE1.output(self._clr_curr_coord, 0)
        FINALIZE1.output(self._infifo_pop[0], 0)
        FINALIZE1.output(self._infifo_pop[1], 0)
        FINALIZE1.output(self._clr_wen_made, 0)
        FINALIZE1.output(self._set_block_size, 0)
        FINALIZE1.output(self._inc_block_write, 0)
        FINALIZE1.output(self._clr_block_write, 0)

        #############
        # FINALIZE2
        #############
        FINALIZE2.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        FINALIZE2.output(self._op_to_fifo, 0)
        FINALIZE2.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        FINALIZE2.output(self._ID_to_fifo, kts.const(0, 16))
        FINALIZE2.output(self._push_to_outs, 1)

        # DONE.output(self._addr_out, kts.const(0, 16))
        # DONE.output(self._wen, 0)
        # DONE.output(self._data_out, kts.const(0, 16))
        FINALIZE2.output(self._inc_seg_addr, 0)
        FINALIZE2.output(self._clr_seg_addr, 0)
        FINALIZE2.output(self._inc_coord_addr, 0)
        FINALIZE2.output(self._clr_coord_addr, 0)
        FINALIZE2.output(self._inc_seg_ctr, 0)
        FINALIZE2.output(self._clr_seg_ctr, 0)
        FINALIZE2.output(self._set_curr_coord, 0)
        FINALIZE2.output(self._clr_curr_coord, 0)
        FINALIZE2.output(self._infifo_pop[0], 0)
        FINALIZE2.output(self._infifo_pop[1], 0)
        FINALIZE2.output(self._clr_wen_made, 0)
        FINALIZE2.output(self._set_block_size, 0)
        FINALIZE2.output(self._inc_block_write, 0)
        FINALIZE2.output(self._clr_block_write, 0)

        #############
        # DONE
        #############
        DONE.output(self._data_to_fifo, kts.const(0, self._data_to_fifo.width))
        DONE.output(self._op_to_fifo, 0)
        DONE.output(self._addr_to_fifo, kts.const(0, self._addr_to_fifo.width))
        DONE.output(self._ID_to_fifo, kts.const(0, 16))
        DONE.output(self._push_to_outs, 0)

        # DONE.output(self._addr_out, kts.const(0, 16))
        # DONE.output(self._wen, 0)
        # DONE.output(self._data_out, kts.const(0, 16))
        DONE.output(self._inc_seg_addr, 0)
        DONE.output(self._clr_seg_addr, 0)
        DONE.output(self._inc_coord_addr, 0)
        DONE.output(self._clr_coord_addr, 0)
        DONE.output(self._inc_seg_ctr, 0)
        DONE.output(self._clr_seg_ctr, 0)
        DONE.output(self._set_curr_coord, 0)
        DONE.output(self._clr_curr_coord, 0)
        DONE.output(self._infifo_pop[0], 0)
        DONE.output(self._infifo_pop[1], 0)
        DONE.output(self._clr_wen_made, 0)
        DONE.output(self._set_block_size, 0)
        DONE.output(self._inc_block_write, 0)
        DONE.output(self._clr_block_write, 0)
        # If doing the blank is not done and should be, we set it here then
        # let the write scanner do its thing
        # DONE.output(self._set_blank_done, self._init_blank & ~self._blank_done & self._spacc_mode)
        DONE.output(self._set_blank_done, 0)
        # We should only clear this for next tile - meaning we get the real done in
        # JK we should clear the blank done when we get the appropriate stop token in.
        # DONE.output(self._clr_blank_done, self._init_blank & self._blank_done & self._data_done_in & self._spacc_mode)
        # DONE.output(self._clr_blank_done, self._init_blank & self._blank_done & self._stop_lvl_geq_p1_sticky & self._spacc_mode)
        DONE.output(self._clr_blank_done, 0)
        DONE.output(self._in_done, 1)

        self.scan_fsm.set_start_state(START)

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

            # Count up how many
            mem_request_ctr = add_counter(self, 'mem_request_num', 64,
                                          increment=self._clk_en & self._data_out_fifo_push & ~self._data_out_fifo_full)

            # Start when any of the coord inputs is valid
            self._start_signal = sticky_flag(self, self._data_in_valid_in,
                                             name='start_indicator')
            self.add_performance_indicator(self._start_signal, edge='posedge', label='start', cycle_count=cyc_count)

            # End when we see DONE on the output ref signal
            self._done_signal = sticky_flag(self, self._in_done,
                                            name='done_indicator')
            # self._done_signal = sticky_flag(self, (self._data_out == MemoryController.DONE_PROXY) &
            #                                         self._data_out[MemoryController.EOS_BIT] & self._data_out_valid_out,
            #                                         name='done_indicator')
            self.add_performance_indicator(self._done_signal, edge='posedge', label='done',
                                           cycle_count=cyc_count)

            self.add_performance_indicator(self._done_signal, edge='posedge', label='ops',
                                           cycle_count=mem_request_ctr)

        # Finally, lift the config regs...
        lift_config_reg(self.internal_generator)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "write_scanner"

    # def get_bitstream(self, inner_offset, compressed=0, lowest_level=0, stop_lvl=0, block_mode=0):
    # def get_bitstream(self, compressed=0, lowest_level=0, stop_lvl=0, block_mode=0):
    def get_bitstream(self, config_kwargs):

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        compressed = config_kwargs['compressed']
        lowest_level = config_kwargs['lowest_level']
        # stop_lvl = config_kwargs['stop_lvl']
        block_mode = config_kwargs['block_mode']
        # spacc_mode = config_kwargs['spacc_mode']
        init_blank = config_kwargs['init_blank']

        # Store all configurations here
        config = [
            # ("inner_dim_offset", inner_offset),
            ("compressed", compressed),
            ("lowest_level", lowest_level),
            # ("stop_lvl", stop_lvl),
            ("block_mode", block_mode),
            # ("spacc_mode", spacc_mode),
            ("init_blank", init_blank),
            ("tile_en", 1)]

        return trim_config_list(flattened, config)


if __name__ == "__main__":
    scanner_dut = WriteScanner(data_width=16, defer_fifos=False,
                               perf_debug=True)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(scanner_dut, filename="write_scanner.sv",
            optimize_if=False)
