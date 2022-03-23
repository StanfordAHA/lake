import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.utils.util import add_counter, register, sticky_flag, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class BuffetLike(Generator):
    def __init__(self,
                 data_width=16):

        super().__init__(f"buffet_like_{data_width}", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True

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

        # Buffet-like has a write side and a read side

        ### WRITE SIDE
        self._wr_data = self.input("wr_data", self.data_width, explicit_array=True, packed=True)
        self._wr_data.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._wr_valid = self.input("wr_valid", 1)
        self._wr_valid.add_attribute(ControlSignalAttr(is_control=True))

        # Indicates allocate/finalize or write
        self._wr_op = self.input("wr_op", 1)
        self._wr_op.add_attribute(ControlSignalAttr(is_control=True))

        self._wr_ready = self.output("wr_ready", 1)
        self._wr_ready.add_attribute(ControlSignalAttr(is_control=False))

        ### READ SIDE
        # On read side need both a request and response channel
        self._rd_op_ready = self.output("rd_op_ready", 1)
        self._rd_op_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_op_valid = self.input("rd_op_valid", 1)
        self._rd_op_valid.add_attribute(ControlSignalAttr(is_control=True))

        self._rd_addr = self.input("rd_addr", self.data_width, explicit_array=True, packed=True)
        self._rd_addr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # Free or Read
        self._rd_op_op = self.input("rd_op_op", 1)
        self._rd_op_op.add_attribute(ControlSignalAttr(is_control=True))

        # Read response channel
        self._rd_rsp_data = self.output("rd_rsp_data", self.data_width, explicit_array=True, packed=True)
        self._rd_rsp_data.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._rd_rsp_ready = self.input("rd_rsp_ready", 1)
        self._rd_rsp_ready.add_attribute(ControlSignalAttr(is_control=True))

        self._rd_rsp_valid = self.output("rd_rsp_valid", 1)
        self._rd_rsp_valid.add_attribute(ControlSignalAttr(is_control=False))

# =============================
# FIFO inputs
# =============================

        # WR OP fifo
        self._wr_fifo_pop = self.var("wr_fifo_pop", 1)
        self._wr_fifo_valid = self.var("wr_fifo_valid", 1)

        self._wr_fifo_in = kts.concat(self._wr_data, self._wr_op)
        self._wr_infifo = RegFIFO(data_width=self._wr_fifo_in.width, width_mult=1, depth=8)
        self._wr_fifo_out_data = self.var("wr_fifo_out_data", self.data_width)
        self._wr_fifo_out_op = self.var("wr_fifo_out_op", 1)

        self.add_child(f"wr_fifo",
                       self._wr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._wr_valid,
                       pop=self._wr_fifo_pop,
                       data_in=self._wr_fifo_in,
                       data_out=kts.concat(self._wr_fifo_out_data, self._wr_fifo_out_op))

        self.wire(self._wr_ready, ~self._wr_infifo.ports.full)
        self.wire(self._wr_fifo_valid, ~self._wr_infifo.ports.empty)

        # RD OP fifo
        self._rd_op_fifo_pop = self.var("rd_op_fifo_pop", 1)
        self._rd_op_fifo_valid = self.var("rd_op_fifo_valid", 1)

        self._rd_op_fifo_in = kts.concat(self._rd_addr, self._rd_op_op)
        self._rd_op_infifo = RegFIFO(data_width=self._rd_op_fifo_in.width, width_mult=1, depth=8)
        self._rd_op_fifo_out_addr = self.var("rd_op_fifo_out_addr", self.data_width)
        self._rd_op_fifo_out_op = self.var("rd_op_fifo_out_op", 1)

        self.add_child(f"rd_op_fifo",
                       self._rd_op_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_op_valid,
                       pop=self._rd_op_fifo_pop,
                       data_in=self._rd_op_fifo_in,
                       data_out=kts.concat(self._rd_op_fifo_out_addr, self._rd_op_fifo_out_op))

        self.wire(self._rd_op_ready, ~self._rd_op_infifo.ports.full)
        self.wire(self._rd_op_fifo_valid, ~self._rd_op_infifo.ports.empty)

# =============================
# FIFO outputs
# =============================

        self._rd_rsp_fifo_push = self.var("rd_rsp_fifo_push", 1)
        self._rd_rsp_fifo_full = self.var("rd_rsp_fifo_full", 1)

        self._rd_rsp_fifo_in_data = self.var("rd_rsp_fifo_in_data", self.data_width, packed=True)
        self._rd_rsp_outfifo = RegFIFO(data_width=self._rd_rsp_fifo_in_data.width, width_mult=1, depth=8)

        self.add_child(f"rd_rsp_fifo",
                       self._rd_rsp_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_rsp_fifo_push,
                       pop=self._rd_rsp_ready,
                       data_in=self._rd_rsp_fifo_in_data,
                       data_out=self._rd_rsp_data)

        self.wire(self._rd_rsp_fifo_full, self._rd_rsp_outfifo.ports.full)
        self.wire(self._rd_rsp_valid, ~self._rd_rsp_outfifo.ports.empty)

#         self._addr_infifo = RegFIFO(data_width=1 * self.data_width + 1, width_mult=1, depth=8)
#         self._infifo_pop = self.var("infifo_pop", 2)

#         # For input streams, need coord_in, valid_in, eos_in
#         self._data_infifo_data_in = self.var("data_infifo_data_in", self.data_width)
#         # self._infifo_coord_in = self.var("infifo_coord_in", self.data_width)
#         self._data_infifo_valid_in = self.var("data_infifo_valid_in", 1)
#         self._data_infifo_eos_in = self.var("data_infifo_eos_in", 1)

#         # Stupid convert
#         self._data_infifo_in_packed = self.var("data_infifo_in_packed", 1 * self.data_width + 1, packed=True)
#         self.wire(self._data_infifo_in_packed[self.data_width], self._eos_in[0])
#         self.wire(self._data_infifo_in_packed[self.data_width - 1, 0], self._data_in[0])
#         self._data_infifo_out_packed = self.var("data_infifo_out_packed", 1 * self.data_width + 1, packed=True)
#         self.wire(self._data_infifo_eos_in, self._data_infifo_out_packed[self.data_width])
#         self.wire(self._data_infifo_data_in, self._data_infifo_out_packed[self.data_width - 1, 0])


#         # For input streams, need coord_in, valid_in, eos_in
#         self._addr_infifo_data_in = self.var("addr_infifo_data_in", self.data_width)
#         # self._infifo_coord_in = self.var("infifo_coord_in", self.data_width)
#         self._addr_infifo_valid_in = self.var("addr_infifo_valid_in", 1)
#         self._addr_infifo_eos_in = self.var("addr_infifo_eos_in", 1)

#         self._addr_infifo_in_packed = self.var("addr_infifo_in_packed", 1 * self.data_width + 1, packed=True)
#         self.wire(self._addr_infifo_in_packed[self.data_width], self._eos_in[1])
#         self.wire(self._addr_infifo_in_packed[self.data_width - 1, 0], self._data_in[1])
#         self._addr_infifo_out_packed = self.var("addr_infifo_out_packed", 1 * self.data_width + 1, packed=True)
#         self.wire(self._addr_infifo_eos_in, self._addr_infifo_out_packed[self.data_width])
#         self.wire(self._addr_infifo_data_in, self._addr_infifo_out_packed[self.data_width - 1, 0])

#         self.add_child(f"addr_input_fifo",
#                        self._addr_infifo,
#                        clk=self._gclk,
#                        rst_n=self._rst_n,
#                        clk_en=self._clk_en,
#                        push=self._valid_in[1],
#                        pop=self._infifo_pop[1],
#                        data_in=self._addr_infifo_in_packed,
#                        data_out=self._addr_infifo_out_packed)

#         self.wire(self._ready_out[1], ~self._addr_infifo.ports.full)
#         self.wire(self._addr_infifo_valid_in, ~self._addr_infifo.ports.empty)

#         # State for block writes
#         self._set_block_size = self.var("set_block_size", 1)
#         self._block_size = register(self, self._data_infifo_data_in, self._set_block_size, name="block_size")

#         self._inc_block_write = self.var("inc_block_write", 1)
#         self._clr_block_write = self.var("clr_block_write", 1)
#         self._block_writes = add_counter(self, "block_write_count", 16, increment=self._inc_block_write, clear=self._clr_block_write)

# # =============================
# # SCAN FSM
# # =============================

#         # Address for writing segment
#         self._inc_seg_addr = self.var("inc_seg_addr", 1)
#         self._clr_seg_addr = self.var("clr_seg_addr", 1)
#         self._seg_addr = add_counter(self, "segment_addr", 16, increment=self._inc_seg_addr, clear=self._clr_seg_addr)

#         self._inc_coord_addr = self.var("inc_coord_addr", 1)
#         self._clr_coord_addr = self.var("clr_coord_addr", 1)
#         self._coord_addr = add_counter(self, "coord_addr", 16, increment=self._inc_coord_addr, clear=self._clr_coord_addr)

#         # Value to go to segment
#         self._inc_seg_ctr = self.var("inc_seg_ctr", 1)
#         self._clr_seg_ctr = self.var("clr_seg_ctr", 1)
#         self._seg_ctr = add_counter(self, "segment_counter", 16, increment=self._inc_seg_ctr, clear=self._clr_seg_ctr)

#         self._set_curr_coord = self.var("set_curr_coord", 1)
#         self._clr_curr_coord = self.var("clr_curr_coord", 1)
#         self._curr_coord = register(self, self._data_infifo_data_in, enable=self._set_curr_coord)
#         self._curr_coord_valid = sticky_flag(self, self._set_curr_coord, clear=self._clr_curr_coord, name="valid_coord_sticky", seq_only=True)

#         # Indicates if we are seeing a new coordinate
#         self._new_coord = self.var("new_coord", 1)
#         # We have a new coord if the new coord input is valid and the curr_coord is not valid, or the data is different.
#         self.wire(self._new_coord, (self._data_infifo_valid_in & ~self._data_infifo_eos_in) & (~self._curr_coord_valid | (self._data_infifo_data_in != self._curr_coord)))

#         self._stop_in = self.var("stop_in", 1)
#         self.wire(self._stop_in, self._data_infifo_valid_in & self._data_infifo_eos_in)

#         self._full_stop = self.var("full_stop", 1)
#         self.wire(self._full_stop, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in == 0))

#         self._matching_stop = self.var("matching_stop", 1)
#         self.wire(self._matching_stop, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in == self._stop_lvl))

#         self._clr_wen_made = self.var("clr_wen_made", 1)
#         self._wen_made = sticky_flag(self, self._wen, clear=self._clr_wen_made, name="wen_made", seq_only=True)

#         # Create FSM
#         self.scan_fsm = self.add_fsm("scan_seq", reset_high=False)
#         START = self.scan_fsm.add_state("START")
#         BLOCK_1_SZ = self.scan_fsm.add_state("BLOCK_1_SZ")
#         BLOCK_1_WR = self.scan_fsm.add_state("BLOCK_1_WR")
#         BLOCK_2_SZ = self.scan_fsm.add_state("BLOCK_2_SZ")
#         BLOCK_2_WR = self.scan_fsm.add_state("BLOCK_2_WR")
#         # Lowest level
#         LL = self.scan_fsm.add_state("LL")
#         # Lowest level uncompressed (use address)
#         UnLL = self.scan_fsm.add_state("UnLL")
#         # Lowest level compressed
#         ComLL = self.scan_fsm.add_state("ComLL")
#         UL_WZ = self.scan_fsm.add_state("UL_WZ")
#         UL = self.scan_fsm.add_state("UL")
#         UL_EMIT_COORD = self.scan_fsm.add_state("UL_EMIT_COORD")
#         UL_EMIT_SEG = self.scan_fsm.add_state("UL_EMIT_SEG")
#         DONE = self.scan_fsm.add_state("DONE")

#         ####################
#         # Next State Logic
#         ####################

#         ####################
#         # START #
#         ####################
#         # Start state goes to either lowest level or upper level
#         START.next(BLOCK_1_SZ, self._block_mode)
#         START.next(LL, self._lowest_level)
#         START.next(UL_WZ, ~self._lowest_level)

#         ####################
#         # BLOCK_1_SZ
#         ####################
#         # Get the first block size...
#         BLOCK_1_SZ.next(BLOCK_1_WR, self._data_infifo_valid_in)
#         BLOCK_1_SZ.next(BLOCK_1_SZ, None)

#         ####################
#         # BLOCK_1_WR
#         ####################
#         # Write the first block...
#         # If this is just writing a single structure, end after that
#         BLOCK_1_WR.next(BLOCK_2_SZ, (self._block_writes == self._block_size) & ~self._lowest_level)
#         BLOCK_1_WR.next(DONE, (self._block_writes == self._block_size) & self._lowest_level)
#         BLOCK_1_WR.next(BLOCK_1_WR, None)

#         ####################
#         # BLOCK_2_SZ
#         ####################
#         # Get the second block size...
#         BLOCK_2_SZ.next(BLOCK_2_WR, self._data_infifo_valid_in)
#         BLOCK_2_SZ.next(BLOCK_2_SZ, None)

#         ####################
#         # BLOCK_2_WR
#         ####################
#         # Get the first block size...
#         BLOCK_2_WR.next(DONE, (self._block_writes == self._block_size))
#         BLOCK_2_WR.next(BLOCK_2_WR, None)

#         ####################
#         # LL #
#         ####################
#         # Redundant state but helpful in my head
#         # Go to compressed or uncompressed from here
#         LL.next(ComLL, self._compressed)
#         LL.next(UnLL, ~self._compressed)

#         ####################
#         # ComLL
#         ####################
#         # In the compressed state of lowest level, we only need to write the
#         # data values in order...just watching for the stop 0 token
#         ComLL.next(DONE, self._data_infifo_valid_in & self._data_infifo_eos_in & (self._data_infifo_data_in == 0))
#         ComLL.next(ComLL, None)

#         ####################
#         # UnLL
#         ####################
#         # In the uncompressed lowest level, we are writing the data at the specified address, so we are similarly looking
#         # for stop 0 token
#         UnLL.next(DONE, self._data_infifo_valid_in & self._addr_infifo_valid_in & self._data_infifo_eos_in & self._addr_infifo_eos_in &
#                   (self._data_infifo_data_in == 0) & (self._addr_infifo_data_in == 0))
#         UnLL.next(UnLL, None)

#         ####################
#         # UL_WZ
#         ####################
#         # Need to write a 0 to the segment array first...
#         UL_WZ.next(UL, self._ready_in)
#         UL_WZ.next(UL_WZ, ~self._ready_in)

#         ####################
#         # UL #
#         ####################
#         # ASSUMED TO BE COMPRESSED - OTHERWISE DFG LOOKS DIFFERENT - PERFORMS MATH ON COORDINATES
#         # In the upper level, we will emit new coordinates linearly as we see new ones, reset tracking at stop_lvl
#         UL.next(UL_EMIT_COORD, self._new_coord)
#         UL.next(UL_EMIT_SEG, self._matching_stop)
#         UL.next(UL, None)

#         ####################
#         # UL_EMIT_COORD #
#         ####################
#         # From the emit coord, we will send a write out as long the memory is ready for a write
#         # Then go back to UL once we see new data or a stop in
#         UL_EMIT_COORD.next(UL, self._new_coord | self._stop_in)
#         UL_EMIT_COORD.next(UL_EMIT_COORD, None)

#         ####################
#         # UL_EMIT_SEG #
#         ####################
#         # From the emit seg, we will send out the writes to the segment array, will clear all the state
#         # Should go to done if we see a stop 0
#         # Should only move on once we have drained the subsequent stops and see valid data coming in
#         UL_EMIT_SEG.next(UL, self._data_infifo_valid_in & ~self._data_infifo_eos_in)
#         UL_EMIT_SEG.next(DONE, self._full_stop)
#         UL_EMIT_SEG.next(UL_EMIT_SEG, None)

#         ####################
#         # DONE
#         ####################
#         # We are done...
#         # TODO: Accept multiple blocks
#         DONE.next(DONE, None)

#         ####################
#         # FSM Output Logic
#         ####################

#         self.scan_fsm.output(self._addr_out)
#         self.scan_fsm.output(self._wen)
#         self.scan_fsm.output(self._data_out)
#         self.scan_fsm.output(self._inc_seg_addr)
#         self.scan_fsm.output(self._clr_seg_addr)
#         self.scan_fsm.output(self._inc_coord_addr)
#         self.scan_fsm.output(self._clr_coord_addr)
#         self.scan_fsm.output(self._inc_seg_ctr)
#         self.scan_fsm.output(self._clr_seg_ctr)
#         self.scan_fsm.output(self._set_curr_coord)
#         self.scan_fsm.output(self._clr_curr_coord)
#         self.scan_fsm.output(self._infifo_pop[0])
#         self.scan_fsm.output(self._infifo_pop[1])
#         self.scan_fsm.output(self._clr_wen_made)
#         self.scan_fsm.output(self._set_block_size)
#         self.scan_fsm.output(self._inc_block_write)
#         self.scan_fsm.output(self._clr_block_write)

#         #######
#         # START - TODO - Generate general hardware...
#         #######
#         START.output(self._addr_out, kts.const(0, 16))
#         START.output(self._wen, 0)
#         START.output(self._data_out, kts.const(0, 16))
#         START.output(self._inc_seg_addr, 0)
#         START.output(self._clr_seg_addr, 0)
#         START.output(self._inc_coord_addr, 0)
#         START.output(self._clr_coord_addr, 0)
#         START.output(self._inc_seg_ctr, 0)
#         START.output(self._clr_seg_ctr, 0)
#         START.output(self._set_curr_coord, 0)
#         START.output(self._clr_curr_coord, 0)
#         START.output(self._infifo_pop[0], 0)
#         START.output(self._infifo_pop[1], 0)
#         START.output(self._clr_wen_made, 0)
#         START.output(self._set_block_size, 0)
#         START.output(self._inc_block_write, 0)
#         START.output(self._clr_block_write, 0)

#         #######
#         # BLOCK_1_SZ
#         #######
#         BLOCK_1_SZ.output(self._addr_out, kts.const(0, 16))
#         BLOCK_1_SZ.output(self._wen, 0)
#         BLOCK_1_SZ.output(self._data_out, kts.const(0, 16))
#         BLOCK_1_SZ.output(self._inc_seg_addr, 0)
#         BLOCK_1_SZ.output(self._clr_seg_addr, 0)
#         BLOCK_1_SZ.output(self._inc_coord_addr, 0)
#         BLOCK_1_SZ.output(self._clr_coord_addr, 0)
#         BLOCK_1_SZ.output(self._inc_seg_ctr, 0)
#         BLOCK_1_SZ.output(self._clr_seg_ctr, 0)
#         BLOCK_1_SZ.output(self._set_curr_coord, 0)
#         BLOCK_1_SZ.output(self._clr_curr_coord, 0)
#         BLOCK_1_SZ.output(self._infifo_pop[0], self._data_infifo_valid_in)
#         BLOCK_1_SZ.output(self._infifo_pop[1], 0)
#         BLOCK_1_SZ.output(self._clr_wen_made, 0)
#         BLOCK_1_SZ.output(self._set_block_size, self._data_infifo_valid_in)
#         BLOCK_1_SZ.output(self._inc_block_write, 0)
#         BLOCK_1_SZ.output(self._clr_block_write, 1)

#         #######
#         # BLOCK_1_WR
#         #######
#         BLOCK_1_WR.output(self._addr_out, self._block_writes)
#         BLOCK_1_WR.output(self._wen, self._data_infifo_valid_in & (self._block_writes < self._block_size))
#         BLOCK_1_WR.output(self._data_out, self._data_infifo_data_in)
#         BLOCK_1_WR.output(self._inc_seg_addr, 0)
#         BLOCK_1_WR.output(self._clr_seg_addr, 0)
#         BLOCK_1_WR.output(self._inc_coord_addr, 0)
#         BLOCK_1_WR.output(self._clr_coord_addr, 0)
#         BLOCK_1_WR.output(self._inc_seg_ctr, 0)
#         BLOCK_1_WR.output(self._clr_seg_ctr, 0)
#         BLOCK_1_WR.output(self._set_curr_coord, 0)
#         BLOCK_1_WR.output(self._clr_curr_coord, 0)
#         BLOCK_1_WR.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._ready_in)
#         BLOCK_1_WR.output(self._infifo_pop[1], 0)
#         BLOCK_1_WR.output(self._clr_wen_made, 0)
#         BLOCK_1_WR.output(self._set_block_size, 0)
#         BLOCK_1_WR.output(self._inc_block_write, self._ready_in & self._data_infifo_valid_in & (self._block_writes < self._block_size))
#         BLOCK_1_WR.output(self._clr_block_write, 0)

#         #######
#         # BLOCK_2_SZ
#         #######
#         BLOCK_2_SZ.output(self._addr_out, kts.const(0, 16))
#         BLOCK_2_SZ.output(self._wen, 0)
#         BLOCK_2_SZ.output(self._data_out, kts.const(0, 16))
#         BLOCK_2_SZ.output(self._inc_seg_addr, 0)
#         BLOCK_2_SZ.output(self._clr_seg_addr, 0)
#         BLOCK_2_SZ.output(self._inc_coord_addr, 0)
#         BLOCK_2_SZ.output(self._clr_coord_addr, 0)
#         BLOCK_2_SZ.output(self._inc_seg_ctr, 0)
#         BLOCK_2_SZ.output(self._clr_seg_ctr, 0)
#         BLOCK_2_SZ.output(self._set_curr_coord, 0)
#         BLOCK_2_SZ.output(self._clr_curr_coord, 0)
#         BLOCK_2_SZ.output(self._infifo_pop[0], self._data_infifo_valid_in)
#         BLOCK_2_SZ.output(self._infifo_pop[1], 0)
#         BLOCK_2_SZ.output(self._clr_wen_made, 0)
#         BLOCK_2_SZ.output(self._set_block_size, self._data_infifo_valid_in)
#         BLOCK_2_SZ.output(self._inc_block_write, 0)
#         BLOCK_2_SZ.output(self._clr_block_write, 1)

#         #######
#         # BLOCK_2_WR
#         #######
#         BLOCK_2_WR.output(self._addr_out, self._block_writes + self._inner_dim_offset)
#         BLOCK_2_WR.output(self._wen, self._data_infifo_valid_in & (self._block_writes < self._block_size))
#         BLOCK_2_WR.output(self._data_out, self._data_infifo_data_in)
#         BLOCK_2_WR.output(self._inc_seg_addr, 0)
#         BLOCK_2_WR.output(self._clr_seg_addr, 0)
#         BLOCK_2_WR.output(self._inc_coord_addr, 0)
#         BLOCK_2_WR.output(self._clr_coord_addr, 0)
#         BLOCK_2_WR.output(self._inc_seg_ctr, 0)
#         BLOCK_2_WR.output(self._clr_seg_ctr, 0)
#         BLOCK_2_WR.output(self._set_curr_coord, 0)
#         BLOCK_2_WR.output(self._clr_curr_coord, 0)
#         BLOCK_2_WR.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._block_writes < self._block_size) & self._ready_in)
#         BLOCK_2_WR.output(self._infifo_pop[1], 0)
#         BLOCK_2_WR.output(self._clr_wen_made, 0)
#         BLOCK_2_WR.output(self._set_block_size, 0)
#         BLOCK_2_WR.output(self._inc_block_write, self._ready_in & self._data_infifo_valid_in & (self._block_writes < self._block_size))
#         BLOCK_2_WR.output(self._clr_block_write, 0)

#         #######
#         # LL
#         #######
#         LL.output(self._addr_out, kts.const(0, 16))
#         LL.output(self._wen, 0)
#         LL.output(self._data_out, kts.const(0, 16))
#         LL.output(self._inc_seg_addr, 0)
#         LL.output(self._clr_seg_addr, 0)
#         LL.output(self._inc_coord_addr, 0)
#         LL.output(self._clr_coord_addr, 0)
#         LL.output(self._inc_seg_ctr, 0)
#         LL.output(self._clr_seg_ctr, 0)
#         LL.output(self._set_curr_coord, 0)
#         LL.output(self._clr_curr_coord, 0)
#         LL.output(self._infifo_pop[0], 0)
#         LL.output(self._infifo_pop[1], 0)
#         LL.output(self._clr_wen_made, 0)
#         LL.output(self._set_block_size, 0)
#         LL.output(self._inc_block_write, 0)
#         LL.output(self._clr_block_write, 0)

#         #######
#         # UnLL
#         #######
#         UnLL.output(self._addr_out, self._addr_infifo_data_in)
#         # Only write the values
#         UnLL.output(self._wen, (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ~(self._data_infifo_eos_in | self._addr_infifo_eos_in))
#         UnLL.output(self._data_out, self._data_infifo_data_in)
#         UnLL.output(self._inc_seg_addr, 0)
#         UnLL.output(self._clr_seg_addr, 0)
#         UnLL.output(self._inc_coord_addr, 0)
#         UnLL.output(self._clr_coord_addr, 0)
#         UnLL.output(self._inc_seg_ctr, 0)
#         UnLL.output(self._clr_seg_ctr, 0)
#         UnLL.output(self._set_curr_coord, 0)
#         UnLL.output(self._clr_curr_coord, 0)
#         # Pop if the memory is ready for a write, or its eos
#         UnLL.output(self._infifo_pop[0], (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ((self._data_infifo_eos_in & self._addr_infifo_eos_in) | self._ready_in))
#         UnLL.output(self._infifo_pop[1], (self._data_infifo_valid_in & self._addr_infifo_valid_in) & ((self._data_infifo_eos_in & self._addr_infifo_eos_in) | self._ready_in))
#         UnLL.output(self._clr_wen_made, 0)
#         UnLL.output(self._set_block_size, 0)
#         UnLL.output(self._inc_block_write, 0)
#         UnLL.output(self._clr_block_write, 0)

#         #######
#         # ComLL
#         #######
#         # Use the seg addr
#         ComLL.output(self._addr_out, self._seg_addr)
#         # Only write if its data
#         ComLL.output(self._wen, self._data_infifo_valid_in & ~self._data_infifo_eos_in)
#         ComLL.output(self._data_out, self._data_infifo_data_in)
#         # Increase the seg addr only if we are actually writing
#         ComLL.output(self._inc_seg_addr, self._data_infifo_valid_in & ~self._data_infifo_eos_in & self._ready_in)
#         ComLL.output(self._clr_seg_addr, 0)
#         ComLL.output(self._inc_coord_addr, 0)
#         ComLL.output(self._clr_coord_addr, 0)
#         ComLL.output(self._inc_seg_ctr, 0)
#         ComLL.output(self._clr_seg_ctr, 0)
#         ComLL.output(self._set_curr_coord, 0)
#         ComLL.output(self._clr_curr_coord, 0)
#         # Only pop if its eos or the memory is ready for the write
#         ComLL.output(self._infifo_pop[0], self._data_infifo_valid_in & (self._data_infifo_eos_in | self._ready_in))
#         ComLL.output(self._infifo_pop[1], 0)
#         ComLL.output(self._clr_wen_made, 0)
#         ComLL.output(self._set_block_size, 0)
#         ComLL.output(self._inc_block_write, 0)
#         ComLL.output(self._clr_block_write, 0)

#         #######
#         # UL_WZ
#         #######
#         # Write a 0 to the segment array
#         UL_WZ.output(self._addr_out, self._seg_addr)
#         UL_WZ.output(self._wen, 1)
#         UL_WZ.output(self._data_out, kts.const(0, 16))
#         UL_WZ.output(self._inc_seg_addr, self._ready_in)
#         UL_WZ.output(self._clr_seg_addr, 0)
#         UL_WZ.output(self._inc_coord_addr, 0)
#         UL_WZ.output(self._clr_coord_addr, 0)
#         UL_WZ.output(self._inc_seg_ctr, 0)
#         UL_WZ.output(self._clr_seg_ctr, 0)
#         UL_WZ.output(self._set_curr_coord, 0)
#         UL_WZ.output(self._clr_curr_coord, 0)
#         UL_WZ.output(self._infifo_pop[0], 0)
#         UL_WZ.output(self._infifo_pop[1], 0)
#         UL_WZ.output(self._clr_wen_made, 0)
#         UL_WZ.output(self._set_block_size, 0)
#         UL_WZ.output(self._inc_block_write, 0)
#         UL_WZ.output(self._clr_block_write, 0)

#         #######
#         # UL
#         #######
#         UL.output(self._addr_out, kts.const(0, 16))
#         UL.output(self._wen, 0)
#         UL.output(self._data_out, kts.const(0, 16))
#         UL.output(self._inc_seg_addr, 0)
#         UL.output(self._clr_seg_addr, 0)
#         UL.output(self._inc_coord_addr, 0)
#         UL.output(self._clr_coord_addr, 0)
#         UL.output(self._inc_seg_ctr, 0)
#         UL.output(self._clr_seg_ctr, 0)
#         UL.output(self._set_curr_coord, self._new_coord)
#         UL.output(self._clr_curr_coord, 0)
#         # Pop below the stop level
#         UL.output(self._infifo_pop[0], self._stop_in & (self._data_infifo_data_in > self._stop_lvl))
#         # UL.output(self._infifo_pop[0], 0)
#         UL.output(self._infifo_pop[1], 0)
#         UL.output(self._clr_wen_made, 1)
#         UL.output(self._set_block_size, 0)
#         UL.output(self._inc_block_write, 0)
#         UL.output(self._clr_block_write, 0)

#         #######
#         # UL_EMIT_COORD
#         #######
#         UL_EMIT_COORD.output(self._addr_out, self._coord_addr + self._inner_dim_offset)
#         UL_EMIT_COORD.output(self._wen, ~self._wen_made & self._ready_in)
#         UL_EMIT_COORD.output(self._data_out, self._curr_coord)
#         UL_EMIT_COORD.output(self._inc_seg_addr, 0)
#         UL_EMIT_COORD.output(self._clr_seg_addr, 0)
#         UL_EMIT_COORD.output(self._inc_coord_addr, ~self._wen_made & self._ready_in)
#         UL_EMIT_COORD.output(self._clr_coord_addr, 0)
#         UL_EMIT_COORD.output(self._inc_seg_ctr, ~self._wen_made & self._ready_in)
#         UL_EMIT_COORD.output(self._clr_seg_ctr, 0)
#         UL_EMIT_COORD.output(self._set_curr_coord, 0)
#         UL_EMIT_COORD.output(self._clr_curr_coord, 0)
#         # Pop until stop in or new coordinate
#         UL_EMIT_COORD.output(self._infifo_pop[0], ~self._new_coord & ~self._stop_in)
#         UL_EMIT_COORD.output(self._infifo_pop[1], 0)
#         UL_EMIT_COORD.output(self._clr_wen_made, 0)
#         UL_EMIT_COORD.output(self._set_block_size, 0)
#         UL_EMIT_COORD.output(self._inc_block_write, 0)
#         UL_EMIT_COORD.output(self._clr_block_write, 0)

#         #######
#         # UL_EMIT_SEG
#         #######
#         UL_EMIT_SEG.output(self._addr_out, self._seg_addr)
#         UL_EMIT_SEG.output(self._wen, ~self._wen_made & self._ready_in)
#         UL_EMIT_SEG.output(self._data_out, self._seg_ctr)
#         UL_EMIT_SEG.output(self._inc_seg_addr, ~self._wen_made & self._ready_in)
#         UL_EMIT_SEG.output(self._clr_seg_addr, 0)
#         UL_EMIT_SEG.output(self._inc_coord_addr, 0)
#         UL_EMIT_SEG.output(self._clr_coord_addr, 0)
#         UL_EMIT_SEG.output(self._inc_seg_ctr, 0)
#         UL_EMIT_SEG.output(self._clr_seg_ctr, 0)
#         UL_EMIT_SEG.output(self._set_curr_coord, 0)
#         # Make sure to clear the coord on segment emissions so it doesn't get reused
#         UL_EMIT_SEG.output(self._clr_curr_coord, 1)
#         # Assumption is that valid sets of coordinates are always passed here so I should be able to hit new data
#         # Pop until we have data in thats not a stop (or we fall through to DONE)
#         UL_EMIT_SEG.output(self._infifo_pop[0], self._data_infifo_valid_in & self._data_infifo_eos_in)
#         UL_EMIT_SEG.output(self._infifo_pop[1], 0)
#         UL_EMIT_SEG.output(self._clr_wen_made, 0)
#         UL_EMIT_SEG.output(self._set_block_size, 0)
#         UL_EMIT_SEG.output(self._inc_block_write, 0)
#         UL_EMIT_SEG.output(self._clr_block_write, 0)

#         #############
#         # DONE
#         #############
#         DONE.output(self._addr_out, kts.const(0, 16))
#         DONE.output(self._wen, 0)
#         DONE.output(self._data_out, kts.const(0, 16))
#         DONE.output(self._inc_seg_addr, 0)
#         DONE.output(self._clr_seg_addr, 0)
#         DONE.output(self._inc_coord_addr, 0)
#         DONE.output(self._clr_coord_addr, 0)
#         DONE.output(self._inc_seg_ctr, 0)
#         DONE.output(self._clr_seg_ctr, 0)
#         DONE.output(self._set_curr_coord, 0)
#         DONE.output(self._clr_curr_coord, 0)
#         DONE.output(self._infifo_pop[0], 0)
#         DONE.output(self._infifo_pop[1], 0)
#         DONE.output(self._clr_wen_made, 0)
#         DONE.output(self._set_block_size, 0)
#         DONE.output(self._inc_block_write, 0)
#         DONE.output(self._clr_block_write, 0)

#         self.scan_fsm.set_start_state(START)

#         # Force FSM realization first so that flush gets added...
#         kts.passes.realize_fsm(self.internal_generator)

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

    def get_bitstream(self, inner_offset, compressed=0, lowest_level=0, stop_lvl=0, block_mode=0):

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Store all configurations here
        config = [
            ("inner_dim_offset", inner_offset),
            ("compressed", compressed),
            ("lowest_level", lowest_level),
            ("stop_lvl", stop_lvl),
            ("block_mode", block_mode)]

        return trim_config_list(flattened, config)


if __name__ == "__main__":
    buffet_dut = BuffetLike(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(buffet_dut, filename="buffet_like.sv",
            optimize_if=False)
