from turtle import width
import kratos as kts
from kratos import *
from lake.modules.arbiter import Arbiter
from lake.passes.passes import lift_config_reg
from lake.utils.util import add_counter, register, sticky_flag, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class BuffetLike(Generator):
    def __init__(self,
                 data_width=16,
                 num_ID=2):

        super().__init__(f"buffet_like_{data_width}", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True
        self.num_ID = num_ID

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

        # Need an ID to identify which buffet is being accessed
        # self._ID = self.input("ID", self.data_width)
        # self._ID.add_attribute(ConfigRegAttr("Identifier for the buffet controller being addressed"))

        self._buffet_capacity = self.input("buffet_capacity", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        self._buffet_capacity.add_attribute(ConfigRegAttr("Capacity of buffet..."))

        ### WRITE SIDE
        # self._random_write = self.input("random_write")
        # self._random_write.add_attribute(ConfigRegAttr("If we are using random write or linear write..."))

        # Accept an address over the line
        self._wr_ID = self.input("wr_ID", self.data_width, explicit_array=True, packed=True)
        self._wr_ID.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._wr_ID_ready = self.output("wr_ID_ready", 1)
        self._wr_ID_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._wr_ID_valid = self.input("wr_ID_valid", 1)
        self._wr_ID_valid.add_attribute(ControlSignalAttr(is_control=True))

        # Accept an address over the line
        self._wr_addr = self.input("wr_addr", self.data_width, explicit_array=True, packed=True)
        self._wr_addr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._wr_addr_ready = self.output("wr_addr_ready", 1)
        self._wr_addr_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._wr_addr_valid = self.input("wr_addr_valid", 1)
        self._wr_addr_valid.add_attribute(ControlSignalAttr(is_control=True))

        # Accept data + op over the line
        self._wr_data = self.input("wr_data", self.data_width, explicit_array=True, packed=True)
        self._wr_data.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # Indicates allocate/finalize or write
        self._wr_op = self.input("wr_op", 1)
        self._wr_op.add_attribute(ControlSignalAttr(is_control=True))

        self._wr_data_ready = self.output("wr_data_ready", 1)
        self._wr_data_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._wr_data_valid = self.input("wr_data_valid", 1)
        self._wr_data_valid.add_attribute(ControlSignalAttr(is_control=True))

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

        # Read ID
        self._rd_ID_ready = self.output("rd_op_ready", 1)
        self._rd_ID_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_ID_valid = self.input("rd_op_valid", 1)
        self._rd_ID_valid.add_attribute(ControlSignalAttr(is_control=True))

        self._rd_ID = self.input("rd_ID", self.data_width, explicit_array=True, packed=True)
        self._rd_ID.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # Read response channel
        self._rd_rsp_data = self.output("rd_rsp_data", self.data_width, explicit_array=True, packed=True)
        self._rd_rsp_data.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._rd_rsp_ready = self.input("rd_rsp_ready", 1)
        self._rd_rsp_ready.add_attribute(ControlSignalAttr(is_control=True))

        self._rd_rsp_valid = self.output("rd_rsp_valid", 1)
        self._rd_rsp_valid.add_attribute(ControlSignalAttr(is_control=False))

        # Need interface to memory...
        self._addr_to_mem = self.output("addr_to_mem", self.data_width)
        self._addr_to_mem.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._data_to_mem = self.output("data_to_mem", self.data_width)
        self._data_to_mem.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._wen_to_mem = self.output("wen_to_mem", 1)
        self._wen_to_mem.add_attribute(ControlSignalAttr(is_control=False))

        self._ren_to_mem = self.output("ren_to_mem", 1)
        self._ren_to_mem.add_attribute(ControlSignalAttr(is_control=False))

        self._data_from_mem = self.input("data_from_mem", self.data_width)
        self._data_from_mem.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_from_mem = self.input("valid_from_mem", 1)
        self._valid_from_mem.add_attribute(ControlSignalAttr(is_control=True))

        self._ready_from_mem = self.input("ready_from_mem", 1)
        self._ready_from_mem.add_attribute(ControlSignalAttr(is_control=True))

# =============================
# FIFO inputs
# =============================

        # WR OP fifo
        self._wr_data_fifo_pop = self.var("wr_data_fifo_pop", 1)
        self._wr_data_fifo_valid = self.var("wr_data_fifo_valid", 1)

        self._wr_data_fifo_in = kts.concat(self._wr_data, self._wr_op)
        self._wr_data_infifo = RegFIFO(data_width=self._wr_data_fifo_in.width, width_mult=1, depth=8)
        self._wr_data_fifo_out_data = self.var("wr_data_fifo_out_data", self.data_width)
        self._wr_data_fifo_out_op = self.var("wr_data_fifo_out_op", 1)

        self.add_child(f"wr_data_fifo",
                       self._wr_data_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._wr_data_valid,
                       pop=self._wr_data_fifo_pop,
                       data_in=self._wr_data_fifo_in,
                       data_out=kts.concat(self._wr_data_fifo_out_data, self._wr_data_fifo_out_op))

        self.wire(self._wr_data_ready, ~self._wr_data_infifo.ports.full)
        self.wire(self._wr_data_fifo_valid, ~self._wr_data_infifo.ports.empty)

        # WR ADDR fifo
        self._wr_addr_fifo_pop = self.var("wr_addr_fifo_pop", 1)
        self._wr_addr_fifo_valid = self.var("wr_addr_fifo_valid", 1)

        self._wr_addr_fifo_in = kts.concat(self._wr_addr)
        self._wr_addr_infifo = RegFIFO(data_width=self._wr_addr_fifo_in.width, width_mult=1, depth=8)
        self._wr_addr_fifo_out_data = self.var("wr_addr_fifo_out_data", self.data_width)

        self.add_child(f"wr_addr_fifo",
                       self._wr_addr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._wr_addr_valid,
                       pop=self._wr_addr_fifo_pop,
                       data_in=self._wr_addr_fifo_in,
                       data_out=kts.concat(self._wr_addr_fifo_out_data))

        self.wire(self._wr_addr_ready, ~self._wr_addr_infifo.ports.full)
        self.wire(self._wr_addr_fifo_valid, ~self._wr_addr_infifo.ports.empty)

        # WR ID fifo
        self._wr_ID_fifo_pop = self.var("wr_ID_fifo_pop", 1)
        self._wr_ID_fifo_valid = self.var("wr_ID_fifo_valid", 1)

        self._wr_ID_fifo_in = kts.concat(self._wr_ID)
        self._wr_ID_infifo = RegFIFO(data_width=self._wr_ID_fifo_in.width, width_mult=1, depth=8)
        self._wr_ID_fifo_out_data = self.var("wr_ID_fifo_out_data", self.data_width)

        self.add_child(f"wr_ID_fifo",
                       self._wr_ID_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._wr_ID_valid,
                       pop=self._wr_ID_fifo_pop,
                       data_in=self._wr_ID_fifo_in,
                       data_out=kts.concat(self._wr_ID_fifo_out_data))

        self.wire(self._wr_ID_ready, ~self._wr_ID_infifo.ports.full)
        self.wire(self._wr_ID_fifo_valid, ~self._wr_ID_infifo.ports.empty)

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

        # RD ID fifo
        self._rd_ID_fifo_pop = self.var("rd_ID_fifo_pop", 1)
        self._rd_ID_fifo_valid = self.var("rd_ID_fifo_valid", 1)

        self._rd_ID_fifo_in = kts.concat(self._rd_ID)
        self._rd_ID_infifo = RegFIFO(data_width=self._rd_ID_fifo_in.width, width_mult=1, depth=8)
        self._rd_ID_fifo_out_data = self.var("rd_ID_fifo_out_data", self.data_width)

        self.add_child(f"rd_ID_fifo",
                       self._rd_ID_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_ID_valid,
                       pop=self._rd_ID_fifo_pop,
                       data_in=self._rd_ID_fifo_in,
                       data_out=kts.concat(self._rd_ID_fifo_out_data))

        self.wire(self._rd_ID_ready, ~self._rd_ID_infifo.ports.full)
        self.wire(self._rd_ID_fifo_valid, ~self._rd_ID_infifo.ports.empty)

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

# # =============================
# #  FSM
# # =============================

        # Read block base and bounds...
        self._blk_base = self.var("blk_base", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        self._blk_bounds = self.var("blk_bounds", self.data_width, size=self.num_ID, explicit_array=True, packed=True)

        # self._inc_wr_addr = self.var("inc_wr_addr", 1)
        # self._wr_addr = add_counter(self, "write_addr", bitwidth=self.data_width, increment=self._inc_wr_addr)
        self._wr_addr = self.var("write_addr", self.data_width)
        self._wen = self.var("wen", 1)

        # Read side address + ren
        self._rd_addr_loc = self.var("rd_addr_loc", self.data_width)
        self._ren = self.var("ren", 1)

        # Need to define which side (read/write) has access to the memory port
        self._mem_acq = self.var("mem_acq", 2)

        self._inc_bounds_ctr = self.var("inc_bounds_ctr", 1)
        self._clr_bounds_ctr = self.var("clr_bounds_ctr", 1)
        self._bounds_ctr = add_counter(self, "bounds_ctr", bitwidth=self.data_width,
                                       increment=self._inc_bounds_ctr, clear=self._clr_bounds_ctr)

        self._en_curr_base = self.var("en_curr_base", 1)
        # self._en_curr_bounds = self.var("en_curr_bounds", 1)

        self._curr_base = register(self, self._wr_addr, enable=self._en_curr_base)
        # self._curr_bounds = register(self, self._bounds_ctr, enable=self._en_curr_bounds)

        self._push_blk = self.var("push_blk", self.num_ID)
        self._pop_blk = self.var("pop_blk", self.num_ID)
        self._blk_valid = self.var("blk_valid", self.num_ID)
        self._blk_full = self.var("blk_full", self.num_ID)

        self._curr_capacity = self.var("curr_capacity", self.data_width, size=self.num_ID, explicit_array=True, packed=True)

        @always_ff((posedge, self._clk), (negedge, self._rst_n))
        def cap_reg(self, idx):
            if ~self._rst_n:
                self._curr_capacity[idx] = kts.const(0, self.data_width)
            else:
                self._curr_capacity[idx] = self._curr_capacity[idx] + self._inc_wr_addr - kts.ternary(self._pop_blk, self._blk_bounds, kts.const(0, width=self.data_width))

        [self.add_code(cap_reg, idx=i) for i in range(self.num_ID)]

        # Create FSM
        self.write_fsm = self.add_fsm("write_fsm", reset_high=False)
        WR_START = [self.write_fsm.add_state(f"WR_START_{i}") for i in range(self.num_ID)]
        WRITING = self.write_fsm.add_state("WRITING")

        ####################
        # Next State Logic WRITE
        ####################

        ####################
        # WR_START #
        ####################
        # Start state gets an allocate command
        WR_START.next(WRITING, self._wr_fifo_valid & (self._wr_fifo_out_op == 0))
        WR_START.next(WR_START, None)

        ####################
        # WRITING #
        ####################
        # Writing until we get a finalize...
        WRITING.next(WR_START, self._wr_fifo_valid & (self._wr_fifo_out_op == 0) & ~self._blk_full)
        WRITING.next(WRITING, None)

        self.write_fsm.output(self._inc_wr_addr)
        self.write_fsm.output(self._inc_bounds_ctr)
        self.write_fsm.output(self._clr_bounds_ctr)
        self.write_fsm.output(self._push_blk)
        self.write_fsm.output(self._en_curr_base)
        self.write_fsm.output(self._wen)
        self.write_fsm.output(self._wr_fifo_pop)
        # self.write_fsm.output(self._en_curr_bounds)

        ####################
        # Output Logic WRITE
        ####################

        ####################
        # WR_START #
        ####################
        WR_START.output(self._inc_wr_addr, 0)
        WR_START.output(self._inc_bounds_ctr, 0)
        WR_START.output(self._clr_bounds_ctr, self._wr_fifo_valid & (self._wr_fifo_out_op == 0))
        WR_START.output(self._push_blk, 0)
        WR_START.output(self._en_curr_base, self._wr_fifo_valid & (self._wr_fifo_out_op == 0))
        WR_START.output(self._wen, 0)
        WR_START.output(self._wr_fifo_pop, 0)
        # WR_START.output(self._en_curr_bounds, 0)

        ####################
        # WRITING #
        ####################
        # Increment wr addr if we get wr access
        WRITING.output(self._inc_wr_addr, self._mem_acq[0] & self._wr_fifo_valid & (self._wr_fifo_out_op == 1))
        WRITING.output(self._inc_bounds_ctr, self._mem_acq[0] & self._wr_fifo_valid & (self._wr_fifo_out_op == 1))
        WRITING.output(self._clr_bounds_ctr, 0)
        WRITING.output(self._push_blk, self._wr_fifo_valid & (self._wr_fifo_out_op == 0) & ~self._blk_full)
        WRITING.output(self._en_curr_base, 0)
        WRITING.output(self._wen, self._wr_fifo_valid & (self._wr_fifo_out_op == 1) & ~(self._curr_capacity < self._buffet_capacity))
        WRITING.output(self._wr_fifo_pop, self._mem_acq[0] & self._wr_fifo_valid & (self._wr_fifo_out_op == 1) & ~(self._curr_capacity < self._buffet_capacity))
        # WRITING.output(self._en_curr_bounds, 0)

        ##### Create read side fsm separately.

        #####################
        # READING #
        #####################

        self.read_fsm = self.add_fsm("read_fsm", reset_high=False)
        RD_START = self.read_fsm.add_state("RD_START")

        ####################
        # RD_START
        ####################
        # Get the first block size...
        RD_START.next(RD_START, None)

        self.read_fsm.output(self._pop_blk)
        self.read_fsm.output(self._rd_addr_loc)
        self.read_fsm.output(self._ren)
        self.read_fsm.output(self._rd_op_fifo_pop)
        self.read_fsm.output(self._rd_rsp_fifo_push)

        ####################
        # RD_START
        ####################
        RD_START.output(self._pop_blk, (self._rd_op_fifo_out_op == 0) & self._rd_op_fifo_valid)
        RD_START.output(self._rd_addr_loc, self._rd_op_fifo_out_addr)
        # Guarantee there's room for the read to land
        RD_START.output(self._ren, (self._rd_op_fifo_out_op == 1) & self._rd_op_fifo_valid & ~self._rd_rsp_fifo_full & self._blk_valid)
        # RD_START.output(self._rd_op_fifo_pop, self._mem_acq[1] & (self._rd_op_fifo_out_op == 1) & self._rd_op_fifo_valid & ~self._rd_rsp_fifo_full)
        # Pop the op fifo if there is a read that's going through or if it's a free op
        RD_START.output(self._rd_op_fifo_pop, kts.ternary(self._rd_op_fifo_out_op == 1, self._mem_acq[1] & ~self._rd_rsp_fifo_full, kts.const(1, 1)) & self._rd_op_fifo_valid)
        RD_START.output(self._rd_rsp_fifo_push, self._valid_from_mem)

        self.write_fsm.set_start_state(WR_START)
        self.read_fsm.set_start_state(RD_START)

        ### Bookkeeping FIFO
        self._blk_fifo_in = kts.concat(self._curr_base, self._bounds_ctr)
        self._blk_fifo = RegFIFO(data_width=self._blk_fifo_in.width, width_mult=1, depth=8)

        self.add_child(f"blk_fifo",
                       self._blk_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._push_blk,
                       pop=self._pop_blk,
                       data_in=self._blk_fifo_in,
                       data_out=kts.concat(self._blk_base, self._blk_bounds))

        self.wire(self._blk_full, self._blk_fifo.ports.full)
        self.wire(self._blk_valid, ~self._blk_fifo.ports.empty)

        # Force FSM realization first so that flush gets added...
        kts.passes.realize_fsm(self.internal_generator)

        # Arbitrate between the write/read side with RR arbiter
        self.port_arbiter = Arbiter(ins=2,
                                    algo="RR")
        self.add_child(f"rr_arbiter",
                       self.port_arbiter,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       request_in=kts.concat(self._wen, self._ren),
                       grant_out=self._mem_acq,
                       resource_ready=self._ready_from_mem)

        self.wire(self._addr_to_mem, kts.ternary(self._mem_acq[0], self._wr_addr, self._rd_addr_loc + self._blk_base))
        self.wire(self._data_to_mem, self._wr_fifo_out_data)
        self.wire(self._ren_to_mem, self._ren & self._mem_acq[1])
        self.wire(self._wen_to_mem, self._wen & self._mem_acq[0])

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
