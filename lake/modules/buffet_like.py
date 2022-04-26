import kratos as kts
from kratos import *
from lake.modules.arbiter import Arbiter
from lake.passes.passes import lift_config_reg
from lake.utils.util import decode, register, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class BuffetLike(Generator):
    def __init__(self,
                 data_width=16,
                 num_ID=2,
                 mem_depth=512):

        super().__init__(f"buffet_like_{data_width}", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True
        self.num_ID = num_ID
        self.mem_depth = mem_depth

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
        # Free or Read
        self._rd_op_op = self.input("rd_op_op", self.data_width, explicit_array=True, packed=True)
        self._rd_op_op.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_op_ready = self.output("rd_op_ready", 1)
        self._rd_op_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_op_valid = self.input("rd_op_valid", 1)
        self._rd_op_valid.add_attribute(ControlSignalAttr(is_control=True))

        self._rd_addr = self.input("rd_addr", self.data_width, explicit_array=True, packed=True)
        self._rd_addr.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._rd_addr_ready = self.output("rd_addr_ready", 1)
        self._rd_addr_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_addr_valid = self.input("rd_addr_valid", 1)
        self._rd_addr_valid.add_attribute(ControlSignalAttr(is_control=True))

        # Read ID
        self._rd_ID_ready = self.output("rd_ID_ready", 1)
        self._rd_ID_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._rd_ID_valid = self.input("rd_ID_valid", 1)
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
# Miscellaneous forward declarations
# =============================

        # Handle allocating separate buffets in the same physical memory space
        base_chunk = self.mem_depth // self.num_ID
        self._buffet_base = self.var("buffet_base", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        [self.wire(self._buffet_base[i], kts.const(base_chunk * i, self._buffet_base[i].width)) for i in range(self.num_ID)]

        # Read block base and bounds...
        self._blk_base = self.var("blk_base", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        self._blk_bounds = self.var("blk_bounds", self.data_width, size=self.num_ID, explicit_array=True, packed=True)

# =============================
# FIFO inputs
# =============================

        # WR OP fifo
        self._wr_data_fifo_pop = self.var("wr_data_fifo_pop", 1)
        self._wr_data_fifo_valid = self.var("wr_data_fifo_valid", 1)

        self._wr_data_fifo_in = kts.concat(self._wr_data, self._wr_op)
        self._wr_data_infifo = RegFIFO(data_width=self._wr_data_fifo_in.width, width_mult=1, depth=8)
        self._wr_data_fifo_out_data = self.var("wr_data_fifo_out_data", self.data_width, packed=True)
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
        self._wr_addr_fifo_out_data = self.var("wr_addr_fifo_out_data", self.data_width, packed=True)

        self.add_child(f"wr_addr_fifo",
                       self._wr_addr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._wr_addr_valid,
                       pop=self._wr_addr_fifo_pop,
                       data_in=self._wr_addr_fifo_in,
                       data_out=self._wr_addr_fifo_out_data)

        self.wire(self._wr_addr_ready, ~self._wr_addr_infifo.ports.full)
        self.wire(self._wr_addr_fifo_valid, ~self._wr_addr_infifo.ports.empty)

        # WR ID fifo
        self._wr_ID_fifo_pop = self.var("wr_ID_fifo_pop", 1)
        self._wr_ID_fifo_valid = self.var("wr_ID_fifo_valid", 1)

        self._wr_ID_fifo_in = kts.concat(self._wr_ID)
        self._wr_ID_infifo = RegFIFO(data_width=self._wr_ID_fifo_in.width, width_mult=1, depth=8)
        self._wr_ID_fifo_out_data = self.var("wr_ID_fifo_out_data", self.data_width, packed=True)

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

        self._rd_op_fifo_in = kts.concat(self._rd_op_op)
        self._rd_op_infifo = RegFIFO(data_width=self._rd_op_fifo_in.width, width_mult=1, depth=8)
        self._rd_op_fifo_out_op = self.var("rd_op_fifo_out_op", self.data_width, packed=True)

        self.add_child(f"rd_op_fifo",
                       self._rd_op_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_op_valid,
                       pop=self._rd_op_fifo_pop,
                       data_in=self._rd_op_fifo_in,
                       data_out=kts.concat(self._rd_op_fifo_out_op))

        self.wire(self._rd_op_ready, ~self._rd_op_infifo.ports.full)
        self.wire(self._rd_op_fifo_valid, ~self._rd_op_infifo.ports.empty)

        # RD ADDR fifo
        self._rd_addr_fifo_pop = self.var("rd_addr_fifo_pop", 1)
        self._rd_addr_fifo_valid = self.var("rd_addr_fifo_valid", 1)

        self._rd_addr_fifo_in = kts.concat(self._rd_addr)
        self._rd_addr_infifo = RegFIFO(data_width=self._rd_addr_fifo_in.width, width_mult=1, depth=8)
        self._rd_addr_fifo_out_addr = self.var("rd_addr_fifo_out_addr", self.data_width, packed=True)

        self.add_child(f"rd_addr_fifo",
                       self._rd_addr_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._rd_addr_valid,
                       pop=self._rd_addr_fifo_pop,
                       data_in=self._rd_addr_fifo_in,
                       data_out=kts.concat(self._rd_addr_fifo_out_addr))

        self.wire(self._rd_addr_ready, ~self._rd_addr_infifo.ports.full)
        self.wire(self._rd_addr_fifo_valid, ~self._rd_addr_infifo.ports.empty)

        # RD ID fifo
        self._rd_ID_fifo_pop = self.var("rd_ID_fifo_pop", 1)
        self._rd_ID_fifo_valid = self.var("rd_ID_fifo_valid", 1)

        self._rd_ID_fifo_in = kts.concat(self._rd_ID)
        self._rd_ID_infifo = RegFIFO(data_width=self._rd_ID_fifo_in.width, width_mult=1, depth=8)
        self._rd_ID_fifo_out_data = self.var("rd_ID_fifo_out_data", self.data_width, packed=True)

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

        # Size requests al
        self._size_request_full = self.var("size_request_full", self.num_ID)

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

        chosen_size_block = decode(self, self._size_request_full, self._blk_bounds)

        self.wire(self._rd_rsp_fifo_in_data, kts.ternary(self._valid_from_mem, self._data_from_mem, chosen_size_block))

        self.wire(self._rd_rsp_fifo_push, self._valid_from_mem | self._size_request_full.r_or())

# # =============================
# #  Join Logic
# # =============================

        self._joined_in_fifo = self.var("joined_in_fifo", 1)
        self.wire(self._joined_in_fifo, self._wr_data_fifo_valid & self._wr_addr_fifo_valid & self._wr_ID_fifo_valid)

        # Broadcast the pop
        self._pop_in_fifos = self.var("pop_in_fifos", 1)
        self.wire(kts.concat(self._wr_addr_fifo_pop, self._wr_data_fifo_pop, self._wr_ID_fifo_pop), kts.concat(*[self._pop_in_fifos for i in range(3)]))

        # Each FSM can assert pop, need to make sure this is 1-hot though
        self._pop_in_full = self.var("pop_in_full", self.num_ID)
        self.wire(self._pop_in_fifos, self._pop_in_full.r_or())

        self._read_joined = self.var("read_joined", 1)
        self.wire(self._read_joined, self._rd_ID_fifo_valid & self._rd_op_fifo_valid & self._rd_addr_fifo_valid)

        self._read_pop = self.var("read_pop", 1)
        self.wire(kts.concat(self._rd_ID_fifo_pop, self._rd_op_fifo_pop, self._rd_addr_fifo_pop), kts.concat(*[self._read_pop for i in range(3)]))

        self._read_pop_full = self.var("read_pop_full", self.num_ID)
        self.wire(self._read_pop, self._read_pop_full.r_or())

# # =============================
# #  FSM
# # =============================

        self._wen_full = self.var("wen_full", self.num_ID)

        # Read side address + ren
        # self._rd_addr_loc = self.var("rd_addr_loc", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        self._ren_full = self.var("ren_full", self.num_ID)

        # Need to define which side (read/write) has access to the memory port
        self._mem_acq = self.var("mem_acq", 2 * self.num_ID)

        self._en_curr_bounds = self.var("en_curr_bounds", self.num_ID)
        # self._curr_bounds = self.var("curr_bounds", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        self._curr_bounds = [register(self, self._wr_addr, enable=self._en_curr_bounds[i], name=f"curr_bounds_{i}", packed=True) for i in range(self.num_ID)]

        self._en_curr_base = self.var("en_curr_base", self.num_ID)
        self._curr_base = [register(self, self._wr_addr, enable=self._en_curr_base[i], name=f"curr_base_{i}", packed=True) for i in range(self.num_ID)]
        # self._curr_bounds = register(self, self._bounds_ctr, enable=self._en_curr_bounds)

        self._push_blk = self.var("push_blk", self.num_ID)
        self._pop_blk = self.var("pop_blk", self.num_ID)
        self._blk_valid = self.var("blk_valid", self.num_ID)
        self._blk_full = self.var("blk_full", self.num_ID)

        self._curr_capacity_pre = self.var("curr_capacity_pre", self.data_width, size=self.num_ID, explicit_array=True, packed=True)
        # self._curr_capacity = self.var("curr_capacity", self.data_width, size=self.num_ID, explicit_array=True, packed=True)

        @always_ff((posedge, self._clk), (negedge, self._rst_n))
        def cap_reg(self, idx):
            if ~self._rst_n:
                self._curr_capacity_pre[idx] = kts.const(0, self.data_width)
            # Only update when pushed or popped
            elif self._push_blk[idx] or self._pop_blk[idx]:
                self._curr_capacity_pre[idx] = self._curr_capacity_pre[idx] + kts.ternary(self._push_blk[idx], self._blk_bounds[idx], kts.const(0, width=self.data_width)) - kts.ternary(self._pop_blk[idx], self._blk_bounds[idx], kts.const(0, width=self.data_width))

        [self.add_code(cap_reg, idx=i) for i in range(self.num_ID)]

        # Create FSM
        self.write_fsm = [self.add_fsm(f"write_fsm_{i}", reset_high=False) for i in range(self.num_ID)]
        WR_START = [self.write_fsm[i].add_state(f"WR_START_{i}") for i in range(self.num_ID)]
        WRITING = [self.write_fsm[i].add_state(f"WRITING_{i}") for i in range(self.num_ID)]

        ####################
        # Next State Logic WRITE
        ####################
        for ID_idx in range(self.num_ID):

            self.write_fsm[ID_idx].output(self._push_blk[ID_idx])
            self.write_fsm[ID_idx].output(self._en_curr_base[ID_idx])
            self.write_fsm[ID_idx].output(self._en_curr_bounds[ID_idx])
            self.write_fsm[ID_idx].output(self._wen_full[ID_idx])
            self.write_fsm[ID_idx].output(self._pop_in_full[ID_idx])
            # self.write_fsm.output(self._en_curr_bounds)

            ####################
            # WR_START #
            ####################
            # Start state gets an allocate command
            WR_START[ID_idx].next(WRITING[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
            WR_START[ID_idx].next(WR_START[ID_idx], None)

            ####################
            # WRITING #
            ####################
            # Writing until we get a finalize...
            WRITING[ID_idx].next(WR_START[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & ~self._blk_full[ID_idx] & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
            WRITING[ID_idx].next(WRITING[ID_idx], None)

            ####################
            # Output Logic WRITE
            ####################

            ####################
            # WR_START #
            ####################

            WR_START[ID_idx].output(self._push_blk[ID_idx], 0)
            WR_START[ID_idx].output(self._en_curr_base[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
            WR_START[ID_idx].output(self._en_curr_bounds[ID_idx], 0)
            WR_START[ID_idx].output(self._wen_full[ID_idx], 0)
            WR_START[ID_idx].output(self._pop_in_full[ID_idx], 0)
            # WR_START.output(self._en_curr_bounds, 0)

            ####################
            # WRITING #
            ####################
            # Increment wr addr if we get wr access
            # WRITING[ID_idx].output(self._inc_wr_addr, self._mem_acq[0] & self._wr_data_fifo_valid & (self._wr_data_fifo_out_op == 1))
            # WRITING[ID_idx].output(self._inc_bounds_ctr, self._mem_acq[0] & self._wr_data_fifo_valid & (self._wr_data_fifo_out_op == 1))
            # WRITING[ID_idx].output(self._clr_bounds_ctr, 0)
            WRITING[ID_idx].output(self._push_blk[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 0) & ~self._blk_full[ID_idx] & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
            WRITING[ID_idx].output(self._en_curr_base[ID_idx], 0)
            WRITING[ID_idx].output(self._en_curr_bounds[ID_idx], self._mem_acq[2 * ID_idx + 0] & self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
            # WRITING[ID_idx].output(self._wen_full[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & ~(self._curr_capacity < self._buffet_capacity))

            # Only make the wen when there is room and the proper ID is being addressed
            WRITING[ID_idx].output(self._wen_full[ID_idx], self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
            WRITING[ID_idx].output(self._pop_in_full[ID_idx], self._mem_acq[2 * ID_idx + 0] & self._joined_in_fifo & (self._wr_data_fifo_out_op == 1) & (self._wr_addr_fifo_out_data < (self._buffet_capacity[ID_idx] - self._curr_capacity_pre[ID_idx])) & (self._wr_ID_fifo_out_data == kts.const(ID_idx, self._wr_ID_fifo_out_data.width)))
            # WRITING.output(self._en_curr_bounds, 0)

        ##### Create read side fsm separately.

        #####################
        # READING #
        #####################

        self.read_fsm = [self.add_fsm(f"read_fsm_{i}", reset_high=False) for i in range(self.num_ID)]
        RD_START = [self.read_fsm[i].add_state(f"RD_START_{i}") for i in range(self.num_ID)]

        for ID_idx in range(self.num_ID):
            ####################
            # RD_START
            ####################
            # Get the first block size...
            RD_START[ID_idx].next(RD_START[ID_idx], None)

            self.read_fsm[ID_idx].output(self._pop_blk[ID_idx])
            # self.read_fsm[ID_idx].output(self._rd_addr_loc[ID_idx])
            self.read_fsm[ID_idx].output(self._ren_full[ID_idx])
            self.read_fsm[ID_idx].output(self._read_pop_full[ID_idx])
            self.read_fsm[ID_idx].output(self._size_request_full[ID_idx])

            ####################
            # RD_START
            ####################
            RD_START[ID_idx].output(self._pop_blk[ID_idx], (self._rd_op_fifo_out_op == 0) & self._read_joined & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
            # Guarantee there's room for the read to land
            RD_START[ID_idx].output(self._ren_full[ID_idx], (self._rd_op_fifo_out_op == 1) & self._read_joined & ~self._rd_rsp_fifo_full & self._blk_valid[ID_idx] & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
            # Pop the op fifo if there is a read that's going through or if it's a free op
            # If it's a size request, only fulfill it if we aren't pushing a read from memory to the output fifo
            RD_START[ID_idx].output(self._read_pop_full[ID_idx], kts.ternary(self._rd_op_fifo_out_op == 2, ~self._valid_from_mem, kts.ternary(self._rd_op_fifo_out_op == 1, self._mem_acq[2 * ID_idx + 1] & ~self._rd_rsp_fifo_full, kts.const(1, 1))) & self._read_joined & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))
            RD_START[ID_idx].output(self._size_request_full[ID_idx], (self._rd_op_fifo_out_op == 2) & self._read_joined & (self._rd_ID_fifo_out_data == kts.const(ID_idx, self._rd_ID_fifo_out_data.width)))

        for i in range(self.num_ID):
            self.write_fsm[i].set_start_state(WR_START[i])
            self.read_fsm[i].set_start_state(RD_START[i])

        for i in range(self.num_ID):
            ### Bookkeeping FIFO
            blk_fifo_in = kts.concat(self._curr_base[i], self._curr_bounds[i])
            blk_fifo = RegFIFO(data_width=blk_fifo_in.width, width_mult=1, depth=8)

            self.add_child(f"blk_fifo_{i}",
                           blk_fifo,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           clk_en=self._clk_en,
                           push=self._push_blk[i],
                           pop=self._pop_blk[i],
                           data_in=blk_fifo_in,
                           data_out=kts.concat(self._blk_base[i], self._blk_bounds[i]))

            self.wire(self._blk_full[i], blk_fifo.ports.full)
            self.wire(self._blk_valid[i], ~blk_fifo.ports.empty)

        # Force FSM realization first so that flush gets added...
        kts.passes.realize_fsm(self.internal_generator)

        # Arbitrate between the write/read side with RR arbiter
        self.port_arbiter = Arbiter(ins=2 * self.num_ID,
                                    algo="RR")

        base_rr = kts.concat(self._wen_full[0], self._ren_full[0])
        for i in range(self.num_ID - 1):
            base_rr = kts.concat(base_rr, self._wen_full[i + 1], self._ren_full[i + 1])

        self.add_child(f"rr_arbiter",
                       self.port_arbiter,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       request_in=base_rr,
                       grant_out=self._mem_acq,
                       resource_ready=self._ready_from_mem)

        self.wire(self._data_to_mem, self._wr_data_fifo_out_data)
        wr_acq = self._mem_acq[0] & self._wen_full[0]
        rd_acq = self._mem_acq[1] & self._ren_full[0]
        for i in range(self.num_ID - 1):
            wr_acq = kts.concat(wr_acq, self._mem_acq[2 * (i + 1)] & self._wen_full[i + 1])
            rd_acq = kts.concat(rd_acq, self._mem_acq[2 * (i + 1) + 1] & self._ren_full[i + 1])
        self.wire(self._ren_to_mem, rd_acq.r_or())
        self.wire(self._wen_to_mem, wr_acq.r_or())

        # Choose which base block...
        wr_base = kts.ternary(wr_acq[0], self._curr_base[0] + self._buffet_base[0], kts.const(0, self._curr_base[0].width))
        rd_base = kts.ternary(rd_acq[0], self._blk_base[0] + self._buffet_base[0], kts.const(0, self._blk_base[0].width))
        for i in range(self.num_ID - 1):
            wr_base = kts.ternary(wr_acq[i + 1], self._curr_base[i + 1] + self._buffet_base[i + 1], wr_base)
            rd_base = kts.ternary(wr_acq[i + 1], self._blk_base[i + 1] + self._buffet_base[i + 1], rd_base)
        self.wire(self._addr_to_mem, kts.ternary(self._wen_to_mem, self._wr_addr_fifo_out_data + wr_base, self._rd_addr_fifo_out_addr + rd_base))

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
    buffet_dut = BuffetLike(data_width=16,
                            num_ID=2)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(buffet_dut, filename="buffet_like.sv",
            optimize_if=False)
