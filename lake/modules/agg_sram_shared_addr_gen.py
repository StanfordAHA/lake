from kratos import *
from functools import reduce
import operator

import kratos
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


class AggSramSharedAddrGen(Generator):
    '''
    Generate address for reading Agg and writing SRAM
    '''
    def __init__(self,
                 height=512,
                 addr_fifo_depth=8,
                 config_width=16):

        super().__init__(f"agg_sram_shared_addr_gen")

        self.config_width = config_width
        self.height = height
        self.addr_fifo_depth = addr_fifo_depth

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._step = self.input("step", 1)

        self._sram_read = self.input("sram_read", 1)
        self._sram_read_addr = self.input("sram_read_addr", self.config_width)

        # linear or reuse mode configuration register
        self._mode = self.input("mode", 1)
        # self._mode.add_attribute(ConfigRegAttr("Mode of agg_sram shared schedule or addressing"))
        # self._mode.add_attribute(FormalAttr(f"{self._mode.name}", FormalSignalConstraint.SOLVE))

        # OUTPUTS
        self._addr_out = self.output("addr_out", self.config_width)

        # VARS
        self._addr_fifo = self.var("addr_fifo", self.config_width,
                                   size=self.addr_fifo_depth,
                                   packed=True,
                                   explicit_array=True)
        self._lin_addr_cnter = self.var("lin_addr_cnter", self.config_width)
        self._addr_fifo_rd_addr = self.var("addr_fifo_rd_addr", self.config_width)
        self._wr_ptr = self.var("wr_ptr", clog2(self.addr_fifo_depth))
        self._rd_ptr = self.var("rd_ptr", clog2(self.addr_fifo_depth))

        # GENERATION LOGIC: begin
        # # Calculate address linearly and checks for wrap-around
        self.add_code(self.calculate_address)
        self.add_code(self.update_addr_fifo)
        self.wire(self._addr_out, ternary(self._mode, self._addr_fifo_rd_addr, self._lin_addr_cnter))
        # GENERATION LOGIC: end

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def calculate_address(self):
        if ~self._rst_n:
            self._lin_addr_cnter = 0
        elif ~self._mode:
            if self._step:
                if (self._lin_addr_cnter == self.height - 1):
                    self._lin_addr_cnter = 0
                else:
                    self._lin_addr_cnter = self._lin_addr_cnter + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_addr_fifo(self):
        if ~self._rst_n:
            self._wr_ptr = 0
            self._rd_ptr = 0
            self._addr_fifo = 0
            self._addr_fifo_rd_addr = 0
        elif self._mode:
            if self._sram_read:
                self._wr_ptr = self._wr_ptr + 1
                self._addr_fifo[self._wr_ptr] = self._sram_read_addr

            if self._step:
                self._rd_ptr = self._rd_ptr + 1
            self._addr_fifo_rd_addr = self._addr_fifo[self._rd_ptr]


if __name__ == "__main__":
    db_dut = AggSramSharedAddrGen()
    verilog(db_dut,
            filename="agg_sram_shared_addr_gen.sv",
            additional_passes={"lift config regs": lift_config_reg})
