from kratos import *
from functools import reduce
import operator

import kratos
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


class AggSramSharedSchedGen(Generator):
    '''
    Generate schedule
    '''
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 agg_range_width=16,
                 addr_fifo_depth=5,
                 config_width=16):

        super().__init__(f"agg_sram_shared_sched_gen")

        self.config_width = config_width
        self.fetch_width = mem_width // data_width
        self.agg_range_width = agg_range_width
        self.addr_fifo_depth = addr_fifo_depth

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._agg_write = self.input("agg_write", 1)
        self._sram_read = self.input("sram_read", 1)

        # delay configuration register
        self._delay = self.input("delay", clog2(self.addr_fifo_depth))
        # self._delay.add_attribute(ConfigRegAttr("Delay cycles of agg_sram shared schedule"))
        # self._delay.add_attribute(FormalAttr(f"{self._delay.name}", FormalSignalConstraint.SOLVE))

        # linear or reuse mode configuration register
        self._mode = self.input("mode", 1)
        # self._mode.add_attribute(ConfigRegAttr("Mode of agg_sram shared schedule or addressing"))
        # self._mode.add_attribute(FormalAttr(f"{self._mode.name}", FormalSignalConstraint.SOLVE))

        # OUTPUTS
        self._valid_output = self.output("valid_output", 1)

        # VARS

        # new
        self._delay_cnt = self.var("delay_cnt", clog2(self.addr_fifo_depth))
        self._sram_read_delay = self.var("sram_read_delay", self.addr_fifo_depth)
        self.add_code(self.update_delay_cnt)
        self.add_code(self.update_sram_read_delay)
        self.add_code(self.set_valid_output)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_delay_cnt(self):
        if ~self._rst_n:
            self._delay_cnt = 2 ** self._delay_cnt.width - 1
        elif (~self._mode & self._agg_write):
            if (self._delay_cnt == self._delay - 1):
                self._delay_cnt = 0
            else:
                self._delay_cnt = self._delay_cnt + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_sram_read_delay(self):
        if ~self._rst_n:
            self._sram_read_delay = 0
        elif self._mode:
            self._sram_read_delay = concat(self._sram_read_delay[self._sram_read_delay.width - 2, 0], self._sram_read)

    @always_comb
    def set_valid_output(self):
        if self._mode == 0:
            self._valid_output = (self._delay_cnt == self._delay - 1) & self._agg_write
        else:
            self._valid_output = self._sram_read_delay[self._delay - 1]
        # self._valid_output = (self._delay_cnt == self._delay - 1) & ((~self._mode & self._agg_write) | (self._mode & self._sram_read))


if __name__ == "__main__":
    db_dut = AggSramSharedSchedGen()
    verilog(db_dut,
            filename="agg_sram_shared_sched_gen.sv",
            additional_passes={"lift config regs": lift_config_reg})
