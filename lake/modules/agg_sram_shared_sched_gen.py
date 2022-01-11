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
                 addr_fifo_depth=8,
                 agg_read_strt_cycle_width=4,
                 config_width=16):

        super().__init__(f"agg_sram_shared_sched_gen")

        self.config_width = config_width
        self.fetch_width = mem_width // data_width
        self.agg_range_width = agg_range_width
        self.addr_fifo_depth = addr_fifo_depth
        self.agg_read_strt_cycle_width = agg_read_strt_cycle_width

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

        # Agg read start cycle, 0-index
        # because the compiler could wait a few cycles (typically 1) after writing the first 4 words to Agg before reading one wide word out
        self._agg_read_strt_cycle = self.input("agg_read_strt_cycle", clog2(self.agg_read_strt_cycle_width))
        self._agg_read_strt_cycle.add_attribute(ConfigRegAttr("Start cycle to read from Agg after writing the first 4 words to Agg"))
        self._agg_read_strt_cycle.add_attribute(FormalAttr(f"{self._agg_read_strt_cycle.name}", FormalSignalConstraint.SOLVE))

        # OUTPUTS
        self._valid_output = self.output("valid_output", 1)

        # VARS

        # new
        self._delay_cnt = self.var("delay_cnt", clog2(self.fetch_width))
        self._shifter = self.var("shifter", self.addr_fifo_depth)
        self._shifter_in = self.var("shifter_in", 1)

        self._shifter_in = ternary(self._mode, self._sram_read, self._agg_write)
        self.add_code(self.update_delay_cnt)
        self.add_code(self.update_shifter)
        self.add_code(self.set_valid_output)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_delay_cnt(self):
        if ~self._rst_n:
            self._delay_cnt = 0
        elif self._mode == 0:
            # only used in the linear mode where we need to count to the fetch width
            if self._shifter[self._agg_read_strt_cycle]:
                if (self._delay_cnt == self.fetch_width - 1):
                    self._delay_cnt = 0
                else:
                    self._delay_cnt = self._delay_cnt + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_shifter(self):
        if ~self._rst_n:
            self._shifter = 0
        else:
            self._shifter = concat(self._shifter[self._shifter.width - 2, 0], self._shifter_in)

    @always_comb
    def set_valid_output(self):
        if self._mode == 0:
            self._valid_output = (self._delay_cnt == self.fetch_width - 1) & self._shifter[self._agg_read_strt_cycle]
        else:
            self._valid_output = self._shifter[self._delay]


if __name__ == "__main__":
    db_dut = AggSramSharedSchedGen()
    verilog(db_dut,
            filename="agg_sram_shared_sched_gen.sv",
            additional_passes={"lift config regs": lift_config_reg})
