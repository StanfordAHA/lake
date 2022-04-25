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
                 iterator_support=6,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 agg_range_width=16,
                 delay_width=4,
                 agg_read_padding_width=6,
                 interconnect_input_ports=2,
                 config_width=16):

        super().__init__(f"agg_sram_shared_sched_gen")

        self.iterator_support = iterator_support
        self.config_width = config_width
        self.fetch_width = mem_width // data_width
        self.agg_range_width = agg_range_width
        self.delay_width = delay_width
        self.agg_read_padding_width = agg_read_padding_width
        self.interconnect_input_ports = interconnect_input_ports

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._agg_write_restart = self.input("agg_write_restart", 1)
        self._agg_write = self.input("agg_write", 1)
        self._agg_write_addr_l2b = self.input("agg_write_addr_l2b", 2)
        self._agg_write_mux_sel = self.input("agg_write_mux_sel", max(clog2(self.iterator_support), 1))
        self._sram_read = self.input("sram_read", self.interconnect_input_ports)

        # delay configuration register
        self._delay = self.input("delay", self.delay_width)

        # linear or reuse mode configuration register
        self._mode = self.input("mode", 2)

        # Agg read start cycle, 0-index
        # because the compiler could wait a few cycles (typically 1) after writing the first 4 words to Agg before reading one wide word out
        self._agg_read_padding = self.input("agg_read_padding", self.agg_read_padding_width)
        self._agg_read_padding.add_attribute(ConfigRegAttr("Start cycle to read from Agg after writing the first 4 words to Agg"))
        self._agg_read_padding.add_attribute(FormalAttr(f"{self._agg_read_padding.name}", FormalSignalConstraint.SOLVE))

        # OUTPUTS
        self._valid_output = self.output("valid_output", 1)

        # VARS

        # new
        # self._delay_cnt = self.var("delay_cnt", clog2(self.fetch_width))
        self._shifter = self.var("shifter", 2 ** self.delay_width)
        self._shifter_in = self.var("shifter_in", 1)
        self._agg_write_4_r = self.var("agg_write_4_r", 1)
        self._pad_cnt = self.var("pad_cnt", self.agg_read_padding_width)
        self._pad_cnt_en = self.var("pad_cnt_en", 1)

        self.add_code(self.update_agg_write_4_r)
        self.add_code(self.update_pad_delay)
        self.add_code(self.shifter_in_sel)
        # self.add_code(self.update_delay_cnt)
        self.add_code(self.update_shifter)
        self.add_code(self.set_valid_output)

    @always_comb
    def shifter_in_sel(self):
        if self._mode[1] == 0:
            self._shifter_in = 0
        else:
            self._shifter_in = ternary(self._mode[0], self._sram_read[self.interconnect_input_ports - 1], self._sram_read[0])

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_pad_delay(self):
        if ~self._rst_n:
            self._pad_cnt_en = 0
            self._pad_cnt = 0
        elif (self._mode[1] == 0) & (self._agg_read_padding != 0):
            if self._agg_write & ((self._agg_write_mux_sel != 0) | self._agg_write_restart):
                self._pad_cnt_en = 1
            elif self._pad_cnt == self._agg_read_padding:
                self._pad_cnt_en = 0

            if self._pad_cnt == self._agg_read_padding:
                self._pad_cnt = 0
            elif self._pad_cnt_en | (self._agg_write & ((self._agg_write_mux_sel != 0) | self._agg_write_restart)):
                self._pad_cnt = self._pad_cnt + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_agg_write_4_r(self):
        if ~self._rst_n:
            self._agg_write_4_r = 0
        elif self._mode[1] == 0:
            self._agg_write_4_r = self._agg_write & self._agg_write_addr_l2b.r_and()

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_shifter(self):
        if ~self._rst_n:
            self._shifter = 0
        elif self._mode[1] != 0:
            self._shifter = concat(self._shifter[self._shifter.width - 2, 0], self._shifter_in)

    @always_comb
    def set_valid_output(self):
        if self._mode[1] == 0:
            if self._agg_read_padding != 0:
                self._valid_output = (self._pad_cnt == self._agg_read_padding) | self._agg_write_4_r
            else:
                self._valid_output = self._agg_write_4_r
        else:
            self._valid_output = self._shifter[self._delay]


if __name__ == "__main__":
    db_dut = AggSramSharedSchedGen()
    verilog(db_dut,
            filename="agg_sram_shared_sched_gen.sv",
            additional_passes={"lift config regs": lift_config_reg})
