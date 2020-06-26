from kratos import *
from functools import reduce
import operator
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.modules.addr_gen import AddrGen
from lake.passes.passes import lift_config_reg


class SchedGen(Generator):
    '''
    Generate schedule
    '''
    def __init__(self,
                 iterator_support,
                 config_width=16):

        super().__init__(f"sched_gen_{iterator_support}")

        self.iterator_support = iterator_support
        self.config_width = config_width

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # MISC
        self._clk_en = self.input("clk_en", 1)
        self._flush = self.input("flush", 1)

        # OUTPUTS
        self._valid_output = self.output("valid_output", 1)

        # VARS
        self._valid_out = self.var("valid_out", 1)
        self._cycle_num = self.var("cycle_num", self.config_width)
        self._addr_out = self.var("addr_out", self.config_width)

        # PORT DEFS: end

        self.add_child(f"sched_addr_gen",

                       AddrGen(iterator_support=self.iterator_support,
                               config_width=self.config_width),

                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._valid_out,
                       clk_en=self._clk_en,
                       flush=self._flush,
                       addr_out=self._addr_out)

        self.add_code(self.set_valid_out)
        self.add_code(self.set_valid_output)
        self.add_code(self.set_cycle_num)

    @always_comb
    def set_valid_out(self):
        if self._cycle_num == self._addr_out:
            self._valid_out = 1
        else:
            self._valid_out = 0

    @always_comb
    def set_valid_output(self):
        self._valid_output = self._valid_out

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_cycle_num(self):
        if ~self._rst_n:
            self._cycle_num = 0
        else:
            self._cycle_num = self._cycle_num + 1

if __name__ == "__main__":
    db_dut = SchedGen(iterator_support=6)
    verilog(db_dut, filename="sched_gen.sv", additional_passes={"lift config regs": lift_config_reg})
