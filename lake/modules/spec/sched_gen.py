from kratos import *
from functools import reduce
import operator

import kratos
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.modules.addr_gen import AddrGen
from lake.passes.passes import lift_config_reg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


class SchedGen(Generator):
    '''
    Generate schedule
    '''
    def __init__(self,
                 iterator_support=6,
                 config_width=16,
                 use_enable=True,
                 optimization_lvl=2):

        super().__init__(f"sched_gen_{iterator_support}_{config_width}")

        self.iterator_support = iterator_support
        self.config_width = config_width
        self.use_enable = use_enable

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # OUTPUTS
        self._valid_output = self.output("valid_output", 1)

        # VARS
        self._valid_out = self.var("valid_out", 1)
        self._cycle_count = self.input("cycle_count", self.config_width)
        # self._mux_sel = self.input("mux_sel", max(clog2(self.iterator_support), 1))
        self._addr_out = self.var("addr_out", self.config_width)

        # Receive signal on last iteration of looping structure and
        # gate the output...
        self._finished = self.input("finished", 1)
        self._valid_gate_inv = self.var("valid_gate_inv", 1)
        self._valid_gate = self.var("valid_gate", 1)
        self.wire(self._valid_gate, ~self._valid_gate_inv)

        # Since dim = 0 is not sufficient, we need a way to prevent
        # the controllers from firing on the starting offset
        if self.use_enable:
            self._enable = self.input("enable", 1)
            self._enable.add_attribute(ConfigRegAttr("Disable the controller so it never fires..."))
            self._enable.add_attribute(FormalAttr(f"{self._enable.name}", FormalSignalConstraint.SOLVE))
        # Otherwise we set it as a 1 and leave it up to synthesis...
        else:
            self._enable = self.var("enable", 1)
            self.wire(self._enable, kratos.const(1, 1))

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def valid_gate_inv_ff():
            if ~self._rst_n:
                self._valid_gate_inv = 0
            # If we are finishing the looping structure, turn this off to implement one-shot
            elif self._finished:
                self._valid_gate_inv = 1
        self.add_code(valid_gate_inv_ff)

        # Compare based on minimum of addr + global cycle...
        self.c_a_cmp = min(self._cycle_count.width, self._addr_out.width)

        # PORT DEFS: end

        addit_args = {}

        if optimization_lvl == 0:
            self._dim_counter_in = self.input("dim_counter_in", self.config_width,
                                              size=self.iterator_support,
                                              packed=True,
                                              explicit_array=True)
            addit_args = {"dim_counter_in": self._dim_counter_in}
        elif optimization_lvl == 1:
            self._inc_in = self.input("inc_in", self.iterator_support)
            self._clr_in = self.input("clr_in", self.iterator_support)
            addit_args = {"inc_in": self._inc_in,
                          "clr_in": self._clr_in}
        elif optimization_lvl == 2:
            self._mux_sel = self.input("mux_sel", max(clog2(self.iterator_support), 1))
            addit_args = {"mux_sel": self._mux_sel}

        self.add_child(f"sched_addr_gen",
                       AddrGen(iterator_support=self.iterator_support,
                               config_width=self.config_width,
                               optimization_lvl=optimization_lvl),

                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._valid_out,
                       addr_out=self._addr_out,
                       restart=const(0, 1),
                       **addit_args)

        self.add_code(self.set_valid_out)
        self.add_code(self.set_valid_output)

    @always_comb
    def set_valid_out(self):
        self._valid_out = (self._cycle_count == self._addr_out) & self._valid_gate & self._enable

    @always_comb
    def set_valid_output(self):
        self._valid_output = self._valid_out


if __name__ == "__main__":

    for opt_level in range(3):
        db_dut = SchedGen(iterator_support=6, optimization_lvl=opt_level)
        verilog(db_dut,
                filename=f"sched_gen_{opt_level}.sv",
                additional_passes={"lift config regs": lift_config_reg})
