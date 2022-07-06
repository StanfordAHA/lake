from kratos import *
from functools import reduce
import operator

import kratos
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.modules.sched_addr_gen import SchedAddrGen
from lake.passes.passes import lift_config_reg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


class CycleSchedGen(Generator):
    '''
    Generate schedule
    '''
    def __init__(self,
                 iterator_support=6,
                 config_width=16,
                 use_enable=True):

        super().__init__(f"cycle_sched_gen_{iterator_support}_{config_width}")

        self.iterator_support = iterator_support
        self.config_width = config_width
        self.use_enable = use_enable

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._step = self.input("step", 1)

        # OUTPUTS
        self._valid_output = self.output("valid_output", 1)

        # VARS
        self._valid_out = self.var("valid_out", 1)
        # self._cycle_count = self.input("cycle_count", self.config_width)
        self._mux_sel = self.input("mux_sel", max(clog2(self.iterator_support), 1))
        self._addr_out = self.var("addr_out", self.config_width)
        # self._after_start = self.var("after_start", 1)

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
        # self.c_a_cmp = min(self._cycle_count.width, self._addr_out.width)

        # PORT DEFS: end

        self.add_child(f"sched_addr_gen",
                       SchedAddrGen(iterator_support=self.iterator_support,
                                    config_width=self.config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._step,
                       mux_sel=self._mux_sel,
                       addr_out=self._addr_out,
                       restart=const(0, 1))

        self.add_code(self.set_valid_out)
        self.add_code(self.set_valid_output)

    @always_comb
    def set_valid_out(self):
        self._valid_out = (self._addr_out == 0) & self._valid_gate & self._enable
        # self._valid_out = (self._addr_out == 0) & self._after_start & self._valid_gate & self._enable

    @always_comb
    def set_valid_output(self):
        self._valid_output = self._valid_out


if __name__ == "__main__":
    db_dut = CycleSchedGen(iterator_support=6)
    verilog(db_dut,
            filename="cycle_sched_gen.sv",
            additional_passes={"lift config regs": lift_config_reg})
