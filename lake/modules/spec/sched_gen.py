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
                 dual_config=False,
                 iterator_support2=2):

        if dual_config:
            super().__init__(f"sched_gen_dual_config_{iterator_support}_{iterator_support2}_{config_width}")
        else:
            super().__init__(f"sched_gen_{iterator_support}_{config_width}")

        self.iterator_support = iterator_support
        self.config_width = config_width
        self.use_enable = use_enable
        self.dual_config = dual_config
        self.iterator_support2 = iterator_support2
        self.max_iterator_support = max(self.iterator_support, self.iterator_support2)

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # OUTPUTS
        self._valid_output = self.output("valid_output", 1)

        # VARS
        self._valid_out = self.var("valid_out", 1)
        self._cycle_count = self.input("cycle_count", self.config_width)
        if self.dual_config:
            self._mux_sel = self.input("mux_sel", max(clog2(self.max_iterator_support) + 1, 1))
            self._mux_sel_msb_init = self.output("mux_sel_msb_init", 1)
        else:
            self._mux_sel = self.input("mux_sel", max(clog2(self.iterator_support), 1))
        self._addr_out = self.var("addr_out", self.config_width)

        # Receive signal on last iteration of looping structure and
        # gate the output...
        self._finished = self.input("finished", 1)
        if self.dual_config:
            self._valid_gate_inv = self.var("valid_gate_inv", 2)
            self._valid_gate = self.var("valid_gate", 2)
            self._cur_valid_gate = self.var("cur_valid_gate", 1)
            self.wire(self._cur_valid_gate, self._valid_gate[self._mux_sel[self._mux_sel.width - 1]])
        else:
            self._valid_gate_inv = self.var("valid_gate_inv", 1)
            self._valid_gate = self.var("valid_gate", 1)
        self.wire(self._valid_gate, ~self._valid_gate_inv)

        # Since dim = 0 is not sufficient, we need a way to prevent
        # the controllers from firing on the starting offset
        if self.use_enable:
            self._enable = self.input("enable", 1)
            self._enable.add_attribute(ConfigRegAttr("Disable the controller so it never fires..."))
            self._enable.add_attribute(FormalAttr(f"{self._enable.name}", FormalSignalConstraint.SOLVE))
            if self.dual_config:
                self._enable2 = self.input("enable2", 1)
                self._enable2.add_attribute(ConfigRegAttr("Disable the controller so it never fires..."))
                self._enable2.add_attribute(FormalAttr(f"{self._enable2.name}", FormalSignalConstraint.SOLVE))
                self._cur_enable = self.var("cur_enable", 1)
                self.wire(self._cur_enable, ternary(self._mux_sel[self._mux_sel.width - 1], self._enable2, self._enable))
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
                if self.dual_config:
                    self._valid_gate_inv[self._mux_sel[self._mux_sel.width - 1]] = 1
                else:
                    self._valid_gate_inv = 1
        self.add_code(valid_gate_inv_ff)

        # Compare based on minimum of addr + global cycle...
        self.c_a_cmp = min(self._cycle_count.width, self._addr_out.width)

        # PORT DEFS: end

        self.add_child(f"sched_addr_gen",
                       AddrGen(iterator_support=self.iterator_support,
                               config_width=self.config_width,
                               dual_config=self.dual_config,
                               iterator_support2=self.iterator_support2),

                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._valid_out,
                       mux_sel=self._mux_sel,
                       addr_out=self._addr_out,
                       restart=self._finished,
                       mux_sel_msb_init=self._mux_sel_msb_init)

        self.add_code(self.set_valid_out)
        self.add_code(self.set_valid_output)

    @always_comb
    def set_valid_out(self):
        if self.dual_config:
            self._valid_out = (self._cycle_count == self._addr_out) & self._cur_valid_gate & self._cur_enable
        else:
            self._valid_out = (self._cycle_count == self._addr_out) & self._valid_gate & self._enable

    @always_comb
    def set_valid_output(self):
        self._valid_output = self._valid_out


if __name__ == "__main__":
    db_dut = SchedGen(iterator_support=6)
    verilog(db_dut,
            filename="sched_gen.sv",
            additional_passes={"lift config regs": lift_config_reg})
