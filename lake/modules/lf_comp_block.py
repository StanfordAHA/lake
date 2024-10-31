import kratos as kts
from kratos import always_comb
from enum import Enum
import operator
from lake.spec.component import Component
from lake.utils.spec_enum import LFComparisonOperator


class LFCompBlock(Component):

    def __init__(self, name: str, debug: bool = True, is_clone: bool = False, internal_generator=None,
                 in_width: int = None, out_width: int = None, comparisonOp: LFComparisonOperator = None,
                 hard_scalar=None):
        # super().__init__(name, debug, is_clone, internal_generator)
        super().__init__(name=name)
        self.in_width = in_width
        self.out_width = out_width
        self.hard_op = comparisonOp
        self.hard_scalar = hard_scalar
        self.interfaces = None

    def gen_hardware(self):
        # The interface is leader count and follower count
        self._input_counter = self.input("leader_count", self.in_width)
        self._output_counter = self.input("follower_count", self.out_width)

        # The comparison will always be 1 if not enabled - for reduction and in ScheduleGenerator
        self._enable_comparison = self.config_reg(name="enable_comparison", width=1)
        # Output a single comparison
        self._comparison = self.output("comparison", 1)
        self._comparison_lcl = self.var("comparison_lcl", 1)

        # Get number of entries in comparison for doing bit width
        num_comps = len(LFComparisonOperator)
        num_comps_bw = max(1, kts.clog2(num_comps))

        # Generate a reconfigurable block if no hard op given
        if self.hard_op is None:
            # Make it a configuration register
            self._comp_reg = self.config_reg(name="comparison_op", width=num_comps_bw)
            self._scalar_reg = self.config_reg(name="comparison_scalar", width=16)
        else:
            # Otherwise it is a hardened value...
            self._comp_reg = kts.const(self.hard_op.value, num_comps_bw)
            self._scalar_reg = kts.const(self.hard_scalar, width=16)

        @always_comb
        def calculate_comparison():
            self._comparison_lcl = 0
            if self._comp_reg == LFComparisonOperator.LT.value:
                self._comparison_lcl = self._input_counter < self._output_counter + self._scalar_reg
            elif self._comp_reg == LFComparisonOperator.GT.value:
                self._comparison_lcl = self._input_counter > self._output_counter + self._scalar_reg
            elif self._comp_reg == LFComparisonOperator.EQ.value:
                self._comparison_lcl = self._input_counter == self._output_counter + self._scalar_reg
            elif self._comp_reg == LFComparisonOperator.LTE.value:
                self._comparison_lcl = self._input_counter <= self._output_counter + self._scalar_reg

        self.add_code(calculate_comparison)

        self.wire(self._comparison, self._comparison_lcl | ~self._enable_comparison)

        self.interfaces = {}
        self.interfaces['in_counter'] = self._input_counter
        self.interfaces['out_counter'] = self._output_counter
        self.interfaces['comparison'] = self._comparison

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def get_interfaces(self):
        return self.interfaces

    def gen_bitstream(self, comparator, scalar):
        self.configure(self._enable_comparison, 1)
        self.configure(self._comp_reg, comparator)
        self.configure(self._scalar_reg, scalar)
        return self.get_configuration()


if __name__ == "__main__":
    # Check if this works
    comp = LFCompBlock(name='try_this', comparisonOp=LFComparisonOperator.EQ)
    comp.gen_hardware()
    kts.verilog(comp, filename='lfblock.sv')