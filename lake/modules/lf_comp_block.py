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
        self._leader = self.input("leader_count", self.in_width)
        self._follower = self.input("follower_count", self.out_width)

        self._leader_signed = self.var("leader_signed", 17, is_signed=True)
        self._follower_signed = self.var("follower_signed", 17, is_signed=True)

        self._leader_finished = self.input("leader_finished", 1)
        self._follower_finished = self.input("follower_finished", 1)

        # The comparison will always be 1 if not enabled - for reduction and in ScheduleGenerator
        self._enable_comparison = self.config_reg(name="enable_comparison", width=1)
        # Output a single comparison
        self._comparison = self.output("comparison", 1)
        self._comparison_lcl = self.var("comparison_lcl", 1, is_signed=True)

        kts.signed

        # Get number of entries in comparison for doing bit width
        num_comps = len(LFComparisonOperator)
        num_comps_bw = max(1, kts.clog2(num_comps))

        # Generate a reconfigurable block if no hard op given
        if self.hard_op is None:
            # Make it a configuration register
            self._comp_reg = self.config_reg(name="comparison_op", width=num_comps_bw)
            # self._scalar_reg = self.config_reg(name="comparison_scalar", width=16)
            self._scalar_reg = self.config_reg(name="comparison_scalar", width=16, is_signed=False)
            self._scalar_reg_signed = self.var("scalar_reg_signed", 16, is_signed=True)
            self.wire(self._scalar_reg_signed, kts.signed(self._scalar_reg))
        else:
            # Otherwise it is a hardened value...
            self._comp_reg = kts.const(self.hard_op.value, num_comps_bw)
            self._scalar_reg = kts.const(self.hard_scalar, width=16)

        self._l_plus_s = self.var("l_plus_s", 17, is_signed=True)
        self.wire(self._l_plus_s, self._leader_signed + self._scalar_reg_signed)
        self._f_plus_s = self.var("f_plus_s", 17, is_signed=True)
        self.wire(self._f_plus_s, self._follower_signed + self._scalar_reg_signed)

        self.wire(self._leader_signed, kts.concat(kts.const(0, 1, is_signed=True), kts.signed(self._leader)))
        self.wire(self._follower_signed, kts.concat(kts.const(0, 1, is_signed=True), kts.signed(self._follower)))

        @always_comb
        def calculate_comparison():
            self._comparison_lcl = 0
            # RAW
            if self._comp_reg == LFComparisonOperator.LT.value:
                # self._comparison_lcl = self._leader + self._scalar_reg < self._follower
                self._comparison_lcl = self._l_plus_s < self._follower_signed
            # WAR
            elif self._comp_reg == LFComparisonOperator.GT.value:
                # self._comparison_lcl = self._leader < self._follower + self._scalar_reg
                self._comparison_lcl = self._leader_signed < self._f_plus_s
            elif self._comp_reg == LFComparisonOperator.EQ.value:
                # self._comparison_lcl = self._leader == self._follower + self._scalar_reg
                self._comparison_lcl = self._leader_signed == self._f_plus_s
            elif self._comp_reg == LFComparisonOperator.LTE.value:
                # self._comparison_lcl = self._leader <= self._follower + self._scalar_reg
                self._comparison_lcl = self._leader_signed <= self._f_plus_s

        self.add_code(calculate_comparison)

        # self.wire(self._comparison, self._comparison_lcl | ~self._enable_comparison)
        # The comparison has to be highg if one of the leader/follower is finished
        self.wire(self._comparison, self._comparison_lcl | ~self._enable_comparison | self._leader_finished | self._follower_finished)

        self.interfaces = {}
        self.interfaces['in_counter'] = self._leader
        self.interfaces['in_finished'] = self._leader_finished
        self.interfaces['out_counter'] = self._follower
        self.interfaces['out_finished'] = self._follower_finished
        self.interfaces['comparison'] = self._comparison

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def get_interfaces(self):
        return self.interfaces

    def gen_bitstream(self, comparator, scalar):
        self.configure(self._enable_comparison, 1)
        self.configure(self._comp_reg, comparator)
        self.configure(self._scalar_reg, scalar)
        print(f"SCALAR_REG: {scalar}")
        return self.get_configuration()


if __name__ == "__main__":
    # Check if this works
    comp = LFCompBlock(name='try_this', comparisonOp=LFComparisonOperator.EQ)
    comp.gen_hardware()
    kts.verilog(comp, filename='lfblock.sv')
