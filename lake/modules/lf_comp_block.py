import kratos as kts
from kratos import always_comb
from enum import Enum
import operator
from lake.spec.component import Component
from lake.utils.spec_enum import LFComparisonOperator


class LFCompBlock(Component):

    def __init__(self, name: str, debug: bool = True, is_clone: bool = False, internal_generator=None,
                 in_width: int = None, out_width: int = None, comparisonOp: LFComparisonOperator = None,
                 hard_scalar=None, one_sided_op: LFComparisonOperator = None, scalar_width: int = 16,
                 barrier_bit: bool = False):
        """one_sided_op / scalar_width / barrier_bit (the trimmed block, see
        _gen_trimmed_hardware; defaults = the original block, unchanged RTL):
          one_sided_op: LT or GT built in, no `comparison_op` register and one
            adder. LT (leader + s < follower) equals GT (leader < follower - s)
            on the integers, so gen_bitstream takes either op and negates the
            scalar when it is not the built-in one.
          scalar_width: width of the signed `comparison_scalar` register.
          barrier_bit: a `barrier` register that holds the comparison at 0, so
            the port waits until either side finishes (what the converter's
            barrier scalar 16383 does on the counters)."""
        # super().__init__(name, debug, is_clone, internal_generator)
        super().__init__(name=name)
        self.in_width = in_width
        self.out_width = out_width
        self.hard_op = comparisonOp
        self.hard_scalar = hard_scalar
        self.one_sided_op = one_sided_op
        self.scalar_width = scalar_width
        self.barrier_bit = barrier_bit
        self.trimmed = one_sided_op is not None or scalar_width != 16 or barrier_bit
        if self.trimmed and comparisonOp is not None:
            raise ValueError("LFCompBlock: a hardened op/scalar block takes no trim options")
        if one_sided_op is not None and one_sided_op not in (LFComparisonOperator.LT, LFComparisonOperator.GT):
            raise ValueError(f"LFCompBlock: one_sided_op must be LT or GT, not {one_sided_op}")
        if not 2 <= scalar_width <= 16:
            raise ValueError(f"LFCompBlock: scalar_width {scalar_width} not in [2, 16]")
        self.interfaces = None

    def _gen_trimmed_hardware(self):
        """Same function as the original block on in-range scalars, computed
        in one signed width cw that holds every sum exactly (counters
        zero-extended, the scalar sign-extended), so a narrow scalar register
        needs no 16-bit adder."""
        lt, gt = LFComparisonOperator.LT.value, LFComparisonOperator.GT.value
        cw = max(self.in_width, self.out_width, self.scalar_width) + 2
        sw = self.scalar_width
        self._leader = self.input("leader_count", self.in_width)
        self._follower = self.input("follower_count", self.out_width)
        self._leader_finished = self.input("leader_finished", 1)
        self._follower_finished = self.input("follower_finished", 1)
        self._enable_comparison = self.config_reg(name="enable_comparison", width=1)
        self._comparison = self.output("comparison", 1)
        self._comparison_lcl = self.var("comparison_lcl", 1)

        self._leader_signed = self.var("leader_signed", cw, is_signed=True)
        self._follower_signed = self.var("follower_signed", cw, is_signed=True)
        self.wire(self._leader_signed, kts.signed(kts.concat(kts.const(0, cw - self.in_width), self._leader)))
        self.wire(self._follower_signed, kts.signed(kts.concat(kts.const(0, cw - self.out_width), self._follower)))

        self._scalar_reg = self.config_reg(name="comparison_scalar", width=sw, is_signed=False)
        self._scalar_reg_signed = self.var(name="scalar_reg_signed", width=cw, is_signed=True)
        sign = [self._scalar_reg[sw - 1]] * (cw - sw)
        self.wire(self._scalar_reg_signed, kts.signed(kts.concat(*sign, self._scalar_reg)))

        if self.one_sided_op is None:
            num_comps_bw = max(1, kts.clog2(len(LFComparisonOperator)))
            self._comp_reg = self.config_reg(name="comparison_op", width=num_comps_bw)
        else:
            self._comp_reg = None
        ops = [lt, gt] if self.one_sided_op is None else [self.one_sided_op.value]
        cmp = {}
        if lt in ops:
            self._l_plus_s = self.var("l_plus_s", cw, is_signed=True)
            self.wire(self._l_plus_s, self._leader_signed + self._scalar_reg_signed)
            cmp[lt] = self.var("comparison_lt", 1)
            self.wire(cmp[lt], kts.unsigned(self._l_plus_s < self._follower_signed))
        if gt in ops:
            self._f_plus_s = self.var("f_plus_s", cw, is_signed=True)
            self.wire(self._f_plus_s, self._follower_signed + self._scalar_reg_signed)
            cmp[gt] = self.var("comparison_gt", 1)
            self.wire(cmp[gt], kts.unsigned(self._leader_signed < self._f_plus_s))
        if self._comp_reg is None:
            raw = cmp[self.one_sided_op.value]
        else:
            # EQ / LTE (unimplemented, as in the original block) never pass
            raw = kts.ternary(self._comp_reg == lt, cmp[lt],
                              kts.ternary(self._comp_reg == gt, cmp[gt], kts.const(0, 1)))
        if self.barrier_bit:
            self._barrier = self.config_reg(name="barrier", width=1)
            self.wire(self._comparison_lcl, raw & ~self._barrier)
        else:
            self._barrier = None
            self.wire(self._comparison_lcl, raw)
        self.wire(self._comparison, self._comparison_lcl | ~self._enable_comparison | self._leader_finished | self._follower_finished)

        self.interfaces = {}
        self.interfaces['in_counter'] = self._leader
        self.interfaces['in_finished'] = self._leader_finished
        self.interfaces['out_counter'] = self._follower
        self.interfaces['out_finished'] = self._follower_finished
        self.interfaces['comparison'] = self._comparison

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def gen_hardware(self):
        if self.trimmed:
            self._gen_trimmed_hardware()
            return
        # The interface is leader count and follower count
        self._leader = self.input("leader_count", self.in_width)
        self._follower = self.input("follower_count", self.out_width)

        self._leader_signed = self.var("leader_signed", self.in_width + 1, is_signed=True)
        self._follower_signed = self.var("follower_signed", self.out_width + 1, is_signed=True)

        self._leader_finished = self.input("leader_finished", 1)
        self._follower_finished = self.input("follower_finished", 1)

        # The comparison will always be 1 if not enabled - for reduction and in ScheduleGenerator
        self._enable_comparison = self.config_reg(name="enable_comparison", width=1)
        # Output a single comparison
        self._comparison = self.output("comparison", 1)
        self._comparison_lcl = self.var("comparison_lcl", 1, is_signed=True)

        # Get number of entries in comparison for doing bit width
        num_comps = len(LFComparisonOperator)
        num_comps_bw = max(1, kts.clog2(num_comps))

        scalar_reg_width = 16
        # Generate a reconfigurable block if no hard op given
        if self.hard_op is None:
            # Make it a configuration register
            self._comp_reg = self.config_reg(name="comparison_op", width=num_comps_bw)
            # self._scalar_reg = self.config_reg(name="comparison_scalar", width=16)
            self._scalar_reg = self.config_reg(name="comparison_scalar", width=scalar_reg_width, is_signed=False)
            self._scalar_reg_signed = self.var(name="scalar_reg_signed", width=scalar_reg_width, is_signed=True)
            self.wire(self._scalar_reg_signed, kts.signed(self._scalar_reg))
        else:
            # Otherwise it is a hardened value...
            self._comp_reg = kts.const(self.hard_op.value, num_comps_bw)
            self._scalar_reg = kts.const(self.hard_scalar, width=scalar_reg_width)

        bigger_width_l = max(self.in_width, scalar_reg_width)
        bigger_width_f = max(self.out_width, scalar_reg_width)

        self._l_plus_s = self.var("l_plus_s", bigger_width_l + 1, is_signed=True)
        self.wire(self._l_plus_s, kts.ext(self._leader_signed + self._scalar_reg_signed, self._l_plus_s.width))
        self._f_plus_s = self.var("f_plus_s", bigger_width_f + 1, is_signed=True)
        # self.wire(self._f_plus_s, self._follower_signed + self._scalar_reg_signed)
        self.wire(self._f_plus_s, kts.ext(self._follower_signed + self._scalar_reg_signed, self._f_plus_s.width))

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
            # elif self._comp_reg == LFComparisonOperator.EQ.value:
            #     # self._comparison_lcl = self._leader == self._follower + self._scalar_reg
            #     self._comparison_lcl = self._leader_signed == self._f_plus_s
            # elif self._comp_reg == LFComparisonOperator.LTE.value:
            #     # self._comparison_lcl = self._leader <= self._follower + self._scalar_reg
            #     self._comparison_lcl = self._leader_signed <= self._f_plus_s

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

    def gen_bitstream(self, comparator, scalar, barrier=False):
        if self.trimmed:
            return self._gen_trimmed_bitstream(comparator, scalar, barrier)
        if barrier:
            raise ValueError("LFCompBlock: barrier=True needs barrier_bit hardware")
        # TODO: Fix the fact that this bitstream lives across multiple instances...
        self.clear_configuration()
        self.configure(self._enable_comparison, 1)
        self.configure(self._comp_reg, comparator)
        self.configure(self._scalar_reg, scalar)
        self_config = self.get_configuration()
        return self_config

    def _gen_trimmed_bitstream(self, comparator, scalar, barrier):
        lt, gt = LFComparisonOperator.LT.value, LFComparisonOperator.GT.value
        self.clear_configuration()
        self.configure(self._enable_comparison, 1)
        if barrier:
            if not self.barrier_bit:
                raise ValueError("LFCompBlock: barrier=True needs barrier_bit hardware")
            self.configure(self._barrier, 1)
            if self._comp_reg is not None:
                self.configure(self._comp_reg, lt)
            return self.get_configuration()
        if self._comp_reg is not None:
            self.configure(self._comp_reg, comparator)
        elif comparator != self.one_sided_op.value:
            if comparator not in (lt, gt):
                raise ValueError(f"LFCompBlock: op {comparator} on a one-sided {self.one_sided_op.name} block")
            # leader + s < follower  <=>  leader < follower + (-s)
            scalar = -scalar
        lo, hi = -(1 << (self.scalar_width - 1)), (1 << (self.scalar_width - 1)) - 1
        if not lo <= scalar <= hi:
            raise ValueError(f"LFCompBlock: scalar {scalar} does not fit the {self.scalar_width}-bit "
                             f"signed comparison_scalar [{lo}, {hi}]")
        self.configure(self._scalar_reg, scalar & ((1 << self.scalar_width) - 1))
        return self.get_configuration()


if __name__ == "__main__":
    # Check if this works
    comp = LFCompBlock(name='try_this', comparisonOp=LFComparisonOperator.EQ)
    comp.gen_hardware()
    kts.verilog(comp, filename='lfblock.sv')
