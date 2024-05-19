from lake.spec.component import Component
from lake.utils.spec_enum import *
from lake.utils.util import add_counter
from lake.spec.iteration_domain import IterationDomain
import kratos as kts
from kratos import always_ff, always_comb, posedge, negedge


class ScheduleGenerator(Component):

    def __init__(self, dimensionality=6, stride_width=16):
        # super().__init__()
        self.dimensionality_support = dimensionality
        self.stride_width = stride_width

    def gen_hardware(self, id: IterationDomain = None, pos_reset=False):
        assert id is not None
        id_ext_width = id.get_extent_width()
        # Total cycle width right now is stride width + ext_width
        self.total_cycle_width = id_ext_width + self.stride_width + self.dimensionality_support + 1

        module_name = f"sched_gen_{self.dimensionality_support}_{self.stride_width}"
        super().__init__(name=module_name)
        ##########
        ### IO ###
        ##########
        ### Config Regs
        self._strides = self.config_reg(name="strides", width=self.stride_width,
                                   size=self.dimensionality_support,
                                   packed=True, explicit_array=True)

        # Using stride width instead of total cycle to keep config reg smaller
        self._starting_cycle = self.config_reg(name="starting_cycle", width=self.stride_width)
        ### Inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._clk_ctr = add_counter(self, "clk_ctr", bitwidth=self.total_cycle_width, increment=kts.const(1, 1))

        self._flush = self.input("flush", 1)
        self.add_attribute("sync-reset=flush")
        self._mux_sel = self.input("mux_sel", max(kts.clog2(self.dimensionality_support), 1))
        # Use signals directly for now
        self._ctrs = self.input("iterators", id_ext_width,
                                   size=self.dimensionality_support,
                                   packed=True, explicit_array=True)

        ### Outputs
        self._step_out = self.output("step", 1)

        ### Local variables
        self._current_cycle = self.var("current_cycle", self.total_cycle_width)
        self._strt_cycle = self.var("strt_cycle", self.total_cycle_width)
        self._step = self.var("step_lcl", 1)

        ### Logic
        self.wire(self._strt_cycle, kts.ext(self._starting_cycle, self.total_cycle_width))
        self.wire(self._step_out, self._step)

        # step is high when the current cycle matches the counter
        self.wire(self._step, self._clk_ctr == self._current_cycle)

        self.add_code(self.calculate_cycle_count)
        # self.add_code(self.calculate_cycle_delta)

    @always_comb
    def calculate_cycle_count(self):
        self._current_cycle = self._strt_cycle
        for i in range(self.dimensionality_support):
            self._current_cycle = self._current_cycle + self._ctrs[i] * self._strides[i]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def calculate_cycle_delta(self):
        if ~self._rst_n:
            self._current_cycle = self._strt_cycle
        elif self._flush:
            self._current_cycle = self._strt_cycle
        elif self._step:
            self._current_cycle = self._current_cycle + self._strides[self._mux_sel]

    def gen_bitstream(self):
        return super().gen_bitstream()

    def get_step(self):
        return self._step_out


class ExplicitScheduleGenerator(ScheduleGenerator):

    def __init__(self, dimensionality=16):
        super().__init__(dimensionality=dimensionality)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()


class RecurrentScheduleGenerator(ScheduleGenerator):

    def __init__(self, dimensionality=16):
        super().__init__(dimensionality=dimensionality)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()
