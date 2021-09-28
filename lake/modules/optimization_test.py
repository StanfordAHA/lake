from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.utils.util import add_counter


class OptimizationTest(Generator):

    def __init__(self,
                 iterator_support=6,
                 config_width=16,
                 optimization_lvl=0):
        super().__init__(f"optimization_test_iter_support_{iterator_support}_cfg_width_{config_width}_opt_lvl_{optimization_lvl}")

        self.iterator_support = iterator_support
        self.config_width = config_width
        self.optimization_lvl = optimization_lvl

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._valid_out = self.output("valid_out", 1)
        self._addr_out = self.output("addr_out", self.config_width)

        # Create cycle counter to share...
        self._cycle_count = add_counter(self, "cycle_count", 16)

        for_loop_tst = ForLoop(iterator_support=iterator_support,
                               config_width=config_width,
                               optimization_lvl=optimization_lvl)

        sched_gen_tst = SchedGen(iterator_support=iterator_support,
                                 config_width=config_width,
                                 use_enable=True,
                                 optimization_lvl=optimization_lvl)

        addr_gen_tst = AddrGen(iterator_support=iterator_support,
                               config_width=config_width,
                               optimization_lvl=optimization_lvl)

        addit_args = {}

        if optimization_lvl == 0:
            addit_args = {"dim_counter_in": for_loop_tst.ports.dim_counter_out}
        elif optimization_lvl == 1:
            addit_args = {"inc_in": for_loop_tst.ports.inc_out,
                          "clr_in": for_loop_tst.ports.clr_out}
        elif optimization_lvl == 2:
            addit_args = {"mux_sel": for_loop_tst.ports.mux_sel_out}

        self.add_child("for_loop",
                       for_loop_tst,
                       clk=self._clk,
                       rst_n=self._rst_n)

        self.add_child("sched_gen",
                       sched_gen_tst,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       finished=for_loop_tst.ports.restart,
                       cycle_count=self._cycle_count,
                       valid_output=self._valid_out,
                       **addit_args)

        self.add_child("addr_gen",
                       addr_gen_tst,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       restart=for_loop_tst.ports.restart,
                       step=sched_gen_tst.ports.valid_output,
                       addr_out=self._addr_out,
                       **addit_args)

        self.wire(for_loop_tst.ports.step, sched_gen_tst.ports.valid_output)


if __name__ == "__main__":
    for opt_level in range(3):
        lake_dut = OptimizationTest(optimization_lvl=opt_level)
        verilog(lake_dut, filename=f"optimization_test_{opt_level}.sv",
                optimize_if=False,
                additional_passes={"lift config regs": lift_config_reg})
