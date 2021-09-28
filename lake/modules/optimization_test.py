from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from kratos import *

class OptimizationTest(Generator):

    def __init__(self,
                 iterator_support=6,
                 config_width=16,
                 optimization_lvl=0):
        super().__init(f"optimization_test_iter_support_{iterator_support}_cfg_width_{config_width}_opt_lvl_{optimization_lvl}")

        self.iterator_support = iterator_support
        self.config_width = config_width
        self.optimization_lvl = optimization_lvl

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._valid_out = self.output("valid_out", 1)
        self._addr_out = self.output("addr_out", self.config_width)

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

        self.add_child("addr_gen",
                       addr_gen_tst,
                       clk=self._clk,
                       rst_n=self._rst_n    
                       )

        self.add_child("for_loop",
                       for_loop_tst,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=)