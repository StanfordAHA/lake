from kratos import *
import kratos as kts


class OneStateFSM(Generator):
    def __init__(self, debug: bool = False, is_clone: bool = False, internal_generator=None):
        super().__init__("onestate", debug, is_clone, internal_generator)

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self.boring_fsm = self.add_fsm("boring_fsm", reset_high=False)
        START = self.boring_fsm.add_state("START")

        self._boring_var = self.var("boring_var", 1)
        ####################
        # START
        ####################
        # Get the first block size...
        START.next(START, None)

        self.boring_fsm.output(self._boring_var)

        START.output(self._boring_var, 1)

        self.boring_fsm.set_start_state(START)

        # Force FSM realization first so that flush gets added...
        kts.passes.realize_fsm(self.internal_generator)


if __name__ == "__main__":
    onestate_dut = OneStateFSM()
    verilog(onestate_dut, filename="onestate.sv",
            optimize_if=False)
