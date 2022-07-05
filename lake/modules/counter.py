import kratos as kts
from lake.utils.util import add_counter


class Counter(kts.Generator):

    def __init__(self, name: str,
                 bitwidth=16,
                 increment=None,
                 pos_reset=False):
        super().__init__(name, debug=True)

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        inc_var = kts.const(1, 1)
        if increment is not None:
            inc_var = increment
        self.ctr = add_counter(self, f"{name}_ctr", bitwidth, increment=inc_var, clear=None, pos_reset=pos_reset)

        self._count_out = self.output("count_out", bitwidth)
        self.wire(self._count_out, self.ctr)

        self.add_attribute("sync-reset=flush")
        kts.passes.auto_insert_sync_reset(self.internal_generator)
        flush_port = self.internal_generator.get_port("flush")


if __name__ == "__main__":
    ctr_dut = Counter(name="test_ctr",
                      bitwidth=16,
                      increment=None)
    kts.verilog(ctr_dut, filename="counter.sv",
                optimize_if=False)
