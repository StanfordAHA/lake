import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.utils.util import add_counter, safe_wire, register, intercept_cfg, observe_cfg
from lake.attributes.config_reg_attr import ConfigRegAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class OnyxPEInterface(kts.Generator):
    def __init__(self,
                 data_width=16):

        super().__init__("PE", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True

        # For consistency with Core wrapper in garnet...
        self.total_sets = 0

        # inputs
        self._clk = self.clock("CLK")
        # self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("ASYNCRESET")
        self._clk_en = self.clock_en("clk_en", 1)

        # Instruction
        self._inst = self.input("inst", 84)
        self._inst.add_attribute(ConfigRegAttr("PE Instruction"))

        self._data0 = self.input("data0", self.data_width)
        self._data1 = self.input("data1", self.data_width)
        self._data2 = self.input("data2", self.data_width)
        self._bit0 = self.input("bit0", 1)
        self._bit1 = self.input("bit1", 1)
        self._bit2 = self.input("bit2", 1)

        # self._config_addr = self.input("config_addr", 8)
        # self._config_data = self.input("config_data", 32)
        # self._config_en = self.input("config_en", 1)

        self._O0 = self.output("O0", self.data_width)
        self._O1 = self.output("O1", 1)
        # self._O2 = self.output("O2", 2 * self.data_width)

        self.external = True


if __name__ == "__main__":

    pe_dut = OnyxPEInterface(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(pe_dut, filename="pe.sv",
            optimize_if=False)
