from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr


class ConfigReg(Generator):
    def __init__(self,
                 name,
                 data_width,
                 init_val):
        super().__init__(f"config_reg_{name}_{data_width}")

        self.add_attribute(ConfigRegAttr())

        self.init_val = init_val
        self.data_width = data_width

        self._out = self.output("o_data_out", data_width)
        self._clk = self.clock("i_clk")
        self._rst_n = self.reset("i_rst_n")

        self.add_code(self.set_out)

    @always((posedge, "i_clk"), (negedge, "i_rst_n"))
    def set_out(self):
        if ~self._rst_n:
            self._out = self.init_val


if __name__ == "__main__":
    data_width = 16
    configregdut = ConfigReg(name="config_test", data_width=data_width, init_val=2)
    verilog(configregdut, filename=f"config_reg_{data_width}.sv")
