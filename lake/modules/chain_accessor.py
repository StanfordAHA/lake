from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint

import kratos as kts


class ChainAccessor(Generator):
    def __init__(self,
                 data_width,
                 interconnect_output_ports):
        super().__init__("Chain", debug=True)

        # generator parameters
        self.data_width = data_width
        self.interconnect_output_ports = interconnect_output_ports

        # chain enable configuration register
        self._chain_en = self.input("chain_en", 1)
        self._chain_en.add_attribute(ConfigRegAttr("Signal indicating whether to enable chaining"))
        self._chain_en.add_attribute(FormalAttr(self._chain_en.name, FormalSignalConstraint.SET0))

        # inputs
        self._curr_tile_data_out = self.input("curr_tile_data_out",
                                              self.data_width,
                                              size=self.interconnect_output_ports,
                                              packed=True,
                                              explicit_array=True)

        self._chain_data_in = self.input("chain_data_in",
                                         self.data_width,
                                         size=self.interconnect_output_ports,
                                         packed=True,
                                         explicit_array=True)

        self._accessor_output = self.input("accessor_output",
                                           self.interconnect_output_ports)

        self._data_out_tile = self.output("data_out_tile",
                                          self.data_width,
                                          size=self.interconnect_output_ports,
                                          packed=True,
                                          explicit_array=True)

        self.add_code(self.set_data_out)

    @always_comb
    def set_data_out(self):
        for i in range(self.interconnect_output_ports):
            if self._accessor_output[i]:
                self._data_out_tile[i] = self._curr_tile_data_out[i]
            else:
                if self._chain_en:
                    self._data_out_tile[i] = self._chain_data_in[i]
                else:
                    self._data_out_tile[i] = 0


if __name__ == "__main__":
    dut = ChainAccessor(data_width=16,
                        interconnect_output_ports=2)
    verilog(dut, filename="chain_acc.sv")
