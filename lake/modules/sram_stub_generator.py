from kratos import *
from math import log
from lake.attributes.sram_port_attr import SRAMPortAttr


class SRAMStubGenerator(Generator):

    ##########################
    # Generation             #
    ##########################
    def __init__(self,
                 sram_name,
                 # expected order (alphabetical): addr, cen, clk, data_in, data_out, wen
                 data_width,
                 width_mult,
                 depth):
        super().__init__(sram_name)

        self.external = True

        self.data_width = data_width
        self.width_mult = width_mult
        self.depth = depth

        ############################
        # Clock and Reset          #
        ############################
        self._clk = self.clock("sram_clk")

        ############################
        # Inputs                   #
        ############################
        self._wen = self.input("sram_wen", 1)
        self._wen.add_attribute(SRAMPortAttr("sram wen"))

        self._cen = self.input("sram_cen", 1)
        self._cen.add_attribute(SRAMPortAttr("sram cen"))

        self._addr = self.input("sram_addr", clog2(self.depth))
        self._addr.add_attribute(SRAMPortAttr("sram addr"))

        self._data_in = self.input("sram_data_in",
                                   self.data_width * self.width_mult)
        self._data_in.add_attribute(SRAMPortAttr("sram data in"))

        ############################
        # Outputs                  #
        ############################
        self._data_out = self.output("sram_data_out",
                                     self.data_width * self.width_mult)
        self._data_out.add_attribute(SRAMPortAttr("sram data out"))

if __name__ == "__main__":
    dut = SRAMStubGenerator("TSMC", 16, 1, 124)
    verilog(dut, filename="tsmc_macro.sv")
