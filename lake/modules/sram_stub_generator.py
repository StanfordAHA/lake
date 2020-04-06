from kratos import *
from math import log
from lake.attributes.sram_port_attr import SRAMPortAttr


class SRAMStubGenerator(Generator):

    ##########################
    # Generation             #
    ##########################
    def __init__(self,
                 sram_name,
                 data_width,
                 width_mult,
                 depth):
        super().__init__(sram_name)

        # for provided external sram macro
        self.external = True

        self.data_width = data_width
        self.width_mult = width_mult
        self.depth = depth

        ############################
        # Clock and Reset          #
        ############################
        self._clk = self.clock("sram_clk")
        self._clk.add_attribute(SRAMPortAttr("sram clk"))

        ############################
        # Inputs                   #
        ############################

        # attribute indicates that all these ports will be renamed to match
        # the port names for external sram macro
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

        self._wtsel = self.input("sram_wtsel", 2)
        self._wtsel.add_attribute(SRAMPortAttr("sram wtsel"))

        self._rtsel = self.input("sram_rtsel", 2)
        self._rtsel.add_attribute(SRAMPortAttr("sram rtsel"))


if __name__ == "__main__":
    # to see interface, mark self.use_stub = True and self.external = False
    dut = SRAMStubGenerator(sram_name="TSMC",
                            data_width=16,
                            width_mult=1,
                            depth=124)
    verilog(dut, filename="tsmc_macro.sv")
