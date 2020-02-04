from kratos import *
from math import log


class SRAMStub(Generator):

    ##########################
    # Generation             #
    ##########################
    def __init__(self,
                 width,
                 depth):
        super().__init__("sram_stub")

        ############################
        # Clock and Reset          #
        ############################
        self._clk = self.clock("clk")

        ############################
        # Inputs                   #
        ############################
        self._wen = self.input("wen", 1)
        self._cen = self.input("cen", 1)
        self._addr = self.input("addr", clog2(depth))
        self._data_in = self.input("data_in", width)

        ############################
        # Outputs                  #
        ############################
        self._data_out = self.output("data_out", width)

        ############################
        # Local Variables          #
        ############################
        self._data_array = self.var("data_array", width=width, size=depth, packed=True)

        ############################
        # Add seq blocks           #
        ############################
        self.add_code(self.seq_data_access)
        self.add_code(self.seq_data_out)

    ##########################
    # Access sram array      #
    ##########################
    @always_ff((posedge, "clk"))
    def seq_data_access(self):
        if self._cen & self._wen:
            self._data_array[self._addr] = self._data_in

    @always_ff((posedge, "clk"))
    def seq_data_out(self):
        if self._cen:
            self._data_out = self._data_array[self._addr]


if __name__ == "__main__":
    dut = SRAMStub(16, 1024)
    verilog(dut, filename="sram_stub.sv")
