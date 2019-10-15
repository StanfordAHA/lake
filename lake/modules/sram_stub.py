from kratos import *
from math import log

class SRAMStub(Generator):

    ##########################
    # Generation             #
    ##########################
    def __init__(self, width, depth):
        super().__init__("sram_stub", True)

        ############################
        # Clock and Reset          #
        ############################
        self.i_clk = self.clock("i_clk")

        ############################
        # Inputs                   #
        ############################
        self.i_wen = self.input("i_wen", 1)
        self.i_cen = self.input("i_cen", 1)
        self.i_addr = self.input("i_addr", clog2(depth))
        self.i_data = self.input("i_data", width)

        ############################
        # Outputs                  #
        ############################
        self.o_data = self.output("o_data", width)

        ############################
        # Local Variables          #
        ############################
        self.data_array = self.var("data_array", width=width, size=depth, packed=True)

        ############################
        # Add seq blocks           #
        ############################
        self.add_code(self.seq_data_access)
        self.add_code(self.seq_data_out)

    ##########################
    # Access sram array      #
    ##########################
    @always((posedge, "i_clk"))
    def seq_data_access(self):
        if self.i_cen & self.i_wen:
            self.data_array[self.i_addr] = self.i_data

    @always((posedge, "i_clk"))
    def seq_data_out(self):
        if self.i_cen:
            self.o_data = self.data_array[self.i_addr]


if __name__ == "__main__":
    dut = SRAMStub(16, 1024)
    verilog(dut, filename="sram_stub.sv", check_active_high=False)
