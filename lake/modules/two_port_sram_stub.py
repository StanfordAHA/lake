from kratos import *
from math import log


class TwoPortSRAMStub(Generator):
    '''
    2 port SRAM
    '''
    ##########################
    # Generation             #
    ##########################
    def __init__(self, width, depth, bypass):
        super().__init__("two_port_sram_stub", True)

        ############################
        # Clock and Reset          #
        ############################
        self.i_clk = self.clock("i_clk")
        self.i_rst_n = self.reset("i_rst_n", 1)

        ############################
        # Inputs                   #
        ############################
        self.i_wen = self.input("i_wen", 1)
        self.i_ren = self.input("i_ren", 1)

        self.i_wr_addr = self.input("i_wr_addr", clog2(depth))
        self.i_rd_addr = self.input("i_rd_addr", clog2(depth))

        self.i_data = self.input("i_data", width)

        ############################
        # Outputs                  #
        ############################
        self.o_data = self.output("o_data", width)

        ############################
        # Local Variables          #
        ############################
        self.data_array = self.var("data_array", width=width, size=depth, packed=True)

        self.bypass = bypass

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
        if(self.i_wen):
            self.data_array[self.i_wr_addr] = self.i_data

    @always((posedge, "i_clk"), (negedge, "i_rst_n"))
    def seq_data_out(self):
        if ~self.i_rst_n:
            self.o_data = 0
        elif self.i_ren:
            if (self.i_rd_addr == self.i_wr_addr) & (self.bypass == 1):
                self.o_data = self.i_data
            else:
                self.o_data = self.data_array[self.i_rd_addr]


if __name__ == "__main__":
    dut = TwoPortSRAMStub(16, 1024, 0)
    verilog(dut, filename="two_port_sram_stub.sv", check_active_high=False)
