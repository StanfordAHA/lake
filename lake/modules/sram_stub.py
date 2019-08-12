from kratos import *
from math import log

class SRAMStub(Generator):

    ##########################
    # Generation             #
    ##########################
    def __init__(self, width, depth):
        super().__init__("SRAMStub")

        ############################
        # Clock and Reset          #
        ############################
        self.i_clk = self.clock("i_clk")
        self.i_rst_n = self.reset("i_rst_n", 1)

        ############################
        # Inputs                   #
        ############################
        self.i_wen_n = self.input("i_wen_n", 1)
        self.i_addr = self.input("i_addr", int(log(depth,2)))
        self.i_data = self.input("i_data", width)

        ############################
        # Outputs                  #
        ############################
        self.o_data = self.output("o_data", width)

        ############################
        # Local Variables          #
        ############################
        self.data_array = self.var("data_array", width=width, size=depth)

        ############################
        # Add seq blocks           #
        ############################
        self.add_code(self.seq_data_access)

    ##########################
    # Access sram array      #
    ##########################
    @always((posedge, "i_clk")) #, (negedge, "i_rst_n"))
    def seq_data_access(self):
        if ~self.i_rst_n:
            self.o_data = 0
        else:
            if ~self.i_wen_n:
                self.data_array[self.i_addr] = self.i_data
            else:
                self.o_data = self.data_array[self.i_addr]


