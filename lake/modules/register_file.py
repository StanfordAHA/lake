from kratos import *
from math import log


class RegisterFile(Generator):

    ##########################
    # Generation             #
    ##########################
    def __init__(self,
                 data_width,
                 write_ports,
                 read_ports,
                 width_mult,
                 depth
                 ):
        super().__init__("register_file")

        self.width_mult = width_mult
        self.data_width = data_width
        self.write_ports = write_ports
        self.read_ports = read_ports
        self.depth = depth

        ############################
        # Clock and Reset          #
        ############################
        self._clk = self.clock("clk")

        ############################
        # Inputs                   #
        ############################
        self._wen = self.input("wen", self.write_ports)
        self._wr_addr = self.input("wr_addr", clog2(self.depth),
                                   size=self.write_ports,
                                   explicit_array=True,
                                   packed=True)
        self._rd_addr = self.input("rd_addr", clog2(self.depth),
                                   size=self.read_ports,
                                   explicit_array=True,
                                   packed=True)

        self._data_in = self.input("data_in",
                                   self.data_width,
                                   size=(self.write_ports,
                                         self.width_mult),
                                   explicit_array=True,
                                   packed=True)

        ############################
        # Outputs                  #
        ############################
        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=(self.read_ports,
                                           self.width_mult),
                                     explicit_array=True,
                                     packed=True)

        ############################
        # Local Variables          #
        ############################
        # self._data_array = self.var("data_array",
        #                             self.data_width,
        #                             size=self.depth,
        #                             packed=True,
        #                             explicit_array=True)
        self._data_array = self.var("data_array",
                                    self.data_width,
                                    size=(self.depth,
                                          self.width_mult),
                                    packed=True,
                                    explicit_array=True)

        ############################
        # Add seq blocks           #
        ############################
        self.add_code(self.seq_data_access)
        self.add_code(self.comb_data_out)

    ##########################
    # Access sram array      #
    ##########################
    @always_ff((posedge, "clk"))
    def seq_data_access(self):
        for i in range(self.write_ports):
            if self._wen[i]:
                self._data_array[self._wr_addr[i]] = self._data_in[i]

    @always_comb
    def comb_data_out(self):
        for i in range(self.read_ports):
            self._data_out[i] = self._data_array[self._rd_addr[i]]


if __name__ == "__main__":
    dut = RegisterFile(16, 1, 2, 1, 64)
    verilog(dut, filename="register_file.sv")
