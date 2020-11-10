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
                 depth,
                 read_delay=0
                 ):
        super().__init__("register_file")

        self.width_mult = width_mult
        self.data_width = data_width
        self.write_ports = write_ports
        self.read_ports = read_ports
        self.depth = depth
        self.read_delay = read_delay

        ############################
        # Clock and Reset          #
        ############################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        ############################
        # Inputs                   #
        ############################

        if self.read_ports == 1:
            self._rd_addr = self.input("rd_addr", clog2(self.depth))
            self._data_out = self.output("data_out", self.data_width,
                                         size=self.width_mult,
                                         explicit_array=True,
                                         packed=True)
        else:
            self._rd_addr = self.input("rd_addr", clog2(self.depth),
                                       size=self.read_ports,
                                       explicit_array=True,
                                       packed=True)
            self._data_out = self.output("data_out",
                                         self.data_width,
                                         size=(self.read_ports,
                                               self.width_mult),
                                         explicit_array=True,
                                         packed=True)

        self._wen = self.input("wen", self.write_ports)
        if self.write_ports == 1:
            self._wr_addr = self.input("wr_addr", clog2(self.depth))
            self._data_in = self.input("data_in",
                                       self.data_width,
                                       size=self.width_mult,
                                       explicit_array=True,
                                       packed=True)
        else:
            self._wr_addr = self.input("wr_addr", clog2(self.depth),
                                       size=self.write_ports,
                                       explicit_array=True,
                                       packed=True)
            self._data_in = self.input("data_in",
                                       self.data_width,
                                       size=(self.write_ports,
                                             self.width_mult),
                                       explicit_array=True,
                                       packed=True)

        ############################
        # Local Variables          #
        ############################
        self._data_array = self.var("data_array",
                                    self.data_width,
                                    size=(self.depth,
                                          self.width_mult),
                                    packed=True,
                                    explicit_array=True)

        ############################
        # Add seq blocks           #
        ############################
        if self.write_ports == 1:
            self.add_code(self.seq_data_access_one_w)
        else:
            self.add_code(self.seq_data_access)

        if self.read_ports == 1:
            if self.read_delay == 1:
                self._ren = self.input("ren", 1)
                self.add_code(self.seq_data_out_one_r)
            else:
                self.add_code(self.comb_data_out_one_r)
        else:
            if self.read_delay == 1:
                self.add_code(self.seq_data_out)
            else:
                self.add_code(self.comb_data_out)

    ##########################
    # Access sram array      #
    ##########################
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def seq_data_access(self):
        for i in range(self.write_ports):
            if ~self._rst_n:
                self._data_array = 0
            elif self._wen[i]:
                self._data_array[self._wr_addr[i]] = self._data_in[i]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def seq_data_access_one_w(self):
        if ~self._rst_n:
            self._data_array = 0
        elif self._wen:
            self._data_array[self._wr_addr] = self._data_in

    @always_comb
    def comb_data_out(self):
        for i in range(self.read_ports):
            self._data_out[i] = self._data_array[self._rd_addr[i]]

    @always_comb
    def comb_data_out_one_r(self):
        self._data_out = self._data_array[self._rd_addr]

    @always_ff((posedge, "clk"))
    def seq_data_out(self):
        for i in range(self.read_ports):
            if self._ren:
                self._data_out[i] = self._data_array[self._rd_addr[i]]

    @always_ff((posedge, "clk"))
    def seq_data_out_one_r(self):
        if self._ren:
            self._data_out = self._data_array[self._rd_addr]


if __name__ == "__main__":
    dut = RegisterFile(16, 1, 2, 1, 64)
    verilog(dut, filename="register_file.sv", check_flip_flop_always_ff=False)
