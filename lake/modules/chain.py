from kratos import *
import kratos as kts


class Chain(Generator):
    def __init__(self,
                 data_width,
                 interconnect_output_ports,
                 chain_idx_bits):
        super().__init__("Chain", debug=True)

        # generator parameters
        self.data_width = data_width
        self.interconnect_output_ports = interconnect_output_ports
        self.chain_idx_bits = chain_idx_bits

        # inputs

        self._enable_chain_output = self.input("enable_chain_output", 1)

        self._chain_idx_output = self.input("chain_idx_output",
                                            self.chain_idx_bits)

        self._curr_tile_valid_out = self.input("curr_tile_valid_out",
                                               self.interconnect_output_ports)

        self._curr_tile_data_out = self.input("curr_tile_data_out",
                                              self.data_width,
                                              size=self.interconnect_output_ports,
                                              packed=True,
                                              explicit_array=True)

        self._chain_valid_in = self.input("chain_valid_in",
                                          self.interconnect_output_ports)

        self._chain_data_in = self.input("chain_data_in",
                                         self.data_width,
                                         size=self.interconnect_output_ports,
                                         packed=True,
                                         explicit_array=True)

        self._chain_data_out = self.output("chain_data_out",
                                           self.data_width,
                                           size=self.interconnect_output_ports,
                                           packed=True,
                                           explicit_array=True)

        self._chain_valid_out = self.output("chain_valid_out",
                                            self.interconnect_output_ports)

        self._chain_data_out_inter = self.var("chain_data_out_inter",
                                              self.data_width,
                                              size=self.interconnect_output_ports,
                                              packed=True,
                                              explicit_array=True)

        self._chain_valid_out_inter = self.var("chain_valid_out_inter",
                                               self.interconnect_output_ports)

        self._data_out_tile = self.output("data_out_tile",
                                          self.data_width,
                                          size=self.interconnect_output_ports,
                                          packed=True,
                                          explicit_array=True)

        self._valid_out_tile = self.output("valid_out_tile",
                                           self.interconnect_output_ports)

        self.add_code(self.set_data_out)
        self.add_code(self.set_valid_out)
        self.add_code(self.set_chain_data)
        self.add_code(self.set_chain_valid)
        self.add_code(self.set_chain_values)

    @always_comb
    def set_data_out(self):
        if self._enable_chain_output:
            self._data_out_tile = self._chain_data_out_inter
        else:
            self._data_out_tile = self._curr_tile_data_out

    @always_comb
    def set_valid_out(self):
        if self._enable_chain_output:
            if ~(self._chain_idx_output == 0):
                for i in range(self.interconnect_output_ports):
                    self._valid_out_tile[i] = 0
            else:
                self._valid_out_tile = self._chain_valid_out_inter
        else:
            for j in range(self.interconnect_output_ports):
                self._valid_out_tile[j] = self._curr_tile_valid_out[j]

    @always_comb
    def set_chain_data(self):
        self._chain_data_out = self._chain_data_out_inter

    @always_comb
    def set_chain_valid(self):
        if (self._chain_idx_output == 0) | (~self._enable_chain_output):
            for i in range(self.interconnect_output_ports):
                self._chain_valid_out[i] = 0
        else:
            self._chain_valid_out = self._chain_valid_out_inter

    @always_comb
    def set_chain_values(self):
        for i in range(self.interconnect_output_ports):
            if self._chain_valid_in[i] == 0:
                self._chain_data_out_inter[i] = self._curr_tile_data_out[i]
                self._chain_valid_out_inter[i] = self._curr_tile_valid_out[i]
            else:
                self._chain_data_out_inter[i] = self._chain_data_in[i]
                self._chain_valid_out_inter[i] = self._chain_valid_in[i]


if __name__ == "__main__":
    dut = Chain(data_width=16,
                interconnect_output_ports=3,
                chain_idx_bits=1)
    verilog(dut, filename="chain.sv")
