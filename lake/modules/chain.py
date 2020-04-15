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

        # data and valid from current tile
        self._curr_tile_valid_out = self.input("curr_tile_valid_out",
                                               self.interconnect_output_ports)

        self._curr_tile_data_out = self.input("curr_tile_data_out",
                                              self.data_width,
                                              size=self.interconnect_output_ports,
                                              packed=True,
                                              explicit_array=True)

        # data and valid from another tile chained to this tile
        self._chain_valid_in = self.input("chain_valid_in",
                                          self.interconnect_output_ports)

        self._chain_data_in = self.input("chain_data_in",
                                         self.data_width,
                                         size=self.interconnect_output_ports,
                                         packed=True,
                                         explicit_array=True)

        self._tile_output_en = self.input("tile_output_en",
                                          self.interconnect_output_ports)
        # muxed data and valid outputs
        self._chain_data_out = self.output("chain_data_out",
                                           data_width,
                                           size=interconnect_output_ports,
                                           packed=True,
                                           explicit_array=True)

        self._chain_valid_out = self.output("chain_valid_out",
                                            interconnect_output_ports)

        # individual tile outputs
        self._data_out_tile = self.output("data_out_tile",
                                          self.data_width,
                                          size=self.interconnect_output_ports,
                                          packed=True,
                                          explicit_array=True)

        self._valid_out_tile = self.output("valid_out_tile",
                                           self.interconnect_output_ports)

        self.add_code(self.set_data_out)
        self.add_code(self.set_valid_out)
        self.add_code(self.set_chain_outputs)

    @always_comb
    def set_data_out(self):
        if self._chain_idx_output == 0:
            self._data_out_tile = self._chain_data_out
        else:
            self._data_out_tile = self._curr_tile_data_out

    @always_comb
    def set_valid_out(self):
        if self._chain_idx_output == 0:
            self._valid_out_tile = self._chain_valid_out
        else:
            for j in range(self.interconnect_output_ports):
                if self._tile_output_en[j] == 0:
                    self._valid_out_tile[j] = 0
                elif self._enable_chain_output:
                    self._valid_out_tile[j] = 0
                else:
                    self._valid_out_tile[j] = self._curr_tile_valid_out[j]

    @always_comb
    def set_chain_outputs(self):
        if ~self._enable_chain_output | (self._chain_idx_output == 0):
            self._chain_data_out = self._curr_tile_data_out
            for i in range(self.interconnect_output_ports):
                self._chain_valid_out[i] = 0
        else:
            for i in range(self.interconnect_output_ports):
                if self._tile_output_en[i] == 1:
                    self._chain_data_out[i] = self._curr_tile_data_out[i]
                    self._chain_valid_out[i] = self._curr_tile_valid_out[i]
                else:
                    self._chain_data_out[i] = self._chain_data_in[i]
                    self._chain_valid_out[i] = self._chain_valid_in[i]


if __name__ == "__main__":
    dut = Chain(data_width=16,
                interconnect_output_ports=3,
                chain_idx_bits=1)
    verilog(dut, filename="chain.sv")
