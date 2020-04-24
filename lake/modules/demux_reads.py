from kratos import *
import kratos as kts


class DemuxReads(Generator):
    '''
    Demux the SRAM reads into writes to the transpose buffers
    '''
    def __init__(self,
                 fetch_width=16,
                 data_width=16,
                 banks=1,
                 int_out_ports=2,
                 strg_rd_ports=2):

        assert not (fetch_width & (fetch_width - 1)), "Memory width needs to be a power of 2"

        super().__init__("demux_reads")
        # Absorb inputs
        self.fetch_width = fetch_width
        self.data_width = data_width
        self.fw_int = int(self.fetch_width / self.data_width)
        self.int_out_ports = int_out_ports
        self.banks = banks
        self.strg_rd_ports = strg_rd_ports

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        self._data_in = self.input("data_in",
                                   self.data_width,
                                   size=(self.banks * self.strg_rd_ports,
                                         self.fw_int),
                                   explicit_array=True,
                                   packed=True)

        self._valid_in = self.input("valid_in", self.banks * self.strg_rd_ports)
        self._port_in = self.input("port_in",
                                   self.int_out_ports,
                                   size=self.strg_rd_ports * self.banks,
                                   explicit_array=True,
                                   packed=True)

        # Outputs
        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=(self.int_out_ports,
                                           self.fw_int),
                                     explicit_array=True,
                                     packed=True)

        self._valid_out = self.output("valid_out",
                                      self.int_out_ports)

        self._mem_valid_data = self.input("mem_valid_data", self.banks * self.strg_rd_ports)
        self._mem_valid_data_out = self.output("mem_valid_data_out", self.int_out_ports)

        # Vars
        self._done = self.var("done", self.int_out_ports)

        self.add_code(self.set_outs)

    @always_comb
    def set_outs(self):
        for i in range(self.int_out_ports):
            self._valid_out[i] = 0
            self._data_out[i] = 0
            self._done[i] = 0
            self._mem_valid_data_out[i] = 0
            for j in range(self.banks * self.strg_rd_ports):
                if ~self._done[i]:
                    if self._valid_in[j] & self._port_in[j][i]:
                        self._valid_out[i] = 1
                        self._data_out[i] = self._data_in[j]
                        self._mem_valid_data_out[i] = self._mem_valid_data[j]
                        self._done[i] = 1


if __name__ == "__main__":
    demux_dut = DemuxReads()
    verilog(demux_dut, filename="demux_reads.sv")
