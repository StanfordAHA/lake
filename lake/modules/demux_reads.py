from kratos import *
import kratos as kts
from lake.modules.pipe_reg import PipeReg


def create_port_pkt(data_width,
                    consumer_ports):
    return PackedStruct(f"port_pkt_{data_width}_{consumer_ports}",
                        [("data", data_width, False),
                         ("port", consumer_ports, False),
                         ("valid", 1, False)])


class DemuxReads(Generator):
    '''
    Demux the SRAM reads into writes to the transpose buffers
    '''
    def __init__(self,
                 fetch_width,
                 banks,
                 int_out_ports):

        assert not (fetch_width & (fetch_width - 1)), "Memory width needs to be a power of 2"

        super().__init__("demux_reads", debug=True)
        # Absorb inputs
        self.fetch_width = fetch_width
        self.int_out_ports = int_out_ports
        self.banks = banks

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        self._data_in = self.input("data_in",
                                   self.fetch_width,
                                   size=self.banks,
                                   explicit_array=True,
                                   packed=True)
        self._valid_in = self.input("valid_in",
                                    self.banks)
        self._port_in = self.input("port_in",
                                   self.int_out_ports,
                                   size=self.banks,
                                   explicit_array=True,
                                   packed=True)

        # Outputs
        self._data_out = self.output("data_out",
                                     self.fetch_width,
                                     size=self.int_out_ports,
                                     explicit_array=True,
                                     packed=True)

        self._valid_out = self.output("valid_out",
                                      self.int_out_ports)

        # Vars
        self._done = self.var("done", self.int_out_ports)
        
        self.add_code(self.set_outs)

    @always_comb
    def set_outs(self):
        for i in range(self.int_out_ports):
            self._valid_out[i] = 0
            self._data_out[i] = 0
            self._done[i] = 0
            for j in range(self.banks):
                if ~self._done[i]:
                    if self._valid_in[j] & (self._port_in[j][i]):
                        self._valid_out[i] = 1
                        self._data_out[i] = self._data_in[j]
                        self._done[i] = 1

 
if __name__ == "__main__":
    db_dut = DemuxReads(fetch_width=64,
                        banks=4,
                        int_out_ports=2)

    verilog(db_dut, filename="demux_reads.sv")
