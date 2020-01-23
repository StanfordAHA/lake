from kratos import *
import kratos as kts
from lake.modules.pipe_reg import PipeReg


def create_port_pkt(data_width,
                    consumer_ports):
    return PackedStruct(f"port_pkt_{data_width}_{consumer_ports}",
                        [("data", data_width, False),
                        ("port", consumer_ports, False),
                        ("valid", 1, False)])

class RWArbiter(Generator):
    '''
    The read_write arbiter dynamically schedules reads and writes to
    a single bank of SRAM
    '''
    def __init__(self,
                 fetch_width,
                 int_out_ports):

        super().__init__("rw_arbiter", debug=True)
        # Absorb inputs
        self.fetch_width = fetch_width
        self.int_out_ports = int_out_ports

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Generate the packed struct of the right size
        port_pkt_struct = create_port_pkt(self.fetch_width, self.int_out_ports)

        # Inputs
        self._wen_in = self.input("wen_in", 1)
        self._wen_en = self.input("wen_en", 1)
        self._w_data = self.input("w_data", self.fetch_width)

        self._data_from_mem = self.input("data_from_mem", self.fetch_width)

        self._ren_in = self.input("ren_in", self.int_out_ports)
        self._ren_en = self.input("ren_en", 1)

        # Outputs
        #self.port_packed("out_pkt", PortDirection.Out, port_pkt_struct)
        self._out_data = self.output("out_data", self.fetch_width)
        self._out_port = self.output("out_port", self.int_out_ports)
        self._out_valid = self.output("out_valid", 1)

        self._cen_mem = self.output("cen_mem", 1)
        self._wen_mem = self.output("wen_mem", 1)
        self._data_to_mem = self.output("data_to_mem", self.fetch_width)

        # Local
        #self._rd_data = self.var("rd_data", self.fetch_width)
        self._rd_valid = self.var("rd_valid", 1)
        self._rd_port = self.var("rd_port", self.int_out_ports)
        self._next_rd_port = self.var("next_rd_port", self.int_out_ports)

        self._done = self.var("done", 1)

        # Generation
        # self.add_child("pipe_out",
        #                PipeReg(self["out_pkt"].width, 1))
        # self.wire(self["pipe_out"].ports.i_clk, self._clk)
        # self.wire(self["pipe_out"].ports.i_rst_n, self._rst_n)
        # self.wire(self["pipe_out"].ports.i_clk_en, const(1, 1))
        # self.wire(self["pipe_out"].ports.i_data_in,
        #           kts.concat(self._data_from_mem,
        #                      self._rd_port,
        #                      self._rd_valid))
        # self.wire(self["pipe_out"].ports.o_data_out)
        

        self.add_code(self.mem_controls)
        self.add_code(self.set_next_read_port)
        self.add_code(self.next_read_valid)
        self.add_code(self.output_stage)

        # Consume wr over read

    @always_comb
    # Prioritizes writes over reads
    def mem_controls(self):
        self._wen_mem = self._wen_in
        self._cen_mem = (self._wen_in) | kts.reduce_or(self._ren_in.r_or())
        self._data_to_mem = self._w_data

    @always_comb
    # Find lowest ready
    def set_next_read_port(self):
        self._next_rd_port = 0
        self._done = 0
        for i in range(self.int_out_ports):
            if ~self._done:
                if self._ren_in[i]:
                    self._next_rd_port = i
                    self._done = 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def next_read_valid(self):
        if ~self._rst_n:
            self._rd_port = 0
            self._rd_valid = 0
        else:
            self._rd_valid = ~self._wen_in & kts.reduce_or(self._ren_in.r_or())
            self._rd_port = self._next_rd_port

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def output_stage(self):
        if ~self._rst_n:
            self._out_data = 0
            self._out_port = 0
            self._out_valid = 0
        else:
            self._out_data = self._data_from_mem
            self._out_port = self._rd_port
            self._out_valid = self._rd_valid


if __name__ == "__main__":
    db_dut = RWArbiter(fetch_width=64,
                       int_out_ports=3)

    verilog(db_dut,
            filename="rw_arbiter.sv",
            check_multiple_driver=False)