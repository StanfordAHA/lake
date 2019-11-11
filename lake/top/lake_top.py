from kratos import *
from lake.modules.passthru import *
from lake.modules.sram_stub import SRAMStub

class LakeTop(Generator):
    def __init__(
                self, 
                data_width = 16,
                mem_width = 16,
                mem_depth = 512,
                banks = 2,
                iterator_support = 6, # vector to support varying complexity on input + output ports
                interconnect_input_ports = 1,
                interconnect_output_ports = 1,
                mem_input_ports = 1,
                mem_output_ports = 1,
                use_sram_stub = 1
                ):
        super().__init__("LakeTop")
        self._in = self.input("i_data_in", width)
        self._out = self.output("o_data_out", width)
        self._clk = self.clock("i_clk")
        self._rst_n = self.reset("i_rst_n")

        # First wrap sram_stub
        sram_stub = SRAMStub(width, 1024)
        self.add_child_generator(f"u_sram_stub_0", sram_stub)
        self.wire(sram_stub.i_data, self._in)
        self.wire(self._out, sram_stub.o_data)

        self.wire(sram_stub.i_addr, 0)
        self.wire(sram_stub.i_cen, 0)
        self.wire(sram_stub.i_wen, 0)
        self.wire(sram_stub.i_clk, self._clk)
        self.wire(sram_stub.i_rst_n, self._rst_n)


        # Add input aggregations buffers



        # Add transpose buffers at output



        # Add address controller

lake_dut = LakeTop(16)
verilog(lake_dut, filename="lake_top.sv", check_active_high=False)