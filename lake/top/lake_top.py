from kratos import *
from lake.modules.passthru import *
from lake.modules.sram_stub import SRAMStub
from lake.modules.aggregation_buffer import AggregationBuffer

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
                use_sram_stub = 1,
                agg_height = 1,
                transpose_height = 1,
                max_agg_schedule = 12
                ):
        super().__init__("LakeTop")

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.iterator_support = iterator_support
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.use_sram_stub = use_sram_stub
        self.agg_height = agg_height
        self.transpose_height = transpose_height

        self._clk = self.clock("i_clk")
        self._rst_n = self.reset("i_rst_n")

        # Get the input ports from the interconnect
        self._data_in = self.input("i_data_in", self.data_width, size=self.interconnect_input_ports, packed=True, explicit_array=True)
        self._valid_in = self.input("i_valid_in", self.interconnect_input_ports, packed=True, explicit_array=True)

        self._data_out = self.output("o_data_out", self.data_width, size=self.interconnect_output_ports, packed=True, explicit_array=True)
        self._valid_out = self.output("o_valid_out", self.interconnect_output_ports, packed=True, explicit_array=True)

        # First wrap sram_stub
        sram_stub = SRAMStub(mem_width, 1024)
        self.add_child_generator(f"u_sram_stub_0", sram_stub)
        self.wire(sram_stub.i_data, self._in)
        self.wire(self._out, sram_stub.o_data)

        self.wire(sram_stub.i_addr, 0)
        self.wire(sram_stub.i_cen, 0)
        self.wire(sram_stub.i_wen, 0)
        self.wire(sram_stub.i_clk, self._clk)
        self.wire(sram_stub.i_rst_n, self._rst_n)

        # Add input aggregations buffers
        for i in range(self.interconnect_input_ports):
            # add children aggregator buffers...
            self.add_child(f"agg_in_{i}", AggregationBuffer(self.agg_height, self.data_width, self.mem_width))
            # Also add aggregation buffer config nodes...?
            # now wire it up


        # Add transpose buffers at output



        # Add address controller

lake_dut = LakeTop(16)
verilog(lake_dut, filename="lake_top.sv", check_active_high=False)