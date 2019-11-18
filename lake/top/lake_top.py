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
                input_iterator_support = 6, # vector to support varying complexity on input + output ports
                output_iterator_support = 6,
                interconnect_input_ports = 1,
                interconnect_output_ports = 1,
                mem_input_ports = 1,
                mem_output_ports = 1,
                use_sram_stub = 1,
                agg_height = 1,
                transpose_height = 1,
                max_agg_schedule = 64
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

        if self.banks == 1:
            self.address_width = clog2(mem_depth)
        else:
            self.address_width = clog2(mem_depth) + clog2(banks)

        self._clk = self.clock("i_clk")
        self._rst_n = self.reset("i_rst_n")

        # Get the input ports from the interconnect
        self._data_in = self.input("i_data_in", self.data_width, size=self.interconnect_input_ports, packed=True, explicit_array=True)
        self._valid_in = self.input("i_valid_in", self.interconnect_input_ports, packed=True, explicit_array=True)

        self._data_out = self.output("o_data_out", self.data_width, size=self.interconnect_output_ports, packed=True, explicit_array=True)
        self._valid_out = self.output("o_valid_out", self.interconnect_output_ports, packed=True, explicit_array=True)

        # Add input aggregations buffers
        for i in range(self.interconnect_input_ports):
            # add children aggregator buffers...
            self.add_child(f"agg_in_{i}", AggregationBuffer(self.agg_height, self.data_width, self.mem_width))
            # Also add aggregation buffer config nodes...?

            self.wire(self[f"agg_in_{i}"].ports._clk, self._clk)
            self.wire(self[f"agg_in_{i}"].ports._rst_n, self._rst_n)

            self.wire(self[f"agg_in_{i}"].ports._data_in, self._data_in)
            self.wire(self[f"agg_in_{i}"].ports._valid_in, self._valid_in)

            self.wire(self[f"agg_in_{i}"].ports._write_act, ) # From input addr control
            # now wire it up
            self.wire(self[f"agg_in_{i}"].ports._data_out, ) # Demux these to SRAMs
            self.wire(self[f"agg_in_{i}"].ports._valid_out, ) # Also demux these


        self._mem_data_out = self.var("mem_data_out", self.mem_width, size=self.banks, packed=True, explicit_array=True)

        # Wrap sram_stub
        for i in range(self.banks):
            self.add_child(f"mem_{i}", SRAMStub(self.mem_width, self.mem_depth))
            self.wire(self[f"mem_{i}"].ports.i_clk, self._clk)
            self.wire(self[f"mem_{i}"].ports.i_rst_n, self._rst_n)
            self.wire(self[f"mem_{i}"].ports.i_data, 0) # Select based on input ctrl
            self.wire(self[f"mem_{i}"].ports.o_data, self._mem_data_out[i]) # Gather these in local variable

            self.wire(self[f"mem_{i}"].ports.i_addr, 0) # Demux
            self.wire(self[f"mem_{i}"].ports.i_cen, 0) # From input ctrl + output ctrl 
            self.wire(self[f"mem_{i}"].ports.i_wen, 0) # From input ctrl

        # Add transpose buffers at output



        # Add address controller

lake_dut = LakeTop(16)
verilog(lake_dut, filename="lake_top.sv", check_active_high=False)