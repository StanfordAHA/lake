from kratos import *
from lake.modules.passthru import *
from lake.modules.sram_stub import SRAMStub
from lake.modules.aggregation_buffer import AggregationBuffer
from lake.modules.input_addr_ctrl import InputAddrCtrl


class LakeTop(Generator):
    def __init__(self,
                 data_width=16,
                 mem_width=16,
                 mem_depth=512,
                 banks=2,
                 input_iterator_support=6,
                 output_iterator_support=6,
                 interconnect_input_ports=1,
                 interconnect_output_ports=1,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sram_stub=1,
                 agg_height=1,
                 transpose_height=1,
                 max_agg_schedule=64,
                 input_max_port_sched=64,
                 output_max_port_sched=64):
        super().__init__("LakeTop")

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.input_iterator_support = input_iterator_support
        self.output_iterator_support = output_iterator_support
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.use_sram_stub = use_sram_stub
        self.agg_height = agg_height
        self.transpose_height = transpose_height
        self.max_agg_schedule = max_agg_schedule
        self.input_max_port_sched = input_max_port_sched
        self.output_max_port_sched = output_max_port_sched
        self.input_port_sched_width = clog2(self.interconnect_input_ports)

        # Inputss
        self._i_strides = []  # 2D
        self._i_ranges = []  # 2D
        self._i_port_scheds = []  # Config as well
        self._i_dimensionalities = []

        # phases = [] TODO
        self._i_starting_addrs = []  # 1D
        for i in range(self.interconnect_input_ports):
            self._i_strides.append(self.input(f"stride_i_{i}",
                                              32,
                                              size=self.input_iterator_support,
                                              packed=True,
                                              explicit_array=True))
            self._i_ranges.append(self.input(f"range_i_{i}",
                                             32,
                                             size=self.input_iterator_support,
                                             packed=True,
                                             explicit_array=True))
            self._i_starting_addrs.append(self.input(f"starting_addr_i_{i}", 32))
            self._i_dimensionalities.append(self.input(f"dimensionality_i_{i}", 4))

        for i in range(self.banks):
            self._i_port_scheds.append(self.input(f"i_port_sched_b_{i}",
                                                  self.input_port_sched_width,
                                                  size=self.input_max_port_sched,
                                                  packed=True,
                                                  explicit_array=True))
        self._i_port_periods = self.input("i_port_periods",
                                          clog2(self.input_max_port_sched),
                                          size=self.banks)

        self._o_strides = []  # 2D
        self._o_ranges = []  # 2D
        self._o_port_scheds = []  # Config as well
        self._o_dimensionalities = []

        # phases = [] TODO
        self._o_starting_addrs = []  # 1D
        for i in range(self.interconnect_output_ports):
            self._o_strides.append(self.input(f"stride_o_{i}",
                                              32,
                                              size=self.iterator_support,
                                              packed=True,
                                              explicit_array=True))
            self._o_ranges.append(self.input(f"range_o_{i}",
                                             32,
                                             size=self.iterator_support,
                                             packed=True,
                                             explicit_array=True))
            self._o_starting_addrs.append(self.input(f"starting_addr_o_{i}", 32))
            self._o_dimensionalities.append(self.input(f"dimensionality_o_{i}", 4))

        if self.banks == 1:
            self.address_width = clog2(mem_depth)
        else:
            self.address_width = clog2(mem_depth) + clog2(banks)

        self._clk = self.clock("i_clk")
        self._rst_n = self.reset("i_rst_n")

        # Get the input ports from the interconnect
        self._data_in = self.input("i_data_in",
                                   self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)
        self._valid_in = self.input("i_valid_in",
                                    self.interconnect_input_ports,
                                    packed=True,
                                    explicit_array=True)

        self._data_out = self.output("o_data_out",
                                     self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)
        self._valid_out = self.output("o_valid_out",
                                      self.interconnect_output_ports,
                                      packed=True,
                                      explicit_array=True)

        # Add input aggregations buffers
        for i in range(self.interconnect_input_ports):
            # add children aggregator buffers...
            self.add_child(f"agg_in_{i}",
                           AggregationBuffer(self.agg_height,
                                             self.data_width,
                                             self.mem_width))
            # Also add aggregation buffer config nodes...?
            self.wire(self[f"agg_in_{i}"].ports._clk, self._clk)
            self.wire(self[f"agg_in_{i}"].ports._rst_n, self._rst_n)

            self.wire(self[f"agg_in_{i}"].ports._data_in, self._data_in)
            self.wire(self[f"agg_in_{i}"].ports._valid_in, self._valid_in)

            self.wire(self[f"agg_in_{i}"].ports._write_act, )  # From input addr control
            # now wire it up
            self.wire(self[f"agg_in_{i}"].ports._data_out, )  # Demux these to SRAMs
            self.wire(self[f"input_addr_ctrl"].ports.valid_in[i],
                      self[f"agg_in_{i}"].ports._valid_out)  # Also demux these

        # Connect these inputs ports to an address generator'
        self.add_child(f"input_addr_ctrl",
                       InputAddrCtrl(self.interconnect_input_ports,
                                     self.mem_depth,
                                     banks=self.banks,
                                     iterator_support=self.iterator_support,
                                     max_port_schedule=64,
                                     address_width=self.address_width))
        # Normal wires
        self.wire(self[f"input_addr_ctrl"].ports.clk, self._clk)
        self.wire(self[f"input_addr_ctrl"].ports.rst_n, self._rst_n)

        for i in range(self.interconnect_input_ports):
            # Send in each access iterator configuration
            self.wire(self[f"input_addr_ctrl"].ports[f"stride_p_{i}"], self._i_strides[i])
            self.wire(self[f"input_addr_ctrl"].ports[f"range_p_{i}"], self._i_ranges[i])
            self.wire(self[f"input_addr_ctrl"].ports[f"starting_addr_p_{i}"],
                      self._i_starting_addrs[i])
            self.wire(self[f"input_addr_ctrl"].ports[f"dimensionality_{i}"],
                      self._i_dimensionalities[i])

        for i in range(self.banks):
            self.wire(self[f"input_addr_ctrl"].ports[f"port_sched_b_{i}"], self._i_port_scheds[i])
        self.wire(self[f"input_addr_ctrl"].ports.port_periods, self._i_port_periods)

        self._mem_data_out = self.var("mem_data_out",
                                      self.mem_width,
                                      size=self.banks,
                                      packed=True,
                                      explicit_array=True)

        # Wrap sram_stub
        for i in range(self.banks):
            self.add_child(f"mem_{i}", SRAMStub(self.mem_width, self.mem_depth))
            self.wire(self[f"mem_{i}"].ports.i_clk, self._clk)
            self.wire(self[f"mem_{i}"].ports.i_rst_n, self._rst_n)
            self.wire(self[f"mem_{i}"].ports.i_data, 0)  # Select based on input ctrl
            self.wire(self[f"mem_{i}"].ports.o_data,
                      self._mem_data_out[i])  # Gather these in local variable

            self.wire(self[f"mem_{i}"].ports.i_addr, 0)  # Demux
            self.wire(self[f"mem_{i}"].ports.i_cen, 0)  # From input ctrl + output ctrl
            self.wire(self[f"mem_{i}"].ports.i_wen, 0)  # From input ctrl

        # Add transpose buffers at output
        # Add address controller


if __name__ == "__main__":
    lake_dut = LakeTop(16)
    verilog(lake_dut, filename="lake_top.sv", check_active_high=False)
