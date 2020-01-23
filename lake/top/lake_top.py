from kratos import *
from lake.modules.passthru import *
from lake.modules.sram_stub import SRAMStub
from lake.modules.aggregation_buffer import AggregationBuffer
from lake.modules.input_addr_ctrl import InputAddrCtrl
from lake.modules.agg_aligner import AggAligner
from lake.modules.rw_arbiter import RWArbiter


class LakeTop(Generator):
    def __init__(self,
                 data_width=16,
                 mem_width=32,
                 mem_depth=512,
                 banks=2,
                 input_iterator_support=6,
                 output_iterator_support=6,
                 interconnect_input_ports=1,
                 interconnect_output_ports=1,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sram_stub=1,
                 agg_height=2,
                 transpose_height=1,
                 max_agg_schedule=64,
                 input_max_port_sched=64,
                 output_max_port_sched=64,
                 align_input=1,
                 max_line_length=2048):
        super().__init__("LakeTop", debug=True)

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
        self.align_input = align_input
        self.max_line_length = max_line_length

        # phases = [] TODO

        # Inputs
        #
        self._i_strides = self.input("stride_i",
                                     32,
                                     size=(self.input_iterator_support,
                                           self.interconnect_input_ports),
                                     packed=True,
                                     explicit_array=True)
        self._i_ranges = self.input("range_i",
                                     32,
                                     size=(self.input_iterator_support, 
                                           self.interconnect_input_ports),
                                     packed=True,
                                     explicit_array=True)
        self._i_starting_addrs = self.input("starting_addr_i",
                                            32,
                                            size=self.interconnect_input_ports)
        #self._i_port_scheds = []  # Config as well
        self._i_dimensionalities = self.input("dimensionality_i",
                                              4,
                                              size=self.interconnect_input_ports)

        self._o_strides = self.input("stride_o",
                                     32,
                                     size=(self.input_iterator_support, 
                                           self.interconnect_input_ports),
                                     packed=True,
                                     explicit_array=True)
        self._o_ranges = self.input("range_o",
                                     32,
                                     size=(self.input_iterator_support, 
                                           self.interconnect_input_ports),
                                     packed=True,
                                     explicit_array=True)
        self._o_starting_addrs = self.input("starting_addr_o",
                                            32,
                                            size=self.interconnect_input_ports)
        #self._o_port_scheds = []  # Config as well
        self._o_dimensionalities = self.input("dimensionality_o",
                                              4,
                                              size=self.interconnect_input_ports)

        self._o_strides = []  # 2D
        self._o_ranges = []  # 2D
        #self._o_port_scheds = []  # Config as well
        self._o_dimensionalities = []

        # phases = [] TODO

        if self.banks == 1:
            self.address_width = clog2(mem_depth)
        else:
            self.address_width = clog2(mem_depth) + clog2(banks)

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Get the input ports from the interconnect
        self._data_in = self.input("i_data_in",
                                   self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)
        self._addr_in = self.input("i_addr_in",
                                   self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        self._valid_in = self.input("i_valid_in",
                                    self.interconnect_input_ports)

        self._data_out = self.output("o_data_out",
                                     self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)
        self._valid_out = self.output("o_valid_out",
                                      self.interconnect_output_ports,
                                      packed=True,
                                      explicit_array=True)


        ###########################
        ##### INPUT AGG SCHED #####
        ###########################
        self._agg_in_schedule = self.input("agg_in_sched",
                                       clog2(agg_height),
                                       size=self.max_agg_schedule,
                                       explicit_array=True,
                                       packed=True)
        self._agg_in_period = self.input("agg_in_period", clog2(self.max_agg_schedule))
        # ...and which order to output the blocks
        self._agg_out_schedule = self.input("agg_out_sched",
                                        clog2(agg_height),
                                        size=self.max_agg_schedule,
                                        explicit_array=True,
                                        packed=True)
        self._agg_out_period = self.input("agg_out_period", clog2(self.max_agg_schedule))

        ###########################################
        ##### AGGREGATION ALIGNERS (OPTIONAL) #####
        ###########################################
        # These variables are holders and can be swapped out if needed
        self._data_consume = self._data_in
        self._valid_consume = self._valid_in
        # Zero out if not aligning
        self._align_to_agg = const(0, self.interconnect_input_ports)
        # Add the aggregation buffer aligners 
        if(self.align_input):
            self._data_consume = self.var("data_consume",
                                   self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)
            self._valid_consume = self.var("valid_consume",
                                   self.interconnect_input_ports)
            self._align_to_agg = self.var("align_input",
                                     self.interconnect_input_ports)
            # Pass this to agg
            self._line_length = self.input("line_length",
                                           clog2(self.max_line_length),
                                           size=self.interconnect_input_ports,
                                           explicit_array=True,
                                           packed=True)
            # Make new aggregation aligners for each port
            for i in range(self.interconnect_input_ports):
                new_child = AggAligner(self.data_width, self.max_line_length)
                self.add_child(f"agg_align_{i}", new_child)
                self.wire(new_child.ports.in_dat, self._data_in[i])
                self.wire(new_child.ports.in_valid, self._valid_in[i])
                self.wire(new_child.ports.line_length, self._line_length[i])
                self.wire(self._align_to_agg[i], new_child.ports.align)
                self.wire(self._valid_consume[i], new_child.ports.out_valid)
                self.wire(self._data_consume[i], new_child.ports.out_dat)
        ################################################
        ##### END: AGGREGATION ALIGNERS (OPTIONAL) #####
        ################################################

        self._to_mem_dat = self._data_consume
        self._to_mem_valid = self._valid_consume


        ##################################
        ##### AGG BUFFERS (OPTIONAL) #####
        ##################################
        # Only instantiate agg_buffer if needed
        if(self.agg_height > 0):
            
            self._to_mem_dat = self.var("ab_to_mem_dat",
                                           self.mem_width,
                                           size=self.interconnect_input_ports,
                                           packed=True,
                                           explicit_array=True)

            self._to_mem_valid = self.var("ab_to_mem_valid",
                                             self.interconnect_input_ports)

            self._agg_buffers = []
            # Add input aggregations buffers
            for i in range(self.interconnect_input_ports):
                # add children aggregator buffers...
                agg_buffer_new = AggregationBuffer(self.agg_height,
                                                   self.data_width,
                                                   self.mem_width,
                                                   self.max_agg_schedule)
                self._agg_buffers.append(agg_buffer_new)
                self.add_child(f"agg_in_{i}",
                            agg_buffer_new)
                # Also add aggregation buffer config nodes...?
                self.wire(agg_buffer_new.ports.clk, self._clk)
                self.wire(agg_buffer_new.ports.rst_n, self._rst_n)
                # In
                self.wire(agg_buffer_new.ports.data_in, self._data_consume[i])
                self.wire(agg_buffer_new.ports.valid_in, self._valid_consume[i])
                self.wire(agg_buffer_new.ports.align, self._align_to_agg[i])
                self.wire(agg_buffer_new.ports.write_act, const(1, 1))  # From input addr control
                # Out
                # now wire it up
                self.wire(self._to_mem_dat[i], agg_buffer_new.ports.data_out)  # Demux these to SRAMs
                self.wire(self._to_mem_valid[i], agg_buffer_new.ports.valid_out)  # Demux these to SRAMs

                self.wire(agg_buffer_new.ports.in_sched, self._agg_in_schedule)
                self.wire(agg_buffer_new.ports.in_period, self._agg_in_period)
                self.wire(agg_buffer_new.ports.out_sched, self._agg_out_schedule)
                self.wire(agg_buffer_new.ports.out_period, self._agg_out_period)

        #######################################
        ##### END: AGG BUFFERS (OPTIONAL) #####
        #######################################

        ####################################
        ##### INPUT ADDRESS CONTROLLER #####
        ####################################
        # Connect these inputs ports to an address generator'
        iac = InputAddrCtrl(self.interconnect_input_ports,
                            self.mem_depth,
                            banks=self.banks,
                            iterator_support=self.input_iterator_support,
                            max_port_schedule=64,
                            address_width=self.address_width)
        self.add_child(f"input_addr_ctrl", iac)
        # Normal wires
        self.wire(iac.ports.clk, self._clk)
        self.wire(iac.ports.rst_n, self._rst_n)

        for i in range(self.interconnect_input_ports):
            # Send in each access iterator configuration
            self.wire(iac.ports[f"stride_p_{i}"], self._i_strides[i])
            self.wire(iac.ports[f"range_p_{i}"], self._i_ranges[i])
            self.wire(iac.ports[f"starting_addr_p_{i}"],
                      self._i_starting_addrs[i])
            self.wire(iac.ports[f"dimensionality_{i}"],
                      self._i_dimensionalities[i])
            self.wire(iac.ports.valid_in[i], self._to_mem_valid[i])

        # TODO: Hook these up
        # self.wire(self._wen_to_mem, iac.ports.wen_to_sram)
        # self.wire(self._addr_to_mem, iac.ports.addr_out)

        for i in range(self.banks):
            self.wire(iac.ports[f"port_sched_b_{i}"], self._i_port_scheds[i])
        self.wire(iac.ports.port_periods, self._i_port_periods)
        #########################################
        ##### END: INPUT ADDRESS CONTROLLER #####
        #########################################

        ##############################
        ##### READ/WRITE ARBITER #####
        ##############################
        # Hook up the read write arbiters for each bank
        self._arb_dat_out = self.var("arb_dat_out",
                                      self.mem_width,
                                      size=self.banks,
                                      explicit_array=True,
                                      packed=True)
        self._arb_port_out = self.var("arb_port_out",
                                      self.interconnect_output_ports,
                                      size=self.banks,
                                      explicit_array=True,
                                      packed=True)
        self._arb_valid_out = self.var("arb_port_out",
                                       self.banks)

        self._mem_data_out = self.var("mem_data_out",
                                      self.mem_width,
                                      size=self.banks,
                                      packed=True,
                                      explicit_array=True)

        self._mem_data_in = self.var("mem_data_in",
                                      self.mem_width,
                                      size=self.banks,
                                      packed=True,
                                      explicit_array=True)

        self._mem_cen_in = self.var("mem_cen_in", size=self.banks)
        self._mem_wen_in = self.var("mem_wen_in", size=self.banks)

        self.arbiters = []
        for i in range(self.banks):
            rw_arb = RWArbiter(fetch_width=self.mem_width,
                                   int_out_ports=self.interconnect_output_ports)
            self.arbiters.append(rw_arb)
            self.add_child("rw_arb_{i}", rw_arb)
            self.wire(rw_arb.ports.clk, self._clk)
            self.wire(rw_arb.ports.rst_n, self._rst_n)
            self.wire(rw_arb.ports.wen_in, const(0, 1))
            self.wire(rw_arb.ports.wen_en, const(0, 1))
            self.wire(rw_arb.ports.w_data, const(0, self.mem_width))
            self.wire(rw_arb.ports.data_from_mem, self._mem_data_out[i])
            self.wire(rw_arb.ports.ren_in, const(0, self.interconnect_output_ports))
            self.wire(rw_arb.ports.ren_en, const(0, 1))
            self.wire(rw_arb.ports.out_data, self._arb_dat_out[i])
            self.wire(rw_arb.ports.out_port, self._arb_port_out[i])
            self.wire(rw_arb.ports.out_valid, self._arb_valid_out[i])
            self.wire(self._mem_cen_in[i], rw_arb.ports.cen_mem)
            self.wire(self._mem_wen_in[i], rw_arb.ports.wen_mem)
            self.wire(self._mem_data_in[i], rw_arb.ports.data_to_mem)

        ####################################
        ##### DEMUX WRITE/SRAM WRAPPER #####
        ####################################

        # Wrap sram_stub
        for i in range(self.banks):
            mbank = SRAMStub(self.mem_width, self.mem_depth)
            self.add_child(f"mem_{i}", mbank)
            self.wire(mbank.ports.i_clk, self._clk)
            self.wire(mbank.ports.i_rst_n, self._rst_n)
            self.wire(mbank.ports.i_data, self._mem_data_in[i])  # Select based on input ctrl
            self.wire(mbank.ports.o_data,
                      self._mem_data_out[i])  # Gather these in local variable

            self.wire(mbank.ports.i_addr, 0)  # Demux
            self.wire(mbank.ports.i_cen, self._mem_cen_in[i])  # From rw_arbiter
            self.wire(mbank.ports.i_wen, self._mem_wen_in[i])  # From rw_arbiter
        #########################################
        ##### END: DEMUX WRITE/SRAM WRAPPER #####
        #########################################

        # Add output addr ctrl
        # Add transpose buffers at output

        ####################
        ##### ADD CODE #####
        ####################
        #self.add_code(self.zero_mem_data_in)
        self.add_code(self.zero_output)

    @always_comb
    def zero_mem_data_in(self):
        self._mem_data_in = 0

    @always_comb
    def zero_output(self):
        self._data_out  = 0
        self._valid_out = 0


if __name__ == "__main__":
    lake_dut = LakeTop()
    verilog(lake_dut, filename="lake_top.sv")


      #  for i in range(self.banks):
        #     self._i_port_scheds.append(self.input(f"i_port_sched_b_{i}",
        #                                           self.input_port_sched_width,
        #                                           size=self.input_max_port_sched,
        #                                           packed=True,
        #                                           explicit_array=True))
        # self._i_port_periods = self.input("i_port_periods",
        #                                   clog2(self.input_max_port_sched),
        #                                   size=self.banks)
