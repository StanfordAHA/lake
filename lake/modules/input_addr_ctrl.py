from kratos import *
from lake.modules.aggregator import Aggregator
from lake.modules.addr_gen import AddrGen

class InputAddrCtrl(Generator):
    '''
    Input addressing control from the aggregation buffers to the SRAM
    '''
    def __init__(self,
               interconnect_input_ports,
               mem_depth,
               banks,
               iterator_support,
               max_port_schedule,
               address_width
              ):
        super().__init__("input_addr_ctrl", debug=True)

        self.interconnect_input_ports = interconnect_input_ports
        self.mem_depth = mem_depth
        self.banks = banks
        self.iterator_support = iterator_support
        self.address_width = address_width
        self.max_port_schedule = max_port_schedule
        self.port_sched_width = clog2(self.interconnect_input_ports)
        #self.mem_macro_addr_width = address_width["macro"]

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        
        # Inputs
        self._strides = [] # 2D
        self._ranges = [] # 2D
        # self._valid_scheds = [] #2D
        self._port_scheds = [] # Config as well
        self._dimensionalities = []

        #phases = [] TODO
        self._starting_addrs = [] # 1D
        for i in range(self.interconnect_input_ports):
            self._strides.append(self.input(f"stride_p_{i}", 32, size=self.iterator_support, packed=True, explicit_array=True))
            self._ranges.append(self.input(f"range_p_{i}", 32, size=self.iterator_support, packed=True, explicit_array=True))
        #    self._valid_scheds.append(self.input(f"valid_sched_p_{i}", 32, size=self.iterator_support, packed=True, explicit_array=True))
            self._starting_addrs.append(self.input(f"starting_addr_p_{i}", 32))
            self._dimensionalities.append(self.input(f"dimensionality_{i}", 4))

        for i in range(self.banks):
            self._port_scheds.append(self.input(f"port_sched_b_{i}", self.port_sched_width, size=self.max_port_schedule, packed=True, explicit_array=True))
        self._port_periods = self.input("port_periods", clog2(self.max_port_schedule), size=self.banks)
        self._valid_in = self.input("valid_in", 1, size=self.interconnect_input_ports, explicit_array=True, packed=True)

        # Outputs
        self._wen = self.output("wen_to_sram", self.banks)
        self._addresses = self.output("addr_out", self.address_width, size=self.banks, explicit_array=True, packed=True)
        self._port_sels = self.output("port_sel_out", self.port_sched_width, size=self.banks, explicit_array=True, packed=True)

        # LOCAL VARS
        self._local_addrs = self.var("local_addrs", self.address_width, size=self.interconnect_input_ports, packed=True, explicit_array=True)
        self._port_sel_ptrs = self.var("port_sel_ptrs", clog2(self.max_port_schedule), size=self.banks, explicit_array=True, packed=True)

        ### MAIN
        for i in range(self.banks):
            self.wire(self._wen[i], self._valid_in[self._port_sels[i]])

        # Now we should instantiate the child address generators (1 per input port) to send to the sram banks
        for i in range(self.interconnect_input_ports):
            self.add_child(f"address_gen_{i}", AddrGen(mem_depth=self.mem_depth, iterator_support=self.iterator_support, address_width=self.address_width))
            self.wire(self[f"address_gen_{i}"].ports.clk, self._clk)
            self.wire(self[f"address_gen_{i}"].ports.rst_n, self._rst_n)
            self.wire(self[f"address_gen_{i}"].ports.strides, self._strides[i])
            self.wire(self[f"address_gen_{i}"].ports.ranges, self._ranges[i])
            self.wire(self[f"address_gen_{i}"].ports.starting_addr, self._starting_addrs[i])
            self.wire(self[f"address_gen_{i}"].ports.dimensionality, self._dimensionalities[i])
            self.add_stmt(self[f"address_gen_{i}"].ports.step.assign(self._wen[i]))
            #self.wire(self[f"address_gen_{i}"].ports.step, self._wen[i])
            # Get the address for each input port
            self.wire(self._local_addrs[i], self[f"address_gen_{i}"].ports.addr_out[self.address_width-1, 0])

        # Need to check that the address falls into the bank for implicit banking

        # Then, obey the input schedule to send the proper Aggregator to the output
        # The wen to sram should be that the valid for the selected port is high
        # Do the same thing for the output address
        for i in range(self.banks):
            self.wire(self._addresses[i], self._local_addrs[self._port_sels[i]])

        # Add in the code
        self.add_code(self.update_sched_ptr)
        self.add_code(self.port_sel_comb)

    # Update the pointer and mux for input and output schedule
    # Now, obey the input schedule to send to the proper SRAM bank
    @always((posedge, "clk"), (negedge, "rst_n"))
    def update_sched_ptr(self):
        for i in range(self._port_sels.size):
            if ~self._rst_n:
                self._port_sel_ptrs[i] = const(0, self._port_sel_ptrs[i].width)
            elif self._wen[i]:
                self._port_sel_ptrs[i] = \
                ternary(self._port_sel_ptrs[i] == self._port_periods[i], const(0, self._port_sel_ptrs[i].width), self._port_sel_ptrs[i] + const(1, self._port_sel_ptrs[i].width))

    def port_sel_comb(self):
        for i in range(self._port_sels.size):
            self._port_sels[i] = self._port_scheds[i][self._port_sel_ptrs[i]]


if __name__ == "__main__":
    db_dut = InputAddrCtrl(
               interconnect_input_ports=2,
               mem_depth=512,
               banks=4,
               iterator_support=6,
               max_port_schedule=64,
               address_width=16
        )

    verilog(db_dut, filename="input_addr_ctrl.sv", check_active_high=False)


