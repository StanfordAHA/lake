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
        super.__init__("aggregation_buffer")

        self.interconnect_input_ports = interconnect_input_ports
        self.mem_depth = mem_depth
        self.banks = banks
        self.iterator_support = iterator_support
        self.address_width = address_width
        self.max_port_schedule = max_port_schedule
        self.port_sched_width = clog2(self.interconnect_input_ports)
        self.mem_macro_addr_width = address_width["macro"]

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        
        # Inputs
        self._strides = [] # 2D
        self._ranges = [] # 2D
       # self._valid_scheds = [] #2D
        self._port_scheds = [] # Config as well

        #phases = [] TODO
        self._starting_addrs = [] # 1D
        for i in range(self.interconnect_input_ports):
            self._strides.append(self.input(f"stride_p_{i}", 16, size=self.iterator_support, packed=True, explicit_array=True))
            self._ranges.append(self.input(f"range_p_{i}", 32, size=self.iterator_support, packed=True, explicit_array=True))
        #    self._valid_scheds.append(self.input(f"valid_sched_p_{i}", 32, size=self.iterator_support, packed=True, explicit_array=True))
            self._starting_addrs.append(self.input(f"starting_addr_p_{i}"), 32)
            self._port_scheds.append(self.input(f"port_sched_p_{i}", self.port_sched_width, size=self.max_port_schedule, packed=True, explicit_array=True))

        self._valid_in = self.input("valid_in", 1, size=self.interconnect_input_ports, explicit_array=True)

        # Outputs
        self._wen = self.output("wen_to_sram", self.banks, size=self.interconnect_input_ports, explicit_array=True, packed=True)
        self._addresses = self.output("addr_out", self.address_width, size=self.banks, explicit_array=True, packed=True)
        self._port_sels = self.output("port_sel_out", self.port_sched_width, size=self.banks, explicit_array=True, packed=True)


        # LOCAL VARS
        self._local_addrs = self.var("local_addrs", self.address_width, size=self.instance_name, packed=True, explicit_array=True)
        self._port_sel_ptrs = self.var("port_sel_ptrs", clog2(self.max_port_schedule), size=self.banks, explicit_array=True, packed=True)


        # Now we should instantiate the child address generators (1 per input port) to send to the sram banks
        for i in range(self.interconnect_input_ports):
            self.add_child(f"address_gen_{i}", AddrGen(mem_depth=self.mem_depth, banks=self.banks, iterator_support=self.iterator_support))
            self.wire(self[f"address_gen_{i}"].ports._clk, self._clk)
            self.wire(self[f"address_gen_{i}"].ports._rst_n, self._rst_n)
            self.wire(self[f"address_gen_{i}"].ports._stride, self._strides[i])
            self.wire(self[f"address_gen_{i}"].ports._range, self._ranges[i])
            self.wire(self[f"address_gen_{i}"].ports._starting_addr, self._starting_addrs[i])
            # Get the address for each input port
            self.wire(self[f"address_gen_{i}"].ports._addr_out, self._local_addrs[i])


        # Then, obey the input schedule to send the proper Aggregator to the output




