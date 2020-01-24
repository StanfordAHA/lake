from kratos import *
from lake.modules.aggregator import Aggregator
from lake.modules.addr_gen import AddrGen


class OutputAddrCtrl(Generator):
    '''
    Input addressing control from the aggregation buffers to the SRAM
    '''
    def __init__(self,
                 interconnect_output_ports,
                 mem_depth,
                 banks,
                 iterator_support,
                 max_port_schedule,
                 address_width):
        super().__init__("output_addr_ctrl", debug=True)

        self.interconnect_output_ports = interconnect_output_ports
        self.mem_depth = mem_depth
        self.banks = banks
        self.iterator_support = iterator_support
        self.address_width = address_width
        self.max_port_schedule = max_port_schedule
        self.port_sched_width = clog2(self.interconnect_output_ports)

        self.mem_addr_width = clog2(self.mem_depth)
        if self.banks > 1:
            self.bank_addr_width = clog2(self.banks)
        else:
            self.bank_addr_width = 0
        self.address_width = self.mem_addr_width + self.bank_addr_width

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        self._strides = self.input("strides",
                                   32,
                                   size=(self.interconnect_output_ports,
                                         self.iterator_support),
                                   packed=True,
                                   explicit_array=True)  # 2D
        self._ranges = self.input("ranges",
                                  32,
                                  size=(self.interconnect_output_ports,
                                        self.iterator_support),
                                  packed=True,
                                  explicit_array=True)
        self._dimensionalities = self.input("dimensionalities",
                                            4,
                                            size=self.interconnect_output_ports,
                                            explicit_array=True,
                                            packed=True)
        self._starting_addrs = self.input("starting_addrs",
                                          32,
                                          size=self.interconnect_output_ports,
                                          explicit_array=True,
                                          packed=True)
        # phases = [] TODO

        # Take in the valid and attach an address + direct to a port
        self._valid_in = self.input("valid_in", self.interconnect_output_ports)

        # Outputs
        self._ren = self.output("ren",
                                self.interconnect_output_ports,
                                size=self.banks,
                                explicit_array=True,
                                packed=True)
        self._addresses = self.output("addr_out",
                                      self.mem_addr_width,
                                      size=self.interconnect_output_ports,
                                      explicit_array=True,
                                      packed=True)

        # LOCAL VARS
        self._local_addrs = self.var("local_addrs",
                                     self.address_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        if self.banks == 1 and self.interconnect_output_ports == 1:
            # self._port_select = self.var("port_select", self.banks)
            # self.wire(self._port_select, const(0, self.banks))
            self.wire(self._ren, self._valid_in)
        elif self.banks == 1 and self.interconnect_output_ports > 1:
            # self._port_select = self.var("port_select",
            #                     clog2(self.interconnect_output_ports),
            #                     size=self.banks,
            #                     explicit_array=True,
            #                     packed=True)
            self.add_code(self.set_wen_single)
            # self.add_code(self.set_port_select_single)
        elif self.banks > 1 and self.interconnect_output_ports == 1:
            # self._port_select = self.var("port_select", self.banks)
            # self.wire(self._port_select, const(0, self.banks))
            self.add_code(self.set_ren_mult)
        else:
            # self._port_select = self.var("port_select",
            #                              clog2(self.interconnect_output_ports),
            #                              size=self.banks,
            #                              explicit_array=True,
            #                              packed=True)
            self.add_code(self.set_ren_mult)
            # self.add_code(self.set_port_select_mult)

        # MAIN

        self.add_code(self.set_out)

        # Now we should instantiate the child address generators
        # (1 per input port) to send to the sram banks
        for i in range(self.interconnect_output_ports):
            new_addr_gen = AddrGen(mem_depth=self.mem_depth,
                                   iterator_support=self.iterator_support,
                                   address_width=self.address_width)
            self.add_child(f"address_gen_{i}", new_addr_gen)
            self.wire(new_addr_gen.ports.clk, self._clk)
            self.wire(new_addr_gen.ports.rst_n, self._rst_n)
            self.wire(new_addr_gen.ports.strides, self._strides[i])
            self.wire(new_addr_gen.ports.ranges, self._ranges[i])
            self.wire(new_addr_gen.ports.starting_addr, self._starting_addrs[i])
            self.wire(new_addr_gen.ports.dimensionality, self._dimensionalities[i])
            self.wire(new_addr_gen.ports.clk_en, const(1, 1))
            self.wire(new_addr_gen.ports.flush, const(0, 1))
            self.add_stmt(new_addr_gen.ports.step.assign(self._valid_in[i]))

            # Get the address for each input port
            self.wire(self._local_addrs[i],
                      new_addr_gen.ports.addr_out[self.address_width - 1, 0])

    @always_comb
    def port_sel_comb(self):
        for i in range(self._port_sels.size):
            self._port_sels[i] = self._port_scheds[i][self._port_sel_ptrs[i]]

    @always_comb
    def set_ren_mult(self):
        self._ren = 0
        for i in range(self.interconnect_output_ports):
            if(self._valid_in[i]):
                self._ren[self._local_addrs[i][self.mem_addr_width + self.bank_addr_width - 1,
                                               self.mem_addr_width]][i] = 1

    @always_comb
    def set_ren_single(self):
        self._ren = 0
        for i in range(self.interconnect_output_ports):
            if(self._valid_in[i]):
                self._ren[i] = 1

    @always_comb
    def set_port_select_single(self):
        self._port_select = 0
        for i in range(self.interconnect_output_ports):
            if(self._valid_in[i]):
                self._port_select = i

    @always_comb
    def set_port_select_mult(self):
        self._port_select = 0
        for i in range(self.interconnect_output_ports):
            if(self._valid_in[i]):
                self._port_select[
                    self._local_addrs[i][self.mem_addr_width + self.bank_addr_width - 1,
                                         self.mem_addr_width]] = const(i,
                                                                       self._port_select[0].width)

    @always_comb
    def set_out(self):
        self._addresses = 0
        for i in range(self.interconnect_output_ports):
            self._addresses[i] = self._local_addrs[i][self.mem_addr_width - 1, 0]


if __name__ == "__main__":
    db_dut = OutputAddrCtrl(interconnect_output_ports=2,
                            mem_depth=512,
                            banks=2,
                            iterator_support=6,
                            max_port_schedule=64,
                            address_width=16)
    verilog(db_dut, filename="output_addr_ctrl.sv", check_multiple_driver=False)
