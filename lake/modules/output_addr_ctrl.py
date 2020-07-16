from kratos import *
from lake.modules.aggregator import Aggregator
from lake.modules.addr_gen import AddrGen
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg


class OutputAddrCtrl(Generator):
    '''
    Input addressing control from the aggregation buffers to the SRAM
    '''
    def __init__(self,
                 interconnect_output_ports,
                 mem_depth,
                 num_tiles,
                 banks,
                 iterator_support,
                 address_width,
                 config_width=16):
        super().__init__("output_addr_ctrl")

        self.interconnect_output_ports = interconnect_output_ports
        self.mem_depth = mem_depth
        self.num_tiles = num_tiles
        self.banks = banks
        self.iterator_support = iterator_support
        self.address_width = address_width
        self.port_sched_width = clog2(self.interconnect_output_ports)
        self.config_width = config_width

        self.mem_addr_width = clog2(self.num_tiles * self.mem_depth)

        if self.banks > 1:
            self.bank_addr_width = clog2(self.banks)
        else:
            self.bank_addr_width = 0
        self.address_width = self.mem_addr_width + self.bank_addr_width

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        # Take in the valid and attach an address + direct to a port
        self._valid_in = self.input("valid_in", self.interconnect_output_ports)
        self._step_in = self.input("step_in", self.interconnect_output_ports)

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
            self.wire(self._ren[0][0], self._valid_in)
        elif self.banks == 1 and self.interconnect_output_ports > 1:
            self.add_code(self.set_ren_single)
        elif self.banks > 1 and self.interconnect_output_ports == 1:
            self.add_code(self.set_ren_mult)
        else:
            self.add_code(self.set_ren_mult)

        # MAIN

        self.add_code(self.set_out)

        # Now we should instantiate the child address generators
        # (1 per input port) to send to the sram banks
        for i in range(self.interconnect_output_ports):
            new_addr_gen = AddrGen(iterator_support=self.iterator_support,
                                   # address_width=self.address_width,
                                   config_width=self.config_width)

            self.add_child(f"address_gen_{i}", new_addr_gen,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           clk_en=const(1, 1),
                           flush=const(0, 1))
            # self.add_stmt(new_addr_gen.ports.step.assign(self._valid_in[i]))
            self.add_stmt(new_addr_gen.ports.step.assign(self._step_in[i] & self._valid_in[i]))

            # Get the address for each input port
            self.wire(self._local_addrs[i],
                      new_addr_gen.ports.addr_out[self.address_width - 1, 0])

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
                self._ren[0][i] = 1

    @always_comb
    def set_out(self):
        self._addresses = 0
        for i in range(self.interconnect_output_ports):
            self._addresses[i] = self._local_addrs[i][self.mem_addr_width - 1, 0]


if __name__ == "__main__":
    # lifting ports up

    db_dut = OutputAddrCtrl(interconnect_output_ports=2,
                            mem_depth=512,
                            num_tiles=2,
                            banks=2,
                            iterator_support=6,
                            address_width=16)
    verilog(db_dut, filename="output_addr_ctrl.sv",
            additional_passes={"lift config regs": lift_config_reg})
