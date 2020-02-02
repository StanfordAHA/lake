from kratos import *
from lake.modules.aggregator import Aggregator
from lake.modules.addr_gen import AddrGen
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
import kratos as kts


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
                 address_width,
                 data_width,
                 multiwrite):
        super().__init__("input_addr_ctrl", debug=True)

        self.interconnect_input_ports = interconnect_input_ports
        self.mem_depth = mem_depth
        self.banks = banks
        self.iterator_support = iterator_support
        self.address_width = address_width
        self.max_port_schedule = max_port_schedule
        self.port_sched_width = max(1, clog2(self.interconnect_input_ports))
        self.data_width = data_width
        self.multiwrite = multiwrite

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
        # phases = [] TODO
        # Take in the valid and data and attach an address + direct to a port
        self._valid_in = self.input("valid_in", self.interconnect_input_ports)
        self._data_in = self.input("data_in",
                                   self.data_width,
                                   size=self.interconnect_input_ports,
                                   explicit_array=True,
                                   packed=True)

        # Outputs
        self._wen = self.output("wen_to_sram",
                                self.banks)

        wen_full_size = (self.interconnect_input_ports,
                         self.multiwrite)
        self._wen_full = self.var("wen_full",
                                  self.banks,
                                  size=wen_full_size,
                                  explicit_array=True,
                                  packed=True)

        self._wen_reduced = self.var("wen_reduced",
                                     self.banks,
                                     size=self.interconnect_input_ports,
                                     explicit_array=True,
                                     packed=True)

        self._addresses = self.output("addr_out",
                                      self.mem_addr_width,
                                      size=self.banks,
                                      explicit_array=True,
                                      packed=True)

        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=self.banks,
                                     explicit_array=True,
                                     packed=True)

        self._done = self.var("done", self.banks)

        # LOCAL VARS
        self._local_addrs = self.var("local_addrs",
                                     self.address_width,
                                     size=(self.interconnect_input_ports,
                                           self.multiwrite),
                                     packed=True,
                                     explicit_array=True)

        for i in range(self.interconnect_input_ports):
            for j in range(self.banks):
                concat_ports = []
                for k in range(self.multiwrite):
                    concat_ports.append(self._wen_full[i][k][j])
                # if(self.multiwrite == 1):
                #     self.wire(self._wen_reduced[i][j], *concat_ports)
                # else:
                self.wire(self._wen_reduced[i][j], kts.concat(*concat_ports).r_or())

        for i in range(self.banks):
            cat = []
            for j in range(self.interconnect_input_ports):
                cat.append(self._wen_reduced[j][i])
            if(len(cat) > 1):
                self.wire(self._wen[i], kts.concat(*cat).r_or())
            else:
                self.wire(self._wen[i], cat[0])

        if self.banks == 1 and self.interconnect_input_ports == 1:
            self.wire(self._wen, self._valid_in)
        elif self.banks == 1 and self.interconnect_input_ports > 1:
            self.add_code(self.set_wen_single)
        else:
            self.add_code(self.set_wen_mult)

        # MAIN
        # Iterate through all banks to priority decode the wen
        self.add_code(self.decode_out)

        # Now we should instantiate the child address generators
        # (1 per input port) to send to the sram banks
        for i in range(self.interconnect_input_ports):
            self.add_child(f"address_gen_{i}", AddrGen(mem_depth=self.mem_depth,
                                                       iterator_support=self.iterator_support,
                                                       address_width=self.address_width),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           clk_en=const(1, 1),
                           flush=const(0, 1),
                           step=self._valid_in[i])

        # Need to check that the add ress falls into the bank for implicit banking

        # Then, obey the input schedule to send the proper Aggregator to the output
        # The wen to sram should be that the valid for the selected port is high
        # Do the same thing for the output address
        assert self.multiwrite <= self.banks and self.multiwrite > 0,\
            "Multiwrite should be between 1 and banks"
        if self.multiwrite > 1:
            size = (self.interconnect_input_ports, self.multiwrite - 1)
            self._offsets_cfg = self.input("offsets_cfg",
                                           self.address_width,
                                           size=size,
                                           packed=True,
                                           explicit_array=True)
            doc = "These offsets provide the ability to write to multiple banks explicitly"
            self._offsets_cfg.add_attribute(ConfigRegAttr(doc))
        self.add_code(self.set_multiwrite_addrs)

    # Update the pointer and mux for input and output schedule
    # Now, obey the input schedule to send to the proper SRAM bank

    @always_comb
    def set_wen_mult(self):
        for i in range(self.interconnect_input_ports):
            for j in range(self.multiwrite):
                for k in range(self.banks):
                    self._wen_full[i][j][k] = 0
                    if(self._valid_in[i]):
                        if(self._local_addrs[i][j][self.mem_addr_width + self.bank_addr_width - 1,
                                                   self.mem_addr_width] == k):
                            self._wen_full[i][j][k] = 1

    @always_comb
    def set_wen_single(self):
        for i in range(self.interconnect_input_ports):
            for j in range(self.multiwrite):
                for k in range(self.banks):
                    self._wen_full[i][j][k] = 0
                    if(self.valid_in[i]):
                        self._wen_full[i][j][k] = 1

    @always_comb
    def decode_out(self):
        for i in range(self.banks):
            self._done[i] = 0
            self._data_out[i] = 0
            self._addresses[i] = 0
            for j in range(self.interconnect_input_ports):
                if ~self._done[i]:
                    if self._wen_reduced[j][i]:
                        self._done[i] = 1
                        self._data_out[i] = self._data_in[j]
                        self._addresses[i] = self._local_addrs[j][0][self.mem_addr_width - 1, 0]

    @always_comb
    def set_multiwrite_addrs(self):
        for i in range(self.interconnect_input_ports):
            self._local_addrs[i][0] = self[f"address_gen_{i}"].ports.addr_out[self.address_width - 1, 0]
        for i in range(self.multiwrite - 1):
            for j in range(self.interconnect_input_ports):
                self._local_addrs[j][i + 1] = self._local_addrs[j][0] + self._offsets_cfg[j][i]


if __name__ == "__main__":
    db_dut = InputAddrCtrl(interconnect_input_ports=2,
                           data_width=16,
                           mem_depth=512,
                           banks=4,
                           iterator_support=6,
                           max_port_schedule=64,
                           address_width=16,
                           multiwrite=2)
    verilog(db_dut, filename="input_addr_ctrl.sv",
            additional_passes={"lift config regs": lift_config_reg})
