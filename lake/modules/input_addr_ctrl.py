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
                 interconnect_input_ports=2,
                 mem_depth=32,
                 num_tiles=1,
                 banks=1,
                 iterator_support=6,
                 address_width=5,
                 data_width=16,
                 fetch_width=16,
                 multiwrite=1,
                 strg_wr_ports=2,
                 config_width=16):
        super().__init__("input_addr_ctrl", debug=True)

        assert multiwrite >= 1, "Multiwrite must be at least 1..."

        self.interconnect_input_ports = interconnect_input_ports
        self.mem_depth = mem_depth
        self.num_tiles = num_tiles
        self.banks = banks
        self.iterator_support = iterator_support
        self.address_width = address_width
        self.port_sched_width = max(1, clog2(self.interconnect_input_ports))
        self.data_width = data_width
        self.fetch_width = fetch_width
        self.fw_int = int(self.fetch_width / self.data_width)
        self.multiwrite = multiwrite
        self.strg_wr_ports = strg_wr_ports
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
        # phases = [] TODO
        # Take in the valid and data and attach an address + direct to a port
        self._valid_in = self.input("valid_in", self.interconnect_input_ports)
        self._wen_en = self.input("wen_en", self.interconnect_input_ports)

        self._wen_en_saved = self.var("wen_en_saved", self.interconnect_input_ports)

        self._data_in = self.input("data_in",
                                   self.data_width,
                                   size=(self.interconnect_input_ports,
                                         self.fw_int),
                                   explicit_array=True,
                                   packed=True)

        self._data_in_saved = self.var("data_in_saved",
                                       self.data_width,
                                       size=(self.interconnect_input_ports,
                                             self.fw_int),
                                       explicit_array=True,
                                       packed=True)

        # Outputs
        self._wen = self.output("wen_to_sram", self.strg_wr_ports,
                                size=self.banks,
                                explicit_array=True,
                                packed=True)

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

        self._wen_reduced_saved = self.var("wen_reduced_saved",
                                           self.banks,
                                           size=self.interconnect_input_ports,
                                           explicit_array=True,
                                           packed=True)

        self._addresses = self.output("addr_out",
                                      self.mem_addr_width,
                                      size=(self.banks,
                                            self.strg_wr_ports),
                                      explicit_array=True,
                                      packed=True)

        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=(self.banks,
                                           self.strg_wr_ports,
                                           self.fw_int),
                                     explicit_array=True,
                                     packed=True)

        self._port_out_exp = self.var("port_out_exp", self.interconnect_input_ports,
                                      size=self.banks,
                                      explicit_array=True,
                                      packed=True)

        self._port_out = self.output("port_out", self.interconnect_input_ports)

        self._counter = self.var("counter", self.port_sched_width)

        # Wire to port out
        for i in range(self.interconnect_input_ports):
            new_tmp = []
            for j in range(self.banks):
                new_tmp.append(self._port_out_exp[j][i])
            self.wire(self._port_out[i], kts.concat(*new_tmp).r_or())

        self._done = self.var("done", self.strg_wr_ports,
                              size=self.banks,
                              explicit_array=True,
                              packed=True)

        # LOCAL VARS
        self._local_addrs = self.var("local_addrs",
                                     self.address_width,
                                     size=(self.interconnect_input_ports,
                                           self.multiwrite),
                                     packed=True,
                                     explicit_array=True)

        self._local_addrs_saved = self.var("local_addrs_saved",
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
                self.wire(self._wen_reduced[i][j], kts.concat(*concat_ports).r_or())

        if self.banks == 1 and self.interconnect_input_ports == 1:
            self.wire(self._wen_full[0][0][0], self._valid_in)
        elif self.banks == 1 and self.interconnect_input_ports > 1:
            self.add_code(self.set_wen_single)
        else:
            self.add_code(self.set_wen_mult)

        # MAIN
        # Iterate through all banks to priority decode the wen
        self.add_code(self.decode_out_lowest)
        # Also set the write ports on the storage
        if self.strg_wr_ports > 1:
            self._idx_cnt = self.var("idx_cnt", 8,
                                     size=(self.banks,
                                           self.strg_wr_ports - 1),
                                     explicit_array=True,
                                     packed=True)
            for i in range(self.strg_wr_ports - 1):
                self.add_code(self.decode_out_alt, idx=i + 1)

        # Now we should instantiate the child address generators
        # (1 per input port) to send to the sram banks
        for i in range(self.interconnect_input_ports):
            self.add_child(f"address_gen_{i}", AddrGen(iterator_support=self.iterator_support,
                                                       config_width=self.config_width),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           clk_en=const(1, 1),
                           flush=const(0, 1),
                           step=self._valid_in[i])

        # Need to check that the address falls into the bank for implicit banking

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

        # to handle multiple input ports going to fewer SRAM write ports
        self.add_code(self.set_int_ports_counter)
        self.add_code(self.save_mult_int_signals)

    # Update the pointer and mux for input and output schedule
    # Now, obey the input schedule to send to the proper SRAM bank

    @always_comb
    def set_wen_mult(self):
        for i in range(self.interconnect_input_ports):
            for j in range(self.multiwrite):
                for k in range(self.banks):
                    self._wen_full[i][j][k] = 0
                    if (self._valid_in[i]):
                        if(self._local_addrs[i][j][self.mem_addr_width + self.bank_addr_width - 1,
                                                   self.mem_addr_width] == k):
                            self._wen_full[i][j][k] = 1

    @always_comb
    def set_wen_single(self):
        for i in range(self.interconnect_input_ports):
            for j in range(self.multiwrite):
                for k in range(self.banks):
                    self._wen_full[i][j][k] = 0
                    if (self._valid_in[i]):
                        self._wen_full[i][j][k] = 1

    @always_comb
    def decode_out_lowest(self):
        for i in range(self.banks):
            self._wen[i][0] = 0
            self._done[i][0] = 0
            self._port_out_exp[i] = 0
            self._data_out[i][0] = 0
            self._addresses[i][0] = 0
            if ~self._done[i][0]:
                # In comb block, directly write data, addr, wen_en from first interconnect input port
                # If this input port is active on this bank...
                if self._wen_reduced[0][i]:
                    self._done[i][0] = 1
                    # This should only go through if the wen_en is on...
                    self._wen[i][0] = self._wen_en[0]
                    self._port_out_exp[i][0] = 1
                    self._data_out[i][0] = self._data_in[0]
                    self._addresses[i][0] = self._local_addrs[0][0][self.mem_addr_width - 1, 0]
                # Check to write data from other interconnect input ports (other than first)
                # If this input port is active on this bank...
                elif self.interconnect_input_ports > 1:
                    if self._wen_reduced_saved[self._counter + 1][i]:
                        self._done[i][0] = 1
                        # This should only go through if the wen_en is on...
                        self._wen[i][0] = self._wen_en_saved[self._counter + 1]
                        self._port_out_exp[i][self._counter + 1] = 1
                        self._data_out[i][0] = self._data_in_saved[self._counter + 1]
                        self._addresses[i][0] = \
                            self._local_addrs_saved[self._counter + 1][0][self.mem_addr_width - 1, 0]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def save_mult_int_signals(self):
        if ~self._rst_n:
            self._wen_en_saved = 0
            self._wen_reduced_saved = 0
            self._data_in_saved = 0
            self._local_addrs_saved = 0
        # keep track of data from other interconnect input ports since we only are able
        # to write first interconnect input port at first
        elif self._wen_en.r_or():
            self._wen_en_saved = self._wen_en
            self._wen_reduced_saved = self._wen_reduced
            self._data_in_saved = self._data_in
            self._local_addrs_saved = self._local_addrs
        elif self.interconnect_input_ports == 1:
            self._wen_en_saved = 0
            self._wen_reduced_saved = 0
            self._data_in_saved = 0
            self._local_addrs_saved = 0
        # the data/wen from first interconnect input port is directly written, so
        # we do not have to resend that to SRAM
        elif self._counter == self.interconnect_input_ports - 2:
            self._wen_en_saved = 0
            self._wen_reduced_saved = 0
            self._data_in_saved = 0
            self._local_addrs_saved = 0

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_int_ports_counter(self):
        if ~self._rst_n:
            self._counter = 0
        elif self.interconnect_input_ports == 1:
            self._counter = 0
        # the data/wen from first interconnect input port is directly written, so
        # we do not have to resend that to SRAM
        elif self._counter == self.interconnect_input_ports - 2:
            self._counter = 0
        elif self._wen_en_saved.r_or():
            self._counter = self._counter + 1

    @always_comb
    def decode_out_alt(self, idx):
        for i in range(self.banks):
            self._wen[i][idx] = 0
            self._done[i][idx] = 0
            self._data_out[i][idx] = 0
            self._addresses[i][idx] = 0
            self._idx_cnt[i][idx - 1] = 0
            for j in range(self.interconnect_input_ports):
                if ~self._done[i][idx]:
                    if self._wen_reduced[j][i] & (self._idx_cnt[i][idx - 1] == idx):
                        # self._wen[i][idx] = 1
                        self._wen[i][idx] = self._wen_en[j]
                        self._done[i][idx] = 1
                        self._data_out[i][idx] = self._data_in[j]
                        self._addresses[i][idx] = self._local_addrs[j][0][self.mem_addr_width - 1, 0]
                    self._idx_cnt[i][idx - 1] = self._idx_cnt[i][idx - 1] + 1

    @always_comb
    def set_multiwrite_addrs(self):
        for i in range(self.interconnect_input_ports):
            self._local_addrs[i][0] = self[f"address_gen_{i}"].ports.addr_out[self.address_width - 1, 0]
        for i in range(self.multiwrite - 1):
            for j in range(self.interconnect_input_ports):
                self._local_addrs[j][i + 1] = self._local_addrs[j][0] + self._offsets_cfg[j][i]


if __name__ == "__main__":
    # iac_dut = InputAddrCtrl(interconnect_input_ports=2,
    #                        data_width=16,
    #                        mem_depth=512,
    #                        banks=4,
    #                        iterator_support=6,
    #                        max_port_schedule=64,
    #                        address_width=16,
    #                        multiwrite=2)
    iac_dut = InputAddrCtrl()
    verilog(iac_dut, filename="input_addr_ctrl.sv",
            additional_passes={"lift config regs": lift_config_reg})
