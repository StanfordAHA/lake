from lake.modules.chain_accessor import ChainAccessor
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_interface import MemoryPort, MemoryPortType
from lake.top.memory_controller import MemoryController
from lake.utils.parse_clkwork_config import configure_rv_controller, extract_rv_controller_json, map_rv_controller
from kratos import *
from lake.modules.passthru import *
from lake.attributes.formal_attr import *
from lake.passes.passes import lift_config_reg
from lake.modules.rv_write_ctrl import RVWriteCtrl
from lake.modules.rv_read_ctrl import RVReadCtrl
from lake.modules.rv_arbiter import RVArbiter
from lake.utils.util import add_counter, decode, trim_config_list, observe_cfg
from _kratos import create_wrapper_flatten
from lake.attributes.control_signal_attr import ControlSignalAttr
import os


class StrgUBRV(MemoryController):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 iterator_support=6,
                 id_config_width=11,
                 config_width=16,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=1,
                 tb_height=1
                 ):

        super().__init__("strg_ub_rv")

        ##################################################################################
        # Capture constructor parameter...
        ##################################################################################
        self.ctrl_in_name = "rv_write_ctrl"
        self.ctrl_out_name = "rv_read_ctrl"
        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.mem_addr_width = clog2(self.mem_depth)
        self.fetch_width = mem_width // data_width
        self.iterator_support = iterator_support
        self.id_config_width = id_config_width
        self.config_width = config_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.read_delay = read_delay
        self.rw_same_cycle = rw_same_cycle
        self.agg_height = agg_height
        self.tb_height = tb_height
        self.banks = banks

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._valid_in = []
        self._ready_out = []
        self._data_in = []
        for i in range(self.interconnect_input_ports):
            tmp_valid_in = self.input(f"data_in_{i}_valid", 1)
            tmp_ready_out = self.output(f"data_in_{i}_ready", 1)
            tmp_data_in = self.input(f"data_in_{i}", self.data_width + 1)
            tmp_valid_in.add_attribute(ControlSignalAttr(is_control=True))
            tmp_ready_out.add_attribute(ControlSignalAttr(is_control=False))
            tmp_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            self._valid_in.append(tmp_valid_in)
            self._ready_out.append(tmp_ready_out)
            self._data_in.append(tmp_data_in)

        self._valid_out = []
        self._ready_in = []
        self._data_out = []
        for i in range(self.interconnect_output_ports):
            tmp_valid_out = self.output(f"data_out_{i}_valid", 1)
            tmp_ready_in = self.input(f"data_out_{i}_ready", 1)
            tmp_data_out = self.output(f"data_out_{i}", self.data_width + 1)
            tmp_valid_out.add_attribute(ControlSignalAttr(is_control=False))
            tmp_ready_in.add_attribute(ControlSignalAttr(is_control=True))
            tmp_data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            self._valid_out.append(tmp_valid_out)
            self._ready_in.append(tmp_ready_in)
            self._data_out.append(tmp_data_out)

        # memory
        self._data_from_sram = self.input("data_from_strg", self.data_width,
                                          size=self.fetch_width,
                                          packed=True)

        # The interface is slightly different when dealing with a dual port SRAM
        # This could be abstracted away, but for now it's slightly easier to handle this
        if self.rw_same_cycle:
            self._ren_to_sram = self.output("ren_to_strg", 1, packed=True)
            self._wr_addr_to_sram = self.output("wr_addr_out", clog2(self.mem_depth), packed=True)
            self._rd_addr_to_sram = self.output("rd_addr_out", clog2(self.mem_depth), packed=True)
        else:
            self._ren_to_sram = self.output("ren_to_strg", 1, packed=True)
            self._addr_to_sram = self.output("addr_out", clog2(self.mem_depth), packed=True)

        self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
        self._data_to_sram = self.output("data_to_strg", self.data_width,
                                         size=self.fetch_width,
                                         packed=True)

        # chaining
        # self._valid_out = self.output("accessor_output", self.interconnect_output_ports)

        ##################################################################################
        # Internal Registers and Wires
        ##################################################################################
        self._data_in_thin = self.var("data_in_thin", self.data_width,
                                      size=self.interconnect_input_ports,
                                      packed=True,
                                      explicit_array=True)

        for idx in range(self.interconnect_input_ports):
            self.wire(self._data_in_thin[idx], self._data_in[idx][self.data_width - 1, 0])

        self._data_out_thin = self.var("data_out_int_thin", self.data_width,
                                       size=self.interconnect_output_ports,
                                       packed=True,
                                       explicit_array=True)

        for idx in range(self.interconnect_output_ports):
            self.wire(self._data_out[idx][self.data_width - 1, 0], self._data_out_thin[idx])
            self.wire(self._data_out[idx][self.data_width], const(0, 1))

        # write ctrl
        # write -> read dependency
        self._wctrl_token_avail_o = self.var("wctrl_token_avail_o", 2,
                                             size=self.interconnect_input_ports,
                                             packed=True,
                                             explicit_array=True)
        self._wctrl_reset_window_addr_o = self.var("wctrl_reset_window_addr_o", 2,
                                                   size=self.interconnect_input_ports,
                                                   packed=True,
                                                   explicit_array=True)
        self._wctrl_window_addr_o = self.var("wctrl_window_addr_o", self.mem_addr_width,
                                             size=self.interconnect_input_ports,
                                             packed=True,
                                             explicit_array=True)
        self._wctrl_token_dec_i = self.var("wctrl_token_dec_i", 2,
                                           size=self.interconnect_input_ports,
                                           packed=True,
                                           explicit_array=True)
        # read -> write dependency
        self._wctrl_token_avail_i = self.var("wctrl_token_avail_i", self.interconnect_input_ports)
        self._wctrl_reset_window_addr_i = self.var("wctrl_reset_window_addr_i",
                                                   self.interconnect_input_ports)
        self._wctrl_window_addr_i = self.var("wctrl_window_addr_i", self.mem_addr_width,
                                             size=self.interconnect_input_ports,
                                             packed=True,
                                             explicit_array=True)
        self._wctrl_token_dec_o = self.var("wctrl_token_dec_o", self.interconnect_input_ports)

        # read ctrl
        # write -> read dependency
        self._rctrl_token_avail_i = self.var("rctrl_token_avail_i", self.interconnect_input_ports)
        self._rctrl_reset_window_addr_i = self.var("rctrl_reset_window_addr_i",
                                                   self.interconnect_input_ports)
        self._rctrl_window_addr_i = self.var("rctrl_window_addr_i", self.mem_addr_width,
                                             size=self.interconnect_input_ports,
                                             packed=True,
                                             explicit_array=True)
        self._rctrl_token_dec_o = self.var("rctrl_token_dec_o", self.interconnect_input_ports)
        # read -> write dependency
        self._rctrl_token_avail_o = self.var("rctrl_token_avail_o", self.interconnect_input_ports)
        self._rctrl_reset_window_addr_o = self.var("rctrl_reset_window_addr_o",
                                                   self.interconnect_input_ports)
        self._rctrl_window_addr_o = self.var("rctrl_window_addr_o", self.mem_addr_width,
                                             size=self.interconnect_input_ports,
                                             packed=True,
                                             explicit_array=True)
        self._rctrl_token_dec_i = self.var("rctrl_token_dec_i", self.interconnect_input_ports)

        # write requests
        self._wctrl_wen = self.var("wctrl_wen", self.interconnect_input_ports)
        self._wctrl_waddr = self.var("wctrl_waddr", self.mem_addr_width,
                                     size=self.interconnect_input_ports,
                                     packed=True,
                                     explicit_array=True)
        self._wctrl_wdata = self.var("wctrl_wdata", self.data_width,
                                     size=(self.interconnect_input_ports, self.fetch_width),
                                     packed=True,
                                     explicit_array=True)

        # read requests
        self._rctrl_ren = self.var("rctrl_ren", self.interconnect_output_ports)
        self._rctrl_raddr = self.var("rctrl_raddr", self.mem_addr_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        # arbiter
        self._is_granted = self.var("is_granted", 4)

        # # chaining
        # self._valid_out_int = self.var("accessor_output_int", self.interconnect_output_ports)
        # self._data_out_int = self.var("data_out_int", self.data_width,
        #                               size=self.interconnect_output_ports,
        #                               packed=True,
        #                               explicit_array=True)

        ##################################################################################
        # Config Registers
        ##################################################################################
        # dual_token == True
        self._token_gen_0 = self.input(f"rv_write_ctrl_{0}_token_gen", self.config_width)
        self._token_gen_0.add_attribute(ConfigRegAttr("The number of tokens to generate before switching"))
        self._token_gen_0.add_attribute(FormalAttr(f"{self._token_gen_0.name}", FormalSignalConstraint.SOLVE))

        self._token_gen_1 = self.input(f"rv_write_ctrl_{1}_token_gen", self.config_width)
        self._token_gen_1.add_attribute(ConfigRegAttr("The number of tokens to generate before switching"))
        self._token_gen_1.add_attribute(FormalAttr(f"{self._token_gen_1.name}", FormalSignalConstraint.SOLVE))

        self._rctrl_0_dual_dep = self.input(f"rv_read_ctrl_{0}_dual_dep", 1)
        self._rctrl_0_dual_dep.add_attribute(ConfigRegAttr("Makes rctrl_0 depend on both wctrl_0 and wctrl_1"))
        self._rctrl_0_dual_dep.add_attribute(FormalAttr(f"{self._rctrl_0_dual_dep.name}", FormalSignalConstraint.SOLVE))
        ##################################################################################
        # Controllers
        ##################################################################################
        self.write_ctrls = []
        for i in range(self.interconnect_input_ports):
            # heterogeneous write ctrl
            write_ctrl = RVWriteCtrl(data_width=self.data_width,
                                     mem_width=self.mem_width,
                                     mem_depth=self.mem_depth,
                                     iterator_support=self.iterator_support,
                                     config_width=self.config_width,
                                     id_config_width=self.id_config_width,
                                     dependent=(i == 1),
                                     dual_token=(i == 1))

            self.write_ctrls.append(write_ctrl)

            self.add_child(f"{self.ctrl_in_name}_{i}", write_ctrl,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           valid_i=self._valid_in[i],
                           din_i=self._data_in_thin[i],
                           token_dec_i=self._wctrl_token_dec_i[i][0],
                           is_granted_i=self._is_granted[i],
                           ready_o=self._ready_out[i],
                           token_avail_o=self._wctrl_token_avail_o[i][0],
                           reset_window_addr_o=self._wctrl_reset_window_addr_o[i][0],
                           window_addr_o=self._wctrl_window_addr_o[i],
                           wen_o=self._wctrl_wen[i],
                           waddr_o=self._wctrl_waddr[i],
                           wdata_o=self._wctrl_wdata[i]
                           )

            if i == 1:
                # dependent == True
                self.wire(write_ctrl.ports.token_avail_i, self._wctrl_token_avail_i[i])
                self.wire(write_ctrl.ports.reset_window_addr_i, self._wctrl_reset_window_addr_i[i])
                self.wire(write_ctrl.ports.window_addr_i, self._wctrl_window_addr_i[i])
                self.wire(write_ctrl.ports.token_dec_o, self._wctrl_token_dec_o[i])

                # dual_token == True
                self.wire(write_ctrl.ports.token_dec2_i, self._wctrl_token_dec_i[i][1])
                self.wire(write_ctrl.ports.token_avail2_o, self._wctrl_token_avail_o[i][1])
                self.wire(write_ctrl.ports.reset_window_addr2_o, self._wctrl_reset_window_addr_o[i][1])

        # TODO pack the dependency network into a module
        self._token_gen_count = self.var(f"token_gen_count", self.config_width)
        self._token_gen_state = self.var(f"token_gen_state", 1)

        self._wctrl_1_dep = self.var("wctrl_1_dep_observed", 1)
        observe_cfg(self, f"wctrl_1_dep_observed", self.write_ctrls[1], "dep")

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def token_gen_state_ctrl():
            if ~self._rst_n:
                self._token_gen_count = 0
                self._token_gen_state = 0
            elif self._rctrl_token_dec_o[0]:
                self._token_gen_count = self._token_gen_count + 1

                if self._token_gen_state == 0:
                    if self._token_gen_count == self._token_gen_0:
                        self._token_gen_state = 1
                        self._token_gen_count = 0
                elif self._token_gen_state == 1:
                    if self._token_gen_count == self._token_gen_1:
                        self._token_gen_state = 0
                        self._token_gen_count = 0

        # dual_token == True
        self.add_code(token_gen_state_ctrl)

        # write -> read dependency
        @always_comb
        def write_to_read_dep():
            # rctrl_0 is dependent on wctrl_0 and wctrl_1's token_1
            if self._rctrl_0_dual_dep:
                self._rctrl_token_avail_i[0] = ternary(self._token_gen_state,
                                                       self._wctrl_token_avail_o[1][1],
                                                       self._wctrl_token_avail_o[0][0])
            else:
                self._rctrl_token_avail_i[0] = self._wctrl_token_avail_o[0][0]

            if self._rctrl_0_dual_dep:
                if (self._token_gen_state == 1):
                    if self._rctrl_token_dec_o[0] & (self._token_gen_count == self._token_gen_1):
                        self._rctrl_window_addr_i[0] = self._wctrl_window_addr_o[0]
                        self._rctrl_reset_window_addr_i[0] = self._wctrl_reset_window_addr_o[0][0]
                    else:
                        self._rctrl_window_addr_i[0] = self._wctrl_window_addr_o[1]
                        self._rctrl_reset_window_addr_i[0] = self._wctrl_reset_window_addr_o[1][1]
                else:
                    if self._rctrl_token_dec_o[0] & (self._token_gen_count == self._token_gen_0):
                        self._rctrl_window_addr_i[0] = self._wctrl_window_addr_o[1]
                        self._rctrl_reset_window_addr_i[0] = self._wctrl_reset_window_addr_o[1][1]
                    else:
                        self._rctrl_window_addr_i[0] = self._wctrl_window_addr_o[0]
                        self._rctrl_reset_window_addr_i[0] = self._wctrl_reset_window_addr_o[0][0]
            else:
                self._rctrl_window_addr_i[0] = self._wctrl_window_addr_o[0]
                self._rctrl_reset_window_addr_i[0] = self._wctrl_reset_window_addr_o[0][0]

            # rctrl_1 is only dependent on wctrl_1's token_0
            self._rctrl_token_avail_i[1] = self._wctrl_token_avail_o[1][0]
            self._rctrl_window_addr_i[1] = self._wctrl_window_addr_o[1]
            self._rctrl_reset_window_addr_i[1] = self._wctrl_reset_window_addr_o[1][0]

            self._wctrl_token_dec_i[0][0] = ~self._token_gen_state & self._rctrl_token_dec_o[0]
            self._wctrl_token_dec_i[0][1] = 0
            self._wctrl_token_dec_i[1][0] = self._rctrl_token_dec_o[1]
            self._wctrl_token_dec_i[1][1] = self._token_gen_state & self._rctrl_token_dec_o[0]

        self.add_code(write_to_read_dep)

        # read -> write ctrl dependency
        @always_comb
        def read_to_write_dep():
            # wctrl0 is not dependent on any rctrl
            self._wctrl_window_addr_i[0] = 0
            self._wctrl_reset_window_addr_i[0] = 0
            self._rctrl_token_dec_i[1] = 0

            self._wctrl_window_addr_i[1] = self._rctrl_window_addr_o[0]
            self._wctrl_reset_window_addr_i[1] = self._rctrl_reset_window_addr_o[0]
            self._rctrl_token_dec_i[0] = ternary(self._wctrl_1_dep, self._wctrl_token_dec_o[1], 0)

            # TODO check if _wctrl_token_avail_i is needed; currently not used
            self._wctrl_token_avail_i[0] = 1
            self._wctrl_token_avail_i[1] = ternary(self._wctrl_1_dep, self._rctrl_token_avail_o[0], 1)

        # dependent == True
        self.add_code(read_to_write_dep)

        for i in range(self.interconnect_output_ports):
            # heterogeneous read ctrl
            read_ctrl = RVReadCtrl(data_width=self.data_width,
                                   mem_width=self.mem_width,
                                   mem_depth=self.mem_depth,
                                   iterator_support=self.iterator_support,
                                   config_width=self.config_width,
                                   id_config_width=self.id_config_width,
                                   gen_token=(i == 0))

            # temp wire to share the wctrl.ag.starting_addr cfg reg
            wctrl_ag_gen = self.write_ctrls[i].child_generator()
            self._wctrl_ag_starting_addr = self.var(f"wctrl_ag_starting_addr_{i}_observed", self.mem_addr_width)
            observe_cfg(self, f"wctrl_ag_starting_addr_{i}_observed", wctrl_ag_gen["ag"], "starting_addr")

            self.add_child(f"{self.ctrl_out_name}_{i}", read_ctrl,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           ready_i=self._ready_in[i],
                           token_avail_i=self._rctrl_token_avail_i[i],
                           ag_starting_addr=self._wctrl_ag_starting_addr,
                           reset_window_addr_i=self._rctrl_reset_window_addr_i[i],
                           window_addr_i=self._rctrl_window_addr_i[i],
                           is_granted_i=self._is_granted[2 + i],
                           rdata_i=self._data_from_sram,
                           valid_o=self._valid_out[i],
                           dout_o=self._data_out_thin[i],
                           token_dec_o=self._rctrl_token_dec_o[i],
                           ren_o=self._rctrl_ren[i],
                           raddr_o=self._rctrl_raddr[i]
                           )

            # dependent == True
            if (i == 0):
                self._wctrl_0_dep_observed = self.var(f"wctrl_{i}_dep_observed", 1)
                observe_cfg(self, f"wctrl_{i}_dep_observed", self.write_ctrls[1], "dep")
                self.wire(read_ctrl.ports.token_gen_en_i, self._wctrl_0_dep_observed)
                self.wire(read_ctrl.ports.token_avail_o, self._rctrl_token_avail_o[i])
                self.wire(read_ctrl.ports.reset_window_addr_o, self._rctrl_reset_window_addr_o[i])
                self.wire(read_ctrl.ports.window_addr_o, self._rctrl_window_addr_o[i])
                self.wire(read_ctrl.ports.token_dec_i, self._rctrl_token_dec_i[i])

        mem_arbiter = RVArbiter(data_width=self.data_width,
                                mem_width=self.mem_width,
                                mem_depth=self.mem_depth)

        self.add_child("rv_mem_arbiter", mem_arbiter,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       wen_0_i=self._wctrl_wen[0],
                       waddr_0_i=self._wctrl_waddr[0],
                       wdata_0_i=self._wctrl_wdata[0],
                       wen_1_i=self._wctrl_wen[1],
                       waddr_1_i=self._wctrl_waddr[1],
                       wdata_1_i=self._wctrl_wdata[1],
                       ren_0_i=self._rctrl_ren[0],
                       raddr_0_i=self._rctrl_raddr[0],
                       ren_1_i=self._rctrl_ren[1],
                       raddr_1_i=self._rctrl_raddr[1],
                       is_granted_0_o=self._is_granted[0],
                       is_granted_1_o=self._is_granted[1],
                       is_granted_2_o=self._is_granted[2],
                       is_granted_3_o=self._is_granted[3],
                       mem_wen_o=self._wen_to_sram,
                       mem_ren_o=self._ren_to_sram,
                       mem_addr_o=self._addr_to_sram,
                       mem_wdata_o=self._data_to_sram
                       )

        self.base_ports = [[None]]
        # Dual port/single port guard.
        if self.rw_same_cycle:
            pass
            # self.wire(sram_only.ports.ren_to_sram, self.ss)
            # self.wire(sram_only.ports.wr_addr_to_sram, self._wr_addr_to_sram)
            # self.wire(sram_only.ports.rd_addr_to_sram, self._rd_addr_to_sram)
            # self.base_ports = [[None, None]]
            # tmp0_rdaddr = self.output("tmp0_rdaddr", width=self._rd_addr_to_sram.width)
            # tmp0_rden = self.output("tmp0_rden", width=self._ren_to_sram.width)
            # self.wire(tmp0_rdaddr, kts.const(0, width=self._rd_addr_to_sram.width))
            # self.wire(tmp0_rden, kts.const(0, width=self._ren_to_sram.width))
            # # Use first port as just W
            # rw_port = MemoryPort(MemoryPortType.READWRITE)
            # rw_port_intf = rw_port.get_port_interface()
            # rw_port_intf['data_in'] = self._data_to_sram
            # rw_port_intf['data_out'] = None
            # rw_port_intf['write_addr'] = self._wr_addr_to_sram
            # rw_port_intf['write_enable'] = self._wen_to_sram
            # rw_port_intf['read_addr'] = tmp0_rdaddr
            # rw_port_intf['read_enable'] = tmp0_rden
            # rw_port.annotate_port_signals()
            # self.base_ports[0][0] = rw_port
            # # Populate second port as just R
            # r_port = MemoryPort(MemoryPortType.READ)
            # r_port_intf = r_port.get_port_interface()
            # r_port_intf['data_out'] = self._data_from_sram
            # r_port_intf['read_addr'] = self._rd_addr_to_sram
            # r_port_intf['read_enable'] = self._ren_to_sram
            # r_port.annotate_port_signals()
            # self.base_ports[0][1] = r_port
        else:
            self.base_ports = [[None]]
            # Assume a single RW Port
            rw_port = MemoryPort(MemoryPortType.READWRITE)
            rw_port_intf = rw_port.get_port_interface()
            rw_port_intf['data_in'] = self._data_to_sram
            rw_port_intf['data_out'] = self._data_from_sram
            rw_port_intf['write_addr'] = self._addr_to_sram
            rw_port_intf['write_enable'] = self._wen_to_sram
            rw_port_intf['read_addr'] = self._addr_to_sram
            rw_port_intf['read_enable'] = self._ren_to_sram
            rw_port.annotate_port_signals()
            self.base_ports[0][0] = rw_port

        """
        # Add chaining in here... since we only use in the UB case...
        self._chain_data_in = self.input("chain_data_in",
                                         self.data_width + 1,
                                         size=self.interconnect_output_ports,
                                         packed=True,
                                         explicit_array=True)

        self._chain_data_in_thin = self.var("chain_data_in_thin",
                                            self.data_width,
                                            size=self.interconnect_output_ports,
                                            packed=True,
                                            explicit_array=True)

        for idx in range(self.interconnect_input_ports):
            self.wire(self._chain_data_in_thin[idx], self._chain_data_in[idx][self.data_width - 1, 0])

        chaining = ChainAccessor(data_width=self.data_width,
                                 interconnect_output_ports=self.interconnect_output_ports)

        self.add_child(f"chain", chaining,
                       curr_tile_data_out=self._data_out_int,
                       chain_data_in=self._chain_data_in_thin,
                       accessor_output=self._valid_out_int,
                       data_out_tile=self._data_out_thin)

        self.wire(self._valid_out, self._valid_out_int)
        """

    def get_static_bitstream(self, config_path, in_file_name, out_file_name):

        config = []
        return config

    def get_memory_ports(self):
        return self.base_ports

    def get_inputs(self):
        return super().get_inputs()

    def get_outputs(self):
        return super().get_outputs()

    def __str__(self):
        return self.name

    def get_bitstream(self, config_json):
        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        config = []
        in_ctrls = [f"{self.ctrl_in_name}_{i}" for i in range(self.interconnect_input_ports)]
        out_ctrls = [f"{self.ctrl_out_name}_{i}" for i in range(self.interconnect_output_ports)]
        for in_ctrl in in_ctrls:
            if in_ctrl in config_json:
                controller_tmp = (map_rv_controller(extract_rv_controller_json(config_json[in_ctrl]), in_ctrl), 0)
                config += configure_rv_controller(prefix="", name=in_ctrl, controller=controller_tmp)
        for out_ctrl in out_ctrls:
            if out_ctrl in config_json:
                controller_tmp = (map_rv_controller(extract_rv_controller_json(config_json[out_ctrl]), out_ctrl), 1)
                config += configure_rv_controller(prefix="", name=out_ctrl, controller=controller_tmp)

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")
        print("strg_ub_rv raw parsed config")
        print(config)
        return trim_config_list(flattened, config)

    def get_config_mode_str(self):
        return "RV"


if __name__ == "__main__":
    dut = StrgUBRV()
    dut.add_attribute("sync-reset=flush")
    kts.passes.auto_insert_sync_reset(dut.internal_generator)
    verilog(dut, filename="strg_ub_rv.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
