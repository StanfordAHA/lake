from kratos import *
from lake.modules.passthru import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.utils.util import safe_wire


class StrgUBAggOnly(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 input_addr_iterator_support=6,
                 input_sched_iterator_support=6,
                 config_width=16,
                 #  output_config_width=16,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 area_opt=True,
                 reduced_id_config_width=10,
                 addr_fifo_depth=8,
                 agg_iter_support_small=3,
                 agg_height=4,
                 agg_addr_width=3,
                 tb_height=2):

        super().__init__("strg_ub_agg_only")

        assert mem_width > data_width, \
            f"Aggregator only should be used when mem_width is power of 2 times data_width"

        ##################################################################################
        # Capture constructor parameter...
        ##################################################################################
        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.tb_height = tb_height
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.config_width = config_width
        self.data_width = data_width
        self.input_addr_iterator_support = input_addr_iterator_support
        self.input_sched_iterator_support = input_sched_iterator_support
        self.mem_addr_width = clog2(self.mem_depth)
        self.area_opt = area_opt
        self.reduced_id_config_width = reduced_id_config_width
        self.addr_fifo_depth = addr_fifo_depth

        self.default_iterator_support = 6
        self.default_config_width = 16
        self.sram_iterator_support = 6
        self.agg_rd_addr_gen_width = 8

        self.agg_iter_support = 6
        self.agg_iter_support_small = agg_iter_support_small
        self.agg_addr_width = agg_addr_width
        self.agg_range_width = 16
        self.agg_wr_addr_width = 2 + clog2(self.agg_height)
        self.agg_rd_addr_width = max(1, clog2(self.agg_height))

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._cycle_count = self.input("cycle_count", 16)

        self._data_in = self.input("data_in", self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        self._agg_read = self.input("agg_read", self.interconnect_input_ports)

        if self.area_opt:
            self._sram_read_addr_in = self.input("sram_read_addr_in", self.mem_addr_width,
                                                 size=self.interconnect_input_ports,
                                                 packed=True,
                                                 explicit_array=True)
            self._tb_read_d_in = self.input("tb_read_d_in", self.interconnect_input_ports)
            self._tb_read_addr_d_in = self.input("tb_read_addr_d_in", 2 + clog2(self.agg_height),
                                                 size=self.interconnect_input_ports,
                                                 packed=True,
                                                 explicit_array=True)
            self._update_mode_in = self.input("update_mode_in", 2,
                                              size=self.interconnect_input_ports,
                                              packed=True,
                                              explicit_array=True)

            self._agg_write_restart_out = self.output("agg_write_restart_out", self.interconnect_input_ports)
            self._agg_write_out = self.output("agg_write_out", self.interconnect_input_ports)
            self._agg_write_addr_l2b_out = self.output("agg_write_addr_l2b_out", 2,
                                                       size=self.interconnect_input_ports,
                                                       packed=True,
                                                       explicit_array=True)
            self._agg_write_mux_sel_out = self.output("agg_write_mux_sel_out", max(clog2(self.agg_iter_support), 1),
                                                      size=self.interconnect_input_ports,
                                                      packed=True,
                                                      explicit_array=True)

        else:
            self._floop_mux_sel = self.input("floop_mux_sel",
                                             width=max(clog2(self.default_iterator_support), 1),
                                             size=self.interconnect_input_ports,
                                             explicit_array=True,
                                             packed=True)
            self._floop_restart = self.input("floop_restart",
                                             width=1,
                                             size=self.interconnect_input_ports,
                                             explicit_array=True,
                                             packed=True)

        self._agg_data_out = self.output(f"agg_data_out", self.data_width,
                                         size=(self.interconnect_input_ports,
                                               self.fetch_width),
                                         packed=True,
                                         explicit_array=True)
        self._agg_data_out.add_attribute(FormalAttr(self._agg_data_out.name, FormalSignalConstraint.SEQUENCE, "sram"))

        ##################################################################################
        # AGG RELEVANT SIGNALS
        ##################################################################################
        # Create an input to agg write scheduler + addressor for each input
        # Also need an addressor for the mux in addition to the read addr
        self._agg = self.var(f"agg",
                             width=self.data_width,
                             size=(self.interconnect_input_ports,
                                   self.agg_height,
                                   self.fetch_width),
                             packed=True,
                             explicit_array=True)

        self._agg_write = self.var("agg_write", self.interconnect_input_ports)
        # Make this based on the size
        if self.area_opt:
            self._agg_write_addr = self.var("agg_write_addr", 2 + clog2(self.agg_height),
                                            size=self.interconnect_input_ports,
                                            packed=True,
                                            explicit_array=True)
            self._agg_read_addr_in = self.var("agg_read_addr_in", max(1, clog2(self.agg_height)),
                                              size=self.interconnect_input_ports,
                                              packed=True,
                                              explicit_array=True)
            self.wire(self._agg_write_out, self._agg_write)
        else:
            self._agg_write_addr = self.var("agg_write_addr", clog2(self.fetch_width) + clog2(self.agg_height),
                                            size=self.interconnect_input_ports,
                                            packed=True,
                                            explicit_array=True)
        self._agg_read_addr = self.var("agg_read_addr", max(1, clog2(self.agg_height)),
                                       size=self.interconnect_input_ports,
                                       packed=True,
                                       explicit_array=True)
        self._agg_read_addr_gen_out = self.var("agg_read_addr_gen_out", self.agg_rd_addr_gen_width,
                                               size=self.interconnect_input_ports,
                                               packed=True,
                                               explicit_array=True)

        ##################################################################################
        # AGG PATHS
        ##################################################################################
        for i in range(self.interconnect_input_ports):

            self.agg_iter_support = 6
            self.agg_addr_width = 4
            self.agg_range_width = 16

            if self.area_opt:
                # mode=1 for update operation mode
                self._mode = self.var(f"mode_{i}", 2)
                self.wire(self._mode, self._update_mode_in[i])

                self.wire(self._agg_write_addr_l2b_out[i], self._agg_write_addr[i][1, 0])

                self._tb_read = self.var(f"tb_read_{i}", 1)
                self._tb_addr = self.var(f"tb_addr_{i}", self._agg_write_addr.width)
                if self.interconnect_input_ports == 1:
                    self.wire(self._tb_read, self._tb_read_d_in[0])
                    self.wire(self._tb_addr, self._tb_read_addr_d_in[0])
                else:
                    self.wire(self._tb_read, ternary(self._mode[0], self._tb_read_d_in[1], self._tb_read_d_in[0]))
                    self.wire(self._tb_addr, ternary(self._mode[0], self._tb_read_addr_d_in[1], self._tb_read_addr_d_in[0]))

                forloop_ctr = ForLoop(iterator_support=self.agg_iter_support_small,
                                      # config_width=self.default_config_width)
                                      config_width=self.reduced_id_config_width)
                loop_itr = forloop_ctr.get_iter()
                loop_wth = forloop_ctr.get_cfg_width()

                self.add_child(f"loops_in2buf_{i}",
                               forloop_ctr,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               step=self._agg_write[i])
                # create a wire to match the small loop ctrl's mux_sel width to the regular size (6 levels)
                self._fl_mux_sel = self.var(f"fl_mux_sel_{i}", max(clog2(self.agg_iter_support), 1))
                safe_wire(gen=self, w_to=self._fl_mux_sel, w_from=forloop_ctr.ports.mux_sel_out)
                self.wire(self._agg_write_mux_sel_out[i], self._fl_mux_sel)
                self.wire(self._agg_write_restart_out[i], forloop_ctr.ports.restart)

                newAG = AddrGen(iterator_support=self.agg_iter_support_small,
                                config_width=self.agg_wr_addr_width)
                self.add_child(f"agg_write_addr_gen_{i}",
                               newAG,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               step=self._agg_write[i],
                               mux_sel=forloop_ctr.ports.mux_sel_out,
                               restart=forloop_ctr.ports.restart)
                safe_wire(gen=self, w_to=self._agg_write_addr[i], w_from=ternary(self._mode[1], self._tb_addr, newAG.ports.addr_out))

                newSG = SchedGen(iterator_support=self.agg_iter_support_small,
                                 # config_width=self.agg_addr_width)
                                 config_width=16)

                self.add_child(f"agg_write_sched_gen_{i}",
                               newSG,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               mux_sel=forloop_ctr.ports.mux_sel_out,
                               finished=forloop_ctr.ports.restart,
                               cycle_count=self._cycle_count)
                self.wire(self._agg_write[i], ternary(self._mode[1], self._tb_read, newSG.ports.valid_output))
            else:
                forloop_ctr = ForLoop(iterator_support=self.agg_iter_support,
                                      # config_width=self.default_config_width)
                                      config_width=self.agg_range_width)
                loop_itr = forloop_ctr.get_iter()
                loop_wth = forloop_ctr.get_cfg_width()

                self.add_child(f"loops_in2buf_{i}",
                               forloop_ctr,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               step=self._agg_write[i])

                newAG = AddrGen(iterator_support=self.agg_iter_support,
                                config_width=self.agg_addr_width)
                self.add_child(f"agg_write_addr_gen_{i}",
                               newAG,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               step=self._agg_write[i],
                               mux_sel=forloop_ctr.ports.mux_sel_out,
                               restart=forloop_ctr.ports.restart)
                safe_wire(gen=self, w_to=self._agg_write_addr[i], w_from=newAG.ports.addr_out)

                newSG = SchedGen(iterator_support=self.agg_iter_support,
                                 # config_width=self.agg_addr_width)
                                 config_width=16)
                self.add_child(f"agg_write_sched_gen_{i}",
                               newSG,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               mux_sel=forloop_ctr.ports.mux_sel_out,
                               finished=forloop_ctr.ports.restart,
                               cycle_count=self._cycle_count,
                               valid_output=self._agg_write[i])

            @always_ff((posedge, "clk"))
            def agg_ctrl():
                if self._agg_write[i]:
                    if self.agg_height == 1:
                        self._agg[i][0][self._agg_write_addr[i][clog2(self.fetch_width) - 1, 0]] = self._data_in[i]
                    else:
                        self._agg[i][self._agg_write_addr[i]
                                     [self._agg_write_addr[0].width - 1, clog2(self.fetch_width)]]\
                                    [self._agg_write_addr[i][clog2(self.fetch_width) - 1, 0]] = self._data_in[i]

            self.add_code(agg_ctrl)

            if self.area_opt:
                self.wire(self._agg_read_addr_in[i], self._sram_read_addr_in[i][self.agg_rd_addr_width - 1, 0])
                safe_wire(gen=self, w_to=self._agg_read_addr_gen_out[i], w_from=self._agg_read_addr_in[i])
            else:
                newAG = AddrGen(iterator_support=self.default_iterator_support,
                                config_width=self.agg_addr_width)

                self.add_child(f"agg_read_addr_gen_{i}",
                               newAG,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               step=self._agg_read[i],
                               #  (self._input_port_sel_addr == const(i, self._input_port_sel_addr.width))),
                               # mux_sel=self._floop_mux_sel[i],
                               restart=self._floop_restart[i])
                safe_wire(gen=self, w_to=newAG.ports.mux_sel, w_from=self._floop_mux_sel[i])
                safe_wire(gen=self, w_to=self._agg_read_addr_gen_out[i], w_from=newAG.ports.addr_out)
            self.wire(self._agg_read_addr[i], self._agg_read_addr_gen_out[i][self._agg_read_addr.width - 1, 0])

            # Now pick out the data from the agg...
            @always_comb
            def get_agg_data():
                self._agg_data_out[i] = self._agg[i][self._agg_read_addr[i]]
            self.add_code(get_agg_data)


if __name__ == "__main__":
    lake_dut = StrgUBAggOnly()
    verilog(lake_dut, filename="strg_ub_agg_only.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
