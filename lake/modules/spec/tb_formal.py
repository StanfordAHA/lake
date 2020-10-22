from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.sram_wrapper import SRAMWrapper
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.utils.util import extract_formal_annotation, safe_wire
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


class TBFormal(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 input_addr_iterator_support=6,
                 output_addr_iterator_support=6,
                 input_sched_iterator_support=6,
                 output_sched_iterator_support=6,
                 config_width=16,
                 #  output_config_width=16,
                 interconnect_input_ports=1,  # Connection to int
                 interconnect_output_ports=1,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4):
        super().__init__("tb_formal", debug=True)

        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.mem_depth = mem_depth
        self.banks = banks
        self.data_width = data_width
        self.config_width = config_width
        self.input_addr_iterator_support = input_addr_iterator_support
        self.output_addr_iterator_support = output_addr_iterator_support
        self.input_sched_iterator_support = input_sched_iterator_support
        self.output_sched_iterator_support = output_sched_iterator_support

        self.default_iterator_support = 6
        self.default_config_width = 16

        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))

        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.increment_cycle_count)
        self._read = self.var("read", 1)
        self._valid_in = self.output("valid_in", 1)
        self.wire(self._read, self._valid_in)
        self._valid_in.add_attribute(FormalAttr(f"{self._valid_in.name}", FormalSignalConstraint.SEQUENCE))

        self._data_in = self.input("data_in", data_width,
                                   size=self.fetch_width,
                                   packed=True,
                                   explicit_array=True)
        self._data_in.add_attribute(FormalAttr(f"{self._data_in.name}", FormalSignalConstraint.SEQUENCE))

        # outputs
        self._data_out = self.output("data_out", self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)
        self._data_out.add_attribute(FormalAttr(f"{self._data_out.name}", FormalSignalConstraint.SEQUENCE))

        self._tb_read = self.var("tb_read", self.interconnect_output_ports)
        # Break out valids for formal!
        self._valid_out = self.output("valid_out", self.interconnect_output_ports)
        self._valid_out.add_attribute(FormalAttr(f"{self._valid_out.name}", FormalSignalConstraint.SEQUENCE))
        self.wire(self._valid_out, self._tb_read)

        self.tb_height = 4

        self._tb_write_addr = self.var("tb_write_addr", 6,
                                       size=self.interconnect_output_ports,
                                       packed=True,
                                       explicit_array=True)
        self._tb_read_addr = self.var("tb_read_addr", 6,
                                      size=self.interconnect_output_ports,
                                      packed=True,
                                      explicit_array=True)

        self._tb = self.var("tb",
                            width=data_width,
                            size=(self.interconnect_output_ports,
                                  self.tb_height,
                                  self.fetch_width),
                            packed=True,
                            explicit_array=True)

        self._output_port_sel_addr = self.var("tb_bank_sel_addr",
                                              max(1, clog2(self.interconnect_output_ports)))

        # -------------------------------- Delineate new group -------------------------------
        fl_ctr_sram_rd = ForLoop(iterator_support=self.default_iterator_support,
                                 config_width=self.default_config_width)
        loop_itr = fl_ctr_sram_rd.get_iter()
        loop_wth = fl_ctr_sram_rd.get_cfg_width()

        self.add_child(f"tb_write_loops",
                       fl_ctr_sram_rd,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._read)

        self.add_child(f"tb_write_sched_gen",
                       SchedGen(iterator_support=self.default_iterator_support,
                                config_width=self.default_config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       mux_sel=fl_ctr_sram_rd.ports.mux_sel_out,
                       finished=fl_ctr_sram_rd.ports.restart,
                       valid_output=self._read)

        for i in range(self.interconnect_output_ports):
            # fl_ctr_tb_wr = ForLoop(iterator_support=self.default_iterator_support,
            #                        config_width=self.default_config_width)
            # loop_itr = fl_ctr_tb_wr.get_iter()
            # loop_wth = fl_ctr_tb_wr.get_cfg_width()

            # self.add_child(f"tb_write_loops_{i}",
            #                fl_ctr_tb_wr,
            #                clk=self._clk,
            #                rst_n=self._rst_n,
            #                step=self._read & (self._output_port_sel_addr ==
            #                                   const(i, self._output_port_sel_addr.width)))

            newAG = AddrGen(iterator_support=self.default_iterator_support,
                            config_width=self.default_config_width)

            self.add_child(f"tb_write_addr_gen_{i}",
                           newAG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._read & (self._output_port_sel_addr ==
                                              const(i, self._output_port_sel_addr.width)),
                           # addr_out=self._tb_write_addr[i])
                           mux_sel=fl_ctr_sram_rd.ports.mux_sel_out,
                           restart=fl_ctr_sram_rd.ports.restart)

            safe_wire(self, self._tb_write_addr[i], newAG.ports.addr_out)

            fl_ctr_tb_rd = ForLoop(iterator_support=self.default_iterator_support,
                                   config_width=self.default_config_width)
            loop_itr = fl_ctr_tb_rd.get_iter()
            loop_wth = fl_ctr_tb_rd.get_cfg_width()

            self.add_child(f"tb_read_loops_{i}",
                           fl_ctr_tb_rd,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._tb_read[i])

            newAG = AddrGen(iterator_support=self.default_iterator_support,
                            config_width=self.default_config_width)

            self.add_child(f"tb_read_addr_gen_{i}",
                           newAG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._tb_read[i],
                           # addr_out=self._tb_read_addr[i])
                           mux_sel=fl_ctr_tb_rd.ports.mux_sel_out,
                           restart=fl_ctr_tb_rd.ports.restart)

            safe_wire(self, self._tb_read_addr[i], newAG.ports.addr_out)

            self.add_child(f"tb_read_sched_gen_{i}",
                           SchedGen(iterator_support=self.default_iterator_support,
                                    config_width=self.default_config_width),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=fl_ctr_tb_rd.ports.mux_sel_out,
                           finished=fl_ctr_tb_rd.ports.restart,
                           valid_output=self._tb_read[i])

        if self.interconnect_output_ports > 1:

            # fl_ctr_out_sel = ForLoop(iterator_support=self.default_iterator_support,
            #                          # config_width=clog2(self.interconnect_output_ports))
            #                          config_width=self.default_config_width)
            # loop_itr = fl_ctr_out_sel.get_iter()
            # loop_wth = fl_ctr_out_sel.get_cfg_width()

            # self.add_child(f"tb_sel_loops",
            #                fl_ctr_out_sel,
            #                clk=self._clk,
            #                rst_n=self._rst_n,
            #                step=self._read)

            self.add_child(f"out_port_sel_addr",
                           AddrGen(iterator_support=self.default_iterator_support,
                                   config_width=self.default_config_width),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._read,
                           mux_sel=fl_ctr_sram_rd.ports.mux_sel_out,
                           addr_out=self._output_port_sel_addr)
            # Addr for port select should be driven on agg to sram write sched
        else:
            self.wire(self._output_port_sel_addr[0], const(0, self._output_port_sel_addr.width))

        self.add_code(self.tb_ctrl)
        for idx in range(self.interconnect_output_ports):
            self.add_code(self.tb_to_out, idx=idx)

    @always_ff((posedge, "clk"))
    def tb_ctrl(self):
        if self._read:
            self._tb[self._output_port_sel_addr][self._tb_write_addr[self._output_port_sel_addr][1, 0]] = \
                self._data_in

    @always_comb
    def tb_to_out(self, idx):
        self._data_out[idx] = self._tb[idx][self._tb_read_addr[idx][3, 2]][self._tb_read_addr[idx][1, 0]]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def increment_cycle_count(self):
        if ~self._rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1


if __name__ == "__main__":
    tb_dut = TBFormal()

    lift_config_reg(tb_dut.internal_generator)
    extract_formal_annotation(tb_dut, 'tb_formal_annotation.txt')

    verilog(tb_dut, filename="tb_formal.sv",
            optimize_if=False)
