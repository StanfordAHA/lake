from kratos import *
from lake.modules.passthru import *
from lake.modules.register_file import RegisterFile
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.range_group import RangeGroupAttr
from lake.passes.passes import lift_config_reg
from lake.modules.sram_stub import SRAMStub
from lake.modules.for_loop import ForLoop
from lake.modules.outer_for_loop import OuterForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.utils.util import safe_wire, add_counter, decode
import kratos as kts


class StrgUBSRAMTBShared(Generator):
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
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4,
                 tb_height=2):

        super().__init__("strg_ub_sram_tb_shared")

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

        self.default_iterator_support = 6
        self.default_config_width = 16
        self.sram_iterator_support = 6
        self.agg_rd_addr_gen_width = 8
        self.outer_iterator_start = 3
        self.sram2tb_delay_buf = 12  # maximum difference of cycle_starting_addr of sram write and tb read

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._cycle_count = self.input("cycle_count", 16)

        self._loops_sram2tb_mux_sel = self.output("loops_sram2tb_mux_sel",
                                                  width=max(clog2(self.default_iterator_support), 1),
                                                  size=self.interconnect_output_ports,
                                                  explicit_array=True,
                                                  packed=True)
        self._loops_sram2tb_restart = self.output("loops_sram2tb_restart",
                                                  width=1,
                                                  size=self.interconnect_output_ports,
                                                  explicit_array=True,
                                                  packed=True)

        self._outer_loops_tb2out_restart = self.output("outer_loops_tb2out_restart", self.interconnect_input_ports)
        self._outer_loops_tb2out_mux_sel = self.output("outer_loops_tb2out_mux_sel",
                                                       width=max(clog2(self.default_iterator_support), 1),
                                                       size=self.interconnect_input_ports,
                                                       explicit_array=True,
                                                       packed=True)

        self._t_read_out = self.output("t_read_out", self.interconnect_output_ports)
        self._t_read = self.var("t_read", self.interconnect_output_ports)
        self.wire(self._t_read_out, self._t_read)

        ##################################################################################
        # TB PATHS
        ##################################################################################
        for i in range(self.interconnect_output_ports):

            self._sram2tb_delay = self.input(f"sram2tb_delay_{i}", clog2(self.sram2tb_delay_buf))
            self._sram2tb_delay.add_attribute(ConfigRegAttr("delay from sram write to tb read"))
            self._sram2tb_delay.add_attribute(FormalAttr(f"{self._sram2tb_delay.name}_{i}", FormalSignalConstraint.SOLVE))

            self._restart_shift_r = self.var(f"restart_shift_r_{i}", self.sram2tb_delay_buf)
            self._mux_sel_shift_r = self.var(f"mux_sel_shift_r_{i}",
                                             width=max(clog2(self.default_iterator_support), 1),
                                             size=self.sram2tb_delay_buf,
                                             explicit_array=True,
                                             packed=True)
            self._sram2tb_mux_sel = self.var(f"sram2tb_mux_sel_{i}",
                                             width=max(clog2(self.default_iterator_support), 1))
            self._inner_fl_restart = self.var(f"inner_fl_loop_restart_{i}", 1)

            # for loop for sram reads, tb writes
            loops_sram2tb = ForLoop(iterator_support=self.default_iterator_support,
                                    config_width=self.default_config_width)

            self.add_child(f"loops_buf2out_autovec_read_{i}",
                           loops_sram2tb,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._t_read[i],
                           restart=self._inner_fl_restart)

            # safe_wire(gen=self, w_to=self._loops_sram2tb_mux_sel[i], w_from=loops_sram2tb.ports.mux_sel_out)
            # self.wire(self._loops_sram2tb_restart[i], loops_sram2tb.ports.restart)

            # Outer loop factorization with tb2out
            outer_fl_ctr = OuterForLoop(iterator_support=self.default_iterator_support,
                                        iter_start=self.outer_iterator_start,
                                        config_width=self.default_config_width)

            self.add_child(f"outer_loops_autovec_{i}",
                           outer_fl_ctr,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=loops_sram2tb.ports.restart)
            self.wire(self._sram2tb_mux_sel, ternary(self._inner_fl_restart,
                                                     outer_fl_ctr.ports.mux_sel_out,
                                                     loops_sram2tb.ports.mux_sel_out))
            safe_wire(gen=self, w_to=self._loops_sram2tb_mux_sel[i], w_from=self._sram2tb_mux_sel)
            self.wire(self._loops_sram2tb_restart[i], outer_fl_ctr.ports.restart)

            self.add_code(self.update_outer_loops_shift_r, port=i)
            self.wire(self._outer_loops_tb2out_restart[i], self._restart_shift_r[self._sram2tb_delay])
            self.wire(self._outer_loops_tb2out_mux_sel[i], self._mux_sel_shift_r[self._sram2tb_delay])

            # sram read schedule, delay by 1 clock cycle for tb write schedule (done in tb_only)
            self.add_child(f"output_sched_gen_{i}",
                           SchedGen(iterator_support=self.default_iterator_support,
                                    # config_width=self.default_config_width),
                                    config_width=16),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=self._sram2tb_mux_sel,
                           finished=outer_fl_ctr.ports.restart,
                           valid_output=self._t_read[i])

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_outer_loops_shift_r(self, port):
        if ~self._rst_n:
            self._restart_shift_r = 0
            self._mux_sel_shift_r = 0
        # elif self[f"outer_loops_autovec_{port}"].ports.dimensionality:
        else:
            self._restart_shift_r = concat(self._restart_shift_r[self.sram2tb_delay_buf - 2, 0], self[f"outer_loops_autovec_{port}"].ports.restart)
            self._mux_sel_shift_r = concat(self._mux_sel_shift_r[self.sram2tb_delay_buf - 2, 0], self[f"outer_loops_autovec_{port}"].ports.mux_sel_out)


if __name__ == "__main__":
    lake_dut = StrgUBSRAMTBShared()
    verilog(lake_dut, filename="strg_ub_sram_tb_shared.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
