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
                 tb_height=2,
                 max_inner_loops=3,
                 sram2tb_delay_buf=4):

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
        self.max_inner_loops = max_inner_loops
        self.sram2tb_delay_buf = sram2tb_delay_buf  # maximum delay from sram write to tb read to increment the shared outer loops

        self.default_iterator_support = 6
        self.default_config_width = 16
        self.sram_iterator_support = 6
        self.agg_rd_addr_gen_width = 8

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

        self._outer_loops_tb2out_inner_restart = self.input("outer_loops_tb2out_inner_restart", self.interconnect_input_ports)
        self._outer_loops_tb2out_enable = self.output("outer_loops_tb2out_enable", self.interconnect_input_ports)
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
            self._wr_ptr = self.var(f"wr_ptr_{i}", clog2(self.sram2tb_delay_buf))
            self._rd_ptr = self.var(f"rd_ptr_{i}", clog2(self.sram2tb_delay_buf))
            self._restart_fifo = self.var(f"restart_fifo_{i}", self.sram2tb_delay_buf)
            self._mux_sel_fifo = self.var(f"mux_sel_fifo_{i}",
                                          width=max(clog2(self.default_iterator_support), 1),
                                          size=self.sram2tb_delay_buf,
                                          explicit_array=True,
                                          packed=True)
            self._outer_fl_dim = self.var(f"outer_fl_dim_{i}", 1 + clog2(self.default_iterator_support))
            self._outer_fl_step = self.var(f"outer_fl_step_{i}", 1)
            self._outer_factorization = self.var(f"outer_factorization_{i}", 1)
            self._sram2tb_restart = self.var(f"sram2tb_restart_{i}", 1)
            self._sram2tb_mux_sel = self.var(f"sram2tb_mux_sel_{i}",
                                             width=max(clog2(self.default_iterator_support), 1))
            self._inner_fl_restart = self.var(f"inner_fl_loop_restart_{i}", 1)
            self._inner_mux_sel = self.var(f"inner_mux_sel_{i}", width=max(clog2(self.default_iterator_support), 1))

            # for loop for sram reads, tb writes
            loops_sram2tb = ForLoop(iterator_support=self.max_inner_loops,
                                    config_width=self.default_config_width)

            self.add_child(f"loops_buf2out_autovec_read_{i}",
                           loops_sram2tb,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._t_read[i],
                           restart=self._inner_fl_restart)

            # Outer loop factorization with tb2out
            outer_fl_ctr = OuterForLoop(iterator_support=self.default_iterator_support,  # is the total # of iterators
                                        iter_start=self.max_inner_loops,  # starts after inner iterator
                                        config_width=self.default_config_width)

            self.add_child(f"outer_loops_autovec_{i}",
                           outer_fl_ctr,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._outer_fl_step)

            self.wire(self._outer_factorization, self[f"outer_loops_autovec_{i}"].ports.dimensionality > 0)
            self.wire(self._outer_fl_step, ternary(self._outer_factorization, loops_sram2tb.ports.restart, const(0, 1)))
            safe_wire(gen=self, w_to=self._inner_mux_sel, w_from=loops_sram2tb.ports.mux_sel_out)
            self.wire(self._sram2tb_restart, ternary(self._outer_factorization,
                                                     outer_fl_ctr.ports.restart,
                                                     self._inner_fl_restart))
            self.wire(self._sram2tb_mux_sel, ternary(self._inner_fl_restart,
                                                     outer_fl_ctr.ports.mux_sel_out,
                                                     self._inner_mux_sel))
            safe_wire(gen=self, w_to=self._loops_sram2tb_mux_sel[i], w_from=self._sram2tb_mux_sel)
            self.wire(self._loops_sram2tb_restart[i], self._sram2tb_restart)

            self.add_code(self.update_outer_loops_fifo, port=i)
            self.wire(self._outer_loops_tb2out_enable[i], self._outer_factorization)
            self.wire(self._outer_loops_tb2out_restart[i], self._restart_fifo[self._rd_ptr] & self._outer_loops_tb2out_inner_restart[i])
            self.wire(self._outer_loops_tb2out_mux_sel[i], self._mux_sel_fifo[self._rd_ptr])

            # sram read schedule, delay by 1 clock cycle for tb write schedule (done in tb_only)
            self.add_child(f"output_sched_gen_{i}",
                           SchedGen(iterator_support=self.default_iterator_support,
                                    # config_width=self.default_config_width),
                                    config_width=16),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=self._sram2tb_mux_sel,
                           finished=self._sram2tb_restart,
                           valid_output=self._t_read[i])

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_outer_loops_fifo(self, port):
        if ~self._rst_n:
            self._wr_ptr = 0
            self._rd_ptr = 0
            self._restart_fifo = 0
            self._mux_sel_fifo = 0
        elif self._outer_factorization:
            if self._outer_fl_step:
                self._restart_fifo[self._wr_ptr] = self[f"outer_loops_autovec_{port}"].ports.restart
                self._mux_sel_fifo[self._wr_ptr] = self[f"outer_loops_autovec_{port}"].ports.mux_sel_out
                if self._wr_ptr == (self.sram2tb_delay_buf - 1):
                    self._wr_ptr = 0
                else:
                    self._wr_ptr = self._wr_ptr + 1

            if self._outer_loops_tb2out_inner_restart[port]:
                if self._rd_ptr == (self.sram2tb_delay_buf - 1):
                    self._rd_ptr = 0
                else:
                    self._rd_ptr = self._rd_ptr + 1


if __name__ == "__main__":
    lake_dut = StrgUBSRAMTBShared()
    verilog(lake_dut, filename="strg_ub_sram_tb_shared.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
