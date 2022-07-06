from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.spec.cycle_sched_gen import CycleSchedGen
from lake.modules.spec.valid_sched_gen import ValidSchedGen
from lake.utils.util import safe_wire


class ValidCycleCtrl(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 cycle_iterator_support=6,
                 #  input_addr_iterator_support=6,
                 #  output_addr_iterator_support=6,
                 #  input_sched_iterator_support=6,
                 #  output_sched_iterator_support=6,
                 #  config_width=16,
                 #   output_config_width=16,
                 #  interconnect_input_ports=2,  # Connection to int
                 #  interconnect_output_ports=2,
                 #  mem_input_ports=1,
                 #  mem_output_ports=1,
                 #  read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 #  rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4,
                 tb_height=2):

        super().__init__("strg_ub_valid_cycle_ctrl")

        ##################################################################################
        # Capture constructor parameter...
        ##################################################################################
        self.fetch_width = mem_width // data_width
        # self.interconnect_input_ports = interconnect_input_ports
        # self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.tb_height = tb_height
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        # self.config_width = config_width
        self.data_width = data_width
        # self.input_addr_iterator_support = input_addr_iterator_support
        # self.input_sched_iterator_support = input_sched_iterator_support
        self.cycle_iterator_support = cycle_iterator_support

        # self.default_iterator_support = 6
        self.default_config_width = 16
        # self.sram_iterator_support = 6
        # self.agg_rd_addr_gen_width = 8

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._valid_in = self.input("valid_in", 1)
        self._tbread_cycle_done = self.input("tbread_cycle_done", 1)

        self._tbwrite_en = self.output("tbwrite_en", 1)
        self._tbread_cycle_en = self.output("tbread_cycle_en", 1)
        self._mux_sel_out = self.output("mux_sel_out", max(clog2(self.cycle_iterator_support), 1))
        self._restart_out = self.output("restart_out", 1)

        ##################################################################################
        # Valid Domain
        ##################################################################################
        # local valid count for initiating cycle domains
        self._valid_count = self.var("valid_count", 16)
        self._valid_done = self.var("valid_done", 1)
        self._valid_sg_en = self.var("valid_sg_en", 1)

        @always_ff((posedge, self._clk), (negedge, "rst_n"))
        def increment_valid_count(self):
            if ~self._rst_n:
                self._valid_count = 0
            elif self._valid_done:
                self._valid_count = 0
            elif self._valid_in:
                self._valid_count = self._valid_count + 1

        self.add_always(increment_valid_count)

        self._cycle_count = self.var("cycle_count", 16)
        self._cycle_done = self.var("cycle_done", 1)
        self._cycle_sg_en = self.var("cycle_sg_en", 1)

        # # local cycle count for relative affine accesses
        # @always_ff((posedge, self._clk), (negedge, "rst_n"))
        # def increment_cycle_count(self):
        #     if ~self._rst_n:
        #         self._cycle_count = 0
        #     elif self._cycle_done:
        #         self._cycle_count = 0
        #     elif self._valid_sg_en:
        #         self._cycle_count = self._cycle_count + 1

        # self.add_always(increment_cycle_count)

        valid_loops = ForLoop(iterator_support=1,
                              config_width=self.default_config_width)

        self.add_child("valid_id",
                       valid_loops,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._cycle_done,
                       restart=self._valid_done)

        self.add_child("valid_sg",
                       ValidSchedGen(iterator_support=1,
                                     config_width=self.default_config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._valid_count,
                       step=self._tbread_cycle_done,
                       mux_sel=valid_loops.ports.mux_sel_out,
                       finished=self._valid_done,
                       valid_output=self._valid_sg_en)

        self.wire(self._restart_out, self._valid_done)

        ##################################################################################
        # Relative Cycle Domain
        ##################################################################################
        # for loop
        cycle_loops = ForLoop(iterator_support=self.cycle_iterator_support,
                              config_width=10)

        self.add_child("cycle_id",
                       cycle_loops,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._cycle_sg_en,
                       restart=self._cycle_done)

        # safe_wire(gen=self, w_to=self._loops_sram2tb_mux_sel[i], w_from=loops_sram2tb.ports.mux_sel_out)
        # self.wire(self._loops_sram2tb_restart[i], loops_sram2tb.ports.restart)
        self.wire(self._mux_sel_out, cycle_loops.ports.mux_sel_out)
        # self.wire(self._restart_out, self._cycle_done)

        self.add_child("cycle_sg",
                       CycleSchedGen(iterator_support=self.cycle_iterator_support,
                                     config_width=self.default_config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._valid_sg_en,
                       mux_sel=cycle_loops.ports.mux_sel_out,
                       finished=self._cycle_done,
                       valid_output=self._cycle_sg_en)

        self.wire(self._tbwrite_en, self._cycle_sg_en)

        self.wire(self._tbread_cycle_en, self._valid_sg_en)


if __name__ == "__main__":
    lake_dut = ValidCycleCtrl()
    verilog(lake_dut, filename="valid_cycle_ctrl.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
