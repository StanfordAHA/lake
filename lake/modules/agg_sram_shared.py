from kratos import *
from lake.modules.passthru import *
from lake.modules.register_file import RegisterFile
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.range_group import RangeGroupAttr
from lake.passes.passes import lift_config_reg
from lake.modules.sram_stub import SRAMStub
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.utils.util import safe_wire, add_counter, decode
import kratos as kts


class StrgUBAggSRAMShared(Generator):
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

        super().__init__("strg_ub_agg_sram_shared")

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

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._cycle_count = self.input("cycle_count", 16)

        self._floop_mux_sel = self.output("floop_mux_sel",
                                          width=max(clog2(self.default_iterator_support), 1),
                                          size=self.interconnect_input_ports,
                                          explicit_array=True,
                                          packed=True)

        self._floop_restart = self.output("floop_restart",
                                          width=1,
                                          size=self.interconnect_input_ports,
                                          explicit_array=True,
                                          packed=True)

        # The SRAM write is just the OR reduction of the aggregator reads
        self._agg_read_out = self.output("agg_read_out", self.interconnect_input_ports)
        self._agg_read = self.var("agg_read", self.interconnect_input_ports)

        self.wire(self._agg_read_out, self._agg_read)

        ##################################################################################
        # AGG PATHS
        ##################################################################################
        for i in range(self.interconnect_input_ports):

            self.agg_iter_support = 6
            self.agg_addr_width = 4
            self.agg_range_width = 16

            # Create for loop counters that can be shared across the input port selection and SRAM write
            fl_ctr_sram_wr = ForLoop(iterator_support=self.default_iterator_support,
                                     config_width=self.default_config_width)

            self.add_child(f"loops_in2buf_autovec_write_{i}",
                           fl_ctr_sram_wr,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_read[i])

            safe_wire(gen=self, w_to=self._floop_mux_sel[i], w_from=fl_ctr_sram_wr.ports.mux_sel_out)
            self.wire(self._floop_restart[i], fl_ctr_sram_wr.ports.restart)

            # scheduler modules
            self.add_child(f"agg_read_sched_gen_{i}",
                           SchedGen(iterator_support=self.default_iterator_support,
                                    # config_width=self.mem_addr_width),
                                    config_width=16),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=fl_ctr_sram_wr.ports.mux_sel_out,
                           finished=fl_ctr_sram_wr.ports.restart,
                           valid_output=self._agg_read[i])


if __name__ == "__main__":
    lake_dut = StrgUBAggSRAMShared()
    verilog(lake_dut, filename="strg_ub_agg_sram_shared.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
