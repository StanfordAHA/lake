from kratos import *
from lake.modules.passthru import *
from lake.modules.register_file import RegisterFile
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import *
from lake.attributes.range_group import RangeGroupAttr
from lake.passes.passes import lift_config_reg
from lake.modules.sram_stub import SRAMStub
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.modules.agg_only import StrgUBAggOnly
from lake.modules.agg_sram_shared import StrgUBAggSRAMShared
from lake.modules.sram_only import StrgUBSRAMOnly
from lake.modules.sram_tb_shared import StrgUBSRAMTBShared
from lake.modules.tb_only import StrgUBTBOnly
from lake.utils.util import safe_wire, add_counter, decode
import kratos as kts


class StrgUBVec(Generator):
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
                 agg_data_top=False):

        super().__init__("strg_ub_vec")

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
        self.banks = banks
        self.config_width = config_width
        self.data_width = data_width
        self.read_delay = read_delay
        self.rw_same_cycle = rw_same_cycle
        self.input_config_width = config_width
        self.input_addr_iterator_support = input_addr_iterator_support
        self.input_sched_iterator_support = input_sched_iterator_support

        self.input_iterator_support = 6
        self.output_iterator_support = 6
        self.default_iterator_support = 6
        self.default_config_width = 16
        self.sram_iterator_support = 6
        self.agg_rd_addr_gen_width = 8

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._data_in = self.input("data_in", self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        self._data_from_sram = self.input("data_from_strg", self.data_width,
                                          size=self.fetch_width,
                                          packed=True)

        self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
        self._cen_to_sram = self.output("cen_to_strg", 1, packed=True)
        self._addr_to_sram = self.output("addr_out", clog2(self.mem_depth), packed=True)
        self._data_to_sram = self.output("data_to_strg", self.data_width,
                                         size=self.fetch_width,
                                         packed=True)

        self._valid_out = self.output("accessor_output", self.interconnect_output_ports)
        self._data_out = self.output("data_out", self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        ##################################################################################
        # CYCLE COUNTER
        ##################################################################################

        # Create cycle counter to share...
        self._cycle_count = add_counter(self, "cycle_count", 16)

        agg_only = StrgUBAggOnly(data_width=self.data_width,
                                 mem_width=self.mem_width,
                                 mem_depth=self.mem_depth,
                                 banks=self.banks,
                                 input_addr_iterator_support=self.input_iterator_support,
                                 output_addr_iterator_support=self.output_iterator_support,
                                 input_sched_iterator_support=self.input_iterator_support,
                                 output_sched_iterator_support=self.output_iterator_support,
                                 interconnect_input_ports=self.interconnect_input_ports,
                                 interconnect_output_ports=self.interconnect_output_ports,
                                 read_delay=self.read_delay,
                                 rw_same_cycle=self.rw_same_cycle,
                                 agg_height=self.agg_height,
                                 config_width=self.input_config_width)

        agg_sram_shared = StrgUBAggSRAMShared(data_width=self.data_width,
                                              mem_width=self.mem_width,
                                              mem_depth=self.mem_depth,
                                              banks=self.banks,
                                              input_addr_iterator_support=self.input_iterator_support,
                                              output_addr_iterator_support=self.output_iterator_support,
                                              input_sched_iterator_support=self.input_iterator_support,
                                              output_sched_iterator_support=self.output_iterator_support,
                                              interconnect_input_ports=self.interconnect_input_ports,
                                              interconnect_output_ports=self.interconnect_output_ports,
                                              read_delay=self.read_delay,
                                              rw_same_cycle=self.rw_same_cycle,
                                              agg_height=self.agg_height,
                                              config_width=self.input_config_width)

        sram_only = StrgUBSRAMOnly(data_width=self.data_width,
                                   mem_width=self.mem_width,
                                   mem_depth=self.mem_depth,
                                   banks=self.banks,
                                   input_addr_iterator_support=self.input_iterator_support,
                                   output_addr_iterator_support=self.output_iterator_support,
                                   input_sched_iterator_support=self.input_iterator_support,
                                   output_sched_iterator_support=self.output_iterator_support,
                                   interconnect_input_ports=self.interconnect_input_ports,
                                   interconnect_output_ports=self.interconnect_output_ports,
                                   read_delay=self.read_delay,
                                   rw_same_cycle=self.rw_same_cycle,
                                   agg_height=self.agg_height,
                                   config_width=self.input_config_width)

        sram_tb_shared = StrgUBSRAMTBShared(data_width=self.data_width,
                                            mem_width=self.mem_width,
                                            mem_depth=self.mem_depth,
                                            banks=self.banks,
                                            input_addr_iterator_support=self.input_iterator_support,
                                            output_addr_iterator_support=self.output_iterator_support,
                                            input_sched_iterator_support=self.input_iterator_support,
                                            output_sched_iterator_support=self.output_iterator_support,
                                            interconnect_input_ports=self.interconnect_input_ports,
                                            interconnect_output_ports=self.interconnect_output_ports,
                                            read_delay=self.read_delay,
                                            rw_same_cycle=self.rw_same_cycle,
                                            agg_height=self.agg_height,
                                            config_width=self.input_config_width)

        tb_only = StrgUBTBOnly(data_width=self.data_width,
                               mem_width=self.mem_width,
                               mem_depth=self.mem_depth,
                               banks=self.banks,
                               input_addr_iterator_support=self.input_iterator_support,
                               output_addr_iterator_support=self.output_iterator_support,
                               input_sched_iterator_support=self.input_iterator_support,
                               output_sched_iterator_support=self.output_iterator_support,
                               interconnect_input_ports=self.interconnect_input_ports,
                               interconnect_output_ports=self.interconnect_output_ports,
                               read_delay=self.read_delay,
                               rw_same_cycle=self.rw_same_cycle,
                               agg_height=self.agg_height,
                               config_width=self.input_config_width)

        self.add_child("agg_only",
                       agg_only,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       data_in=self._data_in)

        self.add_child("agg_sram_shared",
                       agg_sram_shared,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count)

        self.add_child("sram_only",
                       sram_only,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       wen_to_sram=self._wen_to_sram,
                       cen_to_sram=self._cen_to_sram,
                       addr_to_sram=self._addr_to_sram,
                       data_to_sram=self._data_to_sram)

        self.add_child("sram_tb_shared",
                       sram_tb_shared,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count)

        self.add_child("tb_only",
                       tb_only,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       sram_read_data=self._data_from_sram,
                       accessor_output=self._valid_out,
                       data_out=self._data_out)

        self.wire(agg_only.ports.agg_read, agg_sram_shared.ports.agg_read_out)
        self.wire(agg_only.ports.floop_mux_sel, agg_sram_shared.ports.floop_mux_sel)
        self.wire(agg_only.ports.floop_restart, agg_sram_shared.ports.floop_restart)

        self.wire(sram_only.ports.floop_mux_sel, agg_sram_shared.ports.floop_mux_sel)
        self.wire(sram_only.ports.floop_restart, agg_sram_shared.ports.floop_restart)
        self.wire(sram_only.ports.loops_sram2tb_mux_sel, sram_tb_shared.ports.loops_sram2tb_mux_sel)
        self.wire(sram_only.ports.loops_sram2tb_restart, sram_tb_shared.ports.loops_sram2tb_restart)
        self.wire(sram_only.ports.agg_read, agg_sram_shared.ports.agg_read_out)
        self.wire(sram_only.ports.t_read, sram_tb_shared.ports.t_read_out)
        self.wire(sram_only.ports.agg_data_out, agg_only.ports.agg_data_out)

        self.wire(tb_only.ports.t_read, sram_tb_shared.ports.t_read_out)
        self.wire(tb_only.ports.loops_sram2tb_mux_sel, sram_tb_shared.ports.loops_sram2tb_mux_sel)
        self.wire(tb_only.ports.loops_sram2tb_restart, sram_tb_shared.ports.loops_sram2tb_restart)

        if agg_data_top:
            self._agg_data_out = self.output(f"strg_ub_agg_data_out", self.data_width,
                                             size=(self.interconnect_input_ports,
                                                   self.fetch_width),
                                             packed=True,
                                             explicit_array=True)
            self.wire(self._agg_data_out, agg_only.ports.agg_data_out)


if __name__ == "__main__":
    lake_dut = StrgUBVec()
    verilog(lake_dut, filename="strg_ub_vec.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
