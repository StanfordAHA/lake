from kratos import *
from lake.modules.passthru import *
from lake.modules.register_file import RegisterFile
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.range_group import RangeGroupAttr
from lake.passes.passes import lift_config_reg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.modules.sram_stub import SRAMStub
from lake.modules.agg_sram_shared_sched_gen import AggSramSharedSchedGen
from lake.modules.agg_sram_shared_addr_gen import AggSramSharedAddrGen
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
                 addr_fifo_depth=8,
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
        self.addr_fifo_depth = addr_fifo_depth

        self.default_iterator_support = 6
        self.default_config_width = 16
        self.sram_iterator_support = 6
        self.agg_rd_addr_gen_width = 8
        self.mem_addr_width = clog2(self.mem_depth)

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._agg_write_in = self.input("agg_write_in", self.interconnect_input_ports)
        self._sram_read_in = self.input("sram_read_in", self.interconnect_input_ports)
        self._sram_read_addr_in = self.input("sram_read_addr_in", self.mem_addr_width,
                                             size=self.interconnect_input_ports,
                                             packed=True,
                                             explicit_array=True)

        self._agg_sram_shared_addr_out = self.output("agg_sram_shared_addr_out", self.mem_addr_width,
                                                     size=self.interconnect_input_ports,
                                                     packed=True,
                                                     explicit_array=True)

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

            # delay configuration register
            self._delay = self.input(f"delay_{i}", clog2(self.addr_fifo_depth))
            self._delay.add_attribute(ConfigRegAttr("Delay cycles of agg_sram shared schedule"))
            self._delay.add_attribute(FormalAttr(f"{self._delay.name}_{i}", FormalSignalConstraint.SOLVE))

            # linear or reuse mode configuration register
            self._mode = self.input(f"mode_{i}", 2)
            self._mode.add_attribute(ConfigRegAttr("Mode of agg_sram shared schedule or addressing"))
            self._mode.add_attribute(FormalAttr(f"{self._mode.name}_{i}", FormalSignalConstraint.SOLVE))

            # scheduler modules
            self.add_child(f"agg_read_sched_gen_{i}",
                           AggSramSharedSchedGen(data_width=self.data_width,
                                                 mem_width=self.mem_width,
                                                 agg_range_width=self.agg_range_width,
                                                 addr_fifo_depth=self.addr_fifo_depth,
                                                 interconnect_input_ports=interconnect_input_ports,
                                                 config_width=self.config_width),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           agg_write=self._agg_write_in[i],
                           sram_read=self._sram_read_in,
                           delay=self._delay,
                           mode=self._mode,
                           valid_output=self._agg_read[i])

            # scheduler modules
            self.add_child(f"agg_sram_shared_addr_gen_{i}",
                           AggSramSharedAddrGen(height=self.mem_depth,
                                                addr_fifo_depth=self.addr_fifo_depth,
                                                interconnect_input_ports=interconnect_input_ports,
                                                config_width=self.mem_addr_width),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_read[i],
                           sram_read=self._sram_read_in,
                           sram_read_addr=self._sram_read_addr_in,
                           mode=self._mode,
                           addr_out=self._agg_sram_shared_addr_out[i])


if __name__ == "__main__":
    lake_dut = StrgUBAggSRAMShared()
    verilog(lake_dut, filename="strg_ub_agg_sram_shared.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
