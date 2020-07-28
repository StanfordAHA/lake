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
import kratos as kts


class AggFormal(Generator):
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
                 agg_height=4):
        super().__init__("strg_ub", debug=True)

        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.mem_depth = mem_depth

        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.increment_cycle_count)

        self._data_in = self.input("data_in", data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        self._agg_write = self.var("agg_write", self.interconnect_input_ports)

        # Make this based on the size
        self._agg_write_addr = self.var("agg_write_addr", 2 + clog2(self.agg_height),
                                        size=self.interconnect_input_ports,
                                        packed=True,
                                        explicit_array=True)
        self._agg_read_addr = self.var("agg_read_addr", max(1, clog2(self.agg_height)),
                                       size=self.interconnect_input_ports,
                                       packed=True,
                                       explicit_array=True)

        self.agg_rd_addr_gen_width = 8
        self._agg_read_addr_gen_out = self.var("agg_read_addr_gen_out", self.agg_rd_addr_gen_width,
                                               size=self.interconnect_input_ports,
                                               packed=True,
                                               explicit_array=True)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def increment_cycle_count(self):
        if ~self._rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1


if __name__ == "__main__":
    lake_dut = AggFormal()
    verilog(lake_dut, filename="agg_formal.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})