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
from lake.utils.util import safe_wire, add_counter, decode
import kratos as kts


class StrgUBSRAMOnly(Generator):
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

        super().__init__("strg_ub_sram_only")

        ##################################################################################
        # Capture constructor parameter...
        ##################################################################################
        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.mem_width = mem_width
        self.tb_height = tb_height
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

        # agg to sram for loop
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

        # sram to tb for loop
        self._loops_sram2tb_mux_sel = self.input("loops_sram2tb_mux_sel",
                                                 width=max(clog2(self.default_iterator_support), 1),
                                                 size=self.interconnect_output_ports,
                                                 explicit_array=True,
                                                 packed=True)

        self._loops_sram2tb_restart = self.input("loops_sram2tb_restart",
                                                 width=1,
                                                 size=self.interconnect_output_ports,
                                                 explicit_array=True,
                                                 packed=True)

        self._agg_read = self.input("agg_read", self.interconnect_input_ports)
        self._t_read = self.input("t_read", self.interconnect_output_ports)

        # data from aggs, get decoded for sram_write_data which is wired to data_to_sram
        self._agg_data_out = self.input(f"agg_data_out", self.data_width,
                                        size=(self.interconnect_input_ports,
                                              self.fetch_width),
                                        packed=True,
                                        explicit_array=True)
        self._agg_data_out.add_attribute(FormalAttr(self._agg_data_out.name, FormalSignalConstraint.SEQUENCE, "agg"))
        # sram attribute for data_in, comes from cut gen of agg_only for agg_data_out_top

        self._wen_to_sram = self.output("wen_to_sram", 1, packed=True)
        self._cen_to_sram = self.output("cen_to_sram", 1, packed=True)
        self._addr_to_sram = self.output("addr_to_sram", clog2(self.mem_depth), packed=True)
        self._data_to_sram = self.output("data_to_sram", self.data_width,
                                         size=self.fetch_width,
                                         packed=True)

        ##################################################################################
        # INTERNAL SIGNALS
        ##################################################################################
        self._s_write_addr = self.var("s_write_addr", self.config_width,
                                      size=self.interconnect_input_ports,
                                      packed=True,
                                      explicit_array=True)

        self._s_read_addr = self.var("s_read_addr",
                                     self.config_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        self._write = self.var("write", 1)
        self._read = self.var("read", 1)
        self._addr = self.var("addr", clog2(self.mem_depth))

        self._sram_write_data = self.var("sram_write_data", data_width,
                                         size=self.fetch_width,
                                         packed=True)

        self.mem_addr_width = clog2(self.mem_depth)

        for i in range(self.interconnect_input_ports):

            _AG = AddrGen(iterator_support=self.default_iterator_support,
                          config_width=self.mem_addr_width)
            self.add_child(f"input_addr_gen_{i}",
                           _AG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_read[i],
                           # mux_sel=self._floop_mux_sel[i],
                           restart=self._floop_restart[i])
            safe_wire(gen=self, w_to=_AG.ports.mux_sel, w_from=self._floop_mux_sel[i])
            safe_wire(gen=self, w_to=self._s_write_addr[i], w_from=_AG.ports.addr_out)

        ##################################################################################
        # TB PATHS
        ##################################################################################
        for i in range(self.interconnect_output_ports):

            _AG = AddrGen(iterator_support=self.default_iterator_support,
                          config_width=self.mem_addr_width)
            self.add_child(f"output_addr_gen_{i}",
                           _AG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._t_read[i],
                           # mux_sel=self._loops_sram2tb_mux_sel[i],
                           restart=self._loops_sram2tb_restart[i])
            safe_wire(gen=self, w_to=_AG.ports.mux_sel, w_from=self._loops_sram2tb_mux_sel[i])
            safe_wire(gen=self, w_to=self._s_read_addr[i], w_from=_AG.ports.addr_out)

        ##################################################################################
        # WIRE TO SRAM INTERFACE
        ##################################################################################
        # Now select the write address as a decode of the underlying enables
        self.wire(self._addr_to_sram, self._addr)
        self.wire(self._data_to_sram, self._sram_write_data)
        self.wire(self._wen_to_sram, self._write)
        self.wire(self._cen_to_sram, self._write | self._read)

        self.wire(self._write, self._agg_read.r_or())
        self.wire(self._read, self._t_read.r_or())

        self.wire(self._sram_write_data, decode(self, self._agg_read, self._agg_data_out))

        self._write_addr = decode(self, self._agg_read, self._s_write_addr)
        self._read_addr = decode(self, self._t_read, self._s_read_addr)
        self.add_code(self.set_sram_addr)

    @always_comb
    def set_sram_addr(self):
        if self._write:
            self._addr = self._write_addr[clog2(self.mem_depth) - 1, 0]
        else:
            self._addr = self._read_addr[clog2(self.mem_depth) - 1, 0]


if __name__ == "__main__":
    lake_dut = StrgUBSRAMOnly()
    verilog(lake_dut, filename="strg_ub_sram_only.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
