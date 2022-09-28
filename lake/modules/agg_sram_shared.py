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
                 area_opt=True,
                 addr_fifo_depth=4,
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
        self.area_opt = area_opt
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

        if self.area_opt:
            self._agg_write_restart_in = self.input("agg_write_restart_in", self.interconnect_input_ports)
            self._agg_write_in = self.input("agg_write_in", self.interconnect_input_ports)
            self._agg_write_addr_l2b_in = self.input("agg_write_addr_l2b_in", 2,
                                                     size=self.interconnect_input_ports,
                                                     packed=True,
                                                     explicit_array=True)
            self._agg_write_mux_sel_in = self.input("agg_write_mux_sel_in", max(clog2(self.default_iterator_support), 1),
                                                    size=self.interconnect_input_ports,
                                                    packed=True,
                                                    explicit_array=True)
            self._sram_read_in = self.input("sram_read_in", self.interconnect_input_ports)
            self._sram_read_d_in = self.input("sram_read_d_in", self.interconnect_input_ports)
            self._sram_read_addr_in = self.input("sram_read_addr_in", self.mem_addr_width,
                                                 size=self.interconnect_input_ports,
                                                 packed=True,
                                                 explicit_array=True)

            self._agg_sram_shared_addr_out = self.output("agg_sram_shared_addr_out", self.mem_addr_width,
                                                         size=self.interconnect_input_ports,
                                                         packed=True,
                                                         explicit_array=True)
            self._update_mode_out = self.output("update_mode_out", 2,
                                                size=self.interconnect_input_ports,
                                                packed=True,
                                                explicit_array=True)
        else:
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

            if self.area_opt:
                # linear or reuse mode configuration register
                self._mode = self.input(f"mode_{i}", 2)
                self._mode.add_attribute(ConfigRegAttr("Mode of agg_sram shared schedule or addressing"))
                self._mode.add_attribute(FormalAttr(f"{self._mode.name}_{i}", FormalSignalConstraint.SOLVE))
                self.wire(self._mode, self._update_mode_out[i])

                # scheduler modules
                shared_sg = AggSramSharedSchedGen(data_width=self.data_width,
                                                  mem_width=self.mem_width,
                                                  agg_range_width=self.agg_range_width,
                                                  addr_fifo_depth=self.addr_fifo_depth,
                                                  interconnect_input_ports=interconnect_input_ports,
                                                  config_width=self.config_width)
                self.add_child(f"agg_read_sched_gen_{i}",
                               shared_sg,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               agg_write_restart=self._agg_write_restart_in[i],
                               agg_write=self._agg_write_in[i],
                               agg_write_addr_l2b=self._agg_write_addr_l2b_in[i],
                               agg_write_mux_sel=self._agg_write_mux_sel_in[i],
                               sram_read_d=self._sram_read_d_in,
                               mode=self._mode,
                               valid_output=self._agg_read[i])

                # addr gen modules
                shared_ag = AggSramSharedAddrGen(height=self.mem_depth,
                                                 addr_fifo_depth=self.addr_fifo_depth,
                                                 interconnect_input_ports=interconnect_input_ports,
                                                 config_width=self.mem_addr_width)
                self.add_child(f"agg_sram_shared_addr_gen_{i}",
                               shared_ag,
                               clk=self._clk,
                               rst_n=self._rst_n,
                               step=self._agg_read[i],
                               sram_read=self._sram_read_in,
                               sram_read_addr=self._sram_read_addr_in,
                               mode=self._mode,
                               addr_out=self._agg_sram_shared_addr_out[i])

            else:
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
