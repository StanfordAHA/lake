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
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.utils.util import extract_formal_annotation, safe_wire
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
                 interconnect_input_ports=1,  # Connection to int
                 interconnect_output_ports=1,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4):
        super().__init__("agg_formal", debug=True)

        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.mem_depth = mem_depth

        self.default_iterator_support = 6
        self.default_config_width = 16

        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))

        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.increment_cycle_count)

        self._data_in = self.input("data_in", data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)
        self._data_in.add_attribute(FormalAttr(f"{self._data_in.name}", FormalSignalConstraint.SEQUENCE))

        self._agg_write = self.var("agg_write", self.interconnect_input_ports)
        self._valid_in = self.output("valid_in", self.interconnect_input_ports)
        self._valid_in.add_attribute(FormalAttr(f"{self._valid_in.name}", FormalSignalConstraint.SEQUENCE))
        self.wire(self._valid_in, self._agg_write)

        self._write = self.var("write", 1)
        self._valid_out = self.output("valid_out", 1)
        self._valid_out.add_attribute(FormalAttr(f"{self._valid_out.name}", FormalSignalConstraint.SEQUENCE))
        self.wire(self._write, self._valid_out)

        self._data_out = self.output("data_out", data_width,
                                     size=self.fetch_width,
                                     packed=True)
        self._data_out.add_attribute(FormalAttr(f"{self._data_out.name}", FormalSignalConstraint.SEQUENCE))

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
        self._input_port_sel_addr = self.var("input_port_sel_addr",
                                             max(1, clog2(self.interconnect_input_ports)))
        # Create an input to agg write scheduler + addressor for each input
        # Also need an addressor for the mux in addition to the read addr
        self._agg = self.var(f"agg",
                             width=data_width,
                             size=(self.interconnect_input_ports,
                                   self.agg_height,
                                   self.fetch_width),
                             packed=True,
                             explicit_array=True)

        output_loops = None

        for i in range(self.interconnect_input_ports):

            forloop_ctr = ForLoop(iterator_support=self.default_iterator_support,
                                  config_width=self.default_config_width)
            loop_itr = forloop_ctr.get_iter()
            loop_wth = forloop_ctr.get_cfg_width()

            self.add_child(f"agg_write_loops_{i}",
                           forloop_ctr,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_write[i])

            newAG = AddrGen(iterator_support=self.default_iterator_support,
                            config_width=self.default_config_width)
            self.add_child(f"agg_write_addr_gen_{i}",
                           newAG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_write[i],
                           # addr_out=self._agg_write_addr[i])
                           mux_sel=forloop_ctr.ports.mux_sel_out,
                           restart=forloop_ctr.ports.restart)
            safe_wire(self, self._agg_write_addr[i], newAG.ports.addr_out)

            newSG = SchedGen(iterator_support=self.default_iterator_support,
                             config_width=self.default_config_width)
            self.add_child(f"agg_write_sched_gen_{i}",
                           newSG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           mux_sel=forloop_ctr.ports.mux_sel_out,
                           finished=forloop_ctr.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self._agg_write[i])

            forloop_ctr_rd = ForLoop(iterator_support=self.default_iterator_support,
                                     config_width=self.default_config_width)
            loop_itr = forloop_ctr_rd.get_iter()
            loop_wth = forloop_ctr_rd.get_cfg_width()

            # Add loops for the output of each agg...
            self.add_child(f"agg_read_loops_{i}",
                           forloop_ctr_rd,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           # (self._input_port_sel_addr == const(i, self._input_port_sel_addr.width))))
                           step=self._write)

            output_loops = forloop_ctr_rd

            # And an associated read address...
            newAG = AddrGen(iterator_support=self.default_iterator_support,
                            config_width=self.default_config_width)
            self.add_child(f"agg_read_addr_gen_{i}",
                           newAG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._write,
                           #  (self._input_port_sel_addr == const(i, self._input_port_sel_addr.width))),
                           # addr_out=self._agg_read_addr_gen_out[i])
                           mux_sel=forloop_ctr_rd.ports.mux_sel_out,
                           restart=forloop_ctr_rd.ports.restart)

            safe_wire(self, self._agg_read_addr_gen_out[i], newAG.ports.addr_out)
            self.wire(self._agg_read_addr[i], self._agg_read_addr_gen_out[i][self._agg_read_addr.width - 1, 0])

        # Now we determine what data goes through to the sram...
        # If we have more than one port, we can generate a selector
        # to pick which input port should go through - then we send
        # the step signal to the appropriate input port
        if self.interconnect_input_ports > 1:

            # Create for loop counters that can be shared across the input port selection and SRAM write
            fl_ctr_sram_wr = ForLoop(iterator_support=self.default_iterator_support,
                                     config_width=self.default_config_width)
            loop_itr = fl_ctr_sram_wr.get_iter()
            loop_wth = fl_ctr_sram_wr.get_cfg_width()

            output_loops = fl_ctr_sram_wr

            self.add_child(f"agg_select_loops",
                           fl_ctr_sram_wr,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._write)

            tmp_AG = AddrGen(iterator_support=self.default_iterator_support,
                             # config_width=clog2(self.interconnect_input_ports)),
                             config_width=self.default_config_width)
            self.add_child(f"port_sel_addr",
                           tmp_AG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._write,
                           # addr_out=self._input_port_sel_addr)
                           mux_sel=fl_ctr_sram_wr.ports.mux_sel_out)
            safe_wire(self, self._input_port_sel_addr, tmp_AG.ports.addr_out)

        else:
            self.wire(self._input_port_sel_addr[0], const(0, self._input_port_sel_addr.width))

        # Addr for port select should be driven on agg to sram write sched
        # scheduler modules
        self.add_child(f"agg_read_output_sched_gen",
                       SchedGen(iterator_support=self.default_iterator_support,
                                config_width=self.default_config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       mux_sel=output_loops.ports.mux_sel_out,
                       finished=output_loops.ports.restart,
                       valid_output=self._write)

        for idx in range(self.interconnect_input_ports):
            self.add_code(self.agg_ctrl, idx=idx)
        self.add_code(self.agg_to_sram)

    @always_ff((posedge, "clk"))
    def agg_ctrl(self, idx):
        if self._agg_write[idx]:
            if self.agg_height == 1:
                self._agg[idx][0][self._agg_write_addr[idx][clog2(self.fetch_width) - 1, 0]]\
                    = self._data_in[idx]
            else:
                self._agg[idx][self._agg_write_addr[idx]
                               [self._agg_write_addr[0].width - 1, clog2(self.fetch_width)]]\
                    [self._agg_write_addr[idx][clog2(self.fetch_width) - 1, 0]]\
                    = self._data_in[idx]

    @always_comb
    def agg_to_sram(self):
        for i in range(self.fetch_width):
            self._data_out[i] = \
                self._agg[self._input_port_sel_addr][self._agg_read_addr[self._input_port_sel_addr]][i]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def increment_cycle_count(self):
        if ~self._rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1


if __name__ == "__main__":
    lake_dut = AggFormal()

    lift_config_reg(lake_dut.internal_generator)
    extract_formal_annotation(lake_dut, "agg_formal_annotation.txt")

    verilog(lake_dut, filename="agg_formal.sv",
            optimize_if=False)
