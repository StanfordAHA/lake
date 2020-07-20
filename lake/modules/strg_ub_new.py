from kratos import *
from lake.modules.passthru import *
from lake.modules.aggregation_buffer import AggregationBuffer
from lake.modules.input_addr_ctrl import InputAddrCtrl
from lake.modules.output_addr_ctrl import OutputAddrCtrl
from lake.modules.agg_aligner import AggAligner
from lake.modules.rw_arbiter import RWArbiter
from lake.modules.transpose_buffer import TransposeBuffer
from lake.modules.transpose_buffer_aggregation import TransposeBufferAggregation
from lake.modules.demux_reads import DemuxReads
from lake.modules.sync_groups import SyncGroups
from lake.modules.prefetcher import Prefetcher
from lake.modules.register_file import RegisterFile
from lake.modules.strg_fifo import StrgFIFO
from lake.modules.strg_RAM import StrgRAM
from lake.modules.app_ctrl import AppCtrl
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.modules.sram_stub import SRAMStub
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
import kratos as kts


class StrgUB(Generator):
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
                 max_agg_schedule=32,
                 input_max_port_sched=32,
                 output_max_port_sched=32,
                 align_input=1,
                 max_line_length=128,
                 max_tb_height=1,
                 tb_range_max=128,
                 tb_range_inner_max=5,
                 tb_sched_max=64,
                 max_tb_stride=15,
                 num_tb=1,
                 tb_iterator_support=2,
                 multiwrite=1,
                 num_tiles=1,
                 max_prefetch=8,
                 app_ctrl_depth_width=16,
                 remove_tb=False,
                 stcl_valid_iter=4):
        super().__init__("strg_ub", debug=True)

        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.mem_depth = mem_depth
        # generation parameters
        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._clk_en = self.input("clk_en", 1)
        self._flush = self.reset("flush", is_async=False, active_high=True)

        self._data_in = self.input("data_in", data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        # Create cycle counter to share...
        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.increment_cycle_count)

        # outputs
        self._data_out = self.output("data_out", data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        # local variables
        self._write = self.var("write", 1)
        self._read = self.var("read", 1)
        self._read_d1 = self.var("read_d1", 1)
        self.add_code(self.delay_read)

        self._write_addr = self.var("write_addr", config_width)
        self._read_addr = self.var("read_addr", config_width)
        self._addr = self.var("addr", clog2(mem_depth))

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

        self._sram_write_data = self.var("sram_write_data", data_width,
                                         size=self.fetch_width,
                                         packed=True)
        self._sram_read_data = self.var("sram_read_data", data_width,
                                        size=self.fetch_width,
                                        packed=True,
                                        explicit_array=True)

        self._data_to_sram = self.output("data_to_strg", data_width,
                                         size=self.fetch_width,
                                         packed=True)
        self._data_from_sram = self.input("data_from_strg", data_width,
                                          size=self.fetch_width,
                                          packed=True)

        self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
        self._cen_to_sram = self.output("cen_to_strg", 1, packed=True)

        self._addr_to_sram = self.output("addr_out", clog2(mem_depth), packed=True)

        self.wire(self._addr_to_sram, self._addr)
        self.wire(self._data_to_sram, self._sram_write_data)
        self.wire(self._data_from_sram, self._sram_read_data)
        self.wire(self._wen_to_sram, self._write)
        self.wire(self._cen_to_sram, self._write | self._read)

        self._agg_write_index = self.var("agg_write_index", 2, size=4)

        self._output_port_sel_addr = self.var("output_port_sel_addr",
                                             max(1, clog2(self.interconnect_output_ports)))

        self.agg_write_scheds = []
        self.agg_read_addrs = []
        self._input_port_sel_addr = self.var("input_port_sel_addr",
                                             max(1, clog2(self.interconnect_input_ports)))
        # agg_to_sram_data_sel = self.var("agg_to_sra")
        # Create an input to agg write scheduler + addressor for each input
        # Also need an addressor for the mux in addition to the read addr

        self._agg = self.var(f"agg",
                             width=data_width,
                             size=(self.interconnect_input_ports,
                                   self.agg_height,
                                   self.fetch_width),
                             packed=True,
                             explicit_array=True)

        for i in range(self.interconnect_input_ports):

            newAG = AddrGen(iterator_support=4,
                            config_width=self._agg_write_addr.width)
            self.add_child(f"agg_write_addr_gen_{i}",
                           newAG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_write[i],
                           addr_out=self._agg_write_addr[i])

            newSG = SchedGen(iterator_support=6,
                             config_width=16)
            self.agg_write_scheds.append(newSG)
            self.add_child(f"agg_write_sched_gen_{i}",
                           newSG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           valid_output=self._agg_write[i])

            newAG = AddrGen(iterator_support=4,
                            config_width=self.agg_rd_addr_gen_width)
            self.agg_read_addrs.append(newAG)
            self.add_child(f"agg_read_addr_gen_{i}",
                           newAG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=(self._write &
                                 (self._input_port_sel_addr == const(i, self._input_port_sel_addr.width))),
                           addr_out=self._agg_read_addr_gen_out[i])
            self.wire(self._agg_read_addr[i], self._agg_read_addr_gen_out[i][self._agg_read_addr.width - 1, 0])

        # Now we determine what data goes through to the sram...
        # If we have more than one port, we can generate a selector
        # to pick which input port should go through - then we send
        # the step signal to the appropriate input port
        if self.interconnect_input_ports > 1:
            self.add_child(f"port_sel_addr",
                           AddrGen(2,
                                   clog2(self.interconnect_input_ports)),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._write,
                           addr_out=self._input_port_sel_addr)
            # Addr for port select should be driven on agg to sram write sched
        else:
            self.wire(self._input_port_sel_addr[0], const(0, self._input_port_sel_addr.width))

        # Whatever comes through here should hopefully just pipe through seamlessly
        # addressor modules
        self.add_child(f"input_addr_gen",
                       AddrGen(input_addr_iterator_support,
                               config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write,
                       addr_out=self._write_addr)

        # scheduler modules
        self.add_child(f"input_sched_gen",
                       SchedGen(input_sched_iterator_support,
                                config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       valid_output=self._write)

        self.add_child(f"output_addr_gen",
                       AddrGen(output_addr_iterator_support,
                               config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._read,
                       addr_out=self._read_addr)

        self.add_child(f"output_sched_gen",
                       SchedGen(output_sched_iterator_support,
                                config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       valid_output=self._read)

        self._tb_read = self.var("tb_read", self.interconnect_output_ports)
        self.tb_height = 4

        self._tb_write_addr = self.var("tb_write_addr", 6,
                                        size=self.interconnect_output_ports,
                                        packed=True,
                                        explicit_array=True)
        self._tb_read_addr = self.var("tb_read_addr", 6,
                                       size=self.interconnect_output_ports,
                                       packed=True,
                                       explicit_array=True)

        self._tb = self.var("tb",
                            width=data_width,
                            size=(self.interconnect_output_ports,
                                  self.tb_height,
                                  self.fetch_width),
                            packed=True,
                            explicit_array=True)

        for i in range(self.interconnect_output_ports):

            self.add_child(f"tb_write_addr_gen_{i}",
                           AddrGen(iterator_support=2,
                                   config_width=6),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._read_d1 & (self._output_port_sel_addr ==
                                                 const(i, self._output_port_sel_addr.width)),
                           #    step=self._read,
                           addr_out=self._tb_write_addr[i])

            self.add_child(f"tb_read_addr_gen_{i}",
                           AddrGen(iterator_support=2,
                                   config_width=6),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._tb_read[i],
                           addr_out=self._tb_read_addr[i])

            self.add_child(f"tb_read_sched_gen_{i}",
                           SchedGen(iterator_support=6,
                                    config_width=16),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           valid_output=self._tb_read[i])

        if self.interconnect_output_ports > 1:
            self.add_child(f"out_port_sel_addr",
                           AddrGen(2,
                                   clog2(self.interconnect_output_ports)),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._read_d1,
                           addr_out=self._output_port_sel_addr)
            # Addr for port select should be driven on agg to sram write sched
        else:
            self.wire(self._output_port_sel_addr[0], const(0, self._output_port_sel_addr.width))

        # lift_config_reg(self.internal_generator)

        self.add_code(self.set_sram_addr)
        for idx in range(self.interconnect_input_ports):
            self.add_code(self.agg_ctrl, idx=idx)

        self.add_code(self.agg_to_sram)
        self.add_code(self.tb_ctrl)

        for idx in range(self.interconnect_output_ports):
            self.add_code(self.tb_to_out, idx=idx)

    @always_comb
    def set_sram_addr(self):
        if self._write:
            self._addr = self._write_addr[clog2(self.mem_depth) - 1, 0]
        else:
            self._addr = self._read_addr[clog2(self.mem_depth) - 1, 0]

    @always_ff((posedge, "clk"))
    def agg_ctrl(self, idx):
        if self._agg_write[idx]:
            self._agg[idx][self._agg_write_addr[idx][self._agg_write_addr[0].width - 1, 2]]\
                          [self._agg_write_addr[idx][1, 0]] = self._data_in[idx]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def delay_read(self):
        if ~self._rst_n:
            self._read_d1 = 0
        else:
            self._read_d1 = self._read

    @always_comb
    def agg_to_sram(self):
        for i in range(self.fetch_width):
            self._sram_write_data[i] = self._agg[self._input_port_sel_addr][self._agg_read_addr[self._input_port_sel_addr]][i]

    @always_ff((posedge, "clk"))
    def tb_ctrl(self):
        if self._read_d1:
            self._tb[self._output_port_sel_addr][self._tb_write_addr[self._output_port_sel_addr][1,0]] = self._sram_read_data

    @always_comb
    def tb_to_out(self, idx):
        self._data_out[idx] = self._tb[idx][self._tb_read_addr[idx][3, 2]][self._tb_read_addr[idx][1, 0]]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def increment_cycle_count(self):
        if ~self._rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1

if __name__ == "__main__":
    lake_dut = StrgUB()
    verilog(lake_dut, filename="strg_ub_new.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
