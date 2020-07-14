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
        # generation parameters
        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._clk_en = self.input("clk_en", 1)
        self._flush = self.reset("flush", is_async=False, active_high=True)

        self._data_in = self.input("data_in", data_width, packed=True)
        
        # outputs
        self._data_out = self.output("data_out", data_width, packed=True)

        # local variables
        self._write = self.var("write", 1)
        self._read = self.var("read", 1)
        self._write_addr = self.var("write_addr", config_width)
        self._read_addr = self.var("read_addr", config_width)
        self._addr = self.var("addr", clog2(mem_depth))

        self._agg_write = self.var("agg_write", 1)
        self._agg_write_addr = self.var("agg_write_addr", 2)
        self._agg_read_addr = self.var("agg_read_addr", 2)

        self._tb_read = self.var("tb_read", 1)
        self._tb_write_addr = self.var("tb_write_addr", 2)
        self._tb_read_addr = self.var("tb_read_addr", 2)

        self._sram_write_data = self.var("sram_write_data", data_width,
                                                            size=self.fetch_width,
                                                            packed=True)
        self._sram_read_data = self.var("sram_read_data", data_width,
                                                          size=self.fetch_width,
                                                          packed=True)

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
#        self._aggw_start_addr = self.input("aggw_start_addr", 2)
#        self._aggw_start_addr.add_attribute(ConfigRegAttr("agg write start addr"))
#        self._agg_start_addr = self.input("agg_start_addr", 2)
#        self._agg_start_addr.add_attribute(ConfigRegAttr("agg read start addr"))

        self._agg_write_index = self.var("agg_write_index", 2, size=4)

        self._agg = self.var("agg",
                             width=data_width,
                             size=self.fetch_width,
                             packed=True)

        self.add_child(f"agg_write_addr_gen",
                       AddrGen(2,
                               2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._agg_write,
                       addr_out=self._agg_write_addr)

        self.add_child(f"agg_read_addr_gen",
                       AddrGen(2,
                               2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write,
                       addr_out=self._agg_read_addr)

        self.add_child(f"agg_write_sched_gen",
                       SchedGen(2,
                                2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       valid_output=self._agg_write)

        self._tb = self.var("tb",
                            width=data_width,
                            size=self.fetch_width)

        self.add_child(f"tb_write_addr_gen",
                       AddrGen(2,
                               2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._read,
                       addr_out=self._tb_write_addr)

        self.add_child(f"tb_read_addr_gen",
                       AddrGen(2,
                               2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._tb_read,
                       addr_out=self._tb_read_addr)

        self.add_child(f"tb_read_sched_gen",
                       SchedGen(2,
                                2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       valid_output=self._tb_read)

        # addressor modules
        self.add_child(f"input_addr_gen",
                       AddrGen(input_addr_iterator_support,
                               config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write,
                       addr_out=self._write_addr)

        self.add_child(f"output_addr_gen",
                       AddrGen(output_addr_iterator_support,
                               config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._read,
                       addr_out=self._read_addr)

        # scheduler modules
        self.add_child(f"input_sched_gen",
                       SchedGen(input_sched_iterator_support,
                                config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       valid_output=self._write)

        self.add_child(f"output_sched_gen",
                       SchedGen(output_sched_iterator_support,
                                config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       valid_output=self._read)


        # lift_config_reg(self.internal_generator)

        self.add_code(self.set_sram_addr)
        self.add_code(self.agg_ctrl)
        self.add_code(self.tb_ctrl)
        self.add_code(self.agg_to_sram)
        self.add_code(self.tb_to_out)
    
    @always_comb
    def set_sram_addr(self):
        if self._write:
            self._addr = self._write_addr[clog2(mem_depth) - 1, 0]
        else:
            self._addr = self._read_addr[clog2(mem_depth) - 1, 0]

    #    for i in range(fetch_width):
    #        self._agg_write_index[i] = self._agg_write_addr + self._aggw_start_addr[i]

    @always_ff((posedge, "clk"))
    def agg_ctrl(self):
        if self._agg_write:
     #       for i in range(fetch_width):
            self._agg[self._agg_write_addr] = self._data_in

    @always_comb
    def agg_to_sram(self):
        for i in range(self.fetch_width):
            self._sram_write_data[i] = self._agg[self._agg_read_addr]

    @always_ff((posedge, "clk"))
    def tb_ctrl(self):
        if self._read:
            for i in range(self.fetch_width):
                self._tb[self._tb_write_addr] = self._sram_read_data[i]

    @always_comb
    def tb_to_out(self):
        self._data_out = self._tb[self._tb_read_addr]


if __name__ == "__main__":
    lake_dut = StrgUB()
    verilog(lake_dut, filename="lake_top.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
