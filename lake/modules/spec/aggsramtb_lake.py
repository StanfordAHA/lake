import kratos
from kratos import *
from lake.modules.sram_stub import SRAMStub
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
# from lake.modules.register_file import RegisterFile
from lake.passes.passes import lift_config_reg
from lake.attributes.config_reg_attr import ConfigRegAttr


class LakeTestTop(Generator):
    def __init__(self,
                 data_width=16,
                 fetch_width=4,
                 mem_depth=512,
                 config_width=9,
                 input_addr_iterator_support=6,
                 output_addr_iterator_support=6,
                 input_sched_iterator_support=6,
                 output_sched_iterator_support=6
                 ):

        super().__init__("lake_top_test")

        # generation parameters

        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._clk_en = self.input("clk_en", 1)
        self._flush = self.input("flush", 1)

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

        self._sram_write_data = self.var("sram_write_data", data_width, size=fetch_width, packed=True)
        self._sram_read_data = self.var("sram_read_data", data_width, size=fetch_width, packed=True)

#        self._aggw_start_addr = self.input("aggw_start_addr", 2)
#        self._aggw_start_addr.add_attribute(ConfigRegAttr("agg write start addr"))
#        self._agg_start_addr = self.input("agg_start_addr", 2)
#        self._agg_start_addr.add_attribute(ConfigRegAttr("agg read start addr"))

        self._agg_write_index = self.var("agg_write_index", 2, size=4)

        self._agg = self.var("agg",
                             width=data_width,
                             size=fetch_width,
                             packed=True)

        self.add_child(f"agg_write_addr_gen",
                       AddrGen(2,
                               2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._agg_write,
                       addr_out=self._agg_write_addr,
                       clk_en=self._clk_en,
                       flush=self._flush)

        self.add_child(f"agg_read_addr_gen",
                       AddrGen(2,
                               2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write,
                       addr_out=self._agg_read_addr,
                       clk_en=self._clk_en,
                       flush=self._flush)

        self.add_child(f"agg_write_sched_gen",
                       SchedGen(2,
                                2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       flush=self._flush,
                       valid_output=self._agg_write)

        self._tb = self.var("tb",
                            width=data_width,
                            size=fetch_width)

        self.add_child(f"tb_write_addr_gen",
                       AddrGen(2,
                               2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._read,
                       addr_out=self._tb_write_addr,
                       clk_en=self._clk_en,
                       flush=self._flush)

        self.add_child(f"tb_read_addr_gen",
                       AddrGen(2,
                               2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._tb_read,
                       addr_out=self._tb_read_addr,
                       clk_en=self._clk_en,
                       flush=self._flush)

        self.add_child(f"tb_read_sched_gen",
                       SchedGen(2,
                                2),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       flush=self._flush,
                       valid_output=self._tb_read)

        # memory module
        self.add_child(f"sram",
                       SRAMStub(data_width,
                                fetch_width,
                                mem_depth),
                       clk=self._clk,
                       wen=self._write,
                       cen=self._write | self._read,
                       addr=self._addr,
                       data_in=self._sram_write_data,
                       data_out=self._sram_read_data)

        # addressor modules
        self.add_child(f"input_addr_gen",
                       AddrGen(input_addr_iterator_support,
                               config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write,
                       addr_out=self._write_addr,
                       clk_en=self._clk_en,
                       flush=self._flush)

        self.add_child(f"output_addr_gen",
                       AddrGen(output_addr_iterator_support,
                               config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._read,
                       addr_out=self._read_addr,
                       clk_en=self._clk_en,
                       flush=self._flush)

        # scheduler modules
        self.add_child(f"input_sched_gen",
                       SchedGen(input_sched_iterator_support,
                                config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       flush=self._flush,
                       valid_output=self._write)

        self.add_child(f"output_sched_gen",
                       SchedGen(output_sched_iterator_support,
                                config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       flush=self._flush,
                       valid_output=self._read)

        lift_config_reg(self.internal_generator)

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

    @always_ff((posedge, "clk"))
    def agg_ctrl(self):
        if self._agg_write:
            self._agg[self._agg_write_addr] = self._data_in

    @always_comb
    def agg_to_sram(self):
        for i in range(fetch_width):
            self._sram_write_data[i] = self._agg[self._agg_read_addr]

    @always_ff((posedge, "clk"))
    def tb_ctrl(self):
        if self._read:
            for i in range(fetch_width):
                self._tb[self._tb_write_addr] = self._sram_read_data[i]

    @always_comb
    def tb_to_out(self):
        self._data_out = self._tb[self._tb_read_addr]


if __name__ == "__main__":
    lake_dut = LakeTestTop()

    verilog(lake_dut, filename="lake_new.sv")
