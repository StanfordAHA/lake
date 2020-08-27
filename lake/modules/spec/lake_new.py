import kratos
from kratos import *
from lake.modules.sram_stub import SRAMStub
from lake.modules.addr_gen import AddrGen
from lake.modules.sched_gen import SchedGen
from lake.passes.passes import lift_config_reg


class LakeTestTop(Generator):
    def __init__(self,
                 data_width=16,
                 fetch_width=1,
                 mem_depth=512,
                 config_width=16,
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

        # memory module
        self.add_child(f"sram",
                       SRAMStub(data_width,
                                fetch_width,
                                mem_depth),
                       clk=self._clk,
                       wen=self._write,
                       cen=self._write | self._read,
                       addr=self._addr,
                       data_in=self._data_in,
                       data_out=self._data_out)

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

    @always_comb
    def set_sram_addr(self):
        if self._write:
            self._addr = self._write_addr[clog2(mem_depth) - 1, 0]
        else:
            self._addr = self._read_addr[clog2(mem_depth) - 1, 0]


if __name__ == "__main__":
    lake_dut = LakeTestTop()
    verilog(lake_dut, filename="lake_new.sv")
