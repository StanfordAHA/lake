import kratos
from kratos import *
from math import log
from lake.modules.transpose_buffer import TransposeBuffer
from lake.passes.passes import lift_config_reg


class TransposeBufferAggregation(Generator):
    def __init__(self,
                 # number of bits in a word
                 word_width,
                 # number of words that can be sotred at an address in SRAM
                 # note fetch_width must be powers of 2
                 fetch_width,
                 # total number of transpose buffers
                 num_tb,
                 # height of this particular transpose buffer
                 max_tb_height,
                 # maximum value for range parameters in nested for loop
                 # (and as a result, maximum length of indices input vector)
                 # specifying inner for loop values for output column
                 # addressing
                 max_range,
                 max_range_inner,
                 max_stride,
                 tb_iterator_support):
        super().__init__("transpose_buffer_aggregation")

        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_width
        self.num_tb = num_tb
        self.max_tb_height = max_tb_height
        self.max_range = max_range
        self.max_range_inner = max_range_inner
        self.max_stride = max_stride
        self.tb_iterator_support = tb_iterator_support

        self.num_tb_bits = max(1, clog2(self.num_tb))

        # inputs
        self.clk = self.clock("clk")
        # active low asynchronous reset
        self.rst_n = self.reset("rst_n", 1)
        self.SRAM_to_tb_data = self.input("SRAM_to_tb_data",
                                          width=self.word_width,
                                          size=self.fetch_width,
                                          packed=True)

        self.valid_data = self.input("valid_data", 1)
        self.tb_index_for_data = self.input("tb_index_for_data", self.num_tb_bits)

        self.tba_ren = self.input("tba_ren", 1)

        # Ack the ready
        self._ack_in = self.input("ack_in", 1)

        self.mem_valid_data = self.input("mem_valid_data", 1)

        # outputs
        self.tb_to_interconnect_data = self.output("tb_to_interconnect_data",
                                                   width=self.word_width,
                                                   size=self.max_tb_height,
                                                   packed=True)
        self.tb_to_interconnect_valid = self.output("tb_to_interconnect_valid", 1)
        self.tb_arbiter_rdy = self.output("tb_arbiter_rdy", 1)

        # local variables
        self.valid_data_all = self.var("valid_data_all",
                                       width=1,
                                       size=self.num_tb,
                                       packed=True)

        self.tb_output_data_all = self.var("tb_output_data_all",
                                           width=self.word_width,
                                           size=[self.num_tb, self.max_tb_height],
                                           packed=True)
        self.tb_output_valid_all = self.var("tb_output_valid_all", width=1, size=self.num_tb, packed=True)
        self.tb_arbiter_rdy_all = self.var("tb_arbiter_rdy_all", width=1, size=self.num_tb, packed=True)
        self.output_valid = self.var("output_valid", 1)
        self.output_inter = self.var("output_inter",
                                     width=self.word_width,
                                     size=self.max_tb_height,
                                     packed=True)

        for i in range(self.num_tb):
            self.add_child(f"tb_{i}",
                           TransposeBuffer(self.word_width,
                                           self.fetch_width,
                                           self.num_tb,
                                           self.max_tb_height,
                                           self.max_range,
                                           self.max_range_inner,
                                           self.max_stride,
                                           self.tb_iterator_support),
                           clk=self.clk,
                           rst_n=self.rst_n,
                           input_data=self.SRAM_to_tb_data,
                           valid_data=self.valid_data_all[i],
                           col_pixels=self.tb_output_data_all[i],
                           output_valid=self.tb_output_valid_all[i],
                           rdy_to_arbiter=self.tb_arbiter_rdy_all[i],
                           ack_in=self._ack_in,
                           mem_valid_data=self.mem_valid_data,
                           ren=self.tba_ren)

        self.add_code(self.set_valid_data_all)
        self.set_output_valid()
        self.send_tba_rdy()

    @always_comb
    def set_valid_data_all(self):
        if ~self.valid_data:
            for i in range(self.num_tb):
                self.valid_data_all[i] = 0
        else:
            for i in range(self.num_tb):
                if i == self.tb_index_for_data:
                    self.valid_data_all[i] = 1
                else:
                    self.valid_data_all[i] = 0

    def set_output_valid(self):
        comb_output = self.combinational()
        valid_count = self.tb_output_valid_all[0]
        comb_output.add_stmt(self.output_inter.assign(self.tb_output_data_all[0]))
        for i in range(1, self.num_tb):
            valid_count = valid_count + self.tb_output_valid_all[i]
            if_vld = IfStmt(self.tb_output_valid_all[i] == 1)
            if_vld.then_(self.output_inter.assign(self.tb_output_data_all[i]))
            comb_output.add_stmt(if_vld)
        if_vld_count = IfStmt(valid_count > 0)
        if_vld_count.then_(self.tb_to_interconnect_valid.assign(1))
        if_vld_count.else_(self.tb_to_interconnect_valid.assign(0))
        comb_output.add_stmt(self.tb_to_interconnect_data.assign(self.output_inter))
        comb_output.add_stmt(if_vld_count)

    def send_tba_rdy(self):
        comb_tb_arbiter_rdy = self.combinational()
        rdy_count = self.tb_arbiter_rdy_all[0]
        for i in range(1, self.num_tb):
            rdy_count = rdy_count + self.tb_arbiter_rdy_all[i]
        if_rdy_count = IfStmt(rdy_count > 0)
        if_rdy_count.then_(self.tb_arbiter_rdy.assign(1))
        if_rdy_count.else_(self.tb_arbiter_rdy.assign(0))
        comb_tb_arbiter_rdy.add_stmt(if_rdy_count)


if __name__ == "__main__":
    dut = TransposeBufferAggregation(word_width=1,
                                     fetch_width=4,
                                     num_tb=3,
                                     max_tb_height=1,
                                     max_range=5,
                                     max_range_inner=3,
                                     max_stride=2,
                                     tb_iterator_support=2)
    verilog(dut, filename="tba.sv", additional_passes={"lift config regs": lift_config_reg})
