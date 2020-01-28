import kratos
from kratos import *
from math import log
from lake.modules.transpose_buffer import TransposeBuffer


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
                 tb_height,
                 # maximum value for range parameters in nested for loop
                 # (and as a result, maximum length of indices input vector)
                 # specifying inner for loop values for output column
                 # addressing
                 max_range,
                 max_schedule_length):
        super().__init__("transpose_buffer_aggregation", True)

        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_width
        self.num_tb = num_tb
        self.tb_height = tb_height
        self.max_range = max_range
        self.max_schedule_length = max_schedule_length

        self.num_tb_bits = max(1, clog2(self.num_tb))
        self.max_range_bits = max(1, clog2(self.max_range))

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
        self.range_outer = self.input("range_outer", clog2(self.max_range))
        self.range_inner = self.input("range_inner", clog2(self.max_range))
        self.stride = self.input("stride", clog2(self.max_range))
        self.indices = self.input("indices",
                                  width=clog2(2 * self.num_tb * self.fetch_width),
                                  # the length of indices is equal to range_inner,
                                  # so the maximum possible size for self.indices
                                  # is the maximum value of range_inner, which if
                                  # self.max_range_value
                                  size=self.max_range,
                                  packed=True,
                                  explicit_array=True)

        self.tb_to_interconnect_data = self.output("tb_to_interconnect_data",
                                                   width=self.word_width,
                                                   size=self.tb_height,
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
                                           size=[self.num_tb, self.tb_height],
                                           packed=True)
        self.tb_output_valid_all = self.var("tb_output_valid_all", width=1, size=self.num_tb, packed=True)
        self.tb_arbiter_rdy_all = self.var("tb_arbiter_rdy_all", width=1, size=self.num_tb, packed=True)
        self.output_valid = self.var("output_valid", 1)
        self.output_inter = self.var("output_inter", width=self.word_width, size=self.tb_height, packed=True)

        for i in range(self.num_tb):
            self.add_child(f"tb_{i}",
                           TransposeBuffer(self.word_width,
                                           self.fetch_width,
                                           self.num_tb,
                                           self.tb_height,
                                           self.max_range),
                           clk=self.clk, rst_n=self.rst_n, input_data=self.SRAM_to_tb_data,
                           valid_data=self.valid_data_all[i], range_outer=self.range_outer,
                           range_inner=self.range_inner, stride=self.stride, indices=self.indices,
                           tb_start_index=self.num_tb * i, col_pixels=self.tb_output_data_all[i],
                           output_valid=self.tb_output_valid_all[i],
                           rdy_to_arbiter=self.tb_arbiter_rdy_all[i])

        self.add_code(self.set_valid_data_all)
        self.set_output_valid()
        self.send_tba_rdy()

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
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
    dut = TransposeBufferAggregation(1, 4, 3, 3, 5, 2)
    verilog(dut, filename="tba.sv")
