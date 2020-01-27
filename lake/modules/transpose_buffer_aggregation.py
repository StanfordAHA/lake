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
                 max_schedule_length,
                 max_stencil_height):
        super().__init__("transpose_buffer_aggregation", True)

        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_width
        self.num_tb = num_tb
        self.tb_height = tb_height
        self.max_range = max_range
        self.max_schedule_length = max_schedule_length
        self.max_stencil_height = max_stencil_height

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
                                  packed=True)

        self.tb_to_interconnect_data = self.output("self.tb_to_interconnect_data",
                                                   width=self.word_width,
                                                   size=self.tb_height,
                                                   packed=True)
        self.tb_to_interconnect_valid = self.output("self.tb_to_interconnect_valid", 1)

        # local variables
        self.valid_data_all = self.var("valid_data_all", 
                                       width=1, 
                                       size=self.num_tb,
                                       packed=True)

        self.tb_output_data_all = self.var("tb_output_data_all", width=self.word_width, size=[self.num_tb, self.tb_height], packed=True)
        self.tb_output_valid_all = self.var("tb_output_valid_all", width=1, size=self.num_tb, packed=True)
        self.output_valid = self.var("output_valid", 1)
 
        for i in range(self.num_tb):
            self.add_child(f"tb_{i}", 
                           TransposeBuffer(self.word_width, 
                                           self.fetch_width, 
                                           self.num_tb, 
                                           self.tb_height, 
                                           self.max_range,
                                           self.max_stencil_height))

            self.wire(self[f"tb_{i}"].ports.clk, self.clk)
            self.wire(self[f"tb_{i}"].ports.rst_n, self.rst_n)
            self.wire(self[f"tb_{i}"].ports.input_data, self.SRAM_to_tb_data)
            self.wire(self[f"tb_{i}"].ports.valid_data, self.valid_data_all[i])
            self.wire(self[f"tb_{i}"].ports.range_outer, self.range_outer)
            self.wire(self[f"tb_{i}"].ports.range_inner, self.range_inner)
            self.wire(self[f"tb_{i}"].ports.stride, self.stride)
            self.wire(self[f"tb_{i}"].ports.indices, self.indices)
            self.wire(self[f"tb_{i}"].ports.tb_start_index, self.num_tb*i)
            self.wire(self.tb_output_data_all[i], self[f"tb_{i}"].ports.col_pixels)
            self.wire(self.tb_output_valid_all[i], self[f"tb_{i}"].ports.output_valid)

        self.add_code(self.set_valid_data_all)
        self.set_output_valid()

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
            count = self.tb_output_valid_all[0]
            for i in range(1, self.num_tb):
                count = count + self.tb_output_valid_all[i]
                if self.tb_output_valid_all[i] == 1:
                    self.add_stmt(self.tb_to_interconnect_data.assign(self.tb_output_data_all[i]))
            self.add_stmt(self.tb_to_interconnect_valid.assign(count))

TransposeBufferAggregation(1,4,1,3,5,2,3)
