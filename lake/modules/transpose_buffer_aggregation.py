import kratos
from kratos import *
from math import log


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
                 # maximum stencil height supported from application
                 max_stencil_height):
        super().__init__("transpose_buffer_aggregation", True)

        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_width
        self.num_tb = num_tb
        self.tb_height = tb_height
        self.max_range = max_range
        self.max_stencil_height = max_stencil_height

        self.fetch_width_bits = max(1, clog2(self.fetch_width))
        self.num_tb_bits = max(1, clog2(self.num_tb))
        self.max_range_bits = max(1, clog2(self.max_range))
        self.tb_col_index_bits = 2 * max(self.fetch_width_bits, self.num_tb_bits) + 1
        self.tb_height_bits2 = clog2(2 * self.tb_height)

        # inputs
        self.clk = self.clock("clk")
        # active low asynchronous reset
        self.rst_n = self.reset("rst_n", 1)
        
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
            self.wire(self[f"tb_self.input_data"].ports

            self.valid_data = self.input("valid_data", 1)

        # the range of the outer for loop in nested for loop for output
        # column address generation
        self.range_outer = self.input("range_outer", self.max_range_bits)
        # the range of the inner for loop in nested for loop for output
        # column address generation
        self.range_inner = self.input("range_inner", self.max_range_bits)

        # stride for the given application
        self.stride = self.input("stride", self.max_range_bits)
        # stencil height for the given application
        self.stencil_height = self.input("stencil_height", clog2(self.max_stencil_height))

