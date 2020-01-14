import kratos
from kratos import *
from math import log

class TransposeBuffer(Generator):
    # note fetch_width must be powers of 2 for mod to work (synthesizable) **for now
    def __init__(self, word_width, fetch_width, stencil_height, max_range_value, img_height):
        super().__init__("transpose_buffer", True)
        
        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_width
        self.stencil_height = stencil_height
        self.max_range_value = max_range_value
        self.img_height = img_height

        # inputs
        self.clk = self.clock("clk")
        # active low asynchronous reset
        self.rst_n = self.reset("rst_n", 1)
        self.input_data = self.input("input_data", width=self.word_width*self.fetch_width, packed=True)
        self.range_outer = self.input("range_outer", clog2(self.max_range_value))
        self.range_inner = self.input("range_inner", clog2(self.max_range_value))
        self.stride = self.input("stride", clog2(self.max_range_value))
#        self.indices = self.input("indices", width=clog2(self.fetch_width), size=self.max_range_value, packed=True)

        # outputs
#        self.col_pixels = self.output("col_pixels", width=self.word_width, size=self.stencil_height, packed=True)
#        self.output_valid = self.output("output_valid", 1)

        # local variables

        self.index_outer = self.var("index_outer", clog2(self.max_range_value))
        self.index_inner = self.var("index_inner", clog2(self.max_range_value))

        self.tb = self.var("tb", width=self.word_width*2*self.stencil_height*self.fetch_width, packed=True)
        self.row_index = self.var("row_index", width=clog2(2*stencil_height))
        self.add_code(self.get_loop_iterators)
        self.add_code(self.input_tb)
        self.add_code(self.update_indices)

    # get loop indices
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def get_loop_iterators(self):
        if ~self.rst_n:
            self.index_outer = 0
            self.index_inner = 0
        else:
            if self.index_outer == self.range_outer - 1:
                self.index_outer = 0
            else:
                self.index_outer = self.index_outer + 1

            if self.index_inner == self.range_inner - 1:
                self.index_inner = 0
            else:
                self.index_inner = self.index_inner + 1

    # input to transpose buffer
    @always_ff((posedge, "clk"))
    def input_tb(self):
        self.tb[(self.row_index + 1)*self.fetch_width*self.word_width - 1, self.row_index*self.fetch_width*self.word_width] = self.input_data

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_indices(self):
        if (~self.rst_n) | (self.row_index == (2*self.stencil_height - 1)):
            self.row_index = 0
        else:
            self.row_index = self.row_index + 1

