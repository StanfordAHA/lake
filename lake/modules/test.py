import kratos
from kratos import *
from math import log


class TransposeBuffer(Generator):
    # note fetch_width must be powers of 2 for mod to work (synthesizable) **for now
    def __init__(self, word_width, fetch_width, stencil_height, max_range_value, img_height):
        super().__init__("transpose_buffer", True)

        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_widt
        self.stencil_height = stencil_height
        self.max_range_value = max_range_value
        self.img_height = img_height

        # inputs
        self.clk = self.clock("clk")
        # active low asynchronous reset
        self.rst_n = self.reset("rst_n", 1)
        self.input_data = self.input("input_data",
                                     width=self.word_width,
                                     size=self.fetch_width,
                                     packed=True)
        self.range_outer = self.input("range_outer", clog2(self.max_range_value))
        self.range_inner = self.input("range_inner", clog2(self.max_range_value))
        self.stride = self.input("stride", clog2(self.max_range_value))
        # self.indices = self.input("indices",
#                                   width=clog2(self.fetch_width),
#                                   size=self.max_range_value, packed=True)

        # outputs
#        self.col_pixels = self.output("col_pixels",
# width=self.word_width, size=self.stencil_height, packed=True)
#        self.output_valid = self.output("output_valid", 1)

        # local variables
        self.tb = self.var("tb", width=self.word_width,
                           size=[2 * self.stencil_height, self.fetch_width],
                           packed=True)
        self.buf_index = self.var("buf_index", 1)

        self.index_outer = self.output("index_outer", clog2(self.max_range_value))
        self.index_inner = self.output("index_inner", clog2(self.max_range_value))
        self.index = self.var("index", clog2(2 * stencil_height))

        self.add_code(self.get_loop_iterators)

    # input to transpose buffer
    @always_ff((posedge, "clk"))
    def input_tb(self):
        for i in range(mem_word_width):
            self.tb[self.input_index][i] = self.input_data[i]

    @always_comb
    def get_tb_input_index(self):
        self.input_index = const(stencil_height, clog2(2 * stencil_height)) * \
            self.buf_index.extend(clog2(2 * stencil_height)) + \
            self.row_index.extend(clog2(2 * stencil_height)) + 1
        if self.index == 2 * stencil_height:
            self.input_index = 0

    # generate output valid
        # check for end of line
        # check for end of image
        # assign pause input and pause output depending on dimension
#    @always_ff((posedge, "clk"), (negedge, "rst_n"))
#    def update_index_vars(self):
        # check for end of line

        # check for end of image

    # output from transpose buffer
    @always_comb
    def output_tb(self):
        for i in range(stencil_height):
            if (self.buf_index == 0):
                self.col_pixels[i] = self.tb[i + stencil_height][self.index]
            else:
                self.col_pixels[i] = self.tb[i][self.index]

    # generate output index from transpose buffer
    @always_comb
    def generate_tb_output_index(self):
        self.index = (self.index_outer * self.stride + self.indices[self.index_inner]) % fetch_width

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

    # when to swap transpose buffer
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def swap_transpose_buffer(self):
        if ~self.rst_n:
            self.buf_index = 0
        elif self.index == 0:
            self.buf_index = ~self.buf_index
        else:
            self.buf_index = self.buf_index
