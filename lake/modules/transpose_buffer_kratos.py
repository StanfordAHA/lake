import kratos
from kratos import *
from math import log

class TransposeBuffer(Generator):
    def __init__(self, word_width, mem_word_width, range_, stride, stencil_height):
        super().__init__("transpose_buffer", True)

        # inputs
        self.clk = self.clock("clk")
        # active low asynchronous reset
        self.rst_n = self.reset("rst_n", 1)
        self.mem_data = self.input("mem_data", width=word_width, size=mem_word_width, packed=True)
        self.valid_input = self.input("valid_input", width=1, size=mem_word_width, packed=True)
        
        # outputs
        self.testing = self.output("testing", clog2(mem_word_width))
#        self.col_pixels = self.output("col_pixels", width=word_width, size=stencil_height, packed=True)
#        self.read_valid = self.output("read_valid", 1)
#        self.stencil_valid = self.output("stencil_valid", 1)

        # local variables
#        self.tb = self.var("tb", width=word_width, size=[2*stencil_height, mem_word_width], packed=True)
#        self.indices = self.var("indices", width=clog2(mem_word_width), size=mem_word_width, packed=True)
        self.col_index = self.var("col_index", clog2(mem_word_width))
        self.row_index = self.var("row_index", clog2(stencil_height))
        self.switch_buf = self.var("switch_buf", 1)
#        self.row = self.var("row", clog2(2*stencil_height))
#        self.out_row_index = self.var("out_row_index", clog2(2*stencil_height))

        # sequential blocks
#        self.add_code(self.get_valid_indices)
#        self.add_code(self.in_buf)
        self.add_code(self.update_index_vars)
#        self.add_code(self.out_buf)

        # combinational blocks

    #updating index variables
    @always((posedge, "clk"), (negedge, "rst_n"))
    def update_index_vars(self):
        if (self.rst_n == 0):
            self.col_index = 0
            self.row_index = 0
            self.switch_buf = 0
            self.testing = self.col_index
        # assuming mem_word_width == stencil_height FOR NOW TO DO CHANGE
        # row_index resets at stencil_height not 2*stencil_height
        elif (self.col_index == mem_word_width - 1):
            self.col_index = 0
            self.row_index = 0
            self.switch_buf = ~self.switch_buf
            self.testing = self.col_index
        else:
            self.col_index = self.col_index + const(1, clog2(mem_word_width))
            self.row_index = self.row_index + const(1, clog2(stencil_height))
            self.switch_buf = self.switch_buf
            self.testing = self.col_index

    # setting valid outputs
'''
    #update transpose buffer with data from memory
    @always((posedge, "clk"))
    def get_valid_indices(self):
        for i in range(mem_word_width):
            self.indices[i] = i
        for i in range(mem_word_width):
            if self.valid_input[i] == 0:
                for j in range(i, mem_word_width - 1):
                    self.indices[j] = self.indices[j+1]

    @always((posedge, "clk"))
    def in_buf(self):
        self.row = const(stencil_height,clog2(2*stencil_height))*self.switch_buf.extend(clog2(2*stencil_height)) + self.row_index.extend(clog2(2*stencil_height))
        for i in range(mem_word_width):
            self.tb[self.row][i] = self.mem_data[self.indices[i]]

    # output appropriate data from transpose buffer
    @always((posedge, "clk"))
    def out_buf(self):
        for i in range(stencil_height):
            if (self.switch_buf == 0):
                self.col_pixels[i] = self.tb[i + stencil_height][self.col_index]
            else:
                self.col_pixels[i] = self.tb[i][self.col_index]
   #         self.out_row_index = self.switch_buf.extend(clog2(2*stencil_height))*const(stencil_height,clog2(2*stencil_height)) + const(i, clog2(2*stencil_height))
#            self.col_pixels[i] = self.tb[0][self.col_index]
'''
