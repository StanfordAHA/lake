import kratos
from kratos import *
from math import log

class TransposeBuffer(Generator):
    def __init__(self, word_width, mem_word_width, range_, stride, stencil_height):
        super().__init__("transpose_buffer", True)
        
        self.word_width = word_width
        self.mem_word_width = mem_word_width
    
        # inputs
        self.clk = self.clock("clk")
        # active low asynchronous reset
        self.rst_n = self.reset("rst_n", 1)
        self.mem_data = self.input("mem_data", width=word_width, size=mem_word_width, packed=True)
        self.valid_input = self.input("valid_input", width=1, size=mem_word_width, packed=True)
        
        # outputs
#        self.col_pixels = self.output("col_pixels", width=word_width, size=stencil_height, packed=True)
#        self.read_valid = self.output("read_valid", 1)
#        self.stencil_valid = self.output("stencil_valid", 1)

        # local variables
#        self.tb = self.var("tb", width=word_width, size=[2*stencil_height, mem_word_width], packed=True)
        self.indices = self.var("indices", width=clog2(mem_word_width + 1), size=mem_word_width, packed=True)
        self.tb_indices = self.var("tb_indices", width=clog2(mem_word_width), size=mem_word_width, packed=True)
        self.col_index = self.var("col_index", clog2(mem_word_width))
        self.row_index = self.var("row_index", clog2(stencil_height))
        self.switch_buf = self.var("switch_buf", 1)
        self.num_valid = self.var("num_valid", mem_word_width)
        self.num_valid_ = self.var("num_valid_", mem_word_width)
        self.valid_data = self.var("valid_data", width=word_width, size=mem_word_width, packed=True)
        self.test = self.var("test", width = clog2(mem_word_width))
        self.test2 = self.var("test2", width = mem_word_width)
#        self.row = self.var("row", clog2(2*stencil_height))
#        self.out_row_index = self.var("out_row_index", clog2(2*stencil_height))

        # sequential blocks
#        self.add_code(self.get_num_valid)
#        self.add_code(self.get_valid_indices)
#        self.add_code(self.in_buf)
        self.add_code(self.update_index_vars)
#        self.add_code(self.out_buf)
        self.get_num_valid()
        # combinational blocks

    def get_num_valid(self):
        self.num_valid_ = self.valid_input[0].extend(self.mem_word_width)
        for i in range(1, self.mem_word_width):
            self.num_valid_ = self.num_valid_ + self.valid_input[i].extend(self.mem_word_width)
            if self.valid_input[i] == 1:
                self.test2 = self.num_valid - 1
                self.test = self.test2[1, 0]
                self.add_stmt(self.valid_data[self.test].assign(self.mem_data[i]))
#            else:
#                self.valid_data[i] = self.mem_data[i]
        if self.num_valid_ < self.mem_word_width:
            for i in range(self.mem_word_width):
                if i > self.num_valid_:
                    self.add_stmt(self.valid_data[i].assign(self.mem_data[i]))
        self.add_stmt(self.num_valid.assign(self.num_valid_))

    #updating index variables
    @always((posedge, "clk"), (negedge, "rst_n"))
    def update_index_vars(self):
        if (self.rst_n == 0):
            self.col_index = 0
            self.row_index = 0
            self.switch_buf = 0
        # assuming mem_word_width == stencil_height FOR NOW TO DO CHANGE
        # row_index resets at stencil_height not 2*stencil_height
        elif (self.col_index == mem_word_width - 1):
            self.col_index = 0
            self.row_index = 0
            self.switch_buf = ~self.switch_buf
        else:
            self.col_index = self.col_index + const(1, clog2(mem_word_width))
            self.row_index = self.row_index + const(1, clog2(stencil_height))
            self.switch_buf = self.switch_buf

#    @always((posedge, "clk"))
#    def in_buf(self):
'''
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
