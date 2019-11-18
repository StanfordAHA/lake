import kratos
from kratos import *
from math import log

class TransposeBuffer(Generator):
    def __init__(self, word_width, mem_word_width, range_, stride, stencil_height):
        super().__init__("transpose_buffer", True)
        
        self.word_width = word_width
        self.mem_word_width = mem_word_width
        self.stencil_height = stencil_height
    
        # inputs
        self.clk = self.clock("clk")
        # active low asynchronous reset
        self.rst_n = self.reset("rst_n", 1)
        self.mem_data = self.input("mem_data", width=word_width, size=mem_word_width, packed=True)
        self.valid_input = self.input("valid_input", width=1, size=mem_word_width, packed=True)
        
        # outputs
        self.col_pixels = self.output("col_pixels", width=word_width, size=stencil_height, packed=True)
        self.output_valid = self.output("output_valid", 1)
#        self.stencil_valid = self.output("stencil_valid", 1)

        # local variables
        self.tb = self.var("tb", width=word_width, size=[2*stencil_height, mem_word_width], packed=True)
        self.col_index = self.var("col_index", clog2(mem_word_width))
        self.row_index = self.var("row_index", clog2(stencil_height))
        self.switch_buf = self.var("switch_buf", 1)
        self.valid_data = self.var("valid_data", width=word_width, size=2**mem_word_width, packed=True)
        self.max_dim = self.var("max_dim", 1)
        self.pause_output = self.var("pause_output", 1)
        self.pause_input = self.var("pause_input", 1)

        self.add_code(self.in_buf)
        self.add_code(self.update_index_vars)
        self.add_code(self.out_buf)
        self.add_code(self.set_output_valid)
        self.get_valid_data()
        self.get_max_dim()

    def get_max_dim(self):
        if self.mem_word_width >= self.stencil_height:
            self.add_stmt(self.max_dim.assign(0))
        else:
            self.add_stmt(self.max_dim.assign(1))

    def get_valid_data(self):
        
        num_valid_ = self.valid_input[0].extend(self.mem_word_width)
        comb = self.combinational()
        for i in range(self.mem_word_width):
            comb.add_stmt(self.valid_data[i].assign(0))
        if__ = IfStmt(self.valid_input[0] == 1)
        if__.then_(self.valid_data[0].assign(self.mem_data[0]))
        comb.add_stmt(if__)
        for i in range(1, self.mem_word_width):
            num_valid_ = num_valid_ + self.valid_input[i].extend(self.mem_word_width)
            if_ = IfStmt(self.valid_input[i] == 1)
            if_.then_(self.valid_data[num_valid_-1].assign(self.mem_data[i]))
            comb.add_stmt(if_)

    # updating index variables
    @always((posedge, "clk"), (negedge, "rst_n"))
    def update_index_vars(self):
        if (self.rst_n == 0):
            self.col_index = 0
            self.row_index = 0
            self.switch_buf = 0
            self.pause_input = 0
            self.pause_output = 0

        elif ((self.max_dim == 0) & (self.row_index == stencil_height - 1)):

            if (self.col_index == mem_word_width - 1):
                self.col_index = 0
                self.row_index = 0
                self.switch_buf = ~self.switch_buf
                self.pause_input = 0
                self.pause_output = self.pause_output
            
            else:
                self.col_index = self.col_index + const(1, clog2(mem_word_width))
                self.row_index = self.row_index
                self.switch_buf = self.switch_buf
                self.pause_input = 1
                self.pause_output = self.pause_output

        elif ((self.max_dim == 1) & (self.col_index == mem_word_width - 1)):

            if (self.row_index == stencil_height - 1):
                self.col_index = 0
                self.row_index = 0
                self.switch_buf = ~self.switch_buf
                self.pause_input = self.pause_input
                self.pause_output = 0
            
            else:
                self.col_index = self.col_index
                self.row_index = self.row_index + const(1, clog2(stencil_height))
                self.switch_buf = self.switch_buf
                self.pause_input = self.pause_input
                self.pause_output = 1

        else:
            self.col_index = self.col_index + const(1, clog2(mem_word_width))
            self.row_index = self.row_index + const(1, clog2(stencil_height))
            self.switch_buf = self.switch_buf
            self.pause_input = 0
            self.pause_output = 0

    @always((posedge, "clk"))
    def in_buf(self):
        if self.pause_input == 0:
            for i in range(mem_word_width):
                self.tb[const(stencil_height,clog2(2*stencil_height))*self.switch_buf.extend(clog2(2*stencil_height)) + self.row_index.extend(clog2(2*stencil_height))][i] = self.valid_data[i]
        else:
            self.tb = self.tb

    # output appropriate data from transpose buffer
    def out_buf(self):
        if self.pause_output == 0:
            for i in range(stencil_height):
                if (self.switch_buf == 0):
                    self.col_pixels[i] = self.tb[i + stencil_height][self.col_index]
                else:
                    self.col_pixels[i] = self.tb[i][self.col_index]
        else:
            self.col_pixels = self.col_pixels

    def set_output_valid(self):
        if (self.rst_n == 0) | (self.pause_output == 1):
            self.output_valid = 0
        else:
            self.output_valid = 1
