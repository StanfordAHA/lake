import kratos
from kratos import *
from math import log

class TransposeBuffer(Generator):
    def __init__(self, word_width, mem_word_width, stencil_height, stencil_width, max_num_output):
        super().__init__("transpose_buffer", True)
        
        self.word_width = word_width
        self.mem_word_width = mem_word_width
        self.stencil_height = stencil_height
        self.stencil_width = stencil_width
        self.max_num_output = max_num_output
    
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
        self.valid_data = self.var("valid_data", width=word_width, size=mem_word_width, packed=True)
        self.max_dim = self.var("max_dim", 1)
        self.pause_output = self.var("pause_output", 1)
        self.pause_input = self.var("pause_input", 1)

        self.valid_cols_count = self.var("valid_cols_count", clog2(stencil_width))

        self.add_code(self.in_buf)
        self.add_code(self.update_index_vars)
        self.add_code(self.out_buf)
        self.add_code(self.set_output_valid)
        self.add_code(self.update_valid_cols_count)
        self.add_code(self.generate_stencil_valid)
        self.get_valid_data()
        self.get_max_dim()

    def get_max_dim(self):
        if self.mem_word_width >= self.stencil_height:
            self.add_stmt(self.max_dim.assign(0))
        else:
            self.add_stmt(self.max_dim.assign(1))

    def get_valid_data(self):
        mem_word_width_log = clog2(self.mem_word_width)
        num_valid = self.valid_input[0].extend(mem_word_width_log)
        comb = self.combinational()
        for i in range(self.mem_word_width):
            comb.add_stmt(self.valid_data[i].assign(0))
        if_first_valid_input = IfStmt(self.valid_input[0] == 1)
        if_first_valid_input.then_(self.valid_data[0].assign(self.mem_data[0]))
        comb.add_stmt(if_first_valid_input)
        for i in range(1, self.mem_word_width):
            num_valid = num_valid + self.valid_input[i].extend(mem_word_width_log)
            if_valid_input = IfStmt(self.valid_input[i] == 1)
            if_valid_input.then_(self.valid_data[num_valid-1].assign(self.mem_data[i]))
            comb.add_stmt(if_valid_input)

    # updating index variables
    @always((posedge, "clk"), (negedge, "rst_n"))
    def update_index_vars(self):
        if ~self.rst_n:
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

'''    @always((posedge, "clk"), (negedge, "rst_n"))
    def update_valid_cols_count(self):
        if ~self.rst_n:
            self.valid_cols_count = 0
        else:
            if (self.output_valid == 1):
                if (self.valid_cols_count == self.stencil_width - 1):
                    self.valid_cols_count = 0
                else:
                    self.valid_cols_count = self.valid_cols_count + 1
            else:
                self.valid_cols_count = self.valid_cols_count

    @always((posedge, "clk"), (negedge, "rst_n"))
    def generate_stencil_valid(self):
        if ~self.rst_n:
            self.stencil_valid = 0
        else:
            if (self.valid_cols_count == self.stencil_width - 1):
                self.stencil_valid = 1
            else:
                self.stencil_valid = 0
'''
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
        if (~self.rst_n) | (self.pause_output == 1):
            self.output_valid = 0
        else:
            self.output_valid = 1
