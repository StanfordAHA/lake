import kratos
from kratos import *
from math import log

class TransposeBuffer(Generator):
    def __init__(self, word_width, mem_word_width, stencil_height, stencil_width, num_output):
        super().__init__("transpose_buffer", True)
        
        self.word_width = word_width
        self.mem_word_width = mem_word_width
        self.stencil_height = stencil_height
        self.stencil_width = stencil_width
        self.num_output = num_output
    
        # inputs
        self.clk = self.clock("clk")
        # active low asynchronous reset
        self.rst_n = self.reset("rst_n", 1)
        self.mem_data = self.input("mem_data", width=word_width, size=mem_word_width, packed=True)
        self.valid_input = self.input("valid_input", width=1, size=mem_word_width, packed=True)

        self.out_indices = self.input("out_indices", width=clog2(mem_word_width), size=num_output, packed=True)
        self.out_indices_valid = self.input("out_indices_valid", width=1, size=num_output, packed=True)
        
        # outputs
        self.col_pixels = self.output("col_pixels", width=word_width, size=stencil_height, packed=True)
        self.output_valid = self.output("output_valid", 1)
        self.stencil_valid = self.output("stencil_valid", 1)

        # local variables
        self.tb = self.var("tb", width=word_width, size=[2*stencil_height, mem_word_width], packed=True)
        self.col_index = self.var("col_index", clog2(num_output))
        self.row_index = self.var("row_index", clog2(stencil_height))
        self.switch_buf = self.var("switch_buf", 1)
        self.valid_data = self.var("valid_data", width=word_width, size=mem_word_width, packed=True)
        self.max_dim = self.var("max_dim", 1)
        self.pause_output = self.var("pause_output", 1)
        self.pause_input = self.var("pause_input", 1)

        self.valid_out_indices = self.var("valid_out_indices", width=clog2(mem_word_width), size=num_output, packed=True)

        self.valid_cols_count = self.var("valid_cols_count", clog2(stencil_width))
        
        self.num_out = self.var("num_out", clog2(mem_word_width))
        self.num = self.var("num", clog2(mem_word_width) + 1)

        self.add_code(self.in_buf)
        self.add_code(self.update_index_vars)
        self.add_code(self.out_buf)
        self.add_code(self.set_output_valid)
        self.add_code(self.update_valid_cols_count)
        self.add_code(self.generate_stencil_valid)
        self.add_code(self.get_max_dim)
        self.get_valid_data()
        self.get_valid_output_indices()

    @always_comb
    def get_max_dim(self):
        self.num = self.num_out.extend(clog2(mem_word_width) + 1) + 1
        if self.num.extend(max(clog2(stencil_height), clog2(mem_word_width) + 1)) >= stencil_height:
            self.max_dim = 0
        else:
            self.max_dim = 1

    def get_valid_data(self):
        mem_word_width_log = clog2(self.mem_word_width)
        num_valid = self.valid_input[0].extend(mem_word_width_log)
        comb_valid_data = self.combinational()

        # initialize to 0
        for i in range(self.mem_word_width):
            comb_valid_data.add_stmt(self.valid_data[i].assign(0))

        if_first_valid_input = IfStmt(self.valid_input[0] == 1)
        if_first_valid_input.then_(self.valid_data[0].assign(self.mem_data[0]))
        comb_valid_data.add_stmt(if_first_valid_input)
        for i in range(1, self.mem_word_width):
            num_valid = num_valid + self.valid_input[i].extend(mem_word_width_log)
            if_valid_input = IfStmt(self.valid_input[i] == 1)
            if_valid_input.then_(self.valid_data[num_valid-1].assign(self.mem_data[i]))
            comb_valid_data.add_stmt(if_valid_input)

    def get_valid_output_indices(self):
        num_output_log = clog2(self.num_output)
        num_valid_out = self.out_indices_valid[0].extend(num_output_log)
        comb_valid_out = self.combinational()

        # initialize to 0
        for i in range(self.num_output):
            comb_valid_out.add_stmt(self.valid_out_indices[i].assign(0))

        if_first_valid_out = IfStmt(self.out_indices_valid[0] == 1)
        if_first_valid_out.then_(self.valid_out_indices[0].assign(self.out_indices[0]))
        comb_valid_out.add_stmt(if_first_valid_out)
        for i in range(1, self.num_output):
            num_valid_out = num_valid_out + self.out_indices_valid[i].extend(num_output_log)
            if_valid_out = IfStmt(self.out_indices_valid[i] == 1)
            if_valid_out.then_(self.valid_out_indices[num_valid_out-1].assign(self.out_indices[i]))
            comb_valid_out.add_stmt(if_valid_out)
        comb_valid_out.add_stmt(self.num_out.assign(num_valid_out-1))

    # updating index variables
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
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

        elif ((self.max_dim == 1) & (self.col_index == self.num_out - 1)):

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

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
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

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def generate_stencil_valid(self):
        if ~self.rst_n:
            self.stencil_valid = 0
        else:
            if (self.valid_cols_count == self.stencil_width - 1):
                self.stencil_valid = 1
            else:
                self.stencil_valid = 0

    @always_ff((posedge, "clk"))
    def in_buf(self):
        if self.pause_input == 0:
            for i in range(mem_word_width):
                self.tb[const(stencil_height,clog2(2*stencil_height))*self.switch_buf.extend(clog2(2*stencil_height)) + self.row_index.extend(clog2(2*stencil_height))][i] = self.valid_data[i]
        else:
            self.tb = self.tb

    # output appropriate data from transpose buffer
    @always_comb
    def out_buf(self):
        for i in range(stencil_height):
            if (self.switch_buf == 0):
                self.col_pixels[i] = self.tb[i + stencil_height][self.valid_out_indices[self.col_index]]
            else:
                self.col_pixels[i] = self.tb[i][self.valid_out_indices[self.col_index]]

    @always_comb
    def set_output_valid(self):
        if (~self.rst_n) | (self.pause_output == 1):
            self.output_valid = 0
        else:
            self.output_valid = 1
