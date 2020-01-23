import kratos
from kratos import *
from math import log


class TransposeBuffer(Generator):
    def __init__(self,
                 word_width,
                 # note fetch_width must be powers of 2
                 fetch_width,
                 num_tb,
                 tb_height,
                 max_range,
                 max_stencil_height):
        super().__init__("transpose_buffer", True)

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
        self.input_data = self.input("input_data",
                                     width=self.word_width,
                                     size=self.fetch_width,
                                     packed=True)
        self.valid_data = self.input("valid_data", 1)
        self.range_outer = self.input("range_outer", self.max_range_bits)
        self.range_inner = self.input("range_inner", self.max_range_bits)
        self.stride = self.input("stride", self.max_range_bits)
        self.indices = self.input("indices",
                                  width=clog2(2 * self.num_tb * self.fetch_width),
                                  # the length of indices is equal to range_inner,
                                  # so the maximum possible size for self.indices
                                  # is the maximum value of range_inner, which if
                                  # self.max_range_value
                                  size=self.max_range,
                                  packed=True)
        self.tb_start_index = self.input("tb_start_index", self.num_tb_bits)
        self.stencil_height = self.input("stencil_height", clog2(self.max_stencil_height))

        # outputs
        self.col_pixels = self.output("col_pixels",
                                      width=self.word_width,
                                      size=self.tb_height,
                                      packed=True)
        self.output_valid = self.output("output_valid", 1)

        # local variables
        self.index_outer = self.var("index_outer", self.max_range_bits)
        self.index_inner = self.var("index_inner", self.max_range_bits)

        self.tb = self.var("tb",
                           width=self.word_width,
                           size=[2 * self.tb_height, self.fetch_width],
                           packed=True)

        self.buf_index = self.var("buf_index", 1)
        self.row_index = self.var("row_index", clog2(self.tb_height))
        self.input_index = self.var("input_index", clog2(2 * self.tb_height))

        self.output_index_abs = self.var("output_index_abs", 2 * self.max_range_bits)
        self.output_index_long = self.var("output_index_long", 2 * self.max_range_bits)
        self.output_index = self.var("output_index", self.fetch_width_bits)
        self.indices_index_inner = self.var("indices_index_inner",
                                            clog2(2 * self.num_tb * self.fetch_width))

        self.tb_distance = self.var("tb_distance", self.tb_col_index_bits)
        # delete this signal? or keep for code clarity
        self.tb0_start = self.var("tb0_start", self.tb_col_index_bits)
        self.tb0_end = self.var("tb0_end", self.tb_col_index_bits)
        self.tb1_start = self.var("tb1_start", self.tb_col_index_bits)
        self.tb1_end = self.var("tb1_end", self.tb_col_index_bits)

        self.pause_tb = self.var("pause_tb", 1)

        x = max(self.tb_col_index_bits, 2 * self.max_range_bits)

        self.add_code(self.get_output_loop_iterators)
        self.add_code(self.get_input_index)
        self.add_code(self.input_to_tb)
        self.add_code(self.update_row_index)
        self.add_code(self.get_tb_indices)
        self.add_code(self.output_from_tb)
        self.add_code(self.set_output_valid_buf_index)
        self.add_code(self.tb_col_indices)

    # get output loop iterators
    # set pause_tb signal to pause input/output depending on
    # output loop iterator values and valid_data
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def get_output_loop_iterators(self):
        if ~self.rst_n:
            self.index_inner = 0
            self.index_outer = 0
            self.pause_tb = ~self.valid_data
        else:
            if self.index_inner == self.range_inner - 1:
                self.index_inner = 0
                if self.index_outer == self.range_outer - 1:
                    self.index_outer = 0
                    self.pause_tb = ~self.valid_data
                else:
                    self.index_outer = self.index_outer + 1
                    self.pause_tb = 0
            else:
                self.index_outer = self.index_outer
                if self.pause_tb:
                    self.index_inner = self.index_inner
                    self.pause_tb = ~self.valid_data
                else:
                    self.index_inner = self.index_inner + 1
                    self.pause_tb = 0

    # get index of row to fill in transpose buffer with input data
    # for one of the two buffers in double buffer
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_row_index(self):
        if ~self.rst_n:
            self.row_index = 0
        elif ~self.valid_data:
            self.row_index = self.row_index
        elif self.row_index == self.tb_height - 1:
            self.row_index = 0
        else:
            self.row_index = self.row_index + 1

    # for double buffer, get index of row to fill in transpose buffer
    # with input data
    @always_comb
    def get_input_index(self):
        if self.buf_index:
            self.input_index = const(tb_height, self.tb_height_bits2) + self.row_index.extend(self.tb_height_bits2)
        else:
            self.input_index = self.row_index.extend(self.tb_height_bits2)

    # input to transpose buffer
    @always_ff((posedge, "clk"))
    def input_to_tb(self):
        if ~self.valid_data:
            self.tb = self.tb
        else:
            for i in range(self.fetch_width):
                self.tb[self.input_index][i] = self.input_data[i]

    # get relative output column index from absolute output column index
    @always_comb
    def get_tb_indices(self):
        self.indices_index_inner = self.indices[self.index_inner]
        self.output_index_abs = (self.index_outer.extend(2 * self.max_range_bits) * self.stride.extend(2 * self.max_range_bits) + self.indices_index_inner.extend(2 * self.max_range_bits))
        self.output_index_long = self.output_index_abs % fetch_width
        self.output_index = self.output_index_long[clog2(fetch_width) - 1, 0]

    # output column from transpose buffer
    @always_ff((posedge, "clk"))
    def output_from_tb(self):
        if self.output_valid:
            for i in range(tb_height):
                if ~self.buf_index:
                    self.col_pixels[i] = self.tb[i][self.output_index]
                else:
                    self.col_pixels[i] = self.tb[i + self.tb_height][self.output_index]
        else:
            self.col_pixels = self.col_pixels

    # generates output valid and updates which buffer in double buffer to output from
    # appropriately
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_output_valid_buf_index(self):
        if ~self.rst_n:
            self.output_valid = 0
            self.buf_index = 0
        else:
            if self.pause_tb:
                self.output_valid = 0
                self.buf_index = 0
            elif (self.tb0_start.extend(x) <= self.output_index_abs.extend(x)) & (self.output_index_abs.extend(x) <= self.tb0_end.extend(x)):
                self.output_valid = 1
                self.buf_index = 0
            elif (self.tb1_start.extend(x) <= self.output_index_abs.extend(x)) & (self.output_index_abs.extend(x) <= self.tb1_end.extend(x)):
                self.output_valid = 1
                self.buf_index = 1
            else:
                self.output_valid = 0
                self.buf_index = self.buf_index

    # get starting and ending column indices that represent both buffers part
    # of transpose buffer double buffer
    @always_comb
    def tb_col_indices(self):
        self.tb_distance = self.fetch_width * self.num_tb
        self.tb0_start = self.tb_start_index.extend(self.tb_col_index_bits)
        self.tb0_end = self.tb0_start + self.fetch_width - 1
        self.tb1_start = self.tb0_start + self.tb_distance
        self.tb1_end = self.tb1_start + self.fetch_width - 1
