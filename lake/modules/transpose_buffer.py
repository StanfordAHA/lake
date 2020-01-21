import kratos
from kratos import *
from math import log


class TransposeBuffer(Generator):
    # note fetch_width must be powers of 2 for mod to work (synthesizable) **for now
    def __init__(self,
                 word_width,
                 fetch_width,
                 num_tb,
                 stencil_height,
                 max_range_value,
                 max_img_height,
                 max_stencil_height,
                 max_initial_delay):
        super().__init__("transpose_buffer", True)

        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_width
        self.num_tb = num_tb
        self.stencil_height = stencil_height
        self.max_range_value = max_range_value
        self.max_img_height = max_img_height
        self.max_stencil_height = max_stencil_height

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
        self.indices = self.input("indices",
                                  width=clog2(2 * self.num_tb * self.fetch_width),
                                  size=self.max_range_value,
                                  packed=True)
        self.tb_start_index = self.input("tb_start_index", max(1, clog2(num_tb)))
        self.img_height = self.input("img_height", clog2(self.max_img_height))
        self.stencil_height_input = self.input("stencil_height_input",
                                               clog2(self.max_stencil_height))
        self.initial_delay = self.input("initial_delay", clog2(self.max_initial_delay))
        # self.img_height should be a config reg, decide if max_range_value and
        # max_img_height value are distinct and make the latter the parameter instead
        # outputs
        self.col_pixels = self.output("col_pixels", width=self.word_width, size=self.stencil_height, packed=True)
        self.output_valid = self.output("output_valid", 1)

        # local variables

        self.prev_rst = self.output("prev_rst", 1)
        self.index_outer = self.output("index_outer", clog2(self.max_range_value))
        self.index_inner = self.output("index_inner", clog2(self.max_range_value))

        self.tb = self.var("tb", width=self.word_width, size=[2*self.stencil_height, self.fetch_width], packed=True)
        self.buf_index = self.output("buf_index", 1)
        #self.prev_buf_index = self.output("prev_buf_index", 1)
        self.switch_buf = self.output("switch_buf", 1)
        self.row_index = self.output("row_index", clog2(self.stencil_height))
        self.input_index = self.output("input_index", clog2(2*self.stencil_height))
        self.output_index_inter_tb = self.output("output_index_inter_tb", 2*clog2(self.max_range_value))
        self.output_index_inter = self.output("output_index_inter", 2*clog2(self.max_range_value))
        self.output_index = self.output("output_index", clog2(self.fetch_width))
        self.indices_index_inner = self.output("indices_index_inner", clog2(2*self.num_tb*self.fetch_width))

        self.tb_distance = self.output("tb_distance", 2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1)
        # delete this signal? or keep for code clarity
        self.tb0_start = self.output("tb0_start", 2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1) 
        self.tb0_end = self.output("tb0_end", 2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1)
        self.tb1_start = self.output("tb1_start", 2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1)
        self.tb1_end = self.output("tb1_end", 2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1)
        self.img_line_cnt = self.output("img_line_cnt", clog2(self.max_range_value))

        x = max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value))

        self.add_code(self.get_output_loop_iterators)
        self.add_code(self.get_input_index)
        self.add_code(self.input_tb)
        self.add_code(self.update_row_index)
        self.add_code(self.get_tb_indices)
        self.add_code(self.output_tb)
        self.add_code(self.generate_output_valid)
        self.add_code(self.output_inter_signals)

    # get loop indices
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def get_output_loop_iterators(self):
        if ~self.rst_n:
            self.index_inner = 0
            self.index_outer = 0
            self.img_line_cnt = 0
            self.initial_delay_cnt = 0
        else:
            if self.index_inner == self.range_inner - 1:
                self.index_inner = 0
                if self.index_outer == self.range_outer - 1:
                    self.index_outer = 0
                    if self.img_line_cnt.extend(max(clog2(self.max_range_value), clog2(self.max_img_height)))== self.img_height.extend(max(clog2(self.max_range_value), clog2(self.max_img_height))) - 1 - self.stencil_height - 1:
                        self.img_line_cnt = self.img_line_cnt
                    else:
                        if self.initial_delay_cnt < self.initial_delay:
                            self.initial_delay_cnt = self.initial_delay_cnt + 1
                            self.img_line_cnt = self.img_line_cnt
                        else:
                            self.initial_delay_cnt = 0
                            self.img_line_cnt = self.img_line_cnt + self.stride.extend(clog2(self.max_range_value))
                else:
                    self.index_outer = self.index_outer + 1
                    self.img_line_cnt = self.img_line_cnt
            else:
                self.index_inner = self.index_inner + 1
                self.index_outer = self.index_outer
                self.img_line_cnt = self.img_line_cnt

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_row_index(self):
        if ~self.rst_n:
            self.prev_rst = 1
        else:
            self.prev_rst = 0

        if ~self.rst_n:
            self.row_index = 0
        elif self.row_index == self.stencil_height - 1:
            if ~self.switch_buf & ~self.prev_rst:
                self.row_index = self.row_index
            else:
                self.row_index = 0
        else:
            self.row_index = self.row_index + const(1, clog2(self.stencil_height))
    
    @always_comb
    def get_input_index(self):
        self.input_index = const(stencil_height,clog2(2 * self.stencil_height)) * self.buf_index.extend(clog2(2 * stencil_height)) + self.row_index.extend(clog2(2 * stencil_height))

    # input to transpose buffer
    @always_ff((posedge, "clk"))
    def input_tb(self):
        if ~self.rst_n:
            self.tb = self.tb
        elif (self.row_index == self.stencil_height - 1) & (~self.switch_buf) & (~self.prev_rst):
            self.tb = self.tb
        else:
            for i in range(self.fetch_width):
                self.tb[self.input_index][i] = self.input_data[i]

    @always_comb
    def get_tb_indices(self):
        self.indices_index_inner = self.indices[self.index_inner]
        self.output_index_inter_tb = (self.index_outer.extend(2 * clog2(max_range_value)) * self.stride.extend(2 * clog2(max_range_value))) + self.indices_index_inner.extend(2 * clog2(max_range_value))
        self.output_index_inter = self.output_index_inter_tb % fetch_width
        self.output_index = self.output_index_inter[clog2(fetch_width) - 1, 0]

    @always_ff((posedge, "clk"))
    def output_tb(self):
        if self.output_valid:
            for i in range(stencil_height):
                if ~self.buf_index:
                    self.col_pixels[i] = self.tb[i][self.output_index]
                else:
                    self.col_pixels[i] = self.tb[i + self.stencil_height][self.output_index]
        else:
            self.col_pixels = self.col_pixels

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def generate_output_valid(self):
        if ~self.rst_n:
            self.output_valid = 0
            self.buf_index = 0
        else:
            #if self.img_line_cnt.extend(max(clog2(self.max_range_value), clog2(self.max_img_height)))== self.img_height.extend(max(clog2(self.max_range_value), clog2(self.max_img_height))) - 1 - self.stencil_height - 1:
            #    self.output_valid = 0
            #    self.buf_index = 0
            if (self.tb0_start.extend(x) <= self.output_index_inter_tb.extend(max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value)))) & (self.output_index_inter_tb.extend(x) <= self.tb0_end.extend(x)):
                self.output_valid = 1
                self.buf_index = 0
            elif (self.tb1_start.extend(max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value))) <= self.output_index_inter_tb.extend(max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value)))) & (self.output_index_inter_tb.extend(max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value)))  <= self.tb1_end.extend(max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value)))):
                self.output_valid = 1
                self.buf_index = 1
            else:
                self.output_valid = 0
                self.buf_index = self.buf_index

    @always_comb
    def output_inter_signals(self):

        if (self.tb0_start.extend(x) <= self.output_index_inter_tb.extend(max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value)))) & (self.output_index_inter_tb.extend(x) <= self.tb0_end.extend(x)):
            self.switch_buf = self.buf_index
        elif (self.tb1_start.extend(max(2 * max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2 * clog2(self.max_range_value))) <= self.output_index_inter_tb.extend(max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value)))) & (self.output_index_inter_tb.extend(max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value)))  <= self.tb1_end.extend(max(2*max(clog2(self.fetch_width), clog2(self.num_tb)) + 1, 2*clog2(self.max_range_value)))):
            self.switch_buf = ~self.buf_index
        else:
            self.switch_buf = 0

        self.tb_distance = self.fetch_width * self.num_tb
        self.tb0_start = self.tb_start_index.extend(2 * max(clog2(self.fetch_width), clog2(self.num_tb)) + 1)
        self.tb0_end = self.tb0_start + self.fetch_width - 1
        self.tb1_start = self.tb0_start + self.tb_distance
        self.tb1_end = self.tb1_start + self.fetch_width - 1
