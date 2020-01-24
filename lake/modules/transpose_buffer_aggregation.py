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
                 max_schedule_length):
        super().__init__("transpose_buffer_aggregation", True)

        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_width
        self.num_tb = num_tb
        self.tb_height = tb_height
        self.max_range = max_range
        self.max_schedule_length = max_schedule_length

        self.max_schedule_length_bits = max(1, clog2(self.max_schedule_length))

        # inputs
        self.clk = self.clock("clk")
        # active low asynchronous reset
        self.rst_n = self.reset("rst_n", 1)
        self.SRAM_to_tb_data = self.input("SRAM_to_tb_data", 
                                          width=self.word_width, 
                                          size=self.fetch_width)        

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

        self.schedule = self.input("schedule", clog2(self.num_tb), size=self.max_schedule_length)
        self.schedule_period = self.input("schedule_period", self.max_schedule_length_bits)
        self.schedule_ptr = self.input("schedule_ptr", self.max_schedule_length_bits)

        # local variables
        self.valid_data_all = self.var("valid_data_all", 
                                       width=1, 
                                       size=self.num_tb)
        
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
            self.wire(self[f"tb_{i}"].ports.input_data, self.SRAM_to_tb_data)
            self.wire(self[f"tb_{i}"].ports.valid_data, self.valid_data_all[i])
            self.wire(self[f"tb_{i}"].ports.range_outer, self.range_outer)
            self.wire(self[f"tb_{i}"].ports.range_inner, self.range_inner)
            self.wire(self[f"tb_{i}"].ports.stride, self.stride)
            self.wire(self[f"tb_{i}"].ports.indices, self.indices)
            self.wire(self[f"tb_{i}"].ports.tb_start_index, self.num_tb*i)

        self.add_code(self.set_valid_data_all)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def set_valid_data_all(self):
            if ~self.valid_data:
                for i in range(self.num_tb):
                    self.valid_data_all[i] = 0
            else:
                for i in range(self.num_tb):
                    if i == self.schedule[self.schedule_ptr]:
                        self.valid_data_all[i] = 1
                    else:
                        self.valid_data_all[i] = 0

