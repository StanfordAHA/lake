from kratos import * 
from math import log

class TransposeBuffer(Generator):
    def __init__(self, word_width, mem_word_width, range_, stride, stencil_height):
        super().__init__("transpose_buffer", True)

        # inputs
        self.clk = self.clock("clk") # clock
        self.rst = self.reset("rst", 1) # asynchronous reset
        self.sram_input = self.input("sram_input", width=word_width, size=mem_word_width)
        self.valid_input = self.input("valid_input", width=1, size=mem_word_width)

        # outputs
        self.col_pixels = self.output("col_pixels", width=word_width, size=stencil_height)
        self.read_valid = self.output("read_valid", 1)
        self.stencil_valid = self.output("stencil_valid", 1)

        # local variables
        self.tb = self.var("tb", width=mem_word_width, size=2*stencil_height)
        self.read_row = self.var("read_row", clog2(stencil_height))
        self.out_col = self.var("out_col", clog2(mem_word_width))
        self.read_buf = self.var("read_buf", 1)
        self.num_valid = self.var("num_valid", clog2(mem_word_width))
        
        # add sequential blocks
        self.add_code(self.update_index_vars)
        self.add_code(self.update_read_buf)
        self.add_code(self.out_write_buf)

        # add combinational blocksa
    
    # updating index variables
    @always((posedge, "clk"))
    def update_index_vars(self):
        if (self.rst == 0):
            self.read_row = 0
            self.out_col = 0
            self.read_buf = 0
            self.read_valid = 0
            self.stencil_valid = 0
#        elif (((stencil_height >= mem_word_width) & (self.read_row == (stencil_height - 1)))| ((stencil_height < mem_word_width) & (self.out_col == mem_word_width))):
 #           self.read_row = 0
  #          self.out_col = 0
   #         self.read_buf = ~self.read_buf
        elif (stencil_height >= mem_word_width):
            self.out_col = 0
            self.read_valid = 0
        #elif (stencil_height < memory_width/word_width):
        else:
            self.read_row = self.read_row + const(1, clog2(stencil_height))
            self.out_col = self.out_col + const(1, clog2(mem_word_width))
            self.read_valid = 0 # check if this is right
            self.stencil_valid = 0

    @always((posedge, "clk"))
    def update_read_buf(self):
        read_col = 0
        for i in range(mem_word_width):
            if (valid_input[i] == 1):
                self.tb[self.read_row + self.read_buf * stencil_height][read_col] = self.sram_input[i]
                read_col = read_col + 1 # will this work??

    @always((posedge, "clk"))
    def out_write_buf(self):
        for i in range(stencil_height):
            self.col_pixels[i] = self.tb[i][self.out_col]

dut = TransposeBuffer(8, 8, 1, 3, 5)
verilog(dut, filename="tb.sv")
