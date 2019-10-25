from kratos import * 
from math import log

class TransposeBuffer(Generator):
    def __init__(self, word_width, memory_width, range_, stride, stencil_height):
        super().__init__("transpose_buffer", True)

        # inputs
        self.clk = self.clock("clk") # clock
        self.rst = self.reset("rst", 1) # asynchronous reset
        self.sram_input = self.input("sram_input", width=word_width, size=memory_width)
        self.valid_input = self.input("sram_input", width=1, size=memory_width)

        # outputs
        self.col_pixels = self.output("col_pixels", width=word_width, size=stencil_height)
        self.read_valid = self.output("read_valid", 1)
        self.stencil_valid = self.output("stencil_valid", 1)
        
        # add sequential blocks

        # add combinational blocks
