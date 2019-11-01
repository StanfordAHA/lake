from kratos import *
from math import log

class TransposeBuffer(Generator):
	def __init__(self, word_width, mem_word_width, range_, stride, stencil_height):
		super().__init__("transpose_buffer", True)
		
		# inputs
		self.clk = self.clock("clk") # clock
		self.rst = self.reset("rst", 1) # asynchronous reset
		self.mem_data = self.input("mem_data", width=word_width, size=mem_word_width)
		self.valid_input = self.input("valid_input", width=1, size=mem_word_width)
		
		# outputs
		self.col_pixels = self.output("col_pixels", width=word_width, size=stencil_height)
		self.read_valid = self.output("read_valid", 1)
		self.stencil_valid = self.output("stencil_valid", 1)
		
		# local variablbes
		self.tb = self.var("tb", width=mem_word_width, size=2*stencil_height)
                self.indices = self.var("indices", width=mem_word_width)
		self.col_index = self.var("col_index", clog2(mem_word_width))
		
		# sequential blocks
		self.add_code(self.in_buf)
                self.add_code(self.update_index_vars)

		# combinational blocks
		self.add_code(self.dummy_func)

	# updating index variables
	@always((posedge, "clk"))
	def update_index_vars(self):        
	    if (self.rst == 0):
    		self.col_index = 0
	    elif (self.col_index == mem_word_width - 1):
                self.col_index = 0
	    else:
	    	self.col_index = self.col_index + const(1, clog2(mem_word_width))
	
	# setting valid outputs
	def dummy_func(self):
            self.read_valid = 1
	    self.stencil_valid = 1
		
	# update transpose buffer with data from memory
	@always((posedge, "clk"))
	def in_buf(self):
            for i in range(len(indices)):
                indices[i] = -1
	    for i in range(len(indices)):
    		if valid[i] == 1:
                    for j in range(i + 1):
                        if indices[j] == -1:
                            indices[j] = self.mem_data[i]
                            if j > 0:
                                indices[j-1] = -1
						
	# output appropriate data from transpose buffer
	@always((posedge, "clk"))
	def out_buf(self):
	    for i in range(stencil_height):
		self.col_pixels[i] = self.tb[i][self.col_index]

