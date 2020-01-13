import kratos
from kratos import *

class Flatten(Generator):
    def __init__(self, array_width, array_size):
        super().__init__("flatten", True)

        self.array_width = array_width
        self.array_size = array_size
        self.in_ = self.input("in", width=self.array_width, size=self.array_size, packed=True)
        self.out_ = self.output("out", width=self.array_width*self.array_size)

        self.add_code(self.flatten_array)

    @always_comb
    def flatten_array(self):
        for i in range(self.array_size):
            self.out_[(i+1)*array_width - 1, i*array_width] = self.in_[i]

