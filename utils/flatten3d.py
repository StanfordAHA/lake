import kratos
from kratos import *

class Flatten3D(Generator):
    def __init__(self, array_width, array_size1, array_size2):
        super().__init__("flatten3d", True)

        self.array_width = array_width
        self.array_size1 = array_size1
        self.array_size2 = array_size2
        self.in_ = self.input("in", width=self.array_width, size=[self.array_size1, self.array_size2])
        self.out_ = self.output("out", width=self.array_width*self.array_size1*self.array_size2)

        apparent_width = self.array_width*self.array_size2

        self.add_code(self.flatten_array)

    @always_comb
    def flatten_array(self):
        for i in range(self.array_size1):
            self.out_[(i+1)*apparent_width - 1, i*apparent_width] = self.in_[i]

