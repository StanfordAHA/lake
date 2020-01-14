import kratos
from kratos import *

class ReverseFlatten(Generator):
    def __init__(self, array_width, *array_size):
        super().__init__("ReverseFlatten", True)

        total_dims = array_width * array_size[0]
        apparent_width = array_width
        for i in range(1, len(array_size)):
            total_dims = total_dims*array_size[i]
            apparent_width = apparent_width*array_size[i]

        self.in_ = self.input("in", width=total_dims)
        self.out_ = self.output("out", width=array_width, size=array_size)

        self.add_code(self.make_array)

    @always_comb
    def make_array(self):
        for i in range(array_size[0]):
            self.out_[i] = self.in_[(i+1)*apparent_width - 1, i*apparent_width]

