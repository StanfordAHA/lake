import kratos
from kratos import *


class FlattenND(Generator):
    def __init__(self, array_width, *array_size):
        super().__init__("flattenND", True)

        total_dims = array_width * array_size[0]
        apparent_width = array_width
        for i in range(1, len(array_size)):
            total_dims = total_dims * array_size[i]
            apparent_width = apparent_width * array_size[i]

        self.in_ = self.input("input_array",
                              width=array_width,
                              size=array_size,
                              packed=True,
                              explicit_array=True)

        self.out_ = self.output("output_array", width=total_dims)

        self.add_code(self.flatten_array)

    @always_comb
    def flatten_array(self):
        for i in range(array_size[0]):
            self.out_[(i + 1) * apparent_width - 1, i * apparent_width] = self.in_[i]


if __name__ == "__main__":
    # input_array needs to be unpacked for third dimension > 1
    dut = FlattenND(16, 4, 1)
    verilog(dut, filename="flatten.sv")
