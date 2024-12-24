import kratos as kts
from kratos import *

class Mux(Generator):
    def __init__(self, height: int, width: int):
        name = "Mux_{0}_{1}".format(width, height)
        super().__init__(name)

        # pass through wires
        if height == 1:
            self.in_ = self.input("I", width)
            self.out_ = self.output("O", width)
            self.wire(self.out_, self.in_)
            return

        self.sel_size = clog2(height)
        input_ = self.input("I", width, size=height)
        self.out_ = self.output("O", width)
        self._sel = self.input("S", self.sel_size)

        # add a combinational block
        comb = self.combinational()

        # add a case statement
        switch_ = comb.switch_(self.ports.S)
        for i in range(height):
            switch_.case_(i, self.out_(input_[i]))
        # add default
        switch_.case_(None, self.out_(0))

if __name__ == "__main__":

    mux_dut = Mux(height=4, width=16)

    verilog(mux_dut, filename="mux.sv",
            optimize_if=False)
