from kratos import *


class PassThroughMod(Generator):
    def __init__(self):
        super().__init__("PassThrough")

        self._clk = self.clock("clk")
        self._data_in = self.input("data_in", 1)
        self._data_out = self.output("data_out", 1)
        self.wire(self._data_out, self._data_in)
