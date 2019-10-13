from kratos import *

class DoubleBufferControl(Generator):
    def __init__(self):
        super().__init__("doublebuffer_control")
        self.in_ = self.input("in", 1)
        self.out_ = self.output("out", 1)
        self.wire(self.out_, self.in_)
