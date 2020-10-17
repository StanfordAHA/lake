from kratos import *
from math import log
from lake.collateral2compiler.memory import mem_inst
from lake.utils.util import safe_wire
from lake.collateral2compiler.helper import *


class TopLakeHW(Generator):
    def __init__(self,
                 word_width,
                 input_ports,
                 output_ports,
                 memories):

        super().__init__("lake_top", debug=True)

        # parameters
        self.word_width = word_width
        self.input_ports = input_ports
        self.output_ports = output_ports

        # inputs
        self.clk = self.clock("clk")
        # active low asynchornous reset
        self.rst_n = self.reset("rst_n", 1)

        self.data_in = self.input("data_in",
                                  width=self.word_width,
                                  size=self.input_ports,
                                  explicit_array=True,
                                  packed=True)

        self.data_out = self.output("data_out",
                                    width=self.word_width,
                                    size=self.output_ports,
                                    explicit_array=True,
                                    packed=True)

        self.memories = memories

        i = 0
        for mem in self.memories.keys():
            m = mem_inst(self.memories[mem])
            self.add_child(f"memory_{i}",
                           m,
                           clk=self.clk,
                           rst_n=self.rst_n)
            i += 1

if __name__ == "__main__":
    a = True
