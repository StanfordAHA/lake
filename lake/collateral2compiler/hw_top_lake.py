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
                 memories,
                 edges,
                 muxes):

        super().__init__("lake_top", debug=True)

        # parameters
        self.word_width = word_width
        self.input_ports = input_ports
        self.output_ports = output_ports

        # objects
        self.memories = memories
        self.edges = edges
        self.muxes = muxes

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

        num_mem = len(memories)
        subscript_mems = list(self.memories.keys())
        self.mem_data_outs = []
        self.mem_data_outs = [self.var(f"mem_data_out_{i}",
                     width=self.word_width,
                     size=self.memories[subscript_mems[i]]["read_port_width" if "read_port_width" in self.memories[subscript_mems[i]] else "read_write_port_width"],
                     explicit_array=True,
                     packed=True) for i in range(num_mem)]
        i = 0
        for mem in self.memories.keys():
            m = mem_inst(self.memories[mem])
            if "read_port_width" in self.memories[mem]:
                dim_size = "read_port_width"
            else:
                dim_size = "read_write_port_width"

            self.add_child(f"memory_{i}",
                           m,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           data_in=0,
                           data_out=self.mem_data_outs[i])
#            self.mem_data_outs.append(self.mem_data_out)
            i += 1

#        for mux in self.muxes:
        self.wire(self.data_out, self.data_in)

#        @always_comb
#        def mux_gen():
#            if mux_sel


if __name__ == "__main__":
    a = True
