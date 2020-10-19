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

        self.mem_data_outs = [self.var(f"mem_data_out_{i}",
                                       width=self.word_width,
                                       size=self.memories[subscript_mems[i]]["read_port_width" if "read_port_width" in self.memories[subscript_mems[i]] else "read_write_port_width"],
                                       explicit_array=True) for i in range(num_mem)]
        # packed=True) for i in range(num_mem)]

        self.mem_insts = {}

        i = 0
        for mem in self.memories.keys():
            m = mem_inst(self.memories[mem])
            self.mem_insts[self.memories[mem]["name"]] = m

            self.add_child(self.memories[mem]["name"],
                           m,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           data_out=self.mem_data_outs[i])
            i += 1

        # wire input and output data
        is_input, is_output = [], []
        for mem_name in self.mem_insts.keys():
            make_input, make_output = True, True
            for e in self.edges:
                if e["to_signal"] == mem_name:
                    make_input = False
                elif e["from_signal"] == mem_name:
                    make_output = False
            if make_input:
                is_input.append(mem_name)
            elif make_output:
                is_output.append(mem_name)

        assert len(is_input) == self.input_ports
        assert len(is_output) == self.output_ports

        # wire input data to input memories
        for i in range(len(is_input)):
            in_mem = is_input[i]
            safe_wire(self, self.mem_insts[in_mem].ports.data_in, self.data_in[i])

        # wire output data from output memories
        for i in range(len(is_output)):
            out_mem = is_output[i]
            self.wire(self.data_out[i], self.mem_data_outs[subscript_mems.index(out_mem)])  # , self.mem_insts[out_mem].ports.data_out)

        for edge in self.edges:
            # create mux if needed

            # create accessor

            # create input addressor

            # create output addressor

            # create accessor

            #        @always_comb
            #        def mux_gen():
            #            if mux_sel


if __name__ == "__main__":
    a = True
