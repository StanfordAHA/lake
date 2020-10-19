from kratos import *
from math import log
from lake.collateral2compiler.memory import mem_inst
from lake.utils.util import safe_wire
from lake.collateral2compiler.helper import *
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen


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

        self.default_config_width = 16

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

        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.increment_cycle_count)

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
            print(self.mem_insts[in_mem].ports)
            safe_wire(self, self.mem_insts[in_mem].ports.data_in, self.data_in[i])

            self.valid = self.var(in_mem + "_accessor_vaild", 1)

            forloop = ForLoop(iterator_support=6,
                              config_width=self.default_config_width)
            loop_itr = forloop.get_iter()
            loop_wth = forloop.get_cfg_width()
            
            self.add_child(in_mem + "_forloop",
                forloop,
                clk=self.clk,
                rst_n=self.rst_n,
                step=self.valid)

            newAG = AddrGen(iterator_support=6,
                            config_width=self.default_config_width)
            self.add_child(in_mem + "write_addr_gen",
                           newAG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out)

            if self.memories[in_mem]["num_read_write_ports"] == 0:
                safe_wire(self, self.mem_insts[in_mem].ports.write_addr, newAG.ports.addr_out)
            else:
                safe_wire(self, self.mem_insts[in_mem].ports.read_write_addr, newAG.ports.addr_out)
                
            newSG = SchedGen(iterator_support=6, 
                        config_width=self.default_config_width)
            self.add_child(in_mem + "write_sched_gen",
                           newSG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           mux_sel=forloop.ports.mux_sel_out,
                           cycle_count=self._cycle_count,
                           valid_output=self.valid)

        # wire output data from output memories
        for i in range(len(is_output)):
            out_mem = is_output[i]
            self.wire(self.data_out[i], self.mem_data_outs[subscript_mems.index(out_mem)])  # , self.mem_insts[out_mem].ports.data_out)

            self.valid = self.var(out_mem + "_accessor_vaild", 1)

            forloop = ForLoop(iterator_support=6,
                              config_width=self.default_config_width)
            loop_itr = forloop.get_iter()
            loop_wth = forloop.get_cfg_width()
            
            self.add_child(out_mem + "_forloop",
                forloop,
                clk=self.clk,
                rst_n=self.rst_n,
                step=self.valid)

            newAG = AddrGen(iterator_support=6,
                            config_width=self.default_config_width)
            self.add_child(out_mem + "write_addr_gen",
                           newAG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out)

            if self.memories[in_mem]["num_read_write_ports"] == 0:
                safe_wire(self, self.mem_insts[in_mem].ports.read_addr, newAG.ports.addr_out)
            else:
                safe_wire(self, self.mem_insts[in_mem].ports.read_write_addr, newAG.ports.addr_out)
                
            newSG = SchedGen(iterator_support=6, 
                        config_width=self.default_config_width)
            self.add_child(out_mem + "read_sched_gen",
                           newSG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           mux_sel=forloop.ports.mux_sel_out,
                           cycle_count=self._cycle_count,
                           valid_output=self.valid)

        for edge in self.edges:

            edge_name = edge["from_signal"] + "_" + edge["to_signal"] + "_edge"

            # create mux if needed 
            # (need to check if previously created)
            need_mux = None
            for mux in self.muxes:
                if edge["to_signal"] in mux[1:]:
                    need_mux = mux
                
            if need_mux is not None:
                num_signals = len(mux) - 1
                
            # create accessor, forloop

            self.valid = self.var(edge_name + "_accessor_vaild", 1)

            forloop = ForLoop(iterator_support=edge["dim"])
            loop_itr = forloop.get_iter()
            loop_wth = forloop.get_cfg_width()

            self.add_child(edge_name + "_forloop",
                forloop,
                clk=self.clk,
                rst_n=self.rst_n,
                step=self.valid)

            # create input addressor
            newAG = AddrGen(iterator_support=edge["dim"],
                            config_width=self.default_config_width)
            self.add_child(edge["to_signal"] + "write_addr_gen",
                           newAG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           # addr_out=self._agg_write_addr[i])
                           mux_sel=forloop.ports.mux_sel_out)

#            safe_wire(self, self._agg_write_addr[i], newAG.ports.addr_out)

            # create output addressor

            # create accessor

            #        @always_comb
            #        def mux_gen():
            #            if mux_sel

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def increment_cycle_count(self):
        if ~self.rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1

    #@always_comb
    #def mux_gen():
    #    for i in range(num_signals):
    #        if mux_sel == i:
    #            self.m_data_out = self.m_data_in[i]
      
if __name__ == "__main__":
    a = True
