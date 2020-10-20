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
                if mem_name in e["to_signal"]:
                    make_input = False
                elif mem_name in e["from_signal"]:
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

            # with static schedule, incoming data is always written
            self.wire(self.mem_insts[in_mem].ports.write, 1)

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

            num_mux_from = len(edge["from_signal"])
            num_mux_to = len(edge["to_signal"])

            from_sigs, to_sigs = "", ""
            for e in edge["from_signal"]:
                from_sigs += e + "_"
            for e in edge["to_signal"]:
                to_sigs += e + "_"

            edge_name = from_sigs + to_sigs + "edge"

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
            readAG = AddrGen(iterator_support=edge["dim"],
                             config_width=self.default_config_width)
            self.add_child(f"{edge_name}_read_addr_gen",
                           readAG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out)

            if self.memories[edge["from_signal"][0]]["num_read_write_ports"] == 0:
                # can assign same read addrs to all the memories
                for i in range(len(edge["from_signal"])):
                    safe_wire(self, self.mem_insts[edge["from_signal"][i]].ports.read_addr, readAG.ports.addr_out)
            else:
                # there needs to be an if valid check here
                for i in range(len(edge["from_signal"])):
                    safe_wire(self, self.mem_insts[edge["from_signal"][i]].ports.read_write_addr, readAG.ports.addr_out)

            if num_mux_from > 1:
                num_mux_bits = clog2(num_mux_from)
                self.mux_sel = self.var(f"{edge_name}_mux_sel",
                                        width=num_mux_bits)

                safe_wire(self, self.mux_sel,
                    readAG.ports.addr_out[self.mem_insts[edge["from_signal"][0]].ports.read_addr.width + num_mux_from - 1,
                    self.mem_insts[edge["from_signal"][0]].ports.read_addr.width])

                comb = self.combinational()
                for i in range(num_mux_from):
                    if_mux_sel = IfStmt(self.mux_sel == i)
                    for j in range(len(edge["to_signal"])):
                        if_mux_sel.then_(self.mem_insts[edge["to_signal"][j]].ports.data_in.assign(self.mem_insts[edge["from_signal"][i]].ports.data_out))
                    comb.add_stmt(if_mux_sel)

            # create output addressor
            writeAG = AddrGen(iterator_support=edge["dim"],
                            config_width=self.default_config_width)
            self.add_child(f"{edge_name}_write_addr_gen",
                           writeAG,
                           clk=self.clk,
                           rst_n=self.rst_n,
                           step=self.valid,
                           mux_sel=forloop.ports.mux_sel_out)

            if self.memories[edge["to_signal"][0]]["num_read_write_ports"] == 0:
                for i in range(len(edge["to_signal"])):
                    safe_wire(self, self.mem_insts[edge["to_signal"][i]].ports.write_addr, writeAG.ports.addr_out)
            else:
                for i in range(len(edge["to_signal"])):
                    safe_wire(self, self.mem_insts[edge["to_signal"][i]].ports.read_write_addr, writeAG.ports.addr_out)

            if num_mux_to > 1:
                num_mux_bits = clog2(num_mux_to)
                self.mux_sel_to = self.var(f"{edge_name}_mux_sel_to",
                                           width=num_mux_bits)

                safe_wire(self, self.mux_sel_to,
                    writeAG.ports.addr_out[self.mem_insts[edge["to_signal"][0]].ports.write_addr.width + num_mux_to - 1,
                    self.mem_insts[edge["to_signal"][0]].ports.write_addr.width])

                comb_mux_to = self.combinational()
                for i in range(num_mux_to):
                    if_mux_sel_to = IfStmt(self.mux_sel_to == i)
                    if_mux_sel_to.then_(self.mem_insts[edge["to_signal"][i]].ports.write.assign(1))
                    if_mux_sel_to.else_(self.mem_insts[edge["to_signal"][i]].ports.write.assign(0))
                    comb_mux_to.add_stmt(if_mux_sel_to)


            # create accessor
            # calculate necessary delay between from_signal to to_signal
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
