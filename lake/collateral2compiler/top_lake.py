import copy

from lake.collateral2compiler.memory import mem_inst, port_to_info
from lake.collateral2compiler.edge import edge_inst, get_full_edge_params
from lake.collateral2compiler.helper import *
from lake.collateral2compiler.hw_top_lake import TopLakeHW


class TopLake():
    def __init__(self,
                 word_width,
                 input_ports,
                 output_ports):

        # TO DO clean up

        # parameters
        self.word_width = word_width
        self.input_ports = input_ports
        self.output_ports = output_ports

        self.mem_collateral = {}
        self.edge_collateral = {}

        self.merged_mems = {}
        self.merged_edges = []

        self.hw_memories = {}
        self.hw_edges = []

        self.mux_count = 0
        self.muxes = {}

        self.memories = {}
        self.mem_insts = {}

        self.edges = []
        self.compiler_mems = {}

    # default for ports is no ports
    def add_memory(self, mem_params, write_ports=[], read_ports=[], read_write_ports=[]):

        mem_name = mem_params["name"]

        self.get_addl_mem_params(mem_params, write_ports, read_ports, read_write_ports)

        # mem = mem_inst(mem_params, self.mem_collateral)

        mem_params["read_ports"] = read_ports
        mem_params["write_ports"] = write_ports
        mem_params["read_write_ports"] = read_write_ports

        self.memories[mem_name] = mem_params

    def get_addl_mem_params(self, mem_params, write_ports, read_ports, read_write_ports):
        mem_params["num_write_ports"] = len(write_ports)
        mem_params["num_read_ports"] = len(read_ports)
        mem_params["num_read_write_ports"] = len(read_write_ports)

    def add_edge(self, edge_params):
        get_full_edge_params(edge_params)
        self.edges.append(edge_params)

    # after all edges are added
    def banking(self):
        self.hw_memories = copy.deepcopy(self.memories)
        self.hw_edges = copy.deepcopy(self.edges)

        memories_from = {}
        memories_to = {}
        for mem in self.memories.keys():
            memories_from[mem] = []
            memories_to[mem] = []
            self.merged_mems[mem] = self.memories[mem]

        for mem in self.memories.keys():
            for edge in self.edges:
                if edge["from_signal"] == mem:
                    memories_to[mem].append(edge["to_signal"])
                if edge["to_signal"] == mem:
                    memories_from[mem].append(edge["from_signal"])

        # print("MEMORIES FROM ", memories_from)
        # print()
        # print("MEMORIES TO ", memories_to)

        self.merge_mems(memories_from, 1)
        self.merge_mems(memories_to, 0)
        # e = Edge(edge_params)
        # self.edges.append(e)

    def merge_mems(self, mems_to_merge, is_from):
        for mem in mems_to_merge.keys():
            # print("MEMORY ", mem, " ", mems_to_merge[mem])
            if len(mems_to_merge[mem]) > 1:
                merged_mem = self.memories[mems_to_merge[mem][0]]

                name = "merged_"
                write_ports = []
                read_ports = []
                rw_ports = []
                merged_cap = 0
                for m in mems_to_merge[mem]:
                    mem_ = self.memories[m]
                    del self.merged_mems[m]

                    name += mem_["name"] + "_"

                    # add mux connections for HW generation
                    if is_from:
                        check, not_check = "from", "to"
                    else:
                        check, not_check = "to", "from"

                    for e in self.hw_edges:
                        if e[check + "_signal"] == mem_["name"]:
                            while f"mux_{self.mux_count}" in self.hw_memories.keys():
                                self.mux_count += 1
                            e[not_check + "_signal"] = f"mux_{self.mux_count}"
                            if f"mux_{self.mux_count}" not in self.muxes:
                                self.muxes[f"mux_{self.mux_count}"] = [check, mem_["name"]]
                            else:
                                self.muxes[f"mux_{self.mux_count}"].append(mem_["name"])
                                
                            break

                    to_edge = {check + "_signal": f"mux_{self.mux_count}",
                               not_check + "_signal": mem}
                    get_full_edge_params(to_edge)
                    if to_edge not in self.hw_edges:
                        self.hw_edges.append(to_edge)

                    # get compiler related information for merged memories
                    rport = mem_["read_ports"].copy()
                    for r in rport:
                        r.set_addr_domain([merged_cap, merged_cap + mem_["capacity"]])
                    read_ports += rport
                    wport = mem_["write_ports"].copy()
                    for w in wport:
                        w.set_addr_domain([merged_cap, merged_cap + mem_["capacity"]])
                    rwport = mem_["read_write_ports"].copy()
                    for rw in rwport:
                        rw.set_addr_domain([merged_cap, merged_cap + mem_["capacity"]])
                    write_ports += wport
                    merged_cap += mem_["capacity"]

                merged_mem["name"] = name[:-1]
                merged_mem["capacity"] = merged_cap
                merged_mem["read_ports"] = read_ports
                merged_mem["write_ports"] = write_ports
                merged_mem["read_write_ports"] = rw_ports

                if not is_from:
                    # print("NOT IS FROM ", mem, merged_mem["name"])
                    self.merged_edges.append({"from_signal": mem, "to_signal": merged_mem["name"]})
                else:
                    # print("IS FROM ", merged_mem["name"], mem)
                    self.merged_edges.append({"to_signal": mem, "from_signal": merged_mem["name"]})

                self.get_addl_mem_params(merged_mem, write_ports, read_ports, [])

                # print(merged_mem)
                self.merged_mems[name] = merged_mem

            # need to handle case where memories are not merged
            # else:
                # self.merged_edges.append({"from_signal": mem, "to_signal": mems_to_merge[mem]})

            self.mux_count += 1

    def get_compiler_json(self, filename="collateral2compiler.json"):

        # print(self.merged_mems)
        # print(self.merged_edges)
        # print(self.hw_edges)
        # print(self.muxes)

        for mem in self.merged_mems:
            params = port_to_info(self.merged_mems[mem])
            self.compiler_mems[mem] = params

        # print(self.compiler_mems)
        get_json(self.compiler_mems, self.merged_edges, filename)

    def generate_hardware(self):
        hw = TopLakeHW(self.word_width,
                  self.input_ports,
                  self.output_ports,
                  self.hw_memories,
                  self.hw_edges,
                  self.muxes)

        verilog(hw, filename="Lake_hw.sv")

    def get_compiler_collateral(self):
        self.banking()
        self.get_compiler_json()

    def construct_lake(self):
        self.banking()
        self.get_compiler_json()
        self.generate_hardware()

    
