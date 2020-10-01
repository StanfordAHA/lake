from lake.collateral2compiler.memory import mem_inst
from lake.collateral2compiler.helper import *


class TopLake():
    def __init__(self):
        self.mem_collateral = {}

        self.memories = {}
        self.mem_insts = []

        self.edges = []
        self.merged_edges = []
        self.compiler_memories = {}

        self.merged_mems = {}
        self.muxes = []

    # default for ports is no ports
    def add_memory(self, mem_params, write_ports=[], read_ports=[], read_write_ports=[]):

        mem_name = mem_params["name"]

        self.get_addl_mem_params(mem_params, write_ports, read_ports, read_write_ports)

        # mem = mem_inst(mem_params, self.mem_collateral)

        mem_params["read_ports"] = read_ports
        mem_params["write_ports"] = write_ports
        # mem = {mem_name: mem_params}
        self.memories[mem_name] = mem_params
        self.compiler_memories[mem_name] = mem_params

    def get_addl_mem_params(self, mem_params, write_ports, read_ports, read_write_ports):
        mem_params["num_write_ports"] = len(write_ports)
        mem_params["num_read_ports"] = len(read_ports)
        mem_params["num_read_write_ports"] = len(read_write_ports)

        # mem_params["write_info"] = [port.port_info for port in write_ports]
        # mem_params["read_info"] = [port.port_info for port in read_ports]
        # mem_params["read_write_info"] = [port.port_info for port in read_write_ports]

    def add_edge(self, edge_params):
        self.edges.append(edge_params)

    # def merge_mems(self):
        

    # after all edges are added
    def banking(self):
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

        print("MEMORIES FROM ", memories_from)
        print()
        print("MEMORIES TO ", memories_to)

        for mem_from in memories_from.keys():
            if len(memories_from[mem_from]) > 1:
                merged_mem = self.memories[memories_from[mem_from][0]]
 
                name = "merged_"
                write_ports = []               
                read_ports = []
                merged_cap = 0
                for m in memories_from[mem_from]:
                    mem = self.memories[m]
                    del self.merged_mems[m]

                    name += mem["name"] + "_"

                    rport = mem["read_ports"].copy()
                    for r in rport:
                        r.set_addr_domain([merged_cap, merged_cap + mem["capacity"]])
                    read_ports += rport
                    wport = mem["write_ports"].copy()
                    for w in wport:
                        w.set_addr_domain([merged_cap, merged_cap + mem["capacity"]])
                    write_ports += wport
                    merged_cap += mem["capacity"]
                    
                merged_mem["name"] = name[:-1]
                merged_mem["capacity"] = merged_cap
                merged_mem["read_ports"] = read_ports
                merged_mem["write_ports"] = write_ports

                self.get_addl_mem_params(merged_mem, write_ports, read_ports, []) 

                print(merged_mem)
                self.merged_mems[name] = merged_mem

        # e = Edge(edge_params)
        # self.edges.append(e)

    def get_compiler_json(self, filename="collateral2compiler.json"):
        for mem in self.merged_mems:
            m = mem_inst(self.merged_mems[mem], self.mem_collateral)
    #        self.mem_insts.append(m)

        get_json(self.mem_collateral, filename)

    def construct_lake(self):
        self.banking()
        self.get_compiler_json()
