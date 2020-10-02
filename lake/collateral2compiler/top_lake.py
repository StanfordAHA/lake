from lake.collateral2compiler.memory import mem_inst
from lake.collateral2compiler.edge import edge_inst
from lake.collateral2compiler.helper import *
# from lake.collateral2compiler.edge 


class TopLake():
    def __init__(self):
        self.mem_collateral = {}
        self.edge_collateral = {}

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
        mem_params["read_write_ports"] = read_write_ports
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

        # print("MEMORIES FROM ", memories_from)
        # print()
        # print("MEMORIES TO ", memories_to)

        self.merge_mems(memories_from)
        self.merge_mems(memories_to)
        # e = Edge(edge_params)
        # self.edges.append(e)

    def merge_mems(self, mems_to_merge):
        for mem in mems_to_merge.keys():
            if len(mems_to_merge[mem]) > 1:
                merged_mem = self.memories[mems_to_merge[mem][0]]

                name = "merged_"
                write_ports = []
                read_ports = []
                rw_ports = []
                merged_cap = 0
                for m in mems_to_merge[mem]:
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
                    rwport = mem["read_write_ports"].copy()
                    for rw in rwport:
                        rw.set_addr_domain([merged_cap, merged_cap + mem["capacity"]])
                    write_ports += wport
                    merged_cap += mem["capacity"]

                merged_mem["name"] = name[:-1]
                merged_mem["capacity"] = merged_cap
                merged_mem["read_ports"] = read_ports
                merged_mem["write_ports"] = write_ports
                merged_mem["read_write_ports"] = rw_ports

                self.get_addl_mem_params(merged_mem, write_ports, read_ports, [])

                # print(merged_mem)
                self.merged_mems[name] = merged_mem

    def get_compiler_json(self, filename="collateral2compiler.json"):
        for mem in self.merged_mems:
            m = mem_inst(self.merged_mems[mem], self.mem_collateral)
    #        self.mem_insts.append(m)

        for edge in self.edges:
            e = edge_inst(edge, self.edge_collateral)
        get_json(self.mem_collateral, self.edge_collateral, filename)

    def construct_lake(self):
        self.banking()
        self.get_compiler_json()
