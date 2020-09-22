from lake.collateral2compiler.memory import mem_inst


class Lake():
    def __init__(self):
        self.mem_collateral = {}

        self.memories = []
        self.edges = []

        self.compiler_memories = []

        self.muxes = []

    # default for ports is no ports
    def add_memory(self, mem_params, write_ports=[], read_ports=[], read_write_ports=[]):

        mem_name = mem_params["name"]

        get_addl_mem_params(mem_params)

        # mem = mem_inst(mem_params, self.mem_collateral)

        mem_params["read_ports"] = read_ports
        mem_params["write_ports"] = write_ports
        mem = {mem_name: mem_params}
        self.memories.append(mem)
        self.compiler_memories.append(mem)

    def get_addl_mem_params(self, mem_params):
        mem_params["num_write_ports"] = len(write_ports)
        mem_params["num_read_ports"] = len(read_ports)
        # mem_params["num_read_write_ports"] = len(read_write_ports)

        # mem_params["write_info"] = [port.port_info for port in write_ports]
        # mem_params["read_info"] = [port.port_info for port in read_ports]
        # mem_params["read_write_info"] = [port.port_info for port in read_write_ports]

    def add_edge(self, edge_params):
        self.edges.append(edge_params)

    # after all edges are added
    def banking(self):
        from_signal = edge_params["from_signal"]
        to_signal = edge_params["to_signal"]

        # infer mux
        share_from = []
        share_to = []
        for edge in edges:
            if edge["from_signal"] == from_signal:
                share_from.append(edge["to_signal"])
                # share_from.append(edge["to_signal"].split(".")[0])
            if edge["to_signal"] == to_signal:
                share_to.append(edge["from_signal"])
                # share_to.append(edge["from_signal"].split(".")[0])

        if len(share_from) > 0:
            from_mem_params = [self.memories[mem] for mem in share_from]
            # for now assuming read_width is the same - is this assumption okay
            merged_mem = from_mem_params[0]
            merged_cap = 0

            for params in from_mem_params:
                assert params["num_write_ports"] == from_mem_params[0]["num_write_ports"]
                assert params["write_port_width"] == from_mem_params[0]["write_port_width"]
                # assert params["write_info"] == from_mem_params[0]["write_info"]

            read_ports = []
            for i in range(len(share_from)):
                mem = share_from[i]

                # delete individual memories for compiler (no mux)
                assert mem in self.compiler_memories
                del self.compiler_memories[mem]

                for read_port in mem["read_ports"]:
                    read_port.set_addr_domain({merged_cap, mem["capacity"] - 1})

                read_ports += mem["read_ports"]
                merged_cap += mem["capacity"]

            get_addl_mem_params(mem_params)

            merged_mem["capacity"] = merged_cap

        if len(share_to) > 0:
            to_mem_params = [self.memories[mem] for mem in share_to]

        e = Edge(edge_params)
        self.edges.append(e)

    def get_compiler_json(self, filename="collateral2compiler.json"):
        for mem in self.memories:
            get_memory_params(mem, self.mem_collateral)

        get_json(self.mem_collateral, filename)

    def construct_lake(self):
        self.banking()
        self.get_compiler_json()
