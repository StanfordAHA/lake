import copy

from lake.collateral2compiler.memory import mem_inst, port_to_info
from lake.collateral2compiler.edge import edge_inst, get_full_edge_params
from lake.collateral2compiler.helper import *
from lake.collateral2compiler.hw_top_lake import TopLakeHW
from lake.utils.sram_macro import SRAMMacroInfo
from lake.passes.passes import change_sram_port_names
from lake.modules.cfg_reg_wrapper import CFGRegWrapper


class TopLake():
    def __init__(self,
                 word_width,
                 input_ports,
                 output_ports):

        # parameters
        self.word_width = word_width
        self.input_ports = input_ports
        self.output_ports = output_ports

        # what user adds
        self.memories = {}
        self.edges = []

        # what compiler sees
        self.compiler_mems = {}
        self.merged_mems = {}
        self.merged_edges = []

        # what hardware sees
        self.hw_memories = {}
        self.hw_edges = []
        self.hardware_edges = []

        # originally created for hardware, but not used
        # keeping here in case logic is useful for the future
        self.mux_count = 0
        self.muxes = {}

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

    # TO DO put the defaults here instead of the additional function
    def add_edge(self,
                 from_signal,
                 to_signal,
                 dim=6,
                 max_range=65536,
                 max_stride=65536):

        edge_params = {"from_signal": from_signal,
                       "to_signal": to_signal,
                       "dim": dim,
                       "max_range": max_range,
                       "max_stride": max_stride}

        # check if producer and consumer port widths match
        from_key, to_key = "read_port_width", "write_port_width"
        if self.memories[edge_params['from_signal']]["num_read_write_ports"] > 0:
            from_key = "read_write_port_width"
        if self.memories[edge_params['to_signal']]["num_read_write_ports"] > 0:
            to_key = "read_write_port_width"

        # producer and consumer port widths must match
        assert self.memories[edge_params['from_signal']][from_key] == self.memories[edge_params['to_signal']][to_key]
        self.edges.append(edge_params)

    # TO DO add to top_lake logic for merging input and output mems if multiple per port
    def add_input_edge(self, port, mem_name, dim=6, max_range=65536, max_stride=65536):
        self.memories[mem_name]["input_edge_params"] = \
            {"dim": dim, "max_range": max_range, "max_stride": max_stride}
        self.memories[mem_name]["is_input"] = True
        self.memories[mem_name]["input_port"] = port

    def add_output_edge(self, port, mem_name, dim=6, max_range=65536, max_stride=65536):
        self.memories[mem_name]["output_edge_params"] = \
            {"dim": dim, "max_range": max_range, "max_stride": max_stride}
        self.memories[mem_name]["is_output"] = True
        self.memories[mem_name]["output_port"] = port

    def add_io_mem_tags(self):
        for mem in self.memories.keys():
            if "is_input" not in self.memories[mem].keys():
                self.memories[mem]["is_input"] = False
            if "is_output" not in self.memories[mem].keys():
                self.memories[mem]["is_output"] = False

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

        for mem in memories_from:
            if len(memories_from[mem]) > 1:
                for e in self.edges:
                    if e["to_signal"] == mem:
                        x = copy.deepcopy(e)
                        x["from_signal"] = memories_from[mem]
                        if x not in self.hardware_edges:
                            self.hardware_edges.append(x)

        for mem in memories_to:
            if len(memories_to[mem]) > 1:
                for e in self.edges:
                    if e["from_signal"] == mem:
                        x = copy.deepcopy(e)
                        x["to_signal"] = memories_to[mem]
                        if x not in self.hardware_edges:
                            self.hardware_edges.append(x)

        for h in self.hardware_edges:
            for sig in ("to_signal", "from_signal"):
                if not isinstance(h[sig], list):
                    h[sig] = [h[sig]]

        for e in self.edges:
            add = 1
            for h in self.hardware_edges:
                if e["from_signal"] in h["from_signal"] and e["to_signal"] in h["to_signal"]:
                    add = 0
                    break
            if add == 1:
                self.hardware_edges.append(e)

        self.merge_mems(memories_from, True)
        self.merge_mems(memories_to, False)

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
                    # TO DO this should be based on min dim, range, and stride
                    # from the original mems that are being muxed - TO DO make this not
                    # a limitation, perhaps with a port restriction?
                    get_full_edge_params(to_edge)
                    if to_edge not in self.hw_edges:
                        self.hw_edges.append(to_edge)

                    # get compiler related information for merged memories
                    rport = mem_["read_ports"].copy()
                    for r in rport:
                        r.set_addr_domain([merged_cap, merged_cap + mem_["capacity"] - 1])
                    read_ports += rport
                    wport = mem_["write_ports"].copy()
                    for w in wport:
                        w.set_addr_domain([merged_cap, merged_cap + mem_["capacity"] - 1])
                    rwport = mem_["read_write_ports"].copy()
                    for rw in rwport:
                        rw.set_addr_domain([merged_cap, merged_cap + mem_["capacity"] - 1])
                    write_ports += wport
                    merged_cap += mem_["capacity"]

                merged_mem["name"] = name[:-1]
                merged_mem["capacity"] = merged_cap
                merged_mem["read_ports"] = read_ports
                merged_mem["write_ports"] = write_ports
                merged_mem["read_write_ports"] = rw_ports

                if is_from:
                    # print("IS FROM ", merged_mem["name"], mem)
                    self.merged_edges.append({"to_signal": mem, "from_signal": merged_mem["name"]})
                else:
                    # print("NOT IS FROM ", mem, merged_mem["name"])
                    self.merged_edges.append({"from_signal": mem, "to_signal": merged_mem["name"]})

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

    def generate_hardware(self, wrap_cfg=True):
        # print(self.hw_memories)
        # print()
        # print(self.hardware_edges)

        hw = TopLakeHW(self.word_width,
                       self.input_ports,
                       self.output_ports,
                       self.hw_memories,
                       self.hardware_edges)

        # Wrap it if we need to...
        if wrap_cfg:
            hw = CFGRegWrapper(hw)

        return hw

    def test_magma_lake(self, wrap_cfg=False):
        # identify which memories are not inputs and/or outputs
        self.add_io_mem_tags()
        # prepare user input for compiler collateral and hardware
        self.banking()
        # generate compiler collateral
        self.get_compiler_json()
        # generate RTL
        hw = self.generate_hardware(wrap_cfg)

        return hw

    def construct_lake(self, filename="Lake_hw.sv", wrap_cfg=False):
        hw = self.test_magma_lake(wrap_cfg)

        tsmc_info = SRAMMacroInfo("tsmc_name")
        sram_port_pass = change_sram_port_names(use_sram_stub=False, sram_macro_info=tsmc_info)
        verilog(hw, filename=filename,
                check_multiple_driver=False,
                optimize_if=False,
                check_flip_flop_always_ff=False,
                additional_passes={"change sram port names": sram_port_pass})
