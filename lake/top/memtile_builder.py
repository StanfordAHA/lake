from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_interface import *
from lake.top.memory_controller import *
from lake.attributes.formal_attr import *
import kratos as kts


class MemoryTileBuilder(kts.Generator):
    '''
    This class provides utilities to add memories and memory controllers and automagically generate the hardware
    '''
    def __init__(self, name, debug, memory_interface: MemoryInterface = None, memory_banks=1, controllers=[]):

        super().__init__(name, debug)

        self.memory_interface = memory_interface
        self.memory_banks = memory_banks
        self.controllers = controllers
        self.num_modes = 0

        self.memories = []

        self.controllers_finalized = False

        self.inputs_dict = {}
        self.outputs_dict = {}

        self.mem_conn = None
        if memory_interface is not None:
            self.allocate_mem_conn()

        # CLK and RST
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(self._clk.name, FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(self._rst_n.name, FormalSignalConstraint.RSTN))

    def allocate_mem_conn(self):
        self.mem_conn = [[{}] * len(self.memory_interface.get_ports())] * self.memory_banks

    def set_banks(self, banks):
        self.memory_banks = banks

    def set_memory_interface(self, name_prefix, mem_params, ports, sim_macro_n, tech_map):
        self.memory_interface = MemoryInterface(name=f"base_memory_interface",
                                                mem_params=mem_params,
                                                ports=ports,
                                                sim_macro_n=sim_macro_n,
                                                tech_map=tech_map)
        self.memories = [MemoryInterface(name=f"{name_prefix}_{i}",
                                         mem_params=mem_params,
                                         ports=ports,
                                         sim_macro_n=sim_macro_n,
                                         tech_map=tech_map) for i in range(self.memory_banks)]
        # Clear and reallocate the new memory port mappings
        self.allocate_mem_conn()

    def get_memory_interface(self, mem: MemoryInterface):
        return self.memory_interface

    def finalize_controllers(self):
        self.controllers_finalized = True
        self.num_modes = len(self.controllers)
        # Create the mode config reg if we have multiple controllers,
        # otherwise we can just wire it to 0 and let synth handle it?
        if self.num_modes > 1:
            self._mode = self.input("mode", kts.clog2(self.num_modes))
            self._mode.add_attribute(ConfigRegAttr("MODE!"))

        self.resolve_memports()
        self.resolve_inputs()
        self.resolve_outputs()

    def size_to_port(self, io_list):
        '''
        io_list contains a set of (signal_name, width) tuples
        This function forms a dictionary where the keys are the widths and
        values are a list of ports of that size
        '''
        s_to_p = {}
        for (signal, width) in io_list:
            if width not in s_to_p:
                s_to_p[width] = []
            s_to_p[width].append(signal)
        return s_to_p

    def resolve_memports(self):
        '''
        This function finds the connections between each controller
        and the requested ports
        '''
        # TODO: Eventually want to add heuristics for resolving the memory ports if a designer
        # doesn't want to know ahead of time - for static scheduling, however, it's crucial

        for mem_ctrl in self.controllers:
            ctrl_name = str(mem_ctrl)
            # Now get the port request - controllers should only request ports
            # based on knowledge of the memory system
            memport_req = mem_ctrl.get_memory_ports()
            # For each bank and port, the controller will either include its own port interface or None
            for bank in range(len(memport_req)):
                for port in range(len(memport_req[0])):
                    if memport_req[bank][port] is not None:
                        self.mem_conn[bank][port][str(mem_ctrl)] = memport_req[bank][port]

    def resolve_inputs(self):
        '''
        This function finds the connections between each controller
        and the requested inputs
        '''
        # Go through each controller and get all the inputs - figure out maximum of each size
        # and indicate in which mode which input goes to which port in the controllers
        # Go through controllers
        for mem_ctrl in self.controllers:
            ctrl_name = str(mem_ctrl)
            ctrl_ins = mem_ctrl.get_inputs()
            # Create a dict from port width to a list of signals with that width
            stop_in = self.size_to_port(ctrl_ins)
            self.merge_io_dicts(to_merge=stop_in, merged_dict=self.inputs_dict, mem_ctrl=mem_ctrl)

    def resolve_outputs(self):
        '''
        This function finds the connections between each controller
        and the requested inputs
        '''
        # Go through each controller and get all the inputs - figure out maximum of each size
        # and indicate in which mode which input goes to which port in the controllers
        # Go through controllers
        for mem_ctrl in self.controllers:
            ctrl_name = str(mem_ctrl)
            ctrl_outs = mem_ctrl.get_outputs()
            # Create a dict from port width to a list of signals with that width
            stop_out = self.size_to_port(ctrl_outs)
            self.merge_io_dicts(to_merge=stop_out, merged_dict=self.outputs_dict, mem_ctrl=mem_ctrl)

    def merge_io_dicts(self, to_merge, merged_dict, mem_ctrl):
        '''
        Merges the set of signal indicators into the global list for the tile
        '''
        for (width, signal_list) in to_merge.items():
            # At each input, we can create a dictionary of lists of dictionaries for merging inputs
            if width not in merged_dict:
                merged_dict[width] = []
            # Now check that there's enough room for this number of ports
            num_sigs_needed = len(signal_list)
            num_sigs_have = len(merged_dict[width])
            if num_sigs_needed > num_sigs_have:
                # Create dictionary for the new signals
                for z in range(num_sigs_needed - num_sigs_have):
                    merged_dict[width].append({})
            # Now we can add each signal to the dictionary mapped from each port
            for i in range(num_sigs_needed):
                # print(f'signal_list[i] = {signal_list[i]}')
                merged_dict[width][i][str(mem_ctrl)] = str(signal_list[i])
                # print(f"merged_dict_{i}: {merged_dict}")

    def add_memory_controller(self, mem_ctrl: MemoryController):
        if not self.controllers_finalized:
            self.controllers.append(mem_ctrl)
        else:
            print(f"Controllers finalized - {mem_ctrl} isn't being added...")

    def add_config_hooks(self, config_data_width=32, config_addr_width=8):
        pass

    def __str__(self):
        rep_str = "=====MEMORY TILE BUILDER=====\n===CONTROLLERS===\n"
        rep_str += f"{self.controllers}\n===INPUTS===\n{self.inputs_dict}\n"
        rep_str += f"===OUTPUTS===\n{self.outputs_dict}\n===MEMPORTS===\n"
        for i in range(self.memory_banks):
            for j in range(self.memory_interface.get_num_ports()):
                rep_str += f"BANK: {i}\t PORT: {j}\n"
                for (k, (ctrl, port)) in enumerate(self.mem_conn[i][j].items()):
                    rep_str += f"CONN {k}\tCONTROLLER: {ctrl}\tPORT: {port}\n"
        return rep_str
