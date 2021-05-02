from kratos.stmts import IfStmt
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_interface import *
from lake.top.memory_controller import *
from lake.attributes.formal_attr import *
import kratos as kts


class MemoryTileFinalizedException(Exception):
    pass


class MemoryTileBuilder(kts.Generator):
    '''
    This class provides utilities to add memories and memory controllers and automagically generate the hardware
    '''

    # Need to provide legal widths for top level signals - inputs/outputs - for compatibility
    # with the standard routing network on the AHA CGRA
    legal_widths = [16, 1]

    def __init__(self, name, debug, memory_interface: MemoryInterface = None, memory_banks=1, controllers=[]):

        super().__init__(name, debug)

        self.memory_interface = memory_interface
        self.memory_banks = memory_banks
        self.controllers = controllers
        self.controllers_flat = []
        self.controllers_dict = {}
        self.controllers_flat_dict = {}
        for mem_ctrl in self.controllers:
            self.controllers_dict[mem_ctrl.name] = mem_ctrl
            flat_wrap = MemoryControllerFlatWrapper(mem_ctrl=mem_ctrl, legal_list=MemoryTileBuilder.legal_widths)
            self.controllers_flat_dict[flat_wrap.name] = flat_wrap
            self.controllers_flat.append(flat_wrap)
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
        for mem_ctrl in self.controllers_flat:
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
        for mem_ctrl in self.controllers_flat:
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
                merged_dict[width][i][str(mem_ctrl)] = str(signal_list[i])

    def add_memory_controller(self, mem_ctrl: MemoryController):
        # When we add a memory controller, we really create a flattened wrapper
        # so that we can use it easily after the fact
        if not self.controllers_finalized:
            self.controllers.append(mem_ctrl)
            self.controllers_dict[mem_ctrl.name] = mem_ctrl
            flat_wrap = MemoryControllerFlatWrapper(mem_ctrl=mem_ctrl, legal_list=MemoryTileBuilder.legal_widths)
            self.controllers_flat_dict[flat_wrap.name] = flat_wrap
            self.controllers_flat.append(flat_wrap)
        else:
            raise MemoryTileFinalizedException(f"Controllers finalized - {mem_ctrl} isn't being added...")

    def add_config_hooks(self, config_data_width=32, config_addr_width=8):
        pass

    def realize_hw(self):
        '''
        Go through the motions of finally creating the hardware
        '''
        assert self.controllers_finalized, f"Controllers need to be finalized before realizing HW"
        # To realize the hardware, we basically need to create each input and output in the dictionaries
        # Then, wire up the memory ports with muxes + mode as select signal, then realize the underlying memory
        # Need to flatten stuff...
        self.realize_controllers()
        self.realize_inputs()
        self.realize_outputs()
        for (idx, mem) in enumerate(self.memories):
            mem.realize_hw()
            self.add_child(f"memory_{idx}", mem)
        self.realize_mem_connections()

    def realize_controllers(self):
        for (ctrl_name, ctrl) in self.controllers_flat_dict.items():
            self.add_child(f'mem_ctrl_{ctrl_name}', ctrl)
            # Wire clk and rst....making some decent assumptions here
            self.wire(self._clk, ctrl.ports['clk'])
            self.wire(self._rst_n, ctrl.ports['rst_n'])

    def realize_inputs(self):
        '''
        This function creates the inputs to the tile, tags them, and wires them to the ports of the controllers
        '''
        for (input_width, signal_dicts) in self.inputs_dict.items():
            for (i, signal_dict) in enumerate(signal_dicts):
                new_input = self.input(f'input_width_{input_width}_num_{i}', width=input_width)
                isctrl = input_width == 1
                new_input.add_attribute(ControlSignalAttr(isctrl))
                for (ctrl_name, port) in signal_dict.items():
                    self.wire(new_input, self.controllers_flat_dict[ctrl_name].ports[port])

    def realize_outputs(self):
        '''
        This function creates the outputs from the tile, tags them, and wires them to the ports of the controllers
        '''
        # print(self.controllers_flat_dict)
        for (output_width, signal_dicts) in self.outputs_dict.items():
            for (i, signal_dict) in enumerate(signal_dicts):
                new_output = self.output(f'output_width_{output_width}_num_{i}', width=output_width)
                new_output.add_attribute(ControlSignalAttr(False))
                # We need to choose which output is hooked up based on the mode...
                mux_size = len(signal_dict.keys())
                # print(f"Mux size: {mux_size}")
                mux_comb = self.combinational()
                prev_stmt = None
                # Default assign 0 to prevent latches.
                mux_comb.add_stmt(new_output.assign(0))
                for (idx, (ctrl_name, port)) in enumerate(signal_dict.items()):
                    if mux_size == 1:
                        mux_comb.add_stmt(new_output.assign(self.controllers_flat_dict[ctrl_name].ports[port]))
                    else:
                        if idx == 0:
                            first_if = mux_comb.if_(self._mode == kts.const(idx, width=self._mode.width))
                            first_if.then_(new_output.assign(0))
                            # first_if.then_(new_output.assign(self.controllers_flat_dict[ctrl_name].ports[port]))
                            # print(port_to_assign)
                            # print(self.controllers_flat_dict[ctrl_name].ports[port])
                            prev_stmt = first_if
                        else:
                            chain_if = IfStmt(self._mode == kts.const(idx, width=self._mode.width))
                            chain_if.then_(new_output.assign(self.controllers_flat_dict[ctrl_name].ports[port]))
                            # chain_if.then_(new_output.assign(0))
                            # print(self.controllers_flat_dict[ctrl_name].ports)
                            # print(self.controllers_flat_dict[ctrl_name].ports[port])
                            prev_stmt.else_(chain_if)
                            prev_stmt = chain_if

    def add_mem_port_connection(self, local_port, ctrl_ports):
        '''
        This function handles building the mux to the memory port given the local
        port in the interface and a dict of all controller ports attempting to connect
        '''
        local_intf = local_port.get_port_interface()
        mux_size = len(ctrl_ports)
        if mux_size == 0:
            # Nothing to connect to this port - wire to 0
            for (name, port) in local_intf.items():
                tmp_0 = kts.const(0, width=port.width)
                self.wire(port, tmp_0)
        elif mux_size == 1:
            # Just hook up the one port directly
            ctrl_intf = {}
            for (ctrl_name, ctrl_port) in ctrl_ports.items():
                ctrl_intf = ctrl_port.get_port_interface()
            for (name, port) in local_intf.items():
                self.wire(port, ctrl_intf[name])
        else:
            # Now we procedurally produce an always_comb block to choose between controllers
            mux_comb = self.combinational()
            bc_comb = self.combinational()

            # First, add default 0's for the memory input...
            mux_list = [name for name in local_intf.keys() if 'out' not in name]
            bc_list = [name for name in local_intf.keys() if 'out' in name]

            for (name, port) in local_intf.items():
                if 'out' not in name:
                    mux_comb.add_stmt(port.assign(0))

            prev_stmt = None
            ctrl_intf = {}

            # Go through each signal in the memory port
            for (idx, (ctrl_name, ctrl_port)) in enumerate(ctrl_ports.items()):
                ctrl_intf = ctrl_port.get_port_interface()
                # Broadcast the outputs
                for bc_sign in bc_list:
                    bc_comb.add_stmt(ctrl_intf[bc_sign].assign(local_intf[bc_sign]))
                ass_stmt = [local_intf[name].assign(ctrl_intf[name]) for name in mux_list]
                # Mux in the inputs
                if idx == 0:
                    first_if = mux_comb.if_(self._mode == kts.const(idx, width=self._mode.width))
                    first_if.then_(*ass_stmt)
                    prev_stmt = first_if
                else:
                    chain_if = IfStmt(self._mode == kts.const(idx, width=self._mode.width))
                    chain_if.then_(*ass_stmt)
                    prev_stmt.else_(chain_if)
                    prev_stmt = chain_if

    def realize_mem_connections(self):
        # For each port in the memory system, mux between the different controllers
        # based on the mode
        for bank in range(self.memory_banks):
            for port in range(self.memory_interface.get_num_ports()):
                self.add_mem_port_connection(self.memories[bank].get_ports()[port], self.mem_conn[bank][port])

    def __str__(self):
        rep_str = "=====MEMORY TILE BUILDER=====\n===CONTROLLERS===\n"
        rep_str += f"{self.controllers_flat}\n===INPUTS===\n{self.inputs_dict}\n"
        rep_str += f"===OUTPUTS===\n{self.outputs_dict}\n===MEMPORTS===\n"
        for i in range(self.memory_banks):
            for j in range(self.memory_interface.get_num_ports()):
                rep_str += f"BANK: {i}\t PORT: {j}\n"
                for (k, (ctrl, port)) in enumerate(self.mem_conn[i][j].items()):
                    rep_str += f"CONN {k}\tCONTROLLER: {ctrl}\tPORT: {port}\n"
        return rep_str[:-1]
