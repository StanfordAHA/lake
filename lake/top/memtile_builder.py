from lake.modules.chain_accessor import ChainAccessor
from lake.top.cgra_tile_builder import CGRATileBuilder
from lake.modules.storage_config_seq import StorageConfigSeq
from kratos.stmts import IfStmt
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_interface import *
from lake.top.memory_controller import *
from lake.attributes.formal_attr import *
import kratos as kts
from lake.passes.passes import lift_config_reg
from lake.attributes.dedicated_port import DedicatedPortAttribute


class MemoryTileFinalizedException(Exception):
    pass


class MemoryTileBuilder(kts.Generator, CGRATileBuilder):
    '''
    This class provides utilities to add memories and memory controllers and automagically generate the hardware
    '''

    # Need to provide legal widths for top level signals - inputs/outputs - for compatibility
    # with the standard routing network on the AHA CGRA
    legal_widths = [16, 1]

    def __init__(self, name, debug, memory_interface: MemoryInterface = None, memory_banks=1, controllers=None):

        super().__init__(name, debug)

        self.memory_interface = memory_interface
        self.memory_banks = memory_banks
        if controllers is not None:
            self.controllers = controllers
        else:
            self.controllers = []
        self.controllers_flat = []
        self.controllers_dict = {}
        self.controllers_flat_dict = {}
        self.c_to_flat = {}
        self.flat_to_c = {}
        for mem_ctrl in self.controllers:
            self.controllers_dict[mem_ctrl.name] = mem_ctrl
            flat_wrap = MemoryControllerFlatWrapper(mem_ctrl=mem_ctrl, legal_list=MemoryTileBuilder.legal_widths)
            self.controllers_flat_dict[flat_wrap.name] = flat_wrap
            self.controllers_flat.append(flat_wrap)
            self.c_to_flat[mem_ctrl.name] = flat_wrap.name
            self.flat_to_c[flat_wrap.name] = mem_ctrl.name
        self.num_modes = 0

        self.memories = []

        self.controllers_finalized = False

        self.inputs_dict = {}
        self.outputs_dict = {}

        self.dedicated_inputs = {}
        self.dedicated_outputs = {}

        self.mem_port_code = {}
        self.mem_port_mux_if = {}

        self.ctrl_to_mode = {}

        self.supports_chaining = True

        self.mem_conn = None
        if memory_interface is not None:
            self.allocate_mem_conn()

        # CLK and RST
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(self._clk.name, FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(self._rst_n.name, FormalSignalConstraint.RSTN))

    def add_clock_gate(self):
        # Check if clk_en has already been defined...
        clk_en_port = self.internal_generator.get_port("clk_en")
        if clk_en_port is None:
            clk_en_port = self.clock_en("clk_en")
        # Add input attr and formal attr...
        clk_en_port.add_attribute(ControlSignalAttr(False))
        clk_en_port.add_attribute(FormalAttr(clk_en_port.name, FormalSignalConstraint.SET1))

        kts.passes.auto_insert_clock_enable(self.internal_generator)

    def add_flush(self):
        self.add_attribute("sync-reset=flush")
        kts.passes.auto_insert_sync_reset(self.internal_generator)
        flush_port = self.internal_generator.get_port("flush")
        flush_port.add_attribute(ControlSignalAttr(True))

    def allocate_mem_conn(self):
        self.mem_conn = []
        for i in range(self.memory_banks):
            # Each bank has its own list of ports
            self.mem_conn.append([])
            for j in range(self.memory_interface.get_num_ports()):
                self.mem_conn[i].append({})

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

    def get_memory_interface(self):
        return self.memory_interface

    def finalize_controllers(self):
        self.controllers_finalized = True
        self.num_modes = len(self.controllers)
        # Create the mode config reg if we have multiple controllers,
        # otherwise we can just wire it to 0 and let synth handle it?
        if self.num_modes > 1:
            self._mode = self.input("mode", kts.clog2(self.num_modes))
            self._mode.add_attribute(ConfigRegAttr("MODE!"))
        else:
            self._mode = self.var("mode", 1)
            tmp0 = kts.const(0, 1)
            self.wire(self._mode, tmp0)

        # Provide mapping from controller name to mode
        for i, ctrl in enumerate(self.controllers):
            self.ctrl_to_mode[ctrl.name] = i

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

        def sort_ports(p):
            return p.name
        for width in s_to_p.keys():
            s_to_p[width].sort(key=sort_ports)

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
            ctrl_ins = mem_ctrl.get_inputs()
            # Separate into dedicated and shared...
            ded = []
            shr = []
            for (inp, width) in ctrl_ins:
                dedicated_attr = inp.find_attribute(lambda a: isinstance(a, MemoryPortExclusionAttr))
                if len(dedicated_attr) > 0:
                    ded.append((inp, width))
                else:
                    shr.append((inp, width))
            # Create a dict from port width to a list of signals with that width
            stop_in = self.size_to_port(shr)
            self.merge_io_dicts(to_merge=stop_in, merged_dict=self.inputs_dict, mem_ctrl=mem_ctrl)
            # Now add in dedicated ports
            stop_ded = self.size_to_port(ded)
            self.add_io_dict(to_add=stop_ded, merged_dict=self.inputs_dict, mem_ctrl=mem_ctrl)

    def resolve_outputs(self):
        '''
        This function finds the connections between each controller
        and the requested inputs
        '''
        # Go through each controller and get all the inputs - figure out maximum of each size
        # and indicate in which mode which input goes to which port in the controllers
        # Go through controllers
        for mem_ctrl in self.controllers_flat:
            ctrl_outs = mem_ctrl.get_outputs()
            # Create a dict from port width to a list of signals with that width
            ded = []
            shr = []
            for (inp, width) in ctrl_outs:
                dedicated_attr = inp.find_attribute(lambda a: isinstance(a, DedicatedPortAttribute))
                if len(dedicated_attr) > 0:
                    ded.append((inp, width))
                else:
                    shr.append((inp, width))
            stop_out = self.size_to_port(shr)
            self.merge_io_dicts(to_merge=stop_out, merged_dict=self.outputs_dict, mem_ctrl=mem_ctrl)
            # Now add in dedicated ports
            stop_ded = self.size_to_port(ded)
            self.add_io_dict(to_add=stop_ded, merged_dict=self.outputs_dict, mem_ctrl=mem_ctrl)

    def add_io_dict(self, to_add, merged_dict, mem_ctrl):
        """This function adds in dedicated signals to the dict without merging them

        Args:
            to_add (dict): New dict of width: [signal list] key,value pairs
            merged_dict (dict): The local dictionary to add to
            mem_ctrl (MemoryController): Memory controller that contains the signals
        """
        for (width, signal_list) in to_add.items():
            # Just go through and add new signals
            for idx in range(len(signal_list)):
                merged_dict[width].append({str(mem_ctrl): str(signal_list[idx])})

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
            self.c_to_flat[mem_ctrl.name] = flat_wrap.name
            self.flat_to_c[flat_wrap.name] = mem_ctrl.name
        else:
            raise MemoryTileFinalizedException(f"Controllers finalized - {mem_ctrl} isn't being added...")

    def add_config_hooks(self, config_data_width=32, config_addr_width=8):
        self.config_data_width = config_data_width
        self.config_addr_width = config_addr_width

    def add_tile_enable(self):
        # Add tile enable...
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))
        self._tile_en.add_attribute(FormalAttr(self._tile_en.name, FormalSignalConstraint.SET1))
        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

    def realize_hw(self, clock_gate=False, flush=False, mem_config=False, do_lift_config=False):
        '''
        Go through the motions of finally creating the hardware
        '''
        assert self.controllers_finalized, f"Controllers need to be finalized before realizing HW"
        # To realize the hardware, we basically need to create each input and output in the dictionaries
        # Then, wire up the memory ports with muxes + mode as select signal, then realize the underlying memory
        # Need to flatten stuff...
        self.add_tile_enable()
        self.realize_controllers()
        self.realize_inputs()
        self.realize_outputs()
        for (idx, mem) in enumerate(self.memories):
            mem.realize_hw()
            self.add_child(f"memory_{idx}", mem)
        self.realize_mem_connections()

        # Optionally add in these features to the hardware
        if clock_gate:
            self.add_clock_gate()
        if mem_config:
            self.realize_mem_config()
        if flush:
            self.add_flush()
        if do_lift_config:
            lift_config_reg(self.internal_generator)

    def realize_mem_config(self):
        '''
        This function adds in some hooks in to directly read and write the
        memory banks from the configuration bus - only need a read and write or read/write
        port on each

        This is actually really nasty in general because it needs to comply with CGRA
        '''

        # Need to parameterize this
        self.data_width = 16

        # Ascertain all the information used in creating the config bus
        mem_width = self.memory_interface.get_mem_width()
        mem_depth = self.memory_interface.get_mem_depth()
        address_width = kts.clog2(mem_depth)
        fw_int = int(mem_width / self.data_width)
        self.data_words_per_set = 2 ** self.config_addr_width
        self.sets = int((fw_int * mem_depth) / self.data_words_per_set)
        self.sets_per_macro = max(1, int(mem_depth / self.data_words_per_set))
        self.total_sets = max(1, self.memory_banks * self.sets_per_macro)

        self._config_addr_in = self.input("config_addr_in", self.config_addr_width)
        self._config_addr_in.add_attribute(ControlSignalAttr(False))

        self._config_data_out_shrt = self.var("config_data_out_shrt", self.data_width, size=self.total_sets,
                                              explicit_array=True,
                                              packed=True)

        self._config_data_out = self.output("config_data_out", self.config_data_width, size=self.total_sets,
                                            explicit_array=True,
                                            packed=True)
        self._config_data_out.add_attribute(ControlSignalAttr(False))

        self._config_data_in = self.input("config_data_in", self.config_data_width)
        self._config_data_in.add_attribute(ControlSignalAttr(False))

        self._config_data_in_shrt = self.var("config_data_in_shrt", self.data_width)
        self.wire(self._config_data_in_shrt, self._config_data_in[self.data_width - 1, 0])

        # Extend config_data_out, which is created for each set as a new feature in the CGRA
        for i in range(self.total_sets):
            self.wire(self._config_data_out[i],
                      self._config_data_out_shrt[i].extend(self.config_data_width))

        self._config_read = self.input("config_read", 1)
        self._config_read.add_attribute(ControlSignalAttr(False))
        self._config_write = self.input("config_write", 1)
        self._config_write.add_attribute(ControlSignalAttr(False))
        self._config_en = self.input("config_en", self.total_sets)
        self._config_en.add_attribute(ControlSignalAttr(False))

        stg_cfg_seq = StorageConfigSeq(data_width=self.data_width,
                                       config_addr_width=self.config_addr_width,
                                       addr_width=address_width,
                                       fetch_width=mem_width,
                                       total_sets=self.total_sets,
                                       sets_per_macro=self.sets_per_macro,
                                       memory_interface=self.memory_interface)

        self.add_child(f"config_seq", stg_cfg_seq,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       config_data_in=self._config_data_in_shrt,
                       config_addr_in=self._config_addr_in,
                       config_wr=self._config_write,
                       config_rd=self._config_read,
                       config_en=self._config_en,
                       rd_data_out=self._config_data_out_shrt,
                       )

        # Wire in the clock enable
        clk_en_port = self.internal_generator.get_port("clk_en")
        if clk_en_port is not None:
            self.wire(stg_cfg_seq.ports['clk_en'], clk_en_port | self._config_en.r_or())
            # Make sure to wire this in if it's in the memory as well
            clk_en_memory = self.memories[0].internal_generator.get_port("clk_en")
            if clk_en_memory is not None:
                self.internal_generator.unwire(clk_en_memory, clk_en_port)
                self.wire(clk_en_memory, kts.clock_en(clk_en_port | self._config_en.r_or()))
        else:
            self.wire(stg_cfg_seq.ports['clk_en'], self._config_en.r_or())

        # Now go through the memory port requests of the configuration module
        # and inject the override if condition into the code
        mem_ports_to_override = stg_cfg_seq.get_memory_ports()
        for bank in range(len(mem_ports_to_override)):
            for port in range(len(mem_ports_to_override[0])):
                override = mem_ports_to_override[bank][port]
                # Finally inject the config override in here...
                if override is not None:
                    self.inject_config_override(bank, port, override)

    def inject_config_override(self, bank, port, override_port):
        # Get local information about the code structure
        local_port = self.memories[bank].get_ports()[port]
        over_intf = override_port.get_port_interface()
        local_intf = local_port.get_port_interface()
        (mux_comb, bc_comb) = self.mem_port_code[local_port]
        first_if = self.mem_port_mux_if[local_port]
        # First, add default 0's for the memory input...
        mux_list = [name for name in local_intf.keys() if 'out' not in name]
        bc_list = [name for name in local_intf.keys() if 'out' in name]
        # Just add in the outputs
        for bc_sign in bc_list:
            bc_comb.add_stmt(over_intf[bc_sign].assign(local_intf[bc_sign]))
        # Now hijack the original muxes and add priority override...
        ass_stmt = [local_intf[name].assign(over_intf[name]) for name in mux_list]
        mux_comb.remove_stmt(first_if)
        override_if = IfStmt(self._config_en.r_or())
        override_if.then_(*ass_stmt)
        override_if.else_(first_if)
        mux_comb.add_stmt(override_if)

    def realize_controllers(self):
        for (idx, (ctrl_name, ctrl)) in enumerate(self.controllers_flat_dict.items()):
            self.add_child(f'mem_ctrl_{ctrl_name}', ctrl)
            # Wire clk and rst....making some decent assumptions here
            # We actually use the tile enabled clock (Which should have been swapped in)
            # and additionally make sure the mode matches to make sure we minimize power.
            if ctrl.get_dedicated_clock():
                # A controller with a dedicated clock should be excluded from the mode gating - dedicated logic basically.
                self.wire(ctrl.ports['clk'], kts.util.clock(self._gclk))
            elif len(self.controllers) > 1:
                self.wire(ctrl.ports['clk'], kts.util.clock(self._gclk & (self._mode == idx)))
            else:
                self.wire(ctrl.ports['clk'], kts.util.clock(self._gclk))
            self.wire(self._rst_n, ctrl.ports['rst_n'])

    def realize_inputs(self):
        '''
        This function creates the inputs to the tile, tags them, and wires them to the ports of the controllers
        '''
        for (input_width, signal_dicts) in self.inputs_dict.items():
            for (i, signal_dict) in enumerate(signal_dicts):
                if input_width != 1:
                    new_input = self.input(f'input_width_{input_width}_num_{i}', width=input_width, explicit_array=True, packed=True)
                else:
                    new_input = self.input(f'input_width_{input_width}_num_{i}', width=input_width)
                isctrl = input_width == 1
                new_input.add_attribute(ControlSignalAttr(isctrl))
                for (ctrl_name, port) in signal_dict.items():
                    self.wire(new_input, self.controllers_flat_dict[ctrl_name].ports[port])

    def realize_outputs(self):
        '''
        This function creates the outputs from the tile, tags them, and wires them to the ports of the controllers
        '''
        for (output_width, signal_dicts) in self.outputs_dict.items():
            for (i, signal_dict) in enumerate(signal_dicts):
                if output_width != 1:
                    new_output = self.output(f'output_width_{output_width}_num_{i}', width=output_width, explicit_array=True, packed=True)
                else:
                    new_output = self.output(f'output_width_{output_width}_num_{i}', width=output_width)
                new_output.add_attribute(ControlSignalAttr(False))
                # We need to choose which output is hooked up based on the mode...
                mux_size = len(signal_dict.keys())
                mux_comb = self.combinational()
                prev_stmt = None
                # Default assign 0 to prevent latches.
                mux_comb.add_stmt(new_output.assign(0))
                on_first = True
                for (ctrl_name, port) in signal_dict.items():
                    # Create a list of ports/values to wire to based on the existence of the port in the mode
                    # for idx in self.num_modes:
                    if mux_size == 1:
                        mux_comb.add_stmt(new_output.assign(self.controllers_flat_dict[ctrl_name].ports[port]))
                    else:
                        # Index is set off the controller mapping
                        ctrl_name_unflat = self.flat_to_c[ctrl_name]
                        idx = self.ctrl_to_mode[ctrl_name_unflat]
                        if on_first is True:
                            first_if = mux_comb.if_(self._mode == kts.const(idx, width=self._mode.width))
                            # Figure out if an output from a controller of the mode idx is actually defined or not
                            first_if.then_(new_output.assign(self.controllers_flat_dict[ctrl_name].ports[port]))
                            prev_stmt = first_if
                            on_first = False

                        else:
                            chain_if = IfStmt(self._mode == kts.const(idx, width=self._mode.width))
                            chain_if.then_(new_output.assign(self.controllers_flat_dict[ctrl_name].ports[port]))
                            prev_stmt.else_(chain_if)
                            prev_stmt = chain_if

    def add_mem_port_connection(self, local_port, ctrl_ports):
        '''
        This function handles building the mux to the memory port given the local
        port in the interface and a dict of all controller ports attempting to connect
        '''
        local_intf = local_port.get_port_interface()
        mux_size = len(ctrl_ports)

        # Add a comb/seq block out here...
        # Now we procedurally produce an always_comb block to choose between controllers
        mux_comb = self.combinational()
        bc_comb = self.combinational()

        self.mem_port_code[local_port] = (mux_comb, bc_comb)
        self.mem_port_mux_if[local_port] = None

        # First, add default 0's for the memory input...
        mux_list = [name for name in local_intf.keys() if 'out' not in name]
        bc_list = [name for name in local_intf.keys() if 'out' in name]

        # Default assign to 0 to prevent latches/handle no connections
        for (name, port) in local_intf.items():
            if 'out' not in name:
                mux_comb.add_stmt(port.assign(0))

        if mux_size > 0:
            prev_stmt = None
            ctrl_intf = {}

            # Go through each signal in the memory port
            for (idx, (ctrl_name, ctrl_port)) in enumerate(ctrl_ports.items()):
                ctrl_intf = ctrl_port.get_port_interface()
                # Broadcast the outputs
                for bc_sign in bc_list:
                    if ctrl_intf[bc_sign] is not None:
                        bc_comb.add_stmt(ctrl_intf[bc_sign].assign(local_intf[bc_sign]))
                ass_stmt = [local_intf[name].assign(ctrl_intf[name]) for name in mux_list]
                # Mux in the inputs
                if idx == 0:
                    # first_if = IfStmt(self._mode == kts.const(idx, width=self._mode.width))
                    first_if = mux_comb.if_(self._mode == kts.const(idx, width=self._mode.width))
                    first_if.then_(*ass_stmt)
                    self.mem_port_mux_if[local_port] = first_if
                    prev_stmt = first_if
                else:
                    chain_if = IfStmt(self._mode == kts.const(idx, width=self._mode.width))
                    chain_if.then_(*ass_stmt)
                    prev_stmt.else_(chain_if)
                    prev_stmt = chain_if
            # mux_comb.add_stmt(first_if)

    def realize_mem_connections(self):
        # For each port in the memory system, mux between the different controllers
        # based on the mode
        for bank in range(self.memory_banks):
            self.wire(self._gclk, self.memories[bank].get_clock())
            for port in range(self.memory_interface.get_num_ports()):
                # Wire clock and reset...
                # self.wire(self._gclk, self.memories[bank].get_clock())
                if self.memories[bank].has_reset():
                    self.wire(self._rst_n, self.memories[bank].get_reset())
                self.add_mem_port_connection(self.memories[bank].get_ports()[port], self.mem_conn[bank][port])

    def get_mode_map(self):
        '''
        Sort of hardcoded/hacky way to refer to specific controller types
        '''
        self.mode_map = {}
        for memctrl in self.controllers:
            self.mode_map[memctrl.get_config_mode_str()] = memctrl
        return self.mode_map

    def get_bitstream(self, config_json):
        '''
        At this level, we can take in the json and figure out which mode we are using
        '''
        # Create blank config - turn the tile on
        config = []
        config.append(("tile_en", 1))
        # Extract the mode to set the mode config reg (if there is more than one mode)
        mode_map = self.get_mode_map()
        if 'init' in config_json:
            pass
        ctrl_config = {}
        # Check for stencil valid
        if 'stencil_valid' in config_json and 'stencil_valid' in mode_map:
            ctrl_config['stencil_valid'] = mode_map['stencil_valid'].get_bitstream(config_json)

        if 'mode' in config_json:
            mode_used = config_json['mode']
            if self.num_modes > 1:
                # Locate the controller in the list...
                for idx, ctrl in enumerate(self.controllers):
                    if mode_used == ctrl.get_config_mode_str():
                        print(f"Found ctrl: {mode_used}")
                        config.append(("mode", idx))
                        break

            ctrl_to_conf = mode_map[mode_used]
            # Have some guard to see if config is in there or not...
            if 'config' in config_json:
                ctrl_config[str(ctrl_to_conf)] = ctrl_to_conf.get_bitstream(config_json['config'])
            else:
                ctrl_config[str(ctrl_to_conf)] = ctrl_to_conf.get_bitstream(config_json)

        for (ctrl, conf_for_ctrl) in ctrl_config.items():
            # Go through each config and prepend the string
            prepend_string = f"mem_ctrl_{ctrl}_flat_{ctrl}_inst_"
            for cfg_reg, val in conf_for_ctrl:
                config.append((prepend_string + cfg_reg, val))
        return config

    def get_inputs_map(self):
        """Return the mapping from input ports to controller ports
        """
        return self.inputs_dict

    def get_outputs_map(self):
        """Return the mapping from output ports to controller ports
        """
        return self.outputs_dict

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
