from lake.attributes.hybrid_port_attr import HybridPortAddr
from lake.modules.chain_accessor import ChainAccessor
from lake.modules.reg_fifo import RegFIFO
from lake.top.cgra_tile_builder import CGRATileBuilder
from lake.modules.storage_config_seq import StorageConfigSeq
from kratos.stmts import IfStmt
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.extract_tile_info import extract_top_config
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
    legal_widths = [17, 16, 1]

    def __init__(self, name, debug, memory_interface: MemoryInterface = None,
                 memory_banks=1, controllers=None,
                 ready_valid=True, fifo_depth=8,
                 io_prefix=None):

        super().__init__(name, debug)

        self.memory_interface = memory_interface
        self.memory_banks = memory_banks
        self.fifo_depth = fifo_depth
        if io_prefix is not None:
            self.io_prefix = io_prefix
        else:
            self.io_prefix = ""
        if controllers is not None:
            self.controllers = controllers
        else:
            self.controllers = []
        self.controllers_flat = []
        self.controllers_dict = {}
        self.controllers_flat_dict = {}
        self.c_to_flat = {}
        self.flat_to_c = {}
        self.ready_valid = ready_valid
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

        self.port_remap_dict = {}

        self.dedicated_inputs = {}
        self.dedicated_outputs = {}

        self.controls_ded_shared = {}

        self.mem_port_code = {}
        self.mem_port_mux_if = {}

        self.ctrl_to_mode = {}

        self.dense = []
        self.rv = []

        self.fifo_map = {}

        self.supports_chaining = True

        self.mem_conn = None
        if memory_interface is not None:
            self.allocate_mem_conn()

        self.tech_map = None

        # CLK and RST
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(self._clk.name, FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(self._rst_n.name, FormalSignalConstraint.RSTN))

    def get_async_reset(self):
        return self._rst_n

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

        if not sim_macro_n:
            self.tech_map = tech_map

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

    def get_port_remap(self):
        return self.port_remap_dict

    def finalize_controllers(self):
        self.controllers_finalized = True
        bulk_ctrl = 0
        exclusive_ctrl = 0
        for ctrl in self.controllers:
            print(ctrl)
            if ctrl.get_exclusive():
                print("FOUND EXCLUSIVE")
                self.ctrl_to_mode[ctrl.name] = (exclusive_ctrl, "excl")
                exclusive_ctrl += 1
            else:
                print("FOUND BULK")
                self.ctrl_to_mode[ctrl.name] = (bulk_ctrl, "bulk")
                bulk_ctrl += 1
        # self.num_modes = len(self.controllers)
        self.num_modes = bulk_ctrl
        # Create the mode config reg if we have multiple controllers,
        # otherwise we can just wire it to 0 and let synth handle it?
        if self.num_modes > 1:
            self._mode = self.input("mode", kts.clog2(self.num_modes))
            self._mode.add_attribute(ConfigRegAttr("MODE!"))
        else:
            self._mode = self.var("mode", 1)
            tmp0 = kts.const(0, 1)
            self.wire(self._mode, tmp0)

        self.num_modes_excl = exclusive_ctrl
        if self.num_modes_excl > 0:
            self._mode_excl = self.input("mode_excl", self.num_modes_excl)
            self._mode_excl.add_attribute(ConfigRegAttr("MODE EXCLUSIVE!"))

        # Provide mapping from controller name to mode
        # Also, while we are here we can set the min depth of all the deferred fifos
        # and then generate them
        for i, ctrl in enumerate(self.controllers):
            # First get the main/shared fifos, minimize their depth and gen them
            ctrl_fifos = ctrl.get_fifos()
            for ctrl_fifo in ctrl_fifos:
                ctrl_fifo.set_min_depth()
                ctrl_fifo.generate_hardware()
            # Clean up the remaining/ungenned fifos and gen them
            alt_fifos = ctrl.get_other_fifos()
            for alt_fifo in alt_fifos:
                alt_fifo.generate_hardware()
            self.fifo_map[ctrl.name] = ctrl.get_fifos()
            # self.ctrl_to_mode[ctrl.name] = i

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
        self.using_mems = True
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
        # Current way to block off mems
        if len(self.mem_conn[0][0]) == 0:
            self.using_mems = False

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
            ctrl_outs = mem_ctrl.get_outputs()
            # Do a pass to block the ready/valids associated with a port
            # All ready/valid ports will be width 1 - there's probably a better way to do this
            wider_than_1_in = [(inp, width) for (inp, width) in ctrl_ins if width > 1]
            wider_than_1_out = [(inp, width) for (inp, width) in ctrl_outs if width > 1]
            width_1_in = [inp.name for (inp, width) in ctrl_ins if width == 1]
            width_1_out = [outp.name for (outp, width) in ctrl_outs if width == 1]
            # Handle main data inputs and get rid of the valid in
            for (inp, width) in wider_than_1_in:
                full_name = inp.name
                stripped_name = full_name.rstrip("_f_")
                has_rv = f"{stripped_name}_ready_f_" in width_1_out and f"{stripped_name}_valid_f_" in width_1_in
                if has_rv:
                    hpa = mem_ctrl.get_port_attributes(inp.name, HybridPortAddr)
                    is_hybrid = len(hpa) > 0
                    self.rv.append((mem_ctrl.name, inp, is_hybrid))
                    valid_p = mem_ctrl.get_port(f"{stripped_name}_valid_f_")
                    # If this port has rv, remove the rv from the ctrl_ins
                    ctrl_ins = [(inp_, width) for (inp_, width) in ctrl_ins if not (inp_.name == valid_p.name and width == 1)]
                else:
                    self.dense.append((mem_ctrl.name, inp, False))
            # Handle data outputs to get rid of the input readys...wasteful work
            for (outp, width) in wider_than_1_out:
                full_name = outp.name
                stripped_name = full_name.rstrip("_f_")
                has_rv = f"{stripped_name}_ready_f_" in width_1_in and f"{stripped_name}_valid_f_" in width_1_out
                if has_rv:
                    ready_p = mem_ctrl.get_port(f"{stripped_name}_ready_f_")
                    ctrl_ins = [(inp_, width) for (inp_, width) in ctrl_ins if not (inp_.name == ready_p.name and width == 1)]

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

            print("INPUTS DICT")
            print(self.inputs_dict)

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
            ctrl_ins = mem_ctrl.get_inputs()
            # Do a pass to block the ready/valids associated with a port
            # All ready/valid ports will be width 1 - there's probably a better way to do this
            wider_than_1_in = [(outp, width) for (outp, width) in ctrl_ins if width > 1]
            wider_than_1_out = [(outp, width) for (outp, width) in ctrl_outs if width > 1]
            width_1_in = [inp.name for (inp, width) in ctrl_ins if width == 1]
            width_1_out = [outp.name for (outp, width) in ctrl_outs if width == 1]
            for (outp, width) in wider_than_1_out:
                full_name = outp.name
                stripped_name = full_name.rstrip("_f_")
                has_rv = f"{stripped_name}_ready_f_" in width_1_in and f"{stripped_name}_valid_f_" in width_1_out
                if has_rv:
                    hpa = mem_ctrl.get_port_attributes(outp.name, HybridPortAddr)
                    is_hybrid = len(hpa) > 0
                    self.rv.append((mem_ctrl.name, outp, is_hybrid))
                    valid_p = mem_ctrl.get_port(f"{stripped_name}_valid_f_")
                    # If this port has rv, remove the rv from the ctrl_ins
                    ctrl_outs = [(inp_, width) for (inp_, width) in ctrl_outs if not (inp_.name == valid_p.name and width == 1)]
                else:
                    self.dense.append((mem_ctrl.name, outp, False))
            # Handle data inputs to get rid of the output readys...wasteful work
            for (inp, width) in wider_than_1_in:
                full_name = inp.name
                stripped_name = full_name.rstrip("_f_")
                has_rv = f"{stripped_name}_ready_f_" in width_1_out and f"{stripped_name}_valid_f_" in width_1_in
                if has_rv:
                    ready_p = mem_ctrl.get_port(f"{stripped_name}_ready_f_")
                    ctrl_outs = [(inp_, width) for (inp_, width) in ctrl_outs if not (inp_.name == ready_p.name and width == 1)]

            # Create a dict from port width to a list of signals with that width
            ded = []
            shr = []
            for (inp, width) in ctrl_outs:
                dedicated_attr = inp.find_attribute(lambda a: isinstance(a, DedicatedPortAttribute))
                if len(dedicated_attr) > 0:
                    ded.append((inp, width))
                    # if mem_ctrl not in self.controls_ded_shared:
                    # Overwrite to dedicated no matter what
                    self.controls_ded_shared[self.flat_to_c[mem_ctrl.name]] = "DEDICATED"
                else:
                    shr.append((inp, width))
                    if mem_ctrl not in self.controls_ded_shared:
                        self.controls_ded_shared[self.flat_to_c[mem_ctrl.name]] = "SHARED"
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
                if width not in merged_dict:
                    merged_dict[width] = []
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
        if self.using_mems:
            for (idx, mem) in enumerate(self.memories):
                mem.realize_hw()
                self.add_child(f"memory_{idx}", mem)
            self.realize_mem_connections()

        # Optionally add in these features to the hardware
        if clock_gate:
            self.add_clock_gate()
        if mem_config and self.using_mems:
            self.realize_mem_config()
        else:
            # Can't forget to set this or core wrapper won't work
            self.total_sets = 0
        if flush:
            self.add_flush()
        # For now, do this merging of the config regs
        if do_lift_config:
            self.num_chopped_cfg = 0
            lift_config_reg(self.internal_generator)
        else:
            self.merge_config_regs()

    def merge_config_regs(self):

        self.config_sizes = {}

        for flat_name, flat_ctrl in self.controllers_flat_dict.items():
            flat_ctrl_int_gen = flat_ctrl.internal_generator
            lift_config_reg(flat_ctrl_int_gen, stop_at_gen=True, flatten=True)
            # Now get all the config regs
            # lift_config_reg(flat_ctrl.internal_generator, stop_at_gen=True)
            cfg_flat_ctrl = extract_top_config(flat_ctrl, verbose=False)
            for cfg in cfg_flat_ctrl:
                name, size, width, _, ro = cfg
                if ro is True:
                    # Should wire this up to the top
                    tmp_cfg = self.output(f"{name}", width, size=size)
                    tmp_cfg.add_attribute(ConfigRegAttr(f"Read only directly brought up... {name}", read_only=True))
                    self.wire(tmp_cfg, flat_ctrl.ports[name])
                    continue
                # Everything should be flattened down at this point
                # assert len(size) == 1 and size[0] == 1
                assert len(size) == 1
                # It's not flattened at this point unfortunately
                # Handle non flat wires...
                for idx_ in range(size[0]):
                    if size[0] > 1:
                        new_name = f"{name}_{idx_}"
                    else:
                        new_name = name
                    if flat_name not in self.config_sizes.keys():
                        tmp_list = []
                        tmp_list.append(new_name)
                        self.config_sizes[flat_name] = (width, tmp_list, self.controls_ded_shared[self.flat_to_c[flat_name]])
                    else:
                        width_curr, name_list_curr, ded_shrd = self.config_sizes[flat_name]
                        name_list_curr.append(new_name)
                        self.config_sizes[flat_name] = (width_curr + width, name_list_curr, ded_shrd)

        max_sizes = []

        ded_ = [[(name, size_struct)] for name, size_struct in self.config_sizes.items() if size_struct[2] == "DEDICATED"]
        shared_ = [(name, size_struct) for name, size_struct in self.config_sizes.items() if size_struct[2] == "SHARED"]

        combo_ = [shared_, *ded_]

        # Figure out the maximum length for each segment
        for idx in range(len(combo_)):
            max_size = 0
            for ii_, (name, size_struct) in enumerate(combo_[idx]):
                size, sig_list, _ = size_struct
                # Sort the list...
                sig_list = sorted(sig_list)
                self.config_sizes[name] = (size, sig_list, _)
                combo_[idx][ii_] = (name, (size, sig_list, _))
                if size > max_size:
                    max_size = size
            max_sizes.append(max_size)

        self.config_mapping = {}

        # Take the largest config space and make one fat bus
        self.config_space = self.var("CONFIG_SPACE", sum(max_sizes))
        self.config_space.add_attribute(ConfigRegAttr(f"Configuration for children modules"))

        # for flat_name, flat_ctrl in self.controllers_flat_dict.items():
        # for name, size_struct in self.config_sizes.items():
        running_width_outer = 0
        for idx in range(len(combo_)):
            if idx > 0:
                # This handles the outer window for shared vs dedicated
                running_width_outer = sum(max_sizes[0:idx])
            for name, size_struct in combo_[idx]:
                # Now wire the chillin
                running_width = 0
                size, sig_list, _ = size_struct
                flat_gen = self.controllers_flat_dict[name]
                # flat_ports = flat_gen.internal_generator.get_port()
                catted_child_signals = []
                for sig in sig_list:
                    # Handle the fact that some port might come back as None
                    sig_port = flat_gen.internal_generator.get_port(sig)
                    if sig_port is None:
                        chopped = sig.rfind('_')
                        port_idx = int(sig[chopped + 1:])
                        base_port = sig[:chopped]
                        sig_port = flat_gen.internal_generator.get_port(base_port)[port_idx]
                        # It is flattened
                        catted_child_signals.append(sig_port)
                        # for flat_prt in range(si)
                    else:
                        catted_child_signals.append(sig_port)
                # catted_child_signals = [flat_gen.internal_generator.get_port(sig) for sig in sig_list]
                # Now create the mapping info
                cfg_mapping_name = name[0:-5]
                self.config_mapping[cfg_mapping_name] = {}

                # Reversed since concat puts MSBs on the left
                for sig in reversed(catted_child_signals):
                    sig_w_ = sig.width
                    self.config_mapping[cfg_mapping_name][sig.name] = (running_width + sig_w_ - 1 + running_width_outer, running_width + running_width_outer)
                    running_width += sig_w_
                kts_catted = kts.concat(*catted_child_signals)

                # Track the outer running width to make sure the config spaces are separated for
                # the dedicated controllers
                self.wire(kts_catted, self.config_space[size - 1 + running_width_outer, running_width_outer])

        self.allowed_reg_size = 32
        self.num_chopped_cfg = 0
        curr_size = 0
        idx = 0
        self.config_bits = sum(max_sizes)
        while curr_size < self.config_bits:
            if self.config_bits - curr_size > self.allowed_reg_size:
                cs_ = self.allowed_reg_size
            else:
                cs_ = self.config_bits - curr_size
            # tmp_cfg_space = self.input(f"CONFIG_SPACE_{idx}_{curr_size}_{curr_size + cs_ - 1}", cs_)
            tmp_cfg_space = self.input(f"CONFIG_SPACE_{idx}", cs_)
            tmp_cfg_space.add_attribute(ConfigRegAttr(f"Configuration for children modules {idx}"))
            self.wire(self.config_space[curr_size + cs_ - 1, curr_size], tmp_cfg_space)
            idx += 1
            curr_size += cs_
            self.num_chopped_cfg += 1

    def get_config_mapping(self):
        return self.config_mapping

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

    def inject_config_override(self, bank, port, override_port, port_num=0):
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

        ignore_sigs = []
        if self.tech_map is not None:
            # Should only use this if there is a tech map
            ignore_sigs = self.tech_map['ports'][port_num]['alt_sigs'].keys()

        # Now hijack the original muxes and add priority override...
        ass_stmt = [local_intf[name].assign(over_intf[name]) for name in mux_list if name not in ignore_sigs]
        # Remove the first if, then chain in the override if
        mux_comb.remove_stmt(first_if)
        override_if = IfStmt(self._config_en.r_or())
        override_if.then_(*ass_stmt)
        # If there is no first_if (no controllers using memory), we should wire ins to 0?
        if first_if is not None:
            override_if.else_(first_if)
        else:
            pass
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

    def create_mode_based_mux(self, out_sig, items, default):
        mux_size = len(items)
        mux_comb = self.combinational()
        prev_stmt = None
        # Default assign 0 to prevent latches.
        mux_comb.add_stmt(out_sig.assign(default))
        on_first = True

        # This is probably an exclusive port...
        if len(items) == 1 and len(items[0]) == 3 and items[0][2] == "excl":
            print("Exclusive port...")
            signal_, mode_bit_, excl_ = items[0]
            first_if = mux_comb.if_(self._mode_excl[mode_bit_] == kts.const(1, 1))
            # Figure out if an output from a controller of the mode idx is actually defined or not
            first_if.then_(out_sig.assign(signal_))
            first_if.else_(out_sig.assign(default))
            return

        # Wire all the inputs to the data/valid, then mux the ready
        # for (signal_, mode_num_) in items:
        for item_ in items:

            if len(item_) == 2:
                signal_, mode_num_ = item_
            elif len(item_) == 3:
                signal_, mode_num_, b_ex_ = item_
            else:
                raise NotImplementedError

            if on_first is True:
                first_if = mux_comb.if_(self._mode == kts.const(mode_num_, width=self._mode.width))
                # Figure out if an output from a controller of the mode idx is actually defined or not
                first_if.then_(out_sig.assign(signal_))
                prev_stmt = first_if
                if mux_size == 1:
                    prev_stmt.else_(out_sig.assign(default))
                on_first = False

            else:
                chain_if = IfStmt(self._mode == kts.const(mode_num_, width=self._mode.width))
                chain_if.then_(out_sig.assign(signal_))
                prev_stmt.else_(chain_if)
                prev_stmt = chain_if

    def realize_inputs(self):
        '''
        This function creates the inputs to the tile, tags them, and wires them to the ports of the controllers
        '''

        for (input_width, signal_dicts) in self.inputs_dict.items():
            for (i, signal_dict) in enumerate(signal_dicts):
                if input_width != 1:
                    new_input = self.input(f'{self.io_prefix}input_width_{input_width}_num_{i}', width=input_width, explicit_array=True, packed=True)
                else:
                    new_input = self.input(f'{self.io_prefix}input_width_{input_width}_num_{i}', width=input_width)
                isctrl = input_width == 1
                new_input.add_attribute(ControlSignalAttr(isctrl))
                # Now to determine if the port is rv/dense
                # If any signal in this dict is rv, we are going to make it an rv
                rvs = [(ctrl_, port_.name, self.ctrl_to_mode[self.flat_to_c[ctrl_]], hybrid_) for ctrl_, port_, hybrid_ in self.rv if ctrl_ in signal_dict and signal_dict[ctrl_] == port_.name]
                any_rvs = len(rvs) > 0
                dense = [(ctrl_, port_.name, self.ctrl_to_mode[self.flat_to_c[ctrl_]], hybrid_) for ctrl_, port_, hybrid_ in self.dense if ctrl_ in signal_dict and signal_dict[ctrl_] == port_.name]
                any_dense = len(dense) > 0
                # assert any_rvs or any_dense

                hybrid_bypass = None
                output_ready_map = []

                if any_rvs:
                    # Create the fifo and send all the relevant R/V signals to it
                    # Now create the ready/valid pair and deal with it
                    new_input_valid = self.input(f'{self.io_prefix}input_width_{input_width}_num_{i}_valid', width=1)
                    new_input_valid.add_attribute(ControlSignalAttr(True))

                    new_input_ready = self.output(f'{self.io_prefix}input_width_{input_width}_num_{i}_ready', width=1)
                    new_input_ready.add_attribute(ControlSignalAttr(False))

                    # Add in the fifo if there are any fifos on this path
                    new_reg_fifo = RegFIFO(data_width=input_width,
                                           width_mult=1, depth=self.fifo_depth,
                                           defer_hrdwr_gen=False)

                    self.add_child(f"input_width_{input_width}_num_{i}_input_fifo",
                                   new_reg_fifo,
                                   clk=self._gclk,
                                   rst_n=self._rst_n,
                                   #    clk_en=kts.const(1, 1),
                                   push=new_input_valid,
                                   data_in=new_input)

                    # self.wire(new_input_ready, ~new_reg_fifo.ports.full)

                    # Alias the new input across the fifo boundary
                    if input_width != 1:
                        new_input_fifo = self.var(f'input_width_{input_width}_num_{i}_fifo_out',
                                                  width=input_width, explicit_array=True, packed=True)
                    else:
                        new_input_fifo = self.var(f'input_width_{input_width}_num_{i}_fifo_out', width=input_width)

                    new_input_valid_fifo = self.var(f'input_width_{input_width}_num_{i}_fifo_out_valid', width=1)
                    new_input_ready_fifo = self.var(f'input_width_{input_width}_num_{i}_fifo_out_ready', width=1)

                    self.wire(new_input_fifo, new_reg_fifo.ports.data_out)
                    self.wire(new_input_valid_fifo, ~new_reg_fifo.ports.empty)
                    self.wire(new_reg_fifo.ports.pop, new_input_ready_fifo)

                    mux_size = len(rvs)
                    mux_comb = self.combinational()
                    prev_stmt = None
                    # Default assign 0 to prevent latches.
                    mux_comb.add_stmt(new_input_ready_fifo.assign(1))
                    on_first = True

                    # Wire all the inputs to the data/valid, then mux the ready
                    # for (ctrl_name, port, mode_num, hybrid) in rvs:
                    for (ctrl_name, port, (mode_num, b_ex), hybrid) in rvs:

                        if hybrid is True and hybrid_bypass is None:
                            hybrid_bypass = self.input(f"{self.io_prefix}input_width_{input_width}_num_{i}_dense", 1)
                            hybrid_bypass.add_attribute(ConfigRegAttr(f"Choose for {self.io_prefix}input_width_{input_width}_num_{i}_dense to bypass input fifo"))

                        port_ready_name = f"{port.rstrip('_f_')}_ready_f_"
                        port_valid_name = f"{port.rstrip('_f_')}_valid_f_"

                        if hybrid:
                            # In the hybrid case, we need to mux between the original input data/valid and the fifo's offering
                            self.wire(self.controllers_flat_dict[ctrl_name].ports[port], kts.ternary(hybrid_bypass,
                                                                                                     new_input,
                                                                                                     new_input_fifo))
                            self.wire(self.controllers_flat_dict[ctrl_name].ports[port_valid_name], kts.ternary(hybrid_bypass,
                                                                                                                # new_input_valid,
                                                                                                                kts.const(1, 1),
                                                                                                                new_input_valid_fifo))
                            # The ready out for this selection should also be ternary
                            tmp_ready_choose = (kts.ternary(hybrid_bypass,
                                                        #    self.controllers_flat_dict[ctrl_name].ports[port_ready_name],
                                                           kts.const(1, 1),
                                                           ~new_reg_fifo.ports.full), mode_num)
                        else:
                            # Otherwise, we can simply directly wire them
                            self.wire(new_input_fifo, self.controllers_flat_dict[ctrl_name].ports[port])
                            self.wire(new_input_valid_fifo, self.controllers_flat_dict[ctrl_name].ports[port_valid_name])
                            tmp_ready_choose = (~new_reg_fifo.ports.full, mode_num)

                        output_ready_map.append(tmp_ready_choose)

                        if on_first is True:
                            first_if = mux_comb.if_(self._mode == kts.const(mode_num, width=self._mode.width))
                            # Figure out if an output from a controller of the mode idx is actually defined or not
                            first_if.then_(new_input_ready_fifo.assign(self.controllers_flat_dict[ctrl_name].ports[port_ready_name]))
                            prev_stmt = first_if
                            if mux_size == 1:
                                prev_stmt.else_(new_input_ready_fifo.assign(1))
                                # mux_comb.add_stmt(new_input_ready_fifo.assign(1))
                            on_first = False

                        else:
                            chain_if = IfStmt(self._mode == kts.const(mode_num, width=self._mode.width))
                            chain_if.then_(new_input_ready_fifo.assign(self.controllers_flat_dict[ctrl_name].ports[port_ready_name]))
                            prev_stmt.else_(chain_if)
                            prev_stmt = chain_if

                if any_dense and not any_rvs:

                    # Wire all the inputs to the data/valid, then mux the ready
                    for (ctrl_name, port, mode_num, _) in dense:
                        self.wire(new_input, self.controllers_flat_dict[ctrl_name].ports[port])

                # Need to handle the ready set to 1
                elif any_dense and any_rvs:

                    # Wire all the inputs to the data/valid, then mux the ready
                    for (ctrl_name, port, (mode_num, b_ex), _) in dense:
                        self.wire(new_input, self.controllers_flat_dict[ctrl_name].ports[port])
                        tmp_ready_choose = (kts.const(1, 1), mode_num)
                        output_ready_map.append(tmp_ready_choose)

                if not any_dense and not any_rvs:
                    # We currently don't allow r/v to be 1b signals
                    # Basically handle 1b inputs w/o r/v
                    for (ctrl_name, port) in signal_dict.items():
                        self.wire(new_input, self.controllers_flat_dict[ctrl_name].ports[port])

                if any_rvs:
                    print(output_ready_map)
                    self.create_mode_based_mux(out_sig=new_input_ready,
                                               items=output_ready_map,
                                               default=1)

                for (ctrl_name, port) in signal_dict.items():

                    # Handle mapping information
                    # Now check if the input is dense or ready/valid so we can hardwire 1 on the ready/not wire the valid
                    normal_ctrl_name = self.flat_to_c[ctrl_name]
                    ctrler = self.controllers_dict[normal_ctrl_name]
                    ctrl_mode_mapping_name = ctrler.get_config_mode_str()
                    if ctrl_mode_mapping_name not in self.port_remap_dict:
                        self.port_remap_dict[ctrl_mode_mapping_name] = {}
                    # Now get unflat name
                    # First get flat name and internal generator and find its sink
                    flat_ig = self.controllers_flat_dict[ctrl_name].internal_generator
                    flat_ig_port = flat_ig.get_port(port)
                    flat_ig_port_sinks = flat_ig_port.sinks
                    assert len(flat_ig_port_sinks) == 1, f"{flat_ig_port} somehow got multiple sinks..."
                    unflat_name = None
                    for p in flat_ig_port_sinks:
                        unflat_name = p.left.name
                        if unflat_name == "":
                            # Then we need to mess with some stuff to flatten the name...
                            unflat_name = str(p.left)
                            unflat_base = unflat_name.split("[")[0]
                            unflat_num = unflat_name.split("[")[1].rstrip(']')
                            unflat_name = f"{unflat_base}_{unflat_num}"
                    self.port_remap_dict[ctrl_mode_mapping_name][unflat_name] = f'{self.io_prefix}input_width_{input_width}_num_{i}'

    def realize_outputs(self):
        '''
        This function creates the outputs from the tile, tags them, and wires them to the ports of the controllers
        '''
        for (output_width, signal_dicts) in self.outputs_dict.items():
            for (i, signal_dict) in enumerate(signal_dicts):
                if output_width != 1:
                    new_output = self.output(f'{self.io_prefix}output_width_{output_width}_num_{i}', width=output_width, explicit_array=True, packed=True)
                else:
                    new_output = self.output(f'{self.io_prefix}output_width_{output_width}_num_{i}', width=output_width)
                new_output.add_attribute(ControlSignalAttr(False))

                rvs = [(ctrl_, port_.name, self.ctrl_to_mode[self.flat_to_c[ctrl_]], hybrid_) for ctrl_, port_, hybrid_ in self.rv if ctrl_ in signal_dict and signal_dict[ctrl_] == port_.name]
                any_rvs = len(rvs) > 0
                dense = [(ctrl_, port_.name, self.ctrl_to_mode[self.flat_to_c[ctrl_]], hybrid_) for ctrl_, port_, hybrid_ in self.dense if ctrl_ in signal_dict and signal_dict[ctrl_] == port_.name]
                any_dense = len(dense) > 0

                hybrid_bypass = None
                output_valid_map = []
                output_data_map = []

                if any_rvs:
                    # Now create the ready/valid pair and deal with it
                    new_output_valid = self.output(f'{self.io_prefix}output_width_{output_width}_num_{i}_valid', width=1)
                    new_output_valid.add_attribute(ControlSignalAttr(False))

                    new_output_ready = self.input(f'{self.io_prefix}output_width_{output_width}_num_{i}_ready', width=1)
                    new_output_ready.add_attribute(ControlSignalAttr(True))

                    # Add in the fifo if there are any fifos on this path
                    new_reg_fifo = RegFIFO(data_width=output_width,
                                           width_mult=1, depth=self.fifo_depth,
                                           defer_hrdwr_gen=False)

                    self.add_child(f"output_width_{output_width}_num_{i}_output_fifo",
                                   new_reg_fifo,
                                   clk=self._gclk,
                                   rst_n=self._rst_n,
                                   #    clk_en=kts.const(1, 1),
                                   pop=new_output_ready)

                    # self.wire(new_output_valid, ~new_reg_fifo.ports.empty)

                    # Alias the new output across the fifo boundary
                    if output_width != 1:
                        new_output_fifo = self.var(f'output_width_{output_width}_num_{i}_fifo_in', width=output_width, explicit_array=True, packed=True)
                    else:
                        new_output_fifo = self.var(f'output_width_{output_width}_num_{i}_fifo_in', width=output_width)

                    new_output_valid_fifo = self.var(f'output_width_{output_width}_num_{i}_fifo_in_valid', width=1)
                    new_output_ready_fifo = self.var(f'output_width_{output_width}_num_{i}_fifo_in_ready', width=1)

                    self.wire(new_output_fifo, new_reg_fifo.ports.data_in)
                    self.wire(new_output_ready_fifo, ~new_reg_fifo.ports.full)
                    self.wire(new_output_valid_fifo, new_reg_fifo.ports.push)

                    # Mux all the inputs to the fifo, broadcast ready back
                    mux_size = len(rvs)
                    mux_comb = self.combinational()
                    prev_stmt = None
                    # Default assign 0 to prevent latches.
                    mux_comb.add_stmt(new_output_fifo.assign(0))
                    mux_comb.add_stmt(new_output_valid_fifo.assign(0))
                    on_first = True

                    # for (ctrl_name, port, mode_num, hybrid) in rvs:
                    for (ctrl_name, port, (mode_num, b_ex), hybrid) in rvs:

                        if hybrid and hybrid_bypass is None:
                            hybrid_bypass = self.input(f"{self.io_prefix}output_width_{output_width}_num_{i}_dense", 1)
                            hybrid_bypass.add_attribute(ConfigRegAttr(f"Choose for {self.io_prefix}output_width_{output_width}_num_{i}_dense to bypass input fifo"))

                        port_valid_name = f"{port.rstrip('_f_')}_valid_f_"
                        port_ready_name = f"{port.rstrip('_f_')}_ready_f_"
                        # Wire ready_in if this is a ready/valid port
                        if hybrid:
                            self.wire(self.controllers_flat_dict[ctrl_name].ports[port_ready_name], kts.ternary(hybrid_bypass,
                                                                                                                # new_output_ready,
                                                                                                                kts.const(1, 1),
                                                                                                                new_output_ready_fifo))
                            # Choose between the controller's valid or the fifo valid
                            tmp_valid_choose = (kts.ternary(hybrid_bypass,
                                                            # self.controllers_flat_dict[ctrl_name].ports[port_valid_name],
                                                            kts.const(1, 1),
                                                            ~new_reg_fifo.ports.empty), mode_num)
                            tmp_data_choose = (kts.ternary(hybrid_bypass,
                                                            self.controllers_flat_dict[ctrl_name].ports[port],
                                                            new_reg_fifo.ports.data_out), mode_num)

                        else:
                            self.wire(new_output_ready_fifo, self.controllers_flat_dict[ctrl_name].ports[port_ready_name])
                            # Choose between the controller's valid or the fifo valid
                            tmp_valid_choose = (~new_reg_fifo.ports.empty, mode_num)
                            tmp_data_choose = (new_reg_fifo.ports.data_out, mode_num)

                        output_valid_map.append(tmp_valid_choose)
                        output_data_map.append(tmp_data_choose)

                        if mux_size == 1:
                            # If there's any rvs, then this one is in it...can assign it directly
                            mux_comb.add_stmt(new_output_fifo.assign(self.controllers_flat_dict[ctrl_name].ports[port]))
                            mux_comb.add_stmt(new_output_valid_fifo.assign(self.controllers_flat_dict[ctrl_name].ports[port_valid_name]))
                        else:
                            # Index is set off the controller mapping
                            # ctrl_name_unflat = self.flat_to_c[ctrl_name]
                            # idx = self.ctrl_to_mode[ctrl_name_unflat]
                            if on_first is True:
                                first_if = mux_comb.if_(self._mode == kts.const(mode_num, width=self._mode.width))
                                # Figure out if an output from a controller of the mode idx is actually defined or not
                                assigns = [new_output_fifo.assign(self.controllers_flat_dict[ctrl_name].ports[port])]
                                assigns.append(new_output_valid_fifo.assign(self.controllers_flat_dict[ctrl_name].ports[port_valid_name]))
                                first_if.then_(*assigns)
                                prev_stmt = first_if
                                on_first = False

                            else:
                                chain_if = IfStmt(self._mode == kts.const(mode_num, width=self._mode.width))
                                assigns = [new_output_fifo.assign(self.controllers_flat_dict[ctrl_name].ports[port])]
                                assigns.append(new_output_valid_fifo.assign(self.controllers_flat_dict[ctrl_name].ports[port_valid_name]))
                                # chain_if.then_(new_output.assign(self.controllers_flat_dict[ctrl_name].ports[port]))
                                chain_if.then_(*assigns)
                                prev_stmt.else_(chain_if)
                                prev_stmt = chain_if

                # assert any_rvs or any_dense

                # Now that we potentially created the rv...
                if any_rvs and not any_dense:
                    pass
                # elif not any_rvs and any_dense:
                elif not any_rvs:

                    # TODO: Generalize - getting stencil_valid working here...

                    # If just dense, just do a simple mux - assumed 1b here
                    for (ctrl_name, port) in signal_dict.items():
                        unflat_name = self.flat_to_c[ctrl_name]
                        # if "stencil_valid" in unflat_name:
                        #     print("GOT STENCIL VALID")
                        # print(self.controllers_dict[unflat_name].get_exclusive())
                        mode_num, b_ex = self.ctrl_to_mode[unflat_name]
                        tmp_data_choose = (self.controllers_flat_dict[ctrl_name].ports[port], mode_num, b_ex)
                        output_data_map.append(tmp_data_choose)

                elif any_rvs and any_dense:

                    for (ctrl_name, port, (mode_num, b_ex), hybrid_) in dense:
                        tmp_data_choose = (self.controllers_flat_dict[ctrl_name].ports[port], mode_num)
                        output_data_map.append(tmp_data_choose)
                        tmp_valid_choose = (kts.const(1, 1), mode_num)
                        output_valid_map.append(tmp_valid_choose)

                else:
                    # This can only happen when there is 1b output that is not ready/valid
                    # No support for 1b ready valid anyway.
                    raise NotImplementedError

                # Create the final output muxes
                self.create_mode_based_mux(out_sig=new_output, items=output_data_map, default=0)
                if any_rvs:
                    print(output_valid_map)
                    self.create_mode_based_mux(out_sig=new_output_valid, items=output_valid_map, default=0)

                # We need to choose which output is hooked up based on the mode...
                for (ctrl_name, port) in signal_dict.items():

                    # Add mapping information
                    normal_ctrl_name = self.flat_to_c[ctrl_name]
                    ctrler = self.controllers_dict[normal_ctrl_name]
                    ctrl_mode_mapping_name = ctrler.get_config_mode_str()
                    if ctrl_mode_mapping_name not in self.port_remap_dict:
                        self.port_remap_dict[ctrl_mode_mapping_name] = {}
                    # Now get unflat name
                    # First get flat name and internal generator and find its sink
                    flat_ig = self.controllers_flat_dict[ctrl_name].internal_generator
                    flat_ig_port = flat_ig.get_port(port)
                    flat_ig_port_sources = flat_ig_port.sources
                    assert len(flat_ig_port_sources) == 1, f"{flat_ig_port} somehow got multiple sinks..."
                    unflat_name = None
                    for p in flat_ig_port_sources:
                        unflat_name = p.right.name
                        if unflat_name == "":
                            # Then we need to mess with some stuff to flatten the name...
                            unflat_name = str(p.right)
                            unflat_base = unflat_name.split("[")[0]
                            unflat_num = unflat_name.split("[")[1].rstrip(']')
                            unflat_name = f"{unflat_base}_{unflat_num}"
                    self.port_remap_dict[ctrl_mode_mapping_name][unflat_name] = f'{self.io_prefix}output_width_{output_width}_num_{i}'

    def add_mem_port_connection(self, local_port, ctrl_ports, port_num=0):
        '''
        This function handles building the mux to the memory port given the local
        port in the interface and a dict of all controller ports attempting to connect
        '''
        local_intf = local_port.get_port_interface()
        mux_size = len(ctrl_ports)

        ignore_sigs = []
        alt_sigs_ = []

        if self.tech_map is not None:
            # Should only use this if there is a tech map
            ignore_sigs = self.tech_map['ports'][port_num]['alt_sigs'].keys()
            alt_sigs_ = self.tech_map['ports'][port_num]['alt_sigs']

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
            if 'out' not in name and name not in ignore_sigs:
                mux_comb.add_stmt(port.assign(0))
            if name in ignore_sigs:
                # Check here if it's an alt signal that should be used
                v_, w_ = alt_sigs_[name]
                if type(v_) is not int:
                    self.wire(port, v_)

        print("Printing mode map...")
        print(self.ctrl_to_mode)

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
                ass_stmt = [local_intf[name].assign(ctrl_intf[name]) for name in mux_list if name not in ignore_sigs]
                # Mux in the inputs
                if idx == 0:
                    mode_num, b_ex = self.ctrl_to_mode[ctrl_name]
                    first_if = mux_comb.if_(self._mode == kts.const(mode_num, width=self._mode.width))
                    first_if.then_(*ass_stmt)
                    self.mem_port_mux_if[local_port] = first_if
                    prev_stmt = first_if
                else:
                    mode_num, b_ex = self.ctrl_to_mode[ctrl_name]
                    chain_if = IfStmt(self._mode == kts.const(mode_num, width=self._mode.width))
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
                self.add_mem_port_connection(self.memories[bank].get_ports()[port],
                                             self.mem_conn[bank][port], port_num=port)

    def get_modes_supported(self):
        self.get_mode_map()
        return self.modes_supported

    def get_mode_map(self):
        '''
        Sort of hardcoded/hacky way to refer to specific controller types
        '''
        self.mode_map = {}
        self.modes_supported = []
        for memctrl in self.controllers:
            self.mode_map[memctrl.get_config_mode_str()] = memctrl
            self.modes_supported.append(memctrl.get_config_mode_str())
        return self.mode_map

    def set_bit(self, old_val, bit_to_set, new_bit):
        new_val = old_val | (new_bit << bit_to_set)
        return new_val

    def get_bit(self, val, n):
        return (val >> n & 1)

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
        stencil_valid_used = False
        if 'stencil_valid' in config_json and 'stencil_valid' in mode_map:
            stencil_valid_used = True
            ctrl_config['stencil_valid'] = mode_map['stencil_valid'].get_bitstream(config_json)
            config.append(("mode_excl", 1))

        if 'mode' in config_json:
            mode_used = config_json['mode']
            if self.num_modes > 1:
                # Locate the controller in the list...
                for idx, ctrl in enumerate(self.controllers):
                    if mode_used == ctrl.get_config_mode_str():
                        if not stencil_valid_used:
                            print(f"Found ctrl: {mode_used}")
                            config.append(("mode", idx))
                        break

            ctrl_to_conf = mode_map[mode_used]
            # Have some guard to see if config is in there or not...
            if 'config' in config_json:
                ctrl_config[str(ctrl_to_conf)] = ctrl_to_conf.get_bitstream(config_json['config'])
            else:
                ctrl_config[str(ctrl_to_conf)] = ctrl_to_conf.get_bitstream(config_json)

        # Now need to chop up cfg space
        tmp_cfg_space = [0 for i in range(self.num_chopped_cfg)]

        for (ctrl, conf_for_ctrl) in ctrl_config.items():
            # Go through each config and prepend the string
            prepend_string = f"mem_ctrl_{ctrl}_flat_{ctrl}_inst_"
            for cfg_reg, val in conf_for_ctrl:
                val_int = int(val)
                # Hasn't been chopped...
                if f"{ctrl}_inst_{cfg_reg}" in self.config_mapping[ctrl]:
                    mapping_index = self.config_mapping[ctrl][f"{ctrl}_inst_{cfg_reg}"]
                    map_hi, map_lo = mapping_index
                else:
                    # Recover the original mapping of the chopped config reg
                    unchopped_int = cfg_reg.split('_')[-1]
                    unchopped = cfg_reg.rstrip(f"_{unchopped_int}")
                    assert f"{ctrl}_inst_{unchopped}" in self.config_mapping[ctrl]
                    tmp_hi, tmp_lo = self.config_mapping[ctrl][f"{ctrl}_inst_{unchopped}"]
                    map_lo = self.allowed_reg_size * int(unchopped_int) + tmp_lo
                    if tmp_hi - map_lo >= self.allowed_reg_size:
                        map_hi = map_lo + self.allowed_reg_size - 1
                    else:
                        map_hi = tmp_hi
                assert map_hi - map_lo < self.allowed_reg_size, f"Failed beacuse reg wider than {self.allowed_reg_size} bits"
                chunk_hi = map_hi // self.allowed_reg_size
                chunk_lo = map_lo // self.allowed_reg_size
                # Either all within one chunk...
                if chunk_hi == chunk_lo:
                    bits_hi = map_hi - chunk_hi * self.allowed_reg_size
                    bits_lo = map_lo - chunk_lo * self.allowed_reg_size
                    num_bits = bits_hi - bits_lo + 1
                    tmp_val = tmp_cfg_space[chunk_lo]
                    for z_ in range(num_bits):
                        tmp_val = self.set_bit(tmp_val, z_ + bits_lo, self.get_bit(val_int, z_))
                    tmp_cfg_space[chunk_lo] = tmp_val
                # Or across the boundary...
                else:
                    bits_hi = map_hi - chunk_hi * self.allowed_reg_size + 1
                    bits_lo = map_lo - chunk_lo * self.allowed_reg_size
                    num_bits_lo = self.allowed_reg_size - bits_lo
                    assert (bits_hi + num_bits_lo) == (map_hi - map_lo + 1)
                    tmp_val = tmp_cfg_space[chunk_lo]
                    for z_ in range(num_bits_lo):
                        tmp_val = self.set_bit(tmp_val, z_ + bits_lo, self.get_bit(val_int, z_))
                    tmp_cfg_space[chunk_lo] = tmp_val

                    tmp_val = tmp_cfg_space[chunk_hi]
                    for z_ in range(bits_hi):
                        tmp_val = self.set_bit(tmp_val, z_, self.get_bit(val_int, num_bits_lo + z_))
                    tmp_cfg_space[chunk_hi] = tmp_val

        for idx in range(self.num_chopped_cfg):
            config.append((f"CONFIG_SPACE_{idx}", tmp_cfg_space[idx]))
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

        input_str = ""
        input_sorted_keys = sorted(self.inputs_dict)
        for key in input_sorted_keys:
            input_str += f"{key}: {self.inputs_dict[key]}\n"

        output_str = ""
        output_sorted_keys = sorted(self.outputs_dict)
        for key in output_sorted_keys:
            output_str += f"{key}: {self.outputs_dict[key]}\n"

        rep_str = "=====MEMORY TILE BUILDER=====\n===CONTROLLERS===\n"
        rep_str += f"{self.controllers_flat}\n===INPUTS===\n{input_str}"
        rep_str += f"===OUTPUTS===\n{output_str}===MEMPORTS===\n"
        for i in range(self.memory_banks):
            for j in range(self.memory_interface.get_num_ports()):
                rep_str += f"BANK: {i}\t PORT: {j}\n"
                for (k, (ctrl, port)) in enumerate(self.mem_conn[i][j].items()):
                    rep_str += f"CONN {k}\tCONTROLLER: {ctrl}\tPORT: {port}\n"
        return rep_str[:-1]
