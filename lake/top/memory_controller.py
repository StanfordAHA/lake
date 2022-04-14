from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.top.memory_interface import MemoryPort, MemoryPortExclusionAttr
from lake.attributes.config_reg_attr import ConfigRegAttr
import kratos as kts
from kratos.generator import PortDirection
import _kratos


class MemoryController(kts.Generator):
    '''
    Provides the utilities to interface a memory controller with a memory interface
    '''
    def get_inputs(self):
        '''
        Use this method to return pure inputs to the tile interface
        '''
        ins = []
        int_gen = self.internal_generator
        # Now get the config registers from the top definition
        for port_name in int_gen.get_port_names():
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
            memport_excl_attr = curr_port.find_attribute(lambda a: isinstance(a, MemoryPortExclusionAttr))
            port_dir = PortDirection(curr_port.port_direction)
            if len(attrs) > 0 or len(memport_excl_attr) > 0 or "clk" in curr_port.name or "rst_n" in curr_port.name:
                continue
            if port_dir == PortDirection.Out:
                continue
            ins.append((curr_port, curr_port.width))
        return ins

    def get_outputs(self):
        '''
        Use this method to return pure outputs to the tile interface
        '''
        outs = []
        int_gen = self.internal_generator
        # Now get the config registers from the top definition
        for port_name in int_gen.get_port_names():
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
            memport_excl_attr = curr_port.find_attribute(lambda a: isinstance(a, MemoryPortExclusionAttr))
            port_dir = PortDirection(curr_port.port_direction)
            if len(attrs) > 0 or len(memport_excl_attr) > 0 or "clk" in curr_port.name or "rst_n" in curr_port.name:
                continue
            if port_dir == PortDirection.In:
                continue
            outs.append((curr_port, curr_port.width))
        return outs

    def get_liftable_ports(self):
        '''
        Use this method to return all other ports that can be safely lifted
        '''
        liftable = []
        int_gen = self.internal_generator
        # Now get the config registers from the top definition
        for port_name in int_gen.get_port_names():
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
            memport_excl_attr = curr_port.find_attribute(lambda a: isinstance(a, MemoryPortExclusionAttr))
            port_dir = PortDirection(curr_port.port_direction)
            if len(memport_excl_attr) > 0:
                continue
            if "clk" in curr_port.name or "rst_n" in curr_port.name:
                liftable.append(curr_port)

        return liftable

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        raise NotImplementedError

    def get_bitstream(self, config_json, prefix=""):
        '''
        Pass in a config-related json to return a list of
        (config_reg, value) tuples
        '''
        raise NotImplementedError

    def get_config_mode_str(self):
        '''
        Use this function to define a unique name for the controller type
        - useful when referring through the CoreIR JSON
        '''
        raise NotImplementedError

    def get_dedicated_clock(self):
        return False

    def __str__(self):
        return self.name


class MemoryControllerFlatWrapper(MemoryController):
    '''
    This class exists to take in a memory controller and flatten any
    inputs and outputs, while preserving config registers and memory ports
    '''
    def __init__(self, mem_ctrl: MemoryController, legal_list=[1, 16]):
        super().__init__(f"{str(mem_ctrl)}_flat", debug=True)
        self.mem_ctrl = mem_ctrl
        self.add_child(f"{mem_ctrl}_inst", mem_ctrl)
        self.legal_widths = legal_list
        # Sort the legal widths in descending order...
        self.legal_widths.sort(reverse=True)
        self.lift_ports()
        self.flatten_inputs()
        self.flatten_outputs()
        self.lift_memory_ports()

    def lift_ports(self):
        liftable_ports = self.mem_ctrl.get_liftable_ports()
        for port in liftable_ports:
            pname = port.name
            if pname == "clk":
                tmp_clk = self.clock("clk")
                self.wire(tmp_clk, port)
            elif pname == "rst_n":
                tmp_rst = self.reset("rst_n")
                self.wire(tmp_rst, port)
            else:
                # Need to get the attributes and copy them up...
                port_attrs = port.attributes
                tmp_port = self.port_from_def(port, name=f"{port.name}_f")
                for attr in port_attrs:
                    tmp_port.add_attribute(attr)
                self.wire(tmp_port, port)

    def flatten_port(self, port, in_outn=True, name="", suffix=""):
        '''
        Flatten a port and bring it up based on the width/size of the port
        '''
        port_width = port.width
        if port.name == "flush":
            return
        # Port width either has to be in the legal_list or
        # 1 must be in the list - if width is 1, then the size[0] should be 1 as well
        # if port_width in self.legal_widths or (1 in self.legal_widths and port.size[0] == 1):
        if port_width in self.legal_widths or 1 in self.legal_widths:
            # Now we can handle it
            port_dim = len(port.size)
            # This is a single port - check for widest compatibility
            if port.size[0] == 1 and port_dim == 1:
                # Already sorted in descending order, so we will find the largest compatible...
                for legal_width in self.legal_widths:
                    # Too big - keep looking
                    if legal_width > port_width:
                        continue
                    # Correct size - copy it
                    elif legal_width == port_width:
                        if in_outn:
                            if port_width != 1:
                                # If it is not packed, wire it into a 1D packed deal
                                if not port.is_packed:
                                    intercept = self.var(f"{name}_f_{suffix}_intercept", port_width, explicit_array=True, packed=True)
                                    self.wire(intercept[0], port)
                                    port = intercept
                                tmp_port = self.input(f"{name}_f_{suffix}", port_width, explicit_array=True, packed=True)
                            else:
                                tmp_port = self.input(f"{name}_f_{suffix}", port_width)
                        else:
                            if port_width != 1:
                                if not port.is_packed:
                                    intercept = self.var(f"{name}_f_{suffix}_intercept", port_width, explicit_array=True, packed=True)
                                    self.wire(intercept[0], port)
                                    port = intercept
                                tmp_port = self.output(f"{name}_f_{suffix}", port_width, explicit_array=True, packed=True)
                            else:
                                tmp_port = self.output(f"{name}_f_{suffix}", port_width)
                        self.wire(tmp_port, port)
                        # Copy attributes
                        self.copy_attributes(copy_to=tmp_port, copy_from=port)
                        break
                    # Still a mismatch
                    elif legal_width < port_width and legal_width != 1:
                        continue
                    # Legal width of 1 is guaranteed to be here if there wasn't a direct port width match
                    elif legal_width < port_width and legal_width == 1:
                        for bit in range(port_width):
                            alt_suffix = suffix
                            if len(alt_suffix) > 0:
                                alt_suffix += "_"
                            if in_outn:
                                tmp_bit = self.input(f"{name}_f_{alt_suffix}b_{bit}", 1)
                            else:
                                tmp_bit = self.output(f"{name}_f_{alt_suffix}b_{bit}", 1)
                            self.wire(tmp_bit, port[bit])
                            # Copy attributes
                            self.copy_attributes(copy_to=tmp_bit, copy_from=port)
                        break
            # This is a 2D signal
            elif port.size[0] > 1 and port_dim == 1:
                # Flatten to a bunch of different ports recursively
                for i in range(port.size[0]):
                    self.flatten_port(port[i], in_outn=in_outn, name=port.name, suffix=f"{i}")
            # For now, make it illegal to flatten 3D ports...since we don't really use them
            else:
                print("CANNOT FLATTEN MORE THAN 2D SIGNALS")
                raise NotImplementedError
        else:
            print(f"Cannot flatten this port...{port.name}, {port.width}, {port.size}")

    def copy_attributes(self, copy_to, copy_from):
        for attr in copy_from.attributes:
            copy_to.add_attribute(attr)

    def flatten_inputs(self):
        child_ins = self.mem_ctrl.get_inputs()
        for (inp, width) in child_ins:
            self.flatten_port(inp, in_outn=True, name=inp.name)

    def flatten_outputs(self):
        child_outs = self.mem_ctrl.get_outputs()
        for (outp, width) in child_outs:
            self.flatten_port(outp, in_outn=False, name=outp.name)

    def lift_memory_port(self, mem_prt: MemoryPort):
        new_mem_prt = MemoryPort(mem_prt.get_port_type(), mem_prt.get_port_delay(), mem_prt.get_active_read())
        new_mem_prt_intf = new_mem_prt.get_port_interface()
        # These interfaces should match directly...
        for (name, sig) in mem_prt.get_port_interface().items():
            # These should all comply anyway, so just need to recreate them
            try:
                if sig is None:
                    continue
                # Check if slice and lift the parent
                if isinstance(sig, _kratos.VarSlice):
                    sig = sig.parent_var
                # if self.internal_generator.has_port(f"{sig}_lifted"):
                #     continue
                lifted_port = self.port_from_def(sig, name=f"{sig}_lifted")
                new_mem_prt_intf[name] = lifted_port
                self.wire(lifted_port, sig)
                for attr in sig.attributes:
                    lifted_port.add_attribute(attr)
            except kts.VarException:
                print("Port already exists...copying into MemoryPort")
                new_mem_prt_intf['read_addr'] = new_mem_prt_intf['write_addr']
        return new_mem_prt

    def handle_duplicate_address(self):
        pass

    def lift_memory_ports(self):
        child_mem_ports = self.mem_ctrl.get_memory_ports()
        # Get a deep copy so we have the correct dimensions
        self.mem_ports = child_mem_ports.copy()
        for bank in range(len(child_mem_ports)):
            for port in range(len(child_mem_ports[0])):
                if child_mem_ports[bank][port] is not None:
                    self.mem_ports[bank][port] = self.lift_memory_port(child_mem_ports[bank][port])

    def get_memory_ports(self):
        return self.mem_ports

    def get_dedicated_clock(self):
        return self.mem_ctrl.get_dedicated_clock()
