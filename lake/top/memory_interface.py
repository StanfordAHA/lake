import kratos as kts
from enum import Enum
from kratos.stmts import *
import _kratos
import math
from lake.utils.util import register


class MemoryPortExclusionAttr(kts.Attribute):
    def __init__(self):
        super().__init__()


class MemoryPortType(Enum):
    READ = 0
    WRITE = 1
    READWRITE = 2


class MemoryPort():

    def __init__(self, mpt: MemoryPortType, delay=1, active_read=True) -> None:
        self.mpt = mpt
        self.delay = delay
        self.active_read = active_read

        self.port_interface = {}
        self.port_interface_set = False
        # self.create_port_interface()

    def create_port_interface(self):
        if self.mpt == MemoryPortType.READ:
            self.port_interface['data_out'] = None
            self.port_interface['read_addr'] = None
            self.port_interface['read_enable'] = None
            self.port_interface_set = True
        elif self.mpt == MemoryPortType.WRITE:
            self.port_interface['data_in'] = None
            self.port_interface['write_addr'] = None
            self.port_interface['write_enable'] = None
            self.port_interface_set = True
        elif self.mpt == MemoryPortType.READWRITE:
            self.port_interface['data_in'] = None
            self.port_interface['data_out'] = None
            self.port_interface['write_addr'] = None
            self.port_interface['write_enable'] = None
            self.port_interface['read_addr'] = None
            self.port_interface['read_enable'] = None
            self.port_interface_set = True
        else:
            assert False, "Memory port has illegal interface"

    def get_port_interface(self):
        if self.port_interface_set is False:
            self.create_port_interface()
        return self.port_interface

    def get_port_type(self):
        return self.mpt

    def get_port_delay(self):
        return self.delay

    def get_active_read(self):
        if self.mpt == MemoryPortType.WRITE:
            return False
        else:
            return self.active_read

    def annotate_port_signals(self):
        '''
        This function adds a MemoryPortExclusionAttr to all signals -
        this helps filter them out during collection passes
        '''
        for (name, signal) in self.port_interface.items():
            if signal is not None:
                signal.add_attribute(MemoryPortExclusionAttr())
                if isinstance(signal, _kratos.VarSlice):
                    parent_sig = signal.parent_var
                    parent_sig.add_attribute(MemoryPortExclusionAttr())

    def __str__(self):
        conn_str = ""
        for (name, conn) in self.port_interface.items():
            if conn is None:
                conn_str += f"conn: {name}, DISCONNECTED\n"
            else:
                conn_str += f"conn: {name}, {conn.name}\n"
        return f"Port: Type: {self.get_port_type()}, conns:\n{conn_str}"


class PhysicalMemoryPort(MemoryPort):
    def __init__(self, mpt: MemoryPortType, delay, active_read, active_low=True, port_map=None):
        super().__init__(mpt, delay=delay, active_read=active_read)
        # Capture alternate inputs for testing commonly used in these memories.
        self.alt_sigs = None
        if 'alt_sigs' in port_map:
            self.alt_sigs = port_map['alt_sigs']
        self.active_low = active_low
        self.port_map = port_map

    def get_active_low(self):
        '''
        Lets us know if we need to do active low for the physical map
        '''
        return self.active_low

    def create_port_interface(self):
        # Unlike the virtual memory port, the physical memory port needs an inherent mapping
        # between the "logical" view of the physical ports and the actual names.
        assert self.port_map is not None, f"Need to provide port map first..."

        if self.mpt == MemoryPortType.READ:
            self.port_interface['data_out'] = self.port_map['data_out']
            self.port_interface['read_addr'] = self.port_map['read_addr']
            self.port_interface['cen'] = self.port_map['cen']
            self.port_interface['clk'] = self.port_map['clk']
            self.port_interface_set = True
        elif self.mpt == MemoryPortType.WRITE:
            self.port_interface['data_in'] = self.port_map['data_in']
            self.port_interface['write_addr'] = self.port_map['write_addr']
            self.port_interface['write_enable'] = self.port_map['write_enable']
            self.port_interface['cen'] = self.port_map['cen']
            self.port_interface['clk'] = self.port_map['clk']
            self.port_interface_set = True
        elif self.mpt == MemoryPortType.READWRITE:
            self.port_interface['data_in'] = self.port_map['data_in']
            self.port_interface['data_out'] = self.port_map['data_out']
            self.port_interface['write_enable'] = self.port_map['write_enable']
            self.port_interface['addr'] = self.port_map['addr']
            self.port_interface['cen'] = self.port_map['cen']
            self.port_interface['clk'] = self.port_map['clk']
            self.port_interface_set = True
        else:
            assert False, "Memory port has illegal interface"

        # Add in the alternate signals - usually setting to a specific value
        if self.alt_sigs is not None:
            for signal in self.alt_sigs:
                # (val, width) tuples
                self.port_interface[signal] = self.alt_sigs[signal]

    def set_port_map(self, port_map):
        self.port_map = port_map
        if 'alt_sigs' in port_map:
            self.alt_sigs = port_map['alt_sigs']

    def get_port_map(self):
        return self.port_map

    def get_alt_signals(self):
        return self.alt_sigs


class PhysicalMemoryStub(kts.Generator):
    def __init__(self, name: str, debug=True, mem_params: dict = None, ports: list = None, tech_map: dict = None):
        super().__init__(name, debug=True)
        if mem_params is None:
            self.mem_params = {}
        else:
            self.mem_params = mem_params
        if ports is None:
            self.ports = []
        else:
            self.ports = ports
        assert tech_map is not None, f"tech_map is None"
        self.tech_map = tech_map
        # The name will be the name of the stub
        # Set external to true so that the internals aren't generated when instantiating
        self.external = True
        # Literally the IO for the stub
        self.mem_params = mem_params
        self.ports = ports
        self.mem_width = MemoryInterface.mem_width_dflt if 'mem_width' not in mem_params else mem_params['mem_width']
        self.mem_depth = MemoryInterface.mem_depth_dflt if 'mem_depth' not in mem_params else mem_params['mem_depth']
        self.composed = False

        # First create the top level interface to the physical sram
        self.create_interface()
        # Then run a pass to instantiate more of the tech_map
        self.compose_mems()

    def create_interface(self):
        # Create the port interfaces - clocks belong here now since there are actually clocks
        for port in self.get_ports():
            port_intf = port.get_port_interface()
            port_type = port.get_port_type()
            # The slight deviation here
            if port_type == MemoryPortType.READ:
                # Read port has data out, address, and ren
                port_intf['data_out'] = self.output(port_intf['data_out'], self.mem_width, packed=True)
                port_intf['read_addr'] = self.input(port_intf['read_addr'], kts.clog2(self.mem_depth), packed=True)
                port_intf['cen'] = self.input(port_intf['cen'], 1)
                port_intf['clk'] = self.clock(port_intf['clk'])
            elif port_type == MemoryPortType.WRITE:
                port_intf['data_in'] = self.input(port_intf['data_in'], self.mem_width, packed=True)
                port_intf['write_addr'] = self.input(port_intf['write_addr'], kts.clog2(self.mem_width), packed=True)
                port_intf['write_enable'] = self.input(port_intf['write_enable'], 1)
                port_intf['cen'] = self.input(port_intf['cen'], 1)
                port_intf['clk'] = self.clock(port_intf['clk'])
            elif port_type == MemoryPortType.READWRITE:
                print('HERE')
                print(port_intf)
                print(port_intf['data_in'])
                port_intf['data_in'] = self.input(port_intf['data_in'], self.mem_width, packed=True)
                port_intf['data_out'] = self.output(port_intf['data_out'], self.mem_width, packed=True)
                port_intf['write_enable'] = self.input(port_intf['write_enable'], 1)
                port_intf['addr'] = self.input(port_intf['addr'], kts.clog2(self.mem_depth), packed=True)
                port_intf['cen'] = self.input(port_intf['cen'], 1)
                port_intf['clk'] = self.clock(port_intf['clk'])
            # For now, assume the alt sigs are all inputs
            print(port.get_alt_signals())
            for (alt_sig, (value, width)) in port.get_alt_signals().items():
                print(alt_sig)
                print(width)
                port_intf[alt_sig] = self.input(str(alt_sig), width)

    def compose_mems(self):
        '''
            compose_mems will find out if the tech map mismatches the dimensions of the memory and
            create a composed array of them to match the final interface
        '''
        tech_depth = self.tech_map['depth']
        tech_width = self.tech_map['width']
        num_deep = 1
        if tech_depth == self.mem_depth and tech_width == self.mem_width:
            # If the dimensions match there's nothing to do....
            return
        # Mark this as a composed memory so we know how to deal with stubbing later.
        self.composed = True
        self.external = False
        if tech_depth < self.mem_depth:
            num_deep = math.ceil(self.mem_depth / tech_depth)
        num_wide = 1
        if tech_width < self.mem_width:
            num_wide = math.ceil(self.mem_width / tech_width)
        self.name = self.name + f"_{num_wide}_wide_{num_deep}_deep"
        child_mem_params = {
            'mem_width': tech_width,
            'mem_depth': tech_depth
        }

        # Address range for decoding
        addr_bottom = kts.clog2(tech_depth)
        addr_top = addr_bottom + kts.clog2(num_deep) - 1
        self.composed_children = {}
        for x in range(num_wide):
            for y in range(num_deep):
                port_copies = []
                for port in self.ports:
                    # We have a physicalmemoryport here - copy it to the new stub
                    pt = port.get_port_type()
                    pd = port.get_port_delay()
                    pal = port.get_active_low()
                    pm = port.get_port_map()
                    port_copies.append(PhysicalMemoryPort(pt, pd, True, pal, pm))
                self.composed_children[(x, y)] = PhysicalMemoryStub(name=self.tech_map['name'],
                                                                    mem_params=child_mem_params,
                                                                    ports=port_copies,
                                                                    tech_map=self.tech_map)
                self.add_child(f"{self.tech_map['name']}_{x}_{y}",
                               self.composed_children[(x, y)])
                # Instantiated child, now need to hook up ports
        for i, port in enumerate(self.ports):
            port_intf = port.get_port_interface()
            port_type = port.get_port_type()
            port_delay = port.get_port_delay()
            # The slight deviation here
            if port_type == MemoryPortType.READ:

                concat_data_outs = []

                for y in range(num_deep):
                    # Get the width of children gens
                    child_ports_wide = [self.composed_children[(x, y)] for x in range(num_wide)]
                    # concat their ports and handle broadcasting data, addr
                    child_ports_wide_intf = [child.get_ports()[i].get_port_interface() for child in child_ports_wide]
                    concat_data_out = kts.concat(*[cintf['data_out'] for cintf in child_ports_wide_intf])
                    concat_data_outs.append(concat_data_out)
                    # self.wire(port_intf['data_out'], concat_data_out)
                    # Deal with data out (in the delay version)
                    concat_addr_in = kts.concat(*[cintf['read_addr'] for cintf in child_ports_wide_intf])
                    self.wire(port_intf['read_addr'], concat_addr_in)
                    for ix in range(num_wide):
                        self.wire(port_intf['clk'], child_ports_wide_intf[ix]['clk'])
                        # Now decode cen...
                        if num_deep == 1:
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'])
                        else:
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'] & (port_intf['read_addr'][addr_top, addr_bottom] == y))

                # If only 1 deep, just wire it up and leave, otherwise create a combinational block
                if num_deep == 1:
                    self.add_stmt(port_intf['data_out'].assign(concat_data_outs[0]))
                else:

                    data_out_mux_sel = self.var(f"port_{i}_data_out_mux_sel", addr_top - addr_bottom + 1)
                    self.wire(data_out_mux_sel, port_intf['read_addr'][addr_top, addr_bottom])

                    if port_delay == 1:
                        final_mux_sel = register(self, data_out_mux_sel, enable=port_intf['cen'], name=f'port_{i}_registered_mux_sel_data_out')
                    else:
                        final_mux_sel = data_out_mux_sel

                    prev_stmt = None
                    data_out_comb = self.combinational()
                    data_out_comb.add_stmt(port_intf['data_out'].assign(0))
                    on_first = True
                    for y in range(num_deep):
                        if on_first is True:
                            first_if = data_out_comb.if_(final_mux_sel == kts.const(y, final_mux_sel.width))
                            first_if.then_(port_intf['data_out'].assign(concat_data_outs[y]))
                            prev_stmt = first_if
                            on_first = False
                        else:
                            chain_if = IfStmt(final_mux_sel == kts.const(y, final_mux_sel.width))
                            chain_if.then_(port_intf['data_out'].assign(concat_data_outs[y]))
                            prev_stmt.else_(chain_if)
                            prev_stmt = chain_if

            elif port_type == MemoryPortType.WRITE:
                # Decode the cen/wen, broadcast data, addr to the mems
                for y in range(num_deep):
                    # Get the width of children gens
                    child_ports_wide = [self.composed_children[(x, y)] for x in range(num_wide)]
                    # concat their ports and handle broadcasting data, addr
                    child_ports_wide_intf = [child.get_ports()[i].get_port_interface() for child in child_ports_wide]
                    concat_data_in = kts.concat(*[cintf['data_in'] for cintf in child_ports_wide_intf])
                    self.wire(port_intf['data_in'], concat_data_in)
                    concat_addr_in = kts.concat(*[cintf['write_addr'] for cintf in child_ports_wide_intf])
                    self.wire(port_intf['write_addr'], concat_addr_in)
                    for ix in range(num_wide):
                        self.wire(port_intf['clk'], child_ports_wide_intf[ix]['clk'])
                        # Now decode write enable and cen through here...
                        if num_deep == 1:
                            self.wire(child_ports_wide_intf[ix]['write_enable'], port_intf['write_enable'])
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'])
                        else:
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'] & (port_intf['write_addr'][addr_top, addr_bottom] == y))
                            self.wire(child_ports_wide_intf[ix]['write_enable'], port_intf['write_enable'] & (port_intf['write_addr'][addr_top, addr_bottom] == y))

            elif port_type == MemoryPortType.READWRITE:

                concat_data_outs = []

                for y in range(num_deep):
                    # Get the width of children gens
                    child_ports_wide = [self.composed_children[(x, y)] for x in range(num_wide)]
                    # concat their ports and handle broadcasting data, addr
                    child_ports_wide_intf = [child.get_ports()[i].get_port_interface() for child in child_ports_wide]
                    concat_data_out = kts.concat(*[cintf['data_out'] for cintf in child_ports_wide_intf])
                    concat_data_outs.append(concat_data_out)

                    # self.wire(port_intf['data_out'], concat_data_out)
                    # Deal with data out (in the delay version)
                    concat_data_in = kts.concat(*[cintf['data_in'] for cintf in child_ports_wide_intf])
                    self.wire(port_intf['data_in'], concat_data_in)
                    # concat_addr_in = kts.concat(*[cintf['addr'] for cintf in child_ports_wide_intf])
                    # self.wire(port_intf['addr'], concat_addr_in)
                    for ix in range(num_wide):
                        self.wire(port_intf['clk'], child_ports_wide_intf[ix]['clk'])
                        self.wire(port_intf['addr'], child_ports_wide_intf[ix]['addr'])
                        # Now decode and cen through here...
                        if num_deep == 1:
                            self.wire(child_ports_wide_intf[ix]['write_enable'], port_intf['write_enable'])
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'])
                        else:
                            self.wire(child_ports_wide_intf[ix]['cen'], port_intf['cen'] & (port_intf['addr'][addr_top, addr_bottom] == y))
                            self.wire(child_ports_wide_intf[ix]['write_enable'], port_intf['write_enable'] & (port_intf['addr'][addr_top, addr_bottom] == y))

                # If only 1 deep, just wire it up and leave, otherwise create a combinational block
                if num_deep == 1:
                    self.add_stmt(port_intf['data_out'].assign(concat_data_outs[0]))
                else:

                    data_out_mux_sel = self.var(f"port_{i}_data_out_mux_sel", addr_top - addr_bottom + 1)
                    self.wire(data_out_mux_sel, port_intf['addr'][addr_top, addr_bottom])

                    if port_delay == 1:
                        # Only update the read out mux when it's a read
                        final_mux_sel = register(self, data_out_mux_sel, enable=port_intf['cen'] & ~port_intf['write_enable'], name=f'port_{i}_registered_mux_sel_data_out')
                    else:
                        final_mux_sel = data_out_mux_sel

                    # Build combinational mux - can't directly index since I created the object list
                    prev_stmt = None
                    data_out_comb = self.combinational()
                    data_out_comb.add_stmt(port_intf['data_out'].assign(0))
                    on_first = True
                    for y in range(num_deep):
                        if on_first is True:
                            first_if = data_out_comb.if_(final_mux_sel == kts.const(y, final_mux_sel.width))
                            first_if.then_(port_intf['data_out'].assign(concat_data_outs[y]))
                            prev_stmt = first_if
                            on_first = False
                        else:
                            chain_if = IfStmt(final_mux_sel == kts.const(y, final_mux_sel.width))
                            chain_if.then_(port_intf['data_out'].assign(concat_data_outs[y]))
                            prev_stmt.else_(chain_if)
                            prev_stmt = chain_if

            # For now, assume the alt sigs are all inputs
            for xx in range(num_wide):
                for yy in range(num_deep):
                    child_port_intf = self.composed_children[(xx, yy)].get_ports()[i].get_port_interface()
                    for (alt_sig, (value, width)) in port.get_alt_signals().items():
                        self.wire(child_port_intf[alt_sig], port_intf[alt_sig])

    def get_ports(self):
        return self.ports


class MemoryInterface(kts.Generator):
    '''
    This class provides the generator interface for all storage
    '''

    mem_width_dflt = 32
    mem_depth_dflt = 256

    def __init__(self, name: str, mem_params: dict, ports: list = None, sim_macro_n: bool = True, tech_map=None, reset_in_sim=False):
        super().__init__(name, debug=True)

        if ports is None:
            self.mem_ports = []
        else:
            self.mem_ports = ports

        self.tech_map_provided = False
        self.tech_map = tech_map
        self.sim_macro_n = sim_macro_n
        self.mem_params = mem_params
        # self.mem_ports = ports
        self.reset_in_sim = reset_in_sim

        self._clk = None
        self._rst_n = None

        if self.tech_map is not None:
            self.tech_map_provided = True

        self.mem_width = MemoryInterface.mem_width_dflt if 'mem_width' not in mem_params else mem_params['mem_width']
        self.mem_depth = MemoryInterface.mem_depth_dflt if 'mem_depth' not in mem_params else mem_params['mem_depth']

        self.create_interface()

    def has_reset(self):
        return self.reset_in_sim

    def get_clock(self):
        return self._clk

    def get_reset(self):
        if self.has_reset():
            return self._rst_n
        else:
            return None

    def create_interface(self):
        # Loop through the memory ports and create the proper signals - only need a few signals
        for pnum, port in enumerate(self.mem_ports):
            port_type = port.get_port_type()
            port_intf = port.get_port_interface()
            if port_type == MemoryPortType.READ:
                # Read port has data out, address, and ren
                port_intf['data_out'] = self.output(f"data_out_p{pnum}", self.mem_width, packed=True)
                port_intf['read_addr'] = self.input(f"read_addr_p{pnum}", kts.clog2(self.mem_depth), packed=True)
                port_intf['read_enable'] = self.input(f"read_enable_p{pnum}", 1)
            elif port_type == MemoryPortType.WRITE:
                port_intf['data_in'] = self.input(f"data_in_p{pnum}", self.mem_width, packed=True)
                port_intf['write_addr'] = self.input(f"write_addr_p{pnum}", kts.clog2(self.mem_width), packed=True)
                port_intf['write_enable'] = self.input(f"write_enable_p{pnum}", 1)
            elif port_type == MemoryPortType.READWRITE:
                port_intf['data_out'] = self.output(f"data_out_p{pnum}", self.mem_width, packed=True)
                port_intf['read_addr'] = self.input(f"read_addr_p{pnum}", kts.clog2(self.mem_depth), packed=True)
                port_intf['read_enable'] = self.input(f"read_enable_p{pnum}", 1)
                port_intf['data_in'] = self.input(f"data_in_p{pnum}", self.mem_width, packed=True)
                port_intf['write_addr'] = self.input(f"write_addr_p{pnum}", kts.clog2(self.mem_depth), packed=True)
                port_intf['write_enable'] = self.input(f"write_enable_p{pnum}", 1)

    def set_tech_map(self, tech_map):
        assert tech_map is not None, f"Need to provide valid tech map"
        self.tech_map = tech_map
        self.tech_map_provided = True

    def realize_hw(self):
        if self.sim_macro_n:
            self.create_simulatable_memory()
        else:
            assert self.tech_map_provided, f"Need to provide tech map for macro realization"
            self.create_physical_memory()

        # Probably need to resolve address conflicts?
        self.resolve_addr_conflicts()

    def resolve_addr_conflicts(self):
        pass

    def get_ports(self):
        return self.mem_ports

    def get_num_ports(self):
        return len(self.mem_ports)

    def create_simulatable_memory(self):
        '''
        Based on the ports within the memory
        '''
        # First create the data array...
        self._clk = self.clock("clk")
        if self.reset_in_sim:
            self._rst_n = self.reset("rst_n")
        self._data_array = self.var("data_array", width=self.mem_width, size=self.mem_depth)
        # Now add in the logic for the ports.
        for port in self.get_ports():
            port_type = port.get_port_type()
            if port_type == MemoryPortType.READ:
                self.realize_read_port(port)
            elif port_type == MemoryPortType.READWRITE:
                self.realize_readwrite_port(port)
            elif port_type == MemoryPortType.WRITE:
                self.realize_write_port(port)

    def realize_read_port(self, port: MemoryPort):
        pintf = port.get_port_interface()
        new_read = None
        # Standalone read ports may be comb (register file),
        # or they may be sequential if requiring an active read...
        # seq_comb_n = port.get_active_read() or (port.get_port_delay() > 1)
        seq_comb_n = port.get_port_delay() >= 1
        if seq_comb_n:
            # Gen sequential read...
            sens_lst = [(kts.posedge, self._clk)]
            new_read = self.sequential(*sens_lst)
            new_rd_if = new_read.if_(pintf['read_enable'] == 1)
            new_rd_if.then_(pintf['data_out'].assign(self._data_array[pintf['read_addr']]))
        else:
            # Gen combinational read...
            new_read = self.combinational()
            new_read.add_stmt(pintf['data_out'].assign(self._data_array[pintf['read_addr']]))

    def realize_readwrite_port(self, port: MemoryPort):
        # In reality, a read/write port has to have a read delay,
        # otherwise it would be a separate port - so we only handle this case...
        pintf = port.get_port_interface()
        sens_lst = [(kts.posedge, self._clk)]
        new_write = self.sequential(*sens_lst)
        new_wr_if = new_write.if_(pintf['write_enable'] == 1)
        new_wr_if.then_(self._data_array[pintf['write_addr']].assign(pintf['data_in']))
        # If delay is 1 or more, do active read
        if port.get_port_delay() > 0:
            assert port.get_active_read() is True
            read_if = IfStmt(pintf['read_enable'])
            read_if.then_(pintf['data_out'].assign(self._data_array[pintf['read_addr']]))
            new_wr_if.else_(read_if)
        else:
            self.add_stmt(pintf['data_out'].assign(self._data_array[pintf['read_addr']]))

    def realize_write_port(self, port: MemoryPort):
        pintf = port.get_port_interface()
        sens_lst = [(kts.posedge, self._clk)]
        new_write = self.sequential(*sens_lst)
        new_wr_if = new_write.if_(pintf['write_enable'] == 1)
        new_wr_if.then_(self._data_array[pintf['write_addr']].assign(pintf['data_in']))

    def realize_read_port_phys(self, logical: MemoryPort, physical: PhysicalMemoryPort):
        l_pint = logical.get_port_interface()
        p_pint = physical.get_port_interface()
        # Just wire things through for this...
        self.wire(self._clk, p_pint['clk'])
        self.wire(l_pint['data_out'], p_pint['data_out'])
        self.wire(l_pint['read_addr'], p_pint['read_addr'])
        if physical.get_active_low():
            self.wire(~l_pint['read_enable'], p_pint['cen'])
        else:
            self.wire(l_pint['read_enable'], p_pint['cen'])

    def realize_readwrite_port_phys(self, logical: MemoryPort, physical: PhysicalMemoryPort):
        # In reality, a read/write port has to have a read delay,
        # otherwise it would be a separate port - so we only handle this case...
        l_pint = logical.get_port_interface()
        p_pint = physical.get_port_interface()
        # Just wire things through for this...
        self.wire(self._clk, p_pint['clk'])
        self.wire(l_pint['data_out'], p_pint['data_out'])
        self.wire(l_pint['data_in'], p_pint['data_in'])
        self.wire(kts.ternary(l_pint['write_enable'],
                              l_pint['write_addr'],
                              l_pint['read_addr']), p_pint['addr'])
        # Handle wen and cen
        if physical.get_active_low():
            self.wire(~l_pint['write_enable'], p_pint['write_enable'])
            self.wire(~(l_pint['write_enable'] | l_pint['read_enable']), p_pint['cen'])

        else:
            self.wire(l_pint['write_enable'], p_pint['write_enable'])
            self.wire((l_pint['write_enable'] | l_pint['read_enable']), p_pint['cen'])

    def realize_write_port_phys(self, logical: MemoryPort, physical: PhysicalMemoryPort):
        l_pint = logical.get_port_interface()
        p_pint = physical.get_port_interface()
        # Just wire things through for this...
        self.wire(self._clk, p_pint['clk'])
        self.wire(l_pint['data_in'], p_pint['data_in'])
        self.wire(l_pint['write_addr'], p_pint['write_addr'])
        if physical.get_active_low():
            self.wire(~l_pint['write_enable'], p_pint['write_enable'])
            self.wire(~l_pint['write_enable'], p_pint['cen'])
        else:
            self.wire(l_pint['write_enable'], p_pint['write_enable'])
            self.wire(l_pint['write_enable'], p_pint['cen'])

    def create_physical_memory(self):
        '''
        Creating the physical memory is really just passing the ports through
        to the physical macro provided from some memory compiler + adding in logic
        for active low signals + chip enable
        '''
        # The tech map will provide almost identical information
        assert self.tech_map_provided, f"Need to provide a tech map before creating physical memory"

        # Tech map basically provides a name of the module and a list of port maps

        # First create wrapper clock
        self._clk = self.clock("clk")

        # TODO - multiple clocks available sometimes

        # The port list
        port_maps = self.tech_map['ports']
        self.physical_ports = []

        # Create physical ports to map the logical ports into
        for (idx, port) in enumerate(self.get_ports()):
            new_phys_port = PhysicalMemoryPort(port.get_port_type(), delay=1, active_read=True,
                                               active_low=True, port_map=port_maps[idx])
            self.physical_ports.append(new_phys_port)

        # Create the stub generator
        child_stub = PhysicalMemoryStub(name=self.tech_map['name'],
                                        mem_params=self.mem_params,
                                        ports=self.physical_ports,
                                        tech_map=self.tech_map)
        self.add_child("mem_stub", child_stub)

        # Now wire the logical ports to the physical ports
        log_ports = self.get_ports()
        phy_ports = self.physical_ports

        for port_idx in range(len(log_ports)):
            log_port = log_ports[port_idx]
            phy_port = phy_ports[port_idx]
            port_type = log_port.get_port_type()
            assert port_type == phy_port.get_port_type()
            if port_type == MemoryPortType.READ:
                self.realize_read_port_phys(logical=log_port, physical=phy_port)
            elif port_type == MemoryPortType.READWRITE:
                self.realize_readwrite_port_phys(logical=log_port, physical=phy_port)
            elif port_type == MemoryPortType.WRITE:
                self.realize_write_port_phys(logical=log_port, physical=phy_port)

            # Now set the arbitrary test signals
            alt_sigs = phy_port.get_alt_signals()
            if alt_sigs is not None:
                for (alt_sig, (value, width)) in alt_sigs.items():
                    phy_intf = phy_port.get_port_interface()
                    self.wire(phy_intf[alt_sig], kts.const(value=value, width=width))

    def get_mem_width(self):
        return self.mem_width

    def get_mem_depth(self):
        return self.mem_depth
