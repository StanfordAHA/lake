import kratos as kts
from enum import Enum
from kratos.stmts import *


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
        self.create_port_interface()

    def create_port_interface(self):
        if self.mpt == MemoryPortType.READ:
            self.port_interface['data_out'] = None
            self.port_interface['read_addr'] = None
            self.port_interface['read_enable'] = None
        elif self.mpt == MemoryPortType.WRITE:
            self.port_interface['data_in'] = None
            self.port_interface['write_addr'] = None
            self.port_interface['write_enable'] = None
        elif self.mpt == MemoryPortType.READWRITE:
            self.port_interface['data_in'] = None
            self.port_interface['data_out'] = None
            self.port_interface['write_addr'] = None
            self.port_interface['write_enable'] = None
            self.port_interface['read_addr'] = None
            self.port_interface['read_enable'] = None
        else:
            assert False, "Memory port has illegal interface"

    def get_port_interface(self):
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

    def __str__(self):
        conn_str = ""
        for (name, conn) in self.port_interface.items():
            conn_str += f"conn: {name}, {conn.name}\n"
        return f"Port: Type: {self.get_port_type()}, conns:\n{conn_str}"


class MemoryInterface(kts.Generator):
    '''
    This class provides the generator interface for all storage
    '''

    mem_width_dflt = 32
    mem_depth_dflt = 256

    def __init__(self, name: str, mem_params: dict, ports: list = [], sim_macro_n: bool = True, tech_map=None, reset_in_sim=False):
        super().__init__(name, debug=True)

        self.tech_map_provided = False
        self.tech_map = tech_map
        self.sim_macro_n = sim_macro_n
        self.mem_params = mem_params
        self.mem_ports = ports
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
                port_intf['read_addr'] = self.input(f"read_addr_p{pnum}", kts.clog2(self.mem_depth))
                port_intf['read_enable'] = self.input(f"read_enable_p{pnum}", 1)
            elif port_type == MemoryPortType.WRITE:
                port_intf['data_in'] = self.input(f"data_in_p{pnum}", self.mem_width, packed=True)
                port_intf['write_addr'] = self.input(f"write_addr_p{pnum}", kts.clog2(self.mem_width))
                port_intf['write_enable'] = self.input(f"write_enable_p{pnum}", 1)
            elif port_type == MemoryPortType.READWRITE:
                port_intf['data_out'] = self.output(f"data_out_p{pnum}", self.mem_width, packed=True)
                port_intf['read_addr'] = self.input(f"read_addr_p{pnum}", kts.clog2(self.mem_depth))
                port_intf['read_enable'] = self.input(f"read_enable_p{pnum}", 1)
                port_intf['data_in'] = self.input(f"data_in_p{pnum}", self.mem_width, packed=True)
                port_intf['write_addr'] = self.input(f"write_addr_p{pnum}", kts.clog2(self.mem_depth))
                port_intf['write_enable'] = self.input(f"write_enable_p{pnum}", 1)

    def set_tech_map(self, tech_map):
        assert tech_map is not None, f"Need to provide valid tech map"
        self.tech_map = tech_map

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
        seq_comb_n = port.get_active_read()
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
        read_if = IfStmt(pintf['read_enable'])
        read_if.then_(pintf['data_out'].assign(self._data_array[pintf['read_addr']]))
        new_wr_if.else_(read_if)

    def realize_write_port(self, port: MemoryPort):
        pintf = port.get_port_interface()
        sens_lst = [(kts.posedge, self._clk)]
        new_write = self.sequential(*sens_lst)
        new_wr_if = new_write.if_(pintf['write_enable'] == 1)
        new_wr_if.then_(self._data_array[pintf['write_addr']].assign(pintf['data_in']))

    def create_physical_memory(self):
        '''
        Creating the physical memory is really just passing the ports through
        to the physical macro provided from some memory compiler + adding in logic
        for active low signals + chip enable
        '''
        # TODO
        pass

    def get_mem_width(self):
        return self.mem_width

    def get_mem_depth(self):
        return self.mem_depth
