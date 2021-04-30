import kratos as kts
from enum import Enum


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

    def __init__(self, name: str, mem_params: dict, ports: list = [], sim_macro_n: bool = True, tech_map=None):
        super().__init__(name, debug=True)

        self.tech_map_provided = False
        self.tech_map = tech_map
        self.sim_macro_n = sim_macro_n
        self.mem_params = mem_params
        self.mem_ports = ports

        if self.tech_map is not None:
            self.tech_map_provided = True

        self.mem_width = MemoryInterface.mem_width_dflt if 'mem_width' not in mem_params else mem_params['mem_width']
        self.mem_depth = MemoryInterface.mem_depth_dflt if 'mem_depth' not in mem_params else mem_params['mem_depth']

        self.create_interface()
        self.realize_hw()

    def create_interface(self):
        # Loop through the memory ports and create the proper signals - only need a few signals
        for pnum, port in enumerate(self.mem_ports):
            port_type = port.get_port_type()
            port_intf = port.get_port_interface()
            if port_type == MemoryPortType.READ:
                # Read port has data out, address, and ren
                port_intf['data_out'] = self.output(f"data_out_p{pnum}", self.mem_width)
                port_intf['read_addr'] = self.input(f"read_addr_p{pnum}", kts.clog2(self.mem_width))
                port_intf['read_enable'] = self.input(f"read_enable_p{pnum}", 1)
            elif port_type == MemoryPortType.WRITE:
                port_intf['data_in'] = self.output(f"data_in_p{pnum}", self.mem_width)
                port_intf['write_addr'] = self.input(f"write_addr_p{pnum}", kts.clog2(self.mem_width))
                port_intf['write_enable'] = self.input(f"write_enable_p{pnum}", 1)
            elif port_type == MemoryPortType.READWRITE:
                port_intf['data_out'] = self.output(f"data_out_p{pnum}", self.mem_width)
                port_intf['read_addr'] = self.input(f"read_addr_p{pnum}", kts.clog2(self.mem_width))
                port_intf['read_enable'] = self.input(f"read_enable_p{pnum}", 1)
                port_intf['data_in'] = self.output(f"data_in_p{pnum}", self.mem_width)
                port_intf['write_addr'] = self.input(f"write_addr_p{pnum}", kts.clog2(self.mem_width))
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

    def get_ports(self):
        return self.mem_ports

    def get_num_ports(self):
        return len(self.mem_ports)

    def create_simulatable_memory(self):
        pass

    def create_physical_memory(self):
        pass

    def get_mem_width(self):
        return self.mem_width

    def get_mem_depth(self):
        return self.mem_depth
