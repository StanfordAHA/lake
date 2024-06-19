from lake.spec.component import Component
from lake.utils.spec_enum import *
from kratos import clog2
import kratos as kts


class MemoryPortExclusionAttr(kts.Attribute):
    def __init__(self):
        super().__init__()


class MemoryPort(Component):

    def __init__(self, data_width=16, mptype=MemoryPortType.R, delay=0, active_read=True):
        super().__init__()
        self.width = data_width
        self.mptype = mptype
        self._port_intf = {}
        self._strg_intf = {}
        self.num_addrs = None
        self.addr_width = None
        self.hw_genned = False
        # This is this ^
        # self.port_interface_set = False

        self.delay = delay
        self.active_read = active_read

        self.port_interface = {}
        self.port_interface_set = False

    def __str__(self):
        conn_str = ""
        for (name, conn) in self._port_intf.items():
            if conn is None:
                conn_str += f"conn: {name}, DISCONNECTED\n"
            else:
                conn_str += f"conn: {name}, {conn.name}\n"
        return f"Port: Type: {self.get_type()}, conns:\n{conn_str}"

    def set_num_addrs(self, num_addrs):
        self.num_addrs = num_addrs

    def get_num_addrs(self):
        return self.num_addrs

    def get_port_interface(self):
        if self.port_interface_set is False:
            self.create_port_interface()
        return self.port_interface

    def gen_hardware(self, pos_reset=False, storage_node=None):

        assert storage_node is not None
        # Extract info from storage node
        self.num_addrs = storage_node.get_capacity() // (self.width // 8)
        self.addr_width = clog2(self.num_addrs)

        if self.mptype == MemoryPortType.R:
            # in set
            addr_from_port = self.input(f"memory_port_read_addr_in", self.addr_width)
            data_to_port = self.output(f"memory_port_read_data_out", self.width)
            en_from_port = self.input(f"memory_port_read_en_in", 1)

            # out set
            addr_to_strg = self.output(f"memory_port_read_addr_out", self.addr_width)
            data_from_strg = self.input(f"memory_port_read_data_in", self.width)
            en_to_strg = self.output(f"memory_port_read_en_out", 1)

            # wire together
            self.wire(addr_from_port, addr_to_strg)
            self.wire(data_to_port, data_from_strg)
            self.wire(en_from_port, en_to_strg)

            self._port_intf['addr'] = addr_from_port
            self._port_intf['read_data'] = data_to_port
            self._port_intf['read_en'] = en_from_port

            self._strg_intf['addr'] = addr_to_strg
            self._strg_intf['read_data'] = data_from_strg
            self._strg_intf['read_en'] = en_to_strg

        elif self.mptype == MemoryPortType.W:

            print('mek')

            # in set
            addr_from_port = self.input(f"memory_port_write_addr_in", self.addr_width)
            data_from_port = self.input(f"memory_port_write_data_in", self.width)
            en_from_port = self.input(f"memory_port_write_en_in", 1)

            # out set
            addr_to_strg = self.output(f"memory_port_write_addr_out", self.addr_width)
            data_to_strg = self.output(f"memory_port_write_data_out", self.width)
            en_to_strg = self.output(f"memory_port_write_en_out", 1)

            # wire together
            self.wire(addr_from_port, addr_to_strg)
            self.wire(data_from_port, data_to_strg)
            self.wire(en_from_port, en_to_strg)

            self._port_intf['addr'] = addr_from_port
            self._port_intf['write_data'] = data_from_port
            self._port_intf['write_en'] = en_from_port

            self._strg_intf['addr'] = addr_to_strg
            self._strg_intf['write_data'] = data_to_strg
            self._strg_intf['write_en'] = en_to_strg

        elif self.mptype == MemoryPortType.RW:

            # in set
            addr_from_port = self.input(f"memory_port_write_addr_in", self.addr_width)
            data_from_port_write = self.input(f"memory_port_write_data_in", self.width)
            data_to_port_read = self.output(f"memory_port_read_data_out", self.width)
            wen_from_port = self.input(f"memory_port_write_en_in", 1)
            ren_from_port = self.input(f"memory_port_read_en_in", 1)

            # out set
            addr_to_strg = self.output(f"memory_port_write_addr_out", self.addr_width)
            data_to_strg_write = self.output(f"memory_port_write_data_out", self.width)
            data_from_strg_read = self.input(f"memory_port_read_data_in", self.width)
            wen_to_strg = self.output(f"memory_port_write_en_out", 1)
            ren_to_strg = self.output(f"memory_port_read_en_out", 1)

            # wire together
            self.wire(addr_from_port, addr_to_strg)
            self.wire(data_from_port_write, data_to_strg_write)
            self.wire(data_to_port_read, data_from_strg_read)
            self.wire(wen_from_port, wen_to_strg)
            self.wire(ren_from_port, ren_from_port)

            self._port_intf['addr'] = addr_from_port
            self._port_intf['write_data'] = data_from_port_write
            self._port_intf['read_data'] = data_to_port_read
            self._port_intf['write_en'] = wen_from_port
            self._port_intf['read_en'] = ren_from_port

            self._strg_intf['addr'] = addr_to_strg
            self._strg_intf['write_data'] = data_to_strg_write
            self._strg_intf['read_data'] = data_from_strg_read
            self._strg_intf['write_en'] = wen_to_strg
            self._strg_intf['read_en'] = ren_to_strg

        self.hw_genned = True

    def gen_bitstream(self):
        return super().gen_bitstream()

    def get_width(self):
        return self.width

    def get_type(self):
        return self.mptype

    def get_port_intf(self):
        return self._port_intf

    def get_storage_intf(self):
        return self._strg_intf

    def get_hw_genned(self):
        return self.hw_genned

    def get_port_delay(self):
        return self.delay

    def get_active_read(self):
        if self.mpt == MemoryPortType.WRITE:
            return False
        else:
            return self.active_read


class ReadMemoryPort(MemoryPort):

    def __init__(self, data_width=16, mptype=MemoryPortType.R, delay=0):
        super().__init__(data_width=data_width, mptype=mptype, delay=delay)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()


class WriteMemoryPort(MemoryPort):

    def __init__(self, data_width=16, mptype=MemoryPortType.W, delay=0):
        super().__init__(data_width=data_width, mptype=mptype, delay=delay)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()


class ReadWriteMemoryPort(MemoryPort):

    def __init__(self, data_width=16, mptype=MemoryPortType.RW, delay=0):
        super().__init__(data_width=data_width, mptype=mptype, delay=delay)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()


class PhysicalMemoryPort(MemoryPort):
    def __init__(self, mptype: MemoryPortType, delay, active_read, active='low', port_map=None):
        super().__init__(mptype=mptype, delay=delay, active_read=active_read)
        # Capture alternate inputs for testing commonly used in these memories.
        self.alt_sigs = None
        if 'alt_sigs' in port_map:
            self.alt_sigs = port_map['alt_sigs']
        self.active_low = active == 'low'
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

        print("Printing self port map")
        print(self.port_map)

        if self.mptype == MemoryPortType.R:
            self.port_interface['read_data'] = self.port_map['read_data']
            # self.port_interface['read_addr'] = self.port_map['read_addr']
            self.port_interface['addr'] = self.port_map['addr']
            self.port_interface['cen'] = self.port_map['cen']
            self.port_interface['clk'] = self.port_map['clk']
            self.port_interface_set = True
        elif self.mptype == MemoryPortType.W:
            self.port_interface['write_data'] = self.port_map['write_data']
            self.port_interface['addr'] = self.port_map['addr']
            self.port_interface['write_en'] = self.port_map['write_en']
            self.port_interface['cen'] = self.port_map['cen']
            self.port_interface['clk'] = self.port_map['clk']
            self.port_interface_set = True
        elif self.mptype == MemoryPortType.RW:
            self.port_interface['write_data'] = self.port_map['write_data']
            self.port_interface['read_data'] = self.port_map['read_data']
            self.port_interface['write_en'] = self.port_map['write_en']
            self.port_interface['addr'] = self.port_map['addr']

            # Check for read_enable as well
            if 'read_enable' in self.port_map:
                self.port_interface['read_enable'] = self.port_map['read_enable']
            else:
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
