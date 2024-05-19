from lake.spec.component import Component
from lake.utils.spec_enum import *
from kratos import clog2


class MemoryPort(Component):

    def __init__(self, data_width=16, mptype=MemoryPortType.R, delay=0):
        super().__init__()
        self.width = data_width
        self.mptype = mptype
        self._port_intf = {}
        self._strg_intf = {}
        self.num_addrs = None
        self.addr_width = None
        self.hw_genned = False

    def get_num_addrs(self):
        return self.num_addrs

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
