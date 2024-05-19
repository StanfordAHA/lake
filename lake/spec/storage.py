from lake.spec.component import Component
from lake.utils.spec_enum import *
from kratos import always_ff, posedge, clog2


class Storage(Component):

    def __init__(self, capacity=1024):
        super().__init__()
        self._capacity = capacity
        self.memport_sets = {}

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()

    def get_capacity(self):
        return self._capacity

    def get_memport_intfs(self):
        return self.memport_sets


class SingleBankStorage(Storage):

    def __init__(self, capacity=1024):
        super().__init__(capacity=capacity)

    def _build_array(self):
        self._hw_array = self.var("data_array", self._capacity * 8)

    def _map_array_to_intf(self, tmp_intf, mp_width, num_addrs, rw=0):
        for i_ in range(num_addrs):
            for j_ in range(mp_width):
                if rw == 1:
                    self.wire(self._hw_array[i_ * mp_width + j_], tmp_intf[i_][j_])
                else:
                    self.wire(tmp_intf[i_][j_], self._hw_array[i_ * mp_width + j_])

    def gen_hardware(self, pos_reset=False, memory_ports=None):
        self._clk = self.clock("clk")
        assert memory_ports is not None
        mp_num = 0
        self._build_array()
        print("Mek")
        # Give each memory port an interface into the memory
        for mem_port in memory_ports:
            local_memport_set = {}
            # Get the memoryport information
            mp_width = mem_port.get_width()
            mp_type = mem_port.get_type()
            num_addrs = self._capacity // (mp_width // 8)
            addr_width = clog2(num_addrs)
            # mp_width = const(mp_width, addr_width)
            # tmp_intf = None
            if mp_type == MemoryPortType.R:
                # For read, we just need addr, ren, read data
                tmp_intf = self.var(f"mem_intf_r_{mp_num}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=0)
                addr = self.input(f"memory_port_{mp_num}_read_addr", addr_width)
                data = self.output(f"memory_port_{mp_num}_read_data", mp_width)
                en = self.input(f"memory_port_{mp_num}_read_en", 1)
                local_memport_set['type'] = MemoryPortType.R
                local_memport_set['addr'] = addr
                local_memport_set['read_data'] = data
                local_memport_set['read_en'] = en

                @always_ff((posedge, "clk"))
                def materialize_r(self):
                    if en:
                        data = tmp_intf[addr]
                self.add_code(materialize_r)

            elif mp_type == MemoryPortType.W:
                # For write, we just need addr, wen, write data
                tmp_intf = self.var(f"mem_intf_w_{mp_num}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=1)
                addr = self.input(f"memory_port_{mp_num}_write_addr", addr_width)
                data = self.input(f"memory_port_{mp_num}_write_data", mp_width)
                en = self.input(f"memory_port_{mp_num}_write_en", 1)
                local_memport_set['type'] = MemoryPortType.W
                local_memport_set['addr'] = addr
                local_memport_set['write_data'] = data
                local_memport_set['write_en'] = en

                @always_ff((posedge, "clk"))
                def materialize_w():
                    if en:
                        # self._hw_array[0, 0] = data[0, 0]
                        tmp_intf[addr] = data

                self.add_code(materialize_w)
            elif mp_type == MemoryPortType.RW:
                tmp_intf_r = self.var(f"mem_intf_r_{mp_num}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=0)
                tmp_intf_w = self.var(f"mem_intf_w_{mp_num}", mp_width, size=num_addrs, explicit_array=True)
                # Map the bits from the bare array to the data interface
                self._map_array_to_intf(tmp_intf=tmp_intf, mp_width=mp_width, num_addrs=num_addrs, rw=1)
                addr = self.input(f"memory_port_{mp_num}_read_addr", addr_width)
                wdata = self.input(f"memory_port_{mp_num}_write_data", mp_width)
                rdata = self.output(f"memory_port_{mp_num}_read_data", mp_width)
                wen = self.input(f"memory_port_{mp_num}_write_en", 1)
                ren = self.input(f"memory_port_{mp_num}_read_en", 1)
                local_memport_set['type'] = MemoryPortType.RW
                local_memport_set['addr'] = addr
                local_memport_set['read_data'] = rdata
                local_memport_set['write_data'] = wdata
                local_memport_set['read_en'] = ren
                local_memport_set['write_en'] = wen

                @always_ff((posedge, "clk"))
                def materialize_rw():
                    if wen:
                        tmp_intf_w[addr] = wdata
                    elif ren:
                        rdata = tmp_intf_r[addr]
                self.add_code(materialize_rw)
            # For read/write, we just need addr, wen, ren, write data, read data
                pass
            else:
                raise NotImplementedError

            self.memport_sets[mp_num] = local_memport_set

            print("s")

            mp_num += 1
        return self.memport_sets

    def gen_bitstream(self):
        return super().gen_bitstream()
