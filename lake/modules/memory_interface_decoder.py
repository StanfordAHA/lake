import kratos as kts
from lake.utils.spec_enum import Direction
from lake.spec.memory_port import MemoryPort
from kratos import always_comb


class MemoryInterfaceDecoder(kts.Generator):

    # def __init__(self, name: str, debug: bool = False, is_clone: bool = False, internal_generator=None):
    def __init__(self, name: str = None, port_type: Direction = None, port_intf: dict = None, memports=None):
        super().__init__(name=name, debug=True)
        # super().__init__(name, debug, is_clone, internal_generator)
        self.p_intf = None
        self.mp_intf = {}
        self.memports = memports
        self.port_intf = port_intf
        self.port_direction = port_type
        self.addr_ranges = None

    def gen_hardware(self):

        self.p_intf = {}

        # Create the port facing side
        if self.port_direction == Direction.IN:
            self.p_intf['addr'] = self.input("addr", self.port_intf['addr'].width)
            self.p_intf['data'] = self.input("data", self.port_intf['data'].width)
            self.p_intf['en'] = self.input("en", 1)
        elif self.port_direction == Direction.OUT:
            self.p_intf['addr'] = self.input("addr", self.port_intf['addr'].width)
            self.p_intf['data'] = self.output("data", self.port_intf['data'].width)
            self.p_intf['en'] = self.input("en", 1)
        else:
            raise NotImplementedError

        self.addr_ranges = []
        base = 0
        # This allows us to figure out the address ranges that are associated with each enable
        for i_, mp in enumerate(self.memports):
            mp: MemoryPort
            addr_range = mp.get_num_addrs()
            self.addr_ranges.append((base, base + addr_range - 1))

            self.mp_intf[i_] = {}

            # Create the MemoryPort facing side
            self.mp_intf[i_]['addr'] = self.output(f"mp_addr_{i_}", kts.clog2(mp.get_num_addrs()))
            self.mp_intf[i_]['en'] = self.output(f"mp_en_{i_}", 1)
            if self.port_direction == Direction.IN:
                self.mp_intf[i_]['data'] = self.output(f"mp_data_{i_}", self.port_intf['data'].width)
            elif self.port_direction == Direction.OUT:
                self.mp_intf[i_]['data'] = self.input(f"mp_data_{i_}", self.port_intf['data'].width)
            else:
                raise NotImplementedError

            self.wire(self.p_intf['data'], self.mp_intf[i_]['data'])

            # Now decode
            @always_comb
            def decode_en():
                # The address to send to a specific memory port just needs the base subtracted away
                self.mp_intf[i_]['addr'] = self.p_intf['addr'] - base
                # The enable will be high if the address is in the range of the memport
                self.mp_intf[i_]['en'] = 0
                if (self.p_intf['addr'] >= base) and (self.p_intf['addr'] <= (base + addr_range - 1)):
                    # self.mp_intf[i_]['en'] = 1
                    self.mp_intf[i_]['en'] = self.p_intf['en']

            self.add_code(decode_en)
            base = base + addr_range

    def get_p_intf(self):
        return self.p_intf

    def get_mp_intf(self):
        return self.mp_intf
