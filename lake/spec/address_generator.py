from lake.spec.component import Component
from lake.utils.spec_enum import *
from lake.spec.memory_port import MemoryPort
import kratos as kts
from kratos import always_ff, always_comb, posedge, negedge
from lake.spec.iteration_domain import IterationDomain


class AddressGenerator(Component):

    def __init__(self, dimensionality=6):
        self.dimensionality_support = dimensionality
        self.total_num_addrs = None
        self.addr_width = None

    def gen_hardware(self, memports=None, id: IterationDomain = None, pos_reset=False):
        assert memports is not None
        assert id is not None
        print(id.get_name())
        id_ext_width = id.get_extent_width()
        for memport in memports:
            print(memport.get_name())
        # Strides and addr width will be clog2 of the stacked address space
        self.total_num_addrs = 0
        for memport in memports:
            memport: MemoryPort
            print(memport)
            print(memport.get_hw_genned())
            self.total_num_addrs += memport.get_num_addrs()

        self.addr_width = kts.clog2(self.total_num_addrs)
        module_name = f"addr_gen_{self.dimensionality_support}_{self.addr_width}"
        super().__init__(name=module_name)
        ##########
        ### IO ###
        ##########

        ### Config Regs
        self._strides = self.config_reg(name="strides", width=self.addr_width,
                                        size=self.dimensionality_support,
                                        packed=True, explicit_array=True)

        self._starting_addr = self.config_reg(name="starting_addr", width=self.addr_width)
        ### Inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._flush = self.input("flush", 1)
        self.add_attribute("sync-reset=flush")
        self._step = self.input("step", 1)
        self._mux_sel = self.input("mux_sel", max(kts.clog2(self.dimensionality_support), 1))
        # Use signals directly for now
        self._ctrs = self.input("iterators", id_ext_width,
                                size=self.dimensionality_support,
                                packed=True, explicit_array=True)

        ### Outputs
        self._addr_out = self.output("addr_out", self.addr_width)

        ### Local variables
        self._strt_addr = self.var("strt_addr", self.addr_width)
        self._current_addr = self.var("current_addr", self.addr_width)

        ### Logic
        self.wire(self._strt_addr, self._starting_addr)
        self.wire(self._addr_out, self._current_addr)

        self.add_code(self.calculate_address_count)
        # self.add_code(self.calculate_address_delta)

    @always_comb
    def calculate_address_count(self):
        self._current_addr = self._strt_addr
        for i in range(self.dimensionality_support):
            # self._current_addr = self._current_addr + self._ctrs[i] * self._strides[i]
            self._current_addr = self._current_addr + (self._ctrs[i] * self._strides[i])[self.addr_width - 1, 0]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def calculate_address_delta(self):
        if ~self._rst_n:
            self._current_addr = self._strt_addr
        elif self._flush:
            self._current_addr = self._strt_addr
        elif self._step:
            self._current_addr = self._current_addr + self._strides[self._mux_sel]

    def gen_bitstream(self):
        return super().gen_bitstream()

    def get_address(self):
        return self._current_addr


class ExplicitAddressGenerator(AddressGenerator):

    def __init__(self, dimensionality=16):
        super().__init__(dimensionality=dimensionality)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()


class RecurrentAddressGenerator(AddressGenerator):

    def __init__(self, dimensionality=16):
        super().__init__(dimensionality=dimensionality)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()
