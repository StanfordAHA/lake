from lake.spec.component import Component
from lake.utils.spec_enum import *
from lake.spec.memory_port import MemoryPort
import kratos as kts
from kratos import always_ff, always_comb, posedge, negedge
from lake.spec.iteration_domain import IterationDomain


class AddressGenerator(Component):

    def __init__(self, dimensionality=6, recurrence=True):
        super().__init__()
        self.dimensionality_support = dimensionality
        self.total_num_addrs = None
        self.addr_width = None
        self.exploit_recurrence = recurrence
        self.width_mult = 1
        #  We want to handle flush and clk_en ourselves
        self.sync_reset_no_touch = True
        self.clk_en_no_touch = True

    def set_width_mult(self, width_mult):
        self.width_mult = width_mult

    def gen_hardware(self, memports=None, id: IterationDomain = None, pos_reset=False):
        assert memports is not None
        assert id is not None
        id_ext_width = id.get_extent_width()

        # Strides and addr width will be clog2 of the stacked address space
        self.total_num_addrs = 0
        for memport in memports:
            memport: MemoryPort
            self.total_num_addrs += memport.get_num_addrs()

        self.addr_width = kts.clog2(self.total_num_addrs) + kts.clog2(self.width_mult)
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
        # self._clk = self.clock("clk")
        # self._rst_n = self.reset("rst_n")
        # self._flush = self.input("flush", 1)
        # self.add_attribute("sync-reset=flush")
        self._step = self.input("step", 1)
        self._mux_sel = self.input("mux_sel", max(kts.clog2(self.dimensionality_support), 1))
        self._restart = self.input("restart", 1)
        self._finished = self.input("finished", 1)
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

        if self.exploit_recurrence:
            self.add_code(self.calculate_address_delta)
        else:
            self._tmp_addr_calc = []
            for i in range(self.dimensionality_support):
                tmp = self.var(f"tmp_addr_calc_{i}", self._ctrs[i].width + self._strides[i].width)
                self.wire(tmp, kts.ext(self._ctrs[i] * self._strides[i], tmp.width))
                self._tmp_addr_calc.append(tmp)

            self.add_code(self.calculate_address_count)

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    @always_comb
    def calculate_address_count(self):
        self._current_addr = self._strt_addr
        for i in range(self.dimensionality_support):
            self._current_addr += (self._tmp_addr_calc[i])[self.addr_width - 1, 0]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def calculate_address_delta(self):
        if ~self._rst_n:
            self._current_addr = 0
        elif self._flush:
            self._current_addr = self._strt_addr
        elif self._clk_en:
            if self._step & ~self._finished:
                if self._restart:
                    self._current_addr = self._strt_addr
                else:
                    self._current_addr = self._current_addr + self._strides[self._mux_sel]

    def gen_bitstream(self, address_map, extents, dimensionality):
        assert 'strides' in address_map
        assert 'offset' in address_map

        self.clear_configuration()

        self.configure(self._starting_addr, address_map['offset'])
        if self.exploit_recurrence:
            extent_sub_1 = [extent_item - 1 for extent_item in extents]
            tform_strides = [address_map['strides'][0]]
            offset = 0
            for i in range(dimensionality - 1):
                offset -= (extent_sub_1[i] * address_map['strides'][i])
                tform_strides.append(address_map['strides'][i + 1] + offset)

            self.configure(self._strides, tform_strides)
        else:
            self.configure(self._strides, address_map['strides'])
        # This will return pairs of ranges with values w.r.t. the node's configuration
        return self.get_configuration()

    def get_address_width(self):
        return self.addr_width

    def get_address(self):
        return self._addr_out

    def get_memory_address(self):
        if self.width_mult == 1:
            return self._addr_out
        else:
            trim_bits = kts.clog2(self.width_mult)
            return self._addr_out[self._addr_out.width - 1, trim_bits]


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
