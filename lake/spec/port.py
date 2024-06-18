from lake.spec.component import Component
from lake.utils.spec_enum import *
import random as rand
from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ScheduleGenerator
from lake.spec.storage import SingleBankStorage, Storage


def print_class_hierarchy(obj):
    print("Class Hierarchy:")
    for cls in obj.__class__.__mro__:
        print(cls.__name__)


class Port(Component):

    def __init__(self, ext_data_width=16, int_data_width=16,
                 runtime=Runtime.STATIC, direction=Direction.IN,
                 vec_capacity=None):
        super().__init__()
        self._mp_intf = {}
        self._ub_intf = {}
        self._ext_data_width = ext_data_width
        self._int_data_width = int_data_width
        self._runtime = runtime
        self._direction = direction
        self._fw = self._int_data_width // self._ext_data_width
        if self._fw != 1:
            assert self._vec_capacity is not None
        self._vec_capacity = vec_capacity

    def gen_hardware(self, pos_reset=False):
        if self._direction == Direction.IN:

            # This is the hardware for a single fetch width
            # in set
            data_from_ub = self.input(f"port_write_data_in", self._ext_data_width)
            data_to_memport = self.output(f"port_write_data_out", self._int_data_width)
            self._ub_intf['data'] = data_from_ub
            self._mp_intf['data'] = data_to_memport

            if self._fw == 1:
                # wire together
                self.wire(data_from_ub, data_to_memport)

            else:
                # For the write port with a wide fetch, we need 
                # storage and memory ports for SIPO, ID, AG, SG (original size + 1) on the front
                # Capacity of the storage will be w.r.t. external data width
                self._sipo_strg = SingleBankStorage(capacity=self._vec_capacity)
                self,_id_ext = IterationDomain(dimensionality=6)
                # 2xAG between for read of SIPO and Write of SRAM, ID, SG of original size

        elif self._direction == Direction.OUT:

            # in set
            data_from_memport = self.input(f"port_read_data_in", self._int_data_width)
            data_to_ub = self.output(f"port_read_data_out", self._ext_data_width)
            self._ub_intf['data'] = data_to_ub
            self._mp_intf['data'] = data_from_memport

            if self._fw == 1:
                # wire together
                self.wire(data_from_memport, data_to_ub)
            else:
                raise NotImplementedError

        else:
            raise NotImplementedError

    def gen_bitstream(self):
        return super().gen_bitstream()

    def get_direction(self):
        return self._direction

    def get_runtime(self):
        return self._runtime

    def get_mp_intf(self):
        return self._mp_intf

    def get_ub_intf(self):
        return self._ub_intf


class ReadPort(Port):

    def __init__(self, data_width=16, runtime=Runtime.STATIC):
        super().__init__(data_width=data_width, runtime=runtime)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()


class WritePort(Port):

    def __init__(self, data_width=16, direction=Runtime.STATIC):
        super().__init__(data_width=data_width, direction=direction)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()
