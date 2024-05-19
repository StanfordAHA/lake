from lake.spec.component import Component
from lake.utils.spec_enum import *
import random as rand


def print_class_hierarchy(obj):
    print("Class Hierarchy:")
    for cls in obj.__class__.__mro__:
        print(cls.__name__)


class Port(Component):

    def __init__(self, data_width=16, runtime=Runtime.STATIC, direction=Direction.IN):
        super().__init__()
        self._mp_intf = {}
        self._ub_intf = {}
        self._data_width = data_width
        self._runtime = runtime
        self._direction = direction

    def gen_hardware(self, pos_reset=False):
        if self._direction == Direction.IN:
            # in set
            data_from_ub = self.input(f"port_write_data_in", self._data_width)
            data_to_memport = self.output(f"port_write_data_out", self._data_width)

            # wire together
            self.wire(data_from_ub, data_to_memport)

            self._ub_intf['data'] = data_from_ub
            self._mp_intf['data'] = data_to_memport

        elif self._direction == Direction.OUT:
            # in set
            data_from_memport = self.input(f"port_read_data_in", self._data_width)
            data_to_ub = self.output(f"port_read_data_out", self._data_width)

            # wire together
            self.wire(data_from_memport, data_to_ub)
            self._ub_intf['data'] = data_to_ub

            self._mp_intf['data'] = data_from_memport

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
