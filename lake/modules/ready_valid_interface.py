import kratos as kts
from kratos import Generator
from lake.utils.spec_enum import Direction


class RVInterface():

    def __init__(self, generator: Generator, direction: Direction, **port_kwargs):

        self.port_interface = {
            'data': None,
            'valid': None,
            'ready': None
        }
        self.generator = generator
        self.direction = direction
        self.port_ready = None
        self.port_valid = None
        self.port = None
        self.signal_name = port_kwargs['name']
        if self.direction == Direction.IN:
            self.port = self.generator.input(**port_kwargs)
            self.port_valid = self.generator.input(f"{self.signal_name}_valid", 1)
            self.port_ready = self.generator.output(f"{self.signal_name}_ready", 1)
        elif self.direction == Direction.OUT:
            self.port = self.generator.output(**port_kwargs)
            self.port_valid = self.generator.output(f"{self.signal_name}_valid", 1)
            self.port_ready = self.generator.input(f"{self.signal_name}_ready", 1)
        else:
            raise NotImplementedError

        self.port_interface['data'] = self.port
        self.port_interface['valid'] = self.port_valid
        self.port_interface['ready'] = self.port_ready

    def get_port_interface(self):
        return self.port_interface

    def get_port(self):
        return self.port

    def get_valid(self):
        return self.port_valid

    def get_ready(self):
        return self.port_ready
