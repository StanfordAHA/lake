from kratos import *
from enum import IntEnum, auto


class PortType(IntEnum):
    READ = auto()
    WRITE = auto()
    READWRITE = auto()


class MemPort():
    def __init__(self,
                 latency,
                 II,
                 addr_domain=[-1, -1]):

        self.port_info = {"latency": latency,
                          "initiation_interval": II,
                          "addr_domain": {"min": addr_domain[0], "max": addr_domain[1]}}

    def set_addr_domain(self, addr_domain):
        self.port_info["addr_domain"] = {"min": addr_domain[0], "max": addr_domain[1]}
