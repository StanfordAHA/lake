from abc import ABC
from lake.asplos.port_type import PortType


class Port(ABC):

    def __init__(self,
                 data_width=16,
                 port_type=None,
                 port_direction=None,
                 addr_gen=None,
                 enable=None) -> None:
        super().__init__()
