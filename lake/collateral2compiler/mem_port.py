from kratos import *
from lake.modules.for_loop import ForLoop


class MemPort():
    def __init__(self,
                 latency,
                 II):

        self.port_info = {"latency": latency, "initiation_interval": II}
