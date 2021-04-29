from lake.top.memory_interface import *
from lake.top.memory_controller import *

class MemoryTileBuilder():
    '''
    This class provides utilities to add memories and memory controllers and automagically generate the hardware
    '''
    def __init__(self) -> None:

        self.memory_interface

    def add_memory_interface(self, mem: MemoryInterface):
        pass

    def add_memory_controller(self, mem_ctrl: MemoryController)
