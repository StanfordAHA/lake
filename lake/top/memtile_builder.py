from lake.top.memory_interface import *
from lake.top.memory_controller import *


class MemoryTileBuilder():
    '''
    This class provides utilities to add memories and memory controllers and automagically generate the hardware
    '''
    def __init__(self, memory_interface: MemoryInterface = None, memory_banks=1, controllers=[]):

        self.memory_interface = memory_interface
        self.memory_banks = memory_banks
        self.controllers = controllers

    def set_memory_interface(self, mem: MemoryInterface):
        self.memory_interface = mem

    def add_memory_controller(self, mem_ctrl: MemoryController):
        self.controllers += mem_ctrl
