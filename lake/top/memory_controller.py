from abc import ABC, abstractmethod


class MemoryController(ABC):
    '''
    Provides the utilities to interface a memory controller with a memory interface
    '''
    @abstractmethod
    def get_inputs(self):
        '''
        Use this method to return inputs that go to the tile interface
        '''
        pass

    @abstractmethod
    def get_outputs(self):
        '''
        Use this method to return outputs that go to the tile interface
        '''
        pass

    @abstractmethod
    def get_num_banks_supported(self):
        '''
        Use this method to indicate how many banks the memory controller needs
        '''
        pass

    @abstractmethod
    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        pass

    @abstractmethod
    def __str__(self):
        pass
