from abc import ABC, abstractmethod
import kratos as kts


class MemoryController(kts.Generator):
    '''
    Provides the utilities to interface a memory controller with a memory interface
    '''
    def get_inputs(self):
        '''
        Use this method to return inputs that go to the tile interface
        '''
        raise NotImplementedError

    def get_outputs(self):
        '''
        Use this method to return outputs that go to the tile interface
        '''
        raise NotImplementedError

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        raise NotImplementedError

    def __str__(self):
        return self.name
