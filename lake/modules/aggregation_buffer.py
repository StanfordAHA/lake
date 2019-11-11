from kratos import *

class Aggregation_Buffer(Generator):
    '''
    Aggregation buffer handles accepting up to interconnect_in sets of data in a cycle and 
    sending them linearly into the aggregators.
    '''
    def __init__(self,
               num_aggregators,
               interconnect_in
              ):
        super.__init__("aggregation_buffer")

