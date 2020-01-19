from kratos import *


class SW_NET(Generator):
    '''
    The switching network will bring in multiple items per cycle and direct
    them to the appropriate aggregation buffer
    '''
    def __init__(self,
                 interconnect_in,
                 offsets,  # Offset between the interconnect_in ports
                 data_width,
                 memory_width,  # Will be the size of the agg buffer
                 num_aggregators,
                 num_banks,
                 stride_0):
        super().__init__("sw_net")

        self.interconnect_in = interconnect_in
        self.offsets = offsets
        self.data_width = data_width
        self.memory_width = memory_width
        self.num_aggregators = num_aggregators
        self.num_banks = num_banks
        self.stride_0 = stride_0

        # PORT DEFS: begin
        self._clk = self.clock("clk")
        self._clk_en = self.input("clk_en", 1)
        self._reset = self.reset("reset")
        self._flush = self.input("flush", 1)

        self._data_in = self.input("data_in",
                                   data_width,
                                   size=interconnect_in,
                                   packed=True,
                                   explicit_array=True)  # Actually an array
        self._valid_in = self.input("valid_in", interconnect_in)

        self._data_out  # should be interconnect_in as well - basically a single stage input pipe
        self._agg_index_out  # should be the num_aggregators
        self._valid_out  # Should be the num of banks

        # Somehow get data to the output ->
        # probably with an address -> which will be 0-num_aggregators
        # Then tag it with valid or not
