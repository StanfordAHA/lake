from kratos import *
from lake.modules.aggregator import Aggregator

class InputAddrCtrl(Generator):
    '''
    Input addressing control from the aggregation buffers to the SRAM
    '''
    def __init__(self,
               interconnect_input_ports,
               data_width,
               mem_width,
               max_agg_schedule
              ):
        super.__init__("aggregation_buffer")

        self.agg_height = agg_height
        self.data_width = data_width
        self.mem_width = mem_width
        self.max_agg_schedule = max_agg_schedule # This is the maximum length of the schedule

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        
        # Inputs
        # Bring in a single element into an AggregationBuffer w/ valid signaling
        self._data_in = self.input("data_in", self.data_width)
        self._valid_in = self.input("valid_in", 1)

        # Outputs
        self._data_out = self.output("data_out", self.data_width)
        self._valid_out = self.output("valid_out", 1)

        # CONFIG:
        # We receive a periodic (doesn't need to be, but has a maximum() schedule, so...possibly the schedule is a for loop?
        # Tells us where to write successive elements...
        self._in_schedule = self.var("in_schedule", clog2(agg_height), size=self.max_agg_schedule)
        self._in_period = self.var("in_period", clog2(self.max_agg_schedule)) # CONFIG REG : lets us know when to start over
        # ...and which order to output the blocks
        self._out_schedule = self.var("out_schedule", clog2(agg_height), size=self.max_agg_schedule)
        self._out_period = self.var("out_period", clog2(self.max_agg_schedule)) # CONFIG REG : lets us know when to start over

        self._in_ptr = self.var("in_sched_ptr", clog2(self.max_agg_schedule))
        self._out_ptr = self.var("out_sched_ptr", clog2(self.max_agg_schedule))

        for i in range(self.agg_height):
            # Add in the children aggregators...
            self.add_child(f"agg_{i}", Aggregator(self.data_width, self.mem_width))
            # Wire it up



        # Update the pointer and mux for input and output schedule
        # Now, obey the input schedule to send to the proper Aggregator
        @always((posedge, "clk"), (negedge, "rst_n"))
        def update_counter_valid(self):
            return

        # Then, obey the output schedule to send the proper Aggregator to the output




