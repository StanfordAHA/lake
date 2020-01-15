from kratos import *
from lake.modules.aggregator import Aggregator
import kratos as kts

class AggregationBuffer(Generator):
    '''
    Aggregation buffer handles an item coming in and directs it to the proper row based on the scheduling
    '''
    def __init__(self,
               agg_height,
               data_width,
               mem_width,
               max_agg_schedule
              ):
        super().__init__("aggregation_buffer")

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
        self._write_act = self.input("write_act", 1)

        # Outputs
        self._data_out = self.output("data_out", self.data_width)
        self._valid_out = self.output("valid_out", 1)

        # CONFIG:
        # We receive a periodic (doesn't need to be, but has a maximum schedule, so...possibly the schedule is a for loop?
        # Tells us where to write successive elements...
        self._in_schedule = self.var("in_schedule", clog2(agg_height), size=self.max_agg_schedule)
        self._in_period = self.var("in_period", clog2(self.max_agg_schedule)) # CONFIG REG : lets us know when to start over
        # ...and which order to output the blocks
        self._out_schedule = self.var("out_schedule", clog2(agg_height), size=self.max_agg_schedule)
        self._out_period = self.var("out_period", clog2(self.max_agg_schedule)) # CONFIG REG : lets us know when to start over

        self._in_sched_ptr = self.var("in_sched_ptr", clog2(self.max_agg_schedule))
        self._out_sched_ptr = self.var("out_sched_ptr", clog2(self.max_agg_schedule))

        ## Local Signals
        self._aggs_out = self.var("aggs_out", self.mem_width, size=self.agg_height, packed=True, explicit_array=True)
        self._aggs_sep = []
        for i in range(self.agg_height):
            self._aggs_sep.append(self.var(f"aggs_sep_{i}", self.data_width, size=int(self.mem_width/self.data_width), packed=True))
        self._valid_demux = self.var("valid_demux", self.agg_height)
        self._valid_out_mux = self.var("valid_out_mux", self.agg_height)

        for i in range(self.agg_height):
            # Add in the children aggregators...
            self.add_child(f"agg_{i}", Aggregator(self.data_width, mem_word_width=int(self.mem_width/self.data_width)))
            # Wire it up
            self.wire(self[f"agg_{i}"].ports.clk, self._clk)
            self.wire(self[f"agg_{i}"].ports.rst_n, self._rst_n)
            self.wire(self[f"agg_{i}"].ports.in_pixels, self._data_in)
            self.wire(self[f"agg_{i}"].ports.valid_in, self._valid_demux[i])
            self.wire(self[f"agg_{i}"].ports.agg_out, self._aggs_sep[i])
            self.wire(self[f"agg_{i}"].ports.valid_out, self._valid_out_mux[i])
            self.wire(self._aggs_out[i], kts.concat(*self._aggs_sep[i]))


        

        # Sequential code blocks
        self.add_code(self.update_in_sched_ptr)
        self.add_code(self.update_out_sched_ptr)

        # Combinational code blocks
        self.add_code(self.valid_demux_comb)
        self.add_code(self.valid_out_comb)
        self.add_code(self.output_data_comb)

        # Update the pointer and mux for input and output schedule
        # Now, obey the input schedule to send to the proper Aggregator
        @always((posedge, "clk"), (negedge, "rst_n"))
        def update_in_sched_ptr(self):
            if ~self._rst_n:
                self._in_sched_ptr = 0
            elif self._valid_in:
                self._in_sched_ptr = \
                    ternary(self._in_sched_ptr == self._in_period, 0, self._in_sched_ptr + 1)

        @always((posedge, "clk"), (negedge, "rst_n"))
        def update_out_sched_ptr(self):
            if ~self._rst_n:
                self._out_sched_ptr = 0
            elif self._write_act:
                self._out_sched_ptr = \
                    ternary(self._out_sched_ptr == self._out_period, 0, self._out_sched_ptr + 1)

        def valid_demux_comb(self):
            self._valid_demux = 0
            self._valid_demux[self._in_schedule[self._in_sched_ptr]] = 1

        def valid_out_comb(self):
            self._valid_out = self._valid_out_mux[self._out_schedule[self._out_sched_ptr]]

        def output_data_comb(self):
            self._data_out = self._aggs_out[self._out_schedule[self._out_sched_ptr]]
        # Then, obey the output schedule to send the proper Aggregator to the output

if __name__ == "__main__":
    db_dut = AggregationBuffer(agg_height=4,
               data_width=16,
               mem_width=64,
               max_agg_schedule=64)
    verilog(db_dut, filename="aggregation_buffer.sv", check_active_high=False, check_multiple_driver=False)



