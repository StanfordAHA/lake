from kratos import *
from lake.modules.aggregator import Aggregator
import kratos as kts
from lake.attributes.config_reg_attr import ConfigRegAttr


class AggregationBuffer(Generator):
    '''
    Aggregation buffer handles an item coming in and
    directs it to the proper row based on the scheduling
    '''
    def __init__(self,
                 agg_height,
                 data_width,
                 mem_width,
                 max_agg_schedule):

        super().__init__("aggregation_buffer")

        self.agg_height = agg_height
        self.data_width = data_width
        self.mem_width = mem_width
        self.max_agg_schedule = max_agg_schedule  # This is the maximum length of the schedule
        self.fw_int = int(self.mem_width / self.data_width)

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        # Bring in a single element into an AggregationBuffer w/ valid signaling
        self._data_in = self.input("data_in", self.data_width)
        self._valid_in = self.input("valid_in", 1)
        self._align = self.input("align", 1)
        # Outputs
        self._data_out = self.output("data_out", self.mem_width)
        self._valid_out = self.output("valid_out", 1)

        self._data_out_chop = []
        for i in range(self.fw_int):
            self._data_out_chop.append(self.output(f"data_out_chop_{i}", self.data_width))
            self.add_stmt(
                self._data_out_chop[i].assign(self._data_out[(self.data_width * (i + 1)) - 1,
                                                             self.data_width * i]))

        # CONFIG:
        # We receive a periodic (doesn't need to be, but has a maximum schedule,
        # so...possibly the schedule is a for loop?
        # Tells us where to write successive elements...
        self._in_schedule = self.input("in_sched",
                                       max(1, clog2(self.agg_height)),
                                       size=self.max_agg_schedule,
                                       explicit_array=True,
                                       packed=True)
        doc = "Input schedule for aggregation buffer. Enumerate which" + \
              f" of {self.agg_height} buffers to write to."
        self._in_schedule.add_attribute(ConfigRegAttr(doc))

        self._in_period = self.input("in_period", clog2(self.max_agg_schedule))
        doc = "Input period for aggregation buffer. 1 is a reasonable" + \
              " setting for most applications"
        self._in_period.add_attribute(ConfigRegAttr(doc))

        # ...and which order to output the blocks
        self._out_schedule = self.input("out_sched",
                                        max(1, clog2(agg_height)),
                                        size=self.max_agg_schedule,
                                        explicit_array=True,
                                        packed=True)
        doc = "Output schedule for aggregation buffer. Enumerate which" + \
              f" of {self.agg_height} buffers to write to SRAM from."
        self._out_schedule.add_attribute(ConfigRegAttr(doc))

        self._out_period = self.input("out_period", clog2(self.max_agg_schedule))
        self._out_period.add_attribute(ConfigRegAttr("Output period for aggregation buffer"))

        self._in_sched_ptr = self.var("in_sched_ptr", clog2(self.max_agg_schedule))
        self._out_sched_ptr = self.var("out_sched_ptr", clog2(self.max_agg_schedule))

        # Local Signals
        self._aggs_out = self.var("aggs_out",
                                  self.mem_width,
                                  size=self.agg_height,
                                  packed=True,
                                  explicit_array=True)

        self._aggs_sep = []
        for i in range(self.agg_height):
            self._aggs_sep.append(self.var(f"aggs_sep_{i}",
                                           self.data_width,
                                           size=self.fw_int,
                                           packed=True))
        self._valid_demux = self.var("valid_demux", self.agg_height)
        self._align_demux = self.var("align_demux", self.agg_height)
        self._next_full = self.var("next_full", self.agg_height)
        self._valid_out_mux = self.var("valid_out_mux", self.agg_height)
        for i in range(self.agg_height):
            # Add in the children aggregators...
            self.add_child(f"agg_{i}",
                           Aggregator(self.data_width,
                                      mem_word_width=self.fw_int),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           in_pixels=self._data_in,
                           valid_in=self._valid_demux[i],
                           agg_out=self._aggs_sep[i],
                           valid_out=self._valid_out_mux[i],
                           next_full=self._next_full[i],
                           align=self._align_demux[i])
            portlist = []
            if self.fw_int == 1:
                self.wire(self._aggs_out[i], self._aggs_sep[i])
            else:
                for j in range(self.fw_int):
                    portlist.append(self._aggs_sep[i][self.fw_int - 1 - j])
                self.wire(self._aggs_out[i], kts.concat(*portlist))

        # Sequential code blocks
        self.add_code(self.update_in_sched_ptr)
        self.add_code(self.update_out_sched_ptr)

        # Combinational code blocks
        self.add_code(self.valid_demux_comb)
        self.add_code(self.align_demux_comb)
        self.add_code(self.valid_out_comb)
        self.add_code(self.output_data_comb)

    # Update the pointer and mux for input and output schedule
    # Now, obey the input schedule to send to the proper Aggregator
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_in_sched_ptr(self):
        if ~self._rst_n:
            self._in_sched_ptr = 0
        elif self._next_full[self._in_schedule[self._in_sched_ptr]]:
            self._in_sched_ptr = \
                ternary(self._in_sched_ptr == (self._in_period - 1),
                        const(0, self._in_sched_ptr.width),
                        self._in_sched_ptr + const(1, self._in_sched_ptr.width))

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_out_sched_ptr(self):
        if ~self._rst_n:
            self._out_sched_ptr = 0
        elif self._valid_out:
            self._out_sched_ptr = \
                ternary(self._out_sched_ptr == (self._out_period - 1),
                        const(0, self._out_sched_ptr.width),
                        self._out_sched_ptr + const(1, self._out_sched_ptr.width))

    @always_comb
    def valid_demux_comb(self):
        self._valid_demux = 0
        self._valid_demux[self._in_schedule[self._in_sched_ptr]] = self._valid_in

    @always_comb
    def align_demux_comb(self):
        self._align_demux = 0
        self._align_demux[self._in_schedule[self._in_sched_ptr]] = self._align

    @always_comb
    def valid_out_comb(self):
        self._valid_out = self._valid_out_mux[self._out_schedule[self._out_sched_ptr]]

    @always_comb
    def output_data_comb(self):
        self._data_out = self._aggs_out[self._out_schedule[self._out_sched_ptr]]


if __name__ == "__main__":
    db_dut = AggregationBuffer(agg_height=4,
                               data_width=16,
                               mem_width=64,
                               max_agg_schedule=64)

    verilog(db_dut, filename="aggregation_buffer.sv")
