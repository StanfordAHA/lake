from lake.modules.spec.sched_gen import SchedGen
from lake.modules.for_loop import ForLoop
from lake.attributes.dedicated_port import DedicatedPortAttribute
from lake.top.memory_controller import MemoryController
from kratos import *
from lake.utils.parse_clkwork_config import map_controller, extract_controller_json
import kratos as kts


class StencilValid(MemoryController):
    def __init__(self, name="stencil_valid"):
        super().__init__(name)

        self.stencil_valid_width = 16

        self.define_io()

        self._cycle_count = self.var("cycle_count", 16)

        self._loops_stencil_valid = ForLoop(iterator_support=6,
                                            config_width=10)
        self._stencil_valid_int = self.var("stencil_valid_internal", 1)

        # Loop Iterators for stencil valid...
        self.add_child(f"loops_stencil_valid",
                       self._loops_stencil_valid,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._stencil_valid_int)
        # Schedule Generator for stencil valid...
        self.add_child(f"stencil_valid_sched_gen",
                       SchedGen(iterator_support=6,
                                config_width=16),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       mux_sel=self._loops_stencil_valid.ports.mux_sel_out,
                       finished=self._loops_stencil_valid.ports.restart,
                       valid_output=self._stencil_valid_int)

        self._flush = self.input("flush", 1)
        self._flushed = self.var("flushed", 1)
        # Wire out internal wire
        self.wire(self._stencil_valid, self._stencil_valid_int & self._flushed)

        self.add_code(self.cycle_count_inc)
        self.add_code(self.flushed_set)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def flushed_set(self):
        if ~self._rst_n:
            self._flushed = 0
        elif self._flush:
            self._flushed = 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def cycle_count_inc(self):
        if ~self._rst_n:
            self._cycle_count = 0
        elif self._flushed:
            self._cycle_count = self._cycle_count + 1

    def define_io(self):
        """[Defines the inputs and outputs to the block for ease of reading]
        """
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._stencil_valid = self.output("stencil_valid", 1)
        self._stencil_valid.add_attribute(DedicatedPortAttribute())

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_bitstream(self, config_json):
        '''
        Pass in a config-related json to return a list of
        (config_reg, value) tuples
        '''
        config = []
        stencil_valid = map_controller(extract_controller_json(config_json['stencil_valid']), "stencil_valid")
        config.append((f"stencil_valid_sched_gen_enable", 1))
        config.append((f"stencil_valid_sched_gen_sched_addr_gen_starting_addr", stencil_valid.cyc_strt))
        config.append((f"loops_stencil_valid_dimensionality", stencil_valid.dim))
        for i in range(stencil_valid.dim):
            config.append((f"loops_stencil_valid_ranges_{i}", stencil_valid.extent[i]))
            config.append((f"stencil_valid_sched_gen_sched_addr_gen_strides_{i}", stencil_valid.cyc_stride[i]))
        return config

    def get_config_mode_str(self):
        return "stencil_valid"

    def get_dedicated_clock(self):
        return True
