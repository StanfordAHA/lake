from lake.spec.component import Component
from lake.utils.spec_enum import *
from lake.utils.util import add_counter
from lake.spec.iteration_domain import IterationDomain
import kratos as kts
from kratos import always_ff, always_comb, posedge, negedge


class ScheduleGenerator(Component):

    def __init__(self, dimensionality=6, stride_width=16, rv=False, name=None,
                 recurrence=True, external_count=False):
        self.mod_name = name
        if name is None:
            self.mod_name = f"sched_gen_{dimensionality}_{stride_width}_{recurrence}"
        super().__init__(name=self.mod_name)
        self.dimensionality_support = dimensionality
        self.stride_width = stride_width
        self.rv = rv
        self.mod_name = name
        self.exploit_recurrence = recurrence
        self.external_count = external_count

    def get_rv(self):
        return self.rv

    def gen_hardware(self, id: IterationDomain = None, num_comparisons: int = 1, pos_reset=False,
                     shared_clk_ctr=False):
        '''shared_clk_ctr: take the cycle count from input `shared_clk_ctr` (the
        low total_cycle_width bits of the Spec's one counter, share_clk_ctr)
        instead of a local clk_ctr. Same values: both clear on flush and count
        every clk_en cycle.
        '''
        assert id is not None
        assert not (shared_clk_ctr and self.external_count), "an external_count SG keeps its own counter"
        self.shared_clk_ctr = shared_clk_ctr
        id_ext_width = id.get_extent_width()
        # Total cycle width right now is stride width + ext_width
        self.total_cycle_width = id_ext_width + self.stride_width + self.dimensionality_support + 1

        ##########
        ### IO ###
        ##########
        ### Config Regs
        self._strides = self.config_reg(name="strides", width=self.stride_width,
                                        size=self.dimensionality_support,
                                        packed=True, explicit_array=True)

        #  Need an enable otherwise a schedule that starts off at 0 can accidentally fire...
        self._enable = self.config_reg(name="enable", width=1)

        # Using stride width instead of total cycle to keep config reg smaller
        self._starting_cycle = self.config_reg(name="starting_cycle", width=self.stride_width)
        ### Inputs

        increment_counter = kts.const(1, 1)
        if self.external_count:
            self._external_count_pin = self.input("external_count", 1)
            increment_counter = self._external_count_pin

        self.increment_counter = increment_counter

        if shared_clk_ctr:
            self._clk_ctr = self.input("shared_clk_ctr", self.total_cycle_width)
        else:
            self._clk_ctr = add_counter(self, "clk_ctr", bitwidth=self.total_cycle_width, increment=increment_counter,
                                        clear=self._flush, clk_en=self._clk_en)
        self._mux_sel = self.input("mux_sel", max(kts.clog2(self.dimensionality_support), 1))
        self._restart = self.input("restart", 1)
        self._finished = self.input("finished", 1)
        # Use signals directly for now
        self._ctrs = self.input("iterators", id_ext_width,
                                size=self.dimensionality_support,
                                packed=True, explicit_array=True)

        ### Outputs
        self._step_out = self.output("step", 1)

        ### Local variables
        self._current_cycle = self.var("current_cycle", self.total_cycle_width)
        self._strt_cycle = self.var("strt_cycle", self.total_cycle_width)
        self._step = self.var("step_lcl", 1)

        ### Logic
        self.wire(self._strt_cycle, kts.ext(self._starting_cycle, self.total_cycle_width))
        self.wire(self._step_out, self._step & ~self._flush & self._enable)

        # step is high when the current cycle matches the counter
        # self.wire(self._step, self._clk_ctr == self._current_cycle)
        self.wire(self._step, (self._clk_ctr == self._current_cycle) & ~self._finished)

        if self.exploit_recurrence:
            self.add_code(self.calculate_cycle_delta)
        else:
            self.add_code(self.calculate_cycle_count)

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    @always_comb
    def calculate_cycle_count(self):
        self._current_cycle = self._strt_cycle
        for i in range(self.dimensionality_support):
            self._current_cycle = self._current_cycle + self._ctrs[i] * self._strides[i]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def calculate_cycle_delta(self):
        if ~self._rst_n:
            self._current_cycle = 0
        elif self._flush:
            self._current_cycle = self._strt_cycle
        elif self._clk_en:
            # if self._step & ~self._finished:
            if self.increment_counter & self._step & ~self._finished:
                if self._restart:
                    self._current_cycle = self._strt_cycle
                else:
                    self._current_cycle = self._current_cycle + self._strides[self._mux_sel]

    def gen_bitstream(self, schedule_map, extents, dimensionality):
        assert 'strides' in schedule_map
        assert 'offset' in schedule_map

        self.clear_configuration()

        # Enable through the configuration
        self.configure(self._enable, 1)

        self.configure(self._starting_cycle, schedule_map['offset'])
        if self.exploit_recurrence:
            extent_sub_1 = [extent_item - 1 for extent_item in extents]
            tform_strides = [schedule_map['strides'][0]]
            offset = 0
            for i in range(dimensionality - 1):
                offset -= (extent_sub_1[i] * schedule_map['strides'][i])
                tform_strides.append(schedule_map['strides'][i + 1] + offset)

            self.configure(self._strides, tform_strides)
        else:
            self.configure(self._strides, schedule_map['strides'])
        # This will return pairs of ranges with values w.r.t. the node's configuration
        return self.get_configuration()

    def get_step(self):
        return self._step_out

    def get_dimensionality(self):
        return self.dimensionality_support


class ExplicitScheduleGenerator(ScheduleGenerator):

    def __init__(self, dimensionality=16):
        super().__init__(dimensionality=dimensionality)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        self.clear_configuration()
        return super().gen_bitstream()


class RecurrentScheduleGenerator(ScheduleGenerator):

    def __init__(self, dimensionality=16):
        super().__init__(dimensionality=dimensionality)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        self.clear_configuration()
        return super().gen_bitstream()


class ReadyValidScheduleGenerator(ScheduleGenerator):

    def __init__(self, dimensionality=16, name=None, recurrence=True):
        use_name = name
        if name is None:
            use_name = f"schedulegenerator_rv_dim_{dimensionality}"
        super().__init__(dimensionality=dimensionality, rv=True, name=use_name, recurrence=recurrence)
        self.num_comparisons = 1

    def get_num_comparisons(self):
        return self.num_comparisons

    def gen_hardware(self, id: IterationDomain = None, num_comparisons: int = 1, pos_reset=False):

        # Ready valid hardware implementation - just need the incoming iterators then to link up with all
        # others of this type
        assert id is not None
        self.num_comparisons = num_comparisons
        id_ext_width = id.get_extent_width()
        # Total cycle width right now is stride width + ext_width
        # self.total_cycle_width = id_ext_width + self.stride_width + self.dimensionality_support + 1

        # module_name = f"sched_gen_rv_{self.dimensionality_support}_{self.stride_width}"
        # super().__init__(mod_name=module_name)
        ##########
        ### IO ###
        ##########
        ### Config Regs
        ### Inputs

        # Still accept the iterators/mux_sel
        self._mux_sel = self.input("mux_sel", max(kts.clog2(self.dimensionality_support), 1))
        self._restart = self.input("restart", 1)
        self._finished = self.input("finished", 1)
        self._finished_out = self.output("finished_out", 1)
        # Use signals directly for now
        self._ctrs = self.input("iterators", id_ext_width,
                                size=self.dimensionality_support,
                                packed=True, explicit_array=True)
        self._extents = self.input("extents", id_ext_width,
                                   size=self.dimensionality_support,
                                   packed=True, explicit_array=True)
        #  Need an enable otherwise a schedule that starts off at 0 can accidentally fire...
        self._enable = self.config_reg(name="enable", width=1)
        ### Outputs
        self._step_out = self.output("step", 1)
        # self._step_ack = self.output("step_ack", 1)

        ### Local variables
        # self._current_cycle = self.var("current_cycle", self.total_cycle_width)
        # self._strt_cycle = self.var("strt_cycle", self.total_cycle_width)
        self._step = self.var("step_lcl", 1)

        ### Logic
        # Gate the output step if flush is high.
        self.wire(self._step_out, self._step & ~self._flush & self._enable & ~self._finished)

        # step is high when the current cycle matches the counter
        # self.wire(self._step, self._clk_ctr == self._current_cycle)

        # Now we need to output the iterators and take in the comparisons
        self._iterators_out = self.output("iterators_out_lcl", id_ext_width,
                                          size=self.dimensionality_support,
                                          packed=True, explicit_array=True)
        self._extents_out = self.output("extents_out_lcl", id_ext_width,
                                        size=self.dimensionality_support,
                                        packed=True, explicit_array=True)

        self._comparisons_in = self.input("comparisons", self.num_comparisons)

        self.iterator_intf = {}
        self.iterator_intf['iterators'] = self._iterators_out
        self.iterator_intf['extents'] = self._extents_out
        self.iterator_intf['finished'] = self._finished_out
        self.iterator_intf['comparisons'] = self._comparisons_in

        # We define the step to be the reduction-and of all the in comparisons
        self.wire(self._step, self._comparisons_in.r_and())
        # Everything else will be qualified outside the module to go to the AG, ID

        self.wire(self._iterators_out, self._ctrs)
        for i in range(self.dimensionality_support):
            self.wire(self._extents_out[i], self._extents[i] + 2)

        # Need to pass along the finish to the rv comparison network
        # so that things don't end early...
        self.wire(self._finished_out, self._finished)
        # self.add_code(self.calculate_cycle_count)
        # self.add_code(self.calculate_cycle_delta)

        # this is the valid out
        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def get_iterator_intf(self):
        return self.iterator_intf

    def in_comparison_network(self):
        return True

    def gen_bitstream(self, schedule_map=None, extents=None, dimensionality=None):
        self.clear_configuration()
        # Enable through the configuration
        self.configure(self._enable, 1)
        return self.get_configuration()
        # return super().gen_bitstream(schedule_map=sched_map)


class FIFOScheduleGenerator(ReadyValidScheduleGenerator):
    """Ready-valid schedule generator of one side of a FIFO: the comparison
    network's job, fixed. A write side steps while the FIFO is not full, a read
    side while it is not empty, so a stream passes through in order and the
    ports need no dependence constraints. Full/empty compare the two sides'
    LinearAddressGenerator pointers (address + lap bit): Spec wires `pointer`
    (this side's) and `peer_pointer` (the other side's). Not part of the
    RVComparisonNetwork. Configuration: `enable` only."""

    def __init__(self, direction: Direction, dimensionality=6, name=None):
        assert direction in (Direction.IN, Direction.OUT)
        self.direction = direction
        use_name = name
        if name is None:
            use_name = f"schedulegenerator_fifo_{'w' if direction == Direction.IN else 'r'}_dim_{dimensionality}"
        super().__init__(dimensionality=dimensionality, name=use_name)

    def in_comparison_network(self):
        return False

    def gen_hardware(self, id: IterationDomain = None, pointer_width: int = None, pos_reset=False):
        assert id is not None
        assert pointer_width is not None and pointer_width >= 2
        id_ext_width = id.get_extent_width()
        self.num_comparisons = 0
        # Same inputs as ReadyValidScheduleGenerator (Spec wires them alike);
        # only finished is used.
        self._mux_sel = self.input("mux_sel", max(kts.clog2(self.dimensionality_support), 1))
        self._restart = self.input("restart", 1)
        self._finished = self.input("finished", 1)
        self._ctrs = self.input("iterators", id_ext_width,
                                size=self.dimensionality_support,
                                packed=True, explicit_array=True)
        self._extents = self.input("extents", id_ext_width,
                                   size=self.dimensionality_support,
                                   packed=True, explicit_array=True)
        self._pointer = self.input("pointer", pointer_width)
        self._peer_pointer = self.input("peer_pointer", pointer_width)
        self._enable = self.config_reg(name="enable", width=1)
        self._step_out = self.output("step", 1)
        self._step = self.var("step_lcl", 1)

        msb = pointer_width - 1
        if self.direction == Direction.IN:
            # not full: the pointers differ in their address bits or share a lap
            w, r = self._pointer, self._peer_pointer
            self.wire(self._step, (w[msb - 1, 0] != r[msb - 1, 0]) | (w[msb] == r[msb]))
        else:
            # not empty
            self.wire(self._step, self._pointer != self._peer_pointer)
        self.wire(self._step_out, self._step & ~self._flush & self._enable & ~self._finished)

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def get_iterator_intf(self):
        raise NotImplementedError("a FIFOScheduleGenerator is not in the comparison network")
