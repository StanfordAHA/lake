from lake.spec.component import Component
from lake.utils.spec_enum import *
from lake.utils.util import sticky_flag
import kratos as kts
from lake.modules.for_loop import ForLoop
from kratos import *


class IterationDomain(Component):

    def __init__(self, dimensionality=6, extent_width=16):
        super().__init__(f"for_loop_{dimensionality}_{extent_width}_flush")
        self.dimensionality_support = dimensionality
        self.extent_width = extent_width
        self._interfaces = {}
        #  We want to handle flush and clk_en ourselves
        self.sync_reset_no_touch = True
        self.clk_en_no_touch = True

    def get_dimensionality(self):
        return self.dimensionality_support

    def gen_bitstream(self, dimensionality, extents, rv=False):

        # Actually add one if rv (to be safe) - only need to do this when
        # the comparison is at the top level, but let's just do this for now
        if rv:
            # self.configure(self._dimensionality, dimensionality + 1)
            self.configure(self._dimensionality, dimensionality)
        else:
            self.configure(self._dimensionality, dimensionality)

        # Do a - 2 thing...
        use_exts = [extent - 2 for extent in extents]

        # if rv:
        #   use_exts.append(4 - 2)

        self.configure(self._extents, use_exts)
        # This will return pairs of ranges with values w.r.t. the node's configuration
        return self.get_configuration()

    def get_extent_width(self):
        return self.extent_width

    def get_interfaces(self):
        return self._interfaces

    def gen_hardware(self, pos_reset=False):
        # Internal step, so no inputs? just clock???
        # self._clock = self.clock("clk")
        # self._rst_n = self.reset("rst_n")

        self._extents = self.config_reg(name="extents", width=self.extent_width,
                                        size=self.dimensionality_support,
                                        packed=True, explicit_array=True)
        self._extents_out = self.output(name="extents_out", width=self.extent_width,
                                        size=self.dimensionality_support,
                                        packed=True, explicit_array=True)

        self._dimensionality = self.config_reg(name="dimensionality", width=1 + kts.clog2(self.dimensionality_support))

        self._step = self.input("step", 1)
        # OUTPUTS
        self._mux_sel_out = self.output("mux_sel", max(kts.clog2(self.dimensionality_support), 1), packed=True)
        self._dim_counter_out = self.output("iterators", self.extent_width,
                                            size=self.dimensionality_support,
                                            packed=True,
                                            explicit_array=True)
        self._restart = self.output("restart", 1, packed=True)

        self._last_iter = self.output("last_iter", 1)
        self._last_iter_lcl = self.var("last_iter_lcl", 1)
        self.wire(self._last_iter, self._last_iter_lcl)

        # self._finished_lcl = self.var("finished_lcl", 1)
        self._finished = self.output("finished", 1)

        # PORT DEFS: end

        # LOCAL VARIABLES: begin

        self.wire(self._extents_out, self._extents)
        self._interfaces['extents'] = self._extents_out

        self._dim_counter = self.var("dim_counter_", self.extent_width,
                                     size=self.dimensionality_support,
                                     packed=True,
                                     explicit_array=True)

        # Alias the var to the output
        for i in range(self.dimensionality_support):
            self.wire(self._dim_counter_out[i], self._dim_counter[i])

        self._counter_update = self.var("counter_update", 1)

        self._max_value = self.var("max_value", self.dimensionality_support)

        self._mux_sel = self.var("mux_sel_lcl", max(kts.clog2(self.dimensionality_support), 1))

        # Gate mux_sel if step is low
        for i in range(self._mux_sel.width):
            self.wire(self._mux_sel_out[i], self._mux_sel[i] & self._step)
        # LOCAL VARIABLES: end
        # GENERATION LOGIC: begin
        self._done = self.var("done", 1)
        self._clear = self.var("clear", self.dimensionality_support)
        self._inc = self.var("inc", self.dimensionality_support)

        # We go to a finished state once the last iter is done
        self._finished_lcl = sticky_flag(self, self._last_iter_lcl & self._step, name="finished_lcl_sticky", seq_only=True, verbose=True)
        self.wire(self._finished, self._finished_lcl)

        self._inced_cnt = self.var("inced_cnt", self._dim_counter.width)
        self.wire(self._inced_cnt, self._dim_counter[self._mux_sel] + 1)
        # Next_max_value
        self._maxed_value = self.var("maxed_value", 1)

        self.wire(self._maxed_value, (self._dim_counter[self._mux_sel] == self._extents[self._mux_sel]) & self._inc[self._mux_sel])

        self.add_code(self.set_mux_sel)

        for i in range(self.dimensionality_support):
            self.add_code(self.set_clear, idx=i)
            self.add_code(self.set_inc, idx=i)
            self.add_code(self.dim_counter_update, idx=i)
            self.add_code(self.max_value_update, idx=i)
        # GENERATION LOGIC: end

        self.wire(self._restart, self._step & (~self._done))

        # The last iter is if done doesn't turn 1
        self.wire(self._last_iter_lcl, ~self._done)

        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    @always_comb
    # Find lowest ready
    def set_mux_sel(self):
        self._mux_sel = 0
        self._done = 0
        for i in range(self.dimensionality_support):
            if ~self._done & ~self._finished_lcl:
                if ~self._max_value[i] & (i < self._dimensionality):
                    self._mux_sel = i
                    self._done = 1

    @always_comb
    def set_clear(self, idx):
        self._clear[idx] = 0
        if ((idx < self._mux_sel) | (~self._done)) & self._step:
            self._clear[idx] = 1

    @always_comb
    def set_inc(self, idx):
        self._inc[idx] = 0
        # We always increment the innermost, and then have priority
        # on clear in the flops.
        if (const(idx, 5) == 0) & self._step & (idx < self._dimensionality):
            self._inc[idx] = 1
        elif (idx == self._mux_sel) & self._step & (idx < self._dimensionality):
            self._inc[idx] = 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def dim_counter_update(self, idx):
        if ~self._rst_n:
            self._dim_counter[idx] = 0
        elif self._flush:
            self._dim_counter[idx] = 0
        elif self._clk_en:
            if self._clear[idx]:
                self._dim_counter[idx] = 0
            elif self._inc[idx]:
                self._dim_counter[idx] = self._inced_cnt

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def max_value_update(self, idx):
        if ~self._rst_n:
            self._max_value[idx] = 0
        elif self._flush:
            self._max_value[idx] = 0
        elif self._clk_en:
            if self._clear[idx]:
                self._max_value[idx] = 0
            elif self._inc[idx]:
                self._max_value[idx] = self._maxed_value

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def mux_sel_mbs_r_update(self):
        if ~self._rst_n:
            self._mux_sel_msb_r = 0
        elif self._flush:
            self._mux_sel_msb_r = self._mux_sel_msb_init
        elif self._clk_en:
            if self._restart:
                self._mux_sel_msb_r = ~self._mux_sel_msb_r


class DefaultIterationDomain(IterationDomain):

    def __init__(self, dimensionality=6, extent_width=16):
        super().__init__(dimensionality=dimensionality,
                         extent_width=extent_width)

    def gen_hardware(self, pos_reset=False):
        return super().gen_hardware(pos_reset)

    def gen_bitstream(self):
        return super().gen_bitstream()
