from ast import Constant
from asyncio import constants
from kratos import *
from functools import reduce
import operator
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


class OuterForLoop(Generator):
    '''
    Generate addresses for a single port
    '''
    def __init__(self,
                 iterator_support=6,
                 iter_start=0,  # used for outer for loops
                 config_width=16):

        super().__init__(f"for_loop_{iter_start}_{iterator_support}_{config_width}", debug=True)

        self.iterator_support = iterator_support
        self.iter_start = iter_start
        self.iter_levels = iterator_support - iter_start
        self.config_width = config_width
        # Create params for instancing this module...
        self.iterator_support_par = self.param("ITERATOR_SUPPORT", clog2(self.iter_levels) + 1, value=self.iter_levels)
        self.config_width_par = self.param("CONFIG_WIDTH", clog2(config_width) + 1, value=self.config_width)

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._ranges = self.input("ranges", self.config_width,
                                  size=self.iter_levels,
                                  packed=True, explicit_array=True)
        self._ranges.add_attribute(ConfigRegAttr("Ranges of address generator"))
        self._ranges.add_attribute(FormalAttr(f"{self._ranges.name}", FormalSignalConstraint.SOLVE))

        self._dimensionality = self.input("dimensionality", 1 + clog2(self.iterator_support))
        self._dimensionality.add_attribute(ConfigRegAttr("Dimensionality of address generator"))
        self._dimensionality.add_attribute(FormalAttr(f"{self._dimensionality.name}", FormalSignalConstraint.SOLVE))

        self._step = self.input("step", 1)
        # OUTPUTS

        # PORT DEFS: end

        # LOCAL VARIABLES: begin
        self._dim_counter = self.var("dim_counter", self.config_width,
                                     size=self.iter_levels,
                                     packed=True,
                                     explicit_array=True)

        self._strt_addr = self.var("strt_addr", self.config_width)

        self._counter_update = self.var("counter_update", 1)
        self._calc_addr = self.var("calc_addr", self.config_width)

        self._max_value = self.var("max_value", self.iter_levels)
        self._mux_sel = self.var("mux_sel", max(clog2(self.iterator_support), 1))
        self._mux_sel_out = self.output("mux_sel_out", max(clog2(self.iterator_support), 1))
        self.wire(self._mux_sel_out, self._mux_sel)
        # LOCAL VARIABLES: end
        # GENERATION LOGIC: begin
        self._done = self.var("done", 1)
        self._clear = self.var("clear", self.iter_levels)
        self._inc = self.var("inc", self.iter_levels)

        self._ranges_mux_sel = self.var("ranges_mux_sel", self.config_width)
        self._dim_counter_mux_sel = self.var("dim_counter_mux_sel", self.config_width)
        self._inc_mux_sel = self.var("inc_mux_sel", 1)

        mux_sel_comb = self.combinational()
        switch_ = mux_sel_comb.switch_(self._mux_sel)
        for i in range(self.iter_start, self.iterator_support):
            switch_.case_(i, self._ranges_mux_sel(self._ranges[i - self.iter_start]))
            switch_.case_(i, self._dim_counter_mux_sel(self._dim_counter[i - self.iter_start]))
            switch_.case_(i, self._inc_mux_sel(self._inc[i - self.iter_start]))
        switch_.case_(None, self._ranges_mux_sel(0))
        switch_.case_(None, self._dim_counter_mux_sel(0))
        switch_.case_(None, self._inc_mux_sel(0))

        self._inced_cnt = self.var("inced_cnt", self._dim_counter.width)
        self.wire(self._inced_cnt, self._dim_counter_mux_sel + 1)
        # Next_max_value
        self._maxed_value = self.var("maxed_value", 1)
        self.wire(self._maxed_value, (self._dim_counter_mux_sel ==
                                      self._ranges_mux_sel) & self._inc_mux_sel)

        self.add_code(self.set_mux_sel)
        for i in range(self.iter_start, self.iterator_support):
            self.add_code(self.set_clear, idx=i - self.iter_start)
            self.add_code(self.set_inc, idx=i - self.iter_start)
            self.add_code(self.dim_counter_update, idx=i - self.iter_start)
            self.add_code(self.max_value_update, idx=i - self.iter_start)
        # GENERATION LOGIC: end

        self._restart = self.output("restart", 1)
        self.wire(self._restart, self._step & (~self._done))

    @always_comb
    # Find lowest ready
    def set_mux_sel(self):
        self._mux_sel = self.iter_start
        self._done = 0
        for i in range(self.iter_start, self.iterator_support):
            if ~self._done:
                if ~self._max_value[i - self.iter_start] & (i < self._dimensionality):
                    self._mux_sel = i
                    self._done = 1

    @always_comb
    def set_clear(self, idx):
        self._clear[idx] = 0
        if ((idx + self.iter_start < self._mux_sel) | (~self._done)) & self._step:
            self._clear[idx] = 1

    @always_comb
    def set_inc(self, idx):
        self._inc[idx] = 0
        # We always increment the innermost, and then have priority
        # on clear in the flops.
        if (const(idx, 5) == 0) & self._step & (idx + self.iter_start < self._dimensionality):
            self._inc[idx] = 1
        elif (idx + self.iter_start == self._mux_sel) & self._step & (idx + self.iter_start < self._dimensionality):
            self._inc[idx] = 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def dim_counter_update(self, idx):
        if ~self._rst_n:
            self._dim_counter[idx] = 0
        else:
            if self._clear[idx]:
                self._dim_counter[idx] = 0
            elif self._inc[idx]:
                self._dim_counter[idx] = self._inced_cnt

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def max_value_update(self, idx):
        if ~self._rst_n:
            self._max_value[idx] = 0
        else:
            if self._clear[idx]:
                self._max_value[idx] = 0
            elif self._inc[idx]:
                self._max_value[idx] = self._maxed_value

    def get_iter(self):
        return self.iter_levels

    def get_cfg_width(self):
        return self.config_width


if __name__ == "__main__":
    it_support = 6
    it_0 = 3
    cf_width = 16
    db_dut = OuterForLoop(iterator_support=it_support,
                          iter_start=it_0,
                          config_width=cf_width)
    verilog(db_dut, filename=f"for_loop_{it_0}_{it_support}_{cf_width}.sv")
