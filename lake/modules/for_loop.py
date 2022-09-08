from kratos import *
import operator
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


class ForLoop(Generator):
    '''
    Generate addresses for a single port
    '''
    def __init__(self,
                 iterator_support=6,
                 config_width=16,
                 dual_config=False,
                 iterator_support2=2):

        if dual_config:
            super().__init__(f"for_loop_dual_config_{iterator_support}_{iterator_support2}_{config_width}", debug=True)
        else:
            super().__init__(f"for_loop_{iterator_support}_{config_width}", debug=True)

        self.iterator_support = iterator_support
        self.config_width = config_width
        self.dual_config = dual_config
        self.iterator_support2 = iterator_support2
        self.max_iterator_support = max(self.iterator_support, self.iterator_support2)
        self.iter_idx_w = max(clog2(self.iterator_support), 1)
        self.iter2_idx_w = max(clog2(self.iterator_support2), 1)
        # Create params for instancing this module...
        self.iterator_support_par = self.param("ITERATOR_SUPPORT", clog2(iterator_support) + 1, initial_value=self.iterator_support)
        self.iterator_support_par2 = self.param("ITERATOR_SUPPORT2", clog2(iterator_support2) + 1, initial_value=self.iterator_support2)
        self.config_width_par = self.param("CONFIG_WIDTH", clog2(config_width) + 1, initial_value=self.config_width)

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._ranges = self.input("ranges", self.config_width,
                                  size=self.iterator_support,
                                  packed=True, explicit_array=True)
        self._ranges.add_attribute(ConfigRegAttr("Ranges of address generator"))
        self._ranges.add_attribute(FormalAttr(f"{self._ranges.name}", FormalSignalConstraint.SOLVE))

        self._dimensionality = self.input("dimensionality", 1 + clog2(self.iterator_support))
        self._dimensionality.add_attribute(ConfigRegAttr("Dimensionality of address generator"))
        self._dimensionality.add_attribute(FormalAttr(f"{self._dimensionality.name}", FormalSignalConstraint.SOLVE))

        if self.dual_config:
            self._ranges2 = self.input("ranges2", self.config_width,
                                       size=self.iterator_support2,
                                       packed=True, explicit_array=True)
            self._ranges2.add_attribute(ConfigRegAttr("Ranges of address generator"))
            self._ranges2.add_attribute(FormalAttr(f"{self._ranges2.name}", FormalSignalConstraint.SOLVE))

            self._dimensionality2 = self.input("dimensionality2", 1 + clog2(self.iterator_support2))
            self._dimensionality2.add_attribute(ConfigRegAttr("Dimensionality of address generator"))
            self._dimensionality2.add_attribute(FormalAttr(f"{self._dimensionality2.name}", FormalSignalConstraint.SOLVE))

            self._mux_sel_msb_init = self.input("mux_sel_msb_init", 1)
            self._flush = self.input("flush", 1)
            self.add_attribute("sync-reset=flush")

        self._step = self.input("step", 1)
        # OUTPUTS

        # PORT DEFS: end

        # LOCAL VARIABLES: begin
        if self.dual_config:
            self._dim_counter = self.var("dim_counter", self.config_width,
                                         size=self.max_iterator_support,
                                         packed=True,
                                         explicit_array=True)
        else:
            self._dim_counter = self.var("dim_counter", self.config_width,
                                         size=self.iterator_support,
                                         packed=True,
                                         explicit_array=True)

        self._strt_addr = self.var("strt_addr", self.config_width)

        self._counter_update = self.var("counter_update", 1)
        self._calc_addr = self.var("calc_addr", self.config_width)

        if self.dual_config:
            self._max_value = self.var("max_value", self.max_iterator_support)
        else:
            self._max_value = self.var("max_value", self.iterator_support)

        if self.dual_config:
            self._cur_dimensionality = self.var("cur_dimensionality", 1 + clog2(self.max_iterator_support))
            self._mux_sel = self.var("mux_sel", max(clog2(self.max_iterator_support), 1))
            self._mux_sel_out = self.output("mux_sel_out", max(clog2(self.max_iterator_support) + 1, 1))
            self._mux_sel_msb = self.var("mux_sel_msb", 1)
            self._mux_sel_msb_r = self.var("mux_sel_msb_r", 1)
            self._mux_sel_iter1 = self.var("mux_sel_iter1", self.iter_idx_w)
            self._mux_sel_iter2 = self.var("mux_sel_iter2", self.iter2_idx_w)

            self.wire(self._mux_sel_msb, self._mux_sel_msb_r)
            # self.wire(self._mux_sel_msb, ternary(self._flush, self._mux_sel_msb_init, self._mux_sel_msb_r))
            self.wire(self._cur_dimensionality, ternary(self._mux_sel_msb, self._dimensionality2, self._dimensionality))
            self.wire(self._mux_sel_iter1, self._mux_sel[self.iter_idx_w - 1, 0])
            self.wire(self._mux_sel_iter2, self._mux_sel[self.iter2_idx_w - 1, 0])
            self.wire(self._mux_sel_out, concat(self._mux_sel_msb, self._mux_sel))
        else:
            self._mux_sel = self.var("mux_sel", max(clog2(self.iterator_support), 1))
            self._mux_sel_out = self.output("mux_sel_out", max(clog2(self.iterator_support), 1))
            self.wire(self._mux_sel_out, self._mux_sel)
        # LOCAL VARIABLES: end
        # GENERATION LOGIC: begin
        self._done = self.var("done", 1)
        if self.dual_config:
            self._clear = self.var("clear", self.max_iterator_support)
            self._inc = self.var("inc", self.max_iterator_support)
        else:
            self._clear = self.var("clear", self.iterator_support)
            self._inc = self.var("inc", self.iterator_support)

        self._inced_cnt = self.var("inced_cnt", self._dim_counter.width)
        self.wire(self._inced_cnt, self._dim_counter[self._mux_sel] + 1)
        # Next_max_value
        self._maxed_value = self.var("maxed_value", 1)
        if self.dual_config:
            self._cur_range = self.var("cur_range", self.config_width)
            self.wire(self._cur_range, ternary(self._mux_sel_msb,
                                               self._ranges2[self._mux_sel_iter2],
                                               self._ranges[self._mux_sel_iter1]))
            self.wire(self._maxed_value, (self._dim_counter[self._mux_sel] ==
                                          self._cur_range) & self._inc[self._mux_sel])
        else:
            self.wire(self._maxed_value, (self._dim_counter[self._mux_sel] ==
                                          self._ranges[self._mux_sel]) & self._inc[self._mux_sel])

        self.add_code(self.set_mux_sel)
        if self.dual_config:
            for i in range(self.max_iterator_support):
                self.add_code(self.set_clear, idx=i)
                self.add_code(self.set_inc, idx=i)
                self.add_code(self.dim_counter_update, idx=i)
                self.add_code(self.max_value_update, idx=i)
        else:
            for i in range(self.iterator_support):
                self.add_code(self.set_clear, idx=i)
                self.add_code(self.set_inc, idx=i)
                self.add_code(self.dim_counter_update, idx=i)
                self.add_code(self.max_value_update, idx=i)
        # GENERATION LOGIC: end

        self._restart = self.output("restart", 1)
        if self.dual_config:
            self.add_code(self.mux_sel_mbs_r_update)
        self.wire(self._restart, self._step & (~self._done))

    @always_comb
    # Find lowest ready
    def set_mux_sel(self):
        self._mux_sel = 0
        self._done = 0
        if self.dual_config:
            for i in range(self.max_iterator_support):
                if ~self._done:
                    if ~self._max_value[i] & (i < self._cur_dimensionality):
                        self._mux_sel = i
                        self._done = 1
        else:
            for i in range(self.iterator_support):
                if ~self._done:
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
        if self.dual_config:
            # We always increment the innermost, and then have priority
            # on clear in the flops.
            if (const(idx, 5) == 0) & self._step & (idx < self._cur_dimensionality):
                self._inc[idx] = 1
            elif (idx == self._mux_sel) & self._step & (idx < self._cur_dimensionality):
                self._inc[idx] = 1
        else:
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

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def mux_sel_mbs_r_update(self):
        if ~self._rst_n:
            self._mux_sel_msb_r = 0
        elif self._flush:
            self._mux_sel_msb_r = self._mux_sel_msb_init
        elif self._restart:
            self._mux_sel_msb_r = ~self._mux_sel_msb_r

    def get_iter(self):
        return self.iterator_support

    def get_cfg_width(self):
        return self.config_width


if __name__ == "__main__":
    it_support = 6
    cf_width = 16
    db_dut = ForLoop(iterator_support=it_support,
                     config_width=cf_width)
    verilog(db_dut, filename=f"for_loop_{it_support}.sv")
