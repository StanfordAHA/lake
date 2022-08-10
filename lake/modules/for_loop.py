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

        super().__init__(f"for_loop_{iterator_support}_{config_width}", debug=True)

        self.iterator_support = iterator_support
        self.config_width = config_width
        self.dual_config = dual_config
        self.iterator_support2 = iterator_support2
        self.max_iterator_support = max(self.iterator_support, self.iterator_support2)
        self.iter_idx_w = max(clog2(self.iterator_support), 1)
        self.iter2_idx_w = max(clog2(self.iterator_support2), 1)
        # Create params for instancing this module...
        # self.iterator_support_par = self.param("ITERATOR_SUPPORT", clog2(iterator_support) + 1, value=self.iterator_support)
        # self.iterator_support_par2 = self.param("ITERATOR_SUPPORT2", clog2(iterator_support2) + 1, value=self.iterator_support2)
        # self.config_width_par = self.param("CONFIG_WIDTH", clog2(config_width) + 1, value=self.config_width)

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

        self._step = self.input("step", 1)
        # OUTPUTS

        # PORT DEFS: end

        # LOCAL VARIABLES: begin
        self._dim_counter = self.var("dim_counter", self.config_width,
                                     size=self.iterator_support,
                                     packed=True,
                                     explicit_array=True)
        if self.dual_config:
            self._dim_counter2 = self.var("dim_counter2", self.config_width,
                                          size=self.iterator_support2,
                                          packed=True,
                                          explicit_array=True)

        self._strt_addr = self.var("strt_addr", self.config_width)

        self._counter_update = self.var("counter_update", 1)
        self._calc_addr = self.var("calc_addr", self.config_width)

        self._max_value = self.var("max_value", self.iterator_support)
        if self.dual_config:
            self._max_value2 = self.var("max_value2", self.iterator_support2)
        if self.dual_config:
            self._mux_sel = self.var("mux_sel", max(clog2(self.max_iterator_support) + 1, 1))
            self._mux_sel_out = self.output("mux_sel_out", max(clog2(self.max_iterator_support) + 1, 1))
            self._mux_sel_msb = self.var("mux_sel_msb", 1)
            self._mux_sel_iter1 = self.var("mux_sel_iter1", self.iter_idx_w)
            self._mux_sel_iter2 = self.var("mux_sel_iter2", self.iter2_idx_w)
            self.wire(self._mux_sel_msb, self._mux_sel[self._mux_sel.width - 1])
            self.wire(self._mux_sel_iter1, self._mux_sel[self.iter_idx_w - 1, 0])
            self.wire(self._mux_sel_iter2, self._mux_sel[self.iter2_idx_w - 1, 0])
        else:
            self._mux_sel = self.var("mux_sel", max(clog2(self.iterator_support), 1))
            self._mux_sel_out = self.output("mux_sel_out", max(clog2(self.iterator_support), 1))
        self.wire(self._mux_sel_out, self._mux_sel)
        # LOCAL VARIABLES: end
        # GENERATION LOGIC: begin
        self._done = self.var("done", 1)
        self._clear = self.var("clear", self.iterator_support)
        self._inc = self.var("inc", self.iterator_support)
        if self.dual_config:
            self._clear2 = self.var("clear2", self.iterator_support2)
            self._inc2 = self.var("inc2", self.iterator_support2)

        self._inced_cnt = self.var("inced_cnt", self._dim_counter.width)
        if self.dual_config:
            self._cur_dim_counter = self.var("cur_dim_counter", self.config_width)
            self.wire(self._cur_dim_counter, ternary(self._mux_sel_msb,
                                                     self._dim_counter2[self._mux_sel_iter2],
                                                     self._dim_counter[self._mux_sel_iter1]))
            self.wire(self._inced_cnt, self._cur_dim_counter + 1)
        else:
            self.wire(self._inced_cnt, self._dim_counter[self._mux_sel] + 1)
        # Next_max_value
        self._maxed_value = self.var("maxed_value", 1)
        if self.dual_config:
            self._cur_range = self.var("cur_range", self.config_width)
            self._cur_inc = self.var("cur_inc", 1)
            self.wire(self._cur_range, ternary(self._mux_sel_msb,
                                               self._ranges2[self._mux_sel_iter2],
                                               self._ranges[self._mux_sel_iter1]))
            self.wire(self._cur_inc, ternary(self._mux_sel_msb,
                                             self._inc2[self._mux_sel_iter2],
                                             self._inc[self._mux_sel_iter1]))
            self.wire(self._maxed_value, (self._cur_dim_counter ==
                                          self._cur_range) & self._cur_inc)
        else:
            self.wire(self._maxed_value, (self._dim_counter[self._mux_sel] ==
                                          self._ranges[self._mux_sel]) & self._inc[self._mux_sel])

        self.add_code(self.set_mux_sel)
        for i in range(self.iterator_support):
            self.add_code(self.set_clear, idx=i)
            self.add_code(self.set_inc, idx=i)
            self.add_code(self.dim_counter_update, idx=i)
            self.add_code(self.max_value_update, idx=i)
        if self.dual_config:
            for i in range(self.iterator_support2):
                self.add_code(self.set_clear2, idx=i)
                self.add_code(self.set_inc2, idx=i)
                self.add_code(self.dim_counter_update2, idx=i)
                self.add_code(self.max_value_update2, idx=i)
        # GENERATION LOGIC: end

        self._restart = self.output("restart", 1)
        self.wire(self._restart, self._step & (~self._done))

    @always_comb
    # Find lowest ready
    def set_mux_sel(self):
        self._mux_sel = 0
        self._done = 0
        if self.dual_config:
            for i in range(self.iterator_support):
                if ~self._done:
                    if ~self._max_value[i] & (i < self._dimensionality):
                        self._mux_sel = concat(const(0, 1), const(i, self._mux_sel.width - 1))
                        self._done = 1
            for i in range(self.iterator_support2):
                if ~self._done:
                    if ~self._max_value2[i] & (i < self._dimensionality2):
                        self._mux_sel = concat(const(1, 1), const(i, self._mux_sel.width - 1))
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
        if self.dual_config:
            if ((idx < self._mux_sel_iter1) | (~self._done)) & self._step & (~self._mux_sel_msb):
                self._clear[idx] = 1
        else:
            if ((idx < self._mux_sel) | (~self._done)) & self._step:
                self._clear[idx] = 1

    @always_comb
    def set_clear2(self, idx):
        self._clear2[idx] = 0
        if ((idx < self._mux_sel_iter2) | (~self._done)) & self._step:
            self._clear2[idx] = 1

    @always_comb
    def set_inc(self, idx):
        self._inc[idx] = 0
        if self.dual_config:
            # We always increment the innermost, and then have priority
            # on clear in the flops.
            # do not increment when the second controller is busy
            if (const(idx, 5) == 0) & (~self._mux_sel_msb) & self._step & (idx < self._dimensionality):
                self._inc[idx] = 1
            elif (idx == self._mux_sel_iter1) & (~self._mux_sel_msb) & self._step & (idx < self._dimensionality):
                self._inc[idx] = 1
        else:
            # We always increment the innermost, and then have priority
            # on clear in the flops.
            if (const(idx, 5) == 0) & self._step & (idx < self._dimensionality):
                self._inc[idx] = 1
            elif (idx == self._mux_sel) & self._step & (idx < self._dimensionality):
                self._inc[idx] = 1

    @always_comb
    def set_inc2(self, idx):
        self._inc2[idx] = 0
        # We always increment the innermost, and then have priority
        # on clear in the flops.
        if (const(idx, 5) == 0) & self._mux_sel_msb & self._step & (idx < self._dimensionality2):
            self._inc2[idx] = 1
        elif (idx == self._mux_sel_iter2) & self._mux_sel_msb & self._step & (idx < self._dimensionality2):
            self._inc2[idx] = 1

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
    def dim_counter_update2(self, idx):
        if ~self._rst_n:
            self._dim_counter2[idx] = 0
        else:
            if self._clear2[idx]:
                self._dim_counter2[idx] = 0
            elif self._inc2[idx]:
                self._dim_counter2[idx] = self._inced_cnt

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
    def max_value_update2(self, idx):
        if ~self._rst_n:
            self._max_value2[idx] = 0
        else:
            if self._clear2[idx]:
                self._max_value2[idx] = 0
            elif self._inc2[idx]:
                self._max_value2[idx] = self._maxed_value

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
