from kratos import *
from functools import reduce
import operator
from lake.attributes.config_reg_attr import ConfigRegAttr


class AddrGen(Generator):
    '''
    Generate addresses for a single port
    '''
    def __init__(self,
                 iterator_support,
                 address_width,
                 config_width=16):

        super().__init__(f"addr_gen_{iterator_support}")

        self.iterator_support = iterator_support
        self.config_width = config_width

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._strides = self.input("strides", self.config_width,
                                   size=self.iterator_support,
                                   packed=True, explicit_array=True)
        self._strides.add_attribute(ConfigRegAttr("Strides of address generator"))

        self._ranges = self.input("ranges", self.config_width,
                                  size=self.iterator_support,
                                  packed=True, explicit_array=True)
        self._ranges.add_attribute(ConfigRegAttr("Ranges of address generator"))

        self._starting_addr = self.input("starting_addr", self.config_width)
        self._starting_addr.add_attribute(ConfigRegAttr("Starting address of address generator"))

        self._dimensionality = self.input("dimensionality", 1 + clog2(self.iterator_support))
        self._dimensionality.add_attribute(ConfigRegAttr("Dimensionality of address generator"))

        self._step = self.input("step", 1)

        # OUTPUTS
        self._addr_out = self.output("addr_out", self.config_width)

        # MISC
        self._clk_en = self.input("clk_en", 1)
        self._flush = self.input("flush", 1)

        # PORT DEFS: end

        # LOCAL VARIABLES: begin
        self._dim_counter = self.var("dim_counter", self.config_width,
                                     size=self.iterator_support,
                                     packed=True,
                                     explicit_array=True)

        self._strt_addr = self.var("strt_addr", self.config_width)

        self._counter_update = self.var("counter_update", 1)
        self._calc_addr = self.var("calc_addr", self.config_width)

        self._max_value = self.var("max_value", self.iterator_support)

        # LOCAL VARIABLES: end
        # GENERATION LOGIC: begin
        self.wire(self._strt_addr, self._starting_addr)
        self.wire(self._addr_out, self._calc_addr)

        self._mux_sel = self.var("mux_sel", max(clog2(self.iterator_support), 1))
        self._done = self.var("done", 1)
        self._clear = self.var("clear", self.iterator_support)
        self._inc = self.var("inc", self.iterator_support)

        self._inced_cnt = self.var("inced_cnt", self._dim_counter.width)
        self.wire(self._inced_cnt, self._dim_counter[self._mux_sel] + 1)
        # Next_max_value
        self._maxed_value = self.var("maxed_value", 1)
        self.wire(self._maxed_value, (self._dim_counter[self._mux_sel] ==
                                      self._ranges[self._mux_sel]) & self._inc[self._mux_sel])

        self._current_addr = self.var("current_addr", self.config_width)
        # Calculate address by taking previous calculation and adding the muxed stride
        self.wire(self._calc_addr, self._strt_addr + self._current_addr)

        self.add_code(self.set_mux_sel)
        self.add_code(self.calculate_address)
        for i in range(self.iterator_support):
            self.add_code(self.set_clear, idx=i)
            self.add_code(self.set_inc, idx=i)
            self.add_code(self.dim_counter_update, idx=i)
            self.add_code(self.max_value_update, idx=i)
        # GENERATION LOGIC: end

    @always_comb
    # Find lowest ready
    def set_mux_sel(self):
        self._mux_sel = 0
        self._done = 0
        for i in range(self.iterator_support):
            if ~self._done:
                if ~self._max_value[i]:
                    self._mux_sel = i
                    self._done = 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def calculate_address(self):
        if ~self._rst_n:
            self._current_addr = 0
        elif self._step:
            self._current_addr = self._current_addr + self._strides[self._mux_sel]

    @always_comb
    def set_clear(self, idx):
        self._clear[idx] = 0
        if (idx < self._mux_sel) & self._step:
            self._clear[idx] = 1

    @always_comb
    def set_inc(self, idx):
        self._inc[idx] = 0
        # We always increment the innermost, and then have priority
        # on clear in the flops.
        if (const(idx, 5) == 0) & self._step:
            self._inc[idx] = 1
        elif (idx == self._mux_sel) & self._step:
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


if __name__ == "__main__":
    db_dut = AddrGen(iterator_support=6, address_width=16)
    verilog(db_dut, filename="addr_gen.sv")
