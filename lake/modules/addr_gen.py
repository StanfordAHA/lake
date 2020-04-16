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
        self._current_loc = self.var("current_loc", self.config_width,
                                     size=self.iterator_support,
                                     packed=True,
                                     explicit_array=True)

        self._write_addr = self.var("write_addr", self.config_width)
        self._dim_counter = self.var("dim_counter", self.config_width,
                                     size=self.iterator_support,
                                     packed=True,
                                     explicit_array=True)

        self._update = self.var("update", self.iterator_support)
        self._strt_addr = self.var("strt_addr", self.config_width)

        self._counter_update = self.var("counter_update", 1)
        self._calc_addr = self.var("calc_addr", self.config_width)

        # LOCAL VARIABLES: end

        # GENERATION LOGIC: begin
        self.wire(self._strt_addr, self._starting_addr)

        self.wire(self._addr_out, self._calc_addr)

        # Set update vector

        self.wire(self._update[0], const(1, 1))
        for i in range(self.iterator_support - 1):
            self.wire(self._update[i + 1],
                      (self._dim_counter[i] == (self._ranges[i] - 1)) & self._update[i])

        self.add_code(self.calc_addr_comb)
        self.add_code(self.dim_counter_update)
        self.add_code(self.current_loc_update)

        # GENERATION LOGIC: end

    @always_comb
    def calc_addr_comb(self):
        self._calc_addr = reduce(operator.add,
                                 [(ternary(const(i,
                                                 self._dimensionality.width) < self._dimensionality,
                                   self._current_loc[i], const(0, self._calc_addr.width)))
                                  for i in range(self.iterator_support)] + [self._strt_addr])

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def dim_counter_update(self):
        if ~self._rst_n:
            self._dim_counter = 0
        elif self._clk_en:
            if self._flush:
                for i in range(self.iterator_support):
                    self._dim_counter[i] = 0
            elif (self._step):
                for i in range(self.iterator_support):
                    if self._update[i] & (i < self._dimensionality):
                        if self._dim_counter[i] == (self._ranges[i] - 1):
                            self._dim_counter[i] = 0
                        else:
                            self._dim_counter[i] = self._dim_counter[i] + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def current_loc_update(self):
        if ~self._rst_n:
            self._current_loc = 0
        elif self._clk_en:
            if self._flush:
                for i in range(self.iterator_support):
                    self._current_loc[i] = 0
            elif self._step:
                for i in range(self.iterator_support):
                    if self._update[i] & (i < self._dimensionality):
                        if self._dim_counter[i] == (self._ranges[i] - 1):
                            self._current_loc[i] = 0
                        else:
                            self._current_loc[i] = self._current_loc[i] + self._strides[i]


if __name__ == "__main__":
    db_dut = AddrGen(iterator_support=6, address_width=16)
    verilog(db_dut, filename="addr_gen.sv")
