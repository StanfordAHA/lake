from kratos import *
from functools import reduce
import operator
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


class AddrGen(Generator):
    '''
    Generate addresses for a single port
    '''
    def __init__(self,
                 iterator_support=6,
                 config_width=16,
                 dual_config=False,
                 iterator_support2=2):

        if dual_config:
            super().__init__(f"addr_gen_dual_config_{iterator_support}_{iterator_support2}_{config_width}", debug=True)
        else:
            super().__init__(f"addr_gen_{iterator_support}_{config_width}", debug=True)

        # Store local...
        self.iterator_support = iterator_support
        self.config_width = config_width
        self.dual_config = dual_config
        self.iterator_support2 = iterator_support2
        self.max_iterator_support = max(self.iterator_support, self.iterator_support2)
        self.iter_idx_w = max(clog2(self.iterator_support), 1)
        self.iter2_idx_w = max(clog2(self.iterator_support2), 1)

        # PORT DEFS: begin

        # INPUTS
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._restart = self.input("restart", 1)

        self._strides = self.input("strides", self.config_width,
                                   size=self.iterator_support,
                                   packed=True, explicit_array=True)
        self._strides.add_attribute(ConfigRegAttr("Strides of address generator"))
        self._strides.add_attribute(FormalAttr(f"{self._strides.name}", FormalSignalConstraint.SOLVE))

        self._starting_addr = self.input("starting_addr", self.config_width)
        self._starting_addr.add_attribute(ConfigRegAttr("Starting address of address generator"))
        self._starting_addr.add_attribute(FormalAttr(f"{self._starting_addr.name}", FormalSignalConstraint.SOLVE))

        if self.dual_config:
            self._strides2 = self.input("strides2", self.config_width,
                                        size=self.iterator_support2,
                                        packed=True, explicit_array=True)
            self._strides2.add_attribute(ConfigRegAttr("Strides of address generator"))
            self._strides2.add_attribute(FormalAttr(f"{self._strides2.name}", FormalSignalConstraint.SOLVE))

            self._starting_addr2 = self.input("starting_addr2", self.config_width)
            self._starting_addr2.add_attribute(ConfigRegAttr("Starting address of address generator"))
            self._starting_addr2.add_attribute(FormalAttr(f"{self._starting_addr2.name}", FormalSignalConstraint.SOLVE))

            self._mux_sel_msb_init = self.output("mux_sel_msb_init", 1)
            self.wire(self._mux_sel_msb_init, self._starting_addr2 < self._starting_addr)

        self._step = self.input("step", 1)

        if self.dual_config:
            self._mux_sel = self.input("mux_sel", max(clog2(self.max_iterator_support) + 1, 1))
        else:
            self._mux_sel = self.input("mux_sel", max(clog2(self.iterator_support), 1))

        # OUTPUTS
        # TODO why is this config width instead of address width?
        self._addr_out = self.output("addr_out", self.config_width)

        # PORT DEFS: end

        # LOCAL VARIABLES: begin
        self._strt_addr = self.var("strt_addr", self.config_width)

        self._calc_addr = self.var("calc_addr", self.config_width)

        self._max_value = self.var("max_value", self.iterator_support)

        if self.dual_config:
            self._cur_stride = self.var("cur_stride", self.config_width)
            self._mux_sel_msb = self.var("mux_sel_msb", 1)
            self._mux_sel_iter1 = self.var("mux_sel_iter1", self.iter_idx_w)
            self._mux_sel_iter2 = self.var("mux_sel_iter2", self.iter2_idx_w)
            self.wire(self._mux_sel_iter1, self._mux_sel[self.iter_idx_w - 1, 0])
            self.wire(self._mux_sel_iter2, self._mux_sel[self.iter2_idx_w - 1, 0])

        # LOCAL VARIABLES: end
        # GENERATION LOGIC: begin
        if self.dual_config:
            self.wire(self._mux_sel_msb, self._mux_sel[self._mux_sel.width - 1])
            self.wire(self._strt_addr, ternary(self._mux_sel_msb, self._starting_addr2, self._starting_addr))
            self.wire(self._cur_stride, ternary(self._mux_sel_msb, self._strides2[self._mux_sel_iter2], self._strides[self._mux_sel_iter1]))
        else:
            self.wire(self._strt_addr, self._starting_addr)
        self.wire(self._addr_out, self._calc_addr)

        self._current_addr = self.var("current_addr", self.config_width)
        # Calculate address by taking previous calculation and adding the muxed stride
        self.wire(self._calc_addr, self._strt_addr + self._current_addr)

        self.add_code(self.calculate_address)
        # GENERATION LOGIC: end

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def calculate_address(self):
        if ~self._rst_n:
            self._current_addr = 0
        elif self._step:
            # mux_sel as 0 but update means that the machine is resetting.
            if self._restart:
                self._current_addr = 0
            else:
                if self.dual_config:
                    self._current_addr = self._current_addr + self._cur_stride
                else:
                    self._current_addr = self._current_addr + self._strides[self._mux_sel]


if __name__ == "__main__":
    db_dut = AddrGen(iterator_support=6)
    verilog(db_dut, filename="addr_gen.sv")
