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
                 optimization_lvl=2):

        super().__init__(f"addr_gen_{iterator_support}_{config_width}", debug=True)

        # Store local...
        self.iterator_support = iterator_support
        self.config_width = config_width

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

        self._step = self.input("step", 1)

        # OUTPUTS
        # TODO why is this config width instead of address width?
        self._addr_out = self.output("addr_out", self.config_width)

        self._strt_addr = self.var("strt_addr", self.config_width)
        self._calc_addr = self.var("calc_addr", self.config_width)

        self.wire(self._addr_out, self._calc_addr)
        self.wire(self._strt_addr, self._starting_addr)
        # PORT DEFS: end

        self._current_addr = self.var("current_addr", self.config_width)
        # Calculate address by taking previous calculation and adding the muxed stride
        # self.wire(self._calc_addr, self._strt_addr + self._current_addr)

        if optimization_lvl == 0:
            self._dim_counter_in = self.input("dim_counter_in", self.config_width,
                                              size=self.iterator_support,
                                              packed=True,
                                              explicit_array=True)

            self._local_addr = self.var("local_addr", self.config_width, size=self.iterator_support,
                                        packed=True,
                                        explicit_array=True)

            @always_comb
            def calculate_address(idx):
                self._local_addr[idx] = self._dim_counter_in[idx] * self._strides[idx]
            for i in range(self.iterator_support):
                self.add_code(calculate_address, idx=i)

            # Create a reduction sum of all the local_addr
            red_sum = self._strt_addr
            for i in range(self.iterator_support):
                red_sum = red_sum + self._local_addr[i]
            self.add_stmt(self._calc_addr.assign(red_sum))

        elif optimization_lvl == 1:
            self._inc_in = self.input("inc_in", self.iterator_support)
            self._clr_in = self.input("clr_in", self.iterator_support)
            self._local_addr = self.var("local_addr", self.config_width,
                                        size=self.iterator_support,
                                        packed=True,
                                        explicit_array=True)

            @always_ff((posedge, "clk"), (negedge, "rst_n"))
            def calculate_address(idx):
                if ~self._rst_n:
                    self._local_addr[idx] = 0
                elif self._step:
                    # mux_sel as 0 but update means that the machine is resetting.
                    if self._clr_in[idx]:
                        self._local_addr[idx] = 0
                    elif self._inc_in[idx]:
                        self._local_addr[idx] = self._local_addr[idx] + self._strides[idx]
            for i in range(self.iterator_support):
                self.add_code(calculate_address, idx=i)

            # Create a reduction sum of all the local_addr
            red_sum = self._strt_addr
            for i in range(self.iterator_support):
                red_sum = red_sum + self._local_addr[i]
            self.add_stmt(self._calc_addr.assign(red_sum))

        # Takes mux_sel in as well as step
        elif optimization_lvl == 2:
            self._mux_sel = self.input("mux_sel", max(clog2(self.iterator_support), 1))

            @always_ff((posedge, "clk"), (negedge, "rst_n"))
            def calculate_address():
                if ~self._rst_n:
                    self._current_addr = 0
                elif self._step:
                    # mux_sel as 0 but update means that the machine is resetting.
                    if self._restart:
                        self._current_addr = 0
                    else:
                        self._current_addr = self._current_addr + self._strides[self._mux_sel]
            self.add_code(calculate_address)
            self.wire(self._calc_addr, self._strt_addr + self._current_addr)


if __name__ == "__main__":
    for opt_level in range(3):
        db_dut = AddrGen(iterator_support=6, config_width=16, optimization_lvl=opt_level)
        verilog(db_dut, filename=f"addr_gen_opt_{opt_level}.sv")
