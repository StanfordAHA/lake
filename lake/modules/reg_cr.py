import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import add_counter, safe_wire, register, intercept_cfg, observe_cfg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class Reg(Generator):
    def __init__(self,
                 data_width=16):

        super().__init__("reg_cr", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True

        self.total_sets = 0

        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # Scanner interface will need
        # input data, input valid
        # output address, output valid
        self._data_in = self.input("data_in", self.data_width)
        self._data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._write_en = self.input("write_en", 1)
        self._write_en.add_attribute(ControlSignalAttr(is_control=True))

        self._data_out = self.output("data_out", self.data_width)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

# =============================
# SCAN FSM
# =============================

        self._reg_d_out = self.var("reg_d_out", self.data_width)
        self.wire(self._data_out, self._reg_d_out)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def reg_ff():
            if ~self._rst_n:
                self._reg_d_out = 0
            elif self._write_en:
                self._reg_d_out = self._data_in
        self.add_code(reg_ff)

        # Force FSM realization first so that flush gets added...
        kts.passes.realize_fsm(self.internal_generator)

        if self.add_clk_enable:
            # self.clock_en("clk_en")
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))

        if self.add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

        # Finally, lift the config regs...
        lift_config_reg(self.internal_generator)

    def get_bitstream(self, inner_offset, max_out, outer_stride, outer_range):

        # Store all configurations here
        config = [("inner_dim_offset", inner_offset),
                  ("max_outer_dim", max_out),
                  ("fiber_outer_iter_dimensionality", 1),
                  ("fiber_outer_iter_ranges_0", outer_range - 2),
                  ("fiber_outer_addr_strides_0", outer_stride),
                  ("fiber_outer_addr_starting_addr", 0)]
        return config


if __name__ == "__main__":
    reg_dut = Reg(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(reg_dut, filename="reg.sv",
            optimize_if=False)
