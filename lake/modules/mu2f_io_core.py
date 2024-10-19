from operator import mod
import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import add_counter, safe_wire, register, sticky_flag, transform_strides_and_ranges, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class IOCore_mu2f(Generator):
    def __init__(self,
                 matrix_unit_data_width=16,
                 tile_array_data_width=17,
                 num_ios=2,
                 fifo_depth=2,
                 allow_bypass=True,
                 use_almost_full=False,
                 add_flush=False,
                 add_clk_en=True):

        super().__init__("mu2f_io_core", debug=True)

        self.data_width = tile_array_data_width
        self.add_clk_enable = add_clk_en
        self.add_flush = add_flush
        self.fifo_depth = fifo_depth
        self.allow_bypass = allow_bypass
        self.fifo_name_suffix = "_mu2f_iocore_nof"
        self.use_almost_full = use_almost_full

        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        if self.allow_bypass:
            self._dense_bypass = self.input("dense_bypass", 1)
            self._dense_bypass.add_attribute(ConfigRegAttr("Bypass FIFOS for dense mode..."))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        
        mu_data_width = matrix_unit_data_width

        for io_num in range(num_ios):

            tmp_mu2io = self.input(f"mu2io_{mu_data_width}_{io_num}", mu_data_width, packed=True)
            tmp_mu2io.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            tmp_mu2io_r = self.output(f"mu2io_{mu_data_width}_{io_num}_ready", 1)
            tmp_mu2io_r.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
            tmp_mu2io_v = self.input(f"mu2io_{mu_data_width}_{io_num}_valid", 1)
            tmp_mu2io_v.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

            tmp_io2f = self.output(f"io2f_{tile_array_data_width}_{io_num}", tile_array_data_width, packed=True)
            tmp_io2f.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            tmp_io2f_r = self.input(f"io2f_{tile_array_data_width}_{io_num}_ready", 1)
            tmp_io2f_r.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
            tmp_io2f_v = self.output(f"io2f_{tile_array_data_width}_{io_num}_valid", 1)
            tmp_io2f_v.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

            # mu2io -> io2f fifo
            mu2io_2_io2f_fifo = RegFIFO(data_width=tile_array_data_width,
                                            width_mult=1,
                                            depth=self.fifo_depth,
                                            mod_name_suffix=self.fifo_name_suffix,
                                            almost_full_diff=1)

            self.add_child(f"mu2io_2_io2f_{tile_array_data_width}_{io_num}",
                            mu2io_2_io2f_fifo,
                            clk=self._gclk,
                            rst_n=self._rst_n,
                            clk_en=self._clk_en,
                            push=tmp_mu2io_v,
                            pop=tmp_io2f_r)

            
            
            self.wire(mu2io_2_io2f_fifo.ports.data_in, kts.concat(kts.const(0, 1), tmp_mu2io))

            if self.allow_bypass:
                self.wire(tmp_io2f, kts.ternary(self._dense_bypass,
                                                tmp_mu2io,
                                                mu2io_2_io2f_fifo.ports.data_out))

                if self.use_almost_full:
                    self.wire(tmp_mu2io_r, kts.ternary(self._dense_bypass,
                                                        tmp_io2f_r,
                                                        ~mu2io_2_io2f_fifo.ports.almost_full))
                else:
                    self.wire(tmp_mu2io_r, kts.ternary(self._dense_bypass,
                                                        tmp_io2f_r,
                                                        ~mu2io_2_io2f_fifo.ports.full))

                # self.wire(tmp_io2f_v, ~mu2io_2_io2f_fifo.ports.empty)
                self.wire(tmp_io2f_v, kts.ternary(self._dense_bypass,
                                                    tmp_mu2io_v,
                                                    ~mu2io_2_io2f_fifo.ports.empty))
            else:
                self.wire(tmp_io2f, mu2io_2_io2f_fifo.ports.data_out)
                if self.use_almost_full:
                    self.wire(tmp_mu2io_r, ~mu2io_2_io2f_fifo.ports.almost_full)
                else:
                    self.wire(tmp_mu2io_r, ~mu2io_2_io2f_fifo.ports.full)
                self.wire(tmp_io2f_v, ~mu2io_2_io2f_fifo.ports.empty)

        if self.add_clk_enable:
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))

        if self.add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

    def get_bitstream(self, config_dict):

        # Store all configurations here
        config = [("tile_en", 1)]

        if self.allow_bypass:

            dense_bypass_val = 0

            if 'dense_bypass' in config_dict:
                dense_bypass_val = config_dict['dense_bypass']

            config += [("dense_bypass", dense_bypass_val)]

        return config


if __name__ == "__main__":

    io_core_dut = IOCore_mu2f(tile_array_data_width=17, matrix_unit_data_width=16, allow_bypass=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")
    verilog(io_core_dut, filename="mu2f_iocore.sv",
            optimize_if=False)
