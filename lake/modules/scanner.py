import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import safe_wire, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO

class Scanner(Generator):
    def __init__(self,
                 data_width=16):

        super().__init__("scanner", debug=True)

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
        self._data_in.add_attribute(ControlSignalAttr(is_control=False))

        self._valid_in = self.input("valid_in", 1)
        self._valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ready_out = self.output("ready_out", 1)
        self._ready_out.add_attribute(ControlSignalAttr(is_control=False))
        
        self._data_out = self.output("data_out", self.data_width)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False))

        self._valid_out = self.output("valid_out", 1)
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._ready_in = self.input("ready_in", 1)
        self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._addr_out = self.output("addr_out", self.data_width)
        self._addr_out.add_attribute(ControlSignalAttr(is_control=False))

        self._eos_out = self.output("eos_out", 1)
        self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Intermediates
        self._ren = self.var("ren", 1)

# ==========================================
# Generate addresses to scan over fiber...
# ==========================================

        # Create read address generator
        FIBER_READ_ITER = ForLoop(iterator_support=1,
                                config_width=16)
        FIBER_READ_ADDR = AddrGen(iterator_support=1,
                                config_width=16)

        self.add_child(f"fiber_read_iter",
                        FIBER_READ_ITER,
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        step=self._valid_in)

        # Whatever comes through here should hopefully just pipe through seamlessly
        # addressor modules

        # finished=RF_WRITE_ITER.ports.restart,
        self.add_child(f"fiber_read_addr",
                        FIBER_READ_ADDR,
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        step=self._valid_in,
                        mux_sel=FIBER_READ_ITER.ports.mux_sel_out,
                        restart=FIBER_READ_ITER.ports.restart)
        safe_wire(self, self._addr_out, FIBER_READ_ADDR.ports.addr_out)

# ===================================
# Dump metadata into fifo
# ===================================

        # Stupid convert -
        self._data_in_packed = self.var("fifo_in_packed", self.data_width + 1, packed=True)
        self.wire(self._data_in_packed[self.data_width], FIBER_READ_ITER.ports.restart)
        self.wire(self._data_in_packed[self.data_width - 1, 0], self._data_in)

        self._data_out_packed = self.var("fifo_out_packed", self.data_width + 1, packed=True)
        self.wire(self._eos_out, self._data_out_packed[self.data_width])
        self.wire(self._data_out, self._data_out_packed[self.data_width - 1, 0])

        self._rfifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=8)
        self.add_child(f"coordinate_fifo",
                       self._rfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._valid_in,
                       pop=(self._valid_out & self._ready_in),
                       data_in=self._data_in_packed,
                       data_out=self._data_out_packed,
                       valid=self._valid_out)
        self.wire(self._ready_out, ~self._rfifo.ports.full)

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

    def get_static_bitstream(self,
                                  length):

        # Store all configurations here
        config = []
        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")
        config.append(("scanner_fiber_read_iter_starting_address", 0))
        config.append(("strg_ub_agg_only_agg_write_sched_gen_1_enable", 1))
        config.append(("strg_ub_agg_only_loops_in2buf_1_dimensionality", 1))
        config.append((f"strg_ub_agg_only_loops_in2buf_1_ranges_0", length))
        config.append((f"strg_ub_agg_only_agg_write_addr_gen_1_strides_0", 1))



        # Trim the list
        return trim_config_list(flattened, config)


if __name__ == "__main__":
    scanner_dut = Scanner(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(scanner_dut, filename="scanner.sv",
            optimize_if=False)
