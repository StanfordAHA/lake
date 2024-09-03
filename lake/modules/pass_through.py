from math import e
from struct import pack
import kratos as kts
from kratos import *
from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.passes.passes import lift_config_reg
from lake.utils.util import sticky_flag, trim_config_list, add_counter, register
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_controller import MemoryController
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO
from enum import Enum, unique


class PassThrough(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 add_clk_enable=True,
                 add_flush=False,
                 lift_config=False,
                 defer_fifos=True,
                 perf_debug=True):

        name_str = f"pass_through"
        super().__init__(name=name_str, debug=True)

        self.data_width = data_width
        self.add_clk_enable = add_clk_enable
        self.add_flush = add_flush
        self.lift_config = lift_config
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos
        self.perf_debug = perf_debug

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

        self._stream_in = self.input(f"stream_in", self.data_width + 1, packed=True)
        self._stream_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._stream_in_valid = self.input(f"stream_in_valid", 1)
        self._stream_in_valid.add_attribute(ControlSignalAttr(is_control=True))

        self._stream_in_ready = self.output(f"stream_in_ready", 1)
        self._stream_in_ready.add_attribute(ControlSignalAttr(is_control=False))

        # STREAM OUT.
        self._stream_out = self.output("stream_out", self.data_width + 1, packed=True)
        self._stream_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._stream_out_ready_in = self.input("stream_out_ready", 1)
        self._stream_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._stream_out_valid_out = self.output("stream_out_valid", 1)
        self._stream_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

# ==========================================
# FIFO inputs
# ==========================================

        self._pop_fifo = self.var("pop_fifo", 1)

        # STREAM IN FIFOS
        self._stream_in_fifo = RegFIFO(data_width=self._stream_in.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._stream_in_fifo.add_attribute(SharedFifoAttr(direction="IN"))

        self._stream_in_valid_in = self.var(f"stream_in_fifo_valid_in", 1)

        self._stream_in_fifo_in = self.var(f"stream_in_fifo_in", self.data_width + 1, packed=True)

        self.add_child(f"stream_in_fifo",
                       self._stream_in_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._stream_in_valid,
                       pop=self._pop_fifo,
                       data_in=self._stream_in,
                       data_out=self._stream_in_fifo_in)

        self.wire(self._stream_in_ready, ~self._stream_in_fifo.ports.full)
        self.wire(self._stream_in_valid_in, ~self._stream_in_fifo.ports.empty)  # Filter out unconfigured channels

        # NO perf_debug in RTL


# ==========================================
# FIFO output
# ==========================================

        self._stream_out_fifo = RegFIFO(data_width=self._stream_out.width, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=True)
        self._stream_out_fifo.add_attribute(SharedFifoAttr(direction="OUT"))
        self._stream_out_fifo_ready = self.var("stream_out_fifo_ready", 1)

# ==========================================
# Generate FSM for Intersecting these streams...
# ==========================================

        self._fifo_full = self.var("fifo_full", 1)
        self.wire(self._pop_fifo, (self._stream_out_fifo_ready | self._stream_out_ready_in) & self._stream_in_valid_in) # Not the most performantive
# ===================================
# FIFO output instantiate
# ===================================

        self.add_child(f"stream_out_fifo",
                       self._stream_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._pop_fifo,
                       pop=self._stream_out_ready_in,
                       data_in=self._stream_in_fifo_in,
                       data_out=self._stream_out,
                       full=self._fifo_full)

        self.wire(self._stream_out_valid_out, ~self._stream_out_fifo.ports.empty)
        self.wire(self._stream_out_fifo_ready, ~self._stream_out_fifo.ports.full)

        if self.defer_fifos is False:
            all_fifos = self.get_fifos()
            for child_fifo in all_fifos:
                child_fifo.generate_hardware()

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

        if self.lift_config:
            # Finally, lift the config regs...
            lift_config_reg(self.internal_generator)

    def get_bitstream(self, config_kwargs):
        # Store all configurations here
        config = [("tile_en", 1)]

        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Trim the list
        return trim_config_list(flattened, config)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "pass_through"


if __name__ == "__main__":
    pass_through_dut = PassThrough(data_width=16,
                                   use_merger=False,
                                   defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(pass_through_dut, filename="pass_through.sv",
            optimize_if=False)
