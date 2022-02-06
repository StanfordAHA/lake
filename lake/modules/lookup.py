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


class Lookup(Generator):
    def __init__(self,
                 data_width=16):

        super().__init__("Lookup", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True

        # For consistency with Core wrapper in garnet...
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
        self._pos_in = self.input("pos_in", self.data_width, packed=True)
        self._pos_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_in = self.input("valid_in", 1)
        self._valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ready_out = self.output("ready_out", 1)
        self._ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._eos_in = self.input("eos_in", 1)
        self._eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._data_out = self.output("data_out", self.data_width)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_out = self.output("valid_out", 1)
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._eos_out = self.output("eos_out", 1)
        self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        self._ready_in = self.input("ready_in", 1)
        self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        # In addition to normal signals, need a way to interact with a memory...
        self._ren = self.output("ren", 1)
        self._ren.add_attribute(ControlSignalAttr(is_control=False))

        self._addr_out = self.output("addr_out", self.data_width)
        self._addr_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._data_in = self.input("data_in", self.data_width)
        self._data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

# ==============================
# INPUT FIFO
# ==============================
        self._infifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=8)

        # Ready is just a function of having room in the FIFO
        self.wire(self._ready_out, ~self._infifo.ports.full)
        # Convert to packed
        self._infifo_in_packed = self.var("infifo_in_packed", self.data_width + 1, packed=True)
        self._infifo_out_packed = self.var("infifo_out_packed", self.data_width + 1, packed=True)

        self._infifo_out_eos = self.var("infifo_out_eos", 1)
        self._infifo_out_valid = self.var("infifo_out_valid", 1)
        self._infifo_out_pos = self.var("infifo_out_pos", self.data_width)

        # indicate valid data as well
        self.wire(self._infifo_in_packed[self.data_width], self._eos_in)
        self.wire(self._infifo_in_packed[self.data_width - 1, 0], self._pos_in)
        self.wire(self._infifo_out_eos, self._infifo_out_packed[self.data_width])
        self.wire(self._infifo_out_pos, self._infifo_out_packed[self.data_width - 1, 0])

        # Push when there's incoming transaction and room to accept it
        self._infifo_push = self.var("infifo_push", 1)
        self.wire(self._infifo_push, self._valid_in)

        # Pop when ready to accum more streams
        self._infifo_pop = self.var("infifo_pop", 1)

        self.add_child(f"input_fifo",
                       self._infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._infifo_push,
                       pop=self._infifo_pop,
                       data_in=self._infifo_in_packed,
                       data_out=self._infifo_out_packed)

        self.wire(self._infifo_out_valid, ~self._infifo.ports.empty)

# ==============================
# OUTPUT FIFO
# ==============================

        self._data_to_fifo = self.var("data_to_fifo", self.data_width)
        self._pe_output = self.var("pe_output", self.data_width)
        self._outfifo_in_eos = self.var("outfifo_in_eos", 1)

        self._outfifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=8, almost_full_diff=1)

        # Convert to packed
        self._outfifo_in_packed = self.var("outfifo_in_packed", self.data_width + 1, packed=True)
        self._outfifo_out_packed = self.var("outfifo_out_packed", self.data_width + 1, packed=True)

        self.wire(self._outfifo_in_packed[self.data_width], self._outfifo_in_eos)
        self.wire(self._outfifo_in_packed[self.data_width - 1, 0], self._data_to_fifo)

        self.wire(self._eos_out, self._outfifo_out_packed[self.data_width])
        self.wire(self._data_out, self._outfifo_out_packed[self.data_width - 1, 0])

        # Push when there's incoming transaction and room to accept it
        self._outfifo_push = self.var("outfifo_push", 1)

        # Pop when ready to accum more streams
        self._outfifo_pop = self.var("outfifo_pop", 1)
        self._outfifo_almost_full = self.var("outfifo_almost_full", 1)

        self.add_child(f"output_fifo",
                       self._outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._outfifo_push,
                       pop=self._outfifo_pop,
                       data_in=self._outfifo_in_packed,
                       data_out=self._outfifo_out_packed)

        self.wire(self._valid_out, ~self._outfifo.ports.empty)

        self.wire(self._outfifo_pop, self._ready_in)
        self.wire(self._outfifo_almost_full, self._outfifo.ports.almost_full)
        # self.wire(self._outfifo_empty, self._outfifo.ports.empty)

# =============================
# Instantiate push/pop logic
# =============================

        # Registered version of ren - basically "was there a read last cycle" - assume
        # we have a single cycle delay SRAM as per usual...
        self._ren_d1 = register(self, self._ren)

        # We pass through the eos token whenever there is no read outstanding
        # and there is room in the output fifo
        self._passthru_eos = self.var("passthru_eos", 1)
        self.wire(self._passthru_eos, ~self._ren_d1 & self._infifo_out_valid & self._infifo_out_eos)

        # We can always make a read to the SRAM every cycle as long
        # as the fifo is not full and we didn't just make a read
        @always_comb
        def fifo_push():
            # Push to the fifo any outstanding reads, else you have an eos
            self._outfifo_push = self._ren_d1 | (self._infifo_out_valid & self._infifo_out_eos)
            # Eos is
            self._outfifo_in_eos = self._passthru_eos
            # Data is either the read data or just the stop token
            self._data_to_fifo = kts.ternary(self._ren_d1, self._data_in, self._infifo_out_pos)
            # Need to use fifo almost full for the pop because of single cycle read delay
            # if there is a read in flight, we need to wait for it
            self._infifo_pop = ((self._infifo_out_valid & ~self._infifo_out_eos & ~self._outfifo_almost_full) |
                                (self._infifo_out_valid & self._infifo_out_eos & ~self._ren_d1))
            # Read enable is if we have a non eos input, but should match the infifo pop
            self._ren = self._infifo_out_valid & ~self._infifo_out_eos & ~self._outfifo_almost_full
            # Address is simply the fifo input
            self._addr_out = self._infifo_out_pos
        self.add_code(fifo_push)

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

    def get_bitstream(self):

        # Store all configurations here
        config = [("tile_en", 1)]
        return config


if __name__ == "__main__":

    lookup_dut = Lookup(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")
    verilog(lookup_dut, filename="lookup.sv",
            optimize_if=False)
