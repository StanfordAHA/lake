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


class PE(Generator):
    def __init__(self,
                 data_width=16):

        super().__init__("PE", debug=True)

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
        self._data_in = self.input("data_in", self.data_width, size=2, explicit_array=True, packed=True)
        self._data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_in = self.input("valid_in", 2)
        self._valid_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ready_out = self.output("ready_out", 2)
        self._ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._eos_in = self.input("eos_in", 2)
        self._eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._data_out = self.output("data_out", self.data_width)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_out = self.output("valid_out", 1)
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._eos_out = self.output("eos_out", 1)
        self._eos_out.add_attribute(ControlSignalAttr(is_control=False))

        self._ready_in = self.input("ready_in", 1)
        self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

# ==============================
# INPUT FIFO
# ==============================
        self._infifo = []
        self._infifo.append(RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=8))
        self._infifo.append(RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=8))

        # Ready is just a function of having room in the FIFO
        self.wire(self._ready_out[0], ~self._infifo[0].ports.full)
        self.wire(self._ready_out[1], ~self._infifo[1].ports.full)

        # Convert to packed
        self._infifo_in_packed = self.var("infifo_in_packed", self.data_width + 1, size=2, explicit_array=True, packed=True)
        self._infifo_out_packed = self.var("infifo_out_packed", self.data_width + 1, size=2, explicit_array=True, packed=True)

        self._infifo_out_eos = self.var("infifo_out_eos", 2)
        self._infifo_out_valid = self.var("infifo_out_valid", 2)
        self._infifo_out_data = self.var("infifo_out_data", self.data_width, size=2, explicit_array=True, packed=True)

        # indicate valid data as well
        self.wire(self._infifo_in_packed[0][self.data_width], self._eos_in[0])
        self.wire(self._infifo_in_packed[0][self.data_width - 1, 0], self._data_in[0])
        self.wire(self._infifo_out_eos[0], self._infifo_out_packed[0][self.data_width])
        self.wire(self._infifo_out_data[0], self._infifo_out_packed[0][self.data_width - 1, 0])

        self.wire(self._infifo_in_packed[1][self.data_width], self._eos_in[1])
        self.wire(self._infifo_in_packed[1][self.data_width - 1, 0], self._data_in[1])
        self.wire(self._infifo_out_eos[1], self._infifo_out_packed[1][self.data_width])
        self.wire(self._infifo_out_data[1], self._infifo_out_packed[1][self.data_width - 1, 0])

        # Push when there's incoming transaction and room to accept it
        self._infifo_push = self.var("infifo_push", 2)
        # self.wire(self._infifo_push, (self._valid_in | self._eos_in) & (~self._infifo.ports.full))
        self.wire(self._infifo_push[0], self._valid_in[0])
        self.wire(self._infifo_push[1], self._valid_in[1])

        # Pop when ready to accum more streams
        self._infifo_pop = self.var("infifo_pop", 2)

        for i in range(2):
            self.add_child(f"input_fifo_{i}",
                        self._infifo[i],
                        clk=self._gclk,
                        rst_n=self._rst_n,
                        clk_en=self._clk_en,
                        push=self._infifo_push[i],
                        pop=self._infifo_pop[i],
                        data_in=self._infifo_in_packed[i],
                        data_out=self._infifo_out_packed[i])
                    #    valid=self._infifo_valid_entry)

        self.wire(self._infifo_out_valid[0], ~self._infifo[0].ports.empty)
        self.wire(self._infifo_out_valid[1], ~self._infifo[1].ports.empty)

# ==============================
# OUTPUT FIFO
# ==============================

        self._data_to_fifo = self.var("data_to_fifo", self.data_width) 
        self._pe_output = self.var("pe_output", self.data_width) 
        self._outfifo_in_eos = self.var("outfifo_in_eos", 1)

        self._outfifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=8)

        # Convert to packed
        self._outfifo_in_packed = self.var("outfifo_in_packed", self.data_width + 1, packed=True)
        self._outfifo_out_packed = self.var("outfifo_out_packed", self.data_width + 1, packed=True)

        self.wire(self._outfifo_in_packed[self.data_width], self._outfifo_in_eos)
        self.wire(self._outfifo_in_packed[self.data_width - 1, 0], self._data_to_fifo)

        # self.wire(self._outfifo_out_valid, self._outfifo_out_packed[self.data_width + 1])
        self.wire(self._eos_out, self._outfifo_out_packed[self.data_width])
        self.wire(self._data_out, self._outfifo_out_packed[self.data_width - 1, 0])

        # Push when there's incoming transaction and room to accept it
        self._outfifo_push = self.var("outfifo_push", 1)

        # Pop when ready to accum more streams
        self._outfifo_pop = self.var("outfifo_pop", 1)
        self._outfifo_full = self.var("outfifo_full", 1)
        # self._outfifo_empty = self.var("outfifo_empty", 1)

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
        self.wire(self._outfifo_full, self._outfifo.ports.full)
        # self.wire(self._outfifo_empty, self._outfifo.ports.empty)

# =============================
# Instantiate actual PE
# =============================

        self._execute_op = self.var("execute_op", 1)
        self.wire(self._execute_op, self._infifo_out_valid.r_and() & ~self._infifo_out_eos.r_or() & ~self._outfifo_full)

        self._op = self.input("op", 1)
        self._op.add_attribute(ConfigRegAttr("Operation"))

        @always_comb
        def PE_comb():
            if self._execute_op:
                # ADD
                if self._op == 0:
                    self._pe_output = self._infifo_out_data[0] + self._infifo_out_data[1]
                # MUL
                elif self._op == 1:
                    self._pe_output = self._infifo_out_data[0] * self._infifo_out_data[1]
                else:
                    self._pe_output = 0
            else:
                self._pe_output = 0
        self.add_code(PE_comb)

        @always_comb
        def fifo_push():
            self._outfifo_push = 0
            self._outfifo_in_eos = 0
            self._data_to_fifo = 0
            self._infifo_pop[0] = 0
            self._infifo_pop[1] = 0
            # If both inputs are valid, then we either can perform the op, otherwise we push through EOS
            if self._infifo_out_valid.r_and() & ~self._outfifo_full:
                # if eos's are low, we push through pe output, otherwise we push through the input data (streams are aligned)
                if self._infifo_out_eos.r_and():
                    self._outfifo_push = 1
                    self._outfifo_in_eos = 0
                    self._data_to_fifo = self._pe_output
                    self._infifo_pop[0] = 1
                    self._infifo_pop[1] = 1
                else:
                    self._outfifo_push = 1
                    self._outfifo_in_eos = 1
                    self._data_to_fifo = self._infifo_out_data[0]
                    self._infifo_pop[0] = 1
                    self._infifo_pop[1] = 1
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

    def get_bitstream(self, op):

        # Store all configurations here
        config = [("tile_en", 1),
                  ("op", op)]
        return config


if __name__ == "__main__":

    pe_dut = PE(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(pe_dut, filename="pe.sv",
            optimize_if=False)
