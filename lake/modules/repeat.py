import kratos as kts
from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import add_counter, safe_wire, register, intercept_cfg, observe_cfg, sticky_flag
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class Repeat(Generator):
    def __init__(self,
                 data_width=16):

        super().__init__("Repeat", debug=True)

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

        # Take in a data stream to do repeats (modifier) and a data stream to process

        # REPSIG IN
        self._repsig_data_in = self.input("repsig_data_in", self.data_width, packed=True)
        self._repsig_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._repsig_eos_in = self.input("repsig_eos_in", 1)
        self._repsig_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._repsig_ready_out = self.output("repsig_ready_out", 1)
        self._repsig_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._repsig_valid_in = self.input("repsig_valid_in", 1)
        self._repsig_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        # PROC IN
        self._proc_data_in = self.input("proc_data_in", self.data_width, packed=True)
        self._proc_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._proc_eos_in = self.input("proc_eos_in", 1)
        self._proc_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._proc_ready_out = self.output("proc_ready_out", 1)
        self._proc_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._proc_valid_in = self.input("proc_valid_in", 1)
        self._proc_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        # Data out (right now its a ref...)
        self._ref_data_out = self.output("ref_data_out", self.data_width)
        self._ref_data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._ref_ready_in = self.input("ref_ready_in", 1)
        self._ref_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ref_valid_out = self.output("ref_valid_out", 1)
        self._ref_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._ref_eos_out = self.output("ref_eos_out", 1)
        self._ref_eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Config regs

        # Set the stop token injection level
        self._stop_lvl = self.input("stop_lvl", self.data_width)
        self._stop_lvl.add_attribute(ConfigRegAttr("What level stop tokens should this scanner inject"))


# ==============================
# INPUT FIFO
# ==============================

        # Repsig fifo
        self._repsig_fifo_pop = self.var("repsig_fifo_pop", 1)
        self._repsig_fifo_valid = self.var("repsig_fifo_valid", 1)

        self._repsig_fifo_in = kts.concat(self._repsig_data_in, self._repsig_eos_in)
        self._repsig_in_fifo = RegFIFO(data_width=self._repsig_fifo_in.width, width_mult=1, depth=8)
        self._repsig_fifo_out_data = self.var("repsig_fifo_out_data", self.data_width, packed=True)
        self._repsig_fifo_out_eos = self.var("repsig_fifo_out_eos", 1)

        self.add_child(f"repsig_in_fifo",
                       self._repsig_in_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._repsig_valid_in,
                       pop=self._repsig_fifo_pop,
                       data_in=self._repsig_fifo_in,
                       data_out=kts.concat(self._repsig_fifo_out_data, self._repsig_fifo_out_eos))

        self.wire(self._repsig_ready_out, ~self._repsig_in_fifo.ports.full)
        self.wire(self._repsig_fifo_valid, ~self._repsig_in_fifo.ports.empty)

        # Proc fifo
        self._proc_fifo_pop = self.var("proc_fifo_pop", 1)
        self._proc_fifo_valid = self.var("proc_fifo_valid", 1)

        self._proc_fifo_in = kts.concat(self._proc_data_in, self._proc_eos_in)
        self._proc_in_fifo = RegFIFO(data_width=self._proc_fifo_in.width, width_mult=1, depth=8)
        self._proc_fifo_out_data = self.var("proc_fifo_out_data", self.data_width, packed=True)
        self._proc_fifo_out_eos = self.var("proc_fifo_out_eos", 1)

        self.add_child(f"proc_in_fifo",
                       self._proc_in_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._proc_valid_in,
                       pop=self._proc_fifo_pop,
                       data_in=self._proc_fifo_in,
                       data_out=kts.concat(self._proc_fifo_out_data, self._proc_fifo_out_eos))

        self.wire(self._proc_ready_out, ~self._proc_in_fifo.ports.full)
        self.wire(self._proc_fifo_valid, ~self._proc_in_fifo.ports.empty)


# ==============================
# OUTPUT FIFO
# ==============================

        self._ref_fifo_push = self.var("ref_fifo_push", 1)
        self._ref_fifo_full = self.var("ref_fifo_full", 1)

        self._ref_fifo_in_data = self.var("ref_fifo_in_data", self.data_width)
        self._ref_fifo_in_eos = self.var("ref_fifo_in_eos", 1)

        self._ref_fifo_in = kts.concat(self._ref_fifo_in_data, self._ref_fifo_in_eos)
        self._ref_out_fifo = RegFIFO(data_width=self._ref_fifo_in.width, width_mult=1, depth=8)

        self.add_child(f"ref_out_fifo",
                       self._ref_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._ref_fifo_push,
                       pop=self._ref_ready_in,
                       data_in=self._ref_fifo_in,
                       data_out=kts.concat(self._ref_data_out, self._ref_eos_out))

        self.wire(self._ref_fifo_full, self._ref_out_fifo.ports.full)
        self.wire(self._ref_valid_out, ~self._ref_out_fifo.ports.empty)

# =============================
# Various Logic
# =============================
        # self._seen_root_eos = sticky_flag(self, (self._base_fifo_out_data == 0) & self._base_fifo_out_eos & self._base_fifo_valid, name="seen_root_eos")


# =============================
# Instantiate FSM
# =============================

        self.repeat_fsm = self.add_fsm("repeat_fsm", reset_high=False)

        START = self.repeat_fsm.add_state("START")
        PASS_REPEAT = self.repeat_fsm.add_state("PASS_REPEAT")
        PASS_STOP = self.repeat_fsm.add_state("PASS_STOP")
        DONE = self.repeat_fsm.add_state("DONE")

# =============================
# FSM Transitions
# =============================

        #####################
        # START
        #####################
        START.next(PASS_REPEAT, kts.const(1, 1))

        #####################
        # PASS_REPEAT
        #####################
        PASS_REPEAT.next(PASS_STOP, self._base_fifo_out_eos & self._base_fifo_valid)
        PASS_REPEAT.next(PASS_REPEAT, None)

        #####################
        # PASS_STOP
        #####################
        PASS_STOP.next(DONE, self._seen_root_eos)
        PASS_STOP.next(PASS_REPEAT, self._base_fifo_valid & ~self._base_fifo_out_eos)
        PASS_STOP.next(PASS_STOP, None)

        #####################
        # DONE
        #####################
        DONE.next(DONE, None)

# =============================
# FSM Output Declaration
# =============================
        self.rsg_fsm.output(self._repsig_fifo_in_data)
        self.rsg_fsm.output(self._repsig_fifo_in_eos)
        self.rsg_fsm.output(self._repsig_fifo_push)
        self.rsg_fsm.output(self._base_fifo_pop)

# =============================
# FSM Output Implementation
# =============================
        #####################
        # START
        #####################
        START.output(self._repsig_fifo_in_data, 0)
        START.output(self._repsig_fifo_in_eos, 0)
        START.output(self._repsig_fifo_push, 0)
        START.output(self._base_fifo_pop, 0)

        #####################
        # PASS_REPEAT
        #####################
        PASS_REPEAT.output(self._repsig_fifo_in_data, kts.const(1, 16))
        PASS_REPEAT.output(self._repsig_fifo_in_eos, 0)
        # Push the data on if it is data
        PASS_REPEAT.output(self._repsig_fifo_push, ~self._base_fifo_out_eos & self._base_fifo_valid)
        # Pop the incoming if it is data and there's room in the output fifo
        PASS_REPEAT.output(self._base_fifo_pop, ~self._base_fifo_out_eos & self._base_fifo_valid & ~self._repsig_fifo_full)

        #####################
        # PASS_STOP
        #####################
        PASS_STOP.output(self._repsig_fifo_in_data, self._base_fifo_out_data)
        PASS_STOP.output(self._repsig_fifo_in_eos, 1)
        PASS_STOP.output(self._repsig_fifo_push, self._base_fifo_out_eos & self._base_fifo_valid)
        PASS_STOP.output(self._base_fifo_pop, self._base_fifo_out_eos & self._base_fifo_valid & ~self._repsig_fifo_full)

        #####################
        # DONE
        #####################
        DONE.output(self._repsig_fifo_in_data, 0)
        DONE.output(self._repsig_fifo_in_eos, 0)
        DONE.output(self._repsig_fifo_push, 0)
        DONE.output(self._base_fifo_pop, 0)

        self.rsg_fsm.set_start_state(START)

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

    def get_bitstream(self):

        # Store all configurations here
        config = [("tile_en", 1)]
        return config


if __name__ == "__main__":

    repeat_dut = Repeat(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")
    verilog(repeat_dut, filename="repeat.sv",
            optimize_if=False)
