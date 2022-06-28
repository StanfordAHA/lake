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
        self._repsig_data_in = self.input("repsig_data_in", self.data_width + 1, packed=True)
        self._repsig_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._repsig_eos_in = self.var("repsig_eos_in", 1)
        # self.wire(self._repsig_eos_in, self._repsig_data_in[self.data_width])
        # self._repsig_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._repsig_ready_out = self.output("repsig_data_in_ready", 1)
        self._repsig_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._repsig_valid_in = self.input("repsig_data_in_valid", 1)
        self._repsig_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        # PROC IN
        self._proc_data_in = self.input("proc_data_in", self.data_width + 1, packed=True)
        self._proc_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        # self._proc_eos_in = self.input("proc_eos_in", 1)
        # self._proc_eos_in.add_attribute(ControlSignalAttr(is_control=True))

        self._proc_ready_out = self.output("proc_data_in_ready", 1)
        self._proc_ready_out.add_attribute(ControlSignalAttr(is_control=False))

        self._proc_valid_in = self.input("proc_data_in_valid", 1)
        self._proc_valid_in.add_attribute(ControlSignalAttr(is_control=True))

        # Data out (right now its a ref...)
        self._ref_data_out = self.output("ref_data_out", self.data_width + 1, packed=True)
        self._ref_data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._ref_ready_in = self.input("ref_data_out_ready", 1)
        self._ref_ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._ref_valid_out = self.output("ref_data_out_valid", 1)
        self._ref_valid_out.add_attribute(ControlSignalAttr(is_control=False))

        # self._ref_eos_out = self.output("ref_eos_out", 1)
        # self._ref_eos_out.add_attribute(ControlSignalAttr(is_control=False))

        # Config regs

        # Set the stop token injection level
        self._stop_lvl = self.input("stop_lvl", self.data_width)
        self._stop_lvl.add_attribute(ConfigRegAttr("What level stop tokens should this scanner inject"))

        # Root mode
        self._root = self.input("root", 1)
        self._root.add_attribute(ConfigRegAttr("Is this a root repeater"))

# ==============================
# INPUT FIFO
# ==============================

        # Repsig fifo
        self._repsig_fifo_pop = self.var("repsig_fifo_pop", 1)
        self._repsig_fifo_valid = self.var("repsig_fifo_valid", 1)

        self._repsig_fifo_in = kts.concat(self._repsig_data_in)
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
                       data_out=kts.concat(self._repsig_fifo_out_eos, self._repsig_fifo_out_data))

        self.wire(self._repsig_ready_out, ~self._repsig_in_fifo.ports.full)
        self.wire(self._repsig_fifo_valid, ~self._repsig_in_fifo.ports.empty)

        # Proc fifo
        self._proc_fifo_pop = self.var("proc_fifo_pop", 1)
        self._proc_fifo_valid = self.var("proc_fifo_valid", 1)

        self._proc_fifo_push = self.var("proc_fifo_push", 1)
        self._proc_fifo_full = self.var("proc_fifo_full", 1)

        # Need a path to inject into the input fifo for root mode
        self._proc_fifo_inject_push = self.var("proc_fifo_inject_push", 1)
        self._proc_fifo_inject_data = self.var("proc_fifo_inject_data", 16)
        self._proc_fifo_inject_eos = self.var("proc_fifo_inject_eos", 1)

        self.wire(self._proc_fifo_push, kts.ternary(self._root, self._proc_fifo_inject_push, self._proc_valid_in))

        self._proc_fifo_in = kts.ternary(self._root, kts.concat(self._proc_fifo_inject_eos, self._proc_fifo_inject_data), kts.concat(self._proc_data_in))
        self._proc_in_fifo = RegFIFO(data_width=self._proc_fifo_in.width, width_mult=1, depth=8)
        self._proc_fifo_out_data = self.var("proc_fifo_out_data", self.data_width, packed=True)
        self._proc_fifo_out_eos = self.var("proc_fifo_out_eos", 1)

        self.add_child(f"proc_in_fifo",
                       self._proc_in_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._proc_fifo_push,
                       pop=self._proc_fifo_pop,
                       data_in=self._proc_fifo_in,
                       data_out=kts.concat(self._proc_fifo_out_eos, self._proc_fifo_out_data))

        self.wire(self._proc_ready_out, ~self._proc_in_fifo.ports.full)
        self.wire(self._proc_fifo_full, self._proc_in_fifo.ports.full)
        self.wire(self._proc_fifo_valid, ~self._proc_in_fifo.ports.empty)

# ==============================
# OUTPUT FIFO
# ==============================

        self._ref_fifo_push = self.var("ref_fifo_push", 1)
        self._ref_fifo_full = self.var("ref_fifo_full", 1)

        self._ref_fifo_in_data = self.var("ref_fifo_in_data", self.data_width)
        self._ref_fifo_in_eos = self.var("ref_fifo_in_eos", 1)

        self._ref_fifo_in = kts.concat(self._ref_fifo_in_eos, self._ref_fifo_in_data)
        self._ref_out_fifo = RegFIFO(data_width=self._ref_fifo_in.width, width_mult=1, depth=8)

        self.add_child(f"ref_out_fifo",
                       self._ref_out_fifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._ref_fifo_push,
                       pop=self._ref_ready_in,
                       data_in=self._ref_fifo_in,
                       data_out=kts.concat(self._ref_data_out))

        self.wire(self._ref_fifo_full, self._ref_out_fifo.ports.full)
        self.wire(self._ref_valid_out, ~self._ref_out_fifo.ports.empty)

# =============================
# Various Logic
# =============================
        self._seen_root_eos = sticky_flag(self, (self._proc_fifo_out_data == 0) & self._proc_fifo_out_eos & self._proc_fifo_valid, name="seen_root_eos")

# =============================
# Instantiate FSM
# =============================

        self.repeat_fsm = self.add_fsm("repeat_fsm", reset_high=False)

        START = self.repeat_fsm.add_state("START")
        INJECT0 = self.repeat_fsm.add_state("INJECT0")
        INJECT1 = self.repeat_fsm.add_state("INJECT1")
        PASS_REPEAT = self.repeat_fsm.add_state("PASS_REPEAT")
        PASS_STOP = self.repeat_fsm.add_state("PASS_STOP")
        DONE = self.repeat_fsm.add_state("DONE")

# =============================
# FSM Transitions
# =============================

        #####################
        # START
        #####################
        START.next(INJECT0, self._root)
        START.next(PASS_REPEAT, ~self._root)

        #####################
        # INJECT0
        #####################
        INJECT0.next(INJECT1, ~self._proc_fifo_full)
        INJECT0.next(INJECT0, None)

        #####################
        # INJECT1
        #####################
        INJECT1.next(PASS_REPEAT, ~self._proc_fifo_full)
        INJECT1.next(INJECT1, None)

        #####################
        # PASS_REPEAT
        #####################
        # We can go to the PASS_STOP state when the stream being repeated has an actual stop token

        # We go to pass stop when we have a stop token on the repsig line - either to pass through or coalesce with proc stop
        # PASS_REPEAT.next(PASS_STOP, self._proc_fifo_out_eos & self._proc_fifo_valid)
        PASS_REPEAT.next(PASS_STOP, self._repsig_fifo_out_eos & self._repsig_fifo_valid & (self._repsig_fifo_out_data[9, 8] == kts.const(0, 2)))
        PASS_REPEAT.next(PASS_REPEAT, None)

        #####################
        # PASS_STOP
        #####################
        # We are done if we see the root EOS on the proc stream
        # PASS_STOP.next(DONE, self._seen_root_eos)
        # Go to DONE on the done signal
        PASS_STOP.next(DONE, (self._proc_fifo_out_data[9, 8] == kts.const(1, 2)) & self._proc_fifo_valid & self._proc_fifo_out_eos)
        # PASS_STOP.next(DONE, (self._proc_fifo_out_data[9, 8] == kts.const(1, 2)) & self._proc_fifo_valid & self._proc_fifo_out_eos & ~self._ref_fifo_full)
        # If we aren't done, we should just wait for the next valid data as one must come eventually - we have to push either way, so make sure it's
        # not full
        PASS_STOP.next(PASS_REPEAT, self._proc_fifo_valid & ~self._proc_fifo_out_eos & ~self._ref_fifo_full)
        PASS_STOP.next(PASS_STOP, None)

        #####################
        # DONE
        #####################
        DONE.next(START, ~self._ref_fifo_full)

# =============================
# FSM Output Declaration
# =============================

        self.repeat_fsm.output(self._ref_fifo_in_data)
        self.repeat_fsm.output(self._ref_fifo_in_eos)
        self.repeat_fsm.output(self._ref_fifo_push)
        self.repeat_fsm.output(self._proc_fifo_pop)
        self.repeat_fsm.output(self._repsig_fifo_pop)
        self.repeat_fsm.output(self._proc_fifo_inject_push)
        self.repeat_fsm.output(self._proc_fifo_inject_data)
        self.repeat_fsm.output(self._proc_fifo_inject_eos)

# =============================
# FSM Output Implementation
# =============================

        #####################
        # START
        #####################
        START.output(self._ref_fifo_in_data, 0)
        START.output(self._ref_fifo_in_eos, 0)
        START.output(self._ref_fifo_push, 0)
        START.output(self._proc_fifo_pop, 0)
        START.output(self._repsig_fifo_pop, 0)
        START.output(self._proc_fifo_inject_push, 0)
        START.output(self._proc_fifo_inject_data, 0)
        START.output(self._proc_fifo_inject_eos, 0)

        #####################
        # INJECT0
        #####################
        INJECT0.output(self._ref_fifo_in_data, 0)
        INJECT0.output(self._ref_fifo_in_eos, 0)
        INJECT0.output(self._ref_fifo_push, 0)
        INJECT0.output(self._proc_fifo_pop, 0)
        INJECT0.output(self._repsig_fifo_pop, 0)
        INJECT0.output(self._proc_fifo_inject_push, 1)
        INJECT0.output(self._proc_fifo_inject_data, 0)
        INJECT0.output(self._proc_fifo_inject_eos, 0)

        #####################
        # INJECT1
        #####################
        INJECT1.output(self._ref_fifo_in_data, 0)
        INJECT1.output(self._ref_fifo_in_eos, 0)
        INJECT1.output(self._ref_fifo_push, 0)
        INJECT1.output(self._proc_fifo_pop, 0)
        INJECT1.output(self._repsig_fifo_pop, 0)
        INJECT1.output(self._proc_fifo_inject_push, 1)
        # INJECT1.output(self._proc_fifo_inject_data, 0)
        INJECT1.output(self._proc_fifo_inject_data, kts.const(2**8, 16))
        INJECT1.output(self._proc_fifo_inject_eos, 1)

        #####################
        # PASS_REPEAT
        #####################
        # Either injecting the original data or the stop token if the repsig is giving an eos
        # PASS_REPEAT.output(self._ref_fifo_in_data, kts.ternary(self._repsig_fifo_out_eos, self._stop_lvl, self._proc_fifo_out_data))
        PASS_REPEAT.output(self._ref_fifo_in_data, self._proc_fifo_out_data)
        # PASS_REPEAT.output(self._ref_fifo_in_eos, self._repsig_fifo_out_eos)
        PASS_REPEAT.output(self._ref_fifo_in_eos, 0)
        # PASS_REPEAT.output(self._ref_fifo_push, (self._repsig_fifo_valid & self._proc_fifo_valid) | (self._repsig_fifo_valid & self._repsig_fifo_out_eos))
        # We need both inputs to be valid to push out
        PASS_REPEAT.output(self._ref_fifo_push, (self._repsig_fifo_valid & self._proc_fifo_valid) & ~self._repsig_fifo_out_eos)
        # If we are injecting the repsig stop token, then we should simultaneously pop the proc fifo as we are moving to the next data
        # I believe it is guaranteed by construction that the proc fifo HAS to have data on its line, it could never be a stop token on the proc fifo at this point
        # PASS_REPEAT.output(self._proc_fifo_pop, (self._repsig_fifo_valid & self._repsig_fifo_out_eos) & ~self._ref_fifo_full)
        # Just rip the data off once the stop token on the repsig line is hit
        PASS_REPEAT.output(self._proc_fifo_pop, (self._repsig_fifo_valid & self._repsig_fifo_out_eos))
        # Only pop the repsig fifo if there's room in the output fifo and join of input fifos (and not EOS)
        PASS_REPEAT.output(self._repsig_fifo_pop, ~self._ref_fifo_full & (self._repsig_fifo_valid & ~self._repsig_fifo_out_eos) & self._proc_fifo_valid)
        PASS_REPEAT.output(self._proc_fifo_inject_push, 0)
        PASS_REPEAT.output(self._proc_fifo_inject_data, 0)
        PASS_REPEAT.output(self._proc_fifo_inject_eos, 0)

        #####################
        # PASS_STOP
        #####################
        # Here we have a stop on the repsig line - need to either pass it (if data next on proc line) or increment the stop token on the proc line
        # PASS_STOP.output(self._ref_fifo_in_data, self._proc_fifo_out_data)
        # PASS_STOP.output(self._ref_fifo_in_data, kts.ternary(self._proc_fifo_out_eos, self._proc_fifo_out_data + 1, kts.const(0, self.data_width)))
        PASS_STOP.output(self._ref_fifo_in_data, kts.ternary(self._proc_fifo_out_eos & (self._proc_fifo_out_data[9, 8] == kts.const(0, 2)),
                                                             self._proc_fifo_out_data + 1, kts.const(0, self.data_width)))
        # Since it's only stop tokens here, this can be 1
        # PASS_STOP.output(self._ref_fifo_in_eos, self._proc_fifo_out_eos)
        PASS_STOP.output(self._ref_fifo_in_eos, 1)
        # Push once there is valid data on the proc line and there is room - unless it's a DONE token
        PASS_STOP.output(self._ref_fifo_push, self._proc_fifo_valid & ~self._ref_fifo_full & kts.ternary(self._proc_fifo_out_eos, (self._proc_fifo_out_data[9, 8] == kts.const(0, 2)), kts.const(1, 1)))
        # Pop the proc stream only if we get an eos on it
        PASS_STOP.output(self._proc_fifo_pop, ~self._ref_fifo_full & self._proc_fifo_valid & self._proc_fifo_out_eos & (self._proc_fifo_out_data[9, 8] == kts.const(0, 2)))
        # PASS_STOP.output(self._repsig_fifo_pop, 0)
        # Pop the repsig stream once we have the valid on the proc stream
        PASS_STOP.output(self._repsig_fifo_pop, ~self._ref_fifo_full & self._proc_fifo_valid)
        PASS_STOP.output(self._proc_fifo_inject_push, 0)
        PASS_STOP.output(self._proc_fifo_inject_data, 0)
        PASS_STOP.output(self._proc_fifo_inject_eos, 0)

        #####################
        # DONE
        #####################
        DONE.output(self._ref_fifo_in_data, self._proc_fifo_out_data)
        DONE.output(self._ref_fifo_in_eos, 1)
        DONE.output(self._ref_fifo_push, ~self._ref_fifo_full)
        DONE.output(self._proc_fifo_pop, 0)
        DONE.output(self._repsig_fifo_pop, 0)
        DONE.output(self._proc_fifo_inject_push, 0)
        DONE.output(self._proc_fifo_inject_data, 0)
        DONE.output(self._proc_fifo_inject_eos, 0)

        self.repeat_fsm.set_start_state(START)

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

    def get_bitstream(self, stop_lvl=0, root=0):

        # Store all configurations here
        config = [("tile_en", 1),
                  ("stop_lvl", stop_lvl),
                  ("root", root)
                  ]
        return config


if __name__ == "__main__":

    repeat_dut = Repeat(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")
    verilog(repeat_dut, filename="repeat.sv",
            optimize_if=False)
