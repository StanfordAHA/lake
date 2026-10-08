#!/bin/bash

# Stripped-down VCS sim Makefile for the idle-power / active-power tests.
# Since 2026-10-07 it simulates the SYNTHESIZED netlist (the construct feeds
# synth's design.v), not the RTL: ptpx-synth powers that netlist, and an RTL
# sim's activity only matches its flops/ports by name (a few % of nets).
# The same bitstream, PARGS and input stream drive it; zero-delay (no SDF).
# Differences vs. pd/thesis/synopsys-vcs-sim-rtl/Makefile:
#   - `sim` target does compile + run only (no test_comparison.py compare
#     step — power tests have no gold to check).
#   - The bitstream + PARGS are variant-specific: the connecting graph
#     node hands us either bitstream.idle.bs / PARGS.idle.txt or the
#     .active variants. We rename the incoming variant to
#     bitstream.bs / PARGS.txt before invoking VCS so the shared tb.sv
#     can locate them the same way it does in the existing sim flows.

TOOL ?= VCS
export WAVEFORM = 1
export BITSTREAM_SIZE ?= 0

# Always dump VCD for power tests — that's the whole point of running
# this Makefile. Downstream vcd2saif expects outputs/run.vcd populated.
DUMP_VCD ?= 1

CLK_PERIOD ?= 1ns

DESIGN_FILES += tb.sv \
                inputs/design.v \
                inputs/sram.v

# Standard-cell models for the netlist. The adk's power-aware *pwr* models
# redefine the same cells with supply pins the netlist doesn't have.
GL_LIB_FILES := $(filter-out %pwr.v,$(wildcard inputs/adk/*.v))
DESIGN_FILES += $(GL_LIB_FILES)

# Zero-delay: no SDF at synthesis, so skip specify delays and timing checks
# (a check firing on an ideal-clock edge would X the flop).
GL_ZERO_DELAY_VCS  = +nospecify +notimingcheck
GL_ZERO_DELAY_XRUN = -nospecify

VCS = vcs \
      -sverilog \
      -timescale=1ns/1ns \
      -full64 \
      -top tb \
      +vcs+lic+wait \
      +v2k \
      -l vcs.log \
      $(COMPILE_ARGS) \
      $(INPUT_ARGS)

XRUN = xrun \
       -64bit \
       -sv \
       -timescale 1ns/1ns \
       -debug \
       -sysv \
       -top tb \
       -elaborate \
       -licqueue \
       -l xrun.log \
       -covoverwrite \
       +maxdelays \
       -access r \
       -notimingchecks \
       $(COMPILE_ARGS) \
       $(INPUT_ARGS)

COMPILE_RTL_ARGS += +define+CLK_PERIOD=$(CLK_PERIOD)
ifeq ($(TOOL), XCELIUM)
    COMPILE = $(XRUN)
    RUN = xrun -R -licqueue
else ifeq ($(TOOL), VCS)
    COMPILE = $(VCS)
    RUN = ./simv -exitstatus
else
    @echo "TOOL must be XCELIUM or VCS"
endif

ifeq ($(TOOL), XCELIUM)
    DUMP_ARGS = -input run_sim.tcl
else ifeq ($(TOOL), VCS)
    DUMP_ARGS =
endif

# `variant` is set by mflowgen (idle | active). configure.yml's commands
# copy the correct variant files into inputs/bitstream.bs + inputs/PARGS.txt
# before invoking make sim.

.PHONY: compile
ifeq ($(TOOL), XCELIUM)
compile: COMPILE_ARGS = $(COMPILE_RTL_ARGS) $(GL_ZERO_DELAY_XRUN)
else
compile: COMPILE_ARGS = $(COMPILE_RTL_ARGS) $(GL_ZERO_DELAY_VCS)
endif
compile: INPUT_ARGS = $(DESIGN_FILES) $(TB_FILES) $(IP_FILES)
compile:
	$(COMPILE) $(shell cat "./inputs/comp_args.txt")

.PHONY: run
run:
	$(RUN) $(DUMP_ARGS) $(RUN_ARGS) +TEST_DIRECTORY=./ +dump_vcd=$(DUMP_VCD) $(shell cat "./inputs/PARGS.txt")
	@echo "PASS"

.PHONY: sim
sim: compile run

.PHONY: clean
clean:
	rm -rf simv.daidir vcs.log
