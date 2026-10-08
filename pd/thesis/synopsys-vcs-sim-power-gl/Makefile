#!/bin/bash

# GATE-LEVEL (post-PnR / signoff) variant of synopsys-vcs-sim-power.
#
# Same idle/active power-test mechanics as synopsys-vcs-sim-power (bitstream
# + PARGS loaded at runtime via the config PORT interface, so they work
# unchanged on a routed netlist), but it simulates the SIGNOFF gate netlist
# instead of the behavioural RTL. Two extra pieces vs. the RTL step:
#   - the ADK standard-cell Verilog models (inputs/adk/*.v) are added to the
#     compile so the mapped cells resolve;
#   - if inputs/design.sdf is present, back-annotate delays with it so the
#     activity (and thus power) reflects real gate timing / glitches.
# The connecting graph node feeds the signoff design.vcs.v in as
# inputs/design.v and the signoff design.sdf in as inputs/design.sdf.

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

# Gate-level extras: pull in the ADK standard-cell models (if the adk input
# is wired) so the synthesized/routed cells link, and back-annotate SDF when
# the signoff step provides it.
# (Not the adk's power-aware *pwr* models: they redefine the same cells with
# supply pins that design.vcs.v doesn't have.)
GL_LIB_FILES := $(filter-out %pwr.v,$(wildcard inputs/adk/*.v))
DESIGN_FILES += $(GL_LIB_FILES)

SDF_ARGS :=
ifneq ($(wildcard inputs/design.sdf),)
    SDF_ARGS := +sdfverbose -sdf typ:tb.dut_gen.dut:inputs/design.sdf
endif

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
compile: COMPILE_ARGS = $(COMPILE_RTL_ARGS) $(SDF_ARGS)
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
