#!/bin/bash

# -------------------------------------------------------------------
# Variables
# -------------------------------------------------------------------
TOOL ?= XCELIUM
export WAVEFORM ?= 0
export BITSTREAM_SIZE ?= 0

# -------------------------------------------------------------------
# Compile Parameters
# -------------------------------------------------------------------
CLK_PERIOD ?= 1ns

DESIGN_FILES += tb.sv \
				inputs/lakespec.sv

XRUN = xrun \
	   -64bit \
	   -sv \
	   -timescale 1ns/1ns \
	   -debug \
	   -sysv \
	   -top tb \
	   -elaborate \
	   -l xrun.log \
	   -covoverwrite \
	   +maxdelays \
	   -access r \
	   -notimingchecks \
	   $(COMPILE_ARGS) \
	   $(INPUT_ARGS)

# -------------------------------------------------------------------
# Compile & Run
# -------------------------------------------------------------------
COMPILE_RTL_ARGS += +define+CLK_PERIOD=$(CLK_PERIOD)
COMPILE_GLS_ARGS += +define+CLK_PERIOD=$(CLK_PERIOD)
ifeq ($(TOOL), XCELIUM)
    COMPILE = $(XRUN)
    RUN = xrun -R
else
    @echo "TOOL must be XCELIUM"
endif

ifeq ($(TOOL), XCELIUM)
	DUMP_ARGS = -input run_sim.tcl
else ifeq ($(TOOL), VCS)
	DUMP_ARGS = -ucli -i dump_fsdb.tcl
endif

.PHONY: compile
compile: COMPILE_ARGS = $(COMPILE_RTL_ARGS)
compile: INPUT_ARGS = $(DESIGN_FILES) $(TB_FILES) $(IP_FILES)
compile:
	$(COMPILE) $(shell cat "./inputs/comp_args.txt")

.PHONY: run
run:
	$(RUN) $(DUMP_ARGS) $(RUN_ARGS) $(APP_ARGS) +TEST_DIRECTORY=./ $(shell cat "./inputs/PARGS.txt")

.PHONY: compare
compare:
	python test_comparison.py --dir ./

.PHONY: sim
sim: compile run compare

# -------------------------------------------------------------------
# Clean
# -------------------------------------------------------------------
.PHONY: clean
clean:
	rm -rf simv.daidir
