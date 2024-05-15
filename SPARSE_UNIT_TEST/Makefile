#!/bin/bash

# -------------------------------------------------------------------
# Variables
# -------------------------------------------------------------------
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(dir $(mkfile_path))
module_dir := $(mkfile_dir)/modules
# garnet_dir := $(mkfile_dir)/../..
# gemstone_dir := $(realpath $(garnet_dir))/../gemstone
# netlist_dir := $(mkfile_dir)/netlist
# glc_dir := $(garnet_dir)/global_controller
# APP_ARGS ?= +APP0=app0

TOOL ?= VCS
TEST_TAR ?= 0
TOP ?= 0
TEST_UNIT ?= 0
INST ?= 0
NUM_INPUTS ?= 0
FIBER_ACCESS_ROOT ?= 0

export WAVEFORM ?= 0
# export WAVEFORM_GLB_ONLY ?= 0
# export SAIF ?= 0

# -------------------------------------------------------------------
# Compile Parameters
# -------------------------------------------------------------------
CLK_PERIOD ?= 1ns
TX_NUM_GLB ?= 1

DESIGN_FILES += $(TEST_TAR) \
				tile_read.sv \
				tile_write.sv\
				glb_read.sv \
				glb_write.sv\
				sram_sp.sv\
				behavioral_pe_add.sv\
				-y /cad/cadence/GENUS_19.10.000_lnx86/share/synth/lib/chipware/sim/verilog/CW/ \
				+libext+.v+.sv 

DESIGN_FILES += $(module_dir)/$(TEST_UNIT)
# TB_FILES += -F tb/tb_cgra.f

# IP_FILES += -y /cad/cadence/GENUS_19.10.000_lnx86/share/synth/lib/chipware/sim/verilog/CW/ \
# 			-y /cad/cadence/GENUS_19.10.000_lnx86/share/synth/lib/chipware/sim/verilog/CWTECH/ \
# 			+libext+.v+.sv 

# -------------------------------------------------------------------
# GLS Parameters
# -------------------------------------------------------------------
# GLB_TOP ?= glb_top
# GLB_TILE ?= glb_tile
# TILE_ARRAY ?= tile_array
# TILE_PE ?= Tile_PE
# TILE_MEM ?= Tile_MemCore
# NETLIST_FILES ?= -v $(garnet_dir)/garnet.v -v $(netlist_dir)/glb_top.vcs.v -v $(netlist_dir)/glb_tile.vcs.v \
# 				 -v $(netlist_dir)/global_controller.vcs.v -v $(netlist_dir)/tile_array.vcs.v -v $(netlist_dir)/Tile_PE.vcs.v -v $(netlist_dir)/Tile_MemCore.vcs.v \
# 				 -v $(netlist_dir)/sram.v -v $(netlist_dir)/tile_array.sram.v -v $(netlist_dir)/stdcells.v -v $(netlist_dir)/stdcells-lvt.v -v $(netlist_dir)/stdcells-ulvt.v -v $(netlist_dir)/stdcells-pm.v

# -------------------------------------------------------------------
# Run Parameters
# -------------------------------------------------------------------
# RUN_ARGS ?=
# RUN_LOG ?= run.log

# -------------------------------------------------------------------
# Command
# -------------------------------------------------------------------


WAVEFORM_ARGS = -debug_access+all -kdb +vpi +memcbk +vcsd


# Picked XRUN by a "coin"
VCS = vcs \
	  -sverilog \
	  $(TIMESCALE) \
	  -full64 \
	  -ldflags "-Wl,--no-as-needed" \
	  -CFLAGS "-m64" \
	  -top $(TOP) \
	  +vcs+lic+wait \
	  +vcs+initreg+random \
	  +overlap \
	  +v2k \
	  -l vcs.log \
	  $(WAVEFORM_ARGS) \
	  $(COMPILE_ARGS) \
	  $(INPUT_ARGS)

XRUN = xrun \
	   -64bit \
	   -sv \
	   -timescale 1ns/1ps \
	   -debug \
	   -sysv \
	   -top $(TOP) \
	   -elaborate \
	   -l xrun.log \
	   -covoverwrite \
	   +maxdelays \
	   -notimingchecks \
	   $(COMPILE_ARGS) \
	   $(INPUT_ARGS)

# -------------------------------------------------------------------
# C API
# -------------------------------------------------------------------
# .PHONY: libcgra.so
# libcgra.so: $(shell find lib -type f) $(garnet_dir)/global_buffer/header/global_buffer_param.h $(garnet_dir)/global_buffer/header/glb.h $(garnet_dir)/global_controller/header/glc.h
# 	gcc -Wno-error -Wall lib/*.c -I$(garnet_dir)/global_buffer/header -I$(garnet_dir)/global_controller/header -shared -o libcgra.so -fPIC

# -------------------------------------------------------------------
# Compile & Run
# -------------------------------------------------------------------
COMPILE_RTL_ARGS += +define+CLK_PERIOD=$(CLK_PERIOD) \
					+define+TX_NUM_GLB=$(TX_NUM_GLB) \
					+define+FIBER_ACCESS_ROOT=$(FIBER_ACCESS_ROOT)

COMPILE_GLS_ARGS += +define+CLK_PERIOD=$(CLK_PERIOD) \
					+define+TX_NUM_GLB=$(TX_NUM_GLB)
ifeq ($(TOOL), XCELIUM)
    COMPILE = $(XRUN)
    # COMPILE_RTL_ARGS += -xminitialize 0 -xminit_log init.log -nospecify
    # COMPILE_GLS_ARGS += -xminitialize 0 -xminit_log init.log
    # COMPILE_GLS_ARGS += -ALLOWREDEFINITION
    RUN = xrun -R -l $(RUN_LOG) -sv_lib libcgra.so
else ifeq ($(TOOL), VCS)
    COMPILE = $(VCS)
    # COMPILE_RTL_ARGS += +nospecify
    #RUN = ./simv +inst="'h4_8000_0400_0100_0040_0000" -lca -l $(RUN_LOG) +vcs+initmem+0 +vcs+initreg+0 -exitstatus
    RUN = ./simv $(INST) $(NUM_INPUTS) -lca -l $(RUN_LOG) +vcs+initmem+0 +vcs+initreg+0 -exitstatus
else
    @echo "TOOL must be either XCELIUM or VCS"
endif

ifeq ($(TOOL), XCELIUM)
	DUMP_ARGS = -input unit_test_shm.tcl
else ifeq ($(TOOL), VCS)
	DUMP_ARGS = -ucli -i unit_test_fsdb.tcl
endif

TEST_DIR ?= $(mkfile_dir)/TEST_DIR

.PHONY: compile
compile: COMPILE_ARGS = $(COMPILE_RTL_ARGS)
compile: INPUT_ARGS = $(DESIGN_FILES) $(TB_FILES) $(IP_FILES)
compile:
	$(COMPILE)

.PHONY: run
run:
	rm -f $(TEST_DIR)/OUTPUT_DIR/*
	$(RUN) $(DUMP_ARGS)
# $(RUN) $(DUMP_ARGS) $(RUN_ARGS) $(APP_ARGS)

# -------------------------------------------------------------------
# GLS Compile
# -------------------------------------------------------------------
# compile testbench of garnet with xcelium
.PHONY: compile-gls
compile-gls: COMPILE_GLS_ARGS += +define+NON_STOP_IF_INPUT_Z
compile-gls: COMPILE_GLS_ARGS += +define+TSMC_CM_NO_WARNING
compile-gls: COMPILE_GLS_ARGS += +define+TSMC_CM_UNIT_DELAY
compile-gls: COMPILE_GLS_ARGS += +define+TSMC_INITIALIZE_MEM_USING_DEFAULT_TASKS
compile-gls: COMPILE_GLS_ARGS += +define+TSMC_MEM_LOAD_0
compile-gls: COMPILE_GLS_ARGS += -negdelay
compile-gls: COMPILE_ARGS = $(COMPILE_GLS_ARGS)
compile-gls: INPUT_ARGS = $(NETLIST_FILES) $(TB_FILES) $(IP_FILES)
compile-gls:
	@rm -rf $(SDF_LOG); mkdir $(SDF_LOG)
	$(COMPILE)


# -------------------------------------------------------------------
# Clean
# -------------------------------------------------------------------
.PHONY: clean
clean:
	rm -rf xrun.log xrun.history xcelium.d simv simv.daidir csrc vcs.log cgra.shm cgra.fsdb sdf_stats.txt sdf_logs

.PHONY: sim
# sim: libcgra.so compile run
sim: clean compile run
