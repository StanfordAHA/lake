#!/bin/bash
# Smoke test: 4 configs covering known-passing + gen_sram regression targets.
#
# Usage:
#   bash ASPLOS_EXP/smoke_test.sh                  # default build root: /tmp/SMOKE_BUILDS
#   bash ASPLOS_EXP/smoke_test.sh /path/to/builds  # override build root
#
# Prereqs: ensure the EDA toolchain is loaded before running, e.g.
#   module load base && module load vcs
# (genus + vcs + ADKs must be available; this script just materializes the
# mflowgen build dirs and kicks off synthesis via --run_builds.)

set -euo pipefail

BUILD_ROOT="${1:-/tmp/SMOKE_BUILDS}"
mkdir -p "$BUILD_ROOT"

PORT_EXP_DIR="$BUILD_ROOT/PORT_EXP"
MEMORY_EXP_DIR="$BUILD_ROOT/MEMORY_EXP"

echo "[smoke_test] build root: $BUILD_ROOT"

# Note: we omit --run_builds because that path uses hardcoded step numbers
# (make 6/18/29) that shift when new steps are added. Drive synthesis via
# run_synth_pool.py instead — it resolves step numbers dynamically:
#   python ASPLOS_EXP/run_synth_pool.py \
#       --sh-script ASPLOS_EXP/smoke_test.sh \
#       --build-root "$BUILD_ROOT/PORT_EXP" --build-root "$BUILD_ROOT/MEMORY_EXP" \
#       --jobs 4 --phase both

# Known pass: PORT_EXP fw=4/dw=16 and fw=2/dw=32 (W01024B064)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir "$PORT_EXP_DIR" \
    --design_filter thesis_sweep --physical \
    --fetch_width 4 \
    --vec_capacity 2 \
    --data_width 16 \
    --storage_capacity 8192 \
    --frequency 700

# fw=2 multi-out requires dual-port SRAM to satisfy bandwidth: with 1 writer +
# 2 readers all firing every 2 cycles, single-port SRAM is over-subscribed
# (3 accesses / 2 cycles vs supply of 2 / 2). Dual-port splits R/W into
# separate physical ports so the schedule fits.
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir "$PORT_EXP_DIR" \
    --design_filter thesis_sweep --physical \
    --fetch_width 2 \
    --vec_capacity 2 \
    --data_width 32 \
    --storage_capacity 8192 \
    --dual_port \
    --frequency 700

# gen_sram fix: MEMORY_EXP sc=2048 fw=2 single-port (needed W00512B032, got W01024B064 before)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir "$MEMORY_EXP_DIR" \
    --design_filter thesis_sweep --physical \
    --fetch_width 2 \
    --in_ports 1 \
    --out_ports 1 \
    --data_width 16 \
    --storage_capacity 2048 \
    --frequency 700

# gen_sram fix: MEMORY_EXP sc=4096 fw=1 dual-port (needed W02048B016, got W01024B064 before)
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir "$MEMORY_EXP_DIR" \
    --design_filter thesis_sweep --physical \
    --fetch_width 1 \
    --dual_port \
    --in_ports 1 \
    --out_ports 1 \
    --data_width 16 \
    --storage_capacity 4096 \
    --frequency 700
