#!/bin/bash
#=============================================================================
# fmax_sweep.sh
#=============================================================================
# Fmax characterization for a lake spec netlist under the generic (no-ADK)
# flow. Synthesis-only: for each clock target it runs Genus with the SAME
# constraint recipe as generic_synth_power.sh EXCEPT the input delay, which
# defaults to 0 here so the achievable clock reflects the INTERNAL logic
# (not an assumed I/O budget). Reports worst slack per target and derives an
# Fmax estimate = 1 / (target - worst_slack).
#
# Usage:
#   fmax_sweep.sh --design <design.v> --lib <stdcells.lib> --outdir <dir> \
#     [--top lakespec] [--periods "4 3 2.5 2"] [--io-delay-frac 0] \
#     [--effort medium]
#
# Notes:
#   - Each point is a full Genus synth (~10-20 min wall on the 8KB behavioural
#     design); keep the period list short.
#   - "Achieved period" = target - worst_slack (ps). As the target tightens,
#     Genus optimizes harder, so the achieved period converges toward the true
#     minimum -> the tightest target's Fmax is the best estimate.
#=============================================================================
set -euo pipefail

TOP=lakespec
PERIODS="4 3 2.5 2"
IO_DELAY_FRAC=0
EFFORT=medium
DESIGN_V=""; LIB=""; OUTDIR=""

GENUS=${GENUS:-/cad/cadence/GENUS_20.11.000_lnx86/bin/genus}
export CDS_LIC_FILE=${CDS_LIC_FILE:-5280@cadlic0.stanford.edu}
export LM_LICENSE_FILE=${LM_LICENSE_FILE:-5280@cadlic0.stanford.edu:27000@cadlic0.stanford.edu}

while [ $# -gt 0 ]; do
  case "$1" in
    --design)        DESIGN_V="$2"; shift 2;;
    --lib)           LIB="$2";      shift 2;;
    --outdir)        OUTDIR="$2";   shift 2;;
    --top)           TOP="$2";      shift 2;;
    --periods)       PERIODS="$2";  shift 2;;
    --io-delay-frac) IO_DELAY_FRAC="$2"; shift 2;;
    --effort)        EFFORT="$2";   shift 2;;
    -h|--help) grep '^#' "$0" | sed 's/^#//'; exit 0;;
    *) echo "unknown arg: $1" >&2; exit 2;;
  esac
done
for req in DESIGN_V LIB OUTDIR; do
  [ -n "${!req}" ] || { echo "ERROR: --${req,,} required" >&2; exit 2; }
done
mkdir -p "$OUTDIR"
OUTDIR=$(cd "$OUTDIR" && pwd)
DESIGN_V=$(cd "$(dirname "$DESIGN_V")" && pwd)/$(basename "$DESIGN_V")
LIB=$(cd "$(dirname "$LIB")" && pwd)/$(basename "$LIB")
LIBDIR=$(dirname "$LIB"); LIBBASE=$(basename "$LIB")

RESULTS="$OUTDIR/fmax_results.csv"
echo "target_ns,worst_slack_ns,achieved_ns,fmax_mhz,status" > "$RESULTS"
echo "=== fmax_sweep: top=$TOP io_delay_frac=$IO_DELAY_FRAC effort=$EFFORT periods='$PERIODS' ==="

for P in $PERIODS; do
  D="$OUTDIR/p_${P}ns"; mkdir -p "$D/reports"
  echo "--- target ${P} ns ---"
  cat > "$D/syn.tcl" <<EOF
set_db init_lib_search_path $LIBDIR
set_db library {$LIBBASE}
read_hdl -sv $DESIGN_V
elaborate $TOP
current_design $TOP
set clock_period             $P
set ADK_DRIVING_CELL         "INV_X2"
set ADK_TYPICAL_ON_CHIP_LOAD 7
create_clock -name ideal_clock -period \${clock_period} [get_ports clk]
set_load -pin_load \$ADK_TYPICAL_ON_CHIP_LOAD [all_outputs]
set_driving_cell -no_design_rule -lib_cell \$ADK_DRIVING_CELL [all_inputs]
set_input_delay  -clock ideal_clock [expr \${clock_period}*${IO_DELAY_FRAC}] [all_inputs]
set_output_delay -clock ideal_clock 0 [all_outputs]
set_max_fanout 20 $TOP
set_max_transition [expr 0.25*\${clock_period}] $TOP
set_false_path -from {config_memory*}
set_multicycle_path -setup 10 -from {flush}
set_multicycle_path -hold  9 -from {flush}
set_db syn_generic_effort $EFFORT
set_db syn_map_effort     $EFFORT
set_db syn_opt_effort     $EFFORT
syn_generic
syn_map
syn_opt
report_timing -max_paths 1 > reports/timing.rpt
puts "FMAX_POINT_DONE"
exit
EOF
  ( cd "$D" && "$GENUS" -no_gui -files syn.tcl > genus.log 2>&1 ) || true
  if ! grep -q FMAX_POINT_DONE "$D/genus.log"; then
    echo "  ${P}ns: genus did not finish (see $D/genus.log)"
    echo "$P,,,,GENUS_FAIL" >> "$RESULTS"; continue
  fi
  # Genus reports slack in ps: e.g. "Slack:=    3665" or "Path 1: MET (3665 ps)"
  SLACK_PS=$(grep -oE "Slack:=[[:space:]]*-?[0-9]+" "$D/reports/timing.rpt" | head -1 | grep -oE "\-?[0-9]+$")
  [ -n "$SLACK_PS" ] || SLACK_PS=$(grep -oE "\((MET|VIOLATED) *-?[0-9]+ ps\)" "$D/reports/timing.rpt" | head -1 | grep -oE "\-?[0-9]+")
  if [ -z "$SLACK_PS" ]; then
    echo "  ${P}ns: could not parse slack"; echo "$P,,,,NO_SLACK" >> "$RESULTS"; continue
  fi
  # target(ps) - slack(ps) = achieved critical period (ps); fmax = 1e6/achieved_ps MHz
  read SLACK_NS ACH_NS FMAX STAT < <(awk -v p="$P" -v s="$SLACK_PS" 'BEGIN{
      sns=s/1000.0; ach=p - sns; fm=(ach>0)?1e3/ach:0;
      st=(s>=0)?"MET":"VIOLATED";
      printf "%.3f %.3f %.1f %s\n", sns, ach, fm, st}')
  echo "  ${P}ns: slack=${SLACK_NS}ns  achieved=${ACH_NS}ns  Fmax~${FMAX}MHz  [$STAT]"
  echo "$P,$SLACK_NS,$ACH_NS,$FMAX,$STAT" >> "$RESULTS"
done

echo "=== DONE -> $RESULTS ==="
column -t -s, "$RESULTS" 2>/dev/null || cat "$RESULTS"
echo "Best Fmax estimate = tightest target's achieved period (see table above)."
