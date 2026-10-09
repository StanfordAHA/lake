#!/bin/bash
#=============================================================================
# generic_synth_power.sh
#=============================================================================
# Generic (NO-ADK) synthesis -> power path.
#
# Given an RTL design, a generic standard-cell .lib, and pre-generated
# idle/active activity (SAIF or VCD), this:
#   1. converts the .lib to a .db (PrimeTime needs .db)                [lc_shell]
#   2. synthesizes the RTL with the SAME constraint recipe as the       [genus|dc]
#      mflowgen flow (config_memory false-path, flush multicycle,
#      driving cell, load, fanout, transition) -- only the clock
#      period is caller-supplied since a generic node can't hit the
#      real target frequency.
#   3. reports post-synthesis area/timing
#   4. annotates the synth netlist with the idle SAIF and the active     [pt_shell]
#      SAIF separately and reports power for each.
#
# It deliberately needs NO mflowgen ADK: no SRAM macros, no LEF/QRC, no
# PnR. Storage is behavioural (physical=False). Use it as a fast,
# self-contained "synthesis results -> power" check when the gf12 ADK
# flow isn't available. See GENERIC_SYNTH_POWER.md for the full writeup.
#
# Usage:
#   generic_synth_power.sh \
#       --design   <design.v> \
#       --lib      <stdcells.lib> \
#       --idle     <idle.saif|idle.vcd> \
#       --active   <active.saif|active.vcd> \
#       --outdir   <dir> \
#       [--top lakespec] [--clock-ns 10.0] [--strip-path tb/dut] \
#       [--synth genus|dc]
#
# Tool locations are overridable via env: GENUS, DC, LC, PT, VCD2SAIF.
#=============================================================================
set -euo pipefail

#-----------------------------------------------------------------------------
# Defaults
#-----------------------------------------------------------------------------
TOP=lakespec
CLK_NS=10.0
STRIP_PATH=tb/dut
SYNTH=genus
IO_DELAY_FRAC=0.5   # input delay = frac * clock_period. Real flow uses 0.5;
                    # set 0 to time the internal logic only (e.g. Fmax runs).
DESIGN_V=""; LIB=""; IDLE_ACT=""; ACTIVE_ACT=""; OUTDIR=""; DB_PROVIDED=""

GENUS=${GENUS:-/cad/cadence/GENUS_20.11.000_lnx86/bin/genus}
DC=${DC:-/cad/synopsys/syn/X-2025.06-SP4/bin/dc_shell}
LC=${LC:-/cad/synopsys/lc/U-2022.12-SP1/bin/lc_shell}
PT=${PT:-/cad/synopsys/prime/W-2024.09-SP5/bin/pt_shell}
VCD2SAIF=${VCD2SAIF:-/cad/synopsys/pts/M-2017.06-SP3/bin/vcd2saif}

# Licenses (override in env if your servers differ)
export SNPSLMD_LICENSE_FILE=${SNPSLMD_LICENSE_FILE:-27000@cadlic0.stanford.edu}
export CDS_LIC_FILE=${CDS_LIC_FILE:-5280@cadlic0.stanford.edu}
export LM_LICENSE_FILE=${LM_LICENSE_FILE:-27000@cadlic0.stanford.edu:5280@cadlic0.stanford.edu}

#-----------------------------------------------------------------------------
# Parse args
#-----------------------------------------------------------------------------
while [ $# -gt 0 ]; do
  case "$1" in
    --design)     DESIGN_V="$2";   shift 2;;
    --lib)        LIB="$2";        shift 2;;
    --db)         DB_PROVIDED="$2"; shift 2;;
    --idle)       IDLE_ACT="$2";   shift 2;;
    --active)     ACTIVE_ACT="$2"; shift 2;;
    --outdir)     OUTDIR="$2";     shift 2;;
    --top)        TOP="$2";        shift 2;;
    --clock-ns)   CLK_NS="$2";     shift 2;;
    --strip-path) STRIP_PATH="$2"; shift 2;;
    --synth)      SYNTH="$2";      shift 2;;
    --io-delay-frac) IO_DELAY_FRAC="$2"; shift 2;;
    -h|--help)    grep '^#' "$0" | sed 's/^#//'; exit 0;;
    *) echo "unknown arg: $1" >&2; exit 2;;
  esac
done

for req in DESIGN_V LIB IDLE_ACT ACTIVE_ACT OUTDIR; do
  [ -n "${!req}" ] || { echo "ERROR: --${req,,} is required (see --help)" >&2; exit 2; }
done
[ -f "$DESIGN_V" ] || { echo "ERROR: design not found: $DESIGN_V" >&2; exit 2; }
[ -f "$LIB" ]      || { echo "ERROR: lib not found: $LIB" >&2; exit 2; }

mkdir -p "$OUTDIR"/{synth/reports,pt}
OUTDIR=$(cd "$OUTDIR" && pwd)
DESIGN_V=$(cd "$(dirname "$DESIGN_V")" && pwd)/$(basename "$DESIGN_V")
LIB=$(cd "$(dirname "$LIB")" && pwd)/$(basename "$LIB")
LIBDIR=$(dirname "$LIB"); LIBBASE=$(basename "$LIB")

echo "=== generic_synth_power: top=$TOP synth=$SYNTH clk=${CLK_NS}ns ==="

#-----------------------------------------------------------------------------
# Helper: turn a VCD into a SAIF if needed; echo the SAIF path.
#-----------------------------------------------------------------------------
to_saif() {
  local act="$1" tag="$2"
  case "$act" in
    *.saif) echo "$act";;
    *.vcd)  local out="$OUTDIR/${tag}.saif"
            "$VCD2SAIF" -input "$act" -output "$out" > "$OUTDIR/${tag}.vcd2saif.log" 2>&1
            echo "$out";;
    *) echo "ERROR: activity must be .saif or .vcd: $act" >&2; exit 2;;
  esac
}
IDLE_SAIF=$(to_saif "$IDLE_ACT" idle)
ACTIVE_SAIF=$(to_saif "$ACTIVE_ACT" active)

#-----------------------------------------------------------------------------
# 1. .lib -> .db (PrimeTime needs a .db). If a prebuilt .db is supplied via
#    --db, use it and skip lc_shell entirely.
#-----------------------------------------------------------------------------
if [ -n "$DB_PROVIDED" ]; then
  [ -f "$DB_PROVIDED" ] || { echo "ERROR: --db not found: $DB_PROVIDED" >&2; exit 2; }
  DB=$(cd "$(dirname "$DB_PROVIDED")" && pwd)/$(basename "$DB_PROVIDED")
  echo "[1/4] db: $DB (supplied, skipping lib->db)"
else
  DB="$OUTDIR/synth/$(basename "${LIBBASE%.lib}").db"
  if [ ! -f "$DB" ]; then
    # write_lib requires the ACTUAL library name (from `library (NAME) {` in
    # the .lib) as the trailing positional arg -- a wrong name segfaults lc.
    LIBNAME=$(grep -oE 'library[[:space:]]*\([[:space:]]*[A-Za-z0-9_]+' "$LIB" | head -1 | grep -oE '[A-Za-z0-9_]+$')
    [ -n "$LIBNAME" ] || { echo "ERROR: could not parse library name from $LIB" >&2; exit 2; }
    echo "[1/4] lib -> db (library name: $LIBNAME) ..."
    cat > "$OUTDIR/synth/lib2db.tcl" <<EOF
read_lib $LIB
write_lib -format db -output $DB $LIBNAME
exit
EOF
    ( cd "$OUTDIR/synth" && "$LC" -f lib2db.tcl > lib2db.log 2>&1 ) \
      || { echo "ERROR: lib->db failed; see $OUTDIR/synth/lib2db.log (or pass --db a prebuilt .db)" >&2; exit 1; }
  fi
  echo "[1/4] db: $DB"
fi

#-----------------------------------------------------------------------------
# 2. Synthesis (constraints mirror pd/thesis/constraints/constraints.tcl,
#    only clock_period is caller-supplied).
#-----------------------------------------------------------------------------
GATES_V="$OUTDIR/synth/gates.v"
echo "[2/4] synthesis ($SYNTH) ..."
if [ "$SYNTH" = "genus" ]; then
  cat > "$OUTDIR/synth/syn.tcl" <<EOF
set_db init_lib_search_path $LIBDIR
set_db library {$LIBBASE}
read_hdl -sv $DESIGN_V
elaborate $TOP
current_design $TOP
# ---- constraints (mirror of pd/thesis/constraints/constraints.tcl) ----
set clock_period             $CLK_NS
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
# -----------------------------------------------------------------------
set_db syn_generic_effort medium
set_db syn_map_effort     medium
set_db syn_opt_effort     medium
syn_generic
syn_map
syn_opt
report_area              > reports/area.rpt
report_gates             > reports/gates.rpt
report_timing -max_paths 5 > reports/timing.rpt
write_hdl                > $GATES_V
puts "GENUS_SYNTH_DONE"
exit
EOF
  ( cd "$OUTDIR/synth" && "$GENUS" -no_gui -files syn.tcl > genus.log 2>&1 )
  grep -q GENUS_SYNTH_DONE "$OUTDIR/synth/genus.log" || { echo "ERROR: genus did not finish; see $OUTDIR/synth/genus.log" >&2; exit 1; }
  # Genus area.rpt top row: "<top> <cellcount> <cellarea> <netarea> <TOTALarea> <wireload> (D)".
  # Grab the LAST numeric token on the row = Total Area (not $NF, which is the wireload tag).
  AREA=$(awk -v t="$TOP" '$1==t {for(i=NF;i>=1;i--) if($i ~ /^[0-9.]+$/){print $i; exit}}' "$OUTDIR/synth/reports/area.rpt" 2>/dev/null)
elif [ "$SYNTH" = "dc" ]; then
  cat > "$OUTDIR/synth/syn.tcl" <<EOF
set target_library [list $DB]
set link_library "* \$target_library"
read_file -format sverilog $DESIGN_V
current_design $TOP
link
set clock_period $CLK_NS
create_clock -name ideal_clock -period \$clock_period [get_ports clk]
set_input_delay  -clock ideal_clock [expr \$clock_period*${IO_DELAY_FRAC}] [remove_from_collection [all_inputs] [get_ports clk]]
set_output_delay -clock ideal_clock 0 [all_outputs]
set_max_fanout 20 [current_design]
set_max_transition [expr 0.25*\$clock_period] [current_design]
set_false_path -from [get_ports config_memory* -quiet]
compile -exact_map
report_area              > reports/area.rpt
report_timing -max_paths 5 > reports/timing.rpt
change_names -rules verilog -hierarchy
write -format verilog -hierarchy -output $GATES_V
exit
EOF
  ( cd "$OUTDIR/synth" && "$DC" -f syn.tcl > dc.log 2>&1 )
  AREA=$(grep -m1 "Total cell area" "$OUTDIR/synth/reports/area.rpt" | awk '{print $NF}')
else
  echo "ERROR: --synth must be genus or dc" >&2; exit 2
fi
[ -f "$GATES_V" ] || { echo "ERROR: no netlist produced" >&2; exit 1; }
echo "[2/4] netlist: $GATES_V  area=$AREA"

#-----------------------------------------------------------------------------
# 3+4. PrimeTime power, idle then active.
#   NOTE the two hard-won fixes baked in here:
#     * current_design $TOP BEFORE link  -> otherwise link binds a random
#       submodule and power collapses to ~uW.
#     * create_clock directly (do NOT read a Genus-written .sdc) -> Genus's
#       write_sdc embeds a `current_design` line that PT read_sdc rejects,
#       silently dropping the clock so clock_network power reads 0.
#-----------------------------------------------------------------------------
run_pt() {
  local variant="$1" saif="$2"
  cat > "$OUTDIR/pt/pt_${variant}.tcl" <<EOF
set power_enable_analysis true
set link_path [list * $DB]
read_verilog $GATES_V
current_design $TOP
link
create_clock -name clk -period $CLK_NS [get_ports clk]
set_propagated_clock [all_clocks]
read_saif $saif -strip_path $STRIP_PATH
update_power
report_power            > $OUTDIR/pt/power_${variant}.rpt
report_power -hierarchy > $OUTDIR/pt/power_${variant}.hier.rpt
puts "PT_${variant}_DONE"
exit
EOF
  ( cd "$OUTDIR/pt" && "$PT" -f "pt_${variant}.tcl" > "pt_${variant}.log" 2>&1 )
  grep -q "PT_${variant}_DONE" "$OUTDIR/pt/pt_${variant}.log" || { echo "ERROR: PT $variant failed; see $OUTDIR/pt/pt_${variant}.log" >&2; exit 1; }
}
echo "[3/4] PT idle ..."   ; run_pt idle   "$IDLE_SAIF"
echo "[4/4] PT active ..." ; run_pt active "$ACTIVE_SAIF"

#-----------------------------------------------------------------------------
# Summary
#-----------------------------------------------------------------------------
get() { grep -m1 "$1" "$2" 2>/dev/null | awk '{print $(NF-1)}'; }
IDLE_TOT=$(grep -m1 "Total Power" "$OUTDIR/pt/power_idle.rpt"   | awk '{print $4}')
ACT_TOT=$( grep -m1 "Total Power" "$OUTDIR/pt/power_active.rpt" | awk '{print $4}')
IDLE_SW=$( grep -m1 "Net Switching Power" "$OUTDIR/pt/power_idle.rpt"   | awk '{print $5}')
ACT_SW=$(  grep -m1 "Net Switching Power" "$OUTDIR/pt/power_active.rpt" | awk '{print $5}')
{
  echo "design,top,synth,clock_ns,area,idle_total_W,active_total_W,idle_switch_W,active_switch_W"
  echo "$DESIGN_V,$TOP,$SYNTH,$CLK_NS,$AREA,$IDLE_TOT,$ACT_TOT,$IDLE_SW,$ACT_SW"
} > "$OUTDIR/summary.csv"

echo "=== DONE ==="
echo "  area           = $AREA"
echo "  idle  total    = $IDLE_TOT W   (net switching $IDLE_SW W)"
echo "  active total   = $ACT_TOT W   (net switching $ACT_SW W)"
echo "  summary        -> $OUTDIR/summary.csv"
echo "  reports        -> $OUTDIR/{synth/reports,pt}/"
