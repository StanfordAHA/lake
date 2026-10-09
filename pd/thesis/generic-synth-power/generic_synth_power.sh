#!/bin/bash
#=============================================================================
# generic_synth_power.sh
#=============================================================================
# Generic (NO-ADK) synthesis -> power path.
#
# Given an RTL design, a generic standard-cell .lib and the idle/active
# power tests, this:
#   1. converts the .lib to a .db (PrimeTime needs .db)                [lc_shell]
#   2. synthesizes the RTL with the SAME constraint recipe as the       [genus|dc]
#      mflowgen flow (config_memory false-path, flush multicycle,
#      driving cell, load, fanout, transition) -- only the clock
#      period is caller-supplied since a generic node can't hit the
#      real target frequency.
#   3. simulates the SYNTH NETLIST (+ the lib's Verilog cell models) with   [vcs]
#      lake's power testbench and the idle and active programs from
#      power-test-gen (--power-tests), zero-delay, and turns each VCD
#      into a SAIF                                                     [vcd2saif]
#   4. annotates the synth netlist with each SAIF and reports power     [pt_shell]
#      for idle and active.
#
# Step 3 is why the numbers can be trusted: the activity carries the
# netlist's own names, so PrimeTime annotates ~100% of nets. The older
# mode (--idle/--active: activity from an RTL sim) is still accepted, but
# only flop/port names survive synthesis, so PT annotates a few % of nets,
# gives the rest default toggle rates, and idle comes out about equal to
# active (2026-10-08 comparison in GENERIC_SYNTH_POWER.md).
#
# It deliberately needs NO mflowgen ADK: no SRAM macros, no LEF/QRC, no
# PnR. Storage is behavioural (physical=False). Use it as a fast,
# self-contained "synthesis results -> power" check when the gf12 ADK
# flow isn't available. See GENERIC_SYNTH_POWER.md for the full writeup.
#
# Usage:
#   generic_synth_power.sh \
#       --design      <design.v> \
#       --lib         <stdcells.lib> \
#       --cells       <stdcells.v>          # Verilog models of the lib's cells
#       --power-tests <power-test-gen outputs dir> \
#       --outdir      <dir> \
#       [--tb <tb.sv>] [--sim-args "<extra vcs args>"] [--keep-vcd] \
#       [--top lakespec] [--clock-ns 10.0] [--strip-path tb/dut_gen.dut] \
#       [--synth genus|dc]
#   --power-tests DIR holds power-test-gen's outputs for the SAME spec as
#   --design: bitstream.{idle,active}.bs, PARGS.{idle,active}.txt,
#   comp_args.txt, input_data.hex. --tb defaults to
#   ../synopsys-vcs-sim-power/tb.sv.
#
#   Older mode (RTL-sim activity, low annotation; prints a warning):
#       --idle <idle.saif|idle.vcd> --active <active.saif|active.vcd>
#       instead of --cells/--power-tests; --strip-path defaults to tb/dut.
#
# Tool locations are overridable via env: GENUS, DC, LC, PT, VCD2SAIF, VCS.
#=============================================================================
set -euo pipefail
# Every tool reads stdin from /dev/null: on a Tcl error Genus/DC/PT otherwise
# stop at their interactive prompt and the script never returns. The
# GENUS_SYNTH_DONE / PT_*_DONE markers below then report the failure.

#-----------------------------------------------------------------------------
# Defaults
#-----------------------------------------------------------------------------
TOP=lakespec
CLK_NS=10.0
STRIP_PATH=""       # default: tb/dut_gen.dut (netlist sim), tb/dut (--idle/--active)
SYNTH=genus
IO_DELAY_FRAC=0.5   # input delay = frac * clock_period. Real flow uses 0.5;
                    # set 0 to time the internal logic only (e.g. Fmax runs).
DESIGN_V=""; LIB=""; IDLE_ACT=""; ACTIVE_ACT=""; OUTDIR=""; DB_PROVIDED=""
CELLS=""; POWER_TESTS=""; TB=""; SIM_ARGS=""; KEEP_VCD=0
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)

GENUS=${GENUS:-/cad/cadence/GENUS_20.11.000_lnx86/bin/genus}
DC=${DC:-/cad/synopsys/syn/X-2025.06-SP4/bin/dc_shell}
LC=${LC:-/cad/synopsys/lc/U-2022.12-SP1/bin/lc_shell}
PT=${PT:-/cad/synopsys/prime/W-2024.09-SP5/bin/pt_shell}
VCD2SAIF=${VCD2SAIF:-/cad/synopsys/pts/M-2017.06-SP3/bin/vcd2saif}
VCS=${VCS:-/cad/synopsys/vcs/U-2023.03/bin/vcs}

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
    --cells)      CELLS="$2";      shift 2;;
    --power-tests) POWER_TESTS="$2"; shift 2;;
    --tb)         TB="$2";         shift 2;;
    --sim-args)   SIM_ARGS="$2";   shift 2;;
    --keep-vcd)   KEEP_VCD=1;      shift 1;;
    -h|--help)    grep '^#' "$0" | sed 's/^#//'; exit 0;;
    *) echo "unknown arg: $1" >&2; exit 2;;
  esac
done

for req in DESIGN_V LIB OUTDIR; do
  [ -n "${!req}" ] || { echo "ERROR: --${req,,} is required (see --help)" >&2; exit 2; }
done
[ -f "$DESIGN_V" ] || { echo "ERROR: design not found: $DESIGN_V" >&2; exit 2; }
[ -f "$LIB" ]      || { echo "ERROR: lib not found: $LIB" >&2; exit 2; }
if [ -n "$POWER_TESTS" ]; then
  MODE=netlist-sim
  [ -z "$IDLE_ACT$ACTIVE_ACT" ] || { echo "ERROR: --power-tests and --idle/--active are exclusive" >&2; exit 2; }
  [ -n "$CELLS" ] && [ -f "$CELLS" ] || { echo "ERROR: --power-tests needs --cells <Verilog cell models of the lib>" >&2; exit 2; }
  TB=${TB:-$SCRIPT_DIR/../synopsys-vcs-sim-power/tb.sv}
  [ -f "$TB" ] || { echo "ERROR: testbench not found: $TB" >&2; exit 2; }
  for f in bitstream.idle.bs bitstream.active.bs PARGS.idle.txt PARGS.active.txt comp_args.txt input_data.hex; do
    [ -f "$POWER_TESTS/$f" ] || { echo "ERROR: $POWER_TESTS/$f missing (power-test-gen outputs)" >&2; exit 2; }
  done
  POWER_TESTS=$(cd "$POWER_TESTS" && pwd)
  CELLS=$(cd "$(dirname "$CELLS")" && pwd)/$(basename "$CELLS")
  TB=$(cd "$(dirname "$TB")" && pwd)/$(basename "$TB")
  STRIP_PATH=${STRIP_PATH:-tb/dut_gen.dut}
else
  MODE=rtl-saif
  [ -n "$IDLE_ACT" ] && [ -n "$ACTIVE_ACT" ] || { echo "ERROR: give --power-tests + --cells (recommended), or --idle + --active" >&2; exit 2; }
  STRIP_PATH=${STRIP_PATH:-tb/dut}
  echo "WARNING: --idle/--active = RTL-sim activity on the synth netlist: PT annotates only a few % of" >&2
  echo "         nets and idle comes out ~= active. Use --power-tests + --cells for a netlist sim." >&2
fi

mkdir -p "$OUTDIR"/{synth/reports,pt}
OUTDIR=$(cd "$OUTDIR" && pwd)
DESIGN_V=$(cd "$(dirname "$DESIGN_V")" && pwd)/$(basename "$DESIGN_V")
LIB=$(cd "$(dirname "$LIB")" && pwd)/$(basename "$LIB")
LIBDIR=$(dirname "$LIB"); LIBBASE=$(basename "$LIB")

echo "=== generic_synth_power: top=$TOP synth=$SYNTH clk=${CLK_NS}ns activity=$MODE ==="

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
if [ "$MODE" = rtl-saif ]; then
  IDLE_SAIF=$(to_saif "$IDLE_ACT" idle)
  ACTIVE_SAIF=$(to_saif "$ACTIVE_ACT" active)
fi

#-----------------------------------------------------------------------------
# 1. .lib -> .db (PrimeTime needs a .db). If a prebuilt .db is supplied via
#    --db, use it and skip lc_shell entirely.
#-----------------------------------------------------------------------------
if [ -n "$DB_PROVIDED" ]; then
  [ -f "$DB_PROVIDED" ] || { echo "ERROR: --db not found: $DB_PROVIDED" >&2; exit 2; }
  DB=$(cd "$(dirname "$DB_PROVIDED")" && pwd)/$(basename "$DB_PROVIDED")
  echo "[1/5] db: $DB (supplied, skipping lib->db)"
else
  DB="$OUTDIR/synth/$(basename "${LIBBASE%.lib}").db"
  if [ ! -f "$DB" ]; then
    # write_lib requires the ACTUAL library name (from `library (NAME) {` in
    # the .lib) as the trailing positional arg -- a wrong name segfaults lc.
    LIBNAME=$(grep -oE 'library[[:space:]]*\([[:space:]]*[A-Za-z0-9_]+' "$LIB" | head -1 | grep -oE '[A-Za-z0-9_]+$')
    [ -n "$LIBNAME" ] || { echo "ERROR: could not parse library name from $LIB" >&2; exit 2; }
    echo "[1/5] lib -> db (library name: $LIBNAME) ..."
    cat > "$OUTDIR/synth/lib2db.tcl" <<EOF
read_lib $LIB
write_lib -format db -output $DB $LIBNAME
exit
EOF
    ( cd "$OUTDIR/synth" && "$LC" -f lib2db.tcl > lib2db.log 2>&1 < /dev/null ) \
      || { echo "ERROR: lib->db failed; see $OUTDIR/synth/lib2db.log (or pass --db a prebuilt .db)" >&2; exit 1; }
  fi
  echo "[1/5] db: $DB"
fi

#-----------------------------------------------------------------------------
# 2. Synthesis (constraints mirror pd/thesis/constraints/constraints.tcl,
#    only clock_period is caller-supplied).
#-----------------------------------------------------------------------------
GATES_V="$OUTDIR/synth/gates.v"
echo "[2/5] synthesis ($SYNTH) ..."
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
  ( cd "$OUTDIR/synth" && "$GENUS" -no_gui -files syn.tcl > genus.log 2>&1 < /dev/null ) || true
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
  ( cd "$OUTDIR/synth" && "$DC" -f syn.tcl > dc.log 2>&1 < /dev/null )
  AREA=$(grep -m1 "Total cell area" "$OUTDIR/synth/reports/area.rpt" | awk '{print $NF}')
else
  echo "ERROR: --synth must be genus or dc" >&2; exit 2
fi
[ -f "$GATES_V" ] || { echo "ERROR: no netlist produced" >&2; exit 1; }
echo "[2/5] netlist: $GATES_V  area=$AREA"

#-----------------------------------------------------------------------------
# 3. Netlist sims: lake's power tb + the idle / active programs on gates.v and
#    the cell models, zero-delay (no SDF: skip specify delays and timing
#    checks, which would X a flop on an ideal-clock edge). Same programs,
#    PARGS and input stream as the mflowgen synopsys-vcs-sim-power steps.
#-----------------------------------------------------------------------------
run_sim() {
  local variant="$1" S="$OUTDIR/sim_$1"
  rm -rf "$S"; mkdir -p "$S/inputs" "$S/outputs"
  cp "$TB" "$S/tb.sv"
  cp "$POWER_TESTS/comp_args.txt" "$POWER_TESTS/input_data.hex" "$S/inputs/"
  cp "$POWER_TESTS/bitstream.$variant.bs" "$S/inputs/bitstream.bs"
  cp "$POWER_TESTS/PARGS.$variant.txt" "$S/inputs/PARGS.txt"
  ( cd "$S" && "$VCS" -sverilog -timescale=1ns/1ns -full64 -top tb +vcs+lic+wait +v2k \
      +nospecify +notimingcheck $CELL_DEFS $SIM_ARGS -l vcs.log \
      $(cat inputs/comp_args.txt) tb.sv "$GATES_V" "$CELLS" > compile.log 2>&1 ) \
    || { echo "ERROR: VCS compile ($variant) failed; see $S/vcs.log" >&2; exit 1; }
  ( cd "$S" && ./simv +TEST_DIRECTORY=./ +dump_vcd=1 $(cat inputs/PARGS.txt) > sim.log 2>&1 < /dev/null ) || true
  grep -q "^PASS" "$S/sim.log" && ! grep -q "^FAIL" "$S/sim.log" \
    || { echo "ERROR: $variant netlist sim did not PASS; see $S/sim.log" >&2; exit 1; }
  [ -s "$S/waveforms.vcd" ] || { echo "ERROR: $variant sim wrote no waveforms.vcd" >&2; exit 1; }
  "$VCD2SAIF" -input "$S/waveforms.vcd" -output "$S/run.saif" > "$S/vcd2saif.log" 2>&1
  [ -s "$S/run.saif" ] || { echo "ERROR: vcd2saif ($variant) failed; see $S/vcd2saif.log" >&2; exit 1; }
  [ "$KEEP_VCD" = 1 ] || rm -f "$S/waveforms.vcd"
  # Words captured on the read ports (non-X), a sanity check that active
  # streams data and idle doesn't.
  local words; words=$(cat "$S"/outputs/port_r*_data.txt 2>/dev/null | grep -vic '^[x0]*$' || true)
  echo "[3/5]   $variant: PASS, $words non-zero words on the read ports"
}
if [ "$MODE" = netlist-sim ]; then
  export VCS_HOME=${VCS_HOME:-$(dirname "$(dirname "$VCS")")}
  # NanGate-style models (freepdk45) drive their own reset pin through
  # ng_xbuf unless TETRAMAX is defined, which leaves async-reset flops at X.
  CELL_DEFS=""
  if grep -q 'ifdef TETRAMAX' "$CELLS"; then CELL_DEFS="+define+TETRAMAX"; fi
  echo "[3/5] netlist sims (vcs${CELL_DEFS:+, $CELL_DEFS}) ..."
  run_sim idle;   IDLE_SAIF="$OUTDIR/sim_idle/run.saif"
  run_sim active; ACTIVE_SAIF="$OUTDIR/sim_active/run.saif"
else
  echo "[3/5] activity: RTL-sim SAIFs supplied (--idle/--active)"
fi

#-----------------------------------------------------------------------------
# 4+5. PrimeTime power, idle then active.
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
  ( cd "$OUTDIR/pt" && "$PT" -f "pt_${variant}.tcl" > "pt_${variant}.log" 2>&1 < /dev/null ) || true
  grep -q "PT_${variant}_DONE" "$OUTDIR/pt/pt_${variant}.log" || { echo "ERROR: PT $variant failed; see $OUTDIR/pt/pt_${variant}.log" >&2; exit 1; }
}
# "Number of annotated nets = N (P%)" from read_saif -> P
annot() { grep -m1 "annotated nets" "$OUTDIR/pt/pt_$1.log" 2>/dev/null | sed -n 's/.*(\([0-9.]*\)%).*/\1/p'; }
echo "[4/5] PT idle ..."   ; run_pt idle   "$IDLE_SAIF"
echo "[5/5] PT active ..." ; run_pt active "$ACTIVE_SAIF"
IDLE_ANN=$(annot idle); ACT_ANN=$(annot active)

#-----------------------------------------------------------------------------
# Summary
#-----------------------------------------------------------------------------
get() { grep -m1 "$1" "$2" 2>/dev/null | awk '{print $(NF-1)}'; }
IDLE_TOT=$(grep -m1 "Total Power" "$OUTDIR/pt/power_idle.rpt"   | awk '{print $4}')
ACT_TOT=$( grep -m1 "Total Power" "$OUTDIR/pt/power_active.rpt" | awk '{print $4}')
IDLE_SW=$( grep -m1 "Net Switching Power" "$OUTDIR/pt/power_idle.rpt"   | awk '{print $5}')
ACT_SW=$(  grep -m1 "Net Switching Power" "$OUTDIR/pt/power_active.rpt" | awk '{print $5}')
{
  echo "design,top,synth,clock_ns,area,idle_total_W,active_total_W,idle_switch_W,active_switch_W,activity,idle_annotated_pct,active_annotated_pct"
  echo "$DESIGN_V,$TOP,$SYNTH,$CLK_NS,$AREA,$IDLE_TOT,$ACT_TOT,$IDLE_SW,$ACT_SW,$MODE,$IDLE_ANN,$ACT_ANN"
} > "$OUTDIR/summary.csv"

echo "=== DONE ==="
echo "  area           = $AREA"
echo "  idle  total    = $IDLE_TOT W   (net switching $IDLE_SW W)"
echo "  active total   = $ACT_TOT W   (net switching $ACT_SW W)"
echo "  annotated nets = idle ${IDLE_ANN:-?}%, active ${ACT_ANN:-?}%   (activity: $MODE)"
echo "  summary        -> $OUTDIR/summary.csv"
echo "  reports        -> $OUTDIR/{synth/reports,pt}/"
