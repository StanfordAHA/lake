#!/usr/bin/env bash
# Collect synth-level + post-PnR power and area numbers across the listed
# build directories and emit a markdown comparison table.
#
# Usage: collect_power_area.sh <build_dir> [<build_dir> ...]

set -uo pipefail

# Extract a single floating value from a power report.
# $1 = path to power report (synth ptpx-synth or post-PnR ptpx-gl format)
total_power_w() {
    local rpt="$1"
    [[ -f "$rpt" ]] || { echo "NA"; return; }
    # Both ptpx-synth (power.hier) and ptpx-gl (lakespec.power.hier.rpt) put the
    # top-level `lakespec` row first after the header. Field 5 is total power.
    awk '/^lakespec / {print $5; exit}' "$rpt"
}

# Extract synth area from the genus final_area.rpt. Field 5 (Cell-Area) of the
# top-level lakespec row.
synth_area_um2() {
    local rpt="$1"
    [[ -f "$rpt" ]] || { echo "NA"; return; }
    awk '/^lakespec/ {print $4; exit}' "$rpt"
}

# Extract post-PnR signoff area from innovus signoff.area.rpt. Field 4 (Total Area).
pnr_area_um2() {
    local rpt="$1"
    [[ -f "$rpt" ]] || { echo "NA"; return; }
    awk '/^lakespec/ {print $3; exit}' "$rpt"
}

# Extract macro (SRAM) area from signoff.area.rpt — column 11.
pnr_macro_um2() {
    local rpt="$1"
    [[ -f "$rpt" ]] || { echo "NA"; return; }
    awk '/^lakespec/ {print $10; exit}' "$rpt"
}

# Pretty-print a power value (Watts → mW with 3 sig figs).
fmt_power() {
    local v="$1"
    [[ "$v" == "NA" || -z "$v" ]] && { echo "NA"; return; }
    awk -v v="$v" 'BEGIN { printf "%.3f", v * 1000 }'
}

fmt_area() {
    local v="$1"
    [[ "$v" == "NA" || -z "$v" ]] && { echo "NA"; return; }
    awk -v v="$v" 'BEGIN { printf "%.0f", v }'
}

# Header
printf "| %-60s | %s | %s | %s | %s | %s |\n" \
       "Config (short)" "Synth Pwr (mW)" "PnR Pwr (mW)" \
       "Synth Area (µm²)" "PnR Area (µm²)" "Macro (µm²)"
printf "|%s|%s|%s|%s|%s|%s|\n" \
       "$(printf -- '-%.0s' {1..62})" \
       "$(printf -- '-%.0s' {1..16})" \
       "$(printf -- '-%.0s' {1..14})" \
       "$(printf -- '-%.0s' {1..18})" \
       "$(printf -- '-%.0s' {1..16})" \
       "$(printf -- '-%.0s' {1..13})"

for d in "$@"; do
    short=$(basename "$d")

    synth_pwr=$(total_power_w "$d/13-synopsys-ptpx-synth/outputs/power.hier")
    pnr_pwr=$(total_power_w "$d/32-synopsys-ptpx-gl/reports/lakespec.power.hier.rpt")
    synth_a=$(synth_area_um2 "$d/7-cadence-genus-synthesis/results_syn/final_area.rpt")
    pnr_a=$(pnr_area_um2 "$d/21-cadence-innovus-signoff/reports/signoff.area.rpt")
    macro_a=$(pnr_macro_um2 "$d/21-cadence-innovus-signoff/reports/signoff.area.rpt")

    printf "| %-60s | %14s | %12s | %16s | %14s | %11s |\n" \
           "$short" \
           "$(fmt_power $synth_pwr)" \
           "$(fmt_power $pnr_pwr)" \
           "$(fmt_area $synth_a)" \
           "$(fmt_area $pnr_a)" \
           "$(fmt_area $macro_a)"
done
