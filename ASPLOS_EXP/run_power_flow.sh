#!/usr/bin/env bash
# Drive the power-estimation flow on a single mflowgen build directory.
#
# Two paths are supported:
#   - synth : RTL sim (with VCD) → vcd2saif-rtl → ptpx-synth (synth-level power)
#   - pnr   : full PnR → GLS sim (with VCD) → vcd2saif → ptpx-gl (post-PnR power)
#   - both  : run synth first, then pnr
#
# Usage: run_power_flow.sh <build_dir> [synth|pnr|both]
#
# This script is idempotent — it preserves done-stamps for upstream steps
# (synth, RTL sim) by syncing configure.yml from .mflowgen/ and touching the
# build-dir stamps. It only forces re-run of steps it explicitly drives.

set -euo pipefail

BUILD_DIR="${1:?usage: run_power_flow.sh <build_dir> [synth|pnr|both]}"
FLOW="${2:-both}"

if [[ ! -d "$BUILD_DIR" ]]; then
    echo "ERROR: $BUILD_DIR does not exist" >&2
    exit 1
fi

cd "$BUILD_DIR"

echo "=== run_power_flow: $BUILD_DIR ($FLOW) ==="

# 1. Refresh graph and preserve existing done-stamps for upstream steps.
#    `mflowgen run --update` rewrites .mflowgen/X-name/configure.yml with new
#    timestamps. Make's setup rule then triggers a full rebuild for any step
#    whose .stamp is older than its prereq configure.yml. To keep already-built
#    steps "done", we copy the refreshed configure.yml in (so internal step
#    numbers are correct) and future-date every stamp the make graph touches.
mflowgen run --update >/dev/null
PRESERVE_STEPS=(
    0-constraints
    1-custom-genus-scripts
    2-gf12-adk
    4-rtl
    6-gen_sram_macro
    7-cadence-genus-synthesis
)
preserve_step() {
    local d="$1"
    [[ -d "$d" && -f ".mflowgen/$d/configure.yml" ]] || return 0

    cp -f ".mflowgen/$d/configure.yml" "$d/configure.yml"
    # Future-date every stamp file in this step so make sees them all newer
    # than the freshly-rewritten .mflowgen/X/configure.yml. Make rules pull
    # on `.stamp`, `.execstamp`, `.postconditions.stamp`, plus per-file
    # `outputs/.stamp.<file>` and `outputs/.execstamp.<file>` markers — touch
    # them all. The `\(...\)` grouping is required so `-print0` applies to
    # every alternative, not just the last one.
    while IFS= read -r -d '' stamp; do
        touch -d "+1 day" "$stamp"
    done < <(find "$d" -maxdepth 3 \( \
                -name '.stamp' -o \
                -name '.execstamp' -o \
                -name '.postconditions.stamp' -o \
                -name '.stamp.*' -o \
                -name '.execstamp.*' \
             \) -print0 2>/dev/null)
}
for d in "${PRESERVE_STEPS[@]}"; do
    preserve_step "$d"
done

# Helper: resolve step number by name from `make list`.
resolve_step() {
    local name="$1"
    make list 2>/dev/null \
        | awk -v n="$name" '$0 ~ "[0-9]+ : "n"$" {print $2; exit}'
}

# Force-enable VCD dump on the named sim step by patching its mflowgen-run.
enable_vcd_dump() {
    local step_name="$1"
    local step_num
    step_num=$(resolve_step "$step_name")
    local run_script=".mflowgen/${step_num}-${step_name}/mflowgen-run"
    if [[ ! -f "$run_script" ]]; then
        echo "ERROR: cannot find $run_script" >&2
        exit 1
    fi
    sed -i 's/^export dump_vcd=.*/export dump_vcd=1/' "$run_script"
}

run_synth_power() {
    echo "--- Synth-level power flow ---"
    local sim_step vcd2saif_step ptpx_step
    sim_step=$(resolve_step synopsys-vcs-sim-rtl)
    vcd2saif_step=$(resolve_step synopsys-vcd2saif-convert-rtl)
    ptpx_step=$(resolve_step synopsys-ptpx-synth)

    echo "  sim=$sim_step  vcd2saif=$vcd2saif_step  ptpx=$ptpx_step"

    enable_vcd_dump synopsys-vcs-sim-rtl

    # Force re-run of sim (need fresh VCD), then drive vcd2saif and ptpx-synth.
    make "clean-${sim_step}" >/dev/null
    make "${sim_step}" "${vcd2saif_step}" "${ptpx_step}"

    local report_dir="${ptpx_step}-synopsys-ptpx-synth"
    if [[ -f "${report_dir}/outputs/power.hier" ]]; then
        echo "  ✓ Synth-level power report: ${BUILD_DIR}/${report_dir}/outputs/power.hier"
    fi
}

run_pnr_power() {
    echo "--- Post-PnR power flow ---"
    local signoff_step gls_step vcd2saif_step ptpx_step
    signoff_step=$(resolve_step cadence-innovus-signoff)
    gls_step=$(resolve_step synopsys-vcs-sim)
    vcd2saif_step=$(resolve_step synopsys-vcd2saif-convert)
    ptpx_step=$(resolve_step synopsys-ptpx-gl)

    echo "  signoff=$signoff_step  gls=$gls_step  vcd2saif=$vcd2saif_step  ptpx=$ptpx_step"

    enable_vcd_dump synopsys-vcs-sim

    # Drive the full PnR + GLS + power chain. Make resolves dependencies.
    make "${signoff_step}" "${gls_step}" "${vcd2saif_step}" "${ptpx_step}"

    local report_dir="${ptpx_step}-synopsys-ptpx-gl"
    if [[ -d "${report_dir}/reports" ]]; then
        echo "  ✓ Post-PnR power reports: ${BUILD_DIR}/${report_dir}/reports/"
    fi
}

case "$FLOW" in
    synth) run_synth_power ;;
    pnr)   run_pnr_power ;;
    both)  run_synth_power; run_pnr_power ;;
    *)     echo "ERROR: flow must be synth, pnr, or both (got '$FLOW')" >&2; exit 1 ;;
esac

echo "=== run_power_flow: done ($BUILD_DIR) ==="
