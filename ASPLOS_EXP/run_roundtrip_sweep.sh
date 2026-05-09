#!/usr/bin/env bash
# Round-trip a list of thesis-sweep configs through lake → clockwork → lake.
#
# Usage:
#   run_roundtrip_sweep.sh <config_file> [<output_root>]
#
# config_file is a text file where each line is:
#     name|sweep-args|spec-factory-kwargs
# Lines starting with # or blank are skipped. See the inline example below.
#
# Speed: simv is compiled ONCE per config and reused across tiles. For
# multi-tile configs (fw=1 DP, banked configs) this saves ~3 min per extra
# tile vs running `make sim` independently for each.
set -o pipefail

source /cad/modules/tcl/init/bash >/dev/null 2>&1 || true
module load base >/dev/null 2>&1 || true
module load vcs/latest >/dev/null 2>&1 || true

CONFIGS_FILE="${1:?usage: $0 <config_file> [<output_root>]}"
ROOT="${2:-$(pwd)/RT_run}"
mkdir -p "$ROOT"

SUMMARY="$ROOT/summary.txt"
: > "$SUMMARY"

while IFS= read -r line; do
  case "$line" in
    ''|'#'*) continue ;;
  esac
  IFS='|' read -r NAME SWEEP_ARGS SPEC_KWARGS <<< "$line"
  CDIR="$ROOT/$NAME"
  echo
  echo "=== $NAME ===" | tee -a "$SUMMARY"
  rm -rf "$CDIR"
  mkdir -p "$CDIR/TEST"
  cd "$CDIR"

  echo "[1] thesis_sweep.py..."
  python /aha/lake/tests/test_spec/thesis_sweep.py \
      $SWEEP_ARGS --outdir TEST/ --tech GF >sweep.log 2>&1
  if [ $? -ne 0 ] || [ ! -f TEST/inputs/lake_collateral.json ]; then
    echo "  FAIL: sweep failed (see sweep.log)" | tee -a "$SUMMARY"
    echo "$NAME: FAIL_sweep" >> "$SUMMARY"
    continue
  fi

  echo "[2] roundtrip-compile..."
  python /aha/lake/pd/thesis/clockwork-roundtrip-compile/run_clockwork.py \
      --collateral TEST/inputs/lake_collateral.json \
      --app-dir /aha/Halide-to-Hardware/apps/hardware_benchmarks/tests/conv_3_3 \
      --clockwork-path /aha/clockwork --testname conv_3_3 \
      --out map_results --manifest manifest.json >compile.log 2>&1
  if [ $? -ne 0 ]; then
    echo "  FAIL: roundtrip-compile failed (see compile.log)" | tee -a "$SUMMARY"
    echo "$NAME: FAIL_compile" >> "$SUMMARY"
    continue
  fi
  TILES=( map_results/tile_*.json )
  if [ ! -e "${TILES[0]}" ]; then
    echo "  FAIL: no tiles emitted" | tee -a "$SUMMARY"
    echo "$NAME: FAIL_no_tiles" >> "$SUMMARY"
    continue
  fi
  echo "  ${#TILES[@]} tile(s)" | tee -a "$SUMMARY"

  any_fail=0
  COMMON_SIMV=""

  for TJ in "${TILES[@]}"; do
    IDX=$(basename "$TJ" .json | sed 's/^tile_//')
    echo "[3+4] tile $IDX..."
    SIM="cfg_${IDX}_sim"
    rm -rf "$SIM" "cfg_${IDX}"

    python -c "
from lake.utils.clockwork_roundtrip import write_roundtrip_artifacts
r = write_roundtrip_artifacts(dict($SPEC_KWARGS), '$TJ', 'cfg_${IDX}')
print('helper:', r)
" >"artifact_${IDX}.log" 2>&1
    rc=$?
    HSTATUS=$(grep "^helper: " "artifact_${IDX}.log" | head -1)
    if [ $rc -ne 0 ] || ! grep -q "'status': 'ok'" "artifact_${IDX}.log"; then
      echo "  tile $IDX: artifact gen FAIL ($HSTATUS)" | tee -a "$SUMMARY"
      any_fail=1
      continue
    fi

    mkdir -p "$SIM/inputs/gold" "$SIM/outputs"
    cp "cfg_${IDX}/inputs/bitstream.bs" \
       "cfg_${IDX}/inputs/comp_args.txt" \
       "cfg_${IDX}/inputs/PARGS.txt"        "$SIM/inputs/"
    cp "cfg_${IDX}/inputs/gold/"*.txt        "$SIM/inputs/gold/"
    cp TEST/inputs/lakespec.sv               "$SIM/inputs/design.v"
    cp /aha/lake/pd/thesis/synopsys-vcs-sim-rtl/{Makefile,tb.sv,test_comparison.py} "$SIM/"
    echo "// behavioral SRAM stub" >        "$SIM/inputs/sram.v"

    if [ -n "$COMMON_SIMV" ] && [ -x "$COMMON_SIMV/simv" ]; then
      # Reuse compiled simv from a prior tile (design.v, sram.v, tb.sv,
      # comp_args.txt are identical across tiles within one config).
      ln -sf "$COMMON_SIMV/simv"          "$SIM/simv"
      ln -sf "$COMMON_SIMV/simv.daidir"   "$SIM/simv.daidir"
      [ -d "$COMMON_SIMV/csrc" ] && ln -sf "$COMMON_SIMV/csrc" "$SIM/csrc"
      (cd "$SIM" && make run compare) >"sim_${IDX}.log" 2>&1
    else
      # First tile in this config: full compile + run + compare.
      (cd "$SIM" && make sim) >"sim_${IDX}.log" 2>&1
    fi

    if grep -q "Test PASSED!" "sim_${IDX}.log"; then
      echo "  tile $IDX: PASS" | tee -a "$SUMMARY"
      # Cache simv from the first passing tile for subsequent tiles.
      if [ -z "$COMMON_SIMV" ] && [ -x "$SIM/simv" ] && [ ! -L "$SIM/simv" ]; then
        COMMON_SIMV="$CDIR/_common_simv"
        mkdir -p "$COMMON_SIMV"
        # Move (not copy) to save disk: simv.daidir can be hundreds of MB.
        mv "$SIM/simv" "$COMMON_SIMV/"
        mv "$SIM/simv.daidir" "$COMMON_SIMV/"
        [ -d "$SIM/csrc" ] && mv "$SIM/csrc" "$COMMON_SIMV/"
        # Symlink back so this tile's dir still resolves.
        ln -sf "$COMMON_SIMV/simv"        "$SIM/simv"
        ln -sf "$COMMON_SIMV/simv.daidir" "$SIM/simv.daidir"
        [ -d "$COMMON_SIMV/csrc" ] && ln -sf "$COMMON_SIMV/csrc" "$SIM/csrc"
      fi
    else
      echo "  tile $IDX: FAIL (see sim_${IDX}.log)" | tee -a "$SUMMARY"
      tail -10 "sim_${IDX}.log" | sed 's/^/    | /' | tee -a "$SUMMARY"
      any_fail=1
    fi
  done

  if [ $any_fail -eq 0 ]; then
    echo "$NAME: PASS" >> "$SUMMARY"
  else
    echo "$NAME: FAIL" >> "$SUMMARY"
  fi
done < "$CONFIGS_FILE"

echo
echo "=== SUMMARY ==="
grep -E "^[A-Za-z0-9_]+:.*(PASS|FAIL)" "$SUMMARY"
