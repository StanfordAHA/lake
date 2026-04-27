# Thesis Sweep — Replicating the Physical Design Flow

A living document for generating an mflowgen build for one configuration of the
hardware block, then driving it through synthesis and (eventually) backend.

Last verified end-to-end through **post-PnR power estimation (`synopsys-ptpx-gl`)**
on **2026-04-26** for 6 configs. RTL sim (step 8) and the synth-level + post-PnR
power flows (§7) are wired and reproducible.

---

## 1. Pick a configuration and generate the mflowgen graph

Run from the repo root (`/home/mstrange/lake`).

```bash
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS/PORT_EXP \
    --design_filter thesis_sweep \
    --physical \
    --fetch_width 4 \
    --in_ports 2 \
    --out_ports 2 \
    --data_width 16 \
    --storage_capacity 8192 \
    --frequency 700
```

Knob → meaning:

| Flag | Meaning | Notes |
| --- | --- | --- |
| `--build_dir` | Root for all generated mflowgen builds | One subdir per config |
| `--design_filter` | Picks which test spec drives RTL generation | `thesis_sweep` → `tests/test_spec/thesis_sweep.py` |
| `--physical` | Use the physical-tech-mapped storage (GF12 SRAM macro) | Off → behavioral memory |
| `--fetch_width` | Vector width of the SRAM port | 4 here means 4 × `data_width` bits per SRAM word |
| `--in_ports` / `--out_ports` | Number of streaming input/output ports | |
| `--data_width` | Word width in bits | |
| `--storage_capacity` | SRAM capacity in **bytes** (not entries) | See §6 for the units gotcha |
| `--frequency` | Target clock frequency in MHz | Internally translated to `clock_period` ps |

The script prints the build dir it created and the `mflowgen run` command it
ran on your behalf. For the example above:

```
/sim/mstrange/THESIS_BUILDS/PORT_EXP/thesis_sweep_700/\
storage_cap_8192_data_width_16_ccw_64_dim_6_fw_4_inp_2_outp_2
```

The corresponding "design folder" (graph definition) lands under
`gf_physical_design/NEW/thesis_sweep_<freq>/<config>_<freq>/`.

> **Re-running with the same args is safe.** The wrapper preserves existing
> step numbering so unchanged steps don't rebuild. Use `make clean-all` for a
> truly clean slate.

---

## 2. Inspect the graph

```bash
cd /sim/mstrange/THESIS_BUILDS/PORT_EXP/thesis_sweep_700/<config-dir>
make list      # all targets, including debug-N
make status    # which steps are built / need building
make graph     # write a PDF of the dependency DAG
```

As of **2026-04-26** the graph contains 33 steps (0–32). Step numbering shifted
when `synopsys-vcd2saif-convert-rtl` was added — always resolve by name with
`make list | grep <step-name>` rather than hard-coding numbers.

Steps verified end-to-end:

| # | Step | What it does | Wall (fw4/2×2/8KB/700MHz) |
| --- | --- | --- | --- |
| 0 | `constraints` | SDC + clock period from `--frequency` | <1 s |
| 1 | `custom-genus-scripts` | Project-local Genus tcl | <1 s |
| 2 | `gf12-adk` | ADK for GF12 | <1 s |
| 4 | `rtl` | Runs `tests/test_spec/thesis_sweep.py` → `lakespec.sv` + `lake_collateral.json` + bitstream + gold | ~3 s |
| 6 | `gen_sram_macro` | Builds the SRAM macro views | ~2:30 |
| 7 | `cadence-genus-synthesis` | Genus synthesis | ~8 min |
| 8 | `synopsys-vcs-sim-rtl` | RTL VCS sim against gold (passes/fails on data + count match) | ~30 s |
| 10 | `synopsys-vcd2saif-convert-rtl` | RTL VCD → SAIF (only emitted if `+dump_vcd=1`) | ~5 s |
| 12 | `cadence-innovus-init` | Innovus floorplan init | |
| 14 | `cadence-innovus-power` | Power planning (rings/stripes) | |
| 15 | `cadence-innovus-place` | Placement | |
| 16 | `cadence-innovus-cts` | Clock tree synthesis | |
| 17 | `cadence-innovus-postcts_hold` | Post-CTS hold fix | |
| 18 | `cadence-innovus-route` | Routing | |
| 19 | `cadence-innovus-postroute` | Post-route opt | |
| 20 | `cadence-innovus-postroute_hold` | Post-route hold fix | |
| 21 | `cadence-innovus-signoff` | Signoff (extracts SPEF, writes design.vcs.v + design.sdf) | full PnR ≈ 45–90 min |
| 25 | `synopsys-vcs-sim` | Gate-level (SDF-annotated) sim, dumps VCD | ~5–10 min |
| 30 | `synopsys-vcd2saif-convert` | GLS VCD → SAIF | ~5 s |
| 13 | `synopsys-ptpx-synth` | PT power on synth netlist + RTL-derived SAIF | ~40 s |
| 32 | `synopsys-ptpx-gl` | PT power on signoff netlist + GLS-derived SAIF | ~30 s |

Other steps (`mentor-calibre-drc`, `mentor-calibre-lvs`, `synopsys-pt-timing-signoff`,
`mentor-calibre-gdsmerge`, `synopsys-ptpx-genlibdb`, `verif_post_synth`,
`verif_post_layout`, `synopsys-pt-power`, `cadence-innovus-debug-calibre`) exist
in the graph but are not yet on the verified path.

---

## 3. Run synthesis

```bash
cd /sim/mstrange/THESIS_BUILDS/PORT_EXP/thesis_sweep_700/<config-dir>
make 7
```

mflowgen runs all upstream deps (0, 1, 2, 4, 6) before kicking off Genus.

### Where the artifacts land

```
7-cadence-genus-synthesis/
├── results_syn/
│   ├── final.rpt           # combined report
│   ├── final_area.rpt
│   ├── final_gates.rpt
│   ├── final_time.rpt
│   └── final_qor.rpt       # WNS/TNS, total cell area
├── outputs/
│   ├── design.v            # synthesized netlist  (handed to Innovus)
│   ├── design.sdc
│   ├── design.sdf
│   ├── design.spef
│   └── design.namemap
├── logs/                   # genus tool logs
└── mflowgen-run.log        # one-stop combined log
```

Sanity check QoR:
```bash
grep -E "WNS|TNS|Slack|Cell Area|Total" \
  7-cadence-genus-synthesis/results_syn/final_qor.rpt
```

For the reference 8KB / fw4 / 2in / 2out / 700 MHz config: WNS 0 / TNS 0,
Total Cell Area ≈ 12244 µm² (Cell+Physical+Net ≈ 13914 µm²).

---

## 4. RTL simulation (step 8)

```bash
cd <build-dir>
make 8     # synopsys-vcs-sim-rtl
```

The step compiles `tb.sv` from `pd/thesis/synopsys-vcs-sim-rtl/` (mirror of
`tests/test_spec_hw/tb.sv`), runs VCS in batch with the per-config bitstream and
PARGS, then `test_comparison.py` compares the read-port memh dump against
`gold/`. Postcondition asserts `'PASS' in mflowgen-run.log` and `'FAIL' not in`.

Knobs (all set via `pd/thesis/synopsys-vcs-sim-rtl/`):
- `TOOL=VCS` (default; `XCELIUM` is also wired but the Xcelium license has been
  unreliable — see §5.4)
- `DUMP_VCD=0|1` make var, threaded through to `+dump_vcd=$(DUMP_VCD)` plusarg
  in tb.sv. Off by default — sweeps that only need pass/fail don't pay the
  multi-GB VCD cost. Wired into the configure.yml as a step parameter and into
  `mflowgen-run` via `export dump_vcd=…`. The wrapper script (§7) flips it to
  1 automatically when running the power flow.

Expected: `Test PASSED!` in `8-synopsys-vcs-sim-rtl/mflowgen-run.log` for all
fw≥2 configs and (post-2026-04-26 fixes) all fw=1 configs that go through
`get_linear_test_generic`. See §5 gotchas for fw=1 history.

---

## 5. Common gotchas

### 5.1 RTL-gen failures look like a generic assertion

`ASPLOS_EXP/create_all_experiments.py` launches RTL generation as parallel
subprocesses. **As of 2026-04-24** each subprocess writes its full stdout/stderr
to `<outdir>/rtl_gen.log`; if a subprocess returns non-zero, the wrapper dumps
that file inline before raising. To debug an RTL-gen failure:

```bash
# Tail of the make output already includes the inline traceback.
# To re-run a single config interactively, copy the printed
# "python /home/mstrange/lake/.../thesis_sweep.py ..." command and run it.
```

Earlier versions piped stdout/stderr to `DEVNULL`, which collapsed every
failure into `AssertionError: Proc returned bad value...`. If you see that
again, the regression is in `create_all_experiments.py` around the
`subprocess.Popen(...)` call.

### 5.2 `storage_capacity` units

The CLI's `--storage_capacity` is in **bytes**.

`lake_collateral.json`'s `capacity[ctrl]` is in **entries** — bytes are
divided out at serialization (`lake/spec/spec.py:1190-1192`):

```python
collateral['capacity'][ctrl] = cap_bytes // (word_width[ctrl] * data_width_bytes)
```

To convert back, multiply by `word_width[ctrl] * (data_width // 8)`.

The validator in `tests/test_spec/thesis_sweep.py:validate_generated_outputs`
does this conversion before comparing against `expected_storage_capacity`.
Fixed on 2026-04-24 — earlier versions compared bytes against entries and
falsely rejected every config where `word_width × data_width_bytes ≠ 1`.

### 5.3 Resuming after a clean

`make clean-N` only removes step N. To force everything to rebuild:

```bash
make clean-all
```

Don't `rm -rf` numbered subdirs by hand — the stamp files in `.mflowgen/`
get out of sync with the filesystem.

### 5.4 `mflowgen run --update` invalidates downstream stamps

If you change `pd/thesis/construct-commercial-full.py` (e.g. add a step) and
re-run `mflowgen run --update` in an existing build dir, every step's
`.mflowgen/<N>-<step>/configure.yml` gets a fresh mtime. Make's setup rule
`<N>-<step>/.stamp: .mflowgen/<N>-<step>/configure.yml` then triggers a full
`rm -rf <N>-<step>; cp -aL …` rebuild for every step — wiping synth/sim work
that's still valid.

**Workaround used by `run_power_flow.sh`:** after `mflowgen run --update`,
copy each preserved step's new `configure.yml` into the build-dir copy AND
future-date the build-dir stamps (`.stamp`, `.execstamp`, `.postconditions.stamp`,
plus all `outputs/.stamp.*` and `outputs/.execstamp.*`) by 1 day. Make then
sees the build-dir as "newer" and skips re-setup. The configure.yml content
diff is only step-number references in `edges_o`, which don't affect the
step's actual work.

### 5.5 fw=1 + single-port SRAM = silent data corruption

Single-port SRAM macros (`IN12LP_S1DB_*`) can do exactly one op per cycle. The
old `get_linear_test_generic` (vec_width=1 path) scheduled writes at every
cycle starting cycle 1 and reads at every cycle starting cycle 17 — overlap
caused write to win arbitration and the read port returned X for ~48 cycles
before catching up. TB count check passed; data check failed. Fixed
2026-04-26 in `tests/test_spec/thesis_sweep.py:get_linear_test_generic`:

- write/read schedule strides changed `[1] → [4]`, write offset `1 → 4` so
  writes/reads sit on different mod-4 cycle phases (mirrors `get_linear_test`).
- read `vec_out_config` offset `19 → 18` because there's no PISO vectorizer
  for vec_width=1 — gold computation was inserting a phantom 1-cycle delay.

There's also a bitstream-time guard in `lake/spec/iteration_domain.py:gen_bitstream`
that raises `ValueError` if an extent overflows the configured `extent_width`,
so silent truncation can't return.

### 5.6 Innovus license contention with parallel PnR

License is "8 CPU jobs" but each Innovus instance forks ~24 worker threads.
**5+ concurrent Innovus instances starves the license / kernel** and scripts
randomly die mid-flight (see 2026-04-26 power sweep — 4 of 5 parallel runs
silently exited at innovus-init).

Safe pattern: ≤3 concurrent PnR jobs. The wrapper at §7 doesn't enforce this
— if you fan it out across many configs, either use `--jobs 3` semantics or
run them as a make `-j` pipeline.

---

## 6. End-to-end smoke test (synthesis + RTL sim)

```bash
# 1. Generate the graph (full sweep, then pick one config)
cd /home/mstrange/lake
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS_V2/PORT_EXP \
    --design_filter thesis_sweep --physical \
    --fetch_width 4 --in_ports 2 --out_ports 2 \
    --data_width 16 --storage_capacity 8192 --frequency 700

# 2. Drive synthesis + RTL sim
cd /sim/mstrange/THESIS_BUILDS_V2/PORT_EXP/thesis_sweep_700/\
storage_cap_8192_data_width_16_ccw_64_dim_6_fw_4
make 7 8

# 3. Confirm
make status
grep -E "WNS|TNS|Cell Area" \
  7-cadence-genus-synthesis/results_syn/final_qor.rpt
grep "Test PASSED" 8-synopsys-vcs-sim-rtl/mflowgen-run.log
```

Total wall-clock through step 8: ≈ **11–12 min** (rtl 3 s, gen_sram_macro 2:30,
genus 7:46, vcs-sim 30 s).

---

## 7. Power estimation flow (synth-level + post-PnR)

Two paths share most of the graph. Both end up at a `power.hier` report;
the synth-level path is fast (~3 min on top of synth+sim), the post-PnR path
needs a full Innovus PnR (~45–90 min) plus GLS sim.

### 7.1 Graph wiring

`pd/thesis/construct-commercial-full.py` defines two `vcd2saif-convert`
instances:

```
   ┌── 8 vcs-sim-rtl ──→ 10 vcd2saif-convert-rtl ──┐
   │                                               ├→ 13 ptpx-synth (synth power)
   │                                  7 synth ─────┘
   │                                               
   └─→ 9 flowsetup → 12-21 PnR → 25 vcs-sim (GLS)─→ 30 vcd2saif-convert ─┐
                                                                          ├→ 32 ptpx-gl (post-PnR power)
                                                       21 signoff ───────┘
```

Power numbers come from:
- Synth-level: `<N>-synopsys-ptpx-synth/outputs/power.{hier,cell}`
- Post-PnR: `<N>-synopsys-ptpx-gl/reports/lakespec.power.{rpt,hier.rpt}` plus
  `lakespec.activity.{pre,post}.rpt`, `lakespec.parasitics.rpt`, etc.

Area numbers come from:
- Synth: `7-cadence-genus-synthesis/results_syn/final_area.rpt`
  (top row: Cell-Area / Net-Area / Total-Area)
- Post-PnR: `21-cadence-innovus-signoff/reports/signoff.area.rpt`
  (top row: Total Area / per-class breakdown / Macro = SRAM footprint)

### 7.2 Wrapper: `run_power_flow.sh`

```bash
ASPLOS_EXP/run_power_flow.sh <build_dir> [synth|pnr|both]
```

What it does:
1. `mflowgen run --update` to refresh the graph (picks up any
   `construct-commercial-full.py` changes and renumbers steps).
2. Future-dates stamps for steps 0,1,2,4,6,7 so existing synth/sim work isn't
   wiped (see §5.4).
3. Resolves step numbers by name from `make list` (do NOT hard-code; numbers
   shift when steps are added).
4. Sets `+dump_vcd=1` on the relevant sim step's `mflowgen-run` so the TB
   produces a non-empty `waveforms.vcd` (vcd2saif then converts to SAIF).
5. For `synth`: `clean-<sim>; make <sim> <vcd2saif-rtl> <ptpx-synth>`.
   For `pnr`: `make <signoff> <gls-sim> <vcd2saif> <ptpx-gl>` — make resolves
   the full PnR DAG.
6. `both` runs synth then pnr.

The script is idempotent: re-running on a finished config just re-checks
stamps and exits quickly.

### 7.3 Collecting results: `collect_power_area.sh`

```bash
ASPLOS_EXP/collect_power_area.sh <build_dir> [<build_dir> ...]
```

Emits a single markdown table (synth power / PnR power / synth area / PnR
area / Macro area). Pure read-only — pulls fields straight from the report
files listed in §7.1.

### 7.4 Reference numbers (6 configs, 2026-04-26, 700 MHz target)

| Config | Synth Pwr (mW) | PnR Pwr (mW) | Synth Area (µm²) | PnR Area (µm²) | Macro (µm²) |
|---|---:|---:|---:|---:|---:|
| `cap_8192 dw_16 fw_4` (baseline 2×2) | 2.020 | 0.667 | 12,245 | 12,234 | 7,310 |
| `cap_8192 dw_16 fw_4 vc_4` | 2.010 | 0.689 | 12,797 | 12,784 | 7,310 |
| `cap_8192 dw_32 fw_4` | 2.180 | 0.915 | 14,354 | 14,139 | 8,769 |
| `cap_4096 dw_16 fw_4 inp2outp2` | 1.930 | 0.642 |  9,696 |  9,680 | 4,798 |
| `cap_16384 dw_16 fw_4 inp2outp2` | 2.020 | 0.737 | 17,305 | 17,289 | 12,334 |
| `cap_8192 dw_16 fw_8 inp4outp4` | 2.160 | 0.779 | 19,657 | 19,551 | 8,769 |

Observations:
- Post-PnR power lands at **0.33–0.42×** the synth-level estimate. Synth-level
  is wireload-based with no real clock tree, so it dominates internal power
  through the SRAM model (~88%); post-PnR adds a real clock tree (~33% of
  total) and properly extracted SPEF.
- Synth and post-PnR area agree to within 1–2% — the synth `Cell-Area`
  estimate is good. SRAM `Macro` area scales with the chosen SRAM macro
  (`fw·dw`-bit-wide × `cap/(fw·dw_bytes)`-deep).

### 7.5 End-to-end smoke test (power flow, 1 config)

```bash
cd /home/mstrange/lake
python ASPLOS_EXP/create_mflowgen_experiments.py \
    --build_dir /sim/mstrange/THESIS_BUILDS_V2/PORT_EXP \
    --design_filter thesis_sweep --physical \
    --fetch_width 4 --in_ports 2 --out_ports 2 \
    --data_width 16 --storage_capacity 8192 --frequency 700

BD=/sim/mstrange/THESIS_BUILDS_V2/PORT_EXP/thesis_sweep_700/storage_cap_8192_data_width_16_ccw_64_dim_6_fw_4

# Synth + RTL sim first (cheap)
( cd "$BD" && make 7 8 )

# Synth-level power (~3 min once synth+sim done)
ASPLOS_EXP/run_power_flow.sh "$BD" synth

# Post-PnR power (full PnR + GLS sim; ~50–90 min single config)
ASPLOS_EXP/run_power_flow.sh "$BD" pnr

# Or both at once
ASPLOS_EXP/run_power_flow.sh "$BD" both

# Build the comparison table (any number of configs)
ASPLOS_EXP/collect_power_area.sh "$BD" <other-build-dirs...>
```

### 7.6 Fanning out across configs

For a many-config sweep:
- **Don't** parallelize >3 PnR jobs at once (§5.6 license contention).
- Synth-level power can safely run with high parallelism (PT-shell is light).
- Suggested pattern: drive `run_power_flow.sh <bd> synth` across all configs
  with `--jobs 8`, then `pnr` with `--jobs 3` (or serial overnight).
- A future `--phase power` mode in `run_synth_pool.py` could fan this out;
  not yet wired.

---

## 8. Updating this doc

Whenever a new step in the flow is verified end-to-end, extend the table in §2
with the runtime + artifact locations and bump the "verified through" date at
the top. If you trip over a new gotcha, add it to §5.
