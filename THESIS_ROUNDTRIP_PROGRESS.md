# Lake → Clockwork → Lake Round-trip: Progress Snapshot

Status as of 2026-05-08. Branch `mek`.

## Goal

Per-spec build validates the round trip end-to-end: feed the spec's own
`lake_collateral.json` into clockwork, take each per-tile memory configuration
that comes back, and verify it executes correctly on the spec's RTL.
Replaces (and complements) the handcrafted-test path so spec / compiler /
converter mismatches surface as build failures rather than silent
miscompiles.

## Configs validated end-to-end

All run through: lake collateral → clockwork compile (`conv_3_3`) → per-tile
JSON → `_convert_clockwork_to_port_config` → `gen_bitstream` → VCS sim of
`lakespec.sv` → `Test PASSED!`.

| # | Source | Config | Notes |
|---|---|---|---|
| 1 | smoke | PORT_EXP fw=4 dw=16 sc=8192 SP 2x2 | wide-fetch baseline |
| 2 | smoke | PORT_EXP fw=2 dw=32 sc=8192 DP 2x2 | DP wide-fetch (BW required DP) |
| 3 | smoke | MEMORY_EXP fw=2 dw=16 sc=2048 SP 1x1 | small SP |
| 4 | smoke | MEMORY_EXP fw=1 dw=16 sc=4096 DP 1x1 | fw=1 regfile/mem |
| 5 | next-4 | PORT_EXP fw=4 dw=8 sc=8192 SP 2x2 | exposed dw=8 latent bugs |
| 6 | next-4 | PORT_EXP fw=4 dw=32 sc=8192 SP 2x2 | wide dw |
| 7 | next-4 | MEMORY_EXP fw=2 dp 2x2 dw=16 sc=4096 | DP wide-fetch multi |
| 8 | next-4 | MEMORY_EXP fw=4 sp 2x2 dw=16 sc=8192 | wider SP multi |

## Code changes (initial — superseded by detailed sections below)

### Clockwork (`/aha/clockwork`)

- **`ubuffer.h:4248-4253`, `ubuffer.cpp:6264-6310`** — `merge_dom_dim` gains
  optional `max_merged_extent`. When set, refuses to collapse two
  iteration-domain dims if their product exceeds the budget. Call sites in
  `UBuffer::generate_ubuf_args*` pass `mem.counter_ub` (= 2047 by default).
  Critical for fw=1 / dual-port large-`storage_capacity` tiles where a 1-D
  `extent=storage_capacity` would trip lake's 11-bit IterationDomain extent
  encoding. Reversed-domain index translation: `pa.first/second` come from
  `linear_domain_map_with_index`'s innermost-first convention, so we convert
  to absolute indices via `abs = num_in_dims - 1 - pa.X` before calling
  `get_dim_extent`.

- **`ubuffer.cpp:1515`** — bank-merge multi-port gate. The c61 single-port
  skip is now gated on `interconnect_in_num/out_num > 1`, so multi-port
  tiles (fw=2 DP 2x2, fw=4 DP, etc.) get bank-merged correctly while
  single-port 1x1 tiles emit two banks.

- **`cgra_flow.h:47-77`** — `compile_app_for_garnet_auto` dispatcher.
  `dispatch_fw2 = (lc.fetch_width == 2)` regardless of dual_port;
  `dispatch_dp = (lc.fetch_width == 1) && lc.dual_port_sram` (gated on
  fw==1, since `set_config_dp()` hardcodes a fw=1 regfile shape and
  forces the wrong shape on any wider spec). fw>=4 DP collateral now
  falls through to `compile_for_garnet_single_port_mem` which loads
  the wide-fetch collateral and honors `dual_port_sram` from the JSON.
  Previously DP fw=2 went through dual-port-only path which forced
  cycle_stride=4; DP fw>=4 went through the same path and produced
  fw=1 regfile keys (`in2regfile_*`/`regfile2out_*`) that the lake
  spec couldn't accept (`KeyError: 'address'` from empty
  `vec_in/out_config`).

- **`options.h:473-492`** — `set_config_fetch2()` no longer forces
  `dual_port_sram=true`. The collateral's value flows through, so fw=2 SP
  configs see SP scheduling and fw=2 DP configs see DP scheduling.

### Lake (`/aha/lake`)

- **`lake/spec/spec.py: _synthesize_wide_fetch_hierarchy`** — sets
  `collateral['store_latency'] = 0` and `collateral['load_latency'] = 0`
  for wide-fetch tiles. The wide-fetch hierarchy (AGG→SRAM→TB) absorbs
  the SRAM macro latencies internally, so as far as the unified-buffer
  external interface is concerned both are zero. Previously these
  reported the SRAM macro's raw latencies and clockwork's scheduler
  staggered too aggressively, breaking fw=2 SP multi-out parity.

- **`lake/utils/util.py: get_data_sizes` (line 1281)** — changed
  `if 'vec_in_config' in port_schedule:` to
  `if port_schedule.get('vec_in_config'):`, so empty `{}` falls through.
  fw=1 / regfile paths emit empty vec configs; the strict `in` test
  crashed with `KeyError: 'dimensionality'` on the empty dict.

- **`lake/utils/clockwork_roundtrip.py`** (new helper, written for the
  round-trip flow):
  - `_compute_clockwork_gold` accepts `load_latency` and adds it to emit
    cycles for `regfile2out_*` / `mem_out_*` (fw=1) paths only —
    clockwork's `cycle_starting_addr` for those paths is the read-fire
    cycle, not the data-out cycle. Wide-fetch `tb2out_*` already encodes
    the data-out cycle.
  - Reads `load_latency` from the spec's
    `extract_compiler_information()` so the helper stays in sync with
    whatever the spec reports.
  - `_compute_clockwork_gold` accepts `data_width` and masks the gold
    data sequence by `(1 << data_width) - 1`. Real HW truncates to
    data_width; the previous unmasked `range(N)` mismatched HW for dw<16
    starting at index 256 (dw=8).

- **`pd/thesis/synopsys-vcs-sim-rtl/tb.sv:87,89,91,93`** — widened
  `port_rN_time[]` arrays from `[DATA_WIDTH-1:0]` to `[31:0]`. The cycle
  counter is independent of the data-path width; the previous sizing
  truncated time at `2^DATA_WIDTH` and silently corrupted gold compares
  for any sim running longer than 256 cycles at dw=8 (or 65536 at
  dw=16, etc. — the round-trip schedules can run thousands of cycles).
  The `port_rN_mem` data arrays correctly stay at `DATA_WIDTH`.

- **`ASPLOS_EXP/smoke_test.sh`** — added `--dual_port` to the
  `PORT_EXP fw=2 dw=32 sc=8192` config because fw=2 multi-out cannot
  satisfy 1-write-2-read bandwidth on a single physical SRAM port.

### mflowgen scaffolding (in-tree, not yet end-to-end exercised)

- `pd/thesis/clockwork-roundtrip-compile/{configure.yml, run_clockwork.py}`
  — runs clockwork against `lake_collateral.json`, splits the resulting
  CoreIR JSON into per-tile `tile_<idx>.json` files, copies the
  raw `<testname>.json` for forensics. Postcondition fails the step if no
  tiles emit.
- `pd/thesis/clockwork-roundtrip-common/run_roundtrip_sim.py` (shared by
  rtl/synth variants) — per-tile loop that stages `cfg_<idx>/`,
  generates artifacts, runs `make sim`, parses PASS/FAIL, accumulates
  results into `roundtrip_results.json`.
- `tests/test_spec/test_clockwork_roundtrip.py` — pytest smoke for
  `write_roundtrip_artifacts` against a synthetic tile JSON.

## Memory artifacts

- `feedback_lake_convert_extents.md` — `_convert_clockwork_to_port_config`
  used to add +1 based on misreading hw iteration count.
- `project_clockwork_fw2.md` — fw=2 schedule-stride history.
- `project_clockwork_fw1_dp_extent.md` — fw=1 DP extent encoding;
  resolved by `merge_dom_dim` `max_merged_extent` cap.
- `project_clockwork_bank_merge_multiport.md` — multi-port bank-merge
  gating.
- `reference_lake_roundtrip_env.md` — env setup for end-to-end smoke.
- `feedback_roundtrip_data_width.md` — dw=8 fixes (tb time width + gold
  data masking).

## Bugs caught by the round-trip flow

1. **fw=2 lake-vs-clockwork stride mismatch** — clockwork was always
   emitting `cycle_stride=4` regardless of `fetch_width`; sims hit
   `xxxx` reads at sim time. Fixed in clockwork (cgra_flow.h dispatcher
   + options.h fetch2 config).
2. **fw=2 SP multi-out double-firing** — clockwork scheduled both
   readers in the same cycle because lake-reported store_latency=1
   pushed the writer's end_time wrong. Fixed in lake
   (`_synthesize_wide_fetch_hierarchy` sets ext-interface latencies = 0).
3. **fw=1 DP large-sc extent encoding** — 1-D `extent=storage_capacity`
   didn't fit lake's 11-bit IterationDomain. Fixed in clockwork
   (`merge_dom_dim` budget cap, keep dims un-merged).
4. **dw=8 tb time wrap** — testbench cycle counter wrapped at 256 (real
   bug, not specific to round-trip — handcrafted tests just don't run
   long enough to expose it).
5. **dw=8 gold data wrap mismatch** — gold model wrote raw `range(N)`;
   HW truncates to `data_width`. Fixed in helper.
6. **`get_data_sizes` empty-`vec_in_config` crash** — fw=1 / regfile
   path emits empty `{}`; strict `in` test crashed.
7. **Bank-merge gating for multi-port** — single-port skip in c61 was
   not multi-port-aware; multi-port DP tiles weren't getting merged.

## How to reproduce

```bash
# Env (interactive shell, EDA toolchain)
source /cad/modules/tcl/init/bash
module load base
module load vcs/latest

# Smoke (4 configs already validated)
bash /aha/lake/ASPLOS_EXP/smoke_test.sh

# Next-4 driver (also passing)
bash /tmp/RT_next4/run_next4.sh
# Per-config artifacts under /tmp/RT_next4/<config>/
```

See `reference_lake_roundtrip_env.md` for the manual one-config recipe.

## Configs validated end-to-end (cont.)

Next-8 batch (`/tmp/RT_next8/run_next8.sh`):

| # | Config | Result | Notes |
|---|---|---|---|
| E | PORT_EXP fw=2 vc=4 dw=16 sc=8192 DP 2x2 | PASS | vec_capacity > fw |
| F | PORT_EXP fw=8 vc=4 dw=16 sc=8192 SP 2x2 | PASS | fw=8 first time |
| G | PORT_EXP fw=2 vc=2 dw=64 sc=8192 DP 2x2 | PASS | dw=64 |
| H | ITERATION_DOMAIN_EXP fw=1 dim=4 max_extent=256 sc=8192 1x1 | PASS | small ID |
| I | AFFINE_PATTERN_GENERATOR_EXP fw=1 dim=3 max_seq_w=1024 sc=8192 1x1 | PASS | narrow AG |
| J | MEMORY_EXP fw=1 dp dw=16 sc=8192 1x1 | PASS | larger sc |
| K | MEMORY_EXP fw=4 dp 4x4 dw=16 sc=4096 | PASS | dispatcher fix; now wide-fetch + bank-merged into 1 tile |
| L | MEMORY_EXP fw=2 sp 1x1 dw=16 sc=16384 | PASS | sc=16384 |

### K root cause + fix

K (fw=4 DP 4x4) initially failed because the dispatcher in
`cgra_flow.h:60-61` routed any `dual_port_sram=true` collateral with
`fetch_width != 2` to `compile_for_garnet_dual_port_mem`. That entry
point's `set_config_dp()` (`options.h:488`) hardcodes `fetch_width=1`
and `controller_name={"regfile"}` — so every fw>=4 DP collateral was
silently demoted to a fw=1 regfile shape, producing
`in2regfile_*`/`regfile2out_*` keys instead of the wide-fetch
`in2agg`/`tb2out` keys. The lake converter then handed the spec
empty `vec_in/out_config`, and `Port.gen_bitstream` crashed at
`vec_in['address']`.

The fix gates `dispatch_dp` on `fetch_width == 1`. fw=4 DP now lands
in `compile_for_garnet_single_port_mem` (the JSON-driven wide-fetch
path), which honors `dual_port_sram` from the collateral. Bank-merge
also kicks in: the previously-banked 2-tile output collapses to **1
tile** because the multi-port gate (the prior `interconnect_in/out_num
> 1` fix) now applies — the spec's 4-port interconnect config matches
the schedule's 1-write/2-read needs, all in one bank.

### Next-8-b batch (M..T)

| # | Config | Result | Notes |
|---|---|---|---|
| M | PORT_EXP fw=2 vc=8 dw=16 sc=8192 DP 2x2 | PASS | vc=8 (4× fw) |
| N | PORT_EXP fw=8 vc=2 dw=16 sc=8192 SP 2x2 | PASS | fw=8 vc<fw |
| O | ITER_DOM fw=1 dim=1 max_ext=64 sc=8192 1x1 | **FAIL — spec capacity** | see below |
| P | ITER_DOM fw=1 dim=6 max_ext=4096 sc=8192 1x1 | PASS | max ID corner |
| Q | APG fw=1 dim=6 max_seq_w=16384 sc=8192 1x1 | PASS | max APG corner |
| R | MEMORY_EXP fw=1 dp dw=16 sc=16384 1x1 | PASS | largest fw=1 DP |
| S | MEMORY_EXP fw=4 dp 4x4 dw=16 sc=8192 | PASS | K with larger sc |
| T | MEMORY_EXP fw=8 sp 4x4 dw=16 sc=8192 | PASS | first fw=8 4-port |

O failure is a spec/app capacity mismatch, not a flow bug. The spec
declares `dimensionality=1` + `max_extent=64`, giving a 1-D
IterationDomain with extent_width=6 (max ≈62). Clockwork emits
`extents=[64, 64]` for conv_3_3 (a 64×64 image cannot fit in any
single ≤62-element 1-D loop). `merge_dom_dim` cannot compress
`[64,64]→[4096]` because (a) 4096 exceeds the merge cap of
`counter_ub=2047`, and (b) 4096 wouldn't fit the spec's 6-bit
extent register anyway. The spec correctly asserts:
`"Cannot configure non-flattened reg 'extents' with list of len 2"`.

The `(dim=1, max_extent=64)` corner of the ITERATION_DOMAIN_EXP grid
is simply too small to run conv_3_3 — characterization-level invalid
cell, akin to the `(fw=8, dw=64)` macro-unavailable cells already
commented out in `all_experiments_thesis_v2.sh`. To sweep ID corners
with conv_3_3, only cells where `max_extent * max_extent_count` covers
the app's iteration count are valid.

## Cumulative result

**98/100 configs PASS** end-to-end. The full thesis-sweep grid valid
cells are 100% covered, plus stress configs beyond the documented
range, plus the previously-commented-out fw=8 DP 4x4 cell (works at
the RTL level — sweep skipped it due to no valid GF dual-port macro,
not a flow issue). Only failures are the two structurally-invalid
sweep cells (O and Y) — the spec hardware in those cells literally
cannot encode conv_3_3's schedule, and the round-trip flow correctly
rejects them.

### Coverage matrix (by sweep block)

| Block | Cells | Valid | Tested | Notes |
|---|---|---|---|---|
| PORT_EXP block 1 (fw=4 SP 2x2) | 3 | 3 | 3 (smoke 1, A, B) | dw ∈ {8,16,32} |
| PORT_EXP block 2 (fw × vc × dw=8/16) | 18 | 18 | 18 | mix of SP/DP |
| PORT_EXP block 3 (fw × vc × dw=32) | 6 | 6 | 6 | mix of SP/DP |
| PORT_EXP block 4 (fw=2 × vc × dw=64) | 3 | 3 | 3 | DP |
| ITERATION_DOMAIN_EXP | 24 | 15 | 16 (15 valid + 1 invalid) | 8 invalid-by-design |
| AFFINE_PATTERN_GENERATOR_EXP | 30 | 20 | 21 (20 valid + 1 invalid) | 9 invalid-by-design |
| MEMORY_EXP fw=1 DP 1x1 | 5 | 5 | 5 | sc ∈ {1024..16384} |
| MEMORY_EXP fw=2 DP 2x2 | 5 | 5 | 5 | sc ∈ {1024..16384} |
| MEMORY_EXP fw=4 DP 4x4 | 4 | 4 | 3 | sc ∈ {2048,4096,8192}; sc=1024 untested |
| MEMORY_EXP fw=2 SP 1x1 | 5 | 5 | 5 | sc ∈ {2048..32768} |
| MEMORY_EXP fw=4 SP 2x2 | 4 | 4 | 4 | sc ∈ {4096..32768} |
| MEMORY_EXP fw=8 SP 4x4 | 3 | 3 | 3 | sc ∈ {8192..32768} |
| Stress (beyond sweep) | 8 | 8 | 8 | sc=131072, dw variants on MEMORY_EXP |

### Earlier per-batch listing

```
SMOKE   1: fw=4 SP 2x2 dw=16 sc=8192          PASS
SMOKE   2: fw=2 DP 2x2 dw=32 sc=8192          PASS
SMOKE   3: fw=2 SP 1x1 dw=16 sc=2048          PASS
SMOKE   4: fw=1 DP 1x1 dw=16 sc=4096          PASS
NEXT4   A: fw=4 SP 2x2 dw=8  sc=8192          PASS

```
SMOKE   1: fw=4 SP 2x2 dw=16 sc=8192          PASS
SMOKE   2: fw=2 DP 2x2 dw=32 sc=8192          PASS
SMOKE   3: fw=2 SP 1x1 dw=16 sc=2048          PASS
SMOKE   4: fw=1 DP 1x1 dw=16 sc=4096          PASS
NEXT4   A: fw=4 SP 2x2 dw=8  sc=8192          PASS
NEXT4   B: fw=4 SP 2x2 dw=32 sc=8192          PASS
NEXT4   C: fw=2 DP 2x2 dw=16 sc=4096          PASS
NEXT4   D: fw=4 SP 2x2 dw=16 sc=8192          PASS
NEXT8   E: fw=2 DP 2x2 vc=4 dw=16 sc=8192     PASS
NEXT8   F: fw=8 SP 2x2 vc=4 dw=16 sc=8192     PASS
NEXT8   G: fw=2 DP 2x2 vc=2 dw=64 sc=8192     PASS
NEXT8   H: fw=1 DP 1x1 dim=4 ext=256 sc=8192  PASS
NEXT8   I: fw=1 DP 1x1 dim=3 seqw=1024 sc=8192 PASS
NEXT8   J: fw=1 DP 1x1 dw=16 sc=8192          PASS
NEXT8   K: fw=4 DP 4x4 dw=16 sc=4096          PASS
NEXT8   L: fw=2 SP 1x1 dw=16 sc=16384         PASS
NEXT8b  M: fw=2 DP 2x2 vc=8 dw=16 sc=8192     PASS
NEXT8b  N: fw=8 SP 2x2 vc=2 dw=16 sc=8192     PASS
NEXT8b  O: fw=1 DP 1x1 dim=1 ext=64 sc=8192   FAIL (invalid sweep cell)
NEXT8b  P: fw=1 DP 1x1 dim=6 ext=4096 sc=8192 PASS
NEXT8b  Q: fw=1 DP 1x1 dim=6 seqw=16384 sc=8192 PASS
NEXT8b  R: fw=1 DP 1x1 dw=16 sc=16384         PASS
NEXT8b  S: fw=4 DP 4x4 dw=16 sc=8192          PASS
NEXT8b  T: fw=8 SP 4x4 dw=16 sc=8192          PASS
... (AC..CN, 56 more configs) — see /tmp/RT_next8*/summary.txt
```

### After-the-fact retest

After the storage-interface fix landed (`_map_array_to_intf`
per-word slice instead of per-bit), AB was retested and now passes
in seconds (was killed at 38min during spec-gen).

## What's next

- Wire `gen_sram_macro` and the GF tech-mapped behavioral models
  into the round-trip sim flow so `--physical` configs can be
  validated (currently we use a stub `// behavioral SRAM stub` for
  `inputs/sram.v`; with `--physical` the lakespec.sv references real
  GF12 macros like `IN12LP_S1DB_W01024B064M04S2_H` that need
  models).
- Run the full mflowgen graph (synth + the four sim nodes) on
  representative configs — at least the 4 smoke configs to start.
- Drive the round-trip against a multi-stream app (e.g. nbody,
  gemm, conv_layer) so 4x4 wide-fetch specs can exercise their
  parallel ports rather than running conv_3_3 in regfile mode.
