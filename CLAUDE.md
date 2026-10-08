# CLAUDE.md — Lake repo index

Working-directory index for Claude Code. **Start with §1** if the task
touches thesis figures/tables; otherwise the pointers in §3 will send you
to the right doc.

---

## 0. Maintaining this file (read first)

CLAUDE.md is the living operating manual for this repo. Every time the
user teaches Claude how to do something — a new command, a workflow, a
convention, a "next time do X instead of Y" — it belongs here, not just
in the current conversation.

**Rules for edits to this file:**

1. **Capture instructions here proactively.** When the user says
   "here's how you do X in this repo" or "the way to run Y is Z" or
   "always do A before B," add it to the appropriate section (or make
   a new section) *in the same turn*. Don't wait for the user to ask.
2. **Reconcile on conflict.** If the new info contradicts something
   already in this file, don't just append — find and update the old
   section so the file stays internally consistent. If two commands
   now do the same thing, delete the stale one. If a path or convention
   changed, edit every mention, not just the first. When in doubt, note
   the change in a short "as of <date>" clause on the updated line.
3. **Prefer editing over accreting.** Sections should shrink and get
   sharper over time as the user's guidance stabilizes. If a paragraph
   is speculative ("we might do X") and the user has now committed
   ("we do X"), rewrite it as fact and drop the hedges.
4. **Cross-link, don't duplicate.** If detailed docs live under
   `THESIS/PIPELINE.md`, `THESIS/REPLICATION.md`, `pd/thesis/README.md`,
   etc., point there from CLAUDE.md with a single line — don't inline a
   copy that will drift.
5. **Preserve pointers.** §3 must always list every non-trivial
   markdown doc in the repo. When a new one is added, register it here.

Corollary: if you notice CLAUDE.md is internally inconsistent while
reading it for another task, fix it before continuing — a stale index is
worse than none.

---

## 1. Thesis figure/table generation

Every figure and table referenced by `main_thesis.tex` is produced by a
registry-driven pipeline. Missing data emits a red-BOGUS-watermarked
placeholder and logs the miss — the tex build never breaks.

- **Source of truth for what needs to be generated:** [`THESIS/pipeline/registry.py`](THESIS/pipeline/registry.py)
- **How the pipeline works, how to add a new figure, current TODOs:** [`THESIS/PIPELINE.md`](THESIS/PIPELINE.md)

If the request is "generate figure X for the thesis," first check the
registry for its entry — the `status`, `source`, and `notes` fields tell
you whether it's real / needs data / is a hand-drawn diagram.

### 1.1 Common commands

Run from repo root. Requires `python3` with `matplotlib` + `pandas`
installed (`pip3 install matplotlib pandas` once).

```bash
# Regenerate every figure + table (real generators + BOGUS fallbacks).
# Reads THESIS_BUILDS from the default (/Users/maxwellstrange/THESIS_BUILDS)
# or pass --top <path>.
python3 THESIS/generate_thesis_artifacts.py

# List every registry entry with status + kind + output path.
python3 THESIS/generate_thesis_artifacts.py --list

# Regenerate one artifact by ID (fast iteration when tweaking a generator).
python3 THESIS/generate_thesis_artifacts.py --only port_area_vs_data_width

# Only rebuild the real (data-backed) figures.
python3 THESIS/generate_thesis_artifacts.py --status real

# Force a re-walk of THESIS_BUILDS (skip the cached CSV under THESIS/data/_cache/).
python3 THESIS/generate_thesis_artifacts.py --refresh-cache

# Use a different builds root.
python3 THESIS/generate_thesis_artifacts.py --top /sim/mstrange/THESIS_BUILDS
```

Outputs land in `THESIS/output/figures/…` and `THESIS/output/tables/…`
at paths that match the LaTeX `\includegraphics{figures/…}` verbatim.
BOGUS emissions append to `THESIS/output/logs/missing.log`; manual
(hand-drawn) entries append to `manual.log`.

### 1.2 Hooking the outputs into the LaTeX build

Pick one — the pipeline doesn't modify `main_thesis.tex` for you.

- **Preamble edit (recommended):** add
  `\graphicspath{{THESIS/output/}{./}}` to `main_thesis.tex` so
  `\includegraphics{figures/foo.pdf}` resolves under `THESIS/output/`.
- **Symlink:** `ln -s THESIS/output/figures figures` at repo root.

### 1.3 Adding a new figure

1. Write a generator in `THESIS/pipeline/generators.py`. Signature:
   `def my_fig(ctx: GenContext, outpath: Path) -> None`. Raise
   `MissingDataError(...)` when the source data isn't available.
2. Append an `Entry(...)` to the right list in
   `THESIS/pipeline/registry.py` — set `output` to the exact
   `\includegraphics` path the tex expects.
3. Regenerate just that artifact:
   `python3 THESIS/generate_thesis_artifacts.py --only <id>`.

Full walkthrough (statuses, TODO conventions, per-generator patterns) is
in [`THESIS/PIPELINE.md`](THESIS/PIPELINE.md).

### 1.4 Reading the logs

```bash
# What's still BOGUS and why (ID + reason per line).
sort -u THESIS/output/logs/missing.log

# What still needs hand-drawn art / curated tables.
sort -u THESIS/output/logs/manual.log

# Any generator crashes (should stay empty).
cat THESIS/output/logs/errors.log 2>/dev/null
```

### 1.5 Adding a new sweep experiment

If a new `THESIS_BUILDS/<EXP>/` directory shows up:

1. Run the extractor once to confirm columns come through:
   `python3 ASPLOS_EXP/extract_power_area.py /path/to/THESIS_BUILDS -o /tmp/x.csv`
2. Delete the cache: `rm -rf THESIS/data/_cache/`
3. Add generators + registry entries for whatever figures the new
   experiment feeds.

### 1.6 Memtile PPA regression (Kahng-hybrid)

`THESIS/pipeline/regression.py` fits area/power/delay for the two
memtile-model tables using the Kahng-hybrid recipe (Kahng et al., DATE
2015 / SLIP 2013). Key design decisions live in the module docstring;
the short version:

- **SRAM split:** total area minus `synth_storage_area_um2` (added to
  the extractor for this purpose). Lasso fits only the surrounding
  logic — SRAM stays a piecewise lookup.
- **Features:** `dim, fw, vc, inp, outp, me, log2(msw),
  log2(storage_cap), log2(data_width)`. Powers-of-two are log-encoded
  so coefficients read as "cost per doubling."
- **CV:** leave-one-experiment-out. Random k-fold silently leaks
  because features co-vary within each per-component sweep.
- **Delay:** censored — only unsaturated points (crit_delay < 99% of
  clock target). PORT_EXP drops out entirely (all pegged at 700 MHz),
  which is the correct signal not a bug.
- **Power:** same fit, but raises `MissingDataError` until `ptpx-synth`
  lands and drops in automatically when it does.

Regenerate the tables in isolation:
```bash
python3 THESIS/generate_thesis_artifacts.py --only memtile_model_coeff memtile_model_verif
```

### 1.7a Auto-generated LaTeX tables

`THESIS/pipeline/tables.py` renders five thesis tables as
`\begin{tabular}` snippets under `THESIS/output/tables/`:

| Table | Status | Source |
| --- | --- | --- |
| `tab:ul_ppa_summary` | data-driven (power col blank until sweep) | extractor DataFrame × `DesignPoint` list |
| `tab:ul_design_points` | data-driven | `THESIS/apps/design_points.DESIGNS_SINGLE_LEVEL` |
| `tab:exploration_applications` | data-driven | `THESIS/apps/registry.APPS` |
| `tab:lake_interfaces` | skeleton (prose TODO) | `lake/spec/*.py` `__init__` signatures |
| `tab:compiler_info` | skeleton (prose TODO) | Component list + `% TODO` per column |

Each `\input{}`-able from `main_thesis.tex`. Skeletons include a
`% skeleton — …` comment header so it's obvious in-file that prose
still needs authoring.

### 1.7 App-mapping harness (Ch. 5)

The exploration figures (`single_level_*`, `two_level_*`,
`tab:ul_perf`, `tab:ul_ppa_summary`) are fed by `THESIS/apps/` — see
[`THESIS/apps/README.md`](THESIS/apps/README.md) for the milestone plan.
As of 2026-09-28, Milestone 1 is wired end-to-end but has no data yet:

- `THESIS/apps/registry.py` — 6 `AppSpec` entries; the `??` markers
  (Halide app dirs) still need verifying against the AHA
  Halide-to-Hardware checkout on the cluster.
- `THESIS/apps/design_points.py` — `DESIGNS_SINGLE_LEVEL` holds the 8
  round-trip-validated configs from `THESIS_ROUNDTRIP_PROGRESS.md`.
- `THESIS/apps/run_matrix.py` — runs one (design, app) cell per call via
  `ASPLOS_EXP/run_roundtrip_sweep.sh --app-dir/--testname`, parses
  per-tile `outputs/util.txt` (written by the `tb.sv` util counters),
  looks up PPA, and writes `THESIS/data/apps/**/results.json`:
  `python3 -m THESIS.apps.run_matrix --apps ... --designs ... [--dry-run]`.
- `THESIS/pipeline/apps_query.py` loads those results for the 5
  `single_level_*` generators (registry status `real`; BOGUS until
  results exist). `*_LI` variants stay bogus (Milestone 3).
- `THESIS/apps/compose.py` — CGRA-level PPA composer with placeholder
  PE-tile numbers.
- Tests: `python3 -m pytest --confcutdir=tests/thesis tests/thesis/`.

### 1.8 Memory-tile (Tile_MemCore) sweep figures

The garnet lake-spec sweep (`garnet/mflowgen/sweep_specs.py --spec-set
thesis --zip`) is a second data source, separate from THESIS_BUILDS: whole
CGRA memory tiles (lakespec + MemCore wrapper + SB/CB) for every thesis spec,
static + RV, Genus synth (+ Innovus for the PnR anchors). As of 2026-10-07 the
figures come from `tile_memcore_thesis_r8cad-gf12_20261006-221414.zip`: all
196 thesis configs synthesized (lakespec flattened, 1.333 ns) + the 12 `full12`
PnR anchors through signoff.

```bash
# Ingest a sweep zip (or extracted dir) -> THESIS/data/tile_sweep/{<sweep>,latest}.csv
python3 -m THESIS.pipeline.tile_sweep ../tile_memcore_thesis_<host>_<ts>.zip
# Regenerate every tile-sweep figure (MEMTILE_CHARACTERIZATION + Ch. 4 component area)
python3 THESIS/generate_thesis_artifacts.py --only memtile_capacity_area memtile_bandwidth_area \
    memtile_interconnect_area memtile_port_buffer_area memtile_control_area \
    memtile_rv_overhead_area memtile_synth_vs_pnr_area \
    port_area_vs_data_width port_area_vs_vc iter_dom_area_vs_dim iter_dom_area_vs_max_extent \
    affine_area_vs_dim affine_area_vs_max_value memory_port_area_vs_interface_width \
    storage_area_vs_capacity
```

It also feeds the Ch. 4 component characterization **area** figures
(`port_area_vs_vc`, `iter_dom_*`, `affine_area_vs_{dim,max_value}`,
`memory_port_area_vs_interface_width`, `storage_area_vs_capacity`), which
replaced the standalone THESIS_BUILDS versions on 2026-10-06 at the same
output paths. Every tile figure keeps only `data_width == 16` configs
(`tile_figures.DATA_WIDTH`): other widths have broken RTL, so
`port_area_vs_data_width` is BOGUS until they're fixed. Tile synthesis
flattens lakespec, so these plot MemCore logic, not isolated components.

Loader/parsers: `THESIS/pipeline/tile_sweep.py` (maps each config to its
experiment by spec, picks up configs still building when the zip was cut,
finds mflowgen step dirs by name since step numbers shift between graphs).
Plots: `THESIS/pipeline/tile_figures.py` → `THESIS/output/figures/memtile/`.
Ready-to-paste figure environments with captions and suggested placement:
`THESIS/output/snippets/memtile_figures.tex` (under the untracked
`THESIS/output/`, so local to this checkout).

Every ingest also overwrites `THESIS/data/tile_sweep/latest.csv`, which is what
the figures read, so the last zip ingested is the figure source. Next dataset
(planned 2026-10-07): garnet's dw16 sweep with `--flatten-effort 0` (garnet
`mflowgen/CLAUDE.md` "dw16 synth sweep + held-out PnR validation") keeps the
lakespec hierarchy. `tile_sweep._hier` only splits CB / SB / MemCore so far;
per-component rows (port SG/AG/ID, storage, memory port, config) still need a
parser before the component figures can show isolated components. Don't mix
flattened and hierarchical rows in one figure: area shifts between the two.

Known quirks of the current dataset (details: garnet `mflowgen/CLAUDE.md`
"Thesis-set synth sweep" result):
- Its tile RTL was built at two lake commits: `82a78497` (PORT, ITERATION_DOMAIN,
  PnR anchors, most AFFINE) and `ebd0948e` (MEMORY_EXP, 12 AFFINE), plus one RV
  re-run at `577c6beb` (`..._dim1_msw256_rv`, +4.5% std-cell area). AFFINE shows
  no step between the first two.
- RV ignores `max_sequence_width`: `build_spec_rv` never passes `stride_width`
  to `ReadyValidScheduleGenerator`, so the 30 RV `_msw*` configs are 6 designs.
  The static msw sweep sizes the ScheduleGenerator's strides, not the
  AddressGenerator's.

---

## 2. Running the physical-design flow

- **How to build one synth config end-to-end (mflowgen, Genus, Innovus,
  PT):** [`THESIS/REPLICATION.md`](THESIS/REPLICATION.md) — verified
  through post-PnR power on 2026-04-26, includes step numbers,
  runtimes, and gotchas.
- **mflowgen design definition:** [`pd/thesis/README.md`](pd/thesis/README.md)
- **Generic (no-ADK) synthesis → power path:**
  [`pd/thesis/generic-synth-power/GENERIC_SYNTH_POWER.md`](pd/thesis/generic-synth-power/GENERIC_SYNTH_POWER.md)
  — self-contained Genus/DC + generic `.lib` + PrimeTime idle/active power,
  no gf12 ADK / macros / PnR. Fast portable "synth results → power" check;
  45nm behavioural, relative numbers only. Driver:
  `pd/thesis/generic-synth-power/generic_synth_power.sh`.
- **Idle/active power tests** (`pd/thesis/power-test-gen` →
  `synopsys-vcs-sim-power[-gl]` → vcd2saif → ptpx): programs and the random
  input stream come from
  `lake/utils/power_test_programs.py`, shared with garnet's Tile_MemCore
  `--memtile-power` (same spec + seed → same programs and input words). Idle =
  empty program, active = max sustainable traffic; both get the SAME random
  stream (`input_data.hex`, a fresh word per input port per cycle). The graph
  kwargs must carry `vec_capacity`, `max_extent`, `max_sequence_width` (not
  only `python_command`), or power-test-gen programs a different spec than
  the RTL. Details: `garnet/mflowgen/CLAUDE.md` "MemTile idle/active power".
  **Synth level simulates the Genus netlist** (2026-10-07):
  `synopsys-vcs-sim-{idle,active}-power` take synth's `design.v` plus the adk
  stdcell models (minus `*pwr*`) and run zero-delay (`+nospecify
  +notimingcheck`). Before that they simulated the RTL, and ptpx-synth matched
  that RTL activity to only ~2–4% of the netlist's nets (no name map). The rest
  got default toggle rates, so idle ≈ active. Checked locally (freepdk45 DC
  netlist, fw2 SP 1×1 2 KB, the step's own Makefile): 100% annotated, and idle
  9.39 / active 17.8 mW, the same as a hand-run gate-level sim. The ptpx
  default `strip_path tb/dut` matches fine (as does `tb/dut_gen.dut`). Not run
  on gf12: the adk `*.v` set and the Genus netlist in VCS. A workspace whose
  power sims ran on the RTL needs `make clean-<sim step>` to redo them.
- **App-driven standalone power** (2026-10-07, graph kwarg `app_bundle`;
  garnet `sweep_specs.py --standalone-synth --app-bundle-dir` passes it for
  static configs): `pd/thesis/app-power-gen/gen_app_stimulus.py` replays one
  MEM tile of a real app into this lakespec. The bundle (garnet
  `mflowgen/common/application/gen_app_bundle.py`) records each MEM tile's
  `lakespec_flat` ports (`core.vcd`): the tile's parallel `config_memory` IS the
  standalone serial bitstream (same spec → identical `gen_bitstream`, checked),
  and the per-cycle port inputs become `input_data.app.hex`. It adds
  `synopsys-{vcs-sim,vcd2saif-convert,ptpx-synth}-app-power` (+ `-gl`), wired
  like idle/active. tb.sv quirk: it reads its stream index (a nonblocking
  counter) in the step it increments it, so word k reaches the design one edge
  late → `--input-shift 1` (default). With it, the standalone outputs equal
  the tile's lakespec outputs in value AND cycle (conv_3_3 line buffer, 2×4096
  taps; RTL and freepdk45 gate level); shift 0 gets 3471/4096 wrong. The step
  fails if the bundle's spec ≠ this spec (build_spec defaults filled) or the
  bundle is RV (no standalone RV). `MAX_DATA_SIZE` is now a tb define
  (comp_args.app.txt sets it to the largest output count). Local result
  (freepdk45 DC netlist, 100 MHz, fw4 SP 2×2 4 KB): idle 18.9 / conv_3_3 app
  30.5 / active 33.4 mW, 100% annotated.
- **Sweep + extraction scripts** (full index in
  [`ASPLOS_EXP/README.md`](ASPLOS_EXP/README.md)). Consumed by the thesis pipeline:
  - `ASPLOS_EXP/extract_power_area.py` — walks THESIS_BUILDS → CSV of
    area/power/timing per build (also parses critical-path endpoints).
  - `ASPLOS_EXP/plot_power_area.py` — standalone plotter (thesis
    pipeline reuses styling but not code).
  - `ASPLOS_EXP/scaling_stats.py` — per-experiment linear fits +
    derived ratios.
  - `ASPLOS_EXP/collect_power_area.sh` — one-shot markdown-table
    summariser used before the Python scripts existed.

---

## 3. Other in-repo docs

- [`README.md`](README.md) — Lake project overview (install, run a test,
  wiki link).
- [`THESIS/PIPELINE.md`](THESIS/PIPELINE.md) — thesis artifact
  pipeline (indexed above in §1).
- [`THESIS/apps/README.md`](THESIS/apps/README.md) — app-mapping
  harness scope + milestone plan (Ch. 5 exploration figures).
- [`THESIS/REPLICATION.md`](THESIS/REPLICATION.md) — mflowgen synth
  flow (indexed above in §2).
- [`THESIS_ROUNDTRIP_PROGRESS.md`](THESIS_ROUNDTRIP_PROGRESS.md) —
  status of the Lake→Clockwork→Lake spec round-trip smoke tests as of
  2026-05-08 (which sweep configs have validated end-to-end).
- [`pd/thesis/README.md`](pd/thesis/README.md) — mflowgen design
  definition.
- [`pd/thesis/generic-synth-power/GENERIC_SYNTH_POWER.md`](pd/thesis/generic-synth-power/GENERIC_SYNTH_POWER.md)
  — generic no-ADK synthesis→power path (indexed above in §2).
- [`ASPLOS_EXP/README.md`](ASPLOS_EXP/README.md) — sweep drivers,
  power/round-trip scripts, extractor; which scripts are current vs legacy.
- [`tests/thesis/README.md`](tests/thesis/README.md) — thesis pipeline
  test suite (run command, per-file coverage).
- [`configure/README.md`](configure/README.md) — configuration-finder
  framework (placeholder, "under construction").

If you write a new markdown doc anywhere under this repo that's
relevant to future tasks, add a one-line pointer here.

---

## 4. Directory quick reference

| Path | Purpose |
| --- | --- |
| `lake/` | Core Lake library (streaming-memory generator). |
| `tests/` | pytest suite + `test_spec/thesis_sweep.py` (per-config RTL gen). |
| `ASPLOS_EXP/` | Sweep drivers + data extraction + ad-hoc plotting. |
| `pd/thesis/` | mflowgen design definition (steps, TCL, SDC). |
| `THESIS/` | Thesis-artifact pipeline + REPLICATION doc. |
| `THESIS_BUILDS/` (outside repo) | Where mflowgen builds land, one dir per sweep config. |
| `main_thesis.tex` | The thesis itself (untracked). |

---

## 5. Spec MemCore RTL generation (the garnet lake-spec sweep)

Garnet's per-spec MemCore sweep drives `garnet.py --lake-spec-config` (see
`garnet/mflowgen/sweep_specs.py` and `garnet/mflowgen/CLAUDE.md`). RTL comes
out of `CoreCombiner` (`lake/top/core_combiner.py`) → `MemoryTileBuilder`
(`lake/top/memtile_builder.py`) → `MemoryInterface`
(`lake/top/memory_interface.py`), NOT the standalone `build_spec*` helpers in
`lake/spec/spec_memory_controller.py`. It runs locally on /aha (no
ADK/Cadence needed); the memory note
`reference_local_memcore_rtl_validation` has the exact command.

### 5.1 SRAM tech-map / physical-macro selection

`CoreCombiner._select_gf_tech_map()` picks the GF SRAM macro by choosing how
many **column-macros** build one `mem_width` word (`mem_width =
data_width * fetch_width`). Each column is `mem_width/cols` bits wide;
`PhysicalMemoryStub` (`memory_interface.py`) lays `cols` of them side by
side, **broadcasting** the (word) address to every column and **splitting**
the data across them. Rules:

- **Prefer `cols=2`, fall back to `cols=1` then up.** cols=2 keeps each
  macro narrow/short for the fixed-height MemCore tile (PnR abutment) and
  leaves already-valid geometries byte-identical to prior RTL. cols=1 (one
  full-width macro) is used only when no 2-column macro exists — e.g. a
  narrow fw=2 word (`mem_width=32, depth=512`) whose 16b half-columns
  undershoot the library floor (16b macros need `depth>=1024`) but which
  fits as one 32b macro. The library table + range/granularity check live in
  `lake/top/tech_maps.py` (`GF_Tech_Map`, `get_gf_macro_options`).
- **Dual-port** (spec `dual_port=True` → CoreCombiner `rw_same_cycle`) builds
  a `[RW, R]` port list and MUST map onto the SDPB 1rw1r macro (2 port maps).
  `_select_gf_tech_map` passes `dual_port=rw_same_cycle` so `GF_Tech_Map`
  returns the SDPB (not single-port S1xB) map; otherwise the R port indexes a
  missing `port_maps[1]` → IndexError.

### 5.2 Dual-port bugs fixed (lake THESIS `8bc6995f`, `897f96f1`)

Dual-port spec MemCores were never exercised through this flow; three
single-port assumptions crashed `garnet.py` RTL gen for `fw2_*_dp*`:

1. `core_combiner` built the tech map without `dual_port` → single-port map,
   1 port → `port_maps[1]` IndexError. (Now `_select_gf_tech_map`.)
2. `memory_interface.py` `PhysicalMemoryPort.create_port_interface` READ
   branch only knew `data_out`/`read_addr`; the SDPB read map names them
   `read_data`/`addr` → KeyError. Added the same fallbacks the READWRITE
   branch had.
3. `PhysicalMemoryStub` READ branch **concatenated** per-column child
   `read_addr`s into the parent (num_wide× too wide → width mismatch).
   Address is a shared word address → **broadcast** it like the READWRITE
   branch. Latent until a READ port with `num_wide>1` (dual-port is first).

### 5.3 Gotcha: flaky coreir/kratos SIGSEGV

`garnet.py` intermittently core-dumps (`rc=139`, "dumped core") in the C++
backend, well past tech-map selection ("Printing mode map..."). It is
non-deterministic — the same geometry passes on retry. The `aha` driver
wraps garnet.py in `retry()` (`aha/util/garnet.py`); any bare local sweep
must retry too or a transient crash reads as a config failure.

---

## 7. Lake-spec ponds (static `build_pond` + `build_pond_rv`) (2026-09-30)

The PE-tile pond is a lake spec in both modes (user direction: the legacy
static pond — garnet `PondCore` → `LakeTop`/`StrgUBThin` — was a separate block
only for historical reasons).

- **`build_pond`** (`lake/spec/spec_memory_controller.py`, static sibling of
  `build_pond_rv`): fw=1 register file, `in_ports` STATIC IN ports sharing ONE W
  memory port, one delay-0 R memory port per OUT port → `[W, R, R]`, which
  CoreCombiner's `new_pond` interface hosts unchanged (its clear port stays
  unused; the tile flush clears the pond). Args: `storage_capacity` (bytes),
  `data_width`, `dims`, `in_ports/out_ports`, `max_extent`,
  `max_sequence_width`, `remote_storage`, `config_passthru`, `comply_17`.
  Clockwork's static keys map directly (`in2regfile_N` → port N,
  `regfile2out_N` → port n_in+N) via `_convert_clockwork_to_port_config`.
- `build_pond_rv` now passes `data_width` to its Ports (`int_data_width`) and
  memory ports (were hardcoded 16 → any other width asserted / mismatched).
  RTL at 16 bits unchanged.
- **Equivalence vs the legacy pond (co-sim, xrun)**: the tile's delay-0 READ
  memory-interface port is an ungated `data_array[read_addr]`, so each spec
  pond output shows `mem[AG addr]` every cycle — the legacy contract clockwork's
  static schedules rely on (e.g. accumulation: the co-located PE consumes the
  pond at WRITE time, not at the scheduled read). Closed-loop co-sim (PE model
  `in0 = out0 + r(t)`, identical flush/stall stimulus) on 8 real clockwork
  static pond configs (resnet_pond acc + weights, matmul, resnet_block 2-D,
  avgpool RMW, pond_accum, depthwise const, legacy unit test) + 4 stalled
  variants: 12/12 match (data at every scheduled read, identical valid
  schedule). Known, unobservable differences: (1) legacy outputs mirror ONE
  shared read address (single dual-config sequencer: `_0`/`_1` are sequential
  phases), spec outputs have independent addresses — differs only on cycles an
  output is not read; (2) legacy can fire a scheduled event in a flush cycle
  (valid only; same data), the spec's SG step is gated by flush; (3) no 1-bit
  `valid_out` data outputs. The spec runs interleaved `_0`/`_1` configs the
  legacy pond cannot. Harness (session scratchpad, not in repo):
  `pond/cosim/{gen.py,run.sh,compare.py}`.
- garnet hosting/knob: `garnet/mflowgen/CLAUDE.md` "PE-tile pond knobs".
- **Compiler collateral (round trip like the MEM spec)**: `build_cgra_pond(params, rv)`
  (`spec_memory_controller.py`) is the single pond factory for garnet's
  `make_pond()` AND the collateral; `resolve_cgra_pond_params` validates the
  pond JSON. The static CGRA pond defaults to 16-bit iteration ranges
  (`max_extent` 2**16, like the legacy LakeTop pond) → its collateral equals
  clockwork's built-in regfile preset (capacity 32, counter_ub 65535).
  `extract_compiler_information(level="regfile")` keys the pond `regfile`
  (`in2regfile_N`), `max_chaining` 1, `interconnect_*` = concurrent W/R
  memory ports (clockwork scheduler resource count), plus `sched_counter_ub`
  and `data_width`; `level="mem"` output is byte-identical to before.
  CLI: `python -m lake.utils.pond_collateral [--pond-spec p.json] [--rv] -o c.json`;
  aha: `aha map --pond-collateral c.json` → `LAKE_COLLATERAL_JSON_REGFILE`.
  Clockwork side (applied 2026-09-30) + verification: `/aha/clockwork/CLAUDE.md` "Pond (regfile level)".
- **RV pond filter (2026-09-30)**: `build_pond_rv(filter=)` gives the data input
  lake's filter hardware; the CGRA RV pond (`build_cgra_pond(rv=True)`) has it by
  default (pond-spec key `filter`, user: ponds mostly serve weights or
  accumulations directly, and broadcast-banked weight ponds need it). Before, a
  configured `filter` was silently dropped (no filter HW) → each resnet_pond RV
  weight bank kept the first 27 broadcast items; now 4/4 banks PASS (pond
  round trip). A port without filter HW still drops a filter config silently.
- **Full-CGRA run on a spec pond (2026-09-30)**: RV `matmul_rvtry3` (n=16,
  accumulation pond) on a 16x16 CGRA — MEM `build_spec_rv(8192,16,4,2,2)` +
  pond spec `{"storage_capacity": 128, "dims": 6}` (`garnet.py
  --lake-pond-spec-config`, filter on) compiled with that pond's collateral
  (`aha map --pond-collateral`): Integer bit-accurate PASS (hooks
  rv_drop_pond_init_pe + rv_compact_output_sched, as for the default pond).
  Driver (session scratchpad): `pond/e2e/run_e2e.sh` (private aha tree via
  `aha --dir`, private garnet with `GARNET_HOME` pinned to it).
