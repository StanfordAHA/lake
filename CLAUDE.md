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

### 1.6 Memory Tile model tables (`tab:memtile_model_*`)

`THESIS/pipeline/regression.py` (tile-sweep part, 2026-10-24) fits a Memory Tile's
non-SRAM area and idle/active power to its spec parameters, one LassoCV per runtime
mode, on `THESIS/data/tile_sweep/latest.csv`, and checks it on held-out configs.
Design notes: `THESIS/PIPELINE.md` §7. The tables are pasted inline into main.tex
(Overleaf), so regenerate and re-paste after a new sweep is ingested. They currently
come from the stock-fabric dw16 sweep; refit them from the thesis-fabric
design-space sweep once lake ingests it.

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

The exploration figures (`single_level_*`, `single_level_*_LI`, `two_level_*`,
`tab:ul_perf`, `tab:ul_ppa_summary`) are fed by `THESIS/apps/`.

**Source since 2026-10-25: garnet's whole-CGRA app power.** Bundles are recorded on
/aha (`gen_app_bundle.py`, thesis fabrics), `cgra_app_power.py` runs on the build
machine, and lake ingests its zip:

```bash
python3 -m THESIS.apps.garnet_configs            # design point <-> garnet config names
python3 -m THESIS.apps.garnet_configs --commands  # /aha bundle script + build-machine command
python3 -m THESIS.apps.cgra_power_ingest <cgra_app_power zip> --kernel-stats ks.json [--dry-run]
```

The full command sequence, field meanings and caveats (power window vs kernel, SRAM
area) are in [`THESIS/apps/CGRA_APP_POWER.md`](THESIS/apps/CGRA_APP_POWER.md), along with
what garnet still has to add (kernel cycles in the manifest, a kernel-only power window,
SRAM busy fraction for utilization). Results land in
`THESIS/data/apps/<design>__<mode>__<fabric>[__pond-<id>]/<app>/results.json`.
`single_level_*` plot static thesis-fabric rows, `_LI` the RV ones
(`generators.APP_FABRIC`, `apps_query.select`). The bundle matrix uses the 8
`DESIGNS_SINGLE_LEVEL` points as they are, duplicate and dual-port points included (user,
2026-10-25). No data yet: the existing thesis-fabric bundles are all on the design point
`fw4_dw16_sc4096`, which isn't one of the 8, so the whole matrix has to be recorded.
Apps (`registry.APPS`, user 2026-10-25): the 4 image apps, matmul single level
(`matmul_agg`, blocked: the no-pond `matmul_rvtry3 sched=1` does not map on 16x16) and
two level (`matmul_pond_agg`, pond accumulation; `level="two"`, not in `single_level_*`),
and the `pondspec_e2e/resnet` copy that ran on both thesis fabrics. Bundles and
cgra_app_power variants are named by app id; `tab:exploration_applications` now has a
"Matrix Multiply (pond)" row.

The older single-memory-tile harness ([`THESIS/apps/README.md`](THESIS/apps/README.md),
never run; Milestone 1 wired as of 2026-09-28):

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

### 1.7b Appendix Halide code

`python3 THESIS/generate_thesis_artifacts.py --only halide_appendix` writes
`THESIS/output/listings/halide_appendix.tex`: the generator code of the six
`tab:exploration_applications` apps at the Halide-to-Hardware commit aha pins,
with only the Clockwork schedule aha's `application_parameters.json` args
select. Paste it over the listings in the Overleaf appendix
(`app:halide_code`). Details: `THESIS/PIPELINE.md` §4.

### 1.8 Memory-tile (Tile_MemCore) sweep figures

The garnet lake-spec sweep (`garnet/mflowgen/sweep_specs.py --spec-set
thesis --zip`) is a second data source, separate from THESIS_BUILDS: whole
CGRA memory tiles (lakespec + MemCore wrapper + SB/CB) for every thesis spec,
static + RV, Genus synth (+ Innovus/PT for the PnR configs), idle/active power.
As of 2026-10-09 the figures come from the **dw16 hierarchy-kept sweep**
`NEW_SWEEP_r8cad-gf12_20261009-213941_slim.zip` (garnet `mflowgen/CLAUDE.md`
"dw16 synth sweep + held-out PnR validation"): 172 configs (89 static + 83 RV),
`--flatten-effort 0`, 1.333 ns, 12 through PnR (8 fit + 4 held out), power at
synth level for all and PnR level for the 12. Its build-side handoff and the
analysis (model checks, per-block breakdown, RV overhead) sit next to the
extracted zip in the user's workspace: `HANDOFF.md`, `<sweep>/analysis/ANALYSIS.md`.
One command rebuilds the whole thesis data set (2026-10-17): garnet
`sweep_specs.py --recipe thesis`. This sweep is its `memtile` part, now at every
data width; the other parts are the switch-box A/B, PE tiles and app power. See
garnet `mflowgen/CLAUDE.md` "Thesis data recipe". Only the `memtile` zip has a lake
ingest so far.

```bash
# Ingest a sweep zip (or extracted dir) -> THESIS/data/tile_sweep/{<sweep>,latest}.csv
python3 -m THESIS.pipeline.tile_sweep ../NEW_SWEEP_<host>_<ts>_slim.zip
# Regenerate every tile-sweep figure (zsh: ${=IDS} to word-split a variable)
python3 THESIS/generate_thesis_artifacts.py --only \
    port_area_vs_data_width port_power_vs_data_width port_area_vs_vc port_power_vs_vc \
    iter_dom_area_vs_dim iter_dom_power_vs_dim iter_dom_area_vs_max_extent iter_dom_power_vs_max_extent \
    affine_area_vs_dim affine_power_vs_dim affine_area_vs_max_value affine_power_vs_max_value \
    memory_port_area_vs_interface_width memory_port_power_vs_interface_width \
    storage_area_vs_capacity storage_power_vs_capacity \
    pub_capacity_area pub_capacity_power pub_bandwidth_area pub_bandwidth_power \
    memtile_interconnect_area memtile_interconnect_power memtile_block_breakdown \
    memtile_rv_overhead_area memtile_rv_energy_breakdown memtile_synth_vs_pnr_area memtile_synth_vs_pnr_power \
    li_sched_ports_area li_sched_ports_power li_sched_loc_area li_sched_loc_power \
    memtile_capacity_area memtile_bandwidth_area memtile_port_buffer_area memtile_control_area
```

Every ingest overwrites `THESIS/data/tile_sweep/latest.csv` (what the figures
read), so the last sweep ingested is the figure source; each sweep also keeps
its own `<sweep>.csv`. The older flattened 196-config sweep
(`tile_memcore_thesis_r8cad-gf12_20261006-221414.csv`) is superseded: it used
older RTL with lakespec flattened (its RV overheads were roughly half the
current ones), so don't quote or mix it.

**Per-block columns** (hierarchy-kept sweeps only; `tile_sweep._LEVELS`): each
level's blocks sum exactly to the level — tile = `sb + cb + memcore +
tile_glue`; memcore = `cfg_regs + mc_mux + inner + mc_glue`; inner = `sram +
ctrl + stencil + rom + fifos + inner_glue`; ctrl (the lake controller,
`lakespec_inst` static / `lakespec_mem_inst` RV) = `c_port + c_id + c_sg + c_ag
+ c_rvnet + c_storage + c_rdbuf + c_mp + c_other` (`c_rvnet/c_storage/c_rdbuf`
RV only). Prefixes: `blk_` Genus cell area, `pblk_` Innovus signoff area,
`pw_{synth,pnr}_{idle,active}_` PT power (W; PT prints 3 significant digits,
so power remainders can come out slightly negative — plot named blocks only).
The sweep's own `correlation.csv` / `memtile_power.csv` columns (PnR role,
synth→PnR projections, PT WNS, power totals) are merged in by config.

What the sweep feeds (registry `tf.*`; all `data_width == 16`,
`tile_figures.DATA_WIDTH`):
- Ch. 4 component **area and power** (replaced the THESIS_BUILDS versions, same
  output paths), each from its own blocks: Port = `c_port` (4 `port_inst`),
  IterationDomain = `c_id`, affine pattern generators = `c_ag + c_sg`,
  MemoryPort = `c_mp`, Storage = `sram`. Power = PT synth-level active (solid)
  and idle (dashed). `port_{area,power}_vs_data_width` are BOGUS (dw16 only).
- PUB = `ctrl + sram`: `pub_capacity_*` (line per memory organization),
  `pub_bandwidth_*` (8 KB, SP vs DP, stacked SRAM / controller).
- Memory tile: `memtile_interconnect_{area,power}` (CB/SB/glue vs ports),
  `memtile_block_breakdown` (area/idle/active share per block, 3 fit
  geometries × static/RV), `memtile_rv_overhead_area`, `memtile_rv_energy_breakdown`
  (static vs RV per block: non-SRAM active pJ per delivered bit, idle power;
  bandwidth = the active test's designed rate, `tile_figures._bw_gbps`).
- Synth→PnR model `memtile_synth_vs_pnr_{area,power}`: the 12 PnR builds by
  `pnr_role` (8 `fit` dots, 4 `validate` stars), one linear fit per runtime mode
  (reproduces garnet `correlation_fit.csv` / `memtile_power_fit.csv`), R² in the
  legend, per-build prediction error below.
- LI ScheduleGenerator (RV) = `c_rvnet + c_sg`: `li_sched_ports_*` (dims-6 RV
  configs by port count), `li_sched_loc_*` (ITERATION_DOMAIN RV, "levels of
  comparison" read as dims; pre-`8748240d` RTL drawn hollow via the
  `rtl_lake_commit` ingest column).
- `tile_figures.SRAM_NDA = True` (the thesis says the GF12 macro numbers are
  under NDA): Storage, PUB and model values are relative to a reference config
  (`NDA_REF`, or the smallest fit tile for the model). Set False for µm²/mW.
- **Raw vs % change per x step, per figure** (2026-10-13): registry
  `Entry.view` = "raw" | "pct" | "both" for the `@_viewable` sweep figures
  (all "both" for now; the user picks per figure). Preview all three before
  choosing: `python3 THESIS/generate_thesis_artifacts.py --only <ids>
  --preview-views <dir>` (side-by-side `index.html`). Details:
  `THESIS/PIPELINE.md` "Views".
Placement + captions for main.tex (the Overleaf project "Thesis Rough Draft" is
the live copy; local `main_thesis.tex` is stale):
`THESIS/output/snippets/memtile_figures.tex` (EDIT 1–7).

Loader/parsers: `THESIS/pipeline/tile_sweep.py` (maps each config to its
experiment by spec, picks up configs still building when the zip was cut,
finds mflowgen step dirs by name since step numbers shift between graphs).
Plots: `THESIS/pipeline/tile_figures.py` → `THESIS/output/figures/` (new
figures under `figures/memtile/`; the whole `THESIS/output/` tree, snippet file
included, is untracked, so local to this checkout).

Known quirks of the dw16 dataset:
- Tile RTL was generated at 8 lake commits (gen_rtl takes `origin/THESIS` at
  RTL time; the sweep ran over 2 days). Only `8748240d` (RV comparison-network
  wrap fix, dims 2/4/8 only) changes RTL: all 9 dims-2 RV configs and 5 of 9
  dims-4 RV configs are pre-fix — +6% comparison network, +0.84% tile area on
  the measured dims-4 pair, power identical. Static and dims-3/5/6 RV are
  unaffected. Each rtl log prints the lake commit after `Mek Mek Mek`.
- RV ignores `max_sequence_width`: `build_spec_rv` never passes `stride_width`
  to `ReadyValidScheduleGenerator`, so the 30 RV `_msw*` configs are 6 designs
  (plus the dims-4 fix step above). The static msw sweep sizes the
  ScheduleGenerators' strides, not the AddressGenerators'.
- 101 configs were synthesized with 3 identical timing modes, 71 with 1 (garnet
  update mid-run); +0.18% logic area on one config built both ways.

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
  `pd/thesis/generic-synth-power/generic_synth_power.sh`. Since 2026-10-08 it
  simulates the synth netlist itself (`--power-tests` = power-test-gen outputs,
  `--cells` = the lib's Verilog models) with the power tb, so PT annotates
  ~100% of nets. The older `--idle/--active` RTL-sim activity mode still runs,
  with a warning: only a few % of nets get annotated, and idle comes out ≈ active.
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
- **ptpx steps read the SDC through `read_sdc_compat.tcl`** (2026-10-08):
  PrimeTime's `read_sdc` stops at the first non-SDC line and silently drops
  the rest. Genus writes `current_design lakespec` at line 13 of its SDC, so
  on the build machine every `synopsys-ptpx-synth*` step (idle/active/app/
  handcrafted/roundtrip) read 12 lines and ran with NO clock (PWR-171 "No
  clock specified"; garnet sweep smoke run 2026-10-07). The local freepdk45
  checks above used DC SDCs and didn't hit it. Innovus's PT SDC (ptpx-gl)
  adds `append_to_collection` (folded the way garnet's Tile_MemCore
  `custom-signoff/outputs/fix-pt-sdc.tcl` does). The read copy is
  `<sdc>.compat.sdc` in the step dir; ptpx-synth/-gl now fail on "Errors
  reading SDC" in `logs/pt.log`. Twin `read_sdc_compat.tcl` in both node
  dirs (keep identical). Checked with PT Q-2019.12-SP2 `read_sdc` (the build
  machine's): plain → stops at `current_design`, no clocks; compat → clock +
  multicycle applied. Existing workspaces: redo the ptpx steps (garnet
  `sweep_specs.py --standalone-only --fresh` rebuilds a standalone in ~16 min).
  Since 5ed60bd1 it also drops the synthesis-only directives PT 2019 rejects
  (set_dont_use, set_dont_touch, set_dont_touch_network, set_ideal_net,
  set_fix_hold): the 2026-10-08 gf12 rerun stopped at Genus's `set_dont_use`
  (the first of 155, after set_driving_cell; only the other 154 were after
  it, so the power numbers didn't change, but the postcondition failed). The list is explicit because read_sdc accepts some
  SDC commands that `info commands` doesn't list (set_logic_zero,
  set_max_dynamic_power, ...). To redo only those steps: `make
  clean-synopsys-ptpx-synth-{idle,active}-power` (sweep_specs
  `--standalone-only --make ...`), then rebuild.
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
- [`THESIS/apps/CGRA_APP_POWER.md`](THESIS/apps/CGRA_APP_POWER.md) — whole-CGRA app
  power → Ch. 5 app figures: config-name map, /aha + build-machine + ingest commands,
  what garnet must add.
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
A crash can also be `rc=134` ("double free or corruption") at exit, after
"garnet.py DONE" with garnet.v already written, and a hung one can sit
sleeping. Check for garnet.v rather than trusting rc.

### 5.4 Gotcha: never iterate `get_port_names()` unsorted (fixed 2026-10-11, session 1fa8a391)

kratos' `Generator.get_port_names()` comes back as a Python **set**, so a loop
over it follows `PYTHONHASHSEED` (random per process). `extract_top_config()`
order becomes the order of garnet `CoreCombinerCore`'s read-only (pass-through)
configs on the core's `read_config_data` mux, so the PE tile's readback mux took
`reduce_pe_cluster` O2/O3/O4 in a different order on every `garnet.py` run, and
RTL (`garnet.py --verilog`) and bitstream (`aha pnr`) are different processes.
Every loop in lake now iterates `sorted(...)`; do the same in new code.
`tests/spec_unit_tests/test_port_order_determinism.py` runs extract_top_config /
get_interface under five seeds. Still seed-dependent but only textual:
kratos' own `util.to_magma` (port declaration / connection order) and its C++
FSM default-assignment order, so compare two `garnet.v` functionally (strip
trailing commas, sort lines), not by md5. Details: garnet mflowgen/CLAUDE.md
"App bundles".

---

### 5.5 Tile data width = spec `data_width` (2026-10-06)

garnet builds the whole fabric at the spec's `data_width` (dw). Garnet side:
garnet `mflowgen/CLAUDE.md` "Fabric data width follows the spec". Lake pieces:

- `MemoryTileBuilder(data_width=)` (CoreCombiner passes it): legal port widths
  are `[dw+1, dw, 1]` per instance. Any other width is still split into 1-bit
  `_b_<i>` pins (silently), so every controller data port must be dw or dw+1.
  The class attribute `legal_widths=[17,16,1]` stays for old callers.
- `realize_mem_config`: the config-bus SRAM word is `min(dw, 32)` (it was a
  hard-coded 16).
- Sparse token layout per width: `MemoryController.token_opcode_bt/
  token_stop_bt/token_proxy(dw, code)` and `const_concat`. EOS is at bit dw,
  opcode `[9:8]` and stop `[7:0]` for dw ≥ 10 (dw 16 returns the old
  `DONE_PROXY`/`OPCODE_BT` objects); dw 8 shrinks to opcode `[7:6]`, stop
  `[5:0]` (sam does not know this layout). Intersect, CrdDrop, Locator, Repeat,
  RSG, Reg, OnyxPE and ReducePECluster take it from `self.data_width`.
- `OnyxPEInterface.inst` width = `Assembler(gen_Inst_fc(dw).Py).width`.
- StrgRAM/StrgRAMRV: `int_addr_width = max(addr_width, mem_addr_width +
  word_width)`, zero-extending a narrow track address (dw8).
- kratos re-reads `@always_*` source from disk: editing a generator file while
  a garnet.py build imports it gives bogus `NameError`s in `<ast>`.
- **Compiler collateral carries the width (2026-10-10, `spec/spec.py`; §5.5
  code committed as lake 2f20b666, aha mek 30abb17).** `extract_compiler_information` now writes `data_width` (the
  ports' external width) at every level. Before, only the pond (`regfile`)
  collateral had it. Halide, clockwork and MetaMapper take the CGRA data width
  from `LAKE_COLLATERAL_JSON_MEM`'s `data_width`, so the spec is the single
  source for fabric and compiler. All other keys are unchanged. The thesis_sweep
  tests check individual keys, so they don't care.
- **Test (2026-10-12):** `tests/spec_unit_tests/test_collateral_data_width.py` (29 tests, about
  1 s).
  - `data_width` at the `mem` level (static, static dual-port, RV) and the `regfile` level
    (static and RV pond), for dw 8/16/32/64.
  - It also runs `spec_config_to_collateral.generate` and `pond_collateral.generate`, the files
    `aha map` uses.
  - dw64 uses fw2, because dw64 × fw4 has no SRAM macro (`build_spec_rv` asserts "No valid
    macros").
- **Spec storage check splits the word like CoreCombiner (2026-10-22, session a16b3973).**
  `build_spec` / `build_spec_rv` check their remote storage against the GF library with
  `GF_Tech_Map_any_columns` (`lake/top/tech_maps.py`): the full-width macro when one exists (as
  before), else the first 2-, 3-, ...-column split that does, else the old AssertionError. The
  remote storage's tech map builds no hardware; the tile's `CoreCombiner._select_gf_tech_map`
  picks the macro (cols=2 first). Before, RV fw2 dw64 dual-port (128-bit word: no single SDPB
  column, but 2 x `W00512B064` SDPB) asserted "No valid macros" although the tile builds; now it
  builds with 2 columns. A thesis RV fw4 dw16 4 KB garnet.v is byte-identical before and after.
  Test: `tests/spec_unit_tests/test_spec_tech_map_columns.py`.

### 5.6 Clock gating: what synthesis gates and what it can't (audit 2026-10-12, session 5b33d095)

- **Synthesis already gates almost every register.** The mflowgen Genus node
  defaults to `gate_clock: True` (`lp_insert_clock_gating true`); no lake or
  garnet graph overrides it. Genus extracts enables even from separate
  `always_comb` next-state logic and gives each array entry its own ICG. Examples:
  each IterationDomain counter is gated separately (so only the levels that step
  are clocked), and the storage is gated per word.
  - Locally: fw4 SP 2×2 4 KB, freepdk45, Genus with `--clock-gating` on
    `generic_synth_power.sh` (uncommitted, see below): 36,620 of 36,644 flops
    gated. Idle 17.8 → 4.73 mW, active 29.9 → 11.4 mW vs no gating.
  - Left ungated: 1–2-bit registers below the minimum bank width (each ID's
    `finished_lcl_sticky`, 1-bit AGs, memintfdec delay bits); gating them costs
    about what it saves. Also the static OUT ports' step/mux_sel/restart delay
    lines (`shift_reg`, enable = 1, spec.py `shreg_*`).
- **The RV comparison network has no registers** (all `always_comb`, plus config
  bits): about 2 µW idle in a gf12 tile. Nothing to gate.
- **What synthesis can't fix is logic that is busy every cycle.** gf12 tile
  synth-level power from the dw16 sweep (fw4 SP 2×2 4 KB static, idle 1.59 mW;
  lakespec 982 µW):
  - The static ScheduleGenerator `clk_ctr` is a free-running cycle counter.
    It is `add_counter(increment=1, clear=flush)` in schedule_generator.py,
    one per SG including the port-internal SIPO/PISO SGs: 4×34 + 4×39 = 292
    flops. It counts every cycle even in an unconfigured port. It costs 564 µW
    in idle AND active: 57% of lakespec idle, 35% of tile idle.
  - ICG cells see the clock every cycle: about 231 µW static, 347 µW RV.
  - RV in-ports' filter SG counts input valids even when unconfigured
    (port.py `external_count`), about 132 µW (RV tile). Gated under
    `clock_gate_ports` (below), including configured ports the app doesn't filter.
  - Smaller items: the arbiter `grant_line` rotates every cycle (~12 µW); the
    static OUT delay lines (~9 µW per port).
- **Two opt-in Spec options cover the biggest items (lake 9f9480c9,
  2026-10-14). Both are off by default; off = byte-identical RTL and bitstreams.**
  They are kwargs of `Spec`, `build_spec`, `build_spec_rv` and thesis_sweep's
  `build_four_port_wide_fetch`, and flags of thesis_sweep.py and
  gen_power_bitstreams.py. garnet passes them through `spec_config.json`.
  - `share_clk_ctr`: one `shared_clk_ctr` at the spec top
    (`Spec._build_shared_clk_ctr`). It is as wide as the widest static SG (39
    bits on fw4 2×2: 292 flops → 39), and each static SG takes its low bits on
    input `shared_clk_ctr`. That covers the port SGs and the SIPO/PISO SGs in
    vectorized ports (`Port.use_shared_clk_ctr`). The values are the same: every
    clk_ctr clears on flush and counts each clk_en cycle, and each wraps at
    its own width. The bitstream is unchanged. Filter SGs (`external_count`)
    keep their own counter. An RV spec has no free-running SG, so the option
    changes nothing there.
  - `clock_gate_ports`: each port's Port/ID/AG/SG/rd_buf/memintfdec runs on
    `port_<i>_gclk` = clk OR ~`port_<i>_active` (`gated_clock` in
    lake/utils/util.py). Held high while the port is off. `active` comes
    straight from a config flop, so it changes only while clk is high, when an
    OR gate cannot glitch. There is no falling-edge logic (see "Gate design"
    below). `active` is a 1-bit `port_enable` config field in the port's ID, which
    `IterationDomain.gen_bitstream` sets. So every port the app configures
    runs, and the rest stop. Dimensionality can't serve as the enable: since
    e4fabdc1, dimensionality 0 means run once. With both options on, the
    shared counter also stops while no port is configured (enable = clk_en AND
    any port_enable). A gated build needs bitstreams from the same gated spec.
    garnet does that, since its power-test generator rebuilds from
    spec_config.json; lake's power-test-gen takes `--clock_gate_ports`.
  - `clock_gate_ports` also gates each RV in-port's **item filter** (2026-10-14,
    user request). That SG counts tokens (`external_count`, 2 × 34-bit regs) on
    every RV in-port, filtered or not: when the app gives a port no filter,
    `Port.gen_bitstream` programs the dense pass-everything schedule (offset 0,
    strides 1, e0, ...), which steps on every token.
    - The filter SG gets a 1-bit `filter_enable` config field, set only when the
      app gives the port a filter (`id_map` has offset/strides). Clear, its
      step is forced to `~finished` (& ~flush & enable) whatever its frozen
      registers hold: the same steps as the dense program. With dense strides
      the recurrence deltas are all 1, so that program's counter and schedule
      stay equal until the ID's `restart` (its last step), and its `finished`
      latches the cycle after. Tokens after the domain are eaten either way.
      The external count is masked too.
    - Its clock is `filter_gclk` = clk OR ~filter_enable (same gate as the
      ports). Flush isn't needed: with the bit clear, the SG's registers don't
      matter.
    - An unconfigured port is gated whole by `port_enable` anyway. The filter
      gate is what saves power in configured ports the app doesn't filter: most
      ports of real apps, and every port of the memtile `active` test.
    - Checked (session 5b33d095):
      - Flag off: byte-identical RTL; the static gated RTL is unchanged too.
      - Spec unit tests pass with clock_gate_ports forced on all specs: RV
        programs with and without filters, and the filter FIFO.
      - garnet RV tile memtile power test: idle + active PASS, same output
        activity as ungated; every `filter_gclk` TC 0.
      - **RV conv_3_3 end to end (garnet `gen_app_bundle.py --config
        fw4_dw16_sc4096_sp_in2_out2_vc2_portcg_rv`, 16×16): bit-accurate.** No
        app filter, so every MEM in-port ran the bypass. This was also the first
        real-app run with port gating.
      - Re-run 2026-10-18 on the OR gate, all the same: gold sims 4/4,
        suites, the three gated tile tests (gated clocks TC 0 in idle, 1999 on
        configured ports in active), and RV conv_3_3 bit-accurate.
  - Checked: thesis_sweep fw4 2×2 gold sim PASS in all four combinations.
    The spec unit tests pass (342, 5 xfail) with each option forced on all 89
    specs they build (scratchpad `sc/run_forced_pytest.py`). The garnet tile
    memtile power tests pass idle + active, with SAIF port clocks at TC 0 in
    idle and running in active (garnet mflowgen/CLAUDE.md "Clock-gating A/B").
    Tests: `tests/spec_unit_tests/test_spec_power_options.py`.
  - Measured so far for port gating:
    - gf12 estimate from the dw16 reports: port-logic modules draw 891 µW of
      the tile's 1590 µW idle (static, ~56%) and 582 of 1410 µW (RV, ~41%),
      almost none of it leakage.
    - Locally (freepdk45, fw4 SP 2×2 4 KB, ideal clocks) with Genus auto-CG,
      port logic in idle drops from 0.56 to 0.28 mW (leakage only). Without
      auto-CG the idle total drops from 17.8 to 16.7 mW (port logic 1.38 to
      0.31). Active is unchanged.
  - **Gate design (2026-10-18).** The first gate captured the enable on the
    falling edge (half a cycle of budget) and ANDed it with clk. The first gf12
    A/B (below) failed timing on those flops, from the tile's `reset` (async
    recovery) and `flush` (filter gate) inputs. Both arrive at `pt_i_delay`
    0.7 ns into the 1.333 ns cycle, after the 0.667 ns falling edge:
    - RV gated: synth -99 ps, PT -82 ps;
    - static gated: PT -1 ps on reset.
    A rising-edge register in front of an OR gate fixed the timing but opened
    the gate a cycle late. lake's standalone gold test writes config at one
    rising edge and drops flush after the next, so its ports missed the flush:
    the schedules kept their reset values and the test failed. The current
    gate is the OR fed straight from the config flop: no falling-edge logic,
    and the gate opens at the edge after the config write, as the falling-edge
    version did.
  - **gf12 A/B through PnR** (garnet `sweep_specs.py --spec-set
    clock_gating`, 750 MHz, fw4 4 KB 2×2; zip
    `/aha/clock_gating_ab_20261017_r8cad-gf12_20261018-022542.zip`, extracted
    in `/aha/sweep_out/clock_gating_ab_20261017/`). This run had the
    falling-edge gate. The static numbers below hold for the OR gate, which
    gates the same flops. The RV configs are being re-run: the RV base was
    OOM-killed in Innovus postroute with six PnR builds at once, and the RV
    gated config failed timing. Tile power in mW, total / port logic:

    | static | synth idle | synth active | PnR idle | PnR active | PnR logic µm² |
    |---|---|---|---|---|---|
    | base | 1.59 / 0.89 | 10.5 / 2.26 | 2.09 / 0.81 | 11.6 / 2.26 | 9340 |
    | share_clk_ctr | 1.02 / 0.31 | 9.95 / 1.68 | 1.52 / 0.25 | 11.0 / 1.71 | 9002 |
    | clock_gate_ports | 0.70 / 0.11 | 10.5 / 2.20 | 1.16 / 0.11 | 11.7 / 2.27 | 9405 |
    | both | 0.70 / 0.11 | 9.90 / 1.62 | 1.21 / 0.11 | 11.1 / 1.72 | 9024 |

    - Shared counter: -36% idle, -5% active at synth (-27% / -5% at PnR);
      -3.6% logic area.
    - Port gating: -56% idle at synth (-44% at PnR), active unchanged.
      +0.7% PnR logic area, and +0.2 mW at PnR in active: the clock-tree cells
      after the gates sit at lakespec's top.
    - Both: -56% / -42% idle (synth / PnR), -6% / -4% active.
    - The leftover 0.11 mW in gated port logic is leakage plus the ungated
      config path.
    Per-block split: scratchpad `ana/hier_split.py` (session 5b33d095) on each
    `*-memtile-power-*/outputs/power.hier`.
  - **Second gf12 run (2026-10-18/19, all six fresh; the reference numbers).**
    Lake 2660629d (OR gate) and garnet with working `--no-pd`, so no power-domain
    logic: compare only within this run. Zip
    `/aha/clock_gating_ab_20261018_r8cad-gf12_20261019-041557_slim.zip`, extracted in
    `/aha/sweep_out/clock_gating_ab_20261018/`. All six built through PT signoff.
    - Synth WNS is 0 everywhere, the RV gated tile included (it was -99 ps with the
      falling-edge gate).
    - PT WNS is -6 to -23 ps, all on the tile's pass-through feedthroughs
      (config_*/flush -> *_out), as in the base. No gate logic is in any top-100
      path.
    - The RV power steps failed: `/sim/docker` was full again at
      memtile-power-test-gen. They are a power-only re-run on the existing builds.

    | static | synth idle | synth active | PnR idle | PnR active | PnR logic µm² |
    |---|---|---|---|---|---|
    | base | 1.58 / 0.89 | 10.5 / 2.26 | 1.98 / 0.81 | 11.5 / 2.26 | 9335 |
    | share_clk_ctr | 1.02 / 0.31 | 9.94 / 1.66 | 1.46 / 0.25 | 10.9 / 1.70 | 8982 |
    | clock_gate_ports | 0.65 / 0.07 | 10.5 / 2.21 | 1.03 / 0.07 | 11.5 / 2.25 | 9345 |
    | both | 0.65 / 0.07 | 9.89 / 1.61 | 1.07 / 0.07 | 11.0 / 1.72 | 8952 |

    (mW, tile total / port logic.)
    - Shared counter: idle -35% / -26% (synth / PnR), active -5% / -5%, PnR
      logic area -3.8%.
    - Port gating: idle -59% / -48%, active 0% / 0%, area +0.1%.
    - Both: idle -59% / -46%, active -6% / -4%, area -4.1%.
    - RV logic area: base 10263, gated 10270 µm² (+0.07%).
  - **Thesis CGRAs (2026-10-21; user: thesis results come from `garnet.py --thesis
    static|rv`).** Both options reach a thesis MEM tile unchanged: `cgra/util_thesis.py`
    builds it with `build_spec` / `build_spec_rv(**spec_config.json, comply_17=False)`.
    garnet's `--spec-set thesis_fabric_opt` / `--recipe thesis_opt` (session cfc4174c)
    builds the gated tiles on gf12.
    - Bit-accurate end to end on 8×8 thesis CGRAs (fw4 dw16 4 KB 2×2; current RV
      flow, no EXHAUSTIVE_PIPE): conv_3_3 and gaussian on static with
      share_clk_ctr + clock_gate_ports (4 port gates + 1 shared counter), and on RV
      with clock_gate_ports + filter_fifo_depth 8 (9 gates: 6 ports + 3 item
      filters); conv_3_3 also on RV with clock_gate_ports and add_filter_path=False
      (6 gates).
    - Harness: scratchpad `thesis_cg/run_app.sh` (session 5b33d095), adapted from
      cfc4174c's `thesis/run_app.sh`.
  - **Every lake spec, the PE-tile pond included (2026-10-22, user: "the pond (and any
    lake-based spec) should see the clock gating").** Both options are kwargs of every
    builder: `build_spec`, `build_spec_rv`, `build_pond`, `build_pond_rv`,
    `build_four_port_wide_fetch_rv` (garnet's built-in MEM spec) and thesis_sweep's
    builder.
    - `build_cgra_pond` / `resolve_cgra_pond_params` accept them as pond-spec keys
      (`CGRA_POND_POWER_KEYS`). The pond collateral is byte-identical with or without
      them.
    - garnet's PE-tile pond inherits them from the MEM spec config unless its own
      JSON sets them (garnet `with_mem_power_options`, stock and thesis fabrics), so
      one spec config gates every lake spec in a CGRA. The legacy PondCore (static
      fabric without `--lake-pond-spec-config`) is not a lake spec and stays ungated.
    - Options off: byte-identical RTL from every builder.
    - Gated static pond: 4 port gates + 1 shared counter. Gated RV pond: 5 gates
      (4 ports + the item filter).
    - Bit-accurate end to end (16×16 thesis CGRAs, scratchpad
      `thesis_cg/run_app.sh`): matmul_rvtry3 (pond packed into a PE, used for the
      accumulation) on static (MEM share_clk_ctr + clock_gate_ports, inherited by the
      pond) and on RV (MEM clock_gate_ports + ff8, pond 128 B / 6 dims, inherited).
- **Other ideas, not implemented:**
  - stop the shared counter once every port's ID is finished;
  - enable the OUT delay lines with `quali_step`;
  - rotate the arbiter only on a request.
- **Measuring gated designs before CTS: use ideal clocks.**
  `generic_synth_power.sh`'s PT sets `set_propagated_clock`. A gated clock
  driven by one unbuffered AND/ICG then has very slow edges, and flop CK
  internal power is inflated: +4.8 mW active on fw4 2×2, which disappears with
  ideal clocks on both builds. Genus netlists also vary ±0.5 mW between small
  RTL changes, so compare by block (port logic vs the rest), not total only.

### 5.7 Hybrid-port policy and the dense RV PE (thesis fabrics, 2026-10-17, session cfc4174c)

garnet `--thesis static|rv` (garnet `mflowgen/CLAUDE.md` "Thesis fabrics") builds a
static-dense and a ready-valid-dense CGRA that carry only their own mode's hardware.
Lake pieces (all default off: the default/Zircon tiles are unchanged):

- `MemoryTileBuilder(hybrid_policy=)` / `CoreCombiner(hybrid_policy=)`: what a
  controller's hybrid port (`HybridPortAddr`: an RV port that can also run static)
  becomes. `"bypass"` (default) = the tile FIFO plus `*_bypass_rv` /
  `*_fine_grain_fifo_bypass` config regs; `"rv"` = the tile FIFO, no bypass regs;
  `"static"` = no FIFO and no tile valid/ready, the controller's valid (in) / ready
  (out) tied 1 (a static spec port ignores valid; its ready is the schedule step).
  `"static"` raises on a true RV port (one without `HybridPortAddr`).
- Controller class attribute `rv_1bit_ports = True`: its 1-bit data ports go on the
  RV interconnect too (as `reduce_pe_cluster_flat` always did).
- `build_spec` / `build_spec_rv` / `build_pond_rv` (and `build_cgra_pond(rv=True)`)
  take `comply_17` (default True = the extra always-0 top bit for a fabric whose
  data tracks carry the sparse end-of-stream bit). The thesis fabrics use False.
- `lake/modules/onyx_dense_rv_pe.py` `OnyxDenseRVPE`: OnyxPE for dense RV only --
  dw-bit data ports (no EOS bit), no static mode (`bypass_rv_mode`), no sparse
  maybe/EOS logic, ports not hybrid; keeps the RV synchronization (active
  encodings, constant PE), the primitive output FIFO with `bypass_prim_outfifo`,
  and the 1-bit output FIFO. Config mode "alu", same `get_bitstream` keys as OnyxPE
  (`bypass_rv=True` asserts). In a PE tile alone: 8 ports, each with its tile FIFO
  (`cfg_reg_fifo`, bogus-init pre-fill).
- Test: `tests/test_hybrid_policy.py` (4 tests, about 5 s).

## 6. Ready-valid (RV) bitstreams from clockwork (2026-09-28/29)

Path: clockwork `DENSE_READY_VALID=1` tile config (static keys + RV program:
`port_mappings`, `access_map`, `domain`, `dep_values`, `precursor_deltas`)
→ `SpecMemoryController.get_bitstream` → `Spec.gen_bitstream` →
`rewrite_app_json` + `convert_app_json_to_config` (`lake/spec/spec.py`).
garnet's default MEM spec is `build_four_port_wide_fetch_rv(4096,16,4)`
(`opt_rv=True`: ports IGNORE vec configs; wide-fetch packing inferred in HW).

- **Routing (committed `140eced8`)**: THESIS-only `_is_clockwork_format` sent
  RV tiles down the static converter; now RV path iff `any_rv_sg and
  "dep_values" in cfg`.
- **Converter fixes (committed `6f73bc91`; RTL-verified; the 11 tiles that were already
  correct are bit-identical, incl. chip-validated conv_3_3/gaussian)**:
  (a) precursor ADDRESS offset = Σ_levels delta × address_stride (was: first
  nonzero level only, × domain EXTENT → wrong when pitch≠extent, e.g. harris
  lxx 68 vs 66, and for multi-level [row,col] deltas); (b) zero-lag reader
  (no precursor, dep `[0,0]`): RAW scalar 0 → `2*fw*vec_capacity+4` (reader
  raced the wide-fetch commit → read previous lap / X).
- **Opt-in `LAKE_RV_ROW_RAW=1`** (committed `6f73bc91`; default unchanged = what runs on
  chip): reader RAW at the ROW level (outermost precursor level, or level 1 for
  zero-lag) with scalar `-delta+1`. The default compares COLUMN counters with a
  single-row wrap fixup, so under writer/reader stalls lagged taps overrun a
  stalled writer and zero-lag readers deadlock (0/30 tiles under stress).
  Row-level RAW costs one row of latency.
- **RTL harness** (session scratchpad `rv_tiles_{gen.py,run.sh,check.py}`, gold
  = reader iteration i ← writer iteration i − precursor delta, independent of
  lake addressing; tb = copy of `tests/test_spec_hw/tb.sv` + a 6-port
  `lakespec_mem` case with external SRAM (`remote_storage`), MAX_DATA_SIZE 64K;
  stress = random handshakes + writer stall 500–1499 + r0 stall 2000–5999).
  Image apps on garnet default, 30 tiles: default encoding before fixes 11/30
  free-flow; after (a)+(b) 25/30 free-flow; with `LAKE_RV_ROW_RAW=1` 25/30
  free-flow AND 23/30 stress (all fw-aligned tiles).
- **FIXED (RTL, `lake/spec/port.py`, committed `6f73bc91`) — opt_rv write-combining
  buffer (WCB) hazard** (harris 66-wide, camera 125-wide rows): the opt_rv IN
  port's 2-entry WCB checked the pending-commit sticky of the *current* entry
  W, but on a new-address cycle the data lands in entry W+1 (`addr_into_wcb`),
  which still holds the word from two addresses back. A short row tail (1–2
  elements) gives the last full word only 1–2 cycles to commit, so the next
  row's first element overwrote its lane 0 whenever the SRAM port was busy with
  reads. Fix: `wcb_target_entry` = `already_written & new_address ? W+1 : W`
  and gate `write_wcb` on that entry's sticky. Applies to every `opt_rv` wide
  IN port (garnet default AND `build_spec_rv` thesis points; not fw1). Result:
  30/30 image tiles correct free-flow (default encoding) and 30/30 under stress
  (`LAKE_RV_ROW_RAW=1`). Lake's own `four_port_single_mp_wide_fetch_rv.py
  --opt_rv --test linear` and `dual_port_wide_fetch_quad_rv.py --opt_rv
  --fetch_width 2` PASS; `--fetch_width 4` FAILS identically before/after
  (outputs byte-identical; gold has 1024 entries vs 4096 dumped — test-harness
  issue, pre-existing). NOTE: silicon has the original logic, so on-chip RV
  apps with row extent % fw ≠ 0 can hit this under read contention.
- **FIXED (RTL, `lake/spec/port.py`, committed `6f73bc91`) — opt_rv READ port deadlocks
  under reader backpressure** (found with reuse buffers: a word re-read many
  times, readers that may prefetch). The read port keeps words in a 2-entry
  buffer (tag CAM) and an 8-deep address queue whose entries carry a
  `pop_wcb` flag ("first access of a new word: evict the oldest word").
  (1) `read_memory_out` for a new word was not gated by queue space, but the
  entry push is → with a full queue the SRAM read goes out, the entry is pushed
  later as a same-word access WITHOUT the flag, the old word is never evicted,
  and the next new word can never be written → hang (resnet kernel banks at the
  word-2 switch). Fix: `& ~reg_fifo.full` on `read_memory_out`. (2) The very
  FIRST read also carried the flag; if a second word is prefetched before that
  entry pops (reader starts under backpressure, e.g. after a barrier), the pop
  evicts word 0 while its accesses are still queued → hang after 1 output. Fix:
  `pop_wcb_in` also requires `already_read`. Both are no-ops for the image apps
  (30/30 tiles still pass free-flow + stress) and lake's own
  `four_port_single_mp_wide_fetch_rv.py --opt_rv --test linear` /
  `dual_port_wide_fetch_quad_rv.py --opt_rv --fetch_width 2` still PASS (fw4
  fails identically before/after). Silicon has the original logic.
- **Reuse buffers (clockwork side, see `clockwork/CLAUDE.md` "RV reuse
  buffers")**: clockwork now emits `filter` for broadcast-banked writers,
  linearized address maps for non-SR banks, and explicit `[level, level,
  scalar]` deps (barrier scalar 16383; read-modify-write sweep `-d` on a
  coalesced level); `LAKE_RV_ROW_RAW` no longer rewrites explicit scalars
  (`spec.py`). resnet 19/19, resnet_block (scratch `resnet_block_rvtry`, output
  un-chained) 27/27, matmul MEM 2/2 tiles pass free-flow + stress.
- **Accumulation ponds**: clockwork now emits lake's `get_vec_accum_pond`
  program for accumulation ponds (clockwork notes "Accumulation ponds"); for
  matmul n=16 the `build_pond_rv` bitstream is bit-identical to
  `get_vec_accum_pond(16, 256)`.
- **FIXED 2026-09-29 (`lake/modules/arbiter.py`, user-approved, committed `6f73bc91`) —
  1-input arbiter grant ignored the request.** `modules/arbiter.py`, `ins == 1`:
  `grant_out = resource_ready` (commit 64a71ead/6293055a, paired with
  `step = mid_grant & quali_step` in `spec.py`). The merge 35523f0e ("update
  some files with main") took master's `spec.py` (`step = mid_grant`) but kept
  THESIS's arbiter, so every fw=1 port whose memory port has ONE requester
  steps its iteration domain / address generator EVERY cycle, access or not,
  and an IN port's `ready` = 1 even while its constraints block it (data
  dropped). Hits every `build_pond_rv` port and garnet default's filter ports
  (ports 2/5, `MemoryPort_28/29`); multi-requester memory ports (all MEM data
  ports of the RV specs) are unaffected. RTL symptom on the pond: update reads
  race the writes, only 1 clear, final read never fires. Fix = master's line
  `grant_out[0] = resource_ready & request_in[0]` (no comb loop: pond/filter
  ports are pass-throughs, valid is external). All pond programs PASS free +
  stress; garnet-default MEM tiles unchanged (image 30/30 free, 30/30 stress
  with `LAKE_RV_ROW_RAW=1`, 0/30 stress default encoding = the known baseline;
  reuse 48/48 free + stress). Pond sims also PASS with garnet's
  `build_pond_rv(dims=4)`. The unqualified grant is THESIS-only (lake master
  has the qualified one); which version any silicon used is not verified.
  The pond's init is its dangling `flush_port` (own ID/AG/RV SG, valid tied 1,
  data 0) on a `flush_mem=True` write memory port: each step clears the WHOLE
  storage (`storage.py` materialize_w_clear; in the CGRA,
  `top/memory_interface.py` ORs the clear port's `clear` into the storage
  flush), sequenced only by the RV constraints.
- **FIXED 2026-09-30 (`lake/spec/spec.py`, committed `6f73bc91`) — narrow RV read ports
  on a delay-1 memory port dropped/duplicated data under backpressure.** For a
  ready-valid OUT port that is not a wide-fetch opt_rv port (fw=1 data ports of
  `build_spec_rv(vec_width=1)`, and the FILTER output port of every RV spec),
  the read was issued on the schedule step alone, the ID/AG stepped on the
  downstream ready of the ISSUE cycle, and the data returned one cycle later
  for one cycle only: whenever ready changed between issue and return an item
  was duplicated or lost (found by `test_spec_rv_programs` on
  fw1 dp 1x1 under stress: reader outputs 0,0,1,2,…). Fix: a 2-entry
  `RegFIFO` (`port_<i>_rd_buf`) between the memory interface decoder and the
  Port; a read (and the ID/AG step) is issued only if
  items + arriving − popping < 2; valid comes from the FIFO. Full throughput.
  Delay-0 memory ports (the pond) keep the old wiring. RTL changes for every
  RV spec (filter out port) and fw1 RV specs; wide-fetch data ports unchanged.
- **FIXED 2026-09-30 (`lake/spec/iteration_domain.py`, committed `6f73bc91`)**:
  `gen_bitstream` accepted extent 2**extent_width + 1, whose last index wraps
  to 0 on the extent_width-bit `iterators` port (read by the RV comparison
  network, the Port filter, non-recurrent AG/SG). Now extent ∈ [2, 2**ew].
- **`lake/modules/rv_comparison_network.py`**: (a) reader→writer `sel_in` sized
  from the reader's own dimensionality (was the last writer's; RTL identical when
  all ports have equal dims, as in every builder) — committed `6f73bc91`.
  (b) **FIXED 2026-10-08, THESIS `8748240d` (aha mek `516647b`):** the wrap fixup
  reads each port's iterator one level up via `sel + 1`. In clog2(dims) bits that
  wrapped to level 0 at the TOP level of power-of-2 dims (2, 4, 8), so the extent
  was added whenever the two ports' level-0 iterators differed (a gated port could
  step early). Out-of-range selects give 0 = "no outer level", so the fix widens
  the select one bit only for those dims (`_outer_counter`). RTL changes only for
  power-of-2-dim ports: of the shipped builders, only the default RV pond (4-D);
  the default MEM tile, thesis `build_spec_rv` points and 6-D ponds are
  byte-identical (1-D was already correct: `sel + 1` = 1 is out of range). The
  6f73bc91 bitstream guard (`_check_fixup_level`) is removed. Verified:
  `test_rvcn_rtl_random_top_level` (old RTL fails the dims 2/4/8 seeds), 12
  clockwork accumulation-pond programs on a 4-D pond free + stress 24/24 with
  identical cycle counts. Silicon built from earlier lake still has the bug; no
  current clockwork program puts a non-barrier constraint at those levels.
  Related limit: the fixup adds `counter + extent` in iterator width, so a
  level whose fixup can fire must have extent ≤ 2**(ew−1) (1024 for 11-bit IDs).
  (c) **Same-storage pruning (2026-10-11, `ab354105`): `build_spec_rv`
  (`prune_rv_comparators=True`, the default there) builds comparators only between
  ports on the same storage.** The filter path's in/out pair sits on its own
  storage and the converter rejects any filter↔data dependence, so 2·(n_in+n_out)
  blocks could never be enabled: 8→4, 18→10, 50→34. A pruned pair's comparisons bit
  is tied to 1 (what a disabled block outputs), and its 25 config bits disappear from
  the top of the config space (the network is the last slice), so every other config
  offset is unchanged. A constraint on a pruned pair raises ValueError.
  `build_four_port_wide_fetch_rv` (garnet default / silicon) keeps the full network
  by default and is byte-identical; `Spec(prune_rv_comparators=...)` /
  `RVComparisonNetwork(prune_cross_group=...)` are the knobs. Checked: the RTL
  differs only in the network + config width; 546 RV MEM tiles of the 09-30 compile
  sweep on 6 specs keep identical config bits below the network. Generic Genus
  (freepdk45, 10 ns, hierarchy kept) lake controller: −12% to −20%, about 593 µm² per
  removed comparator. Each comparator's area splits into 44% level-select muxes,
  12% extent mux, 29% LFCompBlock (both LT and GT adders and compares, 16-bit
  scalar) and 15% wrap fixup. At the tile, the config registers also drop (1×1:
  36→32 words). Results and the app port-dependency audit (4×4 apps enable only
  10 of the 34 remaining comparators, all on W0/W1) are in
  `/aha/sweep_out/rv_prune_20261011/RESULTS.md`. After this lands, an RV
  lake-spec garnet.v built from older lake no longer matches bitstreams from newer
  lake: rebuild garnet.v.
  **Five-way tile comparison (2026-10-14, same flow, fw4 sc8192 2×2 Tile_MemCore):**
  RV as first built 69,437 µm², per-storage network 63,067, FIFO-8 filter path
  57,755, no filter path 50,346, static 58,689. Static is 17% larger than RV without
  a filter path: its cycle-counter SGs are 6.6× RV's, its port buffers carry their
  own schedules, and it needs 69 config words to RV's 40. A 2×2 needs 6 of the 8
  remaining data comparators (never a WAR on W1; R1<-W1 is only ever a barrier).
  Per-comparator dependency table and trimming estimates:
  `/aha/sweep_out/tile_area_20261014/RESULTS.md`.
  (d) **Trimmed comparators: `rv_comparator_trim` (2026-10-25, session cfc4174c; opt-in,
  off = byte-identical RTL and bitstreams).** The user asked to trade a little performance
  for a smaller network. It is a kwarg of `Spec`, `build_spec_rv` and `build_pond_rv`, and an
  RV pond-spec key (`CGRA_POND_KEYS_RV`). garnet's RV pond inherits it from the MEM spec
  (`with_mem_power_options(..., rv=True)`). `True` = `RV_CMP_TRIM_DEFAULT`; a dict picks
  `RVComparisonNetwork`'s options:
  - `one_sided`: each block has its direction's op built in (reader-gated LT, writer-gated
    GT), with no `comparison_op` register and one adder. LT(s) = GT(-s) on the integers, so a
    constraint written with the other op is programmed with -s.
  - `scalar_width` 8: a signed `comparison_scalar`, computed in one width that holds every
    sum. An out-of-range scalar raises at bitstream time.
  - `barrier_bit`: a constraint LT with scalar >= 16383 (the converter's barrier) sets a
    `barrier` bit with selects 0: the comparison stays 0 until either side finishes, so a
    barrier at any level needs no level mux.
  - `max_level` 3: the level selects cover levels 0..2. The wrap fixup's outer counter is a
    mux over levels 1..3 indexed by the select itself. Deeper non-barrier constraints raise.
  - Converter (`_rv_scalar_fits`): when a column-level RAW scalar doesn't fit (rows wider
    than about 129 elements at 8 bits: -(W-1)), the reader gets the row-level RAW instead
    (the `LAKE_RV_ROW_RAW` encoding, one row of latency). Untrimmed specs never take this
    branch, because every column scalar fits 16 bits.
  - Why these limits: the 2026-10-11 app audit (`/aha/sweep_out/rv_prune_20261011/audit/`,
    6,190 constraints) has RAW only on reader-gated blocks (LT) and WAR only on writer-gated
    ones (GT); 35% are barriers. Non-barrier scalars span -127..36 (603 fail at 7 bits), and
    non-barrier levels are 0..2 only.
  - Area (generic Genus, freepdk45, hierarchy kept, 10 ns; thesis RV MemTile controller fw4
    dw16 4 KB 2x2 nofp portcg, and the default RV pond):
    - MemTile: network 4,735 -> 2,580 um2 (-46%), controller 18,986 -> 16,563 (-12.8%).
    - Pond: network 4,142 -> 2,804 (-32%), pond 7,938 -> 6,505 (-18%).
    - MemTile network by option: one_sided -696, 8-bit + barrier -119, max_level 3 -1,404
      (plus -268 outside the network: SG extent outputs nobody reads now).
    - Network config: 200 -> 112 bits (MemTile), 184 -> 112 (pond), in the tile's config
      registers (not in these numbers).
    - Shortest network path (network alone, 0.6 ns target): MemTile 1,022 -> 956 ps, pond
      1,045 -> 945 ps. The fixup adder and the compare stay.
    - Scripts and reports: scratchpad cfc4174c `cmptrim/synth/`.
  - Verified:
    - `tests/spec_unit_tests/test_rv_comparator_trim.py` (26 tests, ~1 min): block and network
      RTL against the full block's models (MemTile and pond shapes, mixed dims, barriers at any
      level, both ops on one-sided blocks), bitstream checks, and the wide-row fallback in RTL,
      free and stressed. Mutation-checked: dropping the op swap, saturating barriers or
      off-by-one fixup levels each fail 3-6 RTL tests.
    - The existing RV suites with the trim forced on every spec (83 tests, 35 RV specs:
      programs free + stress, pond accumulation, filter FIFO, capacity).
    - 16x16 thesis RV CGRAs, MEM and pond trimmed vs untrimmed (RTL lake origin/THESIS b59ea334
      + this change), kernel ns: conv_3_3 4150 / 4150, gaussian 6390 / 6390, harris_color 17838 / 17838,
      pond matmul_rvtry3 13355 / 13355; all bit-accurate, same balancing (driver: scratchpad
      cfc4174c `cmptrim/ab_chain.sh`).
  - gf12 A/B: garnet `sweep_specs.py --recipe thesis_cmptrim` (never run).
  - **Not adopted (user decision 2026-10-26, relayed by session bc48e85f):** the thesis RV fabric
    keeps the full comparators; the option stays here, off by default.
- **The filter path (`build_spec_rv(add_filter_path=True)`) is never used by compiled
  apps (checked 2026-10-11).** It is an extra in/out port pair with its own ID/AG/SG
  and a small local buffer (8–32 words). No clockwork code targets the collateral's
  "filter" controller (`in2filter`/`filter2out` appear nowhere in clockwork's history).
  Collateral from `spec_config_to_collateral` (what `gen_app_bundle.py` uses) is built
  from the static `build_spec`, so there clockwork never sees the filter ports;
  collateral from `build_spec_rv(...).extract_compiler_information()` counts them as
  ordinary ports (resnet's second writer lands there on 1-input specs and lake rejects
  the tile). MetaMapper's `cgralib.Mem` is 2 in / 2 out unless
  `METAMAPPER_MEM_PORTS=3`, so a design that wires MEM `data_in_2` / `data_out_2`
  needs that for `aha map`/`aha pnr`. On `fw4_dw16_sc8192_sp_in2_out2_vc2` the general
  path costs 8.1k µm² of the lake controller (30%, freepdk45) and 12.0k µm² / 1.27 mW of
  the MEM tile (21% / 28%, ungated). **conv_3_3 "eat rows up front"** (B): the newest
  tap goes through the filter path with an item filter skipping 2 rows, and the SRAM
  readers start at rows 0/1; bit-accurate on a 16×16 CGRA, saves 3.1% of PE firings and
  38% of SRAM reads (freepdk45 numbers: `/aha/sweep_out/fp_tradeoff_20261011/`, figure
  https://claude.ai/artifact/NRTbNTFFRQ8fDz6vEhjKhY). Its program is a pre-PnR hook
  (garnet `mflowgen/common/application/hooks/conv_3_3_eat_rows_up_front.py`) using the
  converter's opt-in `lake_constraints` key (`[port, level, port, level, "LT"|"GT",
  scalar]` as the network takes them; the dep_values encodings cannot express a reader
  whose data row leads its iteration row) and SpecSim's `bcast`/`lockstep` options.
  **Pitfall found with it:** a zero-lag column-level RAW (`[r, 0, w, 0, LT, 2·fw·vc+4]`)
  has a one-row wrap fixup, so when the writer can run 3 rows ahead of the reader it
  blocks in the writer's early columns; with the newest tap through a buffer shallower
  than ~2·fw·vc+4 words (an 8-deep FIFO, under stalls) that deadlocks. Use the row level
  (`[r, 1, w, 1, LT, 0]`: row k written) when the reader trails by whole rows.
- **Filter path as a FIFO: `build_spec_rv(filter_fifo_depth=N)` (2026-10-13, opt-in;
  default None = the general buffer, byte-identical RTL + bitstreams).** User
  direction: keep the iteration domains, make the rest a FIFO. Each side keeps its ID
  (and the input its item filter); the AG is `LinearAddressGenerator` (address = FIFO
  pointer, +1 per step, wraps at the N-word memory, no config; `pointer` has one lap
  bit more) and the SG is `FIFOScheduleGenerator` (write side steps while not full,
  read side while not empty, from the two pointers: a fixed version of the
  comparator pair; config `enable` only). Spec keeps FIFO ports out of the RV
  network (`_fifo_ports`, `_fifo_pairs`), wires the pointers, renumbers constraints
  for the network (`_network_constraints`; identity without FIFOs) and rejects a
  constraint on a FIFO side. Storage: N words (general: 8 per fetch-width word).
  fw4 sc8192 in2 out2: config 1421 -> 1301 bits. Tests: `test_filter_fifo.py`
  (structure, renumbering, rejected constraint, RTL pass-through at depth 2/8, the
  conv_3_3 B program free + stressed). garnet: `--spec-set filter_fifo`
  (`..._ff{8,16,32}_rv`), A/B power driver `mflowgen/cgra_app_power.py` (garnet
  mflowgen/CLAUDE.md "Whole-CGRA power of app variants"). Generic Genus (freepdk45,
  hierarchy kept), fw4 sc8192 in2 out2 controller: no filter path 19.3k µm², FIFO-8
  23.1k, general 27.4k; the filter path 8.1k -> 3.8k µm² (storage 3.8k -> 1.0k,
  comparators 1.2k -> 0, AG+SG 0.46k -> 0.1k; its two IDs, 1.5k, are now the
  largest part). Reports: `/aha/sweep_out/fp_fifo_20261013/area/`.
  **On a 16×16 CGRA (`gen_app_bundle`, ff8, balanced, 2026-10-14):** A and B both run at
  one token per cycle and are bit-accurate (A 4,096 outputs in 4,097 cycles, 4,173 ns;
  B 3,968 in 3,969, 4,189 ns: both set by the 4,096-token input stream). B reads the
  SRAM in 1,984 cycles vs A's 3,204 (−38%; A's two readers issue 56% more fetches than
  the 2,048 their data needs). Unbalanced (2026-10-13, every FIFO depth 8/16/32) B ran
  11% longer (6,167 vs 5,533 ns): not in the MEM, whose readers were never starved, but
  downstream. A row tap forks into a direct branch (3 more multiply-add PEs to the
  output PE) and a 1-token stencil delay branch (1 PE). With RV's `MATCH_BRANCH_DELAY=0`
  nothing balances them, so the loop capped the rate at A 3/4, B 2/3 tokens/cycle
  (archipelago/CLAUDE.md "Dense RV throughput is set by reconvergent forks"). garnet
  `gen_app_bundle.py --rv-balance` latency-matches those joins (garnet mflowgen/CLAUDE.md
  "RV branch balancing"). The tile-level SpecSim (no fabric) runs B at full rate.
- **FIXED 2026-09-30 (`lake/spec/spec.py` converter, committed `6f73bc91`) — capacity.**
  (a) clockwork emits `null` WAR for line buffers and the converter used
  "writer ≤ 8 rows ahead"; on a small memory (512 words = 8 rows of 64) a
  stalled reader was lapped (RTL, fw1 dp 1 KB, stress: reads = expected + 512).
  Now `min(8, R − D − c)` rows, R = capacity // row pitch (addresses wrap),
  D = reader precursor row lag, c = 1 if it also lags by columns; `ValueError`
  if < 1 (row-granular constraints cannot hold it; static timing can).
  (b) explicit `[level, level, scalar]` (barrier / sweep) programs need the
  buffer resident: `ValueError` if addresses exceed the memory (resnet's
  900-element input / 784-element conv banks on 512-word specs; clockwork's
  lake_dp path does not bank for capacity). (c) a dependence between ports on
  DIFFERENT storages raises: on `build_spec_rv(in_ports=1)` the second input /
  output is the filter path (own 8-entry storage) and clockwork's collateral
  counts it as a port, so an accumulator's second writer (resnet conv banks:
  init + update) lands there. Bitstreams unchanged wherever 8 rows fit
  (garnet default: all 107 tiles byte-identical). Test:
  `test_convert_rv_capacity.py`.
  (d) 2026-09-30: (a) raised whenever R − D − c < 1, but that is only
  infeasible for the DEFAULT RAW encoding (the reader may lead the writer
  within one row, the writer may not enter the reader's row → deadlock). With
  a ROW-level RAW (writer row ≥ reader row − D + 2) and WAR scalar R − D − c
  (≤ 0: the writer trails the reader's row counter, whose data is D rows old)
  the window is non-empty iff R − c ≥ 3. `convert_app_json_to_config` now
  gives such readers the row-level RAW automatically (2-level domains only;
  a level above the rows would need the wrap fixup the other way round) and
  raises only if R − c < 3. Case: unsharp gray bank 5 (lag 6 rows + 1 col,
  pitch 68) on fw1 dp 1 KB → RTL PASS free + stress (all 10 unsharp tiles).
  No other tile changes (they either had R − D − c ≥ 1 or raised).
- **Static route (`_convert_clockwork_to_port_config`, 2026-09-30, committed `6f73bc91`)**:
  the SIPO / PISO address of a wide IN port is the agg2sram READ address
  (`read_data_*`; was the write side), of a wide OUT port the sram2tb WRITE
  address (`write_data_*`); falls back to the other side when missing. And
  `AddressGenerator.gen_bitstream` reduces offset and strides modulo
  2**addr_width (addresses wrap in the counter anyway; values that fit are
  unchanged) — clockwork's static circular line buffers emit strides like 16
  for a 4-bit SIPO address.
- **Unit tests (2026-09-30, `tests/spec_unit_tests/`, committed `6f73bc91`)**: xrun-based
  functional tests per unit with Python reference models, run with
  `cd tests/spec_unit_tests && python -m pytest -q . -p no:cacheprovider`
  (~4.5 min; skipped without xrun). `rtl_harness.py` = cycle-vector harness
  (kratos → SV tb → xrun); `spec_rtl.py` = whole-spec harness (generated tb,
  remote storage model, loopback PE, stress stalls). Files: arbiter, LF comp
  block, RV comparison network, memory interface decoder, spec RegFIFO,
  storage, iteration domain, address generator, schedule generators (static +
  RV), controller integration, pond RV accumulation (bit-identity with
  `get_vec_accum_pond` + RTL), spec RV programs (stream / row-lag / barrier /
  sweep on fw1 dp, fw2 sp, fw2 dp, fw4 sp, garnet default; free + stress).
  As of 2026-10-13: 342 pass + 5 xfail; `python -m pycodestyle lake/ tests/` (lake CI,
  setup.cfg) is clean with pycodestyle 2.12. GitHub "CI Test" (linux.yml, stock container)
  runs that pycodestyle step and `pytest -v tests/` FROM THE REPO ROOT: green since 5772c785
  (358 passed, 280 skipped; xrun tests skip there). New tests must collect from the root
  (`tests/spec_unit_tests/conftest.py` puts rtl_harness/spec_rtl on sys.path) and must not
  import aha (CI's aha is master). CI first checks out aha mek's pinned lassen in the
  container (`.github/scripts/aha_mek_deps.sh`, 2026-10-18): `lake/modules/onyx_pe_intf.py`
  imports lassen thesis's `gen_Inst_fc`, which master's lassen lacks (d37dee6c's
  test_hybrid_policy.py failed to collect without it). Since 2026-10-11,
  `test_extract_compiler_information.py` passes too (59). Its 13–16 failures were stale
  expectations, not extraction bugs; see "Compiler collateral facts" below. Remaining
  xfails = latent bugs on paths no builder uses:
  MemoryInterfaceDecoder with >1 memory port cannot be built (and its enable
  concat order looks reversed); SingleBankStorage with two write ports cannot
  be emitted (non-remote pond); LFCompBlock 17-bit sum overflow for counters
  > 32768; LFCompBlock hardened-op constructor AttributeError. To test a
  private copy of the `lake/` package: `PYTHONPATH=<copy>` overrides the
  editable install, also under pytest (verified 2026-10-08); check with
  `python -c 'import lake; print(lake.__file__)'`, and don't run a script that
  sits next to another `lake/` directory (sys.path[0] wins). Pre-existing, unrelated:
  `tests/test_spec/dual_port_rv.py` legacy flow fails "Not enough data on port r0"
  (same before today's fixes).
- **Compiler collateral facts** (`Spec.extract_compiler_information`; checked 2026-10-11
  against clockwork `options.h` `LakeCollateral` and the fw1 dual-port collateral that
  passed on the CGRA):
  - `capacity` is in addresses: storage bytes / (word_width words × data bytes). The
    `_extract_capacity` helper alone returns bytes. The default MEM spec (4096 B, fw4)
    gives sram 512, agg 8, tb 8, which is clockwork's built-in `"mem"` collateral.
  - Wide-fetch specs with one primary storage get `agg`/`tb` synthesized
    (`_synthesize_wide_fetch_hierarchy`): `in2agg_N` / `tb2out_N`, agg→sram→tb
    `read_port`/`write_port`, and load/store latency 0. fw1 specs instead rename the
    `sram` keys to `mem` and set `controller_name` to `["regfile"]`.
  - `dual_port_sram` is set only for the storage named `sram`. All real callers use
    extraction's own naming (single storage → `sram`); an explicit `{stg: "mem"}` map
    hides it. A dual-port SRAM is `bank_num` 1.
  - `lake/utils/generate_collateral.py` (a reference agg/sram/tb spec; nothing calls it)
    matches clockwork's `"mem"` in capacity, word width and bank count. It lacks the
    agg output / tb input widths, because its agg and tb have external-side memory
    ports only.
- **OPEN** (OPEN_ISSUES `lake/spec-memcore-chaining-rom`; not rechecked since 2026-09-30): MEM-tile output CHAINING (`chain_en`) has no lake-spec equivalent.
  The spec MemCore also cannot be loaded with ROM contents (`init`): apps
  whose clockwork mapping uses ROM "port_controller_Counter" tiles (matmul's
  `kernel = 2x + y`) cannot run on lake-spec MemCores (static or RV).
- **Hand-written RV configs (`spec/hack_rv_mem_pond_bitstream.py`) = master's module as of
  b9c7374b^ (ported 2026-10-13, session 352df910; THESIS had the 2025-05-13 version).**
  Halide-to-Hardware's `hw_support/hack_design_top.py` rewrites ~34 apps after clockwork
  (inserts filter MEMs, accumulation ponds and path-balancing ponds with empty configs). For
  an app in `APPS_NEEDING_HACKS` (`TEST_NAME_FOR_HACKING_CHECK`, set by garnet.py) lake
  configures EVERY MEM/pond by (app, tile name) and ignores clockwork's program; tiles with
  `rom_` in the name keep theirs. Path-balancing ponds read `bin/path_balancing.json` from
  the /aha app dir (hard-coded path). The tile name is garnet's instance name:
  core_combiner_core -> `MemoryTileBuilder.get_bitstream(node_name=)` (forwarded to the
  `lakespec` controller only) -> `SpecMemoryController.get_bitstream` -> `Spec.gen_bitstream`
  -> `hack_rv_config` (no name -> ValueError for multi-tile apps). Master's later
  `lake_rv_config` metadata path (b9c7374b, strait templates) is not ported: mek's
  Halide-to-Hardware writes none.
  - THESIS-only: scalar_reduction/max/avg_fp read the sum out twice (`num_output_pixels=2`).
    H2H sets their output IO extent to 2 ("the f2g interrupt does not fire with 1"). Master
    wrote the final read's extent 1 unchecked as -1, so it did not stop after one value;
    THESIS encodes extent 1 exactly (0-D, next bullet), so one value came out and the store
    DMA hung (STRM_F2G timeout).
  - Verified 2026-10-13 on Zircon pr_aha1 (28x16, 12 cols removed; behavioral matrix unit,
    since the MatrixUnit RTL is not on /aha), map + PnR + `aha test --dense-fp`:
    scalar_reduction/max/avg_fp, vector_reduction_fp and stable_softmax_pass2_fp pass;
    layer_norm_pass2_fp passes the regression check (0.86% of outputs outside the per-value
    tolerance, 6% allowed). Needs process.cpp's preset input `.raw` files (aha map makes only
    missing ones since 2026-10-13). The other hacked apps in mek's regression (layer_norm
    pass1/3, softmax pass1/3, gelu, mat_vec_mul, tanh, ...) now get master's configs too;
    not run. Tests: `test_hack_rv_node_name.py`, `test_scalar_accum_pond_rtl` (SpecSim
    `init_zero=True` = the CGRA tb's `-initmem0 -initreg0`; this pond has no clear port).
    Drivers: scratchpad 352df910 `fpc/{compile_fp2.sh,test_fp.sh}`.
- **RV pond extent + extent-1 levels (2026-10-11).** (a) `IterationDomain.gen_bitstream`
  drops OUTERMOST extent-1 levels (index always 0 = where a dropped level's counter
  stays, so AG / comparisons / wrap fixup are unchanged); an all-ones domain is 0-D,
  which runs once (no level is ever "not at max"). Inner extent-1 levels still raise.
  Needed by one-output reductions (`get_accum_pond(num_output_pixels=1)`, final read
  `[1]`). (b) `build_pond_rv(max_extent=)` + pond-spec key `max_extent`; garnet's RV
  pond (`resolve_cgra_pond_params(rv=True)`) defaults to 4096 = 12-bit iteration
  domains (layer_norm_pass2_fp reduces 3072; the old fixed 11 bits stop at 2048).
  `build_pond_rv` called directly keeps 11 bits. Verified: unit suite 324 + 5 xfail;
  RTL one-pixel accumulation + 3072-step reduction on the default pond (free +
  stress); 12 clockwork accumulation programs on a 12-bit 4-D pond cycle-identical
  to 11-bit; Zircon pr_aha1 map + PnR: scalar_reduction / scalar_max / scalar_avg /
  stable_softmax_pass2 compile (scratchpad 352df910 `fpc/`).
- **RESOLVED (2026-10-01) — harris_color end to end** (the STRM_G2F hang above):
  not the MEM tiles. Causes were clockwork stream framing (`rv_reframe_line_buffers`),
  canal register pre-fill landing on the wrong-width register, and the PE-FIFO
  precursor walk double-counting what this converter now applies (all fixed:
  clockwork 440488d4, canal c3894d2, garnet 3c70c41f). harris/unsharp/camera RV
  pass bit-accurate; details in clockwork/CLAUDE.md "RV stream framing bug".

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
