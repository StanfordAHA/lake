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

### 1.7 App-mapping harness (Ch. 5, skeleton)

The exploration figures (`single_level_*`, `two_level_*`,
`tab:ul_perf`, `tab:ul_ppa_summary`) will be fed by
`THESIS/apps/` — see [`THESIS/apps/README.md`](THESIS/apps/README.md)
for the milestone plan. Today it's **skeleton only**:

- `THESIS/apps/registry.py` — 6 `AppSpec` entries with `??` markers
  where Halide app dirs need verifying against the local
  Halide-to-Hardware checkout.
- `THESIS/apps/design_points.py` — empty `DESIGNS_SINGLE_LEVEL` list;
  populate from the 8 round-trip-validated configs in
  `THESIS_ROUNDTRIP_PROGRESS.md`.
- `THESIS/apps/run_matrix.py` — CLI shell; `run_one_cell` raises
  `NotImplementedError` until the mflowgen dispatch is parameterized
  per app.
- `THESIS/apps/compose.py` — CGRA-level PPA composer with placeholder
  PE-tile numbers.

Milestone 1 (~1 week of work) flips 6 registry entries `bogus` →
`real` — full plan in the README.

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
- **Extraction scripts consumed by the thesis pipeline:**
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
