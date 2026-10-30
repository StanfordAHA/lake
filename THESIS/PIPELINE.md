# Thesis-artifact generation pipeline

Source-of-truth doc for how figures and tables in `main_thesis.tex` are
produced. Adding a new figure? Read §2. Wondering why a figure has a red
BOGUS watermark? Read §3.

---

## 1. Layout

```
THESIS/
├── generate_thesis_artifacts.py   # top-level CLI orchestrator
├── PIPELINE.md                    # (this doc)
├── REPLICATION.md                 # how to run the mflowgen synth flow itself
├── pipeline/
│   ├── registry.py                # THE registry — one Entry per fig/table
│   ├── generators.py              # real generators (call these from registry)
│   ├── build_query.py             # loads THESIS_BUILDS via ASPLOS_EXP/extract_power_area.py
│   ├── ingest.py                  # symlinks source builds into data/<fig_id>/
│   ├── bogus.py                   # red-watermark placeholder emitter
│   └── errors.py                  # MissingDataError
├── data/
│   ├── _cache/                    # cached builds CSV (per --top)
│   └── <fig_id>/builds -> …       # symlink into THESIS_BUILDS/<EXP>
└── output/
    ├── figures/…                  # PDF artifacts (paths match \includegraphics)
    ├── tables/…                   # .tex snippets (for \input{…})
    └── logs/
        ├── manual.log             # entries that need hand-authored art
        ├── missing.log            # every BOGUS emission with its reason
        └── errors.log             # unexpected generator crashes
```

Output paths in the registry match the LaTeX `\includegraphics{figures/…}`
paths exactly. Two ways to wire the tex build to consume them:

- **Option A (recommended):** add
  `\graphicspath{{THESIS/output/}{./}}` to the preamble of
  `main_thesis.tex`. Zero filesystem changes; the tex compile finds
  `figures/foo.pdf` under `THESIS/output/`.
- **Option B:** symlink `figures/ -> THESIS/output/figures/` at repo
  root. Slightly cleaner for the tex file but adds an untracked symlink.

---

## 2. Adding a new figure or table

1. **If it's derived from data:** write a generator function in
   `pipeline/generators.py`. Signature:
   ```python
   def my_figure(ctx: GenContext, outpath: Path) -> None:
       edf = _slice(ctx.df, "MY_EXP", "synth_total_area_um2")
       _sweep_lineplot(edf, x="my_param", y="synth_total_area_um2",
                       hue_cols=["other"],
                       title="…", xlabel="…", ylabel="…",
                       outpath=outpath)
   ```
   Raise `MissingDataError("…")` when a required column/build isn't
   there — the orchestrator swaps in a BOGUS placeholder instead of
   crashing.

2. **Append an `Entry` in `pipeline/registry.py`:**
   ```python
   Entry("my_figure_id",
         "fig:my_thesis_label",              # matches \label{…} in tex
         "figures/my_figure_id.pdf",         # matches \includegraphics{…}
         "figure",                           # or "table"
         "one-line description of source data",
         "real",                             # real | bogus | manual
         g.my_figure,                        # None for bogus/manual
         notes="TODO: any known gaps")
   ```

3. **Run** `python3 THESIS/generate_thesis_artifacts.py --only my_figure_id`.

### Statuses

| Status | Meaning | Orchestrator action |
| --- | --- | --- |
| `real` | Backed by a generator that reads THESIS_BUILDS or another CSV. | Call generator; on `MissingDataError` fall back to BOGUS + log. |
| `bogus` | Explicitly a placeholder — data doesn't exist yet. | Always emit BOGUS + log to `missing.log`. |
| `manual` | Hand-authored art or a curated table in the tex source. | Skip; log once to `manual.log` for visibility. |

---

## 3. BOGUS fallback

Any figure that couldn't be produced from real data gets a well-formatted
matplotlib line chart with a giant red diagonal `BOGUS` watermark
across it and an italicized footnote `placeholder — <reason>`. The tex
build still compiles; the visual is unmistakably a placeholder.

Every BOGUS emission appends one line to `output/logs/missing.log` with
the entry ID and reason. Sort that log by ID to see the punch list.

Tables use a red-boxed `\fbox{…BOGUS PLACEHOLDER…}` snippet — safe to
`\input{}` without a graphics driver.

---

## 4. Data sources currently wired

- **garnet Tile_MemCore sweep** (not THESIS_BUILDS; ingest with
  `python3 -m THESIS.pipeline.tile_sweep <sweep.zip>`), all in
  `pipeline/tile_figures.py`, 16-bit-data configs only (other widths have
  broken RTL as of 2026-10-06). From the hierarchy-kept dw16 sweep
  (2026-10-09) each figure plots its own blocks (`blk_*` area, `pw_synth_*`
  power; CLAUDE.md §1.8):
  - the Ch. 4 component **area and power** figures (same output paths as the
    old THESIS_BUILDS versions): port_{area,power}_vs_{data_width,vc},
    iter_dom_*, affine_*_vs_{dim,max_value}, memory_port_*, storage_*. Power =
    active (solid) + idle (dashed). The data_width pair stays BOGUS until
    non-16-bit builds work.
  - PUB (controller + SRAM): pub_capacity_{area,power}, pub_bandwidth_{area,power}.
  - memory tile: memtile_interconnect_{area,power}, memtile_block_breakdown,
    memtile_rv_overhead_area, memtile_rv_energy_breakdown, and the synth->PnR model
    memtile_synth_vs_pnr_{area,power} (8 fit builds as dots, 4 held out as
    stars, per-mode fits + error panel). memtile_{capacity,bandwidth,
    port_buffer,control}_area are generated but superseded (not placed).
  - LI ScheduleGenerator (RV builds): li_sched_{ports,loc}_{area,power}.
  - `tile_figures.SRAM_NDA` (default True): values that include the SRAM
    macros (Storage, PUB, model) are plotted relative to a reference config.
  - **Views** (2026-10-13): every sweep figure above except the
    memtile_{block,rv_*,synth_vs_pnr}* ones and the superseded four is
    `@_viewable` and draws its registry `view`: "raw" values, "pct" = %
    change from each x value to the next (drawn mid-step, with x's own change
    as a gray "y ∝ x" reference: above it = faster than proportional), or
    "both" (raw on top, % panel below). All are "both" for now; pick per
    figure in `registry.py`. `--view {raw,pct,both}` overrides for a run;
    `--preview-views DIR` writes every selected sweep figure in all three
    views + a side-by-side `index.html` without touching `output/`.
  Placement and captions for the tex: `THESIS/output/snippets/memtile_figures.tex`.
- **Halide-to-Hardware + aha** → `halide_appendix` (kind `listing`,
  `output/listings/halide_appendix.tex`): the appendix's Halide code, one
  lstlisting per app, from the H2H commit aha pins, at the app dirs and
  generator args the garnet app bundles record (`apps/registry.py`'s
  `bundle_env`, else aha's `application_parameters.json`), keeping only the
  Clockwork schedule those args select (`pipeline/halide_appendix.py`). Checkouts default to
  `../aha` and `../Halide-to-Hardware`; override with `AHA_DIR` / `H2H_DIR`.
  The snippet is pasted inline into the appendix of main.tex (Overleaf).
- **THESIS_BUILDS** (standalone lakespec) → the `tab:ul_ppa_summary` table.
  (`tab:memtile_model_*` fit the tile sweep since 2026-10-24: §7.)
- **PE tile pitch** → `fig:pe_tile_height` (`tile_figures.pe_tile_pitch`): each
  fabric's PE and Memory Tile PnR area from `data/pe_tile/thesis_tiles.csv`
  (copied by hand from garnet mflowgen/CLAUDE.md, the 2026-10-22 thesis tile set at
  400 MHz; refresh from the newest `thesis_tiles` run). A Memory Tile spans the
  fewest unit heights k with width area/k <= `MEM_MAX_WIDTH` (1.5) PE tiles. The standalone area/power generators remain in
  `generators.py` but are no longer registered for the component figures.

Everything else in `main_thesis.tex` is either BOGUS (data pending) or
manual (block diagram / curated table). Full breakdown: run
`python3 THESIS/generate_thesis_artifacts.py --list`.

---

## 5. Known TODOs (data that doesn't exist yet)

| Blocker | What it unblocks | Status |
| --- | --- | --- |
| Power flow (`ptpx-synth`) hasn't been run for the sweep experiments | Every `*_power` characterization figure + memtile power fit | open |
| No dedicated per-stride / per-offset AFFINE sweep | `affine_{area,power}_vs_{stride,offset}` | **dropped** (2026-10-17): not separate hardware parameters (SG: `max_sequence_width`, AG: address width); figures removed from the tex and the registry |
| No dedicated ScheduleGenerator sweep | `sched_gen_{dim,max_extent}` | open |
| No LI ScheduleGenerator sweep in THESIS_BUILDS | `li_sched_{ports,loc}_{area,power}` | open |
| No app-mapping harness / results | Every `single_level_*`, `two_level_*`, `tab:ul_perf`, `tab:ul_ppa_summary` | **ingest written (2026-10-25), no data yet**: garnet `cgra_app_power.py` → `THESIS.apps.cgra_power_ingest` → `results.json`; `single_level_*` and `single_level_*_LI` are real but draw BOGUS until thesis-fabric bundles for the design points are recorded and powered. Commands: `THESIS/apps/CGRA_APP_POWER.md` |
| No memtile regression fit | `tab:memtile_model_{coeff,verif}` | **DONE** — tile-sweep model (2026-10-24, §7); refit on the thesis-fabric design-space sweep once ingested |
| No pre/post SB PPA numbers | `fig:interconnect_fork_ready_valid_data`, `fig:interconnect_fifo_replace_ff` | **build command exists, not run** (2026-10-17): garnet `sweep_specs.py --recipe fabric_mem` + `pe_tile` (static vs ready-valid fabric, hierarchy kept; garnet mflowgen/CLAUDE.md "Thesis data recipe"). No lake ingest for those zips yet |

Each TODO is also captured in the corresponding `Entry.notes` in
`registry.py` so `grep -n "TODO:" pipeline/registry.py` gives you the
same list from the source of truth.

---

## 7. Memtile regression module

`pipeline/regression.py` fits the two `tab:memtile_model_*` tables on the tile
sweep (`tile_sweep.load()`, i.e. `data/tile_sweep/latest.csv`; since 2026-10-24,
before that on THESIS_BUILDS):

- **Targets.** Non-SRAM cost per config: Genus std-cell area (`syn_std`) and
  PrimeTime synth-level idle / active power minus the SRAM macros' (`pw_synth_*`).
  The macro is a lookup by geometry and its numbers are under NDA, so it is
  left out (Kahng-hybrid split).
- **Features** (`TILE_FEATURES`): dims, log2 fetch width, vec_capacity, ports
  (in + out: they move together in the sweeps), dual-port, log2 max extent
  (default 2^11), log2 max sequence width (default 2^16), log2 capacity, log2
  data width. A feature the sweep does not vary gets no coefficient ("--").
- **Fit.** One `LassoCV` per runtime mode and target (static/RV x area/idle/active).
- **Verification** (`TILE_HOLDOUTS`): refit without, then predict: the sweep's
  held-out PnR builds (`pnr_role == validate`) and three extrapolations
  (32 KB capacity, 4 in / 4 out, max extent 4096). Mean / max |error| per mode.
- The old THESIS_BUILDS functions (`fit_area_logic`, LOEO, delay) are still in the
  module, unregistered.

Regenerate the two tables in isolation:

```bash
python3 THESIS/generate_thesis_artifacts.py --only memtile_model_coeff memtile_model_verif
```

Output lands at `THESIS/output/tables/memtile_model_{coeff,verif}.tex`
and is a `\begin{tabular}` snippet suitable for `\input{}` from the tex.
Partial fits (power missing) prefix a `% partial fit — skipped: ...`
LaTeX comment so it's obvious in-file.

---

## 8. App-mapping harness

Peer directory `THESIS/apps/` — scope + milestone plan in
[`THESIS/apps/README.md`](apps/README.md), which also has the run commands.
Two writers fill `THESIS/data/apps/<cell>/<app_id>/results.json`:
- `python3 -m THESIS.apps.cgra_power_ingest` (2026-10-25): garnet's whole-CGRA app power
  (`cgra_app_power.py`), one cell per (design, mode, fabric, pond). This is the path the
  thesis uses. Commands and field meanings: [`THESIS/apps/CGRA_APP_POWER.md`](apps/CGRA_APP_POWER.md).
- `python3 -m THESIS.apps.run_matrix`: the older one-memory-tile round trip, never run
  (schema in the `run_matrix.py` docstring).

`pipeline/apps_query.py` loads the tree into a DataFrame, keeping only
`sim_status == "PASS"` rows by default; `select()` / its keyword filters pick one
`mode` / `fabric` / `pond_id`. The 5 `single_level_*` generators plot static
thesis-fabric rows, the 5 `single_level_*_LI` the RV ones (`generators.APP_FABRIC`),
and raise `MissingDataError` (drawing BOGUS) while there are none.
`two_level_*` are still `bogus`.

`THESIS/apps/design_points.py` is the source of truth for the
single-level design axis; `tab:ul_ppa_summary` and `tab:ul_design_points`
render straight from it (via `pipeline/tables.py`).

---

## 9. Table renderers

`pipeline/tables.py` holds LaTeX-tabular renderers for the non-figure
artifacts (invoked by generators in `pipeline/generators.py`). Two
tiers:

- **Fully data-driven** (populate now from existing state):
  - `emit_ul_ppa_summary` — per-`DesignPoint` PPA rollup. Area+timing
    live; power blank until the ptpx sweep runs. Missing DataFrame
    rows are called out in a `%`-comment header.
  - `emit_ul_design_points` — DesignPoint axis enumeration with a
    `roundtrip_validated` column.
  - `emit_exploration_applications` — renders `THESIS/apps/registry.APPS`
    as a table. Keeps app registry and thesis table in sync
    automatically.

- **Structural skeletons** (real class/method names + `% TODO: prose`
  markers so the .tex is editable in-place):
  - `emit_lake_interfaces` — scrapes `class __init__` signatures from
    `lake/spec/*.py`.
  - `emit_compiler_info` — same Component list; per-column prose is
    hand-authored.

All five write to `THESIS/output/tables/<name>.tex` and are meant to be
`\input{}`-ed from `main_thesis.tex`.

---

## 6. Related docs

- `THESIS/REPLICATION.md` — how to run the mflowgen synth flow that
  produces the build dirs this pipeline reads.
- `ASPLOS_EXP/extract_power_area.py` — the extractor this pipeline
  wraps.
- `ASPLOS_EXP/plot_power_area.py` — standalone plotter for ad-hoc
  exploration; the thesis pipeline reuses its styling conventions but
  not its code.
