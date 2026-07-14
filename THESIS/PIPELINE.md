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

- **PORT_EXP** → port_area_vs_{data_width,vc}
- **ITERATION_DOMAIN_EXP** → iter_dom_area_vs_{dim,max_extent}
- **AFFINE_PATTERN_GENERATOR_EXP** → affine_area_vs_{dim,max_value}
- **MEMORY_EXP** → memory_port_area_vs_interface_width,
  storage_area_vs_capacity

Everything else in `main_thesis.tex` is either BOGUS (data pending) or
manual (block diagram / curated table). Full breakdown: run
`python3 THESIS/generate_thesis_artifacts.py --list`.

---

## 5. Known TODOs (data that doesn't exist yet)

| Blocker | What it unblocks |
| --- | --- |
| Power flow (`ptpx-synth`) hasn't been run for the sweep experiments | Every `*_power` characterization figure |
| No dedicated per-stride / per-offset AFFINE sweep | `affine_{area,power}_vs_{stride,offset}` |
| No dedicated ScheduleGenerator sweep | `sched_gen_{dim,max_extent}` |
| No LI ScheduleGenerator sweep in THESIS_BUILDS | `li_sched_{ports,loc}_{area,power}` |
| No app-mapping harness / results | Every `single_level_*`, `two_level_*`, `tab:ul_perf`, `tab:ul_ppa_summary` |
| No memtile regression fit | `tab:memtile_model_{coeff,verif}` |
| No pre/post SB PPA numbers | `fig:interconnect_fork_ready_valid_data` |

Each TODO is also captured in the corresponding `Entry.notes` in
`registry.py` so `grep -n "TODO:" pipeline/registry.py` gives you the
same list from the source of truth.

---

## 6. Related docs

- `THESIS/REPLICATION.md` — how to run the mflowgen synth flow that
  produces the build dirs this pipeline reads.
- `ASPLOS_EXP/extract_power_area.py` — the extractor this pipeline
  wraps.
- `ASPLOS_EXP/plot_power_area.py` — standalone plotter for ad-hoc
  exploration; the thesis pipeline reuses its styling conventions but
  not its code.
