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

---

## 2. Running the physical-design flow

- **How to build one synth config end-to-end (mflowgen, Genus, Innovus,
  PT):** [`THESIS/REPLICATION.md`](THESIS/REPLICATION.md) — verified
  through post-PnR power on 2026-04-26, includes step numbers,
  runtimes, and gotchas.
- **mflowgen design definition:** [`pd/thesis/README.md`](pd/thesis/README.md)
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
- [`THESIS_ROUNDTRIP_PROGRESS.md`](THESIS_ROUNDTRIP_PROGRESS.md) —
  status of the Lake→Clockwork→Lake spec round-trip smoke tests as of
  2026-05-08 (which sweep configs have validated end-to-end).
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
