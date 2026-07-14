# App-mapping + PPA-collection harness (Ch. 5)

Skeleton for the harness that will flip every `single_level_*` /
`two_level_*` entry in `THESIS/pipeline/registry.py` from `bogus` →
`real`. Scoped in the "app-mapping harness scoping" research pass;
see `../PIPELINE.md` for the pipeline docs it plugs into.

**Status:** wiring skeleton only — no cells have been run. The plumbing
below is what the wiring plan concluded needs to exist.

---

## 1. Modules

| File | Purpose | Status |
| --- | --- | --- |
| `registry.py`     | `AppSpec` per app × schedule variant (6 apps from `tab:exploration_applications`) | skeleton; `??` markers where Halide dirs need verifying |
| `design_points.py`| `DesignPoint` per single-level config | empty list — populate from `THESIS_BUILDS/*/thesis_sweep_700/` |
| `run_matrix.py`   | CLI that iterates designs × apps and dispatches the round-trip flow per cell | skeleton; `run_one_cell` raises NotImplementedError |
| `compose.py`      | Turns per-memtile PPA into per-app CGRA-level PPA (weighted by tile counts + 3:1 PE:Mem) | skeleton with placeholder PE-tile PPA |

Result files land at `THESIS/data/apps/<design_id>/<app_id>/results.json`
with a stable schema (see `run_matrix.py` docstring). Downstream
generators in `THESIS/pipeline/generators.py` will read that tree.

---

## 2. Milestone 1 (~1 week) — "6 apps × 8 validated designs, single-level static"

Flips 6 registry entries (`single_level_area/power/performance/utilization/energy_efficiency` + `single_level_performance_LI` if quick) from `bogus` → `real`.

1. **Populate `registry.APPS`** — verify each `halide_app_dir` /
   `testname` against `/aha/Halide-to-Hardware/apps/hardware_benchmarks/apps/*`.
2. **Populate `design_points.DESIGNS_SINGLE_LEVEL`** — 8 configs from
   `THESIS_ROUNDTRIP_PROGRESS.md` table (already round-trip-validated on
   conv_3_3).
3. **Parameterize the round-trip flow per app.** Two file diffs:
   - `pd/thesis/clockwork-roundtrip-compile/configure.yml` — add
     `app_dir` + `testname` step params (currently hardcoded to
     `conv_3_3`, line 8 per scoping report).
   - `ASPLOS_EXP/run_roundtrip_sweep.sh` — accept `--app-dir` +
     `--testname` per-line in the config file (currently hardcoded at
     line 51).
4. **Add utilization counters to `pd/thesis/synopsys-vcs-sim-rtl/tb.sv`.**
   Two counters (`active_cycles`, `total_cycles`) + a `$fdisplay` block
   that writes `outputs/util.txt` = `<active> <total>` at sim end.
   Update `pd/thesis/clockwork-roundtrip-common/run_roundtrip_sim.py`
   to slurp it into `roundtrip_results.json`.
5. **Implement `run_matrix.run_one_cell`.** Shell out to the round-trip
   flow, parse `roundtrip_results.json` + `outputs/util.txt` + the
   `extract_power_area` DataFrame row for the design's build dir.
6. **Add 6 generators to `THESIS/pipeline/generators.py`.** Each reads
   `THESIS/data/apps/**/results.json`, groups by design, plots per-app.
   Flip the 6 `single_level_*` entries in `registry.py` from `"bogus"`
   to `"real"`.

At the end of milestone 1, running
`python3 THESIS/generate_thesis_artifacts.py` should show `real` count
jump from 10 → 16.

---

## 3. Later milestones (in dependency order)

- **Milestone 2 — Aggressive/conservative schedule split.** Adds the
  second column set in `tab:ul_perf` + doubles the app matrix.
- **Milestone 3 — LI variants.** Rerun everything with `Spec(opt_rv=True)`
  (already supported in `tests/test_spec/thesis_sweep.py:36`) and a new
  LI round-trip config. Flips `single_level_*_LI` and eventually the
  LI ScheduleGenerator characterization figures.
- **Milestone 4 — Two-level (Pond).** Requires standing up a
  `pond_sweep.py` sibling of `thesis_sweep.py` + a
  `pd/thesis/pond/` mflowgen construct (or a `spec_role={memtile,petile}`
  parameter reusing the existing construct). Then rerun the app matrix
  with two-tier PPA composition. Flips all `two_level_*` figures.
- **Milestone 5 — PE-tile PPA + interconnect share.** Replace the
  placeholder constants in `compose.py` with real numbers from a
  `pd/thesis/pe-tile/` build. Also unlocks
  `fig:interconnect_fork_ready_valid_data`.

---

## 4. Open questions (call out in weekly sync)

- Aggressive vs conservative Halide schedules: is this a separate
  testname per app or a compile-time flag? Answer decides whether
  `registry.APPS` doubles or `run_one_cell` accepts a variant arg.
- PE-tile PPA source: stand up a new mflowgen construct, or pull from
  the upstream AHA CGRA build (and if so, at what commit)?
- Interconnect PPA share: use the SB-alone numbers from Ch. 6, a
  fraction of the AHA CGRA build, or leave as an explicit "logic
  overhead" line item in the exploration figures?
