# `ASPLOS_EXP/` — sweep drivers + data extraction

Scripts that generate the thesis memtile sweeps, run them through
mflowgen, and turn the resulting build directories into data the thesis
pipeline (`THESIS/`) consumes. Run everything from the repo root.

The end-to-end flow:

```
all_experiments_thesis_v2.sh ─► create_mflowgen_experiments.py ─► THESIS_BUILDS/<EXP>/thesis_sweep_700/<config>/
                                                                          │
                          run_synth_pool.py (Genus, in parallel) ◄────────┤
                          run_power_flow.sh (VCD → SAIF → PrimeTime) ◄────┤
                          run_roundtrip_sweep.sh (lake→clockwork→lake) ◄──┘
                                                                          │
extract_power_area.py ─► CSV ─► THESIS/generate_thesis_artifacts.py (figures + tables)
                              └► plot_power_area.py / scaling_stats.py (ad-hoc)
```

## 1. Generating sweep builds

| Script | What it does |
| --- | --- |
| `all_experiments_thesis_v2.sh` | **Current** sweep definition (Ch. 7.2.1): one `create_mflowgen_experiments.py` call per PORT / ITERATION_DOMAIN / AFFINE_PATTERN_GENERATOR / MEMORY config, writing to `/sim/mstrange/THESIS_BUILDS_V2/<EXP>`. |
| `all_experiments_thesis.sh` | Same sweep, targeting the original `/sim/mstrange/THESIS_BUILDS` (the builds pulled locally to `~/THESIS_BUILDS`). |
| `create_mflowgen_experiments.py` | Creates one mflowgen build dir per config (`--physical --run_builds` to also build). Uses `pd/thesis/construct-commercial-full.py`. |
| `create_all_experiments.py` | Lake spec generation for one config (RTL + collateral into `TEST/`). Invoked by the mflowgen `rtl` step — see `pd/thesis/README.md`. |
| `run_synth_pool.py` | Runs Genus synthesis across every discovered config in parallel. Discovers step numbers from `make list`. `--phase discover --dry-run` to preview; `--filter <regex> --limit 1` for a pilot. |
| `smoke_test.sh` | 4-config smoke test (known-passing + gen_sram regression targets). Default build root `/tmp/SMOKE_BUILDS`. |

## 2. Power and round-trip validation

| Script | What it does |
| --- | --- |
| `run_power_flow.sh` | Power flow on one build dir. `synth`: RTL sim with VCD → vcd2saif → `ptpx-synth`. `pnr`: full PnR → GLS → `ptpx-gl`. `both`. Needed before any `*_power` thesis figure becomes real. |
| `run_roundtrip_sweep.sh` | Round-trips a list of configs lake → clockwork → lake and simulates each tile. See below. |

`run_roundtrip_sweep.sh` usage:

```bash
ASPLOS_EXP/run_roundtrip_sweep.sh [--app-dir DIR] [--testname NAME] <config_file> [<output_root>]
```

Each config-file line is `name|sweep-args|spec-factory-kwargs[|app_dir|testname]`.
The app defaults to `conv_3_3`. `--app-dir`/`--testname` change the default
for every line, and the optional 4th/5th fields override it per line. It
writes `summary.txt` plus, per tile, `outputs/util.txt` (`<active> <total>`
handshake cycles from `pd/thesis/synopsys-vcs-sim-rtl/tb.sv`). The Ch. 5
app matrix (`THESIS/apps/run_matrix.py`) builds these config lines and
calls this script, one cell at a time.

## 3. Extracting data

| Script | What it does |
| --- | --- |
| `extract_power_area.py` | Walks a `THESIS_BUILDS`-style tree → one CSV row per build: synth/PnR area (total, cell, SRAM storage), synth/PnR power, critical-path delay + endpoints. **This is the thesis pipeline's data source.** `python3 ASPLOS_EXP/extract_power_area.py ~/THESIS_BUILDS -o out.csv` |
| `plot_power_area.py` | Standalone plots from that CSV (auto-detects the swept parameter per experiment). Output in `figs/`. The thesis pipeline copies its styling but not its code. |
| `scaling_stats.py` | Per-(experiment, parameter) linear fits and derived ratios; markdown table to stdout, CSVs in `figs/`. |
| `collect_power_area.sh` | Older shell summariser: markdown table of synth + post-PnR power/area for the listed build dirs. |

## 4. Legacy (pre-thesis ASPLOS work)

`all_experiments.sh`, `push_cache_model*.py`, `core_combiner_with_rv.py`,
`test_ast_eval.py`, `get_ag_path.tcl` and `signoff.area.rpt` belong to the
2024 ASPLOS experiments. The thesis flow does not use them.

## Tests

`tests/thesis/test_run_roundtrip_sweep_sh.py` covers the argument parsing and
per-line overrides in `run_roundtrip_sweep.sh`. See `tests/thesis/README.md`.
