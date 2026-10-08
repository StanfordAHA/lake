# `tests/thesis/` — thesis pipeline tests

Unit tests for the thesis tooling. This covers the figure/table pipeline in
`THESIS/`, the Ch. 5 app-mapping harness and its hooks in `ASPLOS_EXP/` and
`pd/thesis/`. They don't need EDA tools, `THESIS_BUILDS` or magma, because
each test builds its own small fixture tree under `tmp_path`.

Run from the repo root:

```bash
python3 -m pytest --confcutdir=tests/thesis tests/thesis/
```

`--confcutdir` is required. Without it pytest loads the repo-root
`conftest.py`, which imports `magma`/`kratos` when it collects tests.

| File | Covers |
| --- | --- |
| `test_tables.py` | The 5 LaTeX table emitters in `THESIS/pipeline/tables.py`. |
| `test_apps_query.py` | `THESIS/pipeline/apps_query.py`: loading `THESIS/data/apps/**/results.json` into a DataFrame. |
| `test_generators_single_level.py` | The 5 `single_level_*` generators: real plots from fixture results, `MissingDataError` when data is missing. |
| `test_run_matrix.py` | `THESIS/apps/run_matrix.py`: config-line composition, dry-run, parsing summary + util files, PPA lookup, graceful failure. |
| `test_run_roundtrip_sweep_sh.py` | `ASPLOS_EXP/run_roundtrip_sweep.sh`: `--app-dir`/`--testname` flags and per-line overrides (runs only the argument-parsing part of the script). |
| `test_roundtrip_sim.py` | `parse_util_txt` in `pd/thesis/clockwork-roundtrip-common/run_roundtrip_sim.py`. |

The regression module (`THESIS/pipeline/regression.py`) and the sweep
figure generators have no unit tests. Check them by running
`python3 THESIS/generate_thesis_artifacts.py` against real builds and
comparing the outputs.
