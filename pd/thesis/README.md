# Lake Thesis Physical Design — `pd/thesis/`

This directory is the canonical mflowgen design definition for the Lake thesis sweep.
It contains `construct-commercial-full.py` and all custom mflowgen steps (rtl, constraints,
gen_sram_macro, cadence-innovus-*, synopsys-ptpx-*, etc.).

A single copy of this directory serves **all** sweep configurations. Per-config values are
injected entirely through `--graph-kwargs` at mflowgen graph-generation time — no copying,
no YAML overwriting.

---

## Parameter flow

```
create_mflowgen_experiments.py
  --graph-kwargs  {clock_period, storage_capacity, data_width, fetch_width,
                   dual_port, python_command, test_dir}
        │
        ▼
pd/thesis/construct-commercial-full.py  construct(**kwargs)
  • gen_sram.set_param(num_words, word_size, mux_size, num_subarrays)
  • rtl.set_param(python_command, test_dir)
        │
        ▼
mflowgen step parameters (written into build/.mflowgen/N-<step>/configure.yml)
exported as environment variables when step commands run
```

### Key parameters

| Parameter | Type | Description |
|---|---|---|
| `clock_period` | int (ps) | Target clock period in picoseconds |
| `storage_capacity` | int (bytes) | Total SRAM capacity |
| `data_width` | int (bits) | Data word width |
| `fetch_width` | int (words) | Words per SRAM port (determines SRAM aspect ratio) |
| `dual_port` | bool | Use dual-port SRAM macro |
| `python_command` | str | Full `python create_all_experiments.py ...` invocation for the RTL step |
| `test_dir` | str | Relative path `TEST/<design>/<config>/` where RTL outputs live |

---

## Running a single config manually

### 1. Generate the mflowgen build graph

```bash
cd /path/to/build/dir
mflowgen run \
  --design /home/mstrange/lake/pd/thesis \
  --graph-kwargs "{'clock_period': 700, 'storage_capacity': 8192, 'data_width': 16, \
                   'fetch_width': 4, 'dual_port': False, \
                   'python_command': 'python /home/mstrange/lake/ASPLOS_EXP/create_all_experiments.py \
                     --physical --storage_capacity 8192 --clock_count_width 64 \
                     --data_width 16 --dimensionality 6 --fetch_width 4 \
                     --outdir \$TOP/TEST/ --design_filter thesis_sweep', \
                   'test_dir': 'TEST/thesis_sweep/storage_cap_8192_data_width_16_ccw_64_dim_6_fw_4'}"
```

### 2. Find the synthesis step number

```bash
make list | grep cadence-genus-synthesis
```

### 3. Run up through synthesis

```bash
make <N>   # N = synthesis step number (usually 7)
```

Build outputs land in the build directory under `.mflowgen/N-<step>/`.

---

## Running the full thesis sweep

Use `run_synth_pool.py` to discover all configs from `all_experiments_thesis.sh` and run
them in parallel:

```bash
cd /home/mstrange/lake
python ASPLOS_EXP/run_synth_pool.py \
  --build-dir /sim/mstrange/THESIS_BUILDS \
  --jobs 8 \
  --phase both
```

Pool run status and logs are written to `ASPLOS_EXP/synth_pool_runs/<timestamp>/`.

Build outputs for each config land at:
```
/sim/mstrange/THESIS_BUILDS/<design>_<freq>/<config_folder>/
```
where `<config_folder>` is e.g. `storage_cap_8192_data_width_16_ccw_64_dim_6_fw_4`.

---

## What the `rtl` step does

The rtl step:
1. Runs `$python_command` — invokes `create_all_experiments.py` with the config's flags, which runs Lake spec generation and writes RTL + collateral into `TEST/<design>/<config>/`.
2. Copies outputs from `$TOP/$test_dir/` into `outputs/`:
   - `lakespec.sv` → `design.v`
   - `info.json`
   - `tb.sv` → `testbench.sv`
   - `comp_args.txt`, `PARGS.txt`, `design.args` (concatenation of both)
   - `bitstream.bs`
   - `gold/`
3. Runs `set_test_dir.py --test_dir $test_dir` to write the test dir path for downstream steps.

---

## SRAM macro selection

`construct-commercial-full.py` computes the correct GF12 SRAM macro from `storage_capacity`,
`data_width`, and `fetch_width` using `GF_Tech_Map` from `lake/top/tech_maps.py`, then sets
`gen_sram_macro` step parameters so `gen_srams.sh` builds the right macro.

Only `W01024B064` (1024 words × 64 bits = 8 KB single-port) is confirmed available in the
GF12 ADK. Configs that map to other macro dimensions will fail at the gen_sram_macro step.

---

## Post-synth flow: RTL sim, vcd2saif, PT power

The graph wires two `synopsys-vcd2saif-convert` instances:

- `synopsys-vcd2saif-convert-rtl` (after step 8 RTL sim) → feeds
  `synopsys-ptpx-synth` (synth-level power, no PnR required).
- `synopsys-vcd2saif-convert` (after step 25 GLS sim, post-signoff) → feeds
  `synopsys-ptpx-gl` (post-PnR power, signoff netlist + extracted SPEF).

The RTL-VCD path is gated on `+dump_vcd=1` to the testbench. Off by default
(sweeps that only need pass/fail don't pay multi-GB VCD costs). The plusarg is
threaded through `pd/thesis/synopsys-vcs-sim-rtl/`:

- `configure.yml` — parameter `dump_vcd: 0` (default), commands pass
  `make sim DUMP_VCD=$dump_vcd`.
- `Makefile` — `DUMP_VCD ?= 0`; sim invocation appends `+dump_vcd=$(DUMP_VCD)`.
- `tb.sv` — `if ($value$plusargs("dump_vcd=%d", ...) && ...) $dumpfile/$dumpvars`.

The wrapper at `ASPLOS_EXP/run_power_flow.sh` flips `dump_vcd` to 1 before
re-running the sim step, then drives vcd2saif + ptpx-synth (synth path) or
the full PnR + GLS + ptpx-gl chain (pnr path). `ASPLOS_EXP/collect_power_area.sh`
extracts power and area numbers from the generated reports into a comparison
table.

See `THESIS/REPLICATION.md` §7 for the full flow, sample numbers, and gotchas
(particularly §5.6 on Innovus license contention with parallel PnR).
