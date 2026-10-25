# Whole-CGRA app power → Ch. 5 app figures

This doc covers how garnet's whole-CGRA app power (`mflowgen/cgra_app_power.py`) feeds
these figures: `single_level_*`, `single_level_*_LI` and, later, `two_level_*`,
`tab:ul_perf` and `tab:ul_ppa_summary`. It replaces `run_matrix.py`'s plan, which
simulated one memory tile and used synthetic power. Written 2026-10-25. Nothing below has
run on the build machine yet.

```
/aha                     gen_app_bundle.py     one app run on a 16x16 thesis CGRA, recorded
                         (per app x config)    (bit-accurate aha test, --cgra-tiles)
   │ copy bundles
build machine            cgra_app_power.py     synth each config's MEM + PE tile once, replay
   │ copy zip back       (per config)          every app's tiles on them -> cgra_power.csv
lake                     THESIS.apps.cgra_power_ingest  -> THESIS/data/apps/**/results.json
                         generate_thesis_artifacts.py   -> single_level_*(_LI) figures
```

## 1. Config names

`THESIS/apps/garnet_configs.py` maps lake design points to garnet config names
(`python3 -m THESIS.apps.garnet_configs`). It reproduces garnet `sweep_specs._config_name`
exactly: all 522 configs of garnet's spec sets parse and rename to themselves (checked
2026-10-25 against garnet modern_gf `b52ef369`).

- A design point is its geometry: capacity, data width, fetch width, ports, dual-port,
  vec_capacity, dims, max_extent, max_sequence_width.
- On the thesis fabric every MemTile carries the user's options (garnet
  `THESIS_OPT_KNOBS`): static `_sharectr_portcg_thesis`, RV `_nofp_portcg_thesis_rv`.
- The ingest matches the bundle's `spec_config` by geometry, so the options don't change
  which design point a config belongs to.

| design_id | thesis static | thesis RV |
|---|---|---|
| port_fw4_dw16_sc8k_sp2x2 | fw4_dw16_sc8192_sp_in2_out2_vc2_sharectr_portcg_thesis | fw4_dw16_sc8192_sp_in2_out2_vc2_nofp_portcg_thesis_rv |
| port_fw2_dw32_sc8k_dp2x2 | fw2_dw32_sc8192_dp_in2_out2_vc2_sharectr_portcg_thesis | fw2_dw32_sc8192_dp_in2_out2_vc2_nofp_portcg_thesis_rv |
| mem_fw2_dw16_sc2k_sp1x1 | fw2_dw16_sc2048_sp_in1_out1_vc2_sharectr_portcg_thesis | fw2_dw16_sc2048_sp_in1_out1_vc2_nofp_portcg_thesis_rv |
| mem_fw1_dw16_sc4k_dp1x1 | fw1_dw16_sc4096_dp_in1_out1_sharectr_portcg_thesis | fw1_dw16_sc4096_dp_in1_out1_nofp_portcg_thesis_rv |
| port_fw4_dw8_sc8k_sp2x2 | fw4_dw8_sc8192_sp_in2_out2_vc2_sharectr_portcg_thesis | fw4_dw8_sc8192_sp_in2_out2_vc2_nofp_portcg_thesis_rv |
| port_fw4_dw32_sc8k_sp2x2 | fw4_dw32_sc8192_sp_in2_out2_vc2_sharectr_portcg_thesis | fw4_dw32_sc8192_sp_in2_out2_vc2_nofp_portcg_thesis_rv |
| mem_fw2_dw16_sc4k_dp2x2 | fw2_dw16_sc4096_dp_in2_out2_vc2_sharectr_portcg_thesis | fw2_dw16_sc4096_dp_in2_out2_vc2_nofp_portcg_thesis_rv |
| mem_fw4_dw16_sc8k_sp2x2 | (same as port_fw4_dw16_sc8k_sp2x2) | (same) |

Every name above is in garnet's `thesis_dse` or `thesis_mem_widths` spec set, so
`gen_app_bundle.py --config` takes it. Stock-fabric names drop the knobs and `_thesis`
(`--fabric stock`).

**The bundle matrix uses `DESIGNS_SINGLE_LEVEL` as it is: all 8 points** (user,
2026-10-25). That keeps three things the table shows:

- `port_fw4_dw16_sc8k_sp2x2` and `mem_fw4_dw16_sc8k_sp2x2` are the same garnet spec.
  `--commands` records it once; the ingest writes its rows under both ids, so both
  appear in the figures with the same numbers.
- Three points are dual-port, although the thesis text says the single-level
  evaluation is single-port only.
- The thesis design point (`fw4_dw16_sc4096_sp_in2_out2_vc2`, lake's default spec) is not
  in the set. Every thesis-fabric bundle recorded before 2026-10-25 is on that point, so
  the ingest skips them (`--keep-unmapped` writes them under the garnet name), and the
  whole matrix in §2a has to be recorded.

The text's other axes (dims, ScheduleGenerator duration) have no design points.

**Apps** (`registry.APPS`; matmul in both hierarchies, user 2026-10-25):

| app id | Halide app (`--app`) | level | bundle env |
|---|---|---|---|
| gaussian_agg | apps/gaussian | single | |
| unsharp_agg | apps/unsharp | single | |
| harris_agg | apps/harris_color | single | |
| camera_pipeline_agg | apps/camera_pipeline_2x2 | single | |
| matmul_agg | tests/matmul_rvtry3 | single | HL_TARGET=host-x86-64 HALIDE_GEN_ARGS=sched=1 |
| matmul_pond_agg | tests/matmul_rvtry3 | two | HL_TARGET=host-x86-64-enable_ponds |
| resnet_agg | pondspec_e2e/resnet | single | EXT=mat |

- **matmul** is the n=16 matmul the garnet bundles run. Single level unrolls the
  reduction into a PE adder tree, so only MEM tiles hold data; two level accumulates in
  the PE-tile pond.
  - The single-level matmul is **blocked** (`bundle_blocked`; `--commands` prints its
    lines commented out). On 16×16 the static compile hits clockwork's
    `garnet_single_port_ram_schedule` assert, and RV (23 PEs) does not route (garnet
    mflowgen/CLAUDE.md "No-pond matmul").
  - Its `HALIDE_GEN_ARGS=sched=1` also replaces the app Makefile's other default args;
    check them before unblocking it.
- **resnet** is the copy that ran on both thesis fabrics: `apps/resnet` with the output
  channel innermost, run with `EXT=mat`. Stock `apps/resnet` stops in `parse_design_meta`.
  No bundle of it exists yet. The stock app's `HL_TARGET` has no `enable_ponds`, so it's
  listed as single level; check the copy's `design.place` for pond blocks after recording.
- A cgra_app_power row maps to an app by its variant name (the app id, as `--commands`
  names variants and bundle dirs), else by the Halide app's basename when only one app
  has it. `matmul_rvtry3` is shared by both matmuls, so older sets need `--app-map`
  (e.g. `mmpond=matmul_pond_agg`).
- `single_level_*` plot only `level == "single"` apps; matmul_pond_agg waits for the
  two-level figures, which still need `DESIGNS_TWO_LEVEL` (pond design points).
- `tests/conv_3_3` is not a thesis app and is skipped.

## 2. Commands

### 2a. /aha: record the bundles

```bash
cd ~/lake   # THESIS checkout
python3 -m THESIS.apps.garnet_configs --commands > record.sh   # every design point and app
# on /aha, with LAKE = a lake checkout at origin/THESIS (what the build machine's rtl step
# checks out) and GARNET_REV = the build machine's garnet commit (fetch it into /aha/garnet):
LAKE=/aha/sweep_out/lake_snapshot GARNET_REV=<sha> bash record.sh
```

`record.sh` runs one background job per Halide app dir (the two matmuls share one).
Each job has its own private aha tree (`make_tree`, following garnet `mflowgen/CLAUDE.md`
"App bundles") and records its apps on every config in turn:

```
python3 <tree>/garnet/mflowgen/common/application/gen_app_bundle.py --config <config> \
    --app <Halide app> --bundle-dir <root>/<app id> --aha-dir <tree> --cgra-tiles \
    --pond-spec <root>/pond_default.json [--env KEY=VAL ...]
```

- With the 7 distinct configs × 2 modes × the 6 recordable apps that is 84 bundles; the
  14 single-level matmul lines are commented out.
- harris_color takes 1.5-2.5 h per bundle, so its job sets the wall time: 14 bundles,
  about 1-1.5 days.
- One job per app dir keeps two runs out of one app directory. aha's
  `application_parameters.json` is keyed by the app path, so a private app copy would
  silently lose the app's `HALIDE_GEN_ARGS`.
- `make_tree` is unverified. Compare it with session a16b3973's
  `thesis_dw/make_tree.sh` before the first run; the dw 8/32 configs write other widths'
  headers, which is why the MetaMapper/clockwork dirs must be real.
- **No `--rv-balance` on the thesis fabric.** `gen_app_bundle.py` refuses it there.
  Thesis RV records with DENSE_READY_VALID only, and `aha pnr` latency-matches the
  branches itself (balanced by default since 2026-10-19).
  - Only stock-fabric RV needs `--rv-balance 8` (`--fabric stock` adds it).
  - The existing stock dw16 RV bundles (`app_bundles_dw16_20261014`) are unbalanced.
    Their cycle counts include branch imbalance (harris 333 vs 62 µs), so they don't
    belong in an LI performance figure.
- `--pond-spec {}` (lake's default spec pond) in both modes, so each config's Tile_PE
  serves every app. Two-level runs pass their own pond JSON instead; the ingest then
  files them under `…__pond-<id>`.
- Apps not yet recorded on any thesis CGRA:
  - camera_pipeline_2x2: never run there.
  - unsharp: passed on the thesis RV fabric with EXHAUSTIVE_PIPE and balanced on stock;
    aha test sets RV_FLUSH_FIRST itself after a balanced PnR.
  - resnet: the `pondspec_e2e/resnet` copy has never been bundled; check it is still in
    /aha's Halide-to-Hardware.
  - matmul_pond_agg: bundled on the stock and design-point thesis CGRAs only.
  - dw 8/32: thesis apps there are session a16b3973's.

  Expect failures there. Each job's log is `<root>/<Halide app basename>.log`, each
  bundle's is `<bundle>/logs/`.

Then pack for the build machine, leaving the trees out:
`tar czf app_bundles_thesis_dp.tgz -C /aha/sweep_out app_bundles_thesis_dp`.

### 2b. Build machine: whole-CGRA power

```bash
cd /sim/mstrange/BUILD_CGRA/garnet && git checkout <GARNET_REV>
tar xzf app_bundles_thesis_dp.tgz -C /nsim/mstrange/BUILDS
B=/nsim/mstrange/BUILDS/app_bundles_thesis_dp
./mflowgen/cgra_app_power.py --any-config \
    --variants gaussian_agg=$B/gaussian_agg,unsharp_agg=$B/unsharp_agg,harris_agg=$B/harris_agg,camera_pipeline_agg=$B/camera_pipeline_agg,matmul_pond_agg=$B/matmul_pond_agg,resnet_agg=$B/resnet_agg \
    --clocks-from <the thesis_tiles_mem part dir>/thesis_clocks.json \
    --replay-jobs 4 --out-dir /nsim/mstrange/BUILDS/cgra_app_power_thesis_dp --zip
```

(The last line `--commands` prints is this command.) Every app on a config shares that
config's two tiles: cgra_app_power requires the same spec, mode and pond spec in every
variant of a config, which `--pond-spec {}` for all apps gives.

- `--clocks-from` must be the table the thesis tile and DSE runs used, so the app power
  runs at each fabric's clock. Every clocked part writes `<part>/thesis_clocks.json`; a
  thesis_fmax out dir works too.
- Load is tile-jobs (2) × parallel-jobs (4) × replay-jobs (4) sims. A 16×16 harris
  variant has about 200 tiles.
- `--dry-run` first prints each config's mode and clock.
- Copy `cgra_app_power_thesis_dp_<host>_<ts>.zip` back. It holds per-tile power reports
  and synth area; don't commit it or upload it anywhere (SRAM macro numbers).

### 2c. lake: ingest and figures

```bash
# kernel time + output size per bundle, from each bundle's logs/test.log (on /aha):
python3 -m THESIS.apps.cgra_power_ingest --bundles /aha/sweep_out/app_bundles_thesis_dp \
    --write-kernel-stats ks_thesis_dp.json
# here, with the zip and ks_thesis_dp.json copied over:
python3 -m THESIS.apps.cgra_power_ingest ../cgra_app_power_thesis_dp_<host>_<ts>.zip \
    --kernel-stats ks_thesis_dp.json --dry-run      # check the mapping, then drop --dry-run
python3 THESIS/generate_thesis_artifacts.py --only \
    single_level_power single_level_performance single_level_area single_level_utilization \
    single_level_energy_efficiency single_level_power_LI single_level_performance_LI \
    single_level_area_LI single_level_utilization_LI single_level_energy_efficiency_LI
```

`--tile-sweep THESIS/data/tile_sweep/<thesis_dse>.csv` takes the MemTile area from the
design-space sweep. It also adds `mem_tile_pnr_area_um2` where the sweep has PnR. Without
it, the areas come from the zip's own Genus reports (the tiles the power came from).

## 3. What results.json gets

The ingest writes one file per `cgra_power.csv` row (config × app), at
`THESIS/data/apps/<design_id>__<mode>__<fabric>[__pond-<id>]/<app_id>/results.json`.
`apps_query.select` filters on `mode`, `fabric` and `pond_id`:

- `single_level_*` plot static thesis-fabric rows with the default pond.
- `single_level_*_LI` plot the RV ones.
- Stock-fabric rows are kept but not plotted (`generators.APP_FABRIC`).

| figure field | from |
|---|---|
| `synth_power_w` | `app_W`: synth-level power of placed + route tiles, averaged over the replay window |
| `total_cycles` | kernel ns / tb clock (1 ns) from `aha test`'s log or the manifest's `kernel` |
| `clock_period_ps` | the run's clock (`run_meta.json`) |
| `tile_count` | MEM tiles with a placed core (ul_perf "Number of UL Memories") |
| `output_per_cycle` | output words / kernel cycles (ul_perf "Output Pixels per Cycle") |
| `synth_*_area_um2` | (placed + route tiles) × tile area: the CGRA area the app touches |
| `utilization` | not set: no source yet (§4); `mem_tile_utilization` = MEM tiles used / in array |
| `app_energy_j`, `array_power_w`, `window_cycles`, `sram_power_w` | kept for later figures |

Caveats:

- **The window is not the kernel.** The replay window runs from the stall release to the
  end of the recording, so it includes the testbench reading the output back from the
  GLB. The energy figure's power × kernel cycles × period therefore uses window-average
  power, and `app_energy_j` overcounts by the tail.
- **SRAM area is drawn in µm².** `single_level_area` stacks SRAM area absolute, while the
  tile figures show SRAM numbers relative (`tile_figures.SRAM_NDA`).

## 4. What garnet must add

Garnet is public, so each of these changes needs the user's OK first.

1. **`gen_app_bundle.py`: write `manifest["kernel"]`.** After the test step (which
   already greps `logs/test.log` for "comparison passed"), parse:
   - `It takes <t> ns total time to run kernel` and `kernel start at <t>`;
   - `The size of output is <n> Byte`;
   - the tb clock (`CLK_PERIOD`, Makefile default 1 ns).

   Store them as `{kernel_ns, start_ns, clk_period_ns, cycles, output_bytes,
   output_words, n_kernels}`; words = bytes >> `cgra_byte_offset`, which is dw/8.
   - The application step copies the manifest into the workspace, so the cgra_app_power
     zip carries it, and the ingest uses it ahead of `--bundles`.
   - Existing bundles need no re-record: their `logs/test.log` has the same lines, which
     is what `--bundles` reads.
2. **`cgra_app_power.py summarize()`: copy `kernel.cycles` / `output_words` into
   `cgra_power.csv`.** Add `kernel_s` = cycles / clock and `kernel_J`.
3. **Power over the kernel only.** `generate_testbench.py` opens `saif_window` at the
   stall release. It should also close it `kernel.cycles` vectors later.
   - Vectors are simvisdbutil rows extracted with `-notime`, about one per recording
     cycle; idle tiles have a few fewer. Extract with times to cut it exactly.
   - Then `app_W` is kernel-average power and `app_J` is kernel energy.
   - The window's start stays as is. cmd.tcl would need a stop condition on the window's
     falling edge.
4. **Utilization: SRAM busy fraction per MEM tile.** `run_all_tiles.py` (all three
   copies) should read the replay SAIF before it deletes the tile's sub-workspace.
   - Take the time each SRAM macro instance's chip-enable spends active, plus write-enable
     for the read/write split. Lake drives CEN = ~(re | we), so that is exactly the
     access cycles.
   - Report it as `sram_busy_frac` in `cgra_power_tiles.csv` and its mean over placed MEM
     tiles in `cgra_power.csv`; the ingest maps that to `utilization`.
   - Dual-port macros have two enables.
   - Tile-count utilization (`mem_tile_utilization`, from manifest `array_tiles`) needs
     no change.

## 5. Data that exists today (garnet mflowgen/CLAUDE.md)

- **Stock-fabric dw16 set.** 10 configs × conv/gauss/harris/mmpond, whole-CGRA power done
  2026-10-23: `/aha/sweep_out/cgra_app_power_dw16_20261023`, zip
  `cgra_app_power_dw16_r8cad-gf12_20261023-085057.zip`.
  - Of the lake design points, it matches `port_/mem_fw4_dw16_sc8k_sp2x2` and
    `mem_fw2_dw16_sc4k_dp2x2`.
  - Its apps that map are gaussian, harris_color and, with `--app-map
    mmpond=matmul_pond_agg`, the pond matmul.
  - The ingest takes it (`--bundles /aha/sweep_out/app_bundles_dw16_20261014` for
    cycles), but the figures don't plot stock rows.
- **Thesis-fabric sets.** `app_bundles_thesis_20261022` and
  `app_bundles_thesis_power_20261023`: conv/gauss/harris/mmpond on the design point
  only. No design point in the lake set matches them; a gf12 power run of the 10-23 set
  is not recorded.
