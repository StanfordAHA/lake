# Generic (no-ADK) synthesis → power path

A self-contained way to get **area + idle/active power** for a lake spec
netlist **without the gf12 ADK** — no SRAM macros, no LEF/QRC, no PnR. It
synthesizes the RTL against any generic standard-cell `.lib` (freepdk-45nm
here) with Genus (or DC), simulates the synth netlist with lake's idle and
active power tests, and reports power for each in PrimeTime.

Use it when you want a fast, portable "synthesis results → power" check and
the full mflowgen ADK flow isn't available on the machine.

> ⚠️ **These are 45nm behavioural-storage numbers, not gf12.** Storage is
> flops (`physical=False`), the node is generic, and there's no PnR. Treat
> results as *relative* (SP vs DP, sweep trends), not as tapeout-accurate
> absolutes. For real numbers use the ADK flow — see
> `../construct-commercial-full.py` and `../../THESIS/REPLICATION.md`.

---

## What it does

```
 .lib  ──lc_shell──▶  .db ──────────────────────────────┐
                                                        ├─ PrimeTime ──▶ power_idle.rpt
 RTL ──Genus/DC──▶ gates.v ──┬──────────────────────────┤  (× idle SAIF,  power_active.rpt
                             └─ VCS: power tb + gates.v  │     active SAIF)
 power-test-gen outputs ───────  + cell models ─▶ VCD ──vcd2saif──┘
 (idle/active programs, input stream)
```

1. **`.lib` → `.db`** (`lc_shell`) — PrimeTime consumes `.db`, cached in `synth/`.
2. **Synthesis** (`genus` default, `dc` optional) using the **same constraint
   recipe as the real flow** (`../constraints/constraints.tcl`):
   `set_false_path -from config_memory*`, flush multicycle (setup 10 / hold 9),
   `set_driving_cell INV_X2`, `set_load 7`, `set_max_fanout 20`,
   `set_max_transition 0.25*clk`. **Only the clock period is caller-supplied**,
   because a 45nm generic node can't meet the 12nm target frequency.
3. **Area/timing** reports from synthesis.
4. **Netlist sims** (VCS), idle and active: lake's power testbench
   (`../synopsys-vcs-sim-power/tb.sv`) drives `gates.v` + the lib's Verilog
   cell models with power-test-gen's programs and random input stream,
   zero-delay (`+nospecify +notimingcheck`). Each VCD → SAIF (`vcd2saif`).
5. **PrimeTime power**, idle and active separately, annotating the synth
   netlist with its own SAIF.

Because the activity comes from the netlist itself, its names match the
netlist and PrimeTime annotates ~100% of nets. That's the same method as
the mflowgen synth-level idle/active power since lake 6a79216c.

It needs **no ADK**: the only external inputs are a `.lib` and its Verilog
cell models, the RTL, and power-test-gen's outputs for the same spec.

---

## Prerequisites

- **RTL** — a lake spec `design.v` (e.g. `lakespec.sv` from
  `lake/tests/test_spec/thesis_sweep.py`).
- **A generic stdcell `.lib`** — e.g.
  `/aha/mflowgen/adks/freepdk-45nm/pkgs/base/stdcells.lib`.
- **The lib's Verilog cell models** — e.g.
  `/aha/mflowgen/adks/freepdk-45nm/pkgs/base/stdcells.v`. NanGate-style
  models (that one) need `+define+TETRAMAX`, or their reset flops start at X.
  The script adds it whenever the file has `ifdef TETRAMAX`.
- **Power tests for the same spec** — power-test-gen's outputs
  (`bitstream.{idle,active}.bs`, `PARGS.{idle,active}.txt`, `comp_args.txt`,
  `input_data.hex`), from `../power-test-gen/gen_power_bitstreams.py` with the
  spec kwargs that built the RTL (see `../power-test-gen/configure.yml`).
- **Tools** — Genus (or DC), Library Compiler, VCS, PrimeTime. Paths default
  to the Stanford `/cad` installs and are overridable via env
  (`GENUS`, `DC`, `LC`, `VCS`, `PT`, `VCD2SAIF`).

---

## Usage

```bash
./generic_synth_power.sh \
  --design      <path>/lakespec.sv \
  --lib         /aha/mflowgen/adks/freepdk-45nm/pkgs/base/stdcells.lib \
  --cells       /aha/mflowgen/adks/freepdk-45nm/pkgs/base/stdcells.v \
  --power-tests <power-test-gen outputs dir> \
  --outdir      <out> \
  [--tb <tb.sv>] [--sim-args "<extra vcs args>"] [--keep-vcd] \
  [--top lakespec] [--clock-ns 10.0] [--strip-path tb/dut_gen.dut] [--synth genus|dc]
```

Older mode: `--idle <saif|vcd> --active <saif|vcd>` (RTL-sim activity)
instead of `--cells`/`--power-tests`. It still runs, with a warning, but its
numbers aren't credible (see Caveats).

### Output

```
<out>/
├── summary.csv                 # design,top,synth,clock_ns,area,idle/active total+switching,
│                               #   activity (netlist-sim|rtl-saif), idle/active annotated %
├── sim_{idle,active}/          # netlist sims: vcs.log, sim.log (PASS), run.saif, outputs/
├── synth/
│   ├── <lib>.db                # cached lib->db
│   ├── gates.v                 # synth netlist
│   ├── syn.tcl, genus.log
│   └── reports/{area,gates,timing}.rpt
└── pt/
    ├── power_idle.rpt   power_idle.hier.rpt
    ├── power_active.rpt power_active.hier.rpt
    └── pt_{idle,active}.{tcl,log}
```

### Example (validated 2026-10-08)

freepdk-45nm, 10 ns, Genus (default), 1000-cycle power tests (lake
`lake/utils/power_test_programs.py`, seed 0). Total power in mW. The "RTL
activity" columns are the older `--idle/--active` mode on the SAME netlists:

| spec | area µm² | netlist sim: idle / active (×) | annotated | RTL activity: idle / active | annotated |
|---|---:|---:|---:|---:|---:|
| fw1 DP 1×1, 1 KB | 66,615 | 4.59 / 6.76 (1.47×) | 100% | — | — |
| fw2 SP 1×1, 2 KB | 137,271 | 8.85 / 15.8 (1.79×) | 100% | 13.8 / 13.1 | 3.4% |
| fw4 SP 2×2, 4 KB | 273,399 | 17.8 / 29.9 (1.68×) | 100% | 27.8 / 26.8 | 3.5% |

All six netlist sims print PASS. Idle captures 0 words on the read ports;
active captures 995 / 989 / 1959. The RTL-activity mode puts idle *above*
active. Wall time is Genus (9 / 33 / 57 min, run in parallel); each netlist
sim is ~7 / 16 / 30 s (VCS compile + run). With a DC netlist (`--synth dc`)
the same specs gave 5.31 / 7.86, 9.39 / 17.8 and 18.9 / 33.4 mW (hand-run
check, 2026-10-07).

---

## Fmax characterization (`fmax_sweep.sh`)

`generic_synth_power.sh` runs at a fixed clock (`--clock-ns`). To find how
fast a design *can* run, use `fmax_sweep.sh` — synthesis-only (no PT), it
synthesizes at each target period and reports worst slack, deriving
`Fmax = 1 / (target − worst_slack)`.

```bash
./fmax_sweep.sh \
  --design <path>/lakespec.sv \
  --lib    /aha/mflowgen/adks/freepdk-45nm/pkgs/base/stdcells.lib \
  --outdir <out> \
  --top lakespec --periods "4 3 2" --io-delay-frac 0
```

### `--io-delay-frac`: internal-logic vs system Fmax

The real flow (and `generic_synth_power.sh` by default) sets
`input_delay = 0.5 × clock_period`, reserving half the period for upstream
I/O. That's correct for *system* timing but **artificially caps Fmax** —
the internal logic only ever sees half the budget. For a max-internal-speed
number, pass `--io-delay-frac 0` (the sweep's default) so input→register
paths get the full period. Both `fmax_sweep.sh` and `generic_synth_power.sh`
accept `--io-delay-frac`.

> This is a deliberate deviation from the real-flow constraints: the result
> is **internal-logic Fmax**, not system Fmax with an I/O budget.

### As-the-target-tightens behaviour

Genus optimizes to the target, so a *tighter* target yields a *smaller*
achieved period until the design saturates. Sweep from loose to tight and
watch the **achieved period**; whichever target it stops improving at (or
first VIOLATES) is the real ceiling. Don't read Fmax off a single loose run.

### Example (validated, sp_fw4_sc8k_2x2, freepdk-45nm, `--io-delay-frac 0`)

| Target | Worst slack | Achieved period | Fmax | Status |
|---:|---:|---:|---:|---|
| 4 ns | +1.08 ns | 2.92 ns | ~342 MHz | MET |
| 3 ns | +0.53 ns | 2.47 ns | ~404 MHz | MET |
| 2 ns | +0.07 ns | 1.93 ns | **~518 MHz** | MET |

**~500–520 MHz**, vs a naive DC estimate of ~230 MHz that was pessimized by
a 1 ns input delay and no optimization pressure. The 2 ns target met by only
+0.068 ns, so the ceiling is just below 2 ns — push one more point (e.g.
1.8 ns) to find where slack goes negative.

---

## How this relates to the real ADK flow (consistency)

**Consistent with the mflowgen flow:**
- Same synthesizer (Genus) and **verbatim** the constraint exceptions from
  `../constraints/constraints.tcl` — including the `config_memory*` false-path
  and `flush` multicycle, which materially change QoR.
- Same activity model as the ADK flow's **synth-level** idle/active power
  (`synopsys-vcs-sim-{idle,active}-power` → `synopsys-ptpx-synth`): the same
  testbench and programs simulating the synth netlist (since lake 6a79216c).
- Same simulator: VCS (the ADK flow is VCS-native; Xcelium is only an optional
  `TOOL=` toggle). Genus output does **not** require Xcelium.

**Necessarily different (cannot be fixed by swapping the lib):**
- **Storage** — gf12 uses hard SRAM macros; here storage is behavioural flops.
  This dominates area (~93%) and idle power, so it will not transfer.
- **Clock target** — set per-node; 45nm can't hit the 12nm frequency.
- **No PnR** — no real parasitics. For post-layout power use the ADK flow's
  `synopsys-ptpx-gl` chain (and the new `synopsys-ptpx-gl-{idle,active}-power`
  nodes) on the gf12 machine.

---

## Gotchas baked into the script

All were real failures; the script already handles them:

1. **`current_design <top>` must come BEFORE `link`.** Otherwise PT links a
   random submodule from the netlist as top and power collapses to ~µW with
   0% annotation.
2. **Do NOT `read_sdc` a Genus-written `.sdc`.** Genus `write_sdc` embeds a
   `current_design` line that PT `read_sdc` rejects ("extra positional option"),
   which silently drops the clock so `clock_network` power reads 0. The script
   defines the clock directly with `create_clock` instead.
3. **Tools read stdin from `/dev/null`** (2026-10-08). On a Tcl error,
   Genus/DC/PT otherwise sit at their interactive prompt and the script never
   returns. Now the tool exits, and the missing `GENUS_SYNTH_DONE` /
   `PT_<variant>_DONE` marker reports the failure.

The constraint recipe assumes a lakespec-like top with `flush` and
`config_memory*` ports. On a design without them, Genus stops on
`set_false_path` (TUI-61) and the script reports "genus did not finish".

---

## Caveats to keep in mind when reading the numbers

- **Older `--idle/--active` mode: not credible.** RTL-sim activity matches
  only the flop and port names that survive synthesis, a few % of nets.
  PrimeTime gives the rest default toggle rates, so the storage flops look
  busy even in idle. On 2026-10-07 (freepdk45, 10 ns, DC netlists, same
  stimulus), it gave active ÷ idle = 1.00–1.06 on three of four specs, where
  the netlist sim gave 1.48–1.90. Idle came out ~55–60% high. The error is
  in flop power, not in the clock term. The 2026-08 example this doc used to
  show (51.0 vs 51.9 mW) has the same collapse.
- **The netlist sim adds seconds to tens of seconds per variant.** That's
  VCS compile plus a 1000-cycle run, mostly compile: ~7–30 s for the 1–4 KB
  Genus netlists above, against 9–57 min of synthesis. Run time grows with
  the cycle count; compile time doesn't.
- **Absolute values are 45nm behavioural** — good for relative comparisons and
  flow validation, not for absolute power claims.
