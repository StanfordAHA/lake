# Generic (no-ADK) synthesis → power path

A self-contained way to get **area + idle/active power** for a lake spec
netlist **without the gf12 ADK** — no SRAM macros, no LEF/QRC, no PnR. It
synthesizes the RTL against any generic standard-cell `.lib` (freepdk-45nm
here) with Genus (or DC), then annotates the synth netlist with idle/active
activity and reports power in PrimeTime.

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
 .lib  ──lc_shell──▶  .db ─┐
                           ├─ PrimeTime  ──▶ power_idle.rpt
 RTL ──Genus/DC──▶ gates.v ┤   (× idle SAIF, active SAIF)
                           └─▶ power_active.rpt
 idle/active SAIF (or VCD) ─┘
```

1. **`.lib` → `.db`** (`lc_shell`) — PrimeTime consumes `.db`, cached in `synth/`.
2. **Synthesis** (`genus` default, `dc` optional) using the **same constraint
   recipe as the real flow** (`../constraints/constraints.tcl`):
   `set_false_path -from config_memory*`, flush multicycle (setup 10 / hold 9),
   `set_driving_cell INV_X2`, `set_load 7`, `set_max_fanout 20`,
   `set_max_transition 0.25*clk`. **Only the clock period is caller-supplied**,
   because a 45nm generic node can't meet the 12nm target frequency.
3. **Area/timing** reports from synthesis.
4. **PrimeTime power**, idle and active separately, annotating the synth
   netlist with each SAIF.

It needs **no ADK**: the only external inputs are a `.lib`, the RTL, and the
two activity files.

---

## Prerequisites

- **RTL** — a lake spec `design.v` (e.g. `lakespec.sv` from
  `lake/tests/test_spec/thesis_sweep.py`).
- **A generic stdcell `.lib`** — e.g.
  `/aha/mflowgen/adks/freepdk-45nm/pkgs/base/stdcells.lib`.
- **Idle + active activity** — either `.saif` or `.vcd` (auto-converted with
  `vcd2saif`). Generate these with the power-test bitstream + VCS RTL sim flow;
  the pieces are `lake/pd/thesis/power-test-gen/gen_power_bitstreams.py` +
  `synopsys-vcs-sim-power`. A worked standalone example is
  `/aha/sweep_out/power_tests_rtl_smoke/run_spec.sh`.
- **Tools** — Genus (or DC), Library Compiler, PrimeTime. Paths default to the
  Stanford `/cad` installs and are overridable via env
  (`GENUS`, `DC`, `LC`, `PT`, `VCD2SAIF`).

---

## Usage

```bash
./generic_synth_power.sh \
  --design  <path>/lakespec.sv \
  --lib     /aha/mflowgen/adks/freepdk-45nm/pkgs/base/stdcells.lib \
  --idle    <path>/sim_idle/activity.saif \
  --active  <path>/sim_active/activity.saif \
  --outdir  <out> \
  [--top lakespec] [--clock-ns 10.0] [--strip-path tb/dut] [--synth genus|dc]
```

`--idle`/`--active` accept `.vcd` too (converted automatically).

### Output

```
<out>/
├── summary.csv                 # design,top,synth,clock_ns,area,idle/active total+switching
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

### Example (validated)

`sp_fw4_sc8k_2x2`, freepdk-45nm, 10 ns, Genus:

| | idle | active |
|---|---:|---:|
| Total power | ~51.0 mW | ~51.9 mW |
| Net switching | ~25 µW | ~1.6 mW (≈65× idle) |

Area ≈ 517k µm² / 141k cells. Clock-network-dominated, so the idle→active
*total* moves little; the datapath signature is in **net switching**.

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
- Same activity model as the ADK flow's **synth-level** power
  (`synopsys-ptpx-synth`): RTL-sim SAIF annotated onto the synth netlist.
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

## Two PrimeTime gotchas baked into the script

Both were real failures during bring-up; the script already handles them:

1. **`current_design <top>` must come BEFORE `link`.** Otherwise PT links a
   random submodule from the netlist as top and power collapses to ~µW with
   0% annotation.
2. **Do NOT `read_sdc` a Genus-written `.sdc`.** Genus `write_sdc` embeds a
   `current_design` line that PT `read_sdc` rejects ("extra positional option"),
   which silently drops the clock so `clock_network` power reads 0. The script
   defines the clock directly with `create_clock` instead.

---

## Caveats to keep in mind when reading the numbers

- **SAIF annotation is low (~2%)** — this is inherent to annotating *RTL-sim*
  activity onto a *synth* netlist (only register/boundary nets match by name;
  PT propagates the rest). Same limitation as `synopsys-ptpx-synth`. The
  clock-network term (dominant) is computed from the defined clock, not SAIF,
  so it's robust; the combinational/switching split is partly propagated.
- **Absolute values are 45nm behavioural** — good for relative comparisons and
  flow validation, not for absolute power claims.
