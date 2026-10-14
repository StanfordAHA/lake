"""Figures from the garnet Tile_MemCore lake-spec sweep.

Every generator reads the cached sweep table (``tile_sweep.load()``; ingest
first with ``python3 -m THESIS.pipeline.tile_sweep <sweep.zip>``). Area is
Genus synthesis cell area; power is PrimeTime power of the synthesis netlist
(``pw_synth_*``), active = max sustainable traffic, idle = empty program, both
on the same random input stream (garnet --memtile-power). Groups:

  component   the Lake component characterization figures (Port,
              IterationDomain, AddressGenerator, MemoryPort, Storage), area and
              power, which replace the standalone-lakespec (THESIS_BUILDS)
              versions. From a hierarchy-kept sweep (garnet --flatten-effort 0)
              each plots its own block of the lake controller (tile_sweep
              BLOCKS, ``blk_*`` / ``pw_*``): Port = port_inst, IterationDomain =
              port_id, affine pattern generators = port_ag + port_sg,
              MemoryPort = memory-port muxes/arbiters/decoders, Storage = the
              SRAM block.
  pub_*       the Physical Unified Buffer = lake controller + SRAM block.
  memtile_*   the whole CGRA memory tile (lakespec + MemCore wrapper + SB/CB
              interconnect) unless noted, including the synthesis -> PnR model
              (fit builds vs held-out builds).
  li_sched_*  the latency-insensitive ScheduleGenerator (RV builds).

Static runtime mode unless the figure compares modes or is RV-only, and only
``data_width == DATA_WIDTH`` configs (see below). Values that include the SRAM
macros are relative to a reference when ``SRAM_NDA`` is set. Signature matches
``generators.py``: ``(ctx, outpath)``; ``ctx`` is unused because the tile sweep
is not a THESIS_BUILDS experiment.

Terms used in labels:
  std-cell area  cell area minus the SRAM macros (Genus .lib macro area)
  MemCore logic  std-cell area of the MemCore_inst0 hierarchy (lakespec +
                 wrapper), i.e. without SB/CB interconnect
"""

from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.lines import Line2D  # noqa: E402

from . import tile_sweep  # noqa: E402
from .errors import MissingDataError  # noqa: E402

# ---- style -------------------------------------------------------------------
INK, INK2, MUTED = "#0b0b0b", "#52514e", "#898781"
GRID, AXIS = "#e1e0d9", "#c3c2b7"
SERIES = ["#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#4a3aa7", "#e34948"]
BLUE_RAMP = ["#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#104281"]  # ordinal, light→dark
FULL_W = 6.3  # inches, ~\textwidth
HALF_W = 3.0  # inches, a 0.48\textwidth subfigure

plt.rcParams.update({
    "font.size": 8.5, "axes.labelsize": 9, "axes.titlesize": 9, "legend.fontsize": 7.5,
    "xtick.labelsize": 8, "ytick.labelsize": 8, "lines.linewidth": 1.6, "lines.markersize": 5,
    "axes.edgecolor": AXIS, "axes.labelcolor": INK, "xtick.color": INK2, "ytick.color": INK2,
    "text.color": INK, "pdf.fonttype": 42, "ps.fonttype": 42, "legend.frameon": False,
})

K = 1000.0
UW = 1e-6  # W per µW
AREA_LBL = "Area (10³ µm²)"

# Configs with any other data width have broken RTL (2026-10-06), so their
# areas are meaningless; every figure drops them until that's fixed.
DATA_WIDTH = 16


def _ax_style(ax, ygrid=True, xgrid=False):
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    for s in ("left", "bottom"):
        ax.spines[s].set_linewidth(0.8)
    if ygrid:
        ax.yaxis.grid(True, color=GRID, linewidth=0.6)
    if xgrid:
        ax.xaxis.grid(True, color=GRID, linewidth=0.6)
    ax.set_axisbelow(True)


def _panel_label(ax, s):
    ax.text(-0.14, 1.02, s, transform=ax.transAxes, fontsize=9, fontweight="bold", va="bottom")


def _save(fig, outpath: Path):
    outpath.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(outpath, bbox_inches="tight")
    plt.close(fig)


def _load(mode: str | None = "static") -> pd.DataFrame:
    df = tile_sweep.load()
    df = df[df["data_width"] == DATA_WIDTH]
    if mode:
        df = df[df["mode"] == mode]
    if df.empty:
        raise MissingDataError(f"tile sweep has no {mode} configs")
    return df


def _exp(df: pd.DataFrame, exp: str) -> pd.DataFrame:
    sub = df[df["experiments"].fillna("").str.contains(exp)]
    if sub.empty:
        raise MissingDataError(f"tile sweep has no {exp} configs")
    return sub


def _log2_axis(ax, ticks, fmt=lambda v: f"{v:g}"):
    ax.set_xscale("log", base=2)
    ax.set_xticks(ticks)
    ax.set_xticklabels([fmt(t) for t in ticks])
    ax.minorticks_off()


def _ramp(n: int) -> list:
    """n ordinal blues, light to dark."""
    from matplotlib.colors import LinearSegmentedColormap
    cmap = LinearSegmentedColormap.from_list("blue_ramp", BLUE_RAMP)
    return [cmap(t) for t in np.linspace(0, 1, n)] if n > 1 else [BLUE_RAMP[2]]


# Memory organizations of MEMORY_EXP: color = SRAM bandwidth, line = SP/DP.
_BW_COLOR = {32: SERIES[0], 64: SERIES[1], 128: SERIES[2]}


def _topo_label(r) -> str:
    kind = "DP" if r["dual_port"] else "SP"
    return f"{kind} fw{int(r['vec_width'])}, {int(r['in_ports'])} in, {int(r['out_ports'])} out"


def _topo_style(r):
    return dict(color=_BW_COLOR[int(r["sram_bw_bits"])],
                linestyle="--" if r["dual_port"] else "-",
                marker="s" if r["dual_port"] else "o",
                markeredgecolor="white", markeredgewidth=0.8)


# ---- figures -------------------------------------------------------------------


def memtile_capacity_area(ctx, outpath: Path) -> None:
    """MEMORY_EXP: tile area and std-cell area vs capacity, per memory organization."""
    df = _exp(_load(), "MEMORY_EXP").copy()
    df["kb"] = df["storage_capacity"] / 1024
    fig, axes = plt.subplots(1, 2, figsize=(FULL_W, 2.7))
    groups = sorted(df.groupby(["sram_bw_bits", "dual_port", "vec_width", "in_ports", "out_ports"]),
                    key=lambda kv: (kv[0][0], kv[0][1]))
    for ax, col, lab in ((axes[0], "syn_cell", "(a) Total tile cell area"),
                         (axes[1], "syn_std", "(b) Std-cell area (SRAM excluded)")):
        for _, g in groups:
            g = g.sort_values("kb")
            r = g.iloc[0]
            ax.plot(g["kb"], g[col] / K, label=_topo_label(r), **_topo_style(r))
        _log2_axis(ax, [1, 2, 4, 8, 16, 32])
        ax.set_xlabel("Capacity (KB)")
        ax.set_ylabel(AREA_LBL)
        ax.set_title(lab, loc="left", fontsize=8.5, color=INK2)
        ax.set_ylim(bottom=0)
        _ax_style(ax)
    h, l = axes[0].get_legend_handles_labels()
    fig.legend(h, l, loc="upper center", ncol=3, bbox_to_anchor=(0.5, 0.0),
               title="Solid = single-port, dashed = dual-port; color = SRAM bits/cycle (32, 64, 128)",
               title_fontsize=7.5)
    fig.tight_layout()
    _save(fig, outpath)


def memtile_bandwidth_area(ctx, outpath: Path) -> None:
    """MEMORY_EXP at 8 KB: SRAM vs std-cell area, single- vs dual-port, per bandwidth."""
    df = _exp(_load(), "MEMORY_EXP")
    df = df[df["storage_capacity"] == 8192]
    if df.empty or df["sram_bw_bits"].nunique() < 2:
        raise MissingDataError("need 8 KB MEMORY_EXP configs at several bandwidths")
    bws = sorted(df["sram_bw_bits"].unique())
    fig, ax = plt.subplots(figsize=(FULL_W * 0.62, 2.8))
    w, gap = 0.36, 0.04
    ticks, tick_lbls = [], []
    for i, bw in enumerate(bws):
        for j, dp in enumerate((False, True)):
            r = df[(df["sram_bw_bits"] == bw) & (df["dual_port"] == dp)]
            if r.empty:
                continue
            r = r.iloc[0]
            x = i + (j - 0.5) * (w + gap)
            ax.bar(x, r["syn_sram"] / K, w, color=SERIES[0], edgecolor="white", linewidth=1,
                   label="SRAM macros" if i == 0 and j == 0 else None)
            ax.bar(x, r["syn_std"] / K, w, bottom=r["syn_sram"] / K, color=SERIES[1],
                   edgecolor="white", linewidth=1, label="Std cells" if i == 0 and j == 0 else None)
            ax.text(x, r["syn_cell"] / K * 1.015, f"{r['syn_cell'] / K:.1f}", ha="center",
                    va="bottom", fontsize=7, color=INK2)
            ticks.append(x)
            tick_lbls.append("DP" if dp else "SP")
        # bandwidth group label under the SP/DP tick labels
        ax.text(i, -0.11, f"{int(bw)} b/cycle", transform=ax.get_xaxis_transform(),
                ha="center", va="top", fontsize=8, color=INK2)
    ax.set_xticks(ticks)
    ax.set_xticklabels(tick_lbls, fontsize=7.5)
    ax.tick_params(axis="x", length=0)
    ax.set_ylabel(AREA_LBL)
    ax.set_xlabel("SRAM bandwidth (8 KB capacity, 16-bit data)", labelpad=16)
    ax.set_ylim(0, df["syn_cell"].max() / K * 1.12)
    ax.legend(loc="upper left")
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


def memtile_interconnect_area(ctx, outpath: Path) -> None:
    """CB / SB / other-top-level area vs Lake port count (16-bit data)."""
    df = _load()
    parts = [("syn_cb", "Connection boxes"), ("syn_sb", "Switch box"),
             ("syn_top", "Other top level")]
    cols = [c for c, _ in parts]
    ports = sorted(df["in_ports"].unique())
    med = df.groupby("in_ports")[cols].median()
    lo = df.groupby("in_ports")[cols].min()
    hi = df.groupby("in_ports")[cols].max()
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    w = 0.26
    for k, ((col, lab), colr) in enumerate(zip(parts, SERIES[:3])):
        xs = np.arange(len(ports)) + (k - 1) * w
        ax.bar(xs, med.loc[ports, col] / K, w * 0.92, color=colr, label=lab)
        ax.errorbar(xs, med.loc[ports, col] / K,
                    yerr=[(med - lo).loc[ports, col] / K, (hi - med).loc[ports, col] / K],
                    fmt="none", ecolor=MUTED, elinewidth=0.8, capsize=2)
    ax.set_xticks(range(len(ports)))
    n = df.groupby("in_ports").size()
    ax.set_xticklabels([f"{p}, {p}\n(n={n[p]})" for p in ports])
    ax.set_xlabel(f"Lake ports (in, out), {DATA_WIDTH}-bit data")
    ax.set_ylabel(AREA_LBL)
    ax.set_ylim(0, hi.values.max() / K * 1.08)
    _legend_above(ax, ncol=2)
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


def memtile_port_buffer_area(ctx, outpath: Path) -> None:
    """PORT_EXP: MemCore logic vs vectorization buffer bits (fw × dw × vc)."""
    df = _exp(_load(), "PORT_EXP").copy()
    df["bits"] = df["vec_width"] * df["data_width"] * df["vec_capacity"]
    fws = sorted(df["vec_width"].unique())
    if len(fws) > 3 or df["bits"].nunique() < 3:
        raise MissingDataError("need PORT_EXP at 2-3 fetch widths and several buffer sizes")
    style = {fw: (SERIES[i], m) for i, (fw, m) in enumerate(zip(fws, "os^"))}
    lin = np.polyfit(df["bits"], df["syn_memcore_std"], 1)
    fig, axes = plt.subplots(1, 2, figsize=(FULL_W, 2.7))
    for ax, col, lab in ((axes[0], "syn_memcore_std", "(a) MemCore logic (no interconnect)"),
                         (axes[1], "syn_std", "(b) Whole-tile std-cell area")):
        for fw in fws:
            g = df[df["vec_width"] == fw]
            c, m = style[fw]
            ax.scatter(g["bits"], g[col] / K, s=28, marker=m, color=c, edgecolor="white",
                       linewidth=0.6, zorder=3, label=f"fw{int(fw)}")
        if col == "syn_memcore_std":
            xs = np.geomspace(df["bits"].min(), df["bits"].max(), 60)
            ax.plot(xs, np.polyval(lin, xs) / K, color=MUTED, linewidth=1, zorder=2)
            ax.text(0.04, 0.94, f"linear fit: {lin[0]:.1f} µm² per buffer bit", transform=ax.transAxes,
                    va="top", fontsize=7.5, color=INK2)
        _log2_axis(ax, sorted(df["bits"].unique()), fmt=lambda v: f"{int(v)}")
        ax.set_xlabel(f"Buffer bits per port (fw × {DATA_WIDTH} × vc)")
        ax.set_ylabel(AREA_LBL)
        ax.set_title(lab, loc="left", fontsize=8.5, color=INK2)
        _ax_style(ax, xgrid=True)
    h, l = axes[0].get_legend_handles_labels()
    fig.legend(h, l, loc="lower center", ncol=len(fws), bbox_to_anchor=(0.5, -0.08),
               title="fetch width", title_fontsize=7.5)
    fig.tight_layout()
    _save(fig, outpath)


def memtile_control_area(ctx, outpath: Path) -> None:
    """ITERATION_DOMAIN / AFFINE: MemCore logic vs dimensionality, per counter range."""
    df = _load()
    fig, axes = plt.subplots(1, 2, figsize=(FULL_W, 2.6), sharey=True)
    for ax, exp, col, name, lab in (
            (axes[0], "ITERATION_DOMAIN_EXP", "max_extent", "max extent", "(a) IterationDomain sweep"),
            (axes[1], "AFFINE_PATTERN_GENERATOR_EXP", "max_sequence_width", "max sequence width",
             "(b) AddressGenerator sweep")):
        sub = _exp(df, exp)
        vals = sorted(sub[col].dropna().unique())
        ramp = BLUE_RAMP[-len(vals):] if len(vals) <= len(BLUE_RAMP) else BLUE_RAMP
        for v, c in zip(vals, ramp):
            g = sub[sub[col] == v].sort_values("dims")
            ax.plot(g["dims"], g["syn_memcore_std"] / K, color=c, marker="o",
                    markeredgecolor="white", markeredgewidth=0.6, label=f"{int(v)}")
        ax.set_xticks(range(1, 7))
        ax.set_xlabel("Dimensionality")
        ax.set_title(lab, loc="left", fontsize=8.5, color=INK2)
        ax.legend(title=name, title_fontsize=7.5, loc="upper left")
        _ax_style(ax)
    axes[0].set_ylabel("MemCore logic, " + AREA_LBL)
    fig.tight_layout()
    _save(fig, outpath)


def memtile_rv_overhead_area(ctx, outpath: Path) -> None:
    """Std-cell area change from static to ready-valid (RV), per config, by sweep."""
    df = _load(mode=None)[["config", "base_config", "mode", "experiments", "dual_port",
                           "vec_width", "syn_std"]]
    st = df[df["mode"] == "static"].set_index("config")
    rv = df[df["mode"] == "rv"].set_index("base_config")
    j = st.join(rv[["syn_std"]], rsuffix="_rv", how="inner")
    if j.empty:
        raise MissingDataError("no static/RV pairs in the tile sweep")
    j["pct"] = (j["syn_std_rv"] / j["syn_std"] - 1) * 100

    def group(r):
        e = r["experiments"]
        if "ITERATION_DOMAIN" in e:
            return "IterationDomain sweep (fw1)"
        if "AFFINE" in e:
            return "AddressGenerator sweep (fw1)"
        if "PORT_EXP" in e and "MEMORY" not in e:
            return "Port sweep (fw2–8, 8 KB)"
        kind = "dual-port" if r["dual_port"] else "single-port"
        return f"{kind} fw{int(r['vec_width'])}"
    j["group"] = j.apply(group, axis=1)
    order = (j.groupby("group")["pct"].median().sort_values(ascending=False).index.tolist())
    fig, ax = plt.subplots(figsize=(FULL_W * 0.75, 0.32 * len(order) + 0.8))
    rng = np.random.default_rng(0)
    for i, gname in enumerate(order):
        g = j[j["group"] == gname]["pct"]
        y = i + rng.uniform(-0.18, 0.18, len(g))
        ax.scatter(g, y, s=16, color=SERIES[0], alpha=0.85, edgecolor="white", linewidth=0.5, zorder=3)
        ax.plot([g.median()] * 2, [i - 0.3, i + 0.3], color=INK, linewidth=1.4, zorder=4)
        ax.text(g.max() + 1.2, i, f"median {g.median():+.0f}%  (n={len(g)})", va="center",
                fontsize=7, color=INK2)
    ax.axvline(0, color=AXIS, linewidth=1)
    ax.set_yticks(range(len(order)))
    ax.set_yticklabels(order)
    ax.invert_yaxis()
    ax.set_xlabel("RV std-cell area vs static (%)")
    ax.set_xlim(min(-5, j["pct"].min() - 2), j["pct"].max() + 22)
    _ax_style(ax, ygrid=False, xgrid=True)
    fig.tight_layout()
    _save(fig, outpath)


def memtile_interconnect_power(ctx, outpath: Path) -> None:
    """CB / SB synth-level power vs Lake port count: bars = active (max-traffic)
    power, median with min-max over configs; ticks = idle.

    No "other top level" bar: tile_glue power is the tile total minus its
    children, and PT prints each to 3 significant digits, so that remainder is
    rounding noise (it comes out negative on most configs).
    """
    df = _load()
    parts = [("cb", "Connection boxes"), ("sb", "Switch box")]
    act = [f"pw_synth_active_{b}" for b, _ in parts]
    idle = [f"pw_synth_idle_{b}" for b, _ in parts]
    _blocks(df, act + idle, "interconnect power")
    ports = sorted(df["in_ports"].unique())
    g = df.groupby("in_ports")
    med, lo, hi, imed = g[act].median(), g[act].min(), g[act].max(), g[idle].median()
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    w = 0.32
    for k, ((b, lab), colr) in enumerate(zip(parts, SERIES[:2])):
        xs = np.arange(len(ports)) + (k - 0.5) * w
        a = f"pw_synth_active_{b}"
        ax.bar(xs, med.loc[ports, a] / UW, w * 0.92, color=colr, label=lab)
        ax.errorbar(xs, med.loc[ports, a] / UW,
                    yerr=[(med - lo).loc[ports, a] / UW, (hi - med).loc[ports, a] / UW],
                    fmt="none", ecolor=MUTED, elinewidth=0.8, capsize=2)
        ax.scatter(xs, imed.loc[ports, f"pw_synth_idle_{b}"] / UW, marker="_", s=90, color=INK,
                   linewidth=1.4, zorder=4, label="Idle" if k == 0 else None)
    ax.set_xticks(range(len(ports)))
    ax.set_xticklabels([f"{p}, {p}\n(n={g.size()[p]})" for p in ports])
    ax.set_xlabel(f"Lake ports (in, out), {DATA_WIDTH}-bit data")
    ax.set_ylabel("Active power (µW)")
    ax.set_ylim(0, hi.values.max() / UW * 1.08)
    _legend_above(ax, ncol=2)
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


# Tile blocks grouped for the breakdown figure (tile_sweep BLOCKS).
_BREAKDOWN = [
    ("SRAM", ["sram"]),
    ("Lake controller", ["ctrl"]),
    ("Config registers", ["cfg_regs"]),
    ("FIFOs, StencilValid, ROM", ["fifos", "stencil", "rom"]),
    ("MemCore muxes + glue", ["mc_mux", "mc_glue", "inner_glue"]),
    ("Switch box", ["sb"]),
    ("Connection boxes", ["cb"]),
    ("Tile glue", ["tile_glue"]),
]
# Three of the PnR fit geometries, small to large; each is built static and RV.
_BREAKDOWN_CONFIGS = [
    ("fw1_dw16_sc4096_sp_in1_out1", "fw1, 4 KB, 1 in, 1 out"),
    ("fw4_dw16_sc8192_sp_in2_out2_vc2", "fw4, 8 KB, 2 in, 2 out"),
    ("fw8_dw16_sc32768_sp_in4_out4_vc2", "fw8, 32 KB, 4 in, 4 out"),
]


def memtile_block_breakdown(ctx, outpath: Path) -> None:
    """Share of tile area, idle power and active power per block (synthesis),
    static vs RV, for three tile sizes."""
    df = _load(mode=None).set_index("config")
    rows = [(f"{lab}, {'RV' if rv else 'static'}", name + ("_rv" if rv else ""))
            for name, lab in _BREAKDOWN_CONFIGS for rv in (False, True)]
    missing = [c for _, c in rows if c not in df.index]
    if missing:
        raise MissingDataError(f"breakdown configs not in the tile sweep: {', '.join(missing)}")
    panels = (("blk_", "(a) Cell area"), ("pw_synth_idle_", "(b) Idle power"),
              ("pw_synth_active_", "(c) Active power"))
    fig, axes = plt.subplots(1, 3, figsize=(FULL_W, 2.9), sharey=True)
    for ax, (pre, title) in zip(axes, panels):
        cols = [f"{pre}{b}" for _, bs in _BREAKDOWN for b in bs]
        _blocks(df.reset_index(), cols, title)
        for i, (_, c) in enumerate(rows):
            # PT prints 3 significant digits, so a tiny block can come out < 0
            v = np.array([max(sum(df.at[c, f"{pre}{b}"] for b in bs), 0.0) for _, bs in _BREAKDOWN])
            v = v / v.sum() * 100
            left = 0.0
            for j, ((lab, _), x) in enumerate(zip(_BREAKDOWN, v)):
                ax.barh(i, x, left=left, color=SERIES[j], edgecolor="white", linewidth=0.6,
                        height=0.72, label=lab if i == 0 else None)
                if x >= 9:
                    ax.text(left + x / 2, i, f"{x:.0f}", ha="center", va="center", fontsize=6.5,
                            color="white")
                left += x
        ax.set_xlim(0, 100)
        ax.set_xticks([0, 25, 50, 75, 100])
        ax.set_xlabel("Share of tile (%)")
        ax.set_title(title, loc="left", fontsize=8.5, color=INK2)
        _ax_style(ax, ygrid=False)
    axes[0].set_yticks(range(len(rows)))
    axes[0].set_yticklabels([r for r, _ in rows], fontsize=7.5)
    axes[0].invert_yaxis()
    h, l = axes[0].get_legend_handles_labels()
    fig.legend(h, l, loc="upper center", ncol=4, bbox_to_anchor=(0.5, 0.0), fontsize=7)
    fig.tight_layout()
    _save(fig, outpath)


def _bw_gbps(df: pd.DataFrame) -> pd.Series:
    """Delivered output bandwidth of the active power test (Gb/s). The static
    program (lake utils/power_test_programs.py) moves one SRAM word per port
    per period P = max(fw, n_in + n_out) (single-port) or max(fw, n_in, n_out)
    (dual-port), fw elements at one per cycle, so each port carries fw / P
    elements per cycle. RV twins measure the same output activity (within 1%)."""
    period = np.where(df["dual_port"],
                      np.maximum.reduce([df["vec_width"], df["in_ports"], df["out_ports"]]),
                      np.maximum(df["vec_width"], df["in_ports"] + df["out_ports"]))
    return df["out_ports"] * df["vec_width"] / period * df["data_width"] / df["clock_period_ps"] * 1e3


# RV-vs-static energy breakdown: non-SRAM blocks (tile_sweep BLOCKS) grouped.
_RV_GROUPS = [
    ("Ports (SIPO/PISO)", ["c_port"], SERIES[0]),
    ("IterationDomains + AddressGens", ["c_id", "c_ag"], BLUE_RAMP[0]),
    ("Static ScheduleGens", ["c_sg"], SERIES[7]),
    ("Comparison network", ["c_rvnet"], SERIES[1]),
    ("Filter storage + read buffers", ["c_storage", "c_rdbuf"], SERIES[3]),
    ("Tile I/O FIFOs", ["fifos"], SERIES[2]),
    ("Switch + connection boxes", ["sb", "cb"], SERIES[6]),
    ("Config registers", ["cfg_regs"], MUTED),
    ("MemoryPort, StencilValid, ROM", ["c_mp", "stencil", "rom"], SERIES[4]),
]
_RV_CONFIGS = [
    ("fw1_dw16_sc8192_sp_in2_out2_me1024", "fw1 8 KB\n6-D\n2 in, 2 out"),
    ("fw1_dw16_sc4096_sp_in1_out1", "fw1 4 KB\n1 in, 1 out"),
    ("fw4_dw16_sc8192_sp_in2_out2_vc2", "fw4 8 KB\n2 in, 2 out"),
    ("fw8_dw16_sc32768_sp_in4_out4_vc2", "fw8 32 KB\n4 in, 4 out"),
    ("fw4_dw16_sc8192_dp_in4_out4_vc2", "DP fw4\n8 KB\n4 in, 4 out"),
]


def memtile_rv_energy_breakdown(ctx, outpath: Path) -> None:
    """Static vs RV twins, per block: (a) non-SRAM active energy per delivered
    bit, with the SRAM's share of active power on top; (b) non-SRAM idle power."""
    df = _load(mode=None).set_index("config")
    missing = [c + s for c, _ in _RV_CONFIGS for s in ("", "_rv") if c + s not in df.index]
    if missing:
        raise MissingDataError(f"RV breakdown configs not in the tile sweep: {', '.join(missing)}")
    _blocks(df.reset_index(), [f"pw_synth_{v}_{b}" for v in ("active", "idle")
                               for _, bs, _ in _RV_GROUPS for b in bs if b not in ("c_rvnet", "c_storage", "c_rdbuf")],
            "RV energy breakdown")
    bw = _bw_gbps(df)
    fig, axes = plt.subplots(1, 2, figsize=(FULL_W, 3.0))
    w = 0.38
    for ax, var in zip(axes, ("active", "idle")):
        ticks, labels = [], []
        for i, (cfg, lab) in enumerate(_RV_CONFIGS):
            for j, (suffix, mode) in enumerate((("", "Static"), ("_rv", "RV"))):
                r = df.loc[cfg + suffix]
                x = i + (j - 0.5) * (w + 0.04)
                scale = 1e12 / (bw[cfg + suffix] * 1e9) if var == "active" else 1e3  # pJ/bit or mW
                bottom = 0.0
                for name, bs, c in _RV_GROUPS:
                    # PT prints 3 significant digits, so a tiny block can come out < 0
                    v = max(sum(np.nan_to_num(r.get(f"pw_synth_{var}_{b}", 0.0)) for b in bs), 0.0) * scale
                    ax.bar(x, v, w, bottom=bottom, color=c, edgecolor="white", linewidth=0.5,
                           label=name if (i == 0 and j == 1) else None)
                    bottom += v
                if var == "active":
                    share = r["pw_synth_active_sram"] / r["synth_active_total_power"] * 100
                    note = f"{share:.0f}%" if SRAM_NDA else f"+{r['pw_synth_active_sram'] * scale:.2f}"
                else:
                    note = f"{bottom:.2f}"
                ax.text(x, bottom, note, ha="center", va="bottom", fontsize=5.5, color=INK2)
                ticks.append(x)
                labels.append("S" if mode == "Static" else "RV")
            ax.text(i, -0.09, lab, transform=ax.get_xaxis_transform(), ha="center", va="top",
                    fontsize=6, color=INK2)
        ax.set_xticks(ticks)
        ax.set_xticklabels(labels, fontsize=6)
        ax.tick_params(axis="x", length=0)
        ax.set_ylim(0, ax.get_ylim()[1] * 1.12)
        _ax_style(ax)
    axes[0].set_ylabel("Non-SRAM active energy (pJ/bit)")
    axes[0].set_title("(a) Active per bit (top: SRAM share)" if SRAM_NDA else
                      "(a) Active per bit (top: SRAM pJ/bit)", loc="left", fontsize=8, color=INK2)
    axes[1].set_ylabel("Non-SRAM idle power (mW)")
    axes[1].set_title("(b) Idle (top: total)", loc="left", fontsize=8, color=INK2)
    h, l = axes[0].get_legend_handles_labels()
    fig.legend(h, l, loc="upper center", ncol=3, fontsize=7, bbox_to_anchor=(0.5, 0.0))
    fig.tight_layout()
    fig.subplots_adjust(bottom=0.3, wspace=0.2)
    _save(fig, outpath)


# ---- synthesis -> PnR model: fit set vs held-out set ---------------------------
# The sweep's 12 PnR builds: 8 fit (4 geometries x static/RV, pnr_role "fit")
# and 4 held out (pnr_role "validate"), chosen by structure before any data
# existed. Each runtime mode gets its own linear fit pnr = a * synth + b, as in
# garnet sweep_specs.py (correlation_fit.csv / memtile_power_fit.csv).

_MODE_STYLE = {"static": (SERIES[0], "Static"), "rv": (SERIES[1], "RV")}


def _model_panel(ax, rax, p: pd.DataFrame, xcol: str, ycol: str, norm: float, unit: str):
    """Scatter (fit = dots, held out = stars) with each mode's fit line on
    ``ax``; prediction error per build on ``rax``. Returns the held-out errors."""
    lo, hi = p[xcol].min() / norm, p[xcol].max() / norm
    pad = (hi - lo) * 0.06
    errs = []
    for mode, (c, name) in _MODE_STYLE.items():
        g = p[p["mode"] == mode]
        f, v = g[g["pnr_role"] == "fit"], g[g["pnr_role"] == "validate"]
        if len(f) < 3:
            raise MissingDataError(f"fewer than 3 {mode} fit builds reached PnR")
        a, b = np.polyfit(f[xcol] / norm, f[ycol] / norm, 1)
        r2 = np.corrcoef(f[xcol], f[ycol])[0, 1] ** 2
        # solid over the fit range, dotted where held-out builds extrapolate
        fx = np.array([f[xcol].min(), f[xcol].max()]) / norm
        ax.plot(fx, a * fx + b, color=c, linewidth=1.1, zorder=2,
                label=f"{name} fit: $y = {a:.3f}x {'+' if b >= 0 else '-'} {abs(b):.{_dec(b)}f}$, "
                      f"$R^2 = {r2:.5f}$")
        ex = np.array([min(lo - pad, fx[0]), max(hi + pad, fx[1])])
        ax.plot(ex, a * ex + b, color=c, linewidth=0.9, linestyle=":", zorder=1)
        for sub, m, s, lab in ((f, "o", 26, f"{name}, fit set (n={len(f)})"),
                               (v, "*", 110, f"{name}, held out (n={len(v)})")):
            x, y = sub[xcol] / norm, sub[ycol] / norm
            e = (a * x + b - y) / y * 100
            ax.scatter(x, y, marker=m, s=s, color=c, edgecolor="white", linewidth=0.6, zorder=3,
                       label=lab)
            rax.scatter(x, e, marker=m, s=s * 0.8, color=c, edgecolor="white", linewidth=0.6,
                        zorder=3)
            if m == "*":
                errs.extend(e.abs().tolist())
    ax.set_xlim(lo - pad, hi + pad)
    rax.set_xlim(lo - pad, hi + pad)
    rax.axhline(0, color=AXIS, linewidth=0.9, zorder=1)
    lim = max(1.0, np.ceil(max(abs(v) for v in rax.get_ylim())))
    rax.set_ylim(-lim, lim)
    rax.set_ylabel("Error (%)")
    _ax_style(ax, xgrid=True)
    _ax_style(rax, xgrid=True)
    ax.tick_params(labelbottom=False)
    return errs


def _dec(v: float) -> int:
    """Decimals for an intercept: about 3 significant digits."""
    v = abs(v)
    return 0 if v >= 100 else 1 if v >= 10 else 2 if v >= 1 else 3


def _model_norm(p: pd.DataFrame, col: str) -> tuple[float, str]:
    """Axis scale: the smallest fit build's synthesis value when SRAM_NDA (the
    totals include the SRAM macros), else plain 10³ µm² / mW."""
    if SRAM_NDA:
        return p.loc[p["pnr_role"] == "fit", col].min(), "× smallest fit tile"
    return (K, "10³ µm²") if "area" in col else (1e-3, "mW")


def _pnr_builds() -> pd.DataFrame:
    df = _load(mode=None)
    if "pnr_role" not in df.columns:
        raise MissingDataError("tile sweep has no pnr_role column (no PnR builds)")
    p = df[df["pnr_role"].isin(["fit", "validate"])]
    if p["pnr_role"].eq("validate").sum() == 0:
        raise MissingDataError("tile sweep has no held-out PnR builds")
    return p


def memtile_synth_vs_pnr_area(ctx, outpath: Path) -> None:
    """Synthesis -> PnR area model: PnR total area vs Genus cell area for the 8
    fit builds (dots) and the 4 held-out builds (stars), one fit per runtime
    mode; lower panel = each build's prediction error."""
    p = _pnr_builds().dropna(subset=["tile_synth_cell_area", "tile_pnr_total_area"])
    norm, unit = _model_norm(p, "tile_synth_cell_area")
    fig, (ax, rax) = plt.subplots(2, 1, figsize=(FULL_W * 0.62, 3.9), sharex=True,
                                  gridspec_kw={"height_ratios": [3, 1.15], "hspace": 0.08})
    errs = _model_panel(ax, rax, p, "tile_synth_cell_area", "tile_pnr_total_area", norm, unit)
    ax.set_ylabel(f"PnR total area ({unit})")
    rax.set_xlabel(f"Synthesis cell area ({unit})")
    ax.legend(loc="upper left", fontsize=6.8, handletextpad=0.4)
    ax.text(0.98, 0.04, f"Held-out |error|: mean {np.mean(errs):.2f}%, max {max(errs):.2f}%",
            transform=ax.transAxes, ha="right", va="bottom", fontsize=7.5, color=INK2)
    fig.align_ylabels([ax, rax])
    _save(fig, outpath)


def memtile_synth_vs_pnr_power(ctx, outpath: Path) -> None:
    """Synthesis -> PnR power model, idle and active, same layout as the area
    figure (fit builds = dots, held-out builds = stars)."""
    p = _pnr_builds()
    fig, axes = plt.subplots(2, 2, figsize=(FULL_W, 3.9), sharex="col",
                             gridspec_kw={"height_ratios": [3, 1.15], "hspace": 0.08,
                                          "wspace": 0.3})
    for k, (var, title) in enumerate((("idle", "(a) Idle"), ("active", "(b) Active"))):
        x, y = f"synth_{var}_total_power", f"pnr_{var}_total_power"
        q = p.dropna(subset=[x, y])
        norm, unit = _model_norm(q, x)
        ax, rax = axes[0, k], axes[1, k]
        errs = _model_panel(ax, rax, q, x, y, norm, unit)
        ax.set_title(title, loc="left", fontsize=8.5, color=INK2)
        ax.set_ylabel(f"PnR power ({unit})")
        rax.set_xlabel(f"Synthesis power ({unit})")
        ax.legend(loc="upper left", fontsize=6, handletextpad=0.3)
        ax.text(0.98, 0.04, f"Held-out |error|: mean {np.mean(errs):.1f}%, max {max(errs):.1f}%",
                transform=ax.transAxes, ha="right", va="bottom", fontsize=7, color=INK2)
    fig.align_ylabels(axes[:, 0])
    _save(fig, outpath)


# ---- Ch. 4 component characterization (replaces the standalone versions) ------
# Half-width figures for the 0.48\textwidth subfigures in main_thesis.tex. Same
# output paths and sweep axes as the THESIS_BUILDS generators in generators.py.
# Area = Genus cell area of the component's blocks; power = PT synth-level
# power of the same blocks, active (max sustainable traffic) solid and idle
# (empty program, same input stream) dashed.


def _blocks(df: pd.DataFrame, cols: list[str], what: str) -> pd.Series:
    """Sum of per-block columns; needs a hierarchy-kept sweep."""
    if any(c not in df.columns for c in cols) or df[cols].isna().all().all():
        raise MissingDataError(f"{what}: no per-block values ({', '.join(cols)}); ingest a "
                               "hierarchy-kept sweep (garnet --flatten-effort 0)")
    return df[cols].sum(axis=1, min_count=1)


def _metric(df: pd.DataFrame, blocks: list[str], metric: str, what: str) -> pd.DataFrame:
    """Adds ``y`` (area, or active power) and for power ``y2`` (idle power)."""
    df = df.copy()
    if metric == "area":
        df["y"] = _blocks(df, [f"blk_{b}" for b in blocks], what)
    else:
        df["y"] = _blocks(df, [f"pw_synth_active_{b}" for b in blocks], what)
        df["y2"] = _blocks(df, [f"pw_synth_idle_{b}" for b in blocks], what)
    return df


def _units(df: pd.DataFrame, metric: str) -> tuple[float, str]:
    if metric == "area":
        return (K, "10³ µm²") if df["y"].max() >= 2000 else (1.0, "µm²")
    return (1e-3, "mW") if df["y"].max() >= 2e-3 else (1e-6, "µW")


def _style_handles(metric: str, prefix: bool = False) -> list:
    hs = []
    if metric == "power":
        hs += [Line2D([], [], color=MUTED, linestyle="-", label="active"),
               Line2D([], [], color=MUTED, linestyle="--", linewidth=1.1, label="idle")]
    if prefix:
        hs.append(Line2D([], [], color=MUTED, marker="o", markersize=4, linestyle="none",
                         markerfacecolor="white", label="pre-fix RTL"))
    return hs


def _legend_above(ax, handles=None, labels=None, title=None, ncol=None):
    if handles is None:
        handles, labels = ax.get_legend_handles_labels()
    elif labels is None:
        labels = [h.get_label() for h in handles]
    order = sorted(range(len(labels)), key=lambda i: labels[i].lower().startswith("idle"))
    handles, labels = [handles[i] for i in order], [labels[i] for i in order]
    n = len(handles)
    ncol = ncol or (n if n <= 4 else -(-n // 2))
    ax.legend(handles, labels, title=title, title_fontsize=7, fontsize=7, loc="lower left",
              bbox_to_anchor=(0.0, 1.0), ncol=ncol, columnspacing=0.9, handlelength=1.5,
              borderaxespad=0.3, alignment="left")


def _sweep_fig(outpath: Path, exp: str, x: str, hue: str, hue_name: str, xlabel: str,
               blocks: list[str], ylabel: str, xfmt=lambda v: f"{v:g}", log2: bool = False,
               metric: str = "area", mode: str = "static") -> None:
    """``metric`` of ``blocks`` vs ``x`` for one experiment, one line per ``hue``."""
    df = _exp(_load(mode), exp).dropna(subset=[x, hue])
    if df[x].nunique() < 2:
        raise MissingDataError(f"{exp}: {x} takes a single value at data_width={DATA_WIDTH}")
    df = _metric(df, blocks, metric, ylabel)
    df["prefix"] = _rvnet_prefix(df) if mode == "rv" else False
    scale, unit = _units(df, metric)
    vals = sorted(df[hue].unique())
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    handles = []
    for v, c in zip(vals, _ramp(len(vals))):
        g = df[df[hue] == v].sort_values(x)
        h, = ax.plot(g[x], g["y"] / scale, color=c, marker="o", markersize=4,
                     markeredgecolor="white", markeredgewidth=0.6, label=f"{v:g}")
        handles.append(h)
        if metric == "power":
            ax.plot(g[x], g["y2"] / scale, color=c, linestyle="--", linewidth=1.1, marker="o",
                    markersize=2.6, markeredgewidth=0)
        pre = g[g["prefix"]]
        for col in ["y"] + (["y2"] if metric == "power" else []):
            ax.scatter(pre[x], pre[col] / scale, s=18 if col == "y" else 9, facecolor="white",
                       edgecolor=c, linewidth=0.9, zorder=4)
    ticks = sorted(df[x].unique())
    if log2:
        _log2_axis(ax, ticks, xfmt)
    else:
        ax.set_xticks(ticks)
        ax.set_xticklabels([xfmt(t) for t in ticks])
    ax.set_xlabel(xlabel)
    ax.set_ylabel(f"{'Area' if metric == 'area' else 'Power'}, {ylabel} ({unit})")
    ax.set_ylim(0, df["y"].max() / scale * 1.08)
    _legend_above(ax, handles + _style_handles(metric, df["prefix"].any()), title=hue_name)
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


def _port_data_width(ctx, outpath: Path, metric: str) -> None:
    df = _exp(_load(), "PORT_EXP")
    if df["data_width"].nunique() < 2:
        raise MissingDataError(
            f"needs PORT_EXP tile builds at several data widths; only {DATA_WIDTH}-bit "
            "configs are used (other widths have broken RTL)")
    _sweep_fig(outpath, "PORT_EXP", "data_width", "vec_width", "fetch width",
               "Port data width (bits)", ["c_port"], "4 Ports",
               log2=True, metric=metric)


def port_area_vs_data_width(ctx, outpath: Path) -> None:
    """PORT_EXP: Port area vs port data width, one line per fetch width (vc=2)."""
    _port_data_width(ctx, outpath, "area")


def port_power_vs_data_width(ctx, outpath: Path) -> None:
    _port_data_width(ctx, outpath, "power")


def port_area_vs_vc(ctx, outpath: Path) -> None:
    """PORT_EXP: Port area (the 4 port_inst blocks) vs vectorization capacity,
    one line per fetch width."""
    _sweep_fig(outpath, "PORT_EXP", "vec_capacity", "vec_width", "fetch width",
               "Vectorization capacity (entries)", ["c_port"], "4 Ports", log2=True)


def port_power_vs_vc(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "PORT_EXP", "vec_capacity", "vec_width", "fetch width",
               "Vectorization capacity (entries)", ["c_port"], "4 Ports", log2=True,
               metric="power")


def iter_dom_area_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "dims", "max_extent", "maximum extent",
               "Dimensionality", ["c_id"], "4 IterationDomains")


def iter_dom_power_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "dims", "max_extent", "maximum extent",
               "Dimensionality", ["c_id"], "4 IterationDomains", metric="power")


def iter_dom_area_vs_max_extent(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "max_extent", "dims", "dimensionality",
               "Maximum extent", ["c_id"], "4 IterationDomains", log2=True)


def iter_dom_power_vs_max_extent(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "max_extent", "dims", "dimensionality",
               "Maximum extent", ["c_id"], "4 IterationDomains", log2=True, metric="power")


def affine_area_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "dims", "max_sequence_width",
               "maximum value", "Dimensionality", ["c_ag", "c_sg"], "4 AGs + 4 SGs")


def affine_power_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "dims", "max_sequence_width",
               "maximum value", "Dimensionality", ["c_ag", "c_sg"], "4 AGs + 4 SGs",
               metric="power")


def affine_area_vs_max_value(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "max_sequence_width", "dims",
               "dimensionality", "Maximum value (max sequence width)", ["c_ag", "c_sg"],
               "4 AGs + 4 SGs", log2=True)


def affine_power_vs_max_value(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "max_sequence_width", "dims",
               "dimensionality", "Maximum value (max sequence width)", ["c_ag", "c_sg"],
               "4 AGs + 4 SGs", log2=True, metric="power")


def _sp_dp_fig(outpath: Path, df: pd.DataFrame, x: str, metric: str, ylabel: str,
               xlabel: str, ticks, xfmt, scale: float, unit: str) -> None:
    """Single- vs dual-port lines of ``y`` (and power ``y2``) vs ``x``: median
    over the other parameters with a min-max bar."""
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    for dp, c, m, name in ((False, SERIES[0], "o", "Single-port SRAM"),
                           (True, SERIES[1], "s", "Dual-port SRAM")):
        g = df[df["dual_port"] == dp]
        for col, ls, lw, ms, lab in (("y", "-", 1.6, 4.5, name), ("y2", "--", 1.1, 3, None)):
            if col not in g.columns:
                continue
            st = g.groupby(x)[col].agg(["median", "min", "max"]) / scale
            ax.errorbar(st.index, st["median"], yerr=[st["median"] - st["min"], st["max"] - st["median"]],
                        color=c, marker=m, linestyle=ls, linewidth=lw, markersize=ms,
                        markeredgecolor="white", markeredgewidth=0.6, capsize=2, elinewidth=0.8,
                        label=lab)
    _log2_axis(ax, ticks, xfmt)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(f"{ylabel} ({unit})")
    ax.set_ylim(bottom=0)
    h, l = ax.get_legend_handles_labels()
    _legend_above(ax, h + _style_handles(metric), l + [s.get_label() for s in _style_handles(metric)],
                  ncol=2)
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


def _memory_port(outpath: Path, metric: str) -> None:
    """MEMORY_EXP: MemoryPort (memory-port muxes, arbiters, interface decoders)
    vs SRAM interface width, single- vs dual-port.

    Port count rises with width in this sweep (it's in the tick labels), and
    capacity barely matters to this logic, so each point is the median over
    capacities with a min-max bar.
    """
    df = _metric(_exp(_load(), "MEMORY_EXP"), ["c_mp"], metric, "MemoryPort " + metric)
    scale, unit = _units(df, metric)
    ports = df.groupby("sram_bw_bits")["in_ports"].first().to_dict()
    _sp_dp_fig(outpath, df, "sram_bw_bits", metric, f"MemoryPort {metric}",
               "SRAM interface width (bits/cycle)", sorted(ports),
               lambda b: f"{int(b)}\n{ports[b]} in, {ports[b]} out", scale, unit)


def memory_port_area_vs_interface_width(ctx, outpath: Path) -> None:
    _memory_port(outpath, "area")


def memory_port_power_vs_interface_width(ctx, outpath: Path) -> None:
    _memory_port(outpath, "power")


# GF12 SRAM macro numbers are under NDA (main_thesis.tex, Storage: "we cannot
# publish specific numbers"). With SRAM_NDA set, every figure whose values
# include the macros (Storage, PUB, the synth->PnR model) plots them relative
# to a reference instead of in µm² / mW. Set False for absolute units.
SRAM_NDA = True
NDA_REF = "fw1_dw16_sc1024_dp_in1_out1"  # the smallest tile: 1 KB dual-port, fw1, 1 in, 1 out
NDA_REF_LBL = "× 1 KB DP"


def _nda(df: pd.DataFrame, metric: str, what: str) -> tuple[float, str]:
    """(divisor, unit) for SRAM-bearing ``y``: NDA_REF's own ``y`` (its active
    power for power, so idle reads against the same reference) when SRAM_NDA."""
    if not SRAM_NDA:
        return _units(df, metric)
    ref = df[df["config"] == NDA_REF]
    if ref.empty:
        raise MissingDataError(f"{what}: SRAM_NDA reference {NDA_REF} not in the plotted configs")
    return float(ref["y"].iloc[0]), NDA_REF_LBL


def _storage(outpath: Path, metric: str) -> None:
    """MEMORY_EXP: Storage (the SRAM block: macros + their wrapper) vs capacity,
    single- vs dual-port.

    Organizations that share a macro area coincide (DP fw1 and fw2 everywhere,
    SP fw2/fw4 from 4 KB up), so instead of one line each, each point is the
    median over fetch widths with a min-max bar.
    """
    df = _metric(_exp(_load(), "MEMORY_EXP"), ["sram"], metric, "Storage " + metric)
    df["kb"] = df["storage_capacity"] / 1024
    scale, unit = _nda(df, metric, "Storage")
    _sp_dp_fig(outpath, df, "kb", metric, f"Storage {metric}", "Capacity (KB)",
               sorted(df["kb"].unique()), lambda v: f"{v:g}", scale, unit)


def storage_area_vs_capacity(ctx, outpath: Path) -> None:
    _storage(outpath, "area")


def storage_power_vs_capacity(ctx, outpath: Path) -> None:
    _storage(outpath, "power")


# ---- Physical Unified Buffer (the Lake spec: controller + Storage) ------------


def _pub_capacity(outpath: Path, metric: str) -> None:
    """MEMORY_EXP: PUB (lake controller + SRAM block) vs capacity, one line per
    memory organization (color = SRAM bits/cycle, dashed = dual-port). Power =
    active."""
    df = _metric(_exp(_load(), "MEMORY_EXP"), ["ctrl", "sram"], metric, "PUB " + metric)
    df["kb"] = df["storage_capacity"] / 1024
    scale, unit = _nda(df, metric, "PUB")
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    groups = sorted(df.groupby(["sram_bw_bits", "dual_port", "vec_width", "in_ports"]),
                    key=lambda kv: (kv[0][1], kv[0][0]))
    for _, g in groups:
        g = g.sort_values("kb")
        r = g.iloc[0]
        kind = "DP" if r["dual_port"] else "SP"
        ax.plot(g["kb"], g["y"] / scale, markersize=4,
                label=f"{kind} fw{int(r['vec_width'])}, {int(r['in_ports'])} in, {int(r['out_ports'])} out",
                **_topo_style(r))
    _log2_axis(ax, sorted(df["kb"].unique()))
    ax.set_xlabel("Capacity (KB)")
    ax.set_ylabel(f"PUB {'area' if metric == 'area' else 'active power'} ({unit})")
    ax.set_ylim(bottom=0)
    _legend_above(ax, ncol=2)
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


def pub_capacity_area(ctx, outpath: Path) -> None:
    _pub_capacity(outpath, "area")


def pub_capacity_power(ctx, outpath: Path) -> None:
    _pub_capacity(outpath, "power")


def _pub_bandwidth(outpath: Path, metric: str) -> None:
    """MEMORY_EXP at 8 KB: PUB split into SRAM block and lake controller,
    single- vs dual-port per SRAM bandwidth. Power: bars = active, tick = idle
    total. Relative to the single-port lowest-bandwidth PUB under SRAM_NDA."""
    df = _exp(_load(), "MEMORY_EXP")
    df = df[df["storage_capacity"] == 8192].copy()
    if df.empty or df["sram_bw_bits"].nunique() < 2:
        raise MissingDataError("need 8 KB MEMORY_EXP configs at several bandwidths")
    pre = "blk_" if metric == "area" else "pw_synth_active_"
    _blocks(df, [pre + "sram", pre + "ctrl"], "PUB " + metric)
    df["y"] = df[pre + "sram"] + df[pre + "ctrl"]
    if metric == "power":
        df["y2"] = _blocks(df, ["pw_synth_idle_sram", "pw_synth_idle_ctrl"], "PUB idle power")
    bws = sorted(df["sram_bw_bits"].unique())
    if SRAM_NDA:
        ref = df[(df["sram_bw_bits"] == bws[0]) & ~df["dual_port"]]
        scale, unit = float(ref["y"].iloc[0]), f"× SP {int(bws[0])}b"
    else:
        scale, unit = _units(df, metric)
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    w, gap = 0.36, 0.04
    ticks, tick_lbls = [], []
    for i, bw in enumerate(bws):
        for j, dp in enumerate((False, True)):
            r = df[(df["sram_bw_bits"] == bw) & (df["dual_port"] == dp)]
            if r.empty:
                continue
            r = r.iloc[0]
            x = i + (j - 0.5) * (w + gap)
            first = i == 0 and j == 0
            ax.bar(x, r[pre + "sram"] / scale, w, color=SERIES[0], edgecolor="white", linewidth=1,
                   label="Storage (SRAM)" if first else None)
            ax.bar(x, r[pre + "ctrl"] / scale, w, bottom=r[pre + "sram"] / scale, color=SERIES[1],
                   edgecolor="white", linewidth=1, label="Lake controller" if first else None)
            if metric == "power":
                ax.scatter(x, r["y2"] / scale, marker="_", s=110, color=INK, linewidth=1.4,
                           zorder=4, label="Idle total" if first else None)
            ticks.append(x)
            tick_lbls.append("DP" if dp else "SP")
        ax.text(i, -0.13, f"{int(bw)}", transform=ax.get_xaxis_transform(),
                ha="center", va="top", fontsize=7.5, color=INK2)
    ax.set_xticks(ticks)
    ax.set_xticklabels(tick_lbls, fontsize=7)
    ax.tick_params(axis="x", length=0)
    ax.set_ylabel(f"PUB {'area' if metric == 'area' else 'active power'} ({unit})")
    ax.set_xlabel("SRAM bandwidth (bits/cycle), 8 KB", labelpad=12)
    ax.set_ylim(0, df["y"].max() / scale * 1.06)
    _legend_above(ax, ncol=3)
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


def pub_bandwidth_area(ctx, outpath: Path) -> None:
    _pub_bandwidth(outpath, "area")


def pub_bandwidth_power(ctx, outpath: Path) -> None:
    _pub_bandwidth(outpath, "power")


# ---- latency-insensitive (RV) ScheduleGenerator ---------------------------------
# The RV controller replaces the per-port static ScheduleGenerators with small
# RV ones plus a comparison network across ports (lake spec.py: each port's SG
# compares against every port of the other direction). "LI ScheduleGenerator"
# = c_sg + c_rvnet of the RV builds.

# lake commit that fixed the comparison network's wrap fixup at the top level
# of power-of-2 dims (2/4/8); RV RTL generated before it is marked hollow.
RVNET_FIX = "8748240d"
_LAKE_REPO = Path(__file__).resolve().parents[2]


def _rvnet_prefix(df: pd.DataFrame) -> pd.Series:
    """True for RV rows whose RTL predates RVNET_FIX in a dim it changes."""
    if "rtl_lake_commit" not in df.columns:
        return pd.Series(False, index=df.index)
    import subprocess
    pre = {}
    for c in df["rtl_lake_commit"].dropna().unique():
        rc = subprocess.run(["git", "-C", str(_LAKE_REPO), "merge-base", "--is-ancestor",
                             RVNET_FIX, c], capture_output=True).returncode
        pre[c] = rc == 1  # 0 = has the fix; anything else (unknown commit) = don't mark
    return (df["mode"].eq("rv") & df["dims"].isin([2, 4, 8])
            & df["rtl_lake_commit"].map(pre).fillna(False).astype(bool))


def _li_ports(outpath: Path, metric: str) -> None:
    """LI ScheduleGenerator vs Lake ports, stacked comparison network + RV SGs:
    every RV config at the default 6 dims and extents (MEMORY/PORT sweeps),
    median with a min-max bar on the total. Power: bars = active, tick = idle."""
    df = _load("rv")
    df = df[(df["dims"] == 6) & df["max_extent"].isna() & df["max_sequence_width"].isna()].copy()
    if df["in_ports"].nunique() < 2:
        raise MissingDataError("need RV tile builds at several port counts")
    pre = "blk_" if metric == "area" else "pw_synth_active_"
    _blocks(df, [pre + "c_rvnet", pre + "c_sg"], "LI ScheduleGenerator " + metric)
    df["y"] = df[pre + "c_rvnet"] + df[pre + "c_sg"]
    if metric == "power":
        df["y2"] = _blocks(df, ["pw_synth_idle_c_rvnet", "pw_synth_idle_c_sg"], "LI SG idle")
    scale, unit = _units(df, metric)
    ports = sorted(df["in_ports"].unique())
    g = df.groupby("in_ports")
    med = g[[pre + "c_rvnet", pre + "c_sg"]].median()
    tot = g["y"].agg(["median", "min", "max"])
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    xs = np.arange(len(ports))
    net, sg = med.loc[ports, pre + "c_rvnet"] / scale, med.loc[ports, pre + "c_sg"] / scale
    ax.bar(xs, net, 0.55, color=SERIES[0], label="Comparison network")
    ax.bar(xs, sg, 0.55, bottom=net, color=SERIES[1], label="RV ScheduleGenerators")
    ax.errorbar(xs, (net + sg).values, yerr=[(tot["median"] - tot["min"]).loc[ports] / scale,
                                             (tot["max"] - tot["median"]).loc[ports] / scale],
                fmt="none", ecolor=MUTED, elinewidth=0.8, capsize=2)
    if metric == "power":
        idle = g["y2"].median().loc[ports] / scale
        ax.scatter(xs, idle, marker="_", s=110, color=INK, linewidth=1.4, zorder=4,
                   label="Idle total")
        for xi, top, v in zip(xs, (tot["max"].loc[ports] / scale).values, idle.values):
            ax.text(xi, top, f"idle {v:.1f}", ha="center", va="bottom", fontsize=6.5, color=INK2)
    ax.set_xticks(xs)
    ax.set_xticklabels([f"{p}, {p}\n(n={g.size()[p]})" for p in ports])
    ax.set_xlabel("Lake ports (in, out)")
    ax.set_ylabel(f"LI SG {'area' if metric == 'area' else 'active power'} ({unit})")
    ax.set_ylim(0, tot["max"].max() / scale * 1.12)
    _legend_above(ax, ncol=2)
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


def li_sched_ports_area(ctx, outpath: Path) -> None:
    _li_ports(outpath, "area")


def li_sched_ports_power(ctx, outpath: Path) -> None:
    _li_ports(outpath, "power")


def li_sched_loc_area(ctx, outpath: Path) -> None:
    """ITERATION_DOMAIN_EXP, RV: LI ScheduleGenerator vs levels of comparison
    (iteration-domain dimensionality), one line per maximum extent."""
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "dims", "max_extent", "maximum extent",
               "Levels of comparison (dimensionality)", ["c_rvnet", "c_sg"],
               "LI SG", mode="rv")


def li_sched_loc_power(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "dims", "max_extent", "maximum extent",
               "Levels of comparison (dimensionality)", ["c_rvnet", "c_sg"],
               "LI SG", mode="rv", metric="power")
