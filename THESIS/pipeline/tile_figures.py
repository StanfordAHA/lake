"""Area figures from the garnet Tile_MemCore lake-spec sweep.

Every generator reads the cached sweep table (``tile_sweep.load()``; ingest
first with ``python3 -m THESIS.pipeline.tile_sweep <sweep.zip>``) and plots
Genus synthesis area. Two groups:

  memtile_*   the whole CGRA memory tile (lakespec + MemCore wrapper + SB/CB
              interconnect) unless noted.
  component   the Ch. 4 Lake component characterization figures (Port,
              IterationDomain, AddressGenerator, MemoryPort, Storage), which
              replace the standalone-lakespec (THESIS_BUILDS) versions. From a
              hierarchy-kept sweep (garnet --flatten-effort 0) each plots its
              own block of the lake controller (tile_sweep BLOCKS, ``blk_*``):
              Port = port_inst, IterationDomain = port_id, affine pattern
              generators = port_ag + port_sg, MemoryPort = memory-port
              muxes/arbiters/decoders, Storage = the SRAM block.

Static runtime mode unless the figure compares modes, and only
``data_width == DATA_WIDTH`` configs (see below). Signature matches
``generators.py``: ``(ctx, outpath)``; ``ctx`` is unused because the tile
sweep is not a THESIS_BUILDS experiment.

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
    return f"{kind} fw{int(r['vec_width'])}, {int(r['in_ports'])}×{int(r['out_ports'])} ports"


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
    fig, ax = plt.subplots(figsize=(FULL_W * 0.6, 2.6))
    w = 0.26
    for k, ((col, lab), colr) in enumerate(zip(parts, SERIES[:3])):
        xs = np.arange(len(ports)) + (k - 1) * w
        ax.bar(xs, med.loc[ports, col] / K, w * 0.92, color=colr, label=lab)
        ax.errorbar(xs, med.loc[ports, col] / K,
                    yerr=[(med - lo).loc[ports, col] / K, (hi - med).loc[ports, col] / K],
                    fmt="none", ecolor=MUTED, elinewidth=0.8, capsize=2)
    ax.set_xticks(range(len(ports)))
    ax.set_xticklabels([f"{p}×{p}" for p in ports])
    ax.set_xlabel("Lake ports (in × out), 16-bit data")
    ax.set_ylabel(AREA_LBL)
    ax.set_ylim(0, hi.values.max() / K * 1.3)
    ax.legend(loc="upper left", ncol=3, fontsize=7, columnspacing=1.0, handlelength=1.2)
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


def memtile_synth_vs_pnr_area(ctx, outpath: Path) -> None:
    """PnR anchors: Innovus signoff std-cell area vs Genus synthesis std-cell area."""
    df = _load(mode=None)
    p = df.dropna(subset=["pnr_std"])
    if len(p) < 3:
        raise MissingDataError("fewer than 3 tile configs reached PnR signoff")
    fig, ax = plt.subplots(figsize=(FULL_W * 0.55, 2.9))
    for mode, c, m in (("static", SERIES[0], "o"), ("rv", SERIES[1], "s")):
        g = p[p["mode"] == mode]
        ax.scatter(g["syn_std"] / K, g["pnr_std"] / K, s=30, color=c, marker=m,
                   edgecolor="white", linewidth=0.7, zorder=3,
                   label=f"{'Static' if mode == 'static' else 'RV'} (n={len(g)})")
    lim = np.array([0, max(p["syn_std"].max(), p["pnr_std"].max()) / K * 1.08])
    ax.plot(lim, lim, color=AXIS, linewidth=1, zorder=1)
    a, b = np.polyfit(p["syn_std"], p["pnr_std"], 1)
    ax.plot(lim, (a * lim * K + b) / K, color=MUTED, linewidth=1, linestyle="--", zorder=2)
    ratio = p["pnr_std"] / p["syn_std"]
    ax.text(0.04, 0.96, f"PnR ≈ {a:.3f} × synth {b:+.0f} µm²\nPnR / synth = {ratio.min():.2f}–{ratio.max():.2f}",
            transform=ax.transAxes, va="top", fontsize=7.5, color=INK2)
    ax.set_xlim(lim)
    ax.set_ylim(lim)
    ax.set_xlabel("Synthesis std-cell area (10³ µm²)")
    ax.set_ylabel("PnR signoff std-cell area (10³ µm²)")
    ax.legend(loc="lower right")
    _ax_style(ax, xgrid=True)
    fig.tight_layout()
    _save(fig, outpath)


# ---- Ch. 4 component characterization (replaces the standalone versions) ------
# Half-width figures for the 0.48\textwidth subfigures in main_thesis.tex. Same
# output paths and sweep axes as the THESIS_BUILDS generators in generators.py.


def _blocks(df: pd.DataFrame, cols: list[str], what: str) -> pd.Series:
    """Sum of per-block area columns; needs a hierarchy-kept sweep."""
    if any(c not in df.columns for c in cols) or df[cols].isna().all().all():
        raise MissingDataError(f"{what}: no per-block areas ({', '.join(cols)}); ingest a "
                               "hierarchy-kept sweep (garnet --flatten-effort 0)")
    return df[cols].sum(axis=1, min_count=1)


def _sweep_fig(outpath: Path, exp: str, x: str, hue: str, hue_name: str, xlabel: str,
               blocks: list[str], ylabel: str, xfmt=lambda v: f"{v:g}", log2: bool = False) -> None:
    """Area of ``blocks`` vs ``x`` for one experiment, one line per ``hue`` value."""
    df = _exp(_load(), exp).dropna(subset=[x, hue]).copy()
    if df[x].nunique() < 2:
        raise MissingDataError(f"{exp}: {x} takes a single value at data_width={DATA_WIDTH}")
    df["y"] = _blocks(df, blocks, ylabel)
    scale = K if df["y"].max() >= 2000 else 1.0
    vals = sorted(df[hue].unique())
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    for v, c in zip(vals, _ramp(len(vals))):
        g = df[df[hue] == v].sort_values(x)
        ax.plot(g[x], g["y"] / scale, color=c, marker="o", markersize=4,
                markeredgecolor="white", markeredgewidth=0.6, label=f"{v:g}")
    ticks = sorted(df[x].unique())
    if log2:
        _log2_axis(ax, ticks, xfmt)
    else:
        ax.set_xticks(ticks)
        ax.set_xticklabels([xfmt(t) for t in ticks])
    ax.set_xlabel(xlabel)
    ax.set_ylabel(f"{ylabel} ({'10³ µm²' if scale == K else 'µm²'})")
    # Few lines: the rising lines leave the lower right empty. Many: a 2-row
    # legend over headroom above the (flatter) lines.
    many = len(vals) > 4
    ax.set_ylim(0, df["y"].max() / scale * (1.45 if many else 1.1))
    ax.legend(title=hue_name, title_fontsize=7, fontsize=7, loc="upper left" if many else "lower right",
              ncol=3 if many else 1, columnspacing=0.8, handlelength=1.4)
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


def port_area_vs_data_width(ctx, outpath: Path) -> None:
    """PORT_EXP: MemCore logic vs port data width, one line per fetch width (vc=2)."""
    df = _exp(_load(), "PORT_EXP")
    if df["data_width"].nunique() < 2:
        raise MissingDataError(
            f"needs PORT_EXP tile builds at several data widths; only {DATA_WIDTH}-bit "
            "configs are used (other widths have broken RTL)")
    _sweep_fig(outpath, "PORT_EXP", "data_width", "vec_width", "fetch width",
               "Port data width (bits)", ["blk_c_port"], "Port area, 4 ports", log2=True)


def port_area_vs_vc(ctx, outpath: Path) -> None:
    """PORT_EXP: Port area (the 4 port_inst blocks) vs vectorization capacity,
    one line per fetch width."""
    _sweep_fig(outpath, "PORT_EXP", "vec_capacity", "vec_width", "fetch width",
               "Vectorization capacity (entries)", ["blk_c_port"], "Port area, 4 ports", log2=True)


def iter_dom_area_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "dims", "max_extent", "maximum extent",
               "Dimensionality", ["blk_c_id"], "IterationDomains, 4")


def iter_dom_area_vs_max_extent(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "max_extent", "dims", "dimensionality",
               "Maximum extent", ["blk_c_id"], "IterationDomains, 4", log2=True)


def affine_area_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "dims", "max_sequence_width",
               "maximum value", "Dimensionality", ["blk_c_ag", "blk_c_sg"], "Pattern generators, 8")


def affine_area_vs_max_value(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "max_sequence_width", "dims",
               "dimensionality", "Maximum value (max sequence width)", ["blk_c_ag", "blk_c_sg"],
               "Pattern generators, 8", log2=True)


def memory_port_area_vs_interface_width(ctx, outpath: Path) -> None:
    """MEMORY_EXP: MemoryPort area (memory-port muxes, arbiters, interface
    decoders) vs SRAM interface width, single- vs dual-port.

    Port count rises with width in this sweep (it's in the tick labels), and
    capacity barely matters to this logic, so each point is the median over
    capacities with a min-max bar.
    """
    df = _exp(_load(), "MEMORY_EXP").copy()
    df["y"] = _blocks(df, ["blk_c_mp"], "MemoryPort area")
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    ports = {}
    for dp, c, m, ls, name in ((False, SERIES[0], "o", "-", "Single-port SRAM"),
                               (True, SERIES[1], "s", "--", "Dual-port SRAM")):
        g = df[df["dual_port"] == dp]
        st = g.groupby("sram_bw_bits")["y"].agg(["median", "min", "max"])
        ports.update(g.groupby("sram_bw_bits")["in_ports"].first().to_dict())
        ax.errorbar(st.index, st["median"], yerr=[st["median"] - st["min"], st["max"] - st["median"]],
                    color=c, marker=m, linestyle=ls, markersize=4.5, markeredgecolor="white",
                    markeredgewidth=0.6, capsize=2, elinewidth=0.8, label=name)
    bws = sorted(ports)
    _log2_axis(ax, bws, lambda b: f"{int(b)}\n{ports[b]}×{ports[b]} ports")
    ax.set_xlabel("SRAM interface width (bits/cycle)")
    ax.set_ylabel("MemoryPort area (µm²)")
    ax.set_ylim(bottom=0)
    ax.legend(loc="upper left")
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


def storage_area_vs_capacity(ctx, outpath: Path) -> None:
    """MEMORY_EXP: Storage area (the SRAM block: macros + their wrapper) vs
    capacity, single- vs dual-port.

    Organizations that share a macro area coincide (DP fw1 and fw2 everywhere,
    SP fw2/fw4 from 4 KB up), so instead of one line each, each point is the
    median over fetch widths with a min-max bar.
    """
    df = _exp(_load(), "MEMORY_EXP").copy()
    df["y"] = _blocks(df, ["blk_sram"], "Storage area") / K
    df["kb"] = df["storage_capacity"] / 1024
    fig, ax = plt.subplots(figsize=(HALF_W, 2.3))
    for dp, c, m, ls, name in ((False, SERIES[0], "o", "-", "Single-port SRAM"),
                               (True, SERIES[1], "s", "--", "Dual-port SRAM")):
        st = df[df["dual_port"] == dp].groupby("kb")["y"].agg(["median", "min", "max"])
        ax.errorbar(st.index, st["median"], yerr=[st["median"] - st["min"], st["max"] - st["median"]],
                    color=c, marker=m, linestyle=ls, markersize=4.5, markeredgecolor="white",
                    markeredgewidth=0.6, capsize=2, elinewidth=0.8, label=name)
    _log2_axis(ax, sorted(df["kb"].unique()))
    ax.set_xlabel("Capacity (KB)")
    ax.set_ylabel("Storage area (10³ µm²)")
    ax.set_ylim(bottom=0)
    ax.legend(loc="upper left")
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)