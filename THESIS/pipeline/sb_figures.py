"""Switch-box figures of the interconnect section (Ch. 4), from ``sb_ppa.load()``: the
static and ready/valid (RV) PE and Memory Tile switch boxes of the final run, by part
(Genus synthesis cell area, µm²; the SB holds no SRAM, so values are absolute).

- fig:interconnect_fifo_replace_ff: a track (pipeline) register as a flip-flop vs a
  depth-2 FIFO, and each switch box before and after that swap.
- fig:interconnect_fork_ready_valid_data: the static and RV switch boxes by part, and the
  fork logic alone (ready reduction, valid gating, core-output loopbacks).
"""

from __future__ import annotations

from pathlib import Path

import matplotlib.pyplot as plt
from matplotlib.patches import Patch
import numpy as np
import pandas as pd

from . import sb_ppa
from .errors import MissingDataError
from .tile_figures import FULL_W, GRID, INK2, MUTED, SERIES, _ax_style, _legend_above, _save

TILES = [("PE", "PE Tile"), ("MEM", "Memory Tile")]
K = 1000.0
FF_C, FIFO_C, REST_C = SERIES[0], SERIES[1], "#c9c8c0"


def _pairs(df: pd.DataFrame, tile: str) -> pd.DataFrame:
    """One row per data width: the static and RV columns side by side (suffixes _s, _r)."""
    d = df[df["tile"] == tile]
    s = d[d["mode"] == "static"].set_index("data_width")
    r = d[d["mode"] == "rv"].set_index("data_width")
    both = s.join(r, lsuffix="_s", rsuffix="_r", how="inner").sort_index()
    if both.empty:
        raise MissingDataError(f"no {tile} data width with both a static and an RV switch box")
    return both


def _load() -> pd.DataFrame:
    df = sb_ppa.load()
    for t, _ in TILES:
        _pairs(df, t)
    return df


def _dw_ticks(ax, dws, labels=None) -> None:
    ax.set_xscale("log", base=2)
    ax.set_xticks(dws)
    ax.set_xticklabels(labels or [str(int(d)) for d in dws])
    ax.minorticks_off()


def interconnect_fifo_replace_ff(ctx, outpath: Path) -> None:
    """(a) One track register, flip-flop (static) vs depth-2 FIFO (RV), by width, PE Tile
    lines and Memory Tile markers; (b, c) each tile's switch box before the swap (static)
    and after it (the static SB with its registers replaced by the RV build's FIFOs)."""
    df = _load()
    fig, axes = plt.subplots(1, 3, figsize=(FULL_W, 2.7), gridspec_kw={"width_ratios": [1.05, 1, 1]})
    ax = axes[0]
    pe = _pairs(df, "PE")
    # the 1-bit tracks first (the same in every build: take the dw16 one, else the first)
    one = pe.loc[16] if 16 in pe.index else pe.iloc[0]
    xs = [1] + list(pe.index)
    for suffix, color, lab in (("_s", FF_C, "Flip-flop"), ("_r", FIFO_C, "Depth-2 FIFO")):
        ys = [one["regs_1b_each" + suffix]] + list(pe["regs_dw_each" + suffix])
        ax.plot(xs, ys, color=color, marker="o", label=lab)
        mem = _pairs(df, "MEM")
        ax.scatter(mem.index, mem["regs_dw_each" + suffix], s=34, facecolor="none", edgecolor=color,
                   linewidth=1.1, zorder=4)
    for x, ff, fifo in zip(xs, [one["regs_1b_each_s"]] + list(pe["regs_dw_each_s"]),
                           [one["regs_1b_each_r"]] + list(pe["regs_dw_each_r"])):
        ax.annotate(f"×{fifo / ff:.1f}", (x, fifo), xytext=(0, 5), textcoords="offset points",
                    ha="center", va="bottom", fontsize=8.5, color=INK2)
    _dw_ticks(ax, xs)
    ax.set_xlabel("Track width (bits)")
    ax.set_ylabel("Area per register (µm²)")
    ax.set_ylim(0, max(pe["regs_dw_each_r"]) * 1.2)
    ax.set_title("(a) One track register", loc="left")
    _ax_style(ax)
    mem_handle = ax.scatter([], [], s=34, facecolor="none", edgecolor=MUTED)

    w = 0.38
    for ax, (tile, name) in zip(axes[1:], TILES):
        p = _pairs(df, tile)
        idx = np.arange(len(p))
        rest = (p["sb_area_s"] - p["regs_s"]) / K
        for j, (regs, color) in enumerate(((p["regs_s"] / K, FF_C), (p["regs_r"] / K, FIFO_C))):
            x = idx + (j - 0.5) * (w + 0.04)
            ax.bar(x, rest, w, color=REST_C, label="Rest of switch box" if j == 0 else None)
            ax.bar(x, regs, w, bottom=rest, color=color)
            if j == 1:
                for xi, before, after in zip(x, p["sb_area_s"], rest * K + regs * K):
                    ax.text(xi, after / K, f"+{(after / before - 1) * 100:.0f}%", ha="center",
                            va="bottom", fontsize=8.5, color=INK2)
        ax.set_xticks(idx)
        ax.set_xticklabels([str(int(d)) for d in p.index])
        ax.set_xlabel("Data width (bits)", fontsize=9.5)
        ax.set_title(f"({'b' if tile == 'PE' else 'c'}) {name} switch box", loc="left")
        _ax_style(ax)
    axes[1].set_ylabel("Area (10³ µm²)")
    top = max(a.get_ylim()[1] for a in axes[1:]) * 1.06
    for a in axes[1:]:
        a.set_ylim(0, top)
    handles, labels = axes[0].get_legend_handles_labels()
    h2, l2 = axes[1].get_legend_handles_labels()
    fig.tight_layout(w_pad=0.8)
    _legend_above(axes[0], handles + [mem_handle] + h2, labels + ["Memory Tile"] + l2)
    _save(fig, outpath)


# (column, label, color) stacked bottom to top; "rest" = everything else in the SB
FORK_PARTS = [("regs", "Track registers", SERIES[0]), ("muxes", "Muxes", SERIES[2]),
              ("config", "Configuration", MUTED), ("fork_area", "Fork logic", SERIES[1]),
              ("rest", "Other", "#c9c8c0")]
FORK_SPLIT = [("fan_in", "Ready reduction", SERIES[1]), ("valid", "Valid gating", "#f4a582"),
              ("loopbacks", "Core-output loopbacks", "#fbd3c0")]


def _fork_parts(df: pd.DataFrame) -> pd.DataFrame:
    """SB parts per row, with the fork logic's tile-level gates (valid ANDs into the
    connection boxes, loopbacks) added to the switch box they belong to."""
    d = df.copy()
    d["total"] = d["sb_area"] + d["tile_valid_ands"] + d["loopbacks"]
    d["rest"] = d["total"] - d["regs"] - d["muxes"] - d["config"] - d["fork_area"]
    d["valid"] = d["valid_ands"] + d["tile_valid_ands"]
    return d


def interconnect_fork_ready_valid_data(ctx, outpath: Path) -> None:
    """(a, b) Each tile's switch box, static vs RV, by part; (c) the RV fork logic alone
    by part and data width, both tiles."""
    df = _fork_parts(_load())
    fig, axes = plt.subplots(1, 3, figsize=(FULL_W, 2.8))
    w = 0.38
    for ax, (tile, name) in zip(axes[:2], TILES):
        d = df[df["tile"] == tile]
        dws = sorted(set(d[d["mode"] == "static"]["data_width"]) & set(d[d["mode"] == "rv"]["data_width"]))
        for j, mode in enumerate(("static", "rv")):
            m = d[d["mode"] == mode].set_index("data_width").loc[dws]
            x = np.arange(len(dws)) + (j - 0.5) * (w + 0.04)
            bottom = np.zeros(len(dws))
            for col, lab, color in FORK_PARTS:
                v = m[col].to_numpy(dtype=float) / K
                ax.bar(x, v, w, bottom=bottom, color=color, label=lab if (tile == "PE" and j == 1) else None)
                bottom += v
            if mode == "rv":
                for xi, top, share in zip(x, bottom, m["fork_area"] / m["total"] * 100):
                    ax.text(xi, top, f"{share:.0f}%", ha="center", va="bottom", fontsize=8.5,
                            color=SERIES[1])
        ax.set_xticks(np.arange(len(dws)))
        ax.set_xticklabels([str(dw) for dw in dws])
        ax.text(0.02, 0.98, "left: static\nright: RV", transform=ax.transAxes, ha="left", va="top",
                fontsize=8.5, color=INK2)
        ax.set_xlabel("Data width (bits)", fontsize=9.5)
        ax.set_title(f"({'a' if tile == 'PE' else 'b'}) {name}", loc="left")
        _ax_style(ax)
    axes[0].set_ylabel("Switch box area (10³ µm²)")
    top = max(a.get_ylim()[1] for a in axes[:2])
    for a in axes[:2]:
        a.set_ylim(0, top)

    ax = axes[2]
    rv = df[df["mode"] == "rv"]
    dws = sorted(rv["data_width"].unique())
    for j, (tile, name) in enumerate(TILES):
        m = rv[rv["tile"] == tile].set_index("data_width").reindex(dws)
        x = np.arange(len(dws)) + (j - 0.5) * (w + 0.04)
        bottom = np.zeros(len(dws))
        for col, lab, color in FORK_SPLIT:
            v = m[col].to_numpy(dtype=float)
            ax.bar(x, v, w, bottom=bottom, color=color, edgecolor="white", linewidth=0.6,
                   hatch="////" if tile == "MEM" else None, label=lab if j == 0 else None)
            bottom += v
    ax.set_xticks(np.arange(len(dws)))
    ax.set_xticklabels([str(dw) for dw in dws])
    ax.set_xlabel("Data width (bits)", fontsize=9.5)
    ax.set_ylabel("Fork logic area (µm²)")
    ax.set_title("(c) RV fork logic", loc="left")
    _ax_style(ax)
    handles, labels = axes[0].get_legend_handles_labels()
    h3, l3 = ax.get_legend_handles_labels()
    tiles = [Patch(facecolor="white", edgecolor=MUTED, label="PE Tile"),
             Patch(facecolor="white", edgecolor=MUTED, hatch="////", label="Memory Tile")]
    fig.tight_layout(w_pad=0.8)
    _legend_above(axes[0], handles + h3 + tiles, labels + l3 + ["PE Tile (c)", "Memory Tile (c)"])
    _save(fig, outpath)
