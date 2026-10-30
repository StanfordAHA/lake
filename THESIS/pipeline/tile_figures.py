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

import weakref
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
# Figures are drawn at their printed width, so font sizes are the printed sizes (body
# text is 10 pt). All text is bold and as large as fits, with tight margins (user
# 2026-10-28).
FULL_W = 6.5  # inches, \textwidth (1 in margins on letter)
HALF_W = 3.12  # inches, a 0.48\textwidth subfigure

plt.rcParams.update({
    "font.size": 10, "axes.labelsize": 10.5, "axes.titlesize": 10.5, "legend.fontsize": 9.5,
    "legend.title_fontsize": 9.5, "xtick.labelsize": 10, "ytick.labelsize": 10,
    "font.weight": "bold", "axes.labelweight": "bold", "axes.titleweight": "bold",
    "mathtext.default": "bf", "lines.linewidth": 1.8, "lines.markersize": 5.5,
    "axes.edgecolor": AXIS, "axes.labelcolor": INK, "xtick.color": INK, "ytick.color": INK,
    "text.color": INK, "pdf.fonttype": 42, "ps.fonttype": 42, "legend.frameon": False,
    "savefig.pad_inches": 0.02,
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
    ax.text(-0.14, 1.02, s, transform=ax.transAxes, fontsize=11, fontweight="bold", va="bottom")


def _wrap(lab, s: str, room: float, size) -> bool:
    """Break label ``lab`` (text ``s``) into the fewest lines no longer than ``room``
    pixels (``size(text)`` measures one): before " (" or after ", " where that does
    it, else at any space."""
    def split(s, at_space):
        parts, i = [], 0
        for j in range(1, len(s)):
            if (s[j] == " " if at_space else
                    s.startswith(" (", j) or s.startswith(", ", j - 1)):
                parts.append(s[i:j].rstrip())
                i = j + (1 if s[j] == " " else 0)
        return parts + [s[i:]]

    def fill(parts):
        lines = [parts[0]]
        for p in parts[1:]:
            if size(lines[-1] + " " + p) <= room:
                lines[-1] += " " + p
            else:
                lines.append(p)
        return lines

    lines = fill(split(s, False))
    if any(size(ln) > room for ln in lines):
        lines = fill(split(s, True))
    lab.set_text("\n".join(lines))
    return len(lines) > 1


def _wrap_labels(fig) -> bool:
    """Wrap each y label longer than its axis, and each x label that would stick out
    past the outermost axes (savefig's tight bbox crops it there). True if any
    label changed."""
    r = fig.canvas.get_renderer()
    changed = False
    boxes = [ax.get_window_extent(r) for ax in fig.axes]
    left, right = min(b.x0 for b in boxes), max(b.x1 for b in boxes)
    for ax, box in zip(fig.axes, boxes):
        mid = (box.x0 + box.x1) / 2
        for lab, room in ((ax.yaxis.label, box.height),
                          (ax.xaxis.label, 2 * min(mid - left, right - mid))):
            s = lab.get_text()
            if not s or "\n" in s:
                continue
            ext = lab.get_window_extent(r)
            if max(ext.width, ext.height) <= room:
                continue

            def size(t, lab=lab):
                lab.set_text(t)
                e = lab.get_window_extent(r)
                return max(e.width, e.height)
            if not _wrap(lab, s, room, size):
                lab.set_text(s)
            changed = True
    return changed


def _save(fig, outpath: Path):
    outpath.parent.mkdir(parents=True, exist_ok=True)
    if _wrap_labels(fig):
        fig.tight_layout()
    fig.savefig(outpath, bbox_inches="tight", dpi=200 if outpath.suffix == ".png" else "figure")
    plt.close(fig)


def _load(mode: str | None = "static") -> pd.DataFrame:
    df = tile_sweep.load()
    df = df[df["data_width"] == DATA_WIDTH]
    if mode:
        df = df[df["mode"] == mode]
    if df.empty:
        raise MissingDataError(f"tile sweep has no {mode} configs")
    return df


def _balanced(df: pd.DataFrame) -> np.ndarray:
    """Ports matched to the SRAM: fetch width = in + out ports (single-port, one
    access per cycle) or = the ports per direction (dual-port). Otherwise the SRAM
    is wider than the ports can use, or the ports wait on it (user 2026-10-28)."""
    fw, n_in, n_out = df["vec_width"], df["in_ports"], df["out_ports"]
    return np.where(df["dual_port"], (fw == n_in) & (fw == n_out), fw == n_in + n_out)


def _exp(df: pd.DataFrame, exp: str) -> pd.DataFrame:
    sub = df[df["experiments"].fillna("").str.contains(exp)]
    if exp == "MEMORY_EXP":  # compared at equal SRAM bandwidth: balanced organizations only
        sub = sub[_balanced(sub)]
    if sub.empty:
        raise MissingDataError(f"tile sweep has no {exp} configs")
    return sub


def _pow2_fmt(v) -> str:
    """16384 -> "16K": a multiple of 1024 from 1024 up, else the plain value."""
    return f"{v / 1024:g}K" if v >= 1024 and v % 1024 == 0 else f"{v:g}"


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
    """e.g. "DP 16b (fw1), 1 in, 1 out": the SRAM's word width, so a dual-port
    organization doesn't read as its bandwidth (2 x the word)."""
    kind = "DP" if r["dual_port"] else "SP"
    fw = int(r["vec_width"])
    return (f"{kind} {fw * int(r.get('data_width', DATA_WIDTH))}b (fw{fw}), "
            f"{int(r['in_ports'])} in, {int(r['out_ports'])} out")


def _topo_style(r):
    return dict(color=_BW_COLOR[int(r["sram_bw_bits"])],
                linestyle="--" if r["dual_port"] else "-",
                marker="s" if r["dual_port"] else "o",
                markeredgecolor="white", markeredgewidth=0.8)


# ---- views: raw values, % change per x step, or both ---------------------------
# A sweep figure shows its raw values ("raw"), the % change between neighboring
# x values ("pct"), or both ("both": raw on top, % change in a panel below on the
# same x axis). The registry picks per figure (Entry.view); generators read it
# as ctx.view, and only the ones marked @_viewable honor it. The % panel puts
# each step's change at the middle of the step, next to the change in x itself
# (gray, "y ∝ x"): a series above that line grows faster than proportionally to
# x, and an affine series (fixed overhead + linear) sits below it.
VIEWS = ("raw", "pct", "both")
VIEWABLE: set = set()
PCT_LBL = "Change per step (%)"
_REF_STYLE = dict(color=MUTED, linestyle=":", linewidth=1.1, marker="x", markersize=3.5)
_REF_OFF_SCALE: "weakref.WeakKeyDictionary" = weakref.WeakKeyDictionary()  # pax -> legend label


def _viewable(fn):
    VIEWABLE.add(fn)
    return fn


def _view(ctx) -> str:
    v = getattr(ctx, "view", "raw")
    if v not in VIEWS:
        raise ValueError(f"unknown view {v!r}; expected one of {VIEWS}")
    return v


def _view_axes(view: str, figsize):
    """(fig, ax, pax): ``ax`` for the raw values, ``pax`` for the % change; the
    one the view leaves out is None. "both" stacks them, sharing x."""
    if view == "raw":
        fig, ax = plt.subplots(figsize=figsize)
        return fig, ax, None
    if view == "pct":
        fig, pax = plt.subplots(figsize=figsize)
        return fig, None, pax
    fig, (ax, pax) = plt.subplots(2, 1, figsize=(figsize[0], figsize[1] * 1.3), sharex=True,
                                  gridspec_kw={"height_ratios": [2.2, 1]})
    ax.tick_params(labelbottom=False)
    return fig, ax, pax


def _steps(pos, y, geo: bool = False):
    """Midpoint of each step between consecutive ``pos`` (geometric on a log
    axis) and y's % change over it; NaN where y is missing or not positive."""
    pos, y = np.asarray(pos, float), np.asarray(y, float)
    keep = ~np.isnan(y)
    pos, y = pos[keep], y[keep]
    mid = np.sqrt(pos[:-1] * pos[1:]) if geo else (pos[:-1] + pos[1:]) / 2
    with np.errstate(divide="ignore", invalid="ignore"):
        pct = np.where(y[:-1] > 0, (y[1:] / y[:-1] - 1) * 100, np.nan)
    return mid, pct


def _pct_line(pax, pos, y, geo: bool = False, **style):
    mid, pct = _steps(pos, y, geo)
    style = {"marker": "o", "markersize": 3.5, "markeredgecolor": "white",
             "markeredgewidth": 0.5, "linewidth": 1.2, **style}
    return pax.plot(mid, pct, **style)[0]


def _pct_axis(pax, xvals, pos=None, geo: bool = False):
    """The y ∝ x reference (x's own % change at the same steps), zero line,
    y range, label and grid; call after the series are drawn. ``pos`` = plot
    positions of ``xvals`` (indices for bars). The range fits the series, minus
    outliers (beyond 4x their median |change| and the reference, e.g. a step
    up from a near-zero value), and takes in the reference only if that
    doesn't squash them (at most 3x their largest change). Whatever falls
    outside is noted at the panel edge in its own color; a reference that is
    off scale at every step gets one note."""
    pts = [(x, y, l.get_color()) for l in pax.get_lines()
           for x, y in zip(l.get_xdata(), l.get_ydata()) if np.isfinite(y)]
    vals = np.array([y for _, y, _ in pts] or [0.0])
    mid, ref = _steps(xvals if pos is None else pos, xvals, geo)
    cap = max(4 * np.median(np.abs(vals)), ref.max(initial=0.0), 30.0)
    inl = vals[np.abs(vals) <= cap]
    lo, hi = min(0.0, inl.min(initial=0.0)), max(0.0, inl.max(initial=0.0))
    if ref.max(initial=0.0) <= 3 * max(abs(lo), abs(hi), 5.0):
        hi = max(hi, ref.max(initial=0.0))
    pad = max(0.12 * (hi - lo), 2.0)
    off_ref = [(x, y) for x, y in zip(mid, ref) if y > hi + pad]
    off = [(x, y, c) for x, y, c in pts if y > hi + pad or y < lo - pad]
    if len(off_ref) < len(ref):
        off += [(x, y, MUTED) for x, y in off_ref]
    room_hi = 2.2 * pad if off_ref or any(y > hi for _, y, _ in off) else 0.0  # for the notes
    room_lo = 2.2 * pad if any(y < lo for _, y, _ in off) else 0.0
    pax.set_ylim(lo - (pad if lo < 0 else pad * 0.5) - room_lo, hi + pad + room_hi)
    pax.plot(mid, np.where(ref > hi + pad, np.nan, ref), zorder=1, **_REF_STYLE)
    bottom, top = pax.get_ylim()
    for x, y, c in off:
        up = y > hi
        pax.text(x, top if up else bottom, f"{'↑' if up else '↓'}{y:+.0f}%", ha="center",
                 va="top" if up else "bottom", fontsize=8.5, color=c)
    if off_ref and len(off_ref) == len(ref):  # noted in the legend's y ∝ x entry
        r0, r1 = min(ref), max(ref)
        rng = f"{r0:+.0f}%" if round(r0) == round(r1) else f"{r0:+.0f}% to {r1:+.0f}%"
        _REF_OFF_SCALE[pax] = f"y ∝ x, off scale:\n{rng} per step"
    pax.axhline(0, color=AXIS, linewidth=0.9, zorder=1)
    if pos is not None:  # categories (bars): half a category of margin, as the bars have
        pax.set_xlim(-0.5, len(xvals) - 0.5)
    if len(pax.figure.axes) > 1:  # under a raw panel: a title, so the y labels can't collide
        pax.set_title(PCT_LBL, loc="left", fontsize=9.5, color=INK2, pad=6)
    else:
        pax.set_ylabel(PCT_LBL)
    _ax_style(pax)


def _ref_handle():
    return Line2D([], [], label="y ∝ x", **_REF_STYLE)


def _view_legend(top, pax, handles=None, labels=None, **kw):
    """_legend_above on the top axis, plus the y ∝ x entry when there's a %
    panel. Defaults to the top axis's own labeled artists."""
    if handles is None:
        handles, labels = top.get_legend_handles_labels()
    elif labels is None:
        labels = [h.get_label() for h in handles]
    if pax is not None:
        handles, labels = list(handles) + [_ref_handle()], list(labels) + ["y ∝ x"]
    _legend_above(top, handles, labels, **kw)


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
        ax.set_title(lab, loc="left", fontsize=10.5, color=INK2)
        ax.set_ylim(bottom=0)
        _ax_style(ax)
    h, l = axes[0].get_legend_handles_labels()
    fig.legend(h, l, loc="upper center", ncol=3, bbox_to_anchor=(0.5, 0.0),
               title="Solid = single-port, dashed = dual-port; color = SRAM bits/cycle (32, 64, 128)",
               title_fontsize=9.5)
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
                    va="bottom", fontsize=9, color=INK2)
            ticks.append(x)
            tick_lbls.append("DP" if dp else "SP")
        # bandwidth group label under the SP/DP tick labels
        ax.text(i, -0.11, f"{int(bw)} b/cycle", transform=ax.get_xaxis_transform(),
                ha="center", va="top", fontsize=9.5, color=INK2)
    ax.set_xticks(ticks)
    ax.set_xticklabels(tick_lbls, fontsize=9.5)
    ax.tick_params(axis="x", length=0)
    ax.set_ylabel(AREA_LBL)
    ax.set_xlabel("SRAM bandwidth (8 KB capacity, 16-bit data)", labelpad=16)
    ax.set_ylim(0, df["syn_cell"].max() / K * 1.12)
    ax.legend(loc="upper left")
    _ax_style(ax)
    fig.tight_layout()
    _save(fig, outpath)


@_viewable
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
    fig, ax, pax = _view_axes(_view(ctx), (HALF_W, 2.3))
    w = 0.26
    for k, ((col, lab), colr) in enumerate(zip(parts, SERIES[:3])):
        if ax is not None:
            xs = np.arange(len(ports)) + (k - 1) * w
            ax.bar(xs, med.loc[ports, col] / K, w * 0.92, color=colr, label=lab)
            ax.errorbar(xs, med.loc[ports, col] / K,
                        yerr=[(med - lo).loc[ports, col] / K, (hi - med).loc[ports, col] / K],
                        fmt="none", ecolor=MUTED, elinewidth=0.8, capsize=2)
        if pax is not None:
            _pct_line(pax, range(len(ports)), med.loc[ports, col], color=colr,
                      label=lab if ax is None else None)
    if pax is not None:
        _pct_axis(pax, ports, pos=range(len(ports)))
    bot = pax if pax is not None else ax
    bot.set_xticks(range(len(ports)))
    n = df.groupby("in_ports").size()
    bot.set_xticklabels([f"{p}, {p}\n(n={n[p]})" for p in ports])
    bot.set_xlabel(f"Lake ports (in, out), {DATA_WIDTH}-bit data")
    if ax is not None:
        ax.set_ylabel(AREA_LBL)
        ax.set_ylim(0, hi.values.max() / K * 1.08)
        _ax_style(ax)
    _view_legend(ax if ax is not None else pax, pax)
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
                    va="top", fontsize=9.5, color=INK2)
        _log2_axis(ax, sorted(df["bits"].unique()), fmt=lambda v: f"{int(v)}")
        ax.set_xlabel(f"Buffer bits per port (fw × {DATA_WIDTH} × vc)")
        ax.set_ylabel(AREA_LBL)
        ax.set_title(lab, loc="left", fontsize=10.5, color=INK2)
        _ax_style(ax, xgrid=True)
    h, l = axes[0].get_legend_handles_labels()
    fig.legend(h, l, loc="lower center", ncol=len(fws), bbox_to_anchor=(0.5, -0.08),
               title="fetch width", title_fontsize=9.5)
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
        ax.set_title(lab, loc="left", fontsize=10.5, color=INK2)
        ax.legend(title=name, title_fontsize=9.5, loc="upper left")
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
    fig, ax = plt.subplots(figsize=(FULL_W * 0.75, 0.4 * len(order) + 0.8))
    rng = np.random.default_rng(0)
    for i, gname in enumerate(order):
        g = j[j["group"] == gname]["pct"]
        y = i + rng.uniform(-0.18, 0.18, len(g))
        ax.scatter(g, y, s=16, color=SERIES[0], alpha=0.85, edgecolor="white", linewidth=0.5, zorder=3)
        ax.plot([g.median()] * 2, [i - 0.3, i + 0.3], color=INK, linewidth=1.4, zorder=4)
        ax.annotate(f"median {g.median():+.0f}%\n(n={len(g)})", (1, i),  # a column right of the axes
                    xycoords=("axes fraction", "data"), xytext=(6, 0), textcoords="offset points",
                    va="center", fontsize=9.5, color=INK2, linespacing=1.0)
    ax.axvline(0, color=AXIS, linewidth=1)
    ax.set_yticks(range(len(order)))
    ax.set_yticklabels([g.replace(" (", "\n(") for g in order], linespacing=1.0)
    ax.invert_yaxis()
    ax.set_xlabel("RV std-cell area vs static (%)")
    ax.set_xlim(min(-5, j["pct"].min() - 2), j["pct"].max() + 3)
    _ax_style(ax, ygrid=False, xgrid=True)
    fig.tight_layout()
    _save(fig, outpath)


@_viewable
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
    fig, ax, pax = _view_axes(_view(ctx), (HALF_W, 2.3))
    w = 0.32
    for k, ((b, lab), colr) in enumerate(zip(parts, SERIES[:2])):
        a = f"pw_synth_active_{b}"
        if ax is not None:
            xs = np.arange(len(ports)) + (k - 0.5) * w
            ax.bar(xs, med.loc[ports, a] / UW, w * 0.92, color=colr, label=lab)
            ax.errorbar(xs, med.loc[ports, a] / UW,
                        yerr=[(med - lo).loc[ports, a] / UW, (hi - med).loc[ports, a] / UW],
                        fmt="none", ecolor=MUTED, elinewidth=0.8, capsize=2)
            ax.scatter(xs, imed.loc[ports, f"pw_synth_idle_{b}"] / UW, marker="_", s=90, color=INK,
                       linewidth=1.4, zorder=4, label="Idle" if k == 0 else None)
        if pax is not None:
            _pct_line(pax, range(len(ports)), med.loc[ports, a], color=colr,
                      label=lab if ax is None else None)
            _pct_line(pax, range(len(ports)), imed.loc[ports, f"pw_synth_idle_{b}"], color=colr,
                      linestyle="--", linewidth=0.9, markersize=2.6)
    if pax is not None:
        _pct_axis(pax, ports, pos=range(len(ports)))
    bot = pax if pax is not None else ax
    bot.set_xticks(range(len(ports)))
    bot.set_xticklabels([f"{p}, {p}\n(n={g.size()[p]})" for p in ports])
    bot.set_xlabel(f"Lake ports (in, out), {DATA_WIDTH}-bit data")
    if ax is not None:
        ax.set_ylabel("Active power (µW)")
        ax.set_ylim(0, hi.values.max() / UW * 1.08)
        _ax_style(ax)
    top = ax if ax is not None else pax
    h, l = top.get_legend_handles_labels()
    if pax is not None:  # the % panel's dashed lines are the idle power
        h, l = h + [_style_handles("power")[1]], l + ["idle (step)"]
    _view_legend(top, pax, h, l)
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
    fig, axes = plt.subplots(1, 3, figsize=(FULL_W, 2.6), sharey=True)
    nums = []
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
                if x >= 5:
                    nums.append((ax, x, ax.text(left + x / 2, i, f"{x:.0f}", ha="center",
                                                va="center", fontsize=9, color="white")))
                left += x
        ax.set_xlim(0, 100)
        ax.set_xticks([0, 50, 100])
        ax.set_title(title, loc="left", fontsize=10.5, color=INK2)
        _ax_style(ax, ygrid=False)
    axes[0].set_yticks(range(len(rows)))
    axes[0].set_yticklabels(["RV" if c.endswith("_rv") else "static" for _, c in rows])
    axes[0].invert_yaxis()
    axes[1].set_xlabel("Share of tile (%)")  # once, under the middle panel
    fig.tight_layout(w_pad=0.6)
    r = fig.canvas.get_renderer()
    tick_w = axes[0].get_window_extent(r).x0 - min(
        t.get_window_extent(r).x0 for t in axes[0].get_yticklabels())
    groups = []
    for k, (_, lab) in enumerate(_BREAKDOWN_CONFIGS):  # each geometry once, beside its pair
        parts = lab.split(", ")
        groups.append(axes[0].annotate(
            ", ".join(parts[:2]) + "\n" + ", ".join(parts[2:]), (0, 2 * k + 0.5),
            xycoords=("axes fraction", "data"), xytext=(-(tick_w + 10) * 72 / fig.dpi, 0),
            textcoords="offset points", ha="right", va="center", fontsize=9.5))
    group_w = max(g.get_window_extent(r).width for g in groups)
    fig.tight_layout(rect=((group_w + 10) / fig.bbox.width, 0, 1, 1), w_pad=0.6)
    for ax, x, t in nums:  # a segment's share only where the number fits inside it
        if t.get_window_extent(r).width + 2 > ax.transData.transform((x, 0))[0] - \
                ax.transData.transform((0, 0))[0]:
            t.remove()
    h, l = axes[0].get_legend_handles_labels()
    _fit_legend(fig, list(zip(h, l)), None, 0.0, below=True)
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
    fig, axes = plt.subplots(2, 1, figsize=(FULL_W, 4.4), sharex=True)
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
                ax.text(x, bottom, note, ha="center", va="bottom", fontsize=9, color=INK2)
                ticks.append(x)
                labels.append("S" if mode == "Static" else "RV")
            if var == "idle":
                ax.annotate(lab, (i, 0), xycoords=("data", "axes fraction"), xytext=(0, -19),
                            textcoords="offset points", ha="center", va="top", fontsize=9.5,
                            color=INK2)
        ax.set_xticks(ticks)
        ax.set_xticklabels(labels)
        ax.tick_params(axis="x", length=0)
        ax.set_ylim(0, ax.get_ylim()[1] * 1.12)
        _ax_style(ax)
    axes[0].set_ylabel("Non-SRAM active energy (pJ/bit)")
    axes[0].set_title("(a) Active per bit (top: SRAM share)" if SRAM_NDA else
                      "(a) Active per bit (top: SRAM pJ/bit)", loc="left", fontsize=9.5, color=INK2)
    axes[1].set_ylabel("Non-SRAM idle power (mW)")
    axes[1].set_title("(b) Idle (top: total)", loc="left", fontsize=9.5, color=INK2)
    fig.tight_layout()
    fig.subplots_adjust(bottom=0.2, hspace=0.25)
    h, l = axes[0].get_legend_handles_labels()
    _fit_legend(fig, list(zip(h, l)), None, 0.0, below=True)
    _save(fig, outpath)


# ---- synthesis -> PnR model: fit set vs held-out set ---------------------------
# The sweep's 12 PnR builds: 8 fit (4 geometries x static/RV, pnr_role "fit")
# and 4 held out (pnr_role "validate"), chosen by structure before any data
# existed. Each runtime mode gets its own linear fit pnr = a * synth + b, as in
# garnet sweep_specs.py (correlation_fit.csv / memtile_power_fit.csv).

_MODE_STYLE = {"static": (SERIES[0], "Static"), "rv": (SERIES[1], "RV")}


def _model_panel(ax, rax, p: pd.DataFrame, xcol: str, ycol: str, norm: float, unit: str,
                 note: str, one_line: bool) -> float:
    """Scatter (fit = dots, held out = stars) with each mode's fit line on
    ``ax``; prediction error per build on ``rax``. Each fit's equation (R² on the
    same line if ``one_line``) and the held-out error (``note`` formats its mean
    and max) are stacked above ``ax``; returns that block's height in points. The
    series are labeled for one legend over the figure."""
    lo, hi = p[xcol].min() / norm, p[xcol].max() / norm
    pad = (hi - lo) * 0.06
    errs, lines = [], []
    for mode, (c, name) in _MODE_STYLE.items():
        g = p[p["mode"] == mode]
        f, v = g[g["pnr_role"] == "fit"], g[g["pnr_role"] == "validate"]
        if len(f) < 3:
            raise MissingDataError(f"fewer than 3 {mode} fit builds reached PnR")
        a, b = np.polyfit(f[xcol] / norm, f[ycol] / norm, 1)
        r2 = np.corrcoef(f[xcol], f[ycol])[0, 1] ** 2
        # solid over the fit range, dotted where held-out builds extrapolate
        fx = np.array([f[xcol].min(), f[xcol].max()]) / norm
        ax.plot(fx, a * fx + b, color=c, linewidth=1.1, zorder=2, label=f"{name} fit")
        eq = f"{name} fit: $y = {a:.3f}x {'+' if b >= 0 else '-'} {abs(b):.{_dec(b)}f}$"
        lines += [(f"{eq}, $R^2 = {r2:.5f}$", c)] if one_line else \
            [(eq, c), (f"{' ' * 4}$R^2 = {r2:.5f}$", c)]
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
    lines.append((note.format(np.mean(errs), max(errs)), INK2))
    for k, (text, c) in enumerate(reversed(lines)):
        ax.annotate(text, (0, 1), xycoords="axes fraction", xytext=(0, 4 + 12.5 * k),
                    textcoords="offset points", ha="left", va="bottom", fontsize=9, color=c)
    return 4 + 12.5 * len(lines)


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
    _model_panel(ax, rax, p, "tile_synth_cell_area", "tile_pnr_total_area", norm, unit,
                 "Held-out |error|: mean {:.2f}%, max {:.2f}%", one_line=True)
    ax.set_ylabel(f"PnR total area ({unit})")
    rax.set_xlabel(f"Synthesis cell area ({unit})")
    fig.align_ylabels([ax, rax])
    _legend_above(ax)
    _save(fig, outpath)


def memtile_synth_vs_pnr_power(ctx, outpath: Path) -> None:
    """Synthesis -> PnR power model, idle and active, same layout as the area
    figure (fit builds = dots, held-out builds = stars)."""
    p = _pnr_builds()
    fig, axes = plt.subplots(2, 2, figsize=(FULL_W, 4.6), sharex="col",
                             gridspec_kw={"height_ratios": [3, 1.15], "hspace": 0.08,
                                          "wspace": 0.3})
    for k, (var, title) in enumerate((("idle", "(a) Idle"), ("active", "(b) Active"))):
        x, y = f"synth_{var}_total_power", f"pnr_{var}_total_power"
        q = p.dropna(subset=[x, y])
        norm, unit = _model_norm(q, x)
        ax, rax = axes[0, k], axes[1, k]
        block = _model_panel(ax, rax, q, x, y, norm, unit,
                             "Held-out |error|: mean {:.1f}%, max {:.1f}%", one_line=False)
        ax.set_title(title, loc="left", fontsize=10.5, color=INK2, pad=block + 2)
        ax.set_ylabel(f"PnR power ({unit})")
        rax.set_xlabel(f"Synthesis power ({unit})")
    if axes[0, 0].get_legend_handles_labels()[1] != axes[0, 1].get_legend_handles_labels()[1]:
        raise MissingDataError("idle and active PnR power have different builds; one legend "
                               "can't label both panels")
    fig.align_ylabels(axes[:, 0])
    _legend_above(axes[0, 0])
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


def _fit_legend(fig, entries: list, title, y: float, below: bool = False):
    """A legend above the figure (lower left at figure coords (0, y)), or below it
    (upper left at (0, y)), in the fewest rows that fit the figure's width, its
    columns as even as those rows allow."""
    r, width = fig.canvas.get_renderer(), fig.bbox.width
    kw = dict(title=title, loc="upper left" if below else "lower left", bbox_to_anchor=(0, y),
              columnspacing=0.7,
              handlelength=1.3, handletextpad=0.4, borderaxespad=0.1, alignment="left")
    nc = len(entries)
    while nc > 1:
        leg = fig.legend(*zip(*entries), ncol=nc, **kw)
        fits = leg.get_window_extent(r).width <= width
        leg.remove()
        if fits:
            break
        nc -= 1
    rows = -(-len(entries) // nc)
    return fig.legend(*zip(*entries), ncol=-(-len(entries) // rows), **kw)


def _legend_above(ax, handles=None, labels=None, title=None):
    """Legend above the whole figure: the data series (with ``title``) over the gray
    key (active/idle, y ∝ x, ...), each in as many columns as fit the width."""
    if handles is None:
        handles, labels = ax.get_legend_handles_labels()
    elif labels is None:
        labels = [h.get_label() for h in handles]
    fig = ax.figure
    note = next((v for k, v in _REF_OFF_SCALE.items() if k.figure is fig), None)
    labels = [note if note and lab == "y ∝ x" else lab for lab in labels]
    order = sorted(range(len(labels)), key=lambda i: (labels[i].startswith("y ∝ x"),
                                                      labels[i].lower().startswith("idle")))
    entries = [(handles[i], labels[i]) for i in order]
    is_key = [getattr(h, "get_color", lambda: None)() == MUTED for h, _ in entries]
    fig.tight_layout()
    y = max(1.0, fig.get_tightbbox().y1 / fig.get_figheight())  # over anything poking out
    for group, ttl in (([e for e, k in zip(entries, is_key) if k], None),
                       ([e for e, k in zip(entries, is_key) if not k], title)):
        if group:
            leg = _fit_legend(fig, group, ttl, y)
            y += leg.get_window_extent(fig.canvas.get_renderer()).height / fig.bbox.height


def _sweep_fig(outpath: Path, exp: str, x: str, hue: str, hue_name: str, xlabel: str,
               blocks: list[str], ylabel: str, xfmt=_pow2_fmt, log2: bool = False,
               metric: str = "area", mode: str = "static", view: str = "raw") -> None:
    """``metric`` of ``blocks`` vs ``x`` for one experiment, one line per ``hue``."""
    df = _exp(_load(mode), exp).dropna(subset=[x, hue])
    if df[x].nunique() < 2:
        raise MissingDataError(f"{exp}: {x} takes a single value at data_width={DATA_WIDTH}")
    df = _metric(df, blocks, metric, ylabel)
    df["prefix"] = _rvnet_prefix(df) if mode == "rv" else False
    scale, unit = _units(df, metric)
    vals = sorted(df[hue].unique())
    fig, ax, pax = _view_axes(view, (HALF_W, 2.3))
    handles = []
    for v, c in zip(vals, _ramp(len(vals))):
        g = df[df[hue] == v].sort_values(x)
        style = dict(color=c, marker="o", markersize=4, markeredgecolor="white",
                     markeredgewidth=0.6, label=f"{v:g}")
        if ax is not None:
            h, = ax.plot(g[x], g["y"] / scale, **style)
            if metric == "power":
                ax.plot(g[x], g["y2"] / scale, color=c, linestyle="--", linewidth=1.1, marker="o",
                        markersize=2.6, markeredgewidth=0)
            pre = g[g["prefix"]]
            for col in ["y"] + (["y2"] if metric == "power" else []):
                ax.scatter(pre[x], pre[col] / scale, s=18 if col == "y" else 9, facecolor="white",
                           edgecolor=c, linewidth=0.9, zorder=4)
        else:
            h = Line2D([], [], **style)
        handles.append(h)
        if pax is not None:
            _pct_line(pax, g[x], g["y"], log2, color=c)
            if metric == "power":
                _pct_line(pax, g[x], g["y2"], log2, color=c, linestyle="--", linewidth=0.9,
                          markersize=2.6)
    ticks = sorted(df[x].unique())
    if pax is not None:
        _pct_axis(pax, ticks, geo=log2)
    for a in (ax, pax):
        if a is None:
            continue
        if log2:
            _log2_axis(a, ticks, xfmt)
        else:
            a.set_xticks(ticks)
            a.set_xticklabels([xfmt(t) for t in ticks])
    (pax if pax is not None else ax).set_xlabel(xlabel)
    if ax is not None:
        ax.set_ylabel(f"{'Area' if metric == 'area' else 'Power'}, {ylabel} ({unit})")
        ax.set_ylim(0, df["y"].max() / scale * 1.08)
        _ax_style(ax)
    _view_legend(ax if ax is not None else pax, pax,
                 handles + _style_handles(metric, ax is not None and df["prefix"].any()),
                 title=hue_name)
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
               log2=True, metric=metric, view=_view(ctx))


@_viewable
def port_area_vs_data_width(ctx, outpath: Path) -> None:
    """PORT_EXP: Port area vs port data width, one line per fetch width (vc=2)."""
    _port_data_width(ctx, outpath, "area")


@_viewable
def port_power_vs_data_width(ctx, outpath: Path) -> None:
    _port_data_width(ctx, outpath, "power")


@_viewable
def port_area_vs_vc(ctx, outpath: Path) -> None:
    """PORT_EXP: Port area (the 4 port_inst blocks) vs vectorization capacity,
    one line per fetch width."""
    _sweep_fig(outpath, "PORT_EXP", "vec_capacity", "vec_width", "fetch width",
               "Vectorization capacity (entries)", ["c_port"], "4 Ports", log2=True, view=_view(ctx))


@_viewable
def port_power_vs_vc(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "PORT_EXP", "vec_capacity", "vec_width", "fetch width",
               "Vectorization capacity (entries)", ["c_port"], "4 Ports", log2=True,
               metric="power", view=_view(ctx))


@_viewable
def iter_dom_area_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "dims", "max_extent", "maximum extent",
               "Dimensionality", ["c_id"], "4 IterationDomains", view=_view(ctx))


@_viewable
def iter_dom_power_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "dims", "max_extent", "maximum extent",
               "Dimensionality", ["c_id"], "4 IterationDomains", metric="power", view=_view(ctx))


@_viewable
def iter_dom_area_vs_max_extent(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "max_extent", "dims", "dimensionality",
               "Maximum extent", ["c_id"], "4 IterationDomains", log2=True, view=_view(ctx))


@_viewable
def iter_dom_power_vs_max_extent(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "max_extent", "dims", "dimensionality",
               "Maximum extent", ["c_id"], "4 IterationDomains", log2=True, metric="power", view=_view(ctx))


@_viewable
def affine_area_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "dims", "max_sequence_width",
               "maximum value", "Dimensionality", ["c_ag", "c_sg"], "4 AGs + 4 SGs", view=_view(ctx))


@_viewable
def affine_power_vs_dim(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "dims", "max_sequence_width",
               "maximum value", "Dimensionality", ["c_ag", "c_sg"], "4 AGs + 4 SGs",
               metric="power", view=_view(ctx))


@_viewable
def affine_area_vs_max_value(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "max_sequence_width", "dims",
               "dimensionality", "Maximum value (max sequence width)", ["c_ag", "c_sg"],
               "4 AGs + 4 SGs", log2=True, view=_view(ctx))


@_viewable
def affine_power_vs_max_value(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "AFFINE_PATTERN_GENERATOR_EXP", "max_sequence_width", "dims",
               "dimensionality", "Maximum value (max sequence width)", ["c_ag", "c_sg"],
               "4 AGs + 4 SGs", log2=True, metric="power", view=_view(ctx))


def _sp_dp_fig(outpath: Path, df: pd.DataFrame, x: str, metric: str, ylabel: str,
               xlabel: str, ticks, xfmt, scale: float, unit: str, view: str = "raw") -> None:
    """Single- vs dual-port lines of ``y`` (and power ``y2``) vs ``x``: median
    over the other parameters with a min-max bar."""
    fig, ax, pax = _view_axes(view, (HALF_W, 2.3))
    for dp, c, m, name in ((False, SERIES[0], "o", "Single-port SRAM"),
                           (True, SERIES[1], "s", "Dual-port SRAM")):
        g = df[df["dual_port"] == dp]
        for col, ls, lw, ms, lab in (("y", "-", 1.6, 4.5, name), ("y2", "--", 1.1, 3, None)):
            if col not in g.columns:
                continue
            st = g.groupby(x)[col].agg(["median", "min", "max"]) / scale
            if ax is not None:
                ax.errorbar(st.index, st["median"],
                            yerr=[st["median"] - st["min"], st["max"] - st["median"]],
                            color=c, marker=m, linestyle=ls, linewidth=lw, markersize=ms,
                            markeredgecolor="white", markeredgewidth=0.6, capsize=2, elinewidth=0.8,
                            label=lab)
            if pax is not None:
                _pct_line(pax, st.index, st["median"], True, color=c, marker=m, linestyle=ls,
                          linewidth=lw * 0.75, markersize=ms * 0.8,
                          label=lab if ax is None else None)
    if pax is not None:
        _pct_axis(pax, ticks, geo=True)
    for a in (ax, pax):
        if a is not None:
            _log2_axis(a, ticks, xfmt)
    (pax if pax is not None else ax).set_xlabel(xlabel)
    if ax is not None:
        ax.set_ylabel(f"{ylabel} ({unit})")
        ax.set_ylim(bottom=0)
        _ax_style(ax)
    top = ax if ax is not None else pax
    h, l = top.get_legend_handles_labels()
    _view_legend(top, pax, h + _style_handles(metric),
                 l + [s.get_label() for s in _style_handles(metric)])
    fig.tight_layout()
    _save(fig, outpath)


def _memory_port(outpath: Path, metric: str, view: str = "raw") -> None:
    """MEMORY_EXP: MemoryPort (memory-port muxes, arbiters, interface decoders)
    vs SRAM bandwidth (read + write bits per cycle), single- vs dual-port. At one
    bandwidth the dual-port SRAM is half as wide (32 b/cycle = SP 32b or DP 16b).

    Port count rises with bandwidth in this sweep (it's in the tick labels), and
    capacity barely matters to this logic, so each point is the median over
    capacities with a min-max bar.
    """
    df = _metric(_exp(_load(), "MEMORY_EXP"), ["c_mp"], metric, "MemoryPort " + metric)
    scale, unit = _units(df, metric)
    ports = df.groupby("sram_bw_bits")["in_ports"].first().to_dict()
    _sp_dp_fig(outpath, df, "sram_bw_bits", metric, f"MemoryPort {metric}",
               "SRAM bandwidth, read + write (bits/cycle)", sorted(ports),
               lambda b: f"{int(b)}\n{ports[b]} in\n{ports[b]} out", scale, unit, view)


@_viewable
def memory_port_area_vs_interface_width(ctx, outpath: Path) -> None:
    _memory_port(outpath, "area", _view(ctx))


@_viewable
def memory_port_power_vs_interface_width(ctx, outpath: Path) -> None:
    _memory_port(outpath, "power", _view(ctx))


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


def _storage(outpath: Path, metric: str, view: str = "raw") -> None:
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
               sorted(df["kb"].unique()), lambda v: f"{v:g}", scale, unit, view)


@_viewable
def storage_area_vs_capacity(ctx, outpath: Path) -> None:
    _storage(outpath, "area", _view(ctx))


@_viewable
def storage_power_vs_capacity(ctx, outpath: Path) -> None:
    _storage(outpath, "power", _view(ctx))


# ---- Physical Unified Buffer (the Lake spec: controller + Storage) ------------


def _pub_capacity(outpath: Path, metric: str, view: str = "raw") -> None:
    """MEMORY_EXP: PUB (lake controller + SRAM block) vs capacity, one line per
    memory organization (color = SRAM bits/cycle, dashed = dual-port). Power =
    active."""
    df = _metric(_exp(_load(), "MEMORY_EXP"), ["ctrl", "sram"], metric, "PUB " + metric)
    df["kb"] = df["storage_capacity"] / 1024
    scale, unit = _nda(df, metric, "PUB")
    fig, ax, pax = _view_axes(view, (HALF_W, 2.3))
    groups = sorted(df.groupby(["sram_bw_bits", "dual_port", "vec_width", "in_ports"]),
                    key=lambda kv: (kv[0][1], kv[0][0]))
    for _, g in groups:
        g = g.sort_values("kb")
        r = g.iloc[0]
        lab = _topo_label(r)
        if ax is not None:
            ax.plot(g["kb"], g["y"] / scale, markersize=4, label=lab, **_topo_style(r))
        if pax is not None:
            _pct_line(pax, g["kb"], g["y"], True, **{**_topo_style(r), "markersize": 3.2,
                                                     "markeredgewidth": 0.5, "linewidth": 1.1},
                      label=lab if ax is None else None)
    kbs = sorted(df["kb"].unique())
    if pax is not None:
        _pct_axis(pax, kbs, geo=True)
    for a in (ax, pax):
        if a is not None:
            _log2_axis(a, kbs)
    (pax if pax is not None else ax).set_xlabel("Capacity (KB)")
    if ax is not None:
        ax.set_ylabel(f"PUB {'area' if metric == 'area' else 'active power'} ({unit})")
        ax.set_ylim(bottom=0)
        _ax_style(ax)
    _view_legend(ax if ax is not None else pax, pax)
    fig.tight_layout()
    _save(fig, outpath)


@_viewable
def pub_capacity_area(ctx, outpath: Path) -> None:
    _pub_capacity(outpath, "area", _view(ctx))


@_viewable
def pub_capacity_power(ctx, outpath: Path) -> None:
    _pub_capacity(outpath, "power", _view(ctx))


def _pub_bandwidth(outpath: Path, metric: str, view: str = "raw") -> None:
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
    fig, ax, pax = _view_axes(view, (HALF_W, 2.3))
    bot = pax if pax is not None else ax
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
            if ax is not None:
                ax.bar(x, r[pre + "sram"] / scale, w, color=SERIES[0], edgecolor="white",
                       linewidth=1, label="Storage (SRAM)" if first else None)
                ax.bar(x, r[pre + "ctrl"] / scale, w, bottom=r[pre + "sram"] / scale,
                       color=SERIES[1], edgecolor="white", linewidth=1,
                       label="Lake controller" if first else None)
                if metric == "power":
                    ax.scatter(x, r["y2"] / scale, marker="_", s=110, color=INK, linewidth=1.4,
                               zorder=4, label="Idle total" if first else None)
            ticks.append(x)
            tick_lbls.append("DP" if dp else "SP")
        if pax is None:
            bot.text(i, -0.13, f"{int(bw)}", transform=bot.get_xaxis_transform(),
                     ha="center", va="top", fontsize=9.5, color=INK2)
        else:  # the % panel is short, so offset by points rather than axes height
            bot.annotate(f"{int(bw)}", (i, 0), xycoords=bot.get_xaxis_transform(),
                         xytext=(0, -13), textcoords="offset points", ha="center", va="top",
                         fontsize=9.5, color=INK2)
    step_handles = []
    if pax is not None:  # SP and DP totals, step by step along the bandwidth groups
        for dp, c, m, name in ((False, SERIES[6], "o", "SP total (step)"),
                               (True, SERIES[4], "s", "DP total (step)")):
            s = (df[df["dual_port"] == dp].drop_duplicates("sram_bw_bits")  # = the bars' r.iloc[0]
                 .set_index("sram_bw_bits").reindex(bws))
            step_handles.append(_pct_line(pax, range(len(bws)), s["y"], color=c, marker=m,
                                          label=name))
            if metric == "power":
                _pct_line(pax, range(len(bws)), s["y2"], color=c, marker=m, linestyle="--",
                          linewidth=0.9, markersize=2.6)
        _pct_axis(pax, bws, pos=range(len(bws)))
    bot.set_xticks(ticks)
    bot.set_xticklabels(tick_lbls, fontsize=9)
    bot.tick_params(axis="x", length=0)
    bot.set_xlabel("SRAM bandwidth (bits/cycle), 8 KB", labelpad=12)
    if ax is not None:
        ax.set_ylabel(f"PUB {'area' if metric == 'area' else 'active power'} ({unit})")
        ax.set_ylim(0, df["y"].max() / scale * 1.06)
        _ax_style(ax)
    top = ax if ax is not None else pax
    h, l = (ax.get_legend_handles_labels() if ax is not None else ([], []))
    h, l = h + step_handles, l + [s.get_label() for s in step_handles]
    if pax is not None and metric == "power":
        h, l = h + [_style_handles("power")[1]], l + ["idle (step)"]
    _view_legend(top, pax, h, l)
    fig.tight_layout()
    _save(fig, outpath)


@_viewable
def pub_bandwidth_area(ctx, outpath: Path) -> None:
    _pub_bandwidth(outpath, "area", _view(ctx))


@_viewable
def pub_bandwidth_power(ctx, outpath: Path) -> None:
    _pub_bandwidth(outpath, "power", _view(ctx))


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


def _li_ports(outpath: Path, metric: str, view: str = "raw") -> None:
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
    fig, ax, pax = _view_axes(view, (HALF_W, 2.3))
    xs = np.arange(len(ports))
    net, sg = med.loc[ports, pre + "c_rvnet"] / scale, med.loc[ports, pre + "c_sg"] / scale
    if metric == "power":
        idle = g["y2"].median().loc[ports] / scale
    if ax is not None:
        ax.bar(xs, net, 0.55, color=SERIES[0], label="Comparison network")
        ax.bar(xs, sg, 0.55, bottom=net, color=SERIES[1], label="RV ScheduleGenerators")
        ax.errorbar(xs, (net + sg).values, yerr=[(tot["median"] - tot["min"]).loc[ports] / scale,
                                                 (tot["max"] - tot["median"]).loc[ports] / scale],
                    fmt="none", ecolor=MUTED, elinewidth=0.8, capsize=2)
        if metric == "power":
            ax.scatter(xs, idle, marker="_", s=110, color=INK, linewidth=1.4, zorder=4,
                       label="Idle total")
            for xi, top, v in zip(xs, (tot["max"].loc[ports] / scale).values, idle.values):
                ax.text(xi, top, f"idle {v:.1f}", ha="center", va="bottom", fontsize=8.5,
                        color=INK2)
    step_handles = []
    if pax is not None:  # each part and the stacked total (the bar top), step by step
        for y, c, name in ((net, SERIES[0], "Comparison network"),
                           (sg, SERIES[1], "RV ScheduleGenerators"),
                           (net + sg, INK, "Total (step)")):
            step_handles.append(_pct_line(pax, xs, y, color=c, label=name))
        if metric == "power":
            step_handles.append(_pct_line(pax, xs, idle, color=INK, linestyle="--", linewidth=0.9,
                                          markersize=2.6, label="idle (step)"))
        _pct_axis(pax, ports, pos=xs)
    bot = pax if pax is not None else ax
    bot.set_xticks(xs)
    bot.set_xticklabels([f"{p}, {p}\n(n={g.size()[p]})" for p in ports])
    bot.set_xlabel("Lake ports (in, out)")
    if ax is not None:
        ax.set_ylabel(f"LI SG {'area' if metric == 'area' else 'active power'} ({unit})")
        ax.set_ylim(0, tot["max"].max() / scale * 1.12)
        _ax_style(ax)
    h, l = ax.get_legend_handles_labels() if ax is not None else ([], [])
    for s in step_handles:  # the % panel reuses the bar colors; add only what's new
        if s.get_label() not in l:
            h, l = h + [s], l + [s.get_label()]
    _view_legend(ax if ax is not None else pax, pax, h, l)
    fig.tight_layout()
    _save(fig, outpath)


@_viewable
def li_sched_ports_area(ctx, outpath: Path) -> None:
    _li_ports(outpath, "area", _view(ctx))


@_viewable
def li_sched_ports_power(ctx, outpath: Path) -> None:
    _li_ports(outpath, "power", _view(ctx))


@_viewable
def li_sched_loc_area(ctx, outpath: Path) -> None:
    """ITERATION_DOMAIN_EXP, RV: LI ScheduleGenerator vs levels of comparison
    (iteration-domain dimensionality), one line per maximum extent."""
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "dims", "max_extent", "maximum extent",
               "Levels of comparison (dimensionality)", ["c_rvnet", "c_sg"],
               "LI SG", mode="rv", view=_view(ctx))


@_viewable
def li_sched_loc_power(ctx, outpath: Path) -> None:
    _sweep_fig(outpath, "ITERATION_DOMAIN_EXP", "dims", "max_extent", "maximum extent",
               "Levels of comparison (dimensionality)", ["c_rvnet", "c_sg"],
               "LI SG", mode="rv", metric="power", view=_view(ctx))


# ---- PE tile pitch (fig:pe_tile_height) ----------------------------------------
# PnR area of each fabric's PE and Memory Tile (THESIS/data/pe_tile/thesis_tiles.csv:
# fabric, tile = pe|mem, area_um2, ...). The PE tile is the square unit tile; a Memory
# Tile spans k unit heights, the fewest whose width (area / k) stays within
# MEM_MAX_WIDTH unit tiles.
PE_TILE_CSV = Path(__file__).resolve().parents[1] / "data" / "pe_tile" / "thesis_tiles.csv"
MEM_MAX_WIDTH = 1.5
MAX_HEIGHT = 4


def mem_tile_height(rel_area: float) -> int:
    """Unit heights a Memory Tile of `rel_area` PE tiles spans."""
    return max(1, int(np.ceil(rel_area / MEM_MAX_WIDTH - 1e-9)))


def pe_tile_pitch(ctx, outpath: Path) -> None:
    if not PE_TILE_CSV.is_file():
        raise MissingDataError(f"no PE/Memory Tile PnR areas at {PE_TILE_CSV}")
    d = pd.read_csv(PE_TILE_CSV)
    fabrics = [f for f in ("static", "rv") if {"pe", "mem"} <= set(d.loc[d["fabric"] == f, "tile"])]
    if not fabrics:
        raise MissingDataError(f"{PE_TILE_CSV}: no fabric with both a PE and a Memory Tile")
    fig, ax = plt.subplots(figsize=(FULL_W, 2.5))
    x, gap = 0.0, 0.5
    for f in fabrics:
        color = _MODE_STYLE[f][0]
        pe = float(d.loc[(d["fabric"] == f) & (d["tile"] == "pe"), "area_um2"].iloc[0])
        mem = float(d.loc[(d["fabric"] == f) & (d["tile"] == "mem"), "area_um2"].iloc[0])
        rel = mem / pe
        k = mem_tile_height(rel)
        if k > MAX_HEIGHT:
            raise MissingDataError(f"{f} Memory Tile is {rel:.2f} PE tiles: more than {MAX_HEIGHT} unit heights")
        w = rel / k
        ax.add_patch(plt.Rectangle((x, 0), 1, 1, facecolor=color, alpha=0.35, edgecolor=color, lw=1.2))
        ax.text(x + 0.5, 0.5, "PE\n1.00", ha="center", va="center", fontsize=9.5)
        mx = x + 1 + gap
        ax.add_patch(plt.Rectangle((mx, 0), MEM_MAX_WIDTH, k, fill=False, edgecolor=MUTED, lw=0.8, ls=(0, (3, 2))))
        ax.add_patch(plt.Rectangle((mx, 0), w, k, facecolor=color, alpha=0.6, edgecolor=color, lw=1.2))
        ax.text(mx + w / 2, k / 2, f"Memory\nTile\n{rel:.2f}", ha="center", va="center", fontsize=9.5)
        ax.text(mx + MEM_MAX_WIDTH, k + 0.08, f"{k}x height", ha="right", va="bottom", fontsize=9.5, color=INK2)
        ax.text(x + (1 + gap + MEM_MAX_WIDTH) / 2, -0.32, _MODE_STYLE[f][1] + " fabric",
                ha="center", va="top", fontsize=11)
        x = mx + MEM_MAX_WIDTH + 1.2
    top = max(2, int(np.ceil(ax.dataLim.y1)))
    for y in range(1, top + 1):
        ax.axhline(y, color=GRID, lw=0.6, zorder=0)
    ax.set_xlim(-0.2, x - 1.0)
    ax.set_ylim(-0.75, top + 0.45)
    ax.set_aspect("equal")
    ax.axis("off")
    ax.text(x - 1.0, -0.62, f"Areas in PE tiles (PnR). Dashed: the slot a Memory Tile may fill, "
            f"{MEM_MAX_WIDTH:g} unit tiles wide.", ha="right", va="top", fontsize=9, color=MUTED)
    _save(fig, outpath)
