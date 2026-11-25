"""Real generators for figures + tables backed by actual data.

Each function takes ``(ctx, outpath)`` where ``ctx`` bundles the loaded
builds DataFrame + config (``top_builds`` path), and writes ``outpath``.
Raise ``MissingDataError`` when required data isn't in the DataFrame so
the orchestrator swaps in a BOGUS placeholder.

Reuses ``ASPLOS_EXP/plot_power_area.py`` styling conventions (log-x when
the sweep spans >2 decades; one line per hue-group) but does *not* depend
on that module — the plot code here is small enough to inline.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from itertools import cycle
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

from . import apps_query, tables
from .errors import MissingDataError
from .ingest import symlink_builds


@dataclass
class GenContext:
    """Shared state passed to every generator."""

    df: pd.DataFrame
    top_builds: Path
    # "raw" | "pct" | "both" (registry Entry.view); only tile_figures.VIEWABLE
    # generators read it
    view: str = "raw"


# ---- helpers ---------------------------------------------------------------


def _slice(df: pd.DataFrame, experiment: str, y: str) -> pd.DataFrame:
    if experiment not in set(df["experiment"].dropna()):
        raise MissingDataError(f"no builds for experiment {experiment} in DataFrame")
    edf = df[df["experiment"] == experiment].dropna(subset=[y]).copy()
    if edf.empty:
        raise MissingDataError(f"experiment {experiment} present but {y} column all-null")
    return edf


def _sweep_lineplot(
    edf: pd.DataFrame,
    x: str,
    y: str,
    hue_cols: list[str],
    title: str,
    xlabel: str,
    ylabel: str,
    outpath: Path,
) -> None:
    if x not in edf.columns:
        raise MissingDataError(f"sweep column {x} not in DataFrame")
    edf = edf.dropna(subset=[x, y])
    if edf.empty:
        raise MissingDataError(f"no non-null rows for {y} vs {x}")

    hue_cols = [c for c in hue_cols if c in edf.columns and edf[c].nunique(dropna=True) > 1]

    fig, ax = plt.subplots(figsize=(6.0, 4.0))
    markers = cycle(["o", "s", "^", "D", "v", "P", "X", "*", "<", ">"])
    if hue_cols:
        for key, gdf in edf.groupby(hue_cols, dropna=False, sort=True):
            if not isinstance(key, tuple):
                key = (key,)
            gdf = gdf.sort_values(x)
            label_parts = []
            for k, v in zip(hue_cols, key):
                if pd.isna(v):
                    continue
                if isinstance(v, float) and v.is_integer():
                    v = int(v)
                label_parts.append(f"{k}={v}")
            ax.plot(gdf[x], gdf[y], marker=next(markers), label=", ".join(label_parts) or "all")
        ax.legend(fontsize=7, loc="best", title=", ".join(hue_cols))
    else:
        gdf = edf.sort_values(x)
        ax.plot(gdf[x], gdf[y], marker="o")

    xvals = edf[x].dropna()
    if (xvals > 0).all() and xvals.size and xvals.max() / max(xvals.min(), 1e-12) > 100:
        ax.set_xscale("log")

    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.grid(True, alpha=0.3)

    outpath.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(outpath, dpi=150)
    plt.close(fig)


def _memport_faceted_plot(
    edf: pd.DataFrame, y: str, title: str, ylabel: str, outpath: Path,
) -> None:
    """MEMORY_EXP y vs fetch_width: one panel per port count, one line per capacity.

    fw and port count co-vary in this sweep, so a single axis mixes port
    configs. Faceting on (inp, outp) holds ports fixed inside each panel;
    capacity keeps the same color/marker across panels, and the shared
    legend sits outside the axes so it never covers data.
    """
    edf = edf.dropna(subset=["fw", y])
    if edf.empty:
        raise MissingDataError(f"no non-null rows for {y} vs fw")

    line_cols = ["storage_cap"]
    if "data_width" in edf.columns and edf["data_width"].nunique(dropna=True) > 1:
        line_cols.append("data_width")
    def as_key(k):
        return k if isinstance(k, tuple) else (k,)

    line_keys = sorted(as_key(k) for k in edf.groupby(line_cols).groups.keys())
    colors = plt.get_cmap("tab10")
    markers = ["o", "s", "^", "D", "v", "P", "X", "*", "<", ">"]
    style = {k: (colors(i % 10), markers[i % len(markers)]) for i, k in enumerate(line_keys)}

    panels = sorted(edf.groupby(["inp", "outp"]).groups.keys())
    fws = sorted(edf["fw"].unique())
    fig, axes = plt.subplots(1, len(panels), figsize=(3.0 * len(panels) + 1.5, 3.4),
                             sharex=True, sharey=True, squeeze=False)
    handles: dict = {}
    for ax, (inp, outp) in zip(axes[0], panels):
        pdf = edf[(edf["inp"] == inp) & (edf["outp"] == outp)]
        for key, gdf in pdf.groupby(line_cols, sort=True):
            key = as_key(key)
            color, marker = style[key]
            gdf = gdf.sort_values("fw")
            (line,) = ax.plot(gdf["fw"], gdf[y], color=color, marker=marker)
            handles.setdefault(key, (line, ", ".join(
                f"{int(v)}" if c == "storage_cap" else f"{c}={int(v)}"
                for c, v in zip(line_cols, key))))
        ax.set_title(f"{int(inp)} in / {int(outp)} out port{'s' if inp > 1 else ''}", fontsize=9)
        ax.set_xscale("log", base=2)
        ax.set_xticks(fws)
        ax.set_xticklabels([str(int(f)) for f in fws])
        ax.minorticks_off()
        ax.set_xlabel("fetch_width")
        ax.grid(True, alpha=0.3)
    axes[0][0].set_ylabel(ylabel)

    ordered = [handles[k] for k in line_keys if k in handles]
    fig.legend([h for h, _ in ordered], [lbl for _, lbl in ordered],
               loc="center left", bbox_to_anchor=(1.0, 0.5), fontsize=8,
               title="storage_cap (bytes)" if len(line_cols) == 1 else ", ".join(line_cols))
    fig.suptitle(title)
    outpath.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(outpath, dpi=150, bbox_inches="tight")
    plt.close(fig)


# ---- figure generators -----------------------------------------------------


def port_area_vs_data_width(ctx: GenContext, outpath: Path) -> None:
    """PORT_EXP: synth area vs data_width, hue by fw/vc."""
    symlink_builds("port_characterization", ctx.top_builds / "PORT_EXP")
    edf = _slice(ctx.df, "PORT_EXP", "synth_total_area_um2")
    _sweep_lineplot(
        edf, x="data_width", y="synth_total_area_um2",
        hue_cols=["fw", "vc"],
        title="Port area vs interface width",
        xlabel="data_width (bits)", ylabel="synth area (µm²)",
        outpath=outpath,
    )


def port_area_vs_vc(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("port_characterization", ctx.top_builds / "PORT_EXP")
    edf = _slice(ctx.df, "PORT_EXP", "synth_total_area_um2")
    _sweep_lineplot(
        edf, x="vc", y="synth_total_area_um2",
        hue_cols=["data_width", "fw"],
        title="Port area vs vectorization buffering",
        xlabel="vc (entries)", ylabel="synth area (µm²)",
        outpath=outpath,
    )


def port_power_vs_data_width(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("port_characterization", ctx.top_builds / "PORT_EXP")
    edf = _slice(ctx.df, "PORT_EXP", "synth_power_w")
    _sweep_lineplot(
        edf, x="data_width", y="synth_power_w",
        hue_cols=["fw", "vc"],
        title="Port power vs interface width",
        xlabel="data_width (bits)", ylabel="synth power (W)",
        outpath=outpath,
    )


def port_power_vs_vc(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("port_characterization", ctx.top_builds / "PORT_EXP")
    edf = _slice(ctx.df, "PORT_EXP", "synth_power_w")
    _sweep_lineplot(
        edf, x="vc", y="synth_power_w",
        hue_cols=["data_width", "fw"],
        title="Port power vs vectorization buffering",
        xlabel="vc (entries)", ylabel="synth power (W)",
        outpath=outpath,
    )


def iter_dom_area_vs_dim(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("iteration_domain", ctx.top_builds / "ITERATION_DOMAIN_EXP")
    edf = _slice(ctx.df, "ITERATION_DOMAIN_EXP", "synth_total_area_um2")
    _sweep_lineplot(
        edf, x="dim", y="synth_total_area_um2", hue_cols=["me"],
        title="IterationDomain area vs dimensionality",
        xlabel="dim", ylabel="synth area (µm²)", outpath=outpath,
    )


def iter_dom_power_vs_dim(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("iteration_domain", ctx.top_builds / "ITERATION_DOMAIN_EXP")
    edf = _slice(ctx.df, "ITERATION_DOMAIN_EXP", "synth_power_w")
    _sweep_lineplot(
        edf, x="dim", y="synth_power_w", hue_cols=["me"],
        title="IterationDomain power vs dimensionality",
        xlabel="dim", ylabel="synth power (W)", outpath=outpath,
    )


def iter_dom_area_vs_max_extent(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("iteration_domain", ctx.top_builds / "ITERATION_DOMAIN_EXP")
    edf = _slice(ctx.df, "ITERATION_DOMAIN_EXP", "synth_total_area_um2")
    _sweep_lineplot(
        edf, x="me", y="synth_total_area_um2", hue_cols=["dim"],
        title="IterationDomain area vs max extent",
        xlabel="me (max extent)", ylabel="synth area (µm²)", outpath=outpath,
    )


def iter_dom_power_vs_max_extent(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("iteration_domain", ctx.top_builds / "ITERATION_DOMAIN_EXP")
    edf = _slice(ctx.df, "ITERATION_DOMAIN_EXP", "synth_power_w")
    _sweep_lineplot(
        edf, x="me", y="synth_power_w", hue_cols=["dim"],
        title="IterationDomain power vs max extent",
        xlabel="me (max extent)", ylabel="synth power (W)", outpath=outpath,
    )


def affine_area_vs_dim(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("affine_pattern_generator", ctx.top_builds / "AFFINE_PATTERN_GENERATOR_EXP")
    edf = _slice(ctx.df, "AFFINE_PATTERN_GENERATOR_EXP", "synth_total_area_um2")
    _sweep_lineplot(
        edf, x="dim", y="synth_total_area_um2", hue_cols=["msw"],
        title="Affine PG area vs dimensionality",
        xlabel="dim", ylabel="synth area (µm²)", outpath=outpath,
    )


def affine_power_vs_dim(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("affine_pattern_generator", ctx.top_builds / "AFFINE_PATTERN_GENERATOR_EXP")
    edf = _slice(ctx.df, "AFFINE_PATTERN_GENERATOR_EXP", "synth_power_w")
    _sweep_lineplot(
        edf, x="dim", y="synth_power_w", hue_cols=["msw"],
        title="Affine PG power vs dimensionality",
        xlabel="dim", ylabel="synth power (W)", outpath=outpath,
    )


def affine_area_vs_max_value(ctx: GenContext, outpath: Path) -> None:
    """AFFINE: maximum stride/offset word width (msw) sweep."""
    symlink_builds("affine_pattern_generator", ctx.top_builds / "AFFINE_PATTERN_GENERATOR_EXP")
    edf = _slice(ctx.df, "AFFINE_PATTERN_GENERATOR_EXP", "synth_total_area_um2")
    _sweep_lineplot(
        edf, x="msw", y="synth_total_area_um2", hue_cols=["dim"],
        title="Affine PG area vs max value width (msw)",
        xlabel="msw (bits)", ylabel="synth area (µm²)", outpath=outpath,
    )


def affine_power_vs_max_value(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("affine_pattern_generator", ctx.top_builds / "AFFINE_PATTERN_GENERATOR_EXP")
    edf = _slice(ctx.df, "AFFINE_PATTERN_GENERATOR_EXP", "synth_power_w")
    _sweep_lineplot(
        edf, x="msw", y="synth_power_w", hue_cols=["dim"],
        title="Affine PG power vs max value width (msw)",
        xlabel="msw (bits)", ylabel="synth power (W)", outpath=outpath,
    )


def _memory_slice(df: pd.DataFrame, y: str) -> pd.DataFrame:
    """MEMORY_EXP slice with port counts filled in.

    MEMORY_EXP pairs some (fw, storage_cap) points with both a 1-port and a
    multi-port build. Config names omit ``inp``/``outp`` when they're the
    default (1), so fill that in and let callers group by port count —
    otherwise the two builds land on one line as a vertical zig-zag.
    """
    edf = _slice(df, "MEMORY_EXP", y)
    for c in ("inp", "outp"):
        edf[c] = edf[c].fillna(1) if c in edf.columns else 1
    return edf


def memport_area_vs_fw(ctx: GenContext, outpath: Path) -> None:
    """MEMORY_EXP: interface-width sweep is fetch_width (fw), faceted by port count."""
    symlink_builds("memory_port", ctx.top_builds / "MEMORY_EXP")
    edf = _memory_slice(ctx.df, "synth_total_area_um2")
    _memport_faceted_plot(
        edf, y="synth_total_area_um2",
        title="MemoryPort area vs interface width (fw)",
        ylabel="synth area (µm²)", outpath=outpath,
    )


def memport_power_vs_fw(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("memory_port", ctx.top_builds / "MEMORY_EXP")
    edf = _memory_slice(ctx.df, "synth_power_w")
    _memport_faceted_plot(
        edf, y="synth_power_w",
        title="MemoryPort power vs interface width (fw)",
        ylabel="synth power (W)", outpath=outpath,
    )


def storage_area_vs_capacity(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("storage", ctx.top_builds / "MEMORY_EXP")
    edf = _memory_slice(ctx.df, "synth_total_area_um2")
    _sweep_lineplot(
        edf, x="storage_cap", y="synth_total_area_um2", hue_cols=["fw", "inp", "outp", "data_width"],
        title="Storage area vs capacity",
        xlabel="storage_cap (bytes)", ylabel="synth area (µm²)", outpath=outpath,
    )


def storage_power_vs_capacity(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("storage", ctx.top_builds / "MEMORY_EXP")
    edf = _memory_slice(ctx.df, "synth_power_w")
    _sweep_lineplot(
        edf, x="storage_cap", y="synth_power_w", hue_cols=["fw", "inp", "outp", "data_width"],
        title="Storage power vs capacity",
        xlabel="storage_cap (bytes)", ylabel="synth power (W)", outpath=outpath,
    )


# ---- Exploration & compiler tables (populated / skeletons) -----------------


def ul_ppa_summary(ctx: GenContext, outpath: Path) -> None:
    """PPA rollup per DesignPoint, all from the in-CGRA MemTile sweep (the final
    run's thesis_dse, tile_sweep.load())."""
    from . import tile_sweep
    try:
        memtile = tile_sweep.load()
    except MissingDataError:
        memtile = None
    tables.emit_ul_ppa_summary(ctx.df, outpath, memtile)


def ul_design_points(ctx: GenContext, outpath: Path) -> None:
    """DesignPoint axis enumeration with round-trip validation flag."""
    tables.emit_ul_design_points(ctx.df, outpath)


def exploration_applications(ctx: GenContext, outpath: Path) -> None:
    """The app registry (what each app computes, its memory classes) with the Memory
    Tiles each app places on the static and ready/valid app results, every app of the
    run (the pond apps too). The tile counts cover the APP_DATA_WIDTH (16-bit) design
    points only (user 2026-11-24)."""
    def load(mode):
        return _at_width(_drop_pareto_only(apps_query.load_app_results_df(
            mode=mode, fabric=APP_FABRIC, level=None, pond_id=apps_query.SINGLE_LEVEL_PONDS)))
    tables.emit_exploration_applications(load("static"), load("rv"), outpath)


def lake_interfaces(ctx: GenContext, outpath: Path) -> None:
    """Component × constructor-signature skeleton (prose TODOs inline)."""
    tables.emit_lake_interfaces(outpath)


def compiler_info(ctx: GenContext, outpath: Path) -> None:
    """Compiler ↔ Component metadata skeleton (prose TODOs inline)."""
    tables.emit_compiler_info(outpath)


# ---- Ch. 5 exploration: app x design_point figures -------------------------
# Backed by THESIS/data/apps/<design>/<app>/results.json (produced by
# THESIS/apps/run_matrix.py). Each generator raises MissingDataError until
# that tree is populated, so the orchestrator swaps in a BOGUS placeholder.


# The fabric the app figures plot (user 2026-10-20: thesis results come from the
# thesis fabrics); results without a fabric field (run_matrix) also count.
APP_FABRIC = "thesis"

# The data width of the design points the app figures show (user 2026-11-18: the
# 16-bit ones only for now, since another width changes what each app computes).
APP_DATA_WIDTH = 16


def _drop_pareto_only(df: pd.DataFrame) -> pd.DataFrame:
    """``df`` without the rows on a ``DESIGNS_PARETO_ONLY`` design point: only the
    single-level Pareto figure shows those (user 2026-11-19)."""
    from ..apps.design_points import DESIGNS_PARETO_ONLY
    if df.empty or "design_id" not in df:
        return df
    return df[~df["design_id"].isin({d.id for d in DESIGNS_PARETO_ONLY})]


def _load_apps_or_miss(required_col: str | None = None, mode: str = "static",
                       level: str | None = "single",
                       pareto_only: bool = False) -> pd.DataFrame:
    """The mode's app results; ``pareto_only`` keeps the DESIGNS_PARETO_ONLY rows,
    which every figure and table but single_level_pareto leaves out."""
    df = apps_query.load_app_results_df(mode=mode, fabric=APP_FABRIC, level=level,
                                        pond_id=apps_query.SINGLE_LEVEL_PONDS)
    if not pareto_only:
        df = _drop_pareto_only(df)
    if df.empty:
        raise MissingDataError(
            f"no PASS {mode} {APP_FABRIC}-fabric rows in THESIS/data/apps — ingest garnet "
            "cgra_app_power.py output (python3 -m THESIS.apps.cgra_power_ingest) or run "
            "THESIS.apps.run_matrix to populate results.json per (design, app) cell"
        )
    if required_col is not None:
        if required_col not in df.columns or df[required_col].dropna().empty:
            raise MissingDataError(
                f"{required_col!r} not populated in any results.json — "
                "upstream flow hasn't run to completion yet"
            )
    return df


def _app_designs(ids) -> list[tuple[str, str]]:
    """(design_id, label) of the design points among ``ids``, in design_points order,
    one per config: "PORT fw4 dw16 8KB SP 2x2 (baseline)" -> "fw4 dw16 8KB SP 2x2", and
    the MEM point with the same config (the same results) is drawn once. The
    DESIGNS_PARETO_ONLY points follow DESIGNS_SINGLE_LEVEL's. Unknown ids last."""
    from ..apps.design_points import DESIGNS_ALL
    out, labels = [], set()
    for d in DESIGNS_ALL:
        label = re.sub(r"^\S+ | \(.*\)$", "", d.display)
        if d.id in ids and label not in labels:
            out.append((d.id, label))
            labels.add(label)
    known = {d.id for d in DESIGNS_ALL}
    return out + [(d, d) for d in sorted(set(ids) - known)]


def _at_width(df: pd.DataFrame) -> pd.DataFrame:
    """``df`` without the rows on a design point of another data width than
    APP_DATA_WIDTH (rows on an id that is not a design point stay)."""
    from ..apps.design_points import DESIGNS_SINGLE_LEVEL
    other = {d.id for d in DESIGNS_SINGLE_LEVEL if d.data_width != APP_DATA_WIDTH}
    return df[~df["design_id"].isin(other)]


def _design_colors() -> dict[str, str]:
    """Each design point's bar color: its place among all the design points, so a
    design point keeps its color whichever of them a figure shows (and matches the
    Pareto figure). The DESIGNS_PARETO_ONLY points come after the others, so they
    reuse colors from the top of SERIES; the Pareto figure draws them as diamonds."""
    from ..apps.design_points import DESIGNS_ALL
    from .tile_figures import SERIES
    return {d: SERIES[i % len(SERIES)]
            for i, (d, _) in enumerate(_app_designs({x.id for x in DESIGNS_ALL}))}


def _missing_mark(ax, xs, fontsize: float) -> None:
    from .tile_figures import MUTED
    for x in xs:
        ax.text(x, 0, "×", ha="center", va="bottom", color=MUTED, fontsize=fontsize)


def _missing_handle():
    from matplotlib.lines import Line2D
    from .tile_figures import MUTED
    return Line2D([], [], ls="", marker="$×$", color=MUTED, markersize=8)


def _app_bar(df: pd.DataFrame, *, y: str, ylabel: str, outpath: Path,
             relative: bool = False) -> None:
    """One group per app (registry order), one bar per design point at APP_DATA_WIDTH;
    × where the app has no result there. ``relative``: each app's bars over its first
    design point's."""
    import numpy as np
    from .tile_figures import FULL_W, SERIES, _ax_style, _legend_above, _save
    from ..apps.registry import APPS
    df = _at_width(df.dropna(subset=["app_id", "design_id", y]))
    if df.empty:
        raise MissingDataError(f"no non-null {y} values in the app results")
    designs = _app_designs(set(df["design_id"]))
    order = [a.id for a in APPS if a.id in set(df["app_id"])]
    apps = order + sorted(set(df["app_id"]) - set(order))
    names = {a.id: a.display for a in APPS}
    vals = (df.pivot_table(index="app_id", columns="design_id", values=y, aggfunc="mean")
            .reindex(index=apps, columns=[d for d, _ in designs]))
    if relative:
        vals = vals.div(vals.iloc[:, 0], axis=0)
    fig, ax = plt.subplots(figsize=(FULL_W, 2.6))
    idx = np.arange(len(apps))
    w = 0.84 / len(designs)
    colors = _design_colors()
    for i, (d, label) in enumerate(designs):
        xs = idx - 0.42 + w * (i + 0.5)
        col = vals[d].to_numpy(dtype=float)
        ax.bar(xs, col, width=w, color=colors.get(d, SERIES[i % len(SERIES)]), label=label)
        _missing_mark(ax, xs[np.isnan(col)], 8)
    ax.set_xticks(idx)
    # "Camera Pipeline 2x2" -> "Camera\nPipeline 2x2": five names side by side fit on two lines
    ax.set_xticklabels([n.replace(" ", "\n", 1) if len(n) > 10 else n
                        for n in (names.get(a, a) for a in apps)])
    ax.set_ylabel(ylabel)
    _ax_style(ax)
    handles, labels = ax.get_legend_handles_labels()
    if vals.isna().to_numpy().any():
        handles.append(_missing_handle())
        labels.append("No result")
    _legend_above(ax, handles, labels)
    _save(fig, outpath)


def single_level_power(ctx: GenContext, outpath: Path, mode: str = "static") -> None:
    """Per-app synth power on each single-level design point."""
    df = _load_apps_or_miss(required_col="synth_power_w", mode=mode)
    _app_bar(df.assign(power_mw=df["synth_power_w"] * 1000.0), y="power_mw",
             ylabel="Power (mW)", outpath=outpath)


def single_level_performance(ctx: GenContext, outpath: Path, mode: str = "static") -> None:
    """Per-app kernel wall time (kernel cycles x clock period) on each single-level
    design point: each design point runs at its own clock (user 2026-11-18)."""
    df = _load_apps_or_miss(required_col="total_cycles", mode=mode)
    if "clock_period_ps" not in df.columns or df["clock_period_ps"].dropna().empty:
        raise MissingDataError("clock_period_ps missing — extractor row not resolved")
    _app_bar(df.assign(wall_us=df["total_cycles"] * df["clock_period_ps"] * 1e-6), y="wall_us",
             ylabel="Wall time (µs)", outpath=outpath)


def single_level_area(ctx: GenContext, outpath: Path, mode: str = "static") -> None:
    """Per-app synth area of the CGRA tiles the app is placed on (``_placed_area``, not
    its route-only tiles) on each single-level design point. It includes the SRAM macros,
    so with tile_figures.SRAM_NDA each app's bars are relative to its first design point."""
    from .tile_figures import SRAM_NDA
    df = _with_placed_area(_load_apps_or_miss(mode=mode))
    if SRAM_NDA:
        ref = _app_designs(set(df.dropna(subset=["placed_area_um2"])["design_id"]))[0][1]
        _app_bar(df, y="placed_area_um2", ylabel=f"Area (× {ref})", outpath=outpath,
                 relative=True)
    else:
        _app_bar(df.assign(area_k=df["placed_area_um2"] / 1000.0), y="area_k",
                 ylabel="Area (10³ µm²)", outpath=outpath)


def single_level_energy_efficiency(ctx: GenContext, outpath: Path, mode: str = "static") -> None:
    """Per-app energy per run (lower is better). Requires both power and
    total_cycles; raises MissingDataError until ptpx-synth data lands."""
    df = _load_apps_or_miss(required_col="synth_power_w", mode=mode)
    if "total_cycles" not in df.columns or df["total_cycles"].dropna().empty:
        raise MissingDataError("total_cycles missing — sim hasn't populated util.txt yet")
    if "clock_period_ps" not in df.columns or df["clock_period_ps"].dropna().empty:
        raise MissingDataError("clock_period_ps missing — extractor row not resolved")
    # Energy per app run = power * runtime = P * cycles * T_clk.
    df = df.assign(
        runtime_s=df["total_cycles"] * df["clock_period_ps"] * 1e-12,
    )
    df = df.assign(energy_uj=df["synth_power_w"] * df["runtime_s"] * 1e6)
    _app_bar(df, y="energy_uj", ylabel="Energy per run (µJ)", outpath=outpath)


_PLACED_AREA_COLS = ("mem_tiles_placed", "mem_tile_area_um2", "pe_tiles_placed", "pe_tile_area_um2")


def _placed_area(df: pd.DataFrame) -> pd.Series:
    """Synthesis area of the tiles an app places a core on (MEM + PE, SRAM macros
    included). Tiles it only routes through are left out: they hold none of its data,
    and how many there are moves with placement (user 2026-11-18). NaN where a row lacks
    the tile counts or areas."""
    if not set(_PLACED_AREA_COLS) <= set(df.columns):
        return pd.Series(float("nan"), index=df.index)
    mem = df["mem_tiles_placed"] * df["mem_tile_area_um2"]
    return mem + df["pe_tiles_placed"] * df["pe_tile_area_um2"]


def _with_placed_area(df: pd.DataFrame) -> pd.DataFrame:
    """``df`` with a ``placed_area_um2`` column (``_placed_area``); MissingDataError
    when no row has it."""
    df = df.assign(placed_area_um2=_placed_area(df))
    if df["placed_area_um2"].dropna().empty:
        raise MissingDataError("placed tile area not populated in any results.json "
                               f"(needs {', '.join(_PLACED_AREA_COLS)})")
    return df


def _design_ppa(df: pd.DataFrame) -> list[tuple[str, str, int, dict]]:
    """(design_id, label, n_apps, {area, perf, power}) per design point, in design order:
    each app's value over the same app's on the first design point (the baseline), then
    the geometric mean over the apps that ran on both. area = ``_placed_area`` (placed
    tiles only); power stays the power of every tile the app uses, route-only tiles
    included, since their switch boxes carry its data. perf = 1 / kernel runtime
    (cycles x that design point's clock period), so the clock counts, not just cycles."""
    import numpy as np
    df = df.assign(runtime_s=df["total_cycles"] * df["clock_period_ps"] * 1e-12,
                   placed_area_um2=_placed_area(df))
    cols = {"area": "placed_area_um2", "perf": "runtime_s", "power": "synth_power_w"}
    df = df.dropna(subset=["app_id", "design_id", *cols.values()])
    if df.empty:
        raise MissingDataError("no app results with area, runtime and power")
    designs = _app_designs(set(df["design_id"]))
    base = designs[0][0]
    rel = {}
    for k, c in cols.items():
        v = df.pivot_table(index="app_id", columns="design_id", values=c, aggfunc="mean")
        rel[k] = v.div(v[base], axis=0)
    rel["perf"] = 1 / rel["perf"]
    out = []
    for d, label in designs:
        ok = rel["area"][d].notna() & rel["perf"][d].notna() & rel["power"][d].notna()
        if ok.any():
            out.append((d, label, int(ok.sum()),
                        {k: float(np.exp(np.log(v[d][ok]).mean())) for k, v in rel.items()}))
    return out


def _pareto(pts: list[tuple[float, float]], higher: bool) -> list[tuple[float, float]]:
    """The points no other point beats on both axes (smaller x, and larger y if
    ``higher`` else smaller y), in x order."""
    front = []
    for x, y in sorted(pts, key=lambda p: (p[0], -p[1] if higher else p[1])):
        if not front or (y > front[-1][1] if higher else y < front[-1][1]):
            front.append((x, y))
    return front


def _fronts_by_width(xy, higher: bool) -> dict:
    """{data width: Pareto front (``_pareto``)} of ``xy`` = [(design_id, label, x, y)]:
    each width's points form their own front, since points of different data widths
    are not comparable (user 2026-11-23). Ids that are not design points share width None."""
    from ..apps.design_points import DESIGNS_ALL
    width = {d.id: d.data_width for d in DESIGNS_ALL}
    groups: dict = {}
    for d, _, x, y in xy:
        groups.setdefault(width.get(d), []).append((x, y))
    return {w: _pareto(pts, higher) for w, pts in groups.items()}


def _drop_lone_widths(xy: list) -> list:
    """``xy`` = [(design_id, ...)] without the points whose data width has no other
    point: with nothing comparable, they say nothing on a Pareto plot (user 2026-11-23)."""
    from collections import Counter
    from ..apps.design_points import DESIGNS_ALL
    width = {d.id: d.data_width for d in DESIGNS_ALL}
    n = Counter(width.get(p[0]) for p in xy)
    return [p for p in xy if n[width.get(p[0])] > 1]


# The data width single_level_pareto compares at (user 2026-11-18: 16-bit only for now;
# fronts for the other widths come later, once each width has enough design points).
PARETO_DATA_WIDTH = APP_DATA_WIDTH


def _tile_bandwidth(memtile: pd.DataFrame | None,
                    mode: str = "static") -> list[tuple[str, str, float, float]]:
    """(design_id, label, tile area, peak bandwidth in GB/s) per distinct design point
    with a tile-sweep row, in design order. The tile is tab:ul_ppa_summary's
    (``tables._memtile_power_row``): its synthesis area with the SRAM macros, relative
    to the first design point (the baseline), and its clock. Peak bandwidth is the
    words per cycle the tile can move, the Ports' (inputs + outputs) or the SRAM's
    (fetch width, twice that when dual-port), whichever is smaller, x data width x clock.
    It is the tile's alone, no app, in bytes so every data width fits one axis
    (single_level_pareto still draws one front per width: widths are not comparable)."""
    from ..apps.design_points import DESIGNS_ALL
    by_id = {d.id: d for d in DESIGNS_ALL}
    out = []
    for d, label in _app_designs(set(by_id)):
        t = tables._memtile_power_row(memtile, by_id[d], mode)
        if t is None:
            continue
        mhz = t.get("clock_mhz")
        if pd.isna(mhz) and pd.notna(t.get("clock_period_ps")):
            mhz = 1e6 / float(t["clock_period_ps"])
        dp = by_id[d]
        words = min(dp.in_ports + dp.out_ports, dp.fetch_width * (2 if dp.dual_port else 1))
        out.append((d, label, float(t["syn_std"] + t["syn_sram"]),
                    words * dp.data_width / 8 * float(mhz) / 1000))
    if not out:
        raise MissingDataError("no tile-sweep row for any design point "
                               "(python3 -m THESIS.pipeline.tile_sweep <thesis_dse zip>)")
    base = out[0][2]
    return [(d, label, a / base, bw) for d, label, a, bw in out]


def single_level_pareto(ctx: GenContext, outpath: Path, mode: str = "static",
                        data_width: int | None = PARETO_DATA_WIDTH) -> None:
    """(a) Each design point's tile: peak bandwidth against tile area (``_tile_bandwidth``,
    every data width, from the tile sweep). (b) The apps: power against the area of
    the tiles they are placed on, relative to the baseline (``_design_ppa``), on the
    design points at ``data_width`` only (None: all), since another width changes what
    each app computes. Each data width gets its own dashed Pareto front
    (``_fronts_by_width``; user 2026-11-23: widths are not comparable), its points
    ringed, and a width with a single tile is left out (``_drop_lone_widths``). Colors
    follow the per-app bar figures (``_design_colors``). It also shows the
    DESIGNS_PARETO_ONLY points, as diamonds (user 2026-11-19)."""
    from ..apps.design_points import DESIGNS_ALL, DESIGNS_PARETO_ONLY
    from . import tile_sweep
    from .tile_figures import FULL_W, INK2, MUTED, SERIES, _ax_style, _legend_above, _save
    tiles = _drop_lone_widths(_tile_bandwidth(tile_sweep.load(), mode))
    df = _load_apps_or_miss(required_col="synth_power_w", mode=mode, pareto_only=True)
    if data_width is not None:
        ids = {d.id for d in DESIGNS_ALL if d.data_width == data_width}
        df = df[df["design_id"].isin(ids)]
        if df.empty:
            raise MissingDataError(f"no {mode} app results on a {data_width}-bit design point")
    pts = _design_ppa(df)
    n_base = pts[0][2]
    notes = {d: "" if n == n_base else f" ({n} of {n_base} apps in b)" for d, _, n, _ in pts}
    notes[tiles[0][0]] = " (baseline)"
    colors = _design_colors()
    diamonds = {d.id for d in DESIGNS_PARETO_ONLY}
    panels = (
        ("(a) Tile", "Tile area (× baseline)", "Peak bandwidth (GB/s)", True, tiles),
        ("(b) Applications", "Area used (× baseline)", "Power (× baseline)", False,
         [(d, label, v["area"], v["power"]) for d, label, _, v in pts]),
    )
    fig, axes = plt.subplots(1, 2, figsize=(FULL_W, 2.7))
    handles = {}
    for ax, (title, xlabel, ylabel, higher, xy) in zip(axes, panels):
        for d, label, x, y in xy:
            handles[label] = ax.scatter(x, y, s=50, color=colors.get(d, SERIES[0]),
                                        marker="D" if d in diamonds else "o",
                                        edgecolor="white", linewidth=0.6, zorder=3,
                                        label=label + notes.get(d, ""))
        for front in _fronts_by_width(xy, higher).values():
            # rings mark the front, so a one-point front still shows
            line, = ax.plot(*zip(*front), color=MUTED, linestyle="--", linewidth=1.1,
                            marker="o", markersize=11, markerfacecolor="none",
                            markeredgewidth=1.1, zorder=2, label="Pareto front (per data width)")
        ax.set_title(title, loc="left", fontsize=10.5, color=INK2)
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        _ax_style(ax, xgrid=True)
    fig.tight_layout()
    # one legend for both panels, in design order: (a) shows every design point
    labels = list(dict.fromkeys([label for _, label, *_ in tiles]
                                + [label for _, label, *_ in pts]))
    hs = [handles[label] for label in labels] + [line]
    _legend_above(axes[0], hs, [h.get_label() for h in hs])
    _save(fig, outpath)


def ul_perf(ctx: GenContext, outpath: Path) -> None:
    """Per-app UL memories, output rate and runtime over the single-level design
    points: the static app results, every app of the run (the pond apps too), on the
    APP_DATA_WIDTH (16-bit) design points only (user 2026-11-25)."""
    tables.emit_ul_perf(_at_width(_load_apps_or_miss(required_col="total_cycles", level=None)),
                        outpath, data_width=APP_DATA_WIDTH)


def _li(gen):
    """gen on the RV (latency-insensitive) results."""
    def li(ctx: GenContext, outpath: Path) -> None:
        gen(ctx, outpath, mode="rv")
    li.__name__ = li.__qualname__ = f"{gen.__name__}_LI"
    li.__doc__ = f"{gen.__doc__.splitlines()[0]} RV (latency-insensitive) results."
    return li


single_level_power_LI = _li(single_level_power)
single_level_performance_LI = _li(single_level_performance)
single_level_area_LI = _li(single_level_area)
single_level_energy_efficiency_LI = _li(single_level_energy_efficiency)


# ---- two_level_*: each two-level app against its single-level counterpart ----
# A pond app and the same computation without the pond (AppSpec.single_level_app),
# side by side on every design point: what the second level (the PE-tile pond) buys
# (user 2026-10-30). RV only: static resnet_pond_agg came only with the 2026-11-23 zip,
# and the no-pond matmul maps in neither mode. A missing bar is drawn as an ×: no
# bit-accurate result there. Performance and energy are per output word. The ResNet
# pair does the same work (2352 output words each) but differs in schedule and input
# file (resnet_agg EXT=mat, resnet_pond_agg the default png).
TWO_LEVEL_MODE = "rv"
_LEVEL_LABELS = {"single": "Single level (MEM tiles)", "two": "Two level (+ PE-tile pond)"}


def _level_pairs(required_col: str) -> pd.DataFrame:
    """The RV rows of every (single-level, two-level) app pair, with ``pair`` (the
    single-level app's name) and ``level`` (single | two) columns."""
    from ..apps.registry import APPS, by_id
    df = _drop_pareto_only(apps_query.load_app_results_df(
        mode=TWO_LEVEL_MODE, fabric=APP_FABRIC, level=None, pond_id=apps_query.SINGLE_LEVEL_PONDS))
    parts = []
    for app in APPS:
        if not app.single_level_app or df.empty:
            continue
        for level, app_id in (("single", app.single_level_app), ("two", app.id)):
            rows = df[df["app_id"] == app_id]
            parts.append(rows.assign(pair=by_id(app.single_level_app).display, level=level))
    df = pd.concat(parts) if parts else pd.DataFrame()
    if df.empty:
        raise MissingDataError(
            f"no PASS {TWO_LEVEL_MODE} {APP_FABRIC}-fabric rows of a two-level app or its "
            "single-level counterpart in THESIS/data/apps — ingest garnet cgra_app_power.py "
            "output (python3 -m THESIS.apps.cgra_power_ingest)"
        )
    if required_col not in df.columns or df[required_col].dropna().empty:
        raise MissingDataError(f"{required_col!r} not populated in any two-level pair's results.json")
    return df


def _paired_bar(df: pd.DataFrame, *, y: str, ylabel: str, outpath: Path,
                relative: bool = False) -> None:
    """One panel per app pair: the single- and two-level bar on each design point.
    ``relative``: each panel over its first design point's single-level bar (the
    two-level one where there is none)."""
    import numpy as np
    from .tile_figures import FULL_W, SERIES, _ax_style, _legend_above, _save
    df = _at_width(df)
    if df.empty:
        raise MissingDataError(f"no two-level pair results on a {APP_DATA_WIDTH}-bit design point")
    designs = _app_designs(set(df["design_id"]))
    # "fw4 dw16 8KB SP 2x2" -> "fw4 dw16\n8KB SP\n2x2"
    ticks = [re.sub(r"^(\S+ \S+) (\S+ \S+) ", "\\1\n\\2\n", label) for _, label in designs]
    # pairs with a single-level result first: the ones the figure compares
    pairs = sorted(dict.fromkeys(df["pair"]), key=lambda p: not (df[df["pair"] == p]["level"] == "single").any())
    # 1.3 in per panel: two of these figures fit a page at full \textwidth
    fig, axes = plt.subplots(len(pairs), 1, figsize=(FULL_W, 1.3 * len(pairs) + 0.8), sharex=True,
                             squeeze=False)
    idx = np.arange(len(designs))
    for ax, pair in zip(axes[:, 0], pairs):
        vals = (df[df["pair"] == pair].pivot_table(index="design_id", columns="level", values=y, aggfunc="mean")
                .reindex(index=[d for d, _ in designs], columns=list(_LEVEL_LABELS)))
        if relative:
            first = vals.iloc[0].dropna()
            vals = vals / (first.iloc[0] if not first.empty else np.nan)
        for i, (level, color) in enumerate(zip(_LEVEL_LABELS, SERIES)):
            xs = idx + (i - 0.5) * 0.4
            col = vals[level].to_numpy(dtype=float)
            ax.bar(xs, col, width=0.4, color=color, label=_LEVEL_LABELS[level])
            _missing_mark(ax, xs[np.isnan(col)], 11)
        ax.set_title(pair, loc="left")
        _ax_style(ax)
    fig.supylabel(ylabel, fontsize=10.5, fontweight="bold")
    axes[-1, 0].set_xticks(idx)
    axes[-1, 0].set_xticklabels(ticks, fontsize=8.5)
    handles, labels = axes[0, 0].get_legend_handles_labels()
    _legend_above(axes[0, 0], handles + [_missing_handle()], labels + ["No result"])
    _save(fig, outpath)


def two_level_power(ctx: GenContext, outpath: Path) -> None:
    """Single- vs two-level synth power of each app pair on each design point."""
    df = _level_pairs("synth_power_w")
    _paired_bar(df.assign(power_mw=df["synth_power_w"] * 1000.0), y="power_mw",
                ylabel="Power (mW)", outpath=outpath)


def two_level_performance(ctx: GenContext, outpath: Path) -> None:
    """Single- vs two-level wall time per output word (kernel cycles x clock period /
    output words) of each app pair on each design point."""
    df = _level_pairs("total_cycles")
    for col in ("clock_period_ps", "output_words"):
        if col not in df.columns or df[col].dropna().empty:
            raise MissingDataError(f"{col} missing from the two-level pairs' results.json")
    ns = df["total_cycles"] * df["clock_period_ps"] / df["output_words"] / 1000.0
    _paired_bar(df.assign(ns_per_word=ns), y="ns_per_word", ylabel="Time / output word (ns)",
                outpath=outpath)


def two_level_area(ctx: GenContext, outpath: Path) -> None:
    """Single- vs two-level synth area of the CGRA tiles each app is placed on
    (``_placed_area``, not its route-only tiles). It includes the SRAM macros, so with
    tile_figures.SRAM_NDA each panel is relative to its leftmost bar."""
    from .tile_figures import SRAM_NDA
    df = _with_placed_area(_level_pairs("mem_tiles_placed"))
    if SRAM_NDA:
        _paired_bar(df, y="placed_area_um2", ylabel="Area (× leftmost bar)", outpath=outpath,
                    relative=True)
    else:
        _paired_bar(df.assign(area_k=df["placed_area_um2"] / 1000.0), y="area_k",
                    ylabel="Area (10³ µm²)", outpath=outpath)


def two_level_energy_efficiency(ctx: GenContext, outpath: Path) -> None:
    """Single- vs two-level energy per output word (power x kernel time / output words)."""
    df = _level_pairs("synth_power_w")
    for col in ("total_cycles", "clock_period_ps", "output_words"):
        if col not in df.columns or df[col].dropna().empty:
            raise MissingDataError(f"{col} missing from the two-level pairs' results.json")
    energy_pj = df["synth_power_w"] * df["total_cycles"] * df["clock_period_ps"] / df["output_words"]  # W x ps = pJ
    _paired_bar(df.assign(energy_pj=energy_pj), y="energy_pj", ylabel="Energy / output word (pJ)",
                outpath=outpath)
