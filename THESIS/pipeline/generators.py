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
    """Render the AppSpec registry as a LaTeX table."""
    tables.emit_exploration_applications(outpath)


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


def _load_apps_or_miss(required_col: str | None = None, mode: str = "static",
                       level: str | None = "single") -> pd.DataFrame:
    df = apps_query.load_app_results_df(mode=mode, fabric=APP_FABRIC, level=level,
                                        pond_id=apps_query.SINGLE_LEVEL_PONDS)
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
    the MEM point with the same config (the same results) is drawn once. Unknown ids last."""
    from ..apps.design_points import DESIGNS_SINGLE_LEVEL
    out, labels = [], set()
    for d in DESIGNS_SINGLE_LEVEL:
        label = re.sub(r"^\S+ | \(.*\)$", "", d.display)
        if d.id in ids and label not in labels:
            out.append((d.id, label))
            labels.add(label)
    known = {d.id for d in DESIGNS_SINGLE_LEVEL}
    return out + [(d, d) for d in sorted(set(ids) - known)]


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
    """One group per app (registry order), one bar per design point; × where the app
    has no result there. ``relative``: each app's bars over its first design point's."""
    import numpy as np
    from .tile_figures import FULL_W, SERIES, _ax_style, _legend_above, _save
    from ..apps.registry import APPS
    df = df.dropna(subset=["app_id", "design_id", y])
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
    for i, ((d, label), color) in enumerate(zip(designs, SERIES * 2)):
        xs = idx - 0.42 + w * (i + 0.5)
        col = vals[d].to_numpy(dtype=float)
        ax.bar(xs, col, width=w, color=color, label=label)
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
    """Per-app kernel cycles on each single-level design point."""
    df = _load_apps_or_miss(required_col="total_cycles", mode=mode)
    _app_bar(df.assign(kcycles=df["total_cycles"] / 1000.0), y="kcycles",
             ylabel="Kernel cycles (10³)", outpath=outpath)


def single_level_area(ctx: GenContext, outpath: Path, mode: str = "static") -> None:
    """Per-app synth area of the CGRA tiles the app touches (a run_matrix cell: the
    design's own area) on each single-level design point. It includes the SRAM macros,
    so with tile_figures.SRAM_NDA each app's bars are relative to its first design point."""
    from .tile_figures import SRAM_NDA
    df = _load_apps_or_miss(required_col="synth_total_area_um2", mode=mode)
    if SRAM_NDA:
        ref = _app_designs(set(df.dropna(subset=["synth_total_area_um2"])["design_id"]))[0][1]
        _app_bar(df, y="synth_total_area_um2", ylabel=f"Area (× {ref})", outpath=outpath,
                 relative=True)
    else:
        _app_bar(df.assign(area_k=df["synth_total_area_um2"] / 1000.0), y="area_k",
                 ylabel="Area (10³ µm²)", outpath=outpath)


def single_level_utilization(ctx: GenContext, outpath: Path, mode: str = "static") -> None:
    """Per-app memory-tile utilization (SRAM busy cycles / kernel cycles)."""
    df = _load_apps_or_miss(required_col="utilization", mode=mode)
    _app_bar(df.assign(utilization_pct=df["utilization"] * 100.0), y="utilization_pct",
             ylabel="Utilization (%)", outpath=outpath)


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


def _design_ppa(df: pd.DataFrame) -> list[tuple[str, str, int, dict]]:
    """(design_id, label, n_apps, {area, perf, power}) per design point, in design order:
    each app's value over the same app's on the first design point (the baseline), then
    the geometric mean over the apps that ran on both. perf = 1 / kernel runtime
    (cycles x that design point's clock period), so the clock counts, not just cycles."""
    import numpy as np
    df = df.assign(runtime_s=df["total_cycles"] * df["clock_period_ps"] * 1e-12)
    cols = {"area": "synth_total_area_um2", "perf": "runtime_s", "power": "synth_power_w"}
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


def single_level_pareto(ctx: GenContext, outpath: Path, mode: str = "static") -> None:
    """Each single-level design point's (a) performance and (b) power against its area,
    relative to the baseline (``_design_ppa``), with the Pareto front dashed. Area
    includes the SRAM macros, so it is relative whether or not SRAM_NDA is set."""
    from .tile_figures import FULL_W, INK2, MUTED, SERIES, _ax_style, _legend_above, _save
    pts = _design_ppa(_load_apps_or_miss(required_col="synth_power_w", mode=mode))
    n_base = pts[0][2]
    fig, axes = plt.subplots(1, 2, figsize=(FULL_W, 2.7))
    for ax, key, title, higher in ((axes[0], "perf", "(a) Performance", True),
                                   (axes[1], "power", "(b) Power", False)):
        for i, ((_, label, n, v), c) in enumerate(zip(pts, SERIES * 2)):
            note = " (baseline)" if i == 0 else "" if n == n_base else f" ({n} of {n_base} apps)"
            ax.scatter(v["area"], v[key], s=50, color=c, edgecolor="white", linewidth=0.6,
                       zorder=3, label=label + note)
        front = _pareto([(v["area"], v[key]) for *_, v in pts], higher)
        ax.plot(*zip(*front), color=MUTED, linestyle="--", linewidth=1.1, zorder=2,
                label="Pareto front")
        ax.set_title(title, loc="left", fontsize=10.5, color=INK2)
        ax.set_xlabel("Area (× baseline)")
        ax.set_ylabel(f"{title[4:]} (× baseline)")
        _ax_style(ax, xgrid=True)
    fig.tight_layout()
    _legend_above(axes[0])
    _save(fig, outpath)


def ul_perf(ctx: GenContext, outpath: Path) -> None:
    """Per-app UL memories, output rate and runtime over the single-level design
    points: the static app results, every app of the run (the pond matmul too)."""
    tables.emit_ul_perf(_load_apps_or_miss(required_col="total_cycles", level=None), outpath)


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
single_level_utilization_LI = _li(single_level_utilization)
single_level_energy_efficiency_LI = _li(single_level_energy_efficiency)


# ---- two_level_*: each two-level app against its single-level counterpart ----
# A pond app and the same computation without the pond (AppSpec.single_level_app),
# side by side on every design point: what the second level (the PE-tile pond) buys
# (user 2026-10-30). RV only: resnet_pond_agg records RV only, and the no-pond matmul
# maps in neither mode. A missing bar is drawn as an ×: no bit-accurate result there.
# A pair need not do the same work (resnet_agg runs a tile=14 kernel, resnet_pond_agg
# the whole 28x28 image; aha-08 2026-10-30), so performance and energy are per output
# word.
TWO_LEVEL_MODE = "rv"
_LEVEL_LABELS = {"single": "Single level (MEM tiles)", "two": "Two level (+ PE-tile pond)"}


def _level_pairs(required_col: str) -> pd.DataFrame:
    """The RV rows of every (single-level, two-level) app pair, with ``pair`` (the
    single-level app's name) and ``level`` (single | two) columns."""
    from ..apps.registry import APPS, by_id
    df = apps_query.load_app_results_df(mode=TWO_LEVEL_MODE, fabric=APP_FABRIC, level=None,
                                        pond_id=apps_query.SINGLE_LEVEL_PONDS)
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
    """Single- vs two-level output words per kernel cycle of each app pair on each
    design point."""
    _paired_bar(_level_pairs("output_per_cycle"), y="output_per_cycle",
                ylabel="Output words / cycle", outpath=outpath)


def two_level_area(ctx: GenContext, outpath: Path) -> None:
    """Single- vs two-level synth area of the CGRA tiles each app touches. It includes
    the SRAM macros, so with tile_figures.SRAM_NDA each panel is relative to its
    leftmost bar."""
    from .tile_figures import SRAM_NDA
    df = _level_pairs("synth_total_area_um2")
    if SRAM_NDA:
        _paired_bar(df, y="synth_total_area_um2", ylabel="Area (× leftmost bar)", outpath=outpath,
                    relative=True)
    else:
        _paired_bar(df.assign(area_k=df["synth_total_area_um2"] / 1000.0), y="area_k",
                    ylabel="Area (10³ µm²)", outpath=outpath)


def two_level_utilization(ctx: GenContext, outpath: Path) -> None:
    """Single- vs two-level memory-tile utilization of each app pair."""
    df = _level_pairs("utilization")
    _paired_bar(df.assign(utilization_pct=df["utilization"] * 100.0), y="utilization_pct",
                ylabel="Utilization (%)", outpath=outpath)


def two_level_energy_efficiency(ctx: GenContext, outpath: Path) -> None:
    """Single- vs two-level energy per output word (power x kernel time / output words)."""
    df = _level_pairs("synth_power_w")
    for col in ("total_cycles", "clock_period_ps", "output_words"):
        if col not in df.columns or df[col].dropna().empty:
            raise MissingDataError(f"{col} missing from the two-level pairs' results.json")
    energy_pj = df["synth_power_w"] * df["total_cycles"] * df["clock_period_ps"] / df["output_words"]  # W x ps = pJ
    _paired_bar(df.assign(energy_pj=energy_pj), y="energy_pj", ylabel="Energy / output word (pJ)",
                outpath=outpath)
