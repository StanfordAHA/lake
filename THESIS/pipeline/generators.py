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

from dataclasses import dataclass
from itertools import cycle
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

from . import apps_query, regression, tables
from .errors import MissingDataError
from .ingest import symlink_builds


@dataclass
class GenContext:
    """Shared state passed to every generator."""

    df: pd.DataFrame
    top_builds: Path


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


# ---- Memtile PPA regression tables -----------------------------------------


def memtile_model_coeff(ctx: GenContext, outpath: Path) -> None:
    """Lasso-fit coefficient table (Kahng-hybrid area + power + delay)."""
    regression.emit_coef_table(ctx.df, outpath)


def memtile_model_verif(ctx: GenContext, outpath: Path) -> None:
    """Leave-one-experiment-out predicted-vs-actual residuals table."""
    regression.emit_verif_table(ctx.df, outpath)


# ---- Exploration & compiler tables (populated / skeletons) -----------------


def ul_ppa_summary(ctx: GenContext, outpath: Path) -> None:
    """PPA rollup per DesignPoint. Power blank until sweep runs."""
    tables.emit_ul_ppa_summary(ctx.df, outpath)


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


def _load_apps_or_miss(required_col: str | None = None) -> pd.DataFrame:
    df = apps_query.load_app_results_df()
    if df.empty:
        raise MissingDataError(
            "no PASS rows in THESIS/data/apps — run THESIS.apps.run_matrix "
            "on the cluster to populate results.json per (design, app) cell"
        )
    if required_col is not None:
        if required_col not in df.columns or df[required_col].dropna().empty:
            raise MissingDataError(
                f"{required_col!r} not populated in any results.json — "
                "upstream flow hasn't run to completion yet"
            )
    return df


def _grouped_bar(
    df: pd.DataFrame,
    *,
    x: str,       # e.g. "app_id" — categorical
    hue: str,     # e.g. "design_id" — bars grouped per x value
    y: str,       # value column
    title: str,
    xlabel: str,
    ylabel: str,
    outpath: Path,
) -> None:
    """Grouped bar chart. One bar per (hue) value, grouped along x."""
    df = df.dropna(subset=[x, hue, y])
    if df.empty:
        raise MissingDataError(f"no non-null rows for {y} by ({x}, {hue})")

    pivot = df.pivot_table(index=x, columns=hue, values=y, aggfunc="mean").sort_index()
    hues = list(pivot.columns)
    xs = list(pivot.index)
    n_hue = max(len(hues), 1)
    bar_w = 0.8 / n_hue

    import numpy as np
    fig, ax = plt.subplots(figsize=(max(6.0, 0.9 * len(xs) + 2.5), 4.0))
    idx = np.arange(len(xs))
    for i, h in enumerate(hues):
        offsets = idx - 0.4 + bar_w * (i + 0.5)
        ax.bar(offsets, pivot[h].to_numpy(), width=bar_w, label=str(h))
    ax.set_xticks(idx)
    ax.set_xticklabels(xs, rotation=30, ha="right", fontsize=8)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.set_title(title)
    ax.legend(fontsize=7, loc="best", title=hue)
    ax.grid(True, axis="y", alpha=0.3)
    outpath.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(outpath, dpi=150)
    plt.close(fig)


def single_level_power(ctx: GenContext, outpath: Path) -> None:
    """Per-app synth power on each single-level design point."""
    df = _load_apps_or_miss(required_col="synth_power_w")
    df = df.assign(power_mw=df["synth_power_w"] * 1000.0)
    _grouped_bar(
        df, x="app_id", hue="design_id", y="power_mw",
        title="Single-level: per-app power",
        xlabel="App", ylabel="Synth power (mW)",
        outpath=outpath,
    )


def single_level_performance(ctx: GenContext, outpath: Path) -> None:
    """Per-app cycles-to-complete on each single-level design point."""
    df = _load_apps_or_miss(required_col="total_cycles")
    _grouped_bar(
        df, x="app_id", hue="design_id", y="total_cycles",
        title="Single-level: per-app cycles to complete",
        xlabel="App", ylabel="Total cycles",
        outpath=outpath,
    )


def single_level_area(ctx: GenContext, outpath: Path) -> None:
    """Per-design synth area (design-level, not per-app). Averaged across apps
    to collapse the per-cell duplicates — area doesn't vary with app."""
    df = _load_apps_or_miss(required_col="synth_total_area_um2")
    per_design = (
        df.groupby("design_id", as_index=False)[["synth_total_area_um2",
                                                 "synth_logic_area_um2",
                                                 "synth_storage_area_um2"]]
          .mean()
          .sort_values("synth_total_area_um2")
    )
    import numpy as np
    fig, ax = plt.subplots(figsize=(max(6.0, 0.9 * len(per_design) + 2.5), 4.0))
    idx = np.arange(len(per_design))
    logic = per_design["synth_logic_area_um2"].fillna(0).to_numpy()
    storage = per_design["synth_storage_area_um2"].fillna(0).to_numpy()
    ax.bar(idx, logic, label="Logic", color="tab:blue")
    ax.bar(idx, storage, bottom=logic, label="SRAM", color="tab:orange")
    ax.set_xticks(idx)
    ax.set_xticklabels(per_design["design_id"], rotation=30, ha="right", fontsize=8)
    ax.set_xlabel("Design point")
    ax.set_ylabel("Synth area (µm²)")
    ax.set_title("Single-level: synth area by design point")
    ax.legend(fontsize=8, loc="best")
    ax.grid(True, axis="y", alpha=0.3)
    outpath.parent.mkdir(parents=True, exist_ok=True)
    fig.tight_layout()
    fig.savefig(outpath, dpi=150)
    plt.close(fig)


def single_level_utilization(ctx: GenContext, outpath: Path) -> None:
    """Per-app memory-tile utilization (active handshake cycles / total)."""
    df = _load_apps_or_miss(required_col="utilization")
    df = df.assign(utilization_pct=df["utilization"] * 100.0)
    _grouped_bar(
        df, x="app_id", hue="design_id", y="utilization_pct",
        title="Single-level: memtile utilization per app",
        xlabel="App", ylabel="Utilization (%)",
        outpath=outpath,
    )


def single_level_energy_efficiency(ctx: GenContext, outpath: Path) -> None:
    """Per-app energy-per-op (lower is better). Requires both power and
    total_cycles; raises MissingDataError until ptpx-synth data lands."""
    df = _load_apps_or_miss(required_col="synth_power_w")
    if "total_cycles" not in df.columns or df["total_cycles"].dropna().empty:
        raise MissingDataError("total_cycles missing — sim hasn't populated util.txt yet")
    if "clock_period_ps" not in df.columns or df["clock_period_ps"].dropna().empty:
        raise MissingDataError("clock_period_ps missing — extractor row not resolved")
    # Energy per app run = power * runtime = P * cycles * T_clk.
    df = df.assign(
        runtime_s=df["total_cycles"] * df["clock_period_ps"] * 1e-12,
    )
    df = df.assign(energy_uj=df["synth_power_w"] * df["runtime_s"] * 1e6)
    _grouped_bar(
        df, x="app_id", hue="design_id", y="energy_uj",
        title="Single-level: energy per app run (lower is better)",
        xlabel="App", ylabel="Energy (µJ)",
        outpath=outpath,
    )
