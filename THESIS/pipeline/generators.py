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


def memport_area_vs_fw(ctx: GenContext, outpath: Path) -> None:
    """MEMORY_EXP: interface-width sweep is fetch_width (fw)."""
    symlink_builds("memory_port", ctx.top_builds / "MEMORY_EXP")
    edf = _slice(ctx.df, "MEMORY_EXP", "synth_total_area_um2")
    _sweep_lineplot(
        edf, x="fw", y="synth_total_area_um2", hue_cols=["storage_cap", "data_width"],
        title="MemoryPort area vs interface width (fw)",
        xlabel="fetch_width", ylabel="synth area (µm²)", outpath=outpath,
    )


def memport_power_vs_fw(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("memory_port", ctx.top_builds / "MEMORY_EXP")
    edf = _slice(ctx.df, "MEMORY_EXP", "synth_power_w")
    _sweep_lineplot(
        edf, x="fw", y="synth_power_w", hue_cols=["storage_cap", "data_width"],
        title="MemoryPort power vs interface width (fw)",
        xlabel="fetch_width", ylabel="synth power (W)", outpath=outpath,
    )


def storage_area_vs_capacity(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("storage", ctx.top_builds / "MEMORY_EXP")
    edf = _slice(ctx.df, "MEMORY_EXP", "synth_total_area_um2")
    _sweep_lineplot(
        edf, x="storage_cap", y="synth_total_area_um2", hue_cols=["fw", "data_width"],
        title="Storage area vs capacity",
        xlabel="storage_cap (bytes)", ylabel="synth area (µm²)", outpath=outpath,
    )


def storage_power_vs_capacity(ctx: GenContext, outpath: Path) -> None:
    symlink_builds("storage", ctx.top_builds / "MEMORY_EXP")
    edf = _slice(ctx.df, "MEMORY_EXP", "synth_power_w")
    _sweep_lineplot(
        edf, x="storage_cap", y="synth_power_w", hue_cols=["fw", "data_width"],
        title="Storage power vs capacity",
        xlabel="storage_cap (bytes)", ylabel="synth power (W)", outpath=outpath,
    )
