#!/usr/bin/env python3
"""Generate plots from the CSV produced by ``extract_power_area.py``.

Default behaviour: for each experiment, auto-detect which sweep parameters
actually vary; for each varying param ``P``, write
``<outdir>/<EXPERIMENT>/<metric>_vs_<P>.png`` with one line per combination
of the other varying params.

Designed so additional plot kinds (scatter / bar / heatmap) can be added as
new functions on top of the same loaded DataFrame without touching the data
layer.

Usage:
    python3 plot_power_area.py <csv> [-o figs/] [--metric synth_total_area_um2]
                                     [--experiments AFFINE_PATTERN_GENERATOR_EXP ...]
"""

from __future__ import annotations

import argparse
import sys
from itertools import cycle
from pathlib import Path

import matplotlib.pyplot as plt
import pandas as pd

# Schema markers from extract_power_area.py — sweep params live between these
# columns in the CSV.
LEADING_COLS = ["experiment", "sweep_group", "sweep_name", "frequency_mhz"]
METRIC_COLS = [
    "synth_cell_area_um2",
    "synth_total_area_um2",
    "pnr_total_area_um2",
    "pnr_macro_area_um2",
    "synth_power_w",
    "pnr_power_w",
    "clock_period_ps",
    "wns_ps",
    "crit_path_delay_ps",
]
# String columns emitted alongside the metrics — not plot targets but need
# to be excluded from the sweep-column detection.
TRAILING_COLS = METRIC_COLS + [
    "crit_path_startpoint", "crit_path_endpoint", "build_dir",
]

DEFAULT_METRIC = "synth_total_area_um2"


def sweep_columns(df: pd.DataFrame) -> list[str]:
    """All columns between the leading metadata and the trailing metric block."""
    cols = list(df.columns)
    return [c for c in cols if c not in LEADING_COLS and c not in TRAILING_COLS]


def varying_params(df: pd.DataFrame, sweep_cols: list[str]) -> list[str]:
    """Sweep params that have >1 unique non-null value in the slice."""
    out = []
    for c in sweep_cols:
        vals = df[c].dropna().unique()
        if len(vals) > 1:
            out.append(c)
    return out


def fmt_group_label(keys: list[str], values: tuple) -> str:
    parts = []
    for k, v in zip(keys, values):
        if pd.isna(v):
            continue
        # Show ints as ints (the CSV stored them as floats after pandas read).
        if isinstance(v, float) and v.is_integer():
            v = int(v)
        parts.append(f"{k}={v}")
    return ", ".join(parts) if parts else "all"


def make_lineplot(
    df: pd.DataFrame,
    x: str,
    y: str,
    hue_cols: list[str],
    title: str,
    targets: list[tuple[Path, list[str]]],
) -> None:
    """One figure: y vs x, one line per unique tuple of ``hue_cols`` values.

    ``targets`` is a list of ``(stem, formats)`` pairs; each stem gets every
    listed extension appended. This lets a single render produce e.g. a
    per-experiment png+pdf, an aggregate ALL/ png+pdf, and an ALL_PDF/ pdf
    in one go.
    """
    fig, ax = plt.subplots(figsize=(7.5, 5.0))

    if hue_cols:
        groups = df.groupby(hue_cols, dropna=False, sort=True)
        markers = cycle(["o", "s", "^", "D", "v", "P", "X", "*", "<", ">"])
        for key, gdf in groups:
            if not isinstance(key, tuple):
                key = (key,)
            gdf = gdf.dropna(subset=[x, y]).sort_values(x)
            if gdf.empty:
                continue
            ax.plot(gdf[x], gdf[y], marker=next(markers), label=fmt_group_label(hue_cols, key))
        ax.legend(fontsize=8, loc="best", title=", ".join(hue_cols))
    else:
        gdf = df.dropna(subset=[x, y]).sort_values(x)
        ax.plot(gdf[x], gdf[y], marker="o")

    # Use log x when the data spans more than two decades and is all positive.
    xvals = df[x].dropna()
    if (xvals > 0).all() and xvals.size and xvals.max() / max(xvals.min(), 1e-12) > 100:
        ax.set_xscale("log")

    ax.set_xlabel(x)
    ax.set_ylabel(y)
    ax.set_title(title)
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    for stem, exts in targets:
        stem.parent.mkdir(parents=True, exist_ok=True)
        for ext in exts:
            fig.savefig(stem.with_suffix(f".{ext}"), dpi=150)
    plt.close(fig)


def plot_metric_vs_sweeps(
    df: pd.DataFrame,
    metric: str,
    outdir: Path,
    experiments: list[str] | None,
    formats: list[str],
    all_dir: str | None,
    all_pdf_dir: str | None,
) -> int:
    """For each experiment, plot ``metric`` vs each varying sweep param."""
    if metric not in df.columns:
        print(f"error: metric column '{metric}' not in CSV", file=sys.stderr)
        return 2

    n_plots = 0
    sweep_cols = sweep_columns(df)
    target_experiments = experiments or sorted(df["experiment"].dropna().unique())

    for exp in target_experiments:
        edf = df[df["experiment"] == exp].copy()
        edf = edf.dropna(subset=[metric])
        if edf.empty:
            print(f"skip {exp}: no rows with {metric}", file=sys.stderr)
            continue

        varying = varying_params(edf, sweep_cols)
        if not varying:
            print(f"skip {exp}: no varying sweep params", file=sys.stderr)
            continue

        for p in varying:
            others = [v for v in varying if v != p]
            base = f"{metric}_vs_{p}"
            targets: list[tuple[Path, list[str]]] = [
                (outdir / exp / base, formats),
            ]
            if all_dir:
                targets.append((outdir / all_dir / f"{exp}__{base}", formats))
            if all_pdf_dir and "pdf" in formats:
                targets.append((outdir / all_pdf_dir / f"{exp}__{base}", ["pdf"]))
            make_lineplot(
                edf,
                x=p,
                y=metric,
                hue_cols=others,
                title=f"{exp}: {metric} vs {p}",
                targets=targets,
            )
            n_plots += 1
            for stem, exts in targets:
                print(f"wrote {stem}.{{{','.join(exts)}}}", file=sys.stderr)

    return 0 if n_plots else 1


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("csv", type=Path, help="CSV from extract_power_area.py")
    ap.add_argument(
        "-o", "--outdir", type=Path,
        default=Path(__file__).resolve().parent / "figs",
        help="Figure output directory (default: ASPLOS_EXP/figs).",
    )
    ap.add_argument(
        "--metric", default=DEFAULT_METRIC, choices=METRIC_COLS,
        help=f"Y-axis metric column (default: {DEFAULT_METRIC}).",
    )
    ap.add_argument(
        "--experiments", nargs="*",
        help="Optional experiment-name filter (default: all).",
    )
    ap.add_argument(
        "--format", dest="formats", nargs="+", default=["png", "pdf"],
        choices=["png", "pdf", "svg"],
        help="Output formats; same stem is reused. Default: png and pdf.",
    )
    ap.add_argument(
        "--all-dir", default="ALL",
        help=(
            "Name of an aggregate subdirectory under --outdir that gets a flat "
            "copy of every figure (filenames prefixed with the experiment "
            "name) in every requested format. Pass an empty string to disable."
        ),
    )
    ap.add_argument(
        "--all-pdf-dir", default="ALL_PDF",
        help=(
            "Like --all-dir but PDF-only, for dropping straight into a LaTeX "
            "figs/ folder. Skipped silently if 'pdf' is not in --format. Pass "
            "an empty string to disable."
        ),
    )
    args = ap.parse_args(argv)

    if not args.csv.is_file():
        print(f"error: {args.csv} not found", file=sys.stderr)
        return 2

    df = pd.read_csv(args.csv)
    return plot_metric_vs_sweeps(
        df, args.metric, args.outdir, args.experiments, args.formats,
        args.all_dir or None, args.all_pdf_dir or None,
    )


if __name__ == "__main__":
    sys.exit(main())
