#!/usr/bin/env python3
"""Compute scaling statistics from extract_power_area.py output.

For every (experiment, varying-param P) pair, fit ``area = m*P + b`` within
each combination of the *other* varying params, then aggregate:

    median slope       — "incremental area per unit of P" (um^2 / unit)
    median intercept   — "baseline area" extrapolated to P=0
    median area-per-P  — area / P (only meaningful when P != 0)

Outputs a markdown summary table to stdout and (optionally) a per-group
fit CSV via ``-o``.

Reusable on the same CSV the plot script consumes; new derived quantities
go in DERIVED_RATIOS at the bottom.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from statistics import median

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

DERIVED_DIRNAME = "DERIVED"

LEADING_COLS = ["experiment", "sweep_group", "sweep_name", "frequency_mhz"]
METRIC_COLS = [
    "synth_cell_area_um2", "synth_total_area_um2",
    "pnr_total_area_um2", "pnr_macro_area_um2",
    "synth_power_w", "pnr_power_w",
    "clock_period_ps", "wns_ps", "crit_path_delay_ps",
]
TRAILING_COLS = METRIC_COLS + [
    "crit_path_startpoint", "crit_path_endpoint", "build_dir",
]
DEFAULT_METRIC = "synth_total_area_um2"


def sweep_columns(df: pd.DataFrame) -> list[str]:
    return [c for c in df.columns if c not in LEADING_COLS and c not in TRAILING_COLS]


def varying_params(df: pd.DataFrame, sweep_cols: list[str]) -> list[str]:
    return [c for c in sweep_cols if df[c].dropna().nunique() > 1]


def fit_groups(edf: pd.DataFrame, p: str, others: list[str], y: str):
    """Yield (group_key, slope, intercept, n, area_per_unit) for each
    sub-group with at least 2 data points along ``p``."""
    if not others:
        sub = edf.dropna(subset=[p, y]).sort_values(p)
        if len(sub) >= 2:
            x = sub[p].to_numpy(dtype=float)
            ys = sub[y].to_numpy(dtype=float)
            m, b = np.polyfit(x, ys, 1)
            apu = float(np.median(ys / np.maximum(x, 1e-12)))
            yield ("all",), m, b, len(sub), apu
        return
    for key, sub in edf.groupby(others, dropna=False, sort=True):
        if not isinstance(key, tuple):
            key = (key,)
        sub = sub.dropna(subset=[p, y]).sort_values(p)
        if len(sub) < 2:
            continue
        x = sub[p].to_numpy(dtype=float)
        ys = sub[y].to_numpy(dtype=float)
        m, b = np.polyfit(x, ys, 1)
        apu = float(np.median(ys / np.maximum(x, 1e-12)))
        yield key, m, b, len(sub), apu


def fmt_group_key(others: list[str], key: tuple) -> str:
    parts = []
    for k, v in zip(others, key):
        if pd.isna(v):
            continue
        if isinstance(v, float) and v.is_integer():
            v = int(v)
        parts.append(f"{k}={v}")
    return ", ".join(parts) if parts else "all"


def compute_fits(df: pd.DataFrame, metric: str):
    """Return list[dict] — one row per (experiment, param, group) linear fit."""
    rows = []
    sweep_cols = sweep_columns(df)
    for exp in sorted(df["experiment"].dropna().unique()):
        edf = df[df["experiment"] == exp].dropna(subset=[metric])
        if edf.empty:
            continue
        varying = varying_params(edf, sweep_cols)
        for p in varying:
            others = [v for v in varying if v != p]
            for key, m, b, n, apu in fit_groups(edf, p, others, metric):
                rows.append({
                    "experiment": exp,
                    "param": p,
                    "others": fmt_group_key(others, key),
                    "n_points": n,
                    "slope_um2_per_unit": float(m),
                    "intercept_um2": float(b),
                    "median_area_per_unit_um2": apu,
                    "min_x": float(edf[p].min()),
                    "max_x": float(edf[p].max()),
                })
    return rows


def summary_table(fit_rows: list[dict]) -> str:
    """Per (experiment, param) one-line aggregate of the per-group fits."""
    groups: dict[tuple, list[dict]] = {}
    for r in fit_rows:
        groups.setdefault((r["experiment"], r["param"]), []).append(r)

    out = []
    out.append(f"| {'Experiment':<30} | {'Param':<14} | {'n_grp':>5} | "
               f"{'median slope (µm²/unit)':>22} | {'slope range':>20} | "
               f"{'median intercept':>16} |")
    out.append("|" + "-" * 32 + "|" + "-" * 16 + "|" + "-" * 7 + "|"
               + "-" * 24 + "|" + "-" * 22 + "|" + "-" * 18 + "|")
    for (exp, p), rs in sorted(groups.items()):
        slopes = [r["slope_um2_per_unit"] for r in rs]
        intercepts = [r["intercept_um2"] for r in rs]
        out.append(
            f"| {exp:<30} | {p:<14} | {len(rs):>5} | "
            f"{median(slopes):>22.2f} | "
            f"{min(slopes):>9.2f} .. {max(slopes):>7.2f} | "
            f"{median(intercepts):>16.1f} |"
        )
    return "\n".join(out)


# --- Derived cross-cuts (named ratios) -----------------------------------
def derived_ratios(df: pd.DataFrame, metric: str) -> list[dict]:
    """Cross-cuts that don't fall out of a single per-param linear fit."""
    rows: list[dict] = []

    # MEMORY: area per port-pair added at fixed (fw, storage_cap).
    # The sweep always co-varies inp == outp ∈ {2, 4}, so the delta is the
    # cost of adding +2 input ports AND +2 output ports simultaneously.
    mem = df[df["experiment"] == "MEMORY_EXP"].dropna(subset=[metric])
    paired = mem.dropna(subset=["inp", "outp"])
    deltas = []
    for (fw, cap), sub in paired.groupby(["fw", "storage_cap"]):
        a2 = sub[(sub["inp"] == 2) & (sub["outp"] == 2)][metric]
        a4 = sub[(sub["inp"] == 4) & (sub["outp"] == 4)][metric]
        if len(a2) and len(a4):
            deltas.append({
                "context": f"fw={int(fw)}, storage_cap={int(cap)}",
                "delta_2to4_um2": float(a4.iloc[0] - a2.iloc[0]),
                "per_added_inp_outp_pair_um2": float((a4.iloc[0] - a2.iloc[0]) / 2),
            })
    if deltas:
        rows.append({
            "name": "MEMORY: area added when inp,outp go 2→4 (i.e. +2 in / +2 out)",
            "median_um2": median(d["delta_2to4_um2"] for d in deltas),
            "per_unit_um2": median(d["per_added_inp_outp_pair_um2"] for d in deltas),
            "n": len(deltas),
            "note": "per_unit is per added (1 inp + 1 outp) — the sweep can't separate them",
        })

    # PORT: area added when fw doubles, at fixed (data_width, vc).
    port = df[df["experiment"] == "PORT_EXP"].dropna(subset=[metric, "fw"])
    doubles = []
    for (dw, vc), sub in port.groupby(["data_width", "vc"], dropna=False):
        sub = sub.sort_values("fw")
        fws = sub["fw"].to_list()
        for f_lo in fws:
            f_hi = f_lo * 2
            if f_hi in fws:
                a_lo = sub[sub["fw"] == f_lo][metric].iloc[0]
                a_hi = sub[sub["fw"] == f_hi][metric].iloc[0]
                doubles.append(float(a_hi - a_lo))
    if doubles:
        rows.append({
            "name": "PORT: area added per fw doubling (at fixed data_width, vc)",
            "median_um2": median(doubles),
            "per_unit_um2": None,
            "n": len(doubles),
            "note": "doubling fw at fixed cap halves SRAM depth & doubles width",
        })

    # PORT: area per bit of data_width at fixed fw, vc.
    dw_slopes = []
    for (fw, vc), sub in port.groupby(["fw", "vc"], dropna=False):
        sub = sub.dropna(subset=["data_width"]).sort_values("data_width")
        if len(sub) >= 2:
            m, _ = np.polyfit(sub["data_width"].astype(float),
                              sub[metric].astype(float), 1)
            dw_slopes.append(float(m))
    if dw_slopes:
        rows.append({
            "name": "PORT: area per added bit of data_width (fixed fw, vc)",
            "median_um2": median(dw_slopes),
            "per_unit_um2": median(dw_slopes),
            "n": len(dw_slopes),
            "note": "median of per-(fw,vc) linear fits",
        })

    # AFFINE / ITERATION: area per added dim level at fixed everything else.
    for exp, hue_col in [("AFFINE_PATTERN_GENERATOR_EXP", "msw"),
                        ("ITERATION_DOMAIN_EXP", "me")]:
        sub = df[df["experiment"] == exp].dropna(subset=[metric, "dim", hue_col])
        slopes = []
        for _, g in sub.groupby(hue_col):
            g = g.sort_values("dim")
            if len(g) >= 2:
                m, _ = np.polyfit(g["dim"].astype(float),
                                  g[metric].astype(float), 1)
                slopes.append(float(m))
        if slopes:
            rows.append({
                "name": f"{exp}: area per added dim level (fixed {hue_col})",
                "median_um2": median(slopes),
                "per_unit_um2": median(slopes),
                "n": len(slopes),
                "note": f"median of per-{hue_col} linear fits",
            })

    # MEMORY: area per bit of storage capacity, per port-config.
    cap_slopes = []
    cap_rows = []
    for (fw, inp, outp), g in mem.groupby(["fw", "inp", "outp"], dropna=False):
        g = g.dropna(subset=["storage_cap"]).sort_values("storage_cap")
        if len(g) >= 2:
            m, _ = np.polyfit(g["storage_cap"].astype(float),
                              g[metric].astype(float), 1)
            cap_slopes.append(float(m))
            cap_rows.append((fw, inp, outp, m))
    if cap_slopes:
        rows.append({
            "name": "MEMORY: area per added bit of storage_cap (per port-config)",
            "median_um2": median(cap_slopes),
            "per_unit_um2": median(cap_slopes),
            "n": len(cap_slopes),
            "note": "lower slope ⇒ more area-efficient macro at that port-config",
        })

    return rows


def format_derived(rows: list[dict]) -> str:
    if not rows:
        return ""
    out = ["", "## Derived ratios", ""]
    for r in rows:
        out.append(f"- **{r['name']}**")
        out.append(f"  - median Δarea: **{r['median_um2']:,.2f} µm²**  "
                   f"(n={r['n']})")
        if r["per_unit_um2"] is not None:
            out.append(f"  - per-unit:    **{r['per_unit_um2']:,.4f} µm²/unit**")
        if r.get("note"):
            out.append(f"  - note: {r['note']}")
    return "\n".join(out)


def _save(fig, stem: Path, formats: list[str]) -> None:
    stem.parent.mkdir(parents=True, exist_ok=True)
    for ext in formats:
        fig.savefig(stem.with_suffix(f".{ext}"), dpi=150)


def _save_all(fig, name: str, figdir: Path, formats: list[str],
              all_dir: str | None, all_pdf_dir: str | None) -> list[Path]:
    """Write one figure to the per-section dir, the ALL aggregate, and the
    PDF-only aggregate. Returns the list of stems written."""
    stems = [figdir / DERIVED_DIRNAME / name]
    if all_dir:
        stems.append(figdir / all_dir / f"{DERIVED_DIRNAME}__{name}")
    _save(fig, stems[0], formats)
    if all_dir:
        _save(fig, stems[1], formats)
    if all_pdf_dir and "pdf" in formats:
        pdf_stem = figdir / all_pdf_dir / f"{DERIVED_DIRNAME}__{name}"
        _save(fig, pdf_stem, ["pdf"])
        stems.append(pdf_stem)
    return stems


def plot_slopes_by_param(fit_rows: list[dict], metric: str,
                         figdir: Path, formats: list[str],
                         all_dir: str | None, all_pdf_dir: str | None) -> list[Path]:
    """Bar chart of |median slope| per (experiment, param). Log-x because
    the slopes span ~6 decades (0.02 µm²/unit for msw vs 7800 for fw)."""
    groups: dict[tuple, list[float]] = {}
    for r in fit_rows:
        groups.setdefault((r["experiment"], r["param"]), []).append(r["slope_um2_per_unit"])

    items = sorted(groups.items(), key=lambda kv: median(kv[1]))
    labels = [f"{exp}\n{p}" for (exp, p), _ in items]
    medians = [median(v) for _, v in items]
    mins = [min(v) for _, v in items]
    maxs = [max(v) for _, v in items]
    abs_med = [abs(m) for m in medians]
    colors = ["tab:blue" if m >= 0 else "tab:red" for m in medians]

    fig, ax = plt.subplots(figsize=(9.0, 0.45 * len(labels) + 1.5))
    y = np.arange(len(labels))
    ax.barh(y, abs_med, color=colors, edgecolor="black", linewidth=0.4)
    # Range whiskers in absolute value space (so the log axis stays clean).
    for yi, lo, hi in zip(y, mins, maxs):
        ax.plot([abs(lo), abs(hi)], [yi, yi], color="black", linewidth=0.8, alpha=0.6)
    ax.set_yticks(y)
    ax.set_yticklabels(labels, fontsize=8)
    ax.set_xscale("log")
    ax.set_xlabel(f"|median slope|  (µm² per unit of param)  —  metric: {metric}")
    ax.set_title("Per-(experiment, param) linear-fit slopes\n"
                 "blue = positive (more param ⇒ more area),  red = negative")
    ax.grid(True, axis="x", which="both", alpha=0.3)
    fig.tight_layout()
    stems = _save_all(fig, "slopes_by_param", figdir, formats, all_dir, all_pdf_dir)
    plt.close(fig)
    return stems


def plot_derived_ratios(rows: list[dict], metric: str,
                        figdir: Path, formats: list[str],
                        all_dir: str | None, all_pdf_dir: str | None) -> list[Path]:
    """Bar chart of the named derived per-unit ratios."""
    pruned = [r for r in rows if r.get("per_unit_um2") is not None]
    if not pruned:
        return []
    pruned = sorted(pruned, key=lambda r: abs(r["per_unit_um2"]))
    labels = [r["name"] for r in pruned]
    vals = [r["per_unit_um2"] for r in pruned]
    abs_vals = [abs(v) for v in vals]
    colors = ["tab:blue" if v >= 0 else "tab:red" for v in vals]

    fig, ax = plt.subplots(figsize=(11.0, 0.5 * len(labels) + 1.5))
    y = np.arange(len(labels))
    bars = ax.barh(y, abs_vals, color=colors, edgecolor="black", linewidth=0.4)
    ax.set_yticks(y)
    ax.set_yticklabels(labels, fontsize=8)
    ax.set_xscale("log")
    ax.set_xlabel(f"|per-unit cost|  (µm²/unit)  —  metric: {metric}")
    ax.set_title("Derived per-unit area ratios\n"
                 "blue = positive,  red = negative (e.g. fw doubling shrinks SRAM)")
    for bar, v in zip(bars, vals):
        ax.text(bar.get_width() * 1.05, bar.get_y() + bar.get_height() / 2,
                f"{v:,.2f}", va="center", fontsize=7)
    ax.grid(True, axis="x", which="both", alpha=0.3)
    fig.tight_layout()
    stems = _save_all(fig, "derived_ratios", figdir, formats, all_dir, all_pdf_dir)
    plt.close(fig)
    return stems


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("csv", type=Path)
    ap.add_argument("-o", "--output", type=Path,
                    help="Optional per-group fits CSV (every row preserved).")
    ap.add_argument("--metric", default=DEFAULT_METRIC, choices=METRIC_COLS)
    ap.add_argument("--figdir", type=Path,
                    default=Path(__file__).resolve().parent / "figs",
                    help="Where derived figures land (mirrors plot_power_area.py).")
    ap.add_argument("--format", dest="formats", nargs="+", default=["png", "pdf"],
                    choices=["png", "pdf", "svg"])
    ap.add_argument("--all-dir", default="ALL",
                    help="Aggregate dir for every format. Empty string disables.")
    ap.add_argument("--all-pdf-dir", default="ALL_PDF",
                    help="PDF-only aggregate dir. Empty string disables.")
    ap.add_argument("--no-plots", action="store_true",
                    help="Skip figure generation; just print the table + CSV.")
    args = ap.parse_args(argv)

    if not args.csv.is_file():
        print(f"error: {args.csv} not found", file=sys.stderr)
        return 2

    df = pd.read_csv(args.csv)
    fits = compute_fits(df, args.metric)
    if not fits:
        print(f"error: no fits possible (metric={args.metric} empty?)",
              file=sys.stderr)
        return 1

    derived = derived_ratios(df, args.metric)

    print(f"# Scaling stats for `{args.metric}`")
    print()
    print("## Per-(experiment, param) linear fits")
    print()
    print(summary_table(fits))
    print(format_derived(derived))

    if args.output:
        pd.DataFrame(fits).to_csv(args.output, index=False)
        print(f"\nwrote per-group fits → {args.output}", file=sys.stderr)

    if not args.no_plots:
        all_dir = args.all_dir or None
        all_pdf_dir = args.all_pdf_dir or None
        for stem in plot_slopes_by_param(fits, args.metric, args.figdir,
                                         args.formats, all_dir, all_pdf_dir):
            print(f"wrote {stem}.{{{','.join(args.formats)}}}", file=sys.stderr)
        for stem in plot_derived_ratios(derived, args.metric, args.figdir,
                                        args.formats, all_dir, all_pdf_dir):
            print(f"wrote {stem}.{{{','.join(args.formats)}}}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
