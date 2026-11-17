"""Standalone figure (not in the thesis): what the RV Memory Tile's filter path costs.

The thesis RV MemTile has no filter path (user decision 2026-10-26, CLAUDE.md §6). This
figure is the A/B behind that, from the final run (user 2026-11-16: build it, outside the
thesis):

  (a) garnet ``sweep_specs.py --recipe thesis_filter_ab``: five RV geometries built with
      a FIFO-8 filter path (``_ff8_``) and without (``_nofp_``), through PnR. PnR tile
      area and idle / active power of ff8 over nofp (``correlation.csv``,
      ``memtile_power.csv``).
  (b) garnet ``cgra_app_power.py`` on conv_3_3 (``cgra_power.csv``): A0 on the nofp tile,
      A on the ff8 tile with the filter path unused, B reading its newest row through it
      ("eat rows up front"). Whole-CGRA app power by tile group, as % of A0 on the same
      geometry (the d_*_pct columns are relative to each config's first variant, so they
      are not used).

Every value is relative: tile area and power include the SRAM macros (the
``tile_figures.SRAM_NDA`` convention). Nothing in the thesis build reads this module::

    python3 -m THESIS.pipeline.filter_path_ab [--tiles <thesis_filter_ab zip|dir>]
        [--app <cgra_app_power_thesis_fp_ab zip|dir>] [-o out.pdf]

Defaults: the zips in ``../thesis_final_data/``; output
``THESIS/output/standalone/filter_path_ab.pdf`` (untracked, like all of THESIS/output).
"""

from __future__ import annotations

import argparse
import io
import re
import sys
import zipfile
from pathlib import Path

import numpy as np
import pandas as pd

from .errors import MissingDataError

REPO_ROOT = Path(__file__).resolve().parents[2]
DATA = REPO_ROOT.parent / "thesis_final_data"
TILES_SRC = DATA / "thesis_filter_ab_mem_latest_slim.zip"
APP_SRC = DATA / "cgra_app_power_thesis_fp_ab_latest.zip"
OUT = REPO_ROOT / "THESIS" / "output" / "standalone" / "filter_path_ab.pdf"

FP, NOFP = "_ff8_portcg", "_nofp_portcg"
# (column, label) of panel (a): PnR level, ff8 over nofp
TILE_METRICS = [("tile_pnr_total_area", "Area"), ("pnr_idle_total_power", "Idle power"),
                ("pnr_active_total_power", "Active power")]
VARIANTS = [("conv_A0", "A0"), ("conv_A", "A"), ("conv_B", "B")]
# (cgra_power.csv column, label, color) of panel (b), stacked bottom to top: MEM tiles
# blue, PE tiles orange, routing-only tiles light
GROUPS = [("Tile_MemCore_placed_W", "MEM placed", "#1c5cab"), ("Tile_MemCore_route_W", "MEM route", "#86b6ef"),
          ("Tile_PE_placed_W", "PE placed", "#eb6834"), ("Tile_PE_route_W", "PE route", "#f5b597")]


def read_csv(src: Path, name: str) -> pd.DataFrame:
    """``name`` from a sweep / cgra_app_power zip (one directory deep) or out-dir."""
    src = Path(src)
    if src.is_dir():
        hits = [src / name] if (src / name).is_file() else sorted(src.glob(f"*/{name}"))
        if not hits:
            raise MissingDataError(f"no {name} in {src}")
        return pd.read_csv(hits[0])
    if not src.is_file():
        raise MissingDataError(f"{src} not found: copy the final run's zips to {DATA}")
    with zipfile.ZipFile(src) as z:
        hits = sorted(n for n in z.namelist() if n == name or re.fullmatch(rf"[^/]+/{re.escape(name)}", n))
        if not hits:
            raise MissingDataError(f"no {name} in {src}")
        return pd.read_csv(io.BytesIO(z.read(hits[0])))


def geometry(config: str) -> str:
    """fw4_dw16_sc8192_sp_in2_out2_vc2_ff8_portcg_thesis_rv -> "fw4 dw16\\n8KB SP\\n2x2"."""
    m = re.match(r"fw(\d+)_dw(\d+)_sc(\d+)_(sp|dp)_in(\d+)_out(\d+)", config)
    if not m:
        return config
    fw, dw, sc, port, n_in, n_out = m.groups()
    return f"fw{fw} dw{dw}\n{int(sc) // 1024}KB {port.upper()}\n{n_in}x{n_out}"


def _base(config: str) -> str:
    return config.replace(FP, "").replace(NOFP, "")


def _order(base: str) -> tuple:
    m = re.match(r"fw(\d+)_dw(\d+)_sc(\d+)", base)
    fw, dw, sc = (int(v) for v in m.groups()) if m else (0, 0, 0)
    return dw, fw, sc, base


def tile_deltas(correlation: pd.DataFrame, power: pd.DataFrame) -> pd.DataFrame:
    """Per geometry (index: base config, sorted by data width, fetch width, capacity):
    % change of each TILE_METRICS column, filter path over none."""
    cols = [c for c, _ in TILE_METRICS]
    d = correlation.set_index("config_name").join(power.set_index("config_name"), rsuffix="_pw")
    missing = [c for c in cols if c not in d]
    if missing:
        raise MissingDataError(f"tile tables lack {', '.join(missing)}")
    d = d[cols].apply(pd.to_numeric, errors="coerce")
    fp = d[d.index.str.contains(FP)].rename(index=_base)
    no = d[d.index.str.contains(NOFP)].rename(index=_base)
    both = sorted(set(fp.index) & set(no.index), key=_order)
    if not both:
        raise MissingDataError("no geometry built both with (_ff8_) and without (_nofp_) the filter path")
    return (fp.loc[both] / no.loc[both] - 1) * 100


def app_shares(cgra_power: pd.DataFrame) -> dict[str, pd.DataFrame]:
    """Per geometry (base config, sorted): rows A0/A/B, columns GROUPS' labels, each tile
    group's power as % of A0's app power on that geometry."""
    df = cgra_power.copy()
    df["base"] = df["config"].map(_base)
    out = {}
    for base in sorted(df["base"].unique(), key=_order):
        g = df[df["base"] == base].set_index("variant")
        if not all(v in g.index for v, _ in VARIANTS):
            continue
        ref = float(g.loc["conv_A0", "app_W"])
        rows = pd.DataFrame({lab: [g.loc[v, col] / ref * 100 for v, _ in VARIANTS] for col, lab, _ in GROUPS},
                            index=[lab for _, lab in VARIANTS])
        rows["total"] = [g.loc[v, "app_W"] / ref * 100 for v, _ in VARIANTS]
        rows["kernel_cycles"] = [g.loc[v, "kernel_cycles"] for v, _ in VARIANTS]
        out[base] = rows
    if not out:
        raise MissingDataError("no geometry with all of conv_A0, conv_A and conv_B in cgra_power.csv")
    return out


def draw(tiles: pd.DataFrame, apps: dict[str, pd.DataFrame], outpath: Path) -> None:
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt
    from matplotlib import transforms
    from .tile_figures import FULL_W, INK2, SERIES, _ax_style, _save

    fig, (ta, aa) = plt.subplots(2, 1, figsize=(FULL_W, 5.4))
    legend_kw = dict(loc="lower left", bbox_to_anchor=(0, 1.0), handlelength=1.0, columnspacing=0.8,
                     handletextpad=0.3, borderaxespad=0.2)
    # (a) the tile: filter path over none, per geometry
    idx = np.arange(len(tiles))
    w = 0.84 / len(TILE_METRICS)
    for i, ((col, lab), color) in enumerate(zip(TILE_METRICS, (SERIES[2], SERIES[3], SERIES[6]))):
        xs = idx - 0.42 + w * (i + 0.5)
        bars = ta.bar(xs, tiles[col].to_numpy(dtype=float), width=w, color=color, label=lab)
        ta.bar_label(bars, fmt="%+.1f", fontsize=7.5, padding=1)
    ta.axhline(0, color=INK2, lw=0.8)
    ta.set_xticks(idx)
    ta.set_xticklabels([geometry(b) for b in tiles.index], fontsize=9)
    ta.set_ylabel("FIFO-8 filter path\nvs none (%)")
    ta.set_title("(a) RV Memory Tile after PnR: what the filter path adds", loc="left", pad=24)
    ta.legend(ncol=len(TILE_METRICS), **legend_kw)
    _ax_style(ta)
    ta.margins(y=0.14)

    # (b) conv_3_3 on the CGRA: A0 / A / B per geometry, stacked by tile group
    gap = 1.0
    xs = np.array([k * (len(VARIANTS) + gap) + j for k, rows in enumerate(apps.values())
                   for j in range(len(rows))], dtype=float)
    stack = pd.concat(apps.values())
    bottom = np.zeros(len(stack))
    for _, lab, color in GROUPS:
        vals = stack[lab].to_numpy(dtype=float)
        aa.bar(xs, vals, bottom=bottom, width=0.75, color=color, label=lab)
        bottom += vals
    for x, tot in zip(xs, stack["total"]):
        aa.text(x, tot + 1.5, f"{tot:.1f}", ha="center", va="bottom", fontsize=8.5)
    aa.set_xticks(xs)
    aa.set_xticklabels(list(stack.index), fontsize=9.5)
    under = transforms.blended_transform_factory(aa.transData, aa.transAxes)
    for k, base in enumerate(apps):
        mid = k * (len(VARIANTS) + gap) + (len(VARIANTS) - 1) / 2
        aa.text(mid, -0.16, geometry(base).replace("\n", " "), transform=under, ha="center",
                va="top", fontsize=9.5)
    aa.set_ylabel("Power\n(% of A0)")
    aa.set_title("(b) conv_3_3 on the RV CGRA: app power by tile group", loc="left", pad=24)
    aa.legend(ncol=len(GROUPS), **legend_kw)
    _ax_style(aa)
    aa.set_ylim(0, max(stack["total"]) * 1.12)

    cycles = sorted({int(c) for rows in apps.values() for c in rows["kernel_cycles"]})
    same = (f"all {cycles[0]:,} kernel cycles, so energy follows power" if len(cycles) == 1
            else f"kernel cycles {', '.join(f'{c:,}' for c in cycles)}")
    fig.text(0.0, 0.0, "A0: no filter path.  A: FIFO-8 filter path, unused.  B: conv_3_3 reads its newest "
             f"row through it.\nSynth-level power of the tiles the app places and routes through; {same}.",
             ha="left", va="top", fontsize=8, color=INK2, transform=fig.transFigure)
    fig.tight_layout(h_pad=1.5)
    _save(fig, outpath)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--tiles", type=Path, default=TILES_SRC, help="thesis_filter_ab sweep zip or dir")
    ap.add_argument("--app", type=Path, default=APP_SRC, help="cgra_app_power_thesis_fp_ab zip or dir")
    ap.add_argument("-o", "--out", type=Path, default=OUT)
    args = ap.parse_args(argv)
    tiles = tile_deltas(read_csv(args.tiles, "correlation.csv"), read_csv(args.tiles, "memtile_power.csv"))
    apps = app_shares(read_csv(args.app, "cgra_power.csv"))
    draw(tiles, apps, args.out)
    with pd.option_context("display.float_format", "{:+.1f}".format):
        print(tiles.rename(columns=dict(TILE_METRICS)).to_string(), file=sys.stderr)
    for base, rows in apps.items():
        print(f"{base}: app power % of A0 " + ", ".join(f"{v} {t:.1f}" for v, t in rows["total"].items()),
              file=sys.stderr)
    print(f"-> {args.out}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
