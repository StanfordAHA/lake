"""Switch-box (SB) area of the thesis PE and Memory Tiles, static vs ready/valid (RV),
by part, for fig:interconnect_fifo_replace_ff and fig:interconnect_fork_ready_valid_data.

Source: the final run's Genus hierarchical area reports (``final_area.rpt``, cell area in
µm², hierarchy kept), at the four data widths of garnet's ``thesis_tiles_pe`` recipe:

  PE Tile      thesis_tiles_pe_latest_slim.zip   <cfg>/<N>-cadence-genus-synthesis/...
  Memory Tile  thesis_dse_mem_latest_slim.zip    the same <cfg> names (the MEM spec each
                                                 PE tile was built with)

<cfg> = fw4_dw{dw}_sc{cap}_sp_in2_out2_vc2_ + sharectr_portcg_thesis (static) or
nofp_portcg_thesis_rv (RV). Synthesis rather than PnR: Genus keeps the SB's children
(the PnR report folds the fork logic into the SB), and the Memory Tile set has no PnR
build at every width.

Each tile has two SBs (``SB_ID0_5TRACKS_B{dw}_*`` and ``..._B1_*``). Their direct
children, by canal's instance names (canal/circuit.py):

  regs       REG_T*: the 40 track (pipeline) registers, a flip-flop register in static
             and a depth-2 FIFO (FifoRegWrapper fifo_only) in RV
  muxes      MUX_SB_*, RMUX_*, mux_aoi_*: output and register-bypass muxes (RV: + valid)
  config_regs  config_reg_*: configuration registers (RV: + per-mux enables, FIFO init)
  fan_in     *_fan_in (InclusiveNodeFanout): the fork's ready reduction, a producer is
             ready when every enabled sink that selects it is
  fanout     *_FANOUT (ExclusiveNodeFanout): ready back to an SB mux from its register or
             the bypass, whichever the register mux selects
  ands       and*_inst*: one clock-enable AND per register, plus in RV one valid-gating
             AND per SB input -> SB output (valid only when the fan-in reduction is ready)
  other      anything else, and the SB's own logic outside its children

and, at tile level (outside the SBs): valid-gating ANDs into the connection boxes
(``and<k>_inst<n>``, RV), ``*_loopback`` (ReadyValidLoopBack on core outputs) and the
power-domain pass's ``*_ready_and`` (garnet passes/power_domain: an SB output's ready
ANDed with its enable; isolation, not fork logic).

Fork logic (fork_area) = fan_in + the RV-only valid-gating ANDs (the SB's ANDs beyond the
static build's clock-enable count) + the tile-level valid-gating ANDs + loopbacks.

    python3 -m THESIS.pipeline.sb_ppa [--pe <zip|dir>] [--mem <zip|dir>]
writes THESIS/data/sb_ppa/sb_area.csv (what ``load()`` reads).
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

import pandas as pd

from .errors import MissingDataError
from .tile_sweep import _Src

REPO_ROOT = Path(__file__).resolve().parents[2]
DATA = REPO_ROOT.parent / "thesis_final_data"
PE_SRC = DATA / "thesis_tiles_pe_latest_slim.zip"
MEM_SRC = DATA / "thesis_dse_mem_latest_slim.zip"
OUT = REPO_ROOT / "THESIS" / "data" / "sb_ppa" / "sb_area.csv"

MODES = {"sharectr_portcg_thesis": "static", "nofp_portcg_thesis_rv": "rv"}
_ROW = re.compile(r"^( *)(\S+)\s+(\S+)\s+(\d+)\s+([\d.]+)\s+([\d.]+)\s+([\d.]+)\s*$")
_SB_KIDS = [("regs", r"REG_T\d+_\w+_B\d+"), ("muxes", r"(MUX_SB_\w+|RMUX_\w+|mux_aoi_\w+)"),
            ("config_regs", r"config_reg_\d+"), ("fan_in", r"\w+_fan_in"),
            ("fanout", r"\w+_FANOUT"), ("ands", r"and\d*_inst\d+")]
PARTS = [name for name, _ in _SB_KIDS] + ["other"]


def genus_tree(text: str) -> dict[str, tuple[str, float]]:
    """Genus final_area.rpt -> {path ('' = top, else 'a/b'): (module, cell area µm²)}."""
    out, stack = {}, []
    for line in text.splitlines():
        m = _ROW.match(line)
        if not m:
            continue
        depth = len(m.group(1)) // 2
        stack = stack[:depth] + [m.group(2)]
        out["/".join(stack[1:])] = (m.group(3), float(m.group(5)))
    return out


def tile_parts(tree: dict[str, tuple[str, float]]) -> dict:
    """One tile's SB parts (summed over its SBs) and tile-level handshake gates."""
    if "" not in tree:
        raise MissingDataError("Genus area report has no top row")
    row = {"tile_area": tree[""][1], "sb_area": 0.0, **{p: 0.0 for p in PARTS},
           "ands_n": 0, "regs_dw_n": 0, "regs_dw_area": 0.0, "regs_1b_n": 0, "regs_1b_area": 0.0,
           "tile_valid_ands": 0.0, "loopbacks": 0.0, "pd_ready_ands": 0.0}
    sbs = [p for p in tree if p and "/" not in p and p.startswith("SB_ID0_")]
    if not sbs:
        raise MissingDataError("no SB_ID0_* switch box in the Genus area report (hierarchy flattened?)")
    for sb in sbs:
        row["sb_area"] += tree[sb][1]
        kids = 0.0
        for path, (_, area) in tree.items():
            if not path.startswith(sb + "/") or path.count("/") != 1:
                continue
            leaf = path.split("/")[1]
            kids += area
            part = next((n for n, pat in _SB_KIDS if re.fullmatch(pat, leaf)), "other")
            row[part] += area
            if part == "ands":
                row["ands_n"] += 1
            if part == "regs":
                w = int(re.search(r"_B(\d+)$", leaf).group(1))
                key = "regs_1b" if w == 1 else "regs_dw"
                row[key + "_n"] += 1
                row[key + "_area"] += area
        row["other"] += tree[sb][1] - kids
    for path, (_, area) in tree.items():
        if not path or "/" in path or path.startswith("SB_ID0_"):
            continue
        if re.fullmatch(r"and\d+_inst\d+", path):
            row["tile_valid_ands"] += area
        elif path.endswith("_loopback"):
            row["loopbacks"] += area
        elif path.endswith("_ready_and"):
            row["pd_ready_ands"] += area
    return row


def _config_dw(config: str) -> int:
    m = re.match(r"fw\d+_dw(\d+)_", config)
    return int(m.group(1)) if m else 0


def ingest(pe_src: Path = PE_SRC, mem_src: Path = MEM_SRC) -> pd.DataFrame:
    """One row per (tile, mode, data width): SB parts and tile-level gates (µm²)."""
    pe = _Src(Path(pe_src))
    configs = [c for c in pe.configs() if any(c.endswith("_" + s) for s in MODES)]
    if not configs:
        raise MissingDataError(f"no thesis PE tile configs in {pe_src}")
    rows = []
    for tile, src in (("PE", pe), ("MEM", _Src(Path(mem_src)))):
        for cfg in configs:
            text = src.read(f"{src.step(cfg, 'cadence-genus-synthesis')}/results_syn/final_area.rpt")
            if text is None:
                print(f"sb_ppa: no Genus area report for {tile} {cfg}; skipped", file=sys.stderr)
                continue
            mode = next(m for s, m in MODES.items() if cfg.endswith("_" + s))
            rows.append({"tile": tile, "mode": mode, "data_width": _config_dw(cfg), "config": cfg,
                         **tile_parts(genus_tree(text))})
    df = pd.DataFrame(rows)
    if df.empty:
        raise MissingDataError("no switch-box rows")
    # RV adds one valid-gating AND per SB input -> output beside the static build's
    # clock-enable ANDs (one per register): the ANDs past the static count are fork logic
    static_n = df[df["mode"] == "static"].set_index(["tile", "data_width"])["ands_n"]
    ce_n = df.set_index(["tile", "data_width"]).index.map(lambda k: static_n.get(k, 0))
    per_and = df["ands"] / df["ands_n"].where(df["ands_n"] > 0)
    extra = (df["ands_n"] - pd.Series(ce_n, index=df.index)).clip(lower=0)
    df["valid_ands"] = (per_and * extra).fillna(0.0).where(df["mode"] == "rv", 0.0)
    df["fork_area"] = df["fan_in"] + df["valid_ands"] + df["tile_valid_ands"] + df["loopbacks"]
    df["regs_dw_each"] = df["regs_dw_area"] / df["regs_dw_n"]
    df["regs_1b_each"] = df["regs_1b_area"] / df["regs_1b_n"]
    return df.sort_values(["tile", "data_width", "mode"], ascending=[False, True, False]).reset_index(drop=True)


def load(path: Path | None = None) -> pd.DataFrame:
    path = Path(path) if path else OUT
    if not path.is_file():
        raise MissingDataError(f"no switch-box data at {path} -- run `python3 -m THESIS.pipeline.sb_ppa` first")
    return pd.read_csv(path)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--pe", type=Path, default=PE_SRC, help="thesis_tiles_pe zip or dir")
    ap.add_argument("--mem", type=Path, default=MEM_SRC, help="thesis_dse zip or dir")
    ap.add_argument("-o", "--out", type=Path, default=OUT)
    args = ap.parse_args(argv)
    df = ingest(args.pe, args.mem)
    args.out.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(args.out, index=False)
    cols = ["tile", "mode", "data_width", "tile_area", "sb_area", "regs", "regs_dw_each", "muxes",
            "config_regs", "fork_area", "pd_ready_ands"]
    print(df[cols].round(1).to_string(index=False), file=sys.stderr)
    print(f"-> {args.out}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
