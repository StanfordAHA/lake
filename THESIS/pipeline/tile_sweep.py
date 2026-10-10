"""Tile_MemCore lake-spec sweep (garnet ``mflowgen/sweep_specs.py``) → tidy
DataFrame for the tile-level thesis figures.

The sweep builds every spec of the standalone thesis synthesis set
(``ASPLOS_EXP/all_experiments_thesis_v2.sh``) as a full CGRA memory tile
(lakespec + MemCore wrapper + SB/CB interconnect), in static and RV runtime
modes, through Genus synthesis; a subset continues through Innovus signoff.
Its ``--zip`` archive holds per-config Genus/Innovus reports, which this
module parses directly (zip or extracted dir) into one row per config.

Ingest once, then the generators in ``generators.py`` read the cached CSV::

    python3 -m THESIS.pipeline.tile_sweep /path/to/tile_memcore_thesis_<host>_<ts>.zip

writes ``THESIS/data/tile_sweep/<sweep>.csv`` and copies it to
``THESIS/data/tile_sweep/latest.csv`` (what ``load()`` reads).

Area columns are µm² from Genus ``final_gates.rpt`` (``syn_*``) and Innovus
``signoff.area.rpt`` (``pnr_*``). ``*_std`` = cell area minus that tool's own
SRAM macro area. ``syn_cb``/``syn_sb``/``syn_memcore`` split the tile by its
top-level hierarchy (connection boxes, switch box, memory core); ``syn_top``
is everything else (feature decode, power-domain config, top-level glue).
"""

from __future__ import annotations

import argparse
import io
import json
import re
import shutil
import sys
import zipfile
from pathlib import Path

import pandas as pd

from .errors import MissingDataError

DATA_DIR = Path(__file__).resolve().parents[1] / "data" / "tile_sweep"
LATEST = DATA_DIR / "latest.csv"

GATE_TYPES = {
    "timing_model": "sram", "sequential": "seq", "logic": "logic",
    "inverter": "inv", "buffer": "buf", "clock_gating_integrated_cell": "cg",
}

# Defaults build_spec applies to keys a spec point leaves out.
SPEC_DEFAULTS = dict(storage_capacity=4096, data_width=16, vec_width=4,
                     in_ports=2, out_ports=2, dual_port=False, vec_capacity=2,
                     dims=6, max_extent=None, max_sequence_width=None)
KEY = ("storage_capacity", "data_width", "vec_width", "in_ports", "out_ports",
       "dual_port", "vec_capacity", "dims", "max_extent", "max_sequence_width")


def experiment_grids() -> list[tuple[str, dict]]:
    """(experiment, spec) for every point of the thesis spec set, mirroring
    garnet ``sweep_specs._thesis_spec_points`` + ``DEFAULT_SPEC_POINTS``."""
    pts: list[tuple[str, dict]] = []
    add = lambda exp, **kw: pts.append((exp, kw))  # noqa: E731
    # DEFAULT_SPEC_POINTS (the PnR anchors)
    add("DEFAULT", storage_capacity=8192, data_width=16, vec_width=4, in_ports=2, out_ports=2)
    add("DEFAULT", storage_capacity=4096, data_width=16, vec_width=1, in_ports=1, out_ports=1)
    add("DEFAULT", storage_capacity=2048, data_width=16, vec_width=2, in_ports=1, out_ports=1)
    add("DEFAULT", storage_capacity=32768, data_width=16, vec_width=8, in_ports=4, out_ports=4)
    add("DEFAULT", storage_capacity=4096, data_width=16, vec_width=2, dual_port=True,
        in_ports=2, out_ports=2)
    add("DEFAULT", storage_capacity=8192, data_width=16, vec_width=4, dual_port=True,
        in_ports=4, out_ports=4)
    # PORT_EXP
    for dw in (8, 16, 32):
        add("PORT_EXP", storage_capacity=8192, data_width=dw, vec_width=4, in_ports=2, out_ports=2)
    for fw in (2, 4, 8):
        for vc in (2, 4, 8):
            for dw in (8, 16):
                add("PORT_EXP", storage_capacity=8192, data_width=dw, vec_width=fw,
                    vec_capacity=vc, in_ports=2, out_ports=2)
    for fw in (2, 4):
        for vc in (2, 4, 8):
            add("PORT_EXP", storage_capacity=8192, data_width=32, vec_width=fw,
                vec_capacity=vc, in_ports=2, out_ports=2)
    for vc in (2, 4, 8):
        add("PORT_EXP", storage_capacity=8192, data_width=64, vec_width=2,
            vec_capacity=vc, in_ports=2, out_ports=2)
    # ITERATION_DOMAIN_EXP / AFFINE_PATTERN_GENERATOR_EXP
    for dims in range(1, 7):
        for me in (64, 256, 1024, 4096):
            add("ITERATION_DOMAIN_EXP", storage_capacity=8192, data_width=16, vec_width=1,
                dims=dims, max_extent=me, in_ports=2, out_ports=2)
        for msw in (64, 256, 1024, 4096, 16384):
            add("AFFINE_PATTERN_GENERATOR_EXP", storage_capacity=8192, data_width=16,
                vec_width=1, dims=dims, max_sequence_width=msw, in_ports=2, out_ports=2)
    # MEMORY_EXP: (fetch width, dual port, ports, capacities)
    for fw, dp, ports, caps in (
            (1, True, 1, (1024, 2048, 4096, 8192, 16384)),
            (2, True, 2, (1024, 2048, 4096, 8192, 16384)),
            (4, True, 4, (1024, 2048, 4096, 8192)),
            (2, False, 1, (2048, 4096, 8192, 16384, 32768)),
            (4, False, 2, (4096, 8192, 16384, 32768)),
            (8, False, 4, (8192, 16384, 32768))):
        for sc in caps:
            add("MEMORY_EXP", storage_capacity=sc, data_width=16, vec_width=fw,
                dual_port=dp, in_ports=ports, out_ports=ports)
    return pts


def _norm(v):
    if v is None or (isinstance(v, float) and pd.isna(v)) or v == "":
        return None
    if isinstance(v, str) and v in ("True", "False"):
        return v == "True"
    if isinstance(v, (bool,)):
        return v
    return int(float(v))


def _key(spec: dict) -> tuple:
    given = {k: v for k, v in spec.items() if _norm(v) is not None}
    full = {**SPEC_DEFAULTS, **given}
    return tuple(_norm(full[k]) for k in KEY)


# ---- report parsers ----------------------------------------------------------


def _gates(text: str) -> dict:
    blk = text[text.rfind(" Type "):]
    out = {}
    for ty, col in GATE_TYPES.items():
        m = re.search(rf"^{ty}\s+(\d+)\s+([\d.]+)", blk, re.M)
        out[f"syn_{col}"] = float(m.group(2)) if m else 0.0
        out[f"syn_{col}_n"] = int(m.group(1)) if m else 0
    m = re.search(r"^total\s+(\d+)\s+([\d.]+)", blk, re.M)
    out["syn_cell"], out["syn_inst"] = float(m.group(2)), int(m.group(1))
    macros = re.findall(r"^IN12LP_(S\w+?)_H\s+(\d+)\s+[\d.]+", text, re.M)
    out["sram_macro"] = "; ".join(f"{n} x{c}" for n, c in macros)
    out["sram_count"] = sum(int(c) for _, c in macros)
    return out


def _hier(text: str) -> dict:
    """Top-level children of Tile_MemCore from Genus final_area.rpt."""
    sums = {"syn_cb": 0.0, "syn_sb": 0.0, "syn_memcore": 0.0, "syn_other_children": 0.0}
    for line in text.splitlines():
        m = re.match(r"^  (\S+)\s+\S+\s+(\d+)\s+([\d.]+)\s+[\d.]+\s+[\d.]+\s*$", line)
        if not m:
            continue
        name, area = m.group(1), float(m.group(3))
        if name.startswith("CB_"):
            sums["syn_cb"] += area
        elif name.startswith("SB_"):
            sums["syn_sb"] += area
        elif name == "MemCore_inst0":
            sums["syn_memcore"] += area
        else:
            sums["syn_other_children"] += area
    return sums


def _qos(text: str) -> dict:
    t = text[text.rfind("QoS Summary for"):]
    v = t[t.find("Slack (ps):"):]
    v = v[: v.find("TNS")]
    num = lambda s: float(s.replace(",", ""))  # noqa: E731
    out = {"syn_wns_ps": num(re.search(r"Slack \(ps\):\s+(-?[\d,]+)", v).group(1))}
    for c in ("R2R", "I2R", "R2O", "I2O", "CG"):
        m = re.search(rf"{c}\s+\(ps\):\s+(-?[\d,]+)", v)
        out[f"syn_slack_{c}_ps"] = num(m.group(1)) if m else None
    m = re.search(r"Real Runtime \(h:m:s\):\s+(\d+):(\d+):(\d+)", t)
    out["syn_runtime_min"] = (int(m.group(1)) * 60 + int(m.group(2)) + int(m.group(3)) / 60) if m else None
    return out


def _clock(qor: str) -> dict:
    m = re.search(r"ideal_clock\s+([\d.]+)\s*$", qor, re.M)
    return {"clock_period_ps": float(m.group(1)) if m else None}


def _pnr_area(text: str) -> dict:
    out = {}
    names = ["n", "cell", "buf", "inv", "comb", "flop", "latch", "cg", "macro", "phys"]
    for line in text.splitlines():
        f = line.split()
        if not f:
            continue
        if f[0] == "Tile_MemCore":
            vals, key = f[1:], "pnr"
        elif f[0] == "MemCore_inst0":
            vals, key = f[2:], "pnr_memcore"
        else:
            continue
        for n, v in zip(names, vals):
            out[f"{key}_{n}"] = float(v)
    return out


def _pnr_timing(text: str) -> dict:
    lines = text.splitlines()
    hdr = next((l for l in lines if "mode" in l and "|   all" in l), None)
    row = next((l for l in lines if "WNS (ns):" in l), None)
    if not hdr or not row:
        return {}
    cols = [c.strip() for c in hdr.split("|")[2:-1]]
    vals = [v.strip() for v in row.split("|")[2:-1]]
    out = {}
    for c, v in zip(cols, vals):
        if v != "N/A" and c in ("all", "Reg2Reg", "In2Reg", "In2Out"):
            out[f"pnr_wns_{c}_ps"] = float(v) * 1000
    return out


# ---- per-block breakdown (hierarchy-kept builds, garnet --flatten-effort 0) ------
#
# Each level's children are binned by instance name; children that match no
# pattern go to that level's remainder (_REST), so a level's blocks always add
# up to the level itself:
#   tile    = sb + cb + memcore + tile_glue
#   memcore = cfg_regs + mc_mux + inner + mc_glue
#   inner   = sram + ctrl + stencil + rom + fifos + inner_glue
#   ctrl    = c_port + c_id + c_sg + c_ag + c_rvnet + c_storage + c_rdbuf + c_mp + c_other
# ctrl is the lake spec controller (lakespec_inst static, lakespec_mem_inst RV);
# c_rvnet/c_storage/c_rdbuf exist only in RV builds.
_LEVELS = {
    "tile": [("sb", r"SB_ID\d+_\d+TRACKS_B\d+_MemCore"), ("cb", r"CB_.*"),
             ("memcore", r"MemCore_inst0")],
    "memcore": [("cfg_regs", r"config_reg_\d+"), ("mc_mux", r"mux_aoi_.*"),
                ("inner", r"MemCore_inner_W_inst0")],
    "inner": [("sram", r"memory_\d+"), ("ctrl", r"mem_ctrl_lakespec\w*_flat"),
              ("stencil", r"mem_ctrl_stencil_valid\w*"), ("rom", r"mem_ctrl_strg_ram\w*"),
              ("fifos", r"(input|output)_width_\d+_num_\d+_(input|output)_fifo")],
    "ctrl": [("c_port", r"port_inst_\d+"), ("c_id", r"port_id_\d+"), ("c_sg", r"port_sg_\d+"),
             ("c_ag", r"port_ag_\d+"), ("c_rvnet", r"rv_comp_network\w*"), ("c_storage", r"storage"),
             ("c_rdbuf", r"port_\d+_rd_buf"), ("c_mp", r"(MemoryPort_|memoryport_|memintfdec_).*")],
}
_REST = {"tile": "tile_glue", "memcore": "mc_glue", "inner": "inner_glue", "ctrl": "c_other"}
BLOCKS = [b for lvl in _LEVELS for b in [n for n, _ in _LEVELS[lvl]] + [_REST[lvl]]]


def _indent_tree(text: str, row_re: str, value_group: int) -> dict[str, float]:
    """Path ('' = top, else 'a/b/c' below it) -> value, for reports that show
    the hierarchy by two-space indentation (Genus final_area.rpt, PT power.hier)."""
    vals, stack = {}, []
    for line in text.splitlines():
        m = re.match(row_re, line)
        if not m:
            continue
        depth = len(m.group(1)) // 2
        stack = stack[:depth] + [m.group(2)]
        vals["/".join(stack[1:])] = float(m.group(value_group))
    return vals


def _genus_tree(text: str) -> dict[str, float]:
    """Genus final_area.rpt -> cell area (µm², incl. macros) per instance."""
    return _indent_tree(text, r"^( *)(\S+)\s+\S+\s+\d+\s+([\d.]+)\s+[\d.]+\s+[\d.]+\s*$", 3)


def _power_tree(text: str) -> dict[str, float]:
    """PT `report_power -hierarchy` (power.hier) -> total power (W) per instance."""
    num = r"(?:[\d.]+(?:e[-+]?\d+)?)"
    return _indent_tree(
        text, rf"^( *)(\S+)(?: \(\S+\))?\s+{num}\s+{num}\s+{num}\s+({num})\s+[\d.]+\s*$", 3)


def _innovus_tree(text: str) -> dict[str, float]:
    """Innovus signoff.area.rpt (full hinst paths) -> total area (µm², incl.
    macros) per instance."""
    vals = {}
    for line in text.splitlines():
        f = line.split()
        if len(f) == 11 and f[0] == "Tile_MemCore":
            vals[""] = float(f[2])
        elif len(f) == 12 and re.fullmatch(r"[\d.]+", f[3]):
            vals[f[0]] = float(f[3])
    return vals


def _tree_blocks(vals: dict[str, float]) -> dict[str, float]:
    """Bin a hierarchy into BLOCKS (see _LEVELS). Empty if the tree is flat
    (no lake controller instance), as in --flatten-effort 3 builds."""
    kids: dict[str, list[str]] = {}
    for p in vals:
        if p:
            kids.setdefault(p.rpartition("/")[0], []).append(p)
    inner = "MemCore_inst0/MemCore_inner_W_inst0/MemCore_inner"
    flat = next((c for c in kids.get(inner, []) if re.fullmatch(_LEVELS["inner"][1][1], c.rpartition("/")[2])), None)
    inst = next(iter(kids.get(flat, [])), None) if flat else None
    if "" not in vals or "MemCore_inst0" not in vals or inner not in vals or inst is None:
        return {}
    out = {}
    for level, node, total in (("tile", "", vals[""]), ("memcore", "MemCore_inst0", vals["MemCore_inst0"]),
                               ("inner", inner, vals[inner]), ("ctrl", inst, vals[flat])):
        used = 0.0
        for name, _ in _LEVELS[level]:
            out[name] = 0.0
        for c in kids.get(node, []):
            leaf = c.rpartition("/")[2]
            for name, pat in _LEVELS[level]:
                if re.fullmatch(pat, leaf):
                    out[name] += vals[c]
                    used += vals[c]
                    break
        out[_REST[level]] = total - used
    return out


def _pnr_core(text: str) -> dict:
    m = re.search(r"Total area of Core:\s+([\d.]+)", text)
    return {"pnr_core_area": float(m.group(1))} if m else {}


# ---- ingest --------------------------------------------------------------------


class _Src:
    """Uniform read access to an extracted sweep dir or its zip."""

    def __init__(self, path: Path):
        self.path = path
        self.zip = zipfile.ZipFile(path) if path.suffix == ".zip" else None
        if self.zip:
            names = self.zip.namelist()
            self.root = names[0].split("/")[0]
            self.names = set(names)
            self.dirs = {tuple(n.split("/")[1:3]) for n in names if n.count("/") >= 3}
        else:
            self.root = None

    def configs(self) -> list[str]:
        """Config workspace names (dirs holding a spec_config.json)."""
        if self.zip:
            return sorted({n.split("/")[1] for n in self.names
                           if n.count("/") == 2 and n.endswith("/spec_config.json")})
        return sorted(p.parent.name for p in self.path.glob("*/spec_config.json"))

    def step(self, config: str, step: str) -> str:
        """`<config>/<N>-<step>`. mflowgen numbers steps per graph, so N shifts when
        a step is added (e.g. pre-route moved synthesis from 13 to 14); the
        highest-numbered match wins."""
        pat = re.compile(rf"(\d+)-{re.escape(step)}")
        if self.zip:
            found = [d for c, d in self.dirs if c == config and pat.fullmatch(d)]
        else:
            found = [p.name for p in (self.path / config).glob(f"*-{step}") if pat.fullmatch(p.name)]
        n = max(found, key=lambda d: int(d.split("-", 1)[0]), default=f"0-{step}")
        return f"{config}/{n}"

    def read(self, rel: str) -> str | None:
        if self.zip:
            name = f"{self.root}/{rel}"
            return self.zip.read(name).decode(errors="ignore") if name in self.names else None
        p = self.path / rel
        return p.read_text(errors="ignore") if p.is_file() else None

    @property
    def sweep_name(self) -> str:
        return self.root if self.zip else self.path.name


def ingest(src_path: Path) -> pd.DataFrame:
    src = _Src(src_path)
    results = src.read("results.csv")
    if results is None:
        raise MissingDataError(f"no results.csv in {src_path}")
    res = pd.read_csv(io.StringIO(results))
    # Configs still building when the zip was cut aren't in results.csv yet;
    # pick them up from their spec_config.json / sweep_meta.json.
    extra = []
    for name in sorted(set(src.configs()) - set(res["config_name"])):
        spec = json.loads(src.read(f"{name}/spec_config.json") or "{}")
        meta = json.loads(src.read(f"{name}/sweep_meta.json") or "{}")
        done = src.read(f"{name}/done.flag")
        extra.append({"config_name": name, "runtime_mode": meta.get("runtime_mode", "static"),
                      "targets": meta.get("targets", ""),
                      "status": "PASS" if done else "INCOMPLETE", **spec})
    if extra:
        res = pd.concat([res, pd.DataFrame(extra)], ignore_index=True)
    grids: dict[tuple, list[str]] = {}
    for exp, spec in experiment_grids():
        grids.setdefault(_key(spec), [])
        if exp not in grids[_key(spec)]:
            grids[_key(spec)].append(exp)

    rows = []
    for _, r in res.iterrows():
        name = r["config_name"]
        row = {"config": name, "mode": r.get("runtime_mode", "static"),
               "status": r["status"], "targets": r.get("targets", "")}
        spec = {k: r.get(k) for k in KEY}
        for k in KEY:
            row[k] = _norm(spec[k]) if _norm(spec[k]) is not None else SPEC_DEFAULTS[k]
        row["dual_port"] = bool(row["dual_port"])
        exps = grids.get(_key(spec), [])
        row["experiments"] = ";".join(exps)
        row["base_config"] = name[:-3] if name.endswith("_rv") else name
        syn = f"{src.step(name, 'cadence-genus-synthesis')}/results_syn"
        gates = src.read(f"{syn}/final_gates.rpt")
        if gates:
            row.update(_gates(gates))
            row["syn_std"] = row["syn_cell"] - row["syn_sram"]
            area = src.read(f"{syn}/final_area.rpt")
            if area:
                row.update(_hier(area))
                row.update({f"blk_{k}": v for k, v in _tree_blocks(_genus_tree(area)).items()})
                row["syn_top"] = row["syn_cell"] - row["syn_cb"] - row["syn_sb"] - row["syn_memcore"]
                row["syn_memcore_std"] = row["syn_memcore"] - row["syn_sram"]
            for rel, fn in (("final.rpt", _qos), ("final_qor.rpt", _clock)):
                t = src.read(f"{syn}/{rel}")
                if t:
                    row.update(fn(t))
        so = f"{src.step(name, 'cadence-innovus-signoff')}/reports"
        for rel, fn in (("signoff.area.rpt", _pnr_area), ("signoff.summary", _pnr_timing),
                        ("signoff.summaryReport.rpt", _pnr_core)):
            t = src.read(f"{so}/{rel}")
            if t:
                row.update(fn(t))
                if rel == "signoff.area.rpt":
                    row.update({f"pblk_{k}": v for k, v in _tree_blocks(_innovus_tree(t)).items()})
        # garnet --memtile-power: PT power.hier per level (synth netlist / signoff
        # netlist) and program (idle / active), W
        for level in ("synth", "pnr"):
            for variant in ("idle", "active"):
                t = src.read(f"{src.step(name, f'memtile-power-{level}-{variant}')}/outputs/power.hier")
                if t:
                    tree = _power_tree(t)
                    pre = f"pw_{level}_{variant}_"
                    row[pre + "total"] = tree.get("")
                    row.update({pre + k: v for k, v in _tree_blocks(tree).items()})
        if "pnr_cell" in row:
            row["pnr_std"] = row["pnr_cell"] - row["pnr_macro"]
            row["pnr_flop_only"] = row["pnr_flop"] - row["pnr_cg"]  # Innovus counts ICGs as Flop
        rows.append(row)

    df = pd.DataFrame(rows)
    # The sweep's own model outputs (PnR role, synth->PnR projections, PT WNS,
    # power totals), merged as written by garnet sweep_specs.py.
    for csv_name in ("correlation.csv", "memtile_power.csv"):
        t = src.read(csv_name)
        if not t:
            continue
        sw = pd.read_csv(io.StringIO(t)).dropna(axis=1, how="all")
        sw = sw.drop(columns=[c for c in ("runtime_mode", "targets") if c in sw.columns])
        sw = sw.drop(columns=[c for c in sw.columns if c != "config_name" and c in df.columns])
        df = df.merge(sw.rename(columns={"config_name": "config"}), on="config", how="left")
    df["sweep"] = src.sweep_name
    df["ports"] = df["in_ports"] + df["out_ports"]
    df["mem_width_bits"] = df["vec_width"] * df["data_width"]
    df["sram_bw_bits"] = df["mem_width_bits"] * df["dual_port"].map({True: 2, False: 1})
    return df


def load(path: Path | None = None) -> pd.DataFrame:
    path = Path(path) if path else LATEST
    if not path.is_file():
        raise MissingDataError(
            f"no tile sweep data at {path} -- run "
            "`python3 -m THESIS.pipeline.tile_sweep <sweep.zip>` first")
    df = pd.read_csv(path)
    return df[df["status"] != "FAIL"].dropna(subset=["syn_cell"])


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("src", type=Path, help="sweep zip or extracted sweep dir")
    ap.add_argument("-o", "--out", type=Path, help="CSV path (default: data/tile_sweep/<sweep>.csv)")
    args = ap.parse_args(argv)
    df = ingest(args.src)
    out = args.out or DATA_DIR / f"{df['sweep'].iloc[0]}.csv"
    out.parent.mkdir(parents=True, exist_ok=True)
    df.to_csv(out, index=False)
    if out.resolve() != LATEST.resolve():
        shutil.copyfile(out, LATEST)
    ok = df["syn_cell"].notna().sum()
    print(f"{len(df)} configs ({ok} with synthesis, {df.get('pnr_cell', pd.Series()).notna().sum()} "
          f"with PnR) -> {out}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
