"""garnet ``cgra_app_power.py`` output -> ``THESIS/data/apps/**/results.json``.

``cgra_app_power.py`` (garnet ``mflowgen/``, build machine) replays recorded
app bundles on synthesized Tile_MemCore / Tile_PE tiles and writes
``cgra_power.csv`` (one row per config x variant = one app on one CGRA). This
module turns each row into one ``results.json`` for the Ch. 5 app figures
(``generators.single_level_*``, ``apps_query``)::

    python3 -m THESIS.apps.cgra_power_ingest <cgra_app_power out-dir or zip> \\
        [--bundles <bundle root> ...] [--kernel-stats ks.json ...] [--tile-sweep latest.csv]

    # on /aha, where the bundles are: kernel time and output size of each bundle
    python3 -m THESIS.apps.cgra_power_ingest --bundles <bundle root> --write-kernel-stats ks.json

Per row it reads the bundle manifest (``*-application-<variant>/outputs/
manifest.json`` in the workspace or zip: spec, mode, fabric, pond spec, app,
array size), the workspace's ``run_meta.json`` (clock) and each tile's Genus
``final_gates.rpt`` (tile area; ``--tile-sweep`` takes the MemTile area from a
``tile_sweep`` CSV instead, with PnR area where it has one). Kernel cycles and
output words come from the manifest's ``kernel`` entry when the recorder wrote
one, else from the bundle's ``logs/test.log`` (``aha test`` prints the kernel
time and output bytes; ``--bundles`` / ``--kernel-stats``).

A row maps to a lake design point by geometry (``garnet_configs``) and to a
lake app by the Halide app's basename (``registry.APPS``, ``--app-map``). Rows
that map to neither are reported and skipped (``--keep-unmapped`` writes them
under the garnet names). Output: ``<results-root>/<design_id>__<mode>__<fabric>
[__pond-<id>]/<app_id>/results.json``.

Field meanings (beyond the run_matrix schema):
  synth_power_w      app_W: synth-level power of the placed + route tiles, averaged
                     over the replay window (``window_s``)
  array_power_w      + one idle tile per type x the tiles the app leaves untouched
  app_energy_j       app_J: each tile's power x its window, summed
  window_cycles      window_s x clock: replay window (kernel + the tb's output
                     read-back), not the kernel
  kernel_cycles      aha test's kernel time / its clock period (1 ns unless the
                     bundle set CLK_PERIOD); ``total_cycles`` = this
  output_words       output bytes / (data_width / 8)
  tile_count         MEM tiles with a placed core (the UL memories)
  mem_tile_utilization  that / MEM tiles in the array
  synth_*_area_um2   CGRA area the app touches: (placed + route) tiles x tile area
  utilization        None: needs per-tile SRAM activity (not recorded yet)
"""

from __future__ import annotations

import argparse
import csv
import io
import json
import re
import sys
import zipfile
from pathlib import Path

from . import garnet_configs as gc
from .design_points import DESIGNS_SINGLE_LEVEL
from ..pipeline.tile_sweep import _gates

REPO_ROOT = Path(__file__).resolve().parents[2]
RESULTS_ROOT = REPO_ROOT / "THESIS" / "data" / "apps"
TILES = ("Tile_MemCore", "Tile_PE")
DEFAULT_CLK_NS = 1.0  # garnet tests/test_app/Makefile CLK_PERIOD ?= 1ns


class _Tree:
    """Read access to a cgra_app_power out-dir or its --zip (paths relative to the out-dir)."""

    def __init__(self, path: Path):
        self.path = Path(path)
        self.zip = zipfile.ZipFile(self.path) if self.path.suffix == ".zip" else None
        if self.zip:
            names = [n for n in self.zip.namelist() if not n.endswith("/")]
            # the zip holds <out-dir name>/...; the out-dir is where cgra_power.csv is
            csvs = sorted((n for n in names if n.rsplit("/", 1)[-1] == "cgra_power.csv"), key=len)
            self.prefix = csvs[0][: csvs[0].rfind("/") + 1] if csvs else ""
            self.names = [n[len(self.prefix):] for n in names if n.startswith(self.prefix)]

    def read(self, rel: str) -> str | None:
        if self.zip:
            try:
                return self.zip.read(self.prefix + rel).decode(errors="ignore")
            except KeyError:
                return None
        p = self.path / rel
        return p.read_text(errors="ignore") if p.is_file() else None

    def glob(self, pattern: str) -> list[str]:
        """Paths matching pattern, `*` within one path component (as Path.glob)."""
        if not self.zip:
            return sorted(str(p.relative_to(self.path)) for p in self.path.glob(pattern) if p.is_file())
        rx = re.compile("".join("[^/]*" if c == "*" else re.escape(c) for c in pattern))
        return sorted(n for n in self.names if rx.fullmatch(n))


def _step_file(tree: _Tree, config: str, tile: str, step: str, rel: str) -> str | None:
    """`<config>/<tile>/<N>-<step>/<rel>` with the highest N (mflowgen step numbers shift)."""
    hits = [h for h in tree.glob(f"{config}/{tile}/*-{step}/{rel}")
            if re.fullmatch(rf"\d+-{re.escape(step)}", h.split("/")[2])]
    return tree.read(max(hits, key=lambda h: int(h.split("/")[2].split("-", 1)[0]))) if hits else None


def _float(v):
    try:
        return None if v in (None, "") else float(v)
    except ValueError:
        return None


def _int(v):
    f = _float(v)
    return None if f is None else int(round(f))


# ---- kernel stats (aha test) --------------------------------------------------------


def parse_test_log(text: str) -> dict:
    """Kernel time (ns) and output bytes from garnet's tests/test_app tb output
    (environment.sv Env_kernel_test), summed over the run's kernels."""
    ns = [float(x) for x in re.findall(r"It takes ([\d.]+) ns total time to run kernel", text)]
    out = [int(x) for x in re.findall(r"The size of output is (\d+) Byte", text)]
    if not ns:
        return {}
    return {"kernel_ns": sum(ns), "output_bytes": sum(out) if out else None, "n_kernels": len(ns)}


def kernel_entry(stats: dict, data_width: int, clk_ns: float = DEFAULT_CLK_NS) -> dict:
    """Cycles and words from kernel ns / output bytes."""
    out = dict(stats)
    out.setdefault("clk_period_ns", clk_ns)
    if out.get("kernel_ns") is not None and out.get("cycles") is None:
        out["cycles"] = int(round(out["kernel_ns"] / out["clk_period_ns"]))
    if out.get("output_bytes") is not None and out.get("output_words") is None:
        out["output_words"] = out["output_bytes"] // max(1, data_width // 8)
    return out


def _clk_ns(app_env: dict) -> float:
    m = re.fullmatch(r"([\d.]+)\s*(ps|ns)?", str((app_env or {}).get("CLK_PERIOD", "")).strip())
    if not m:
        return DEFAULT_CLK_NS
    return float(m.group(1)) / (1000.0 if m.group(2) == "ps" else 1.0)


def _bundle_key(man: dict, config: str) -> tuple[str, str, str]:
    """A bundle's identity: app basename, config and the manifest's `created`
    stamp (two variants of one app on one config differ only in that)."""
    return gc.app_basename(man.get("app") or ""), config, man.get("created") or ""


def collect_kernel_stats(roots: list[Path]) -> dict[tuple[str, str, str], dict]:
    """{bundle key: kernel entry} for every bundle (a dir with manifest.json +
    logs/test.log) under roots."""
    out = {}
    for root in roots:
        for man_path in sorted(Path(root).rglob("manifest.json")):
            log = man_path.parent / "logs" / "test.log"
            if not log.is_file():
                continue
            man = json.loads(man_path.read_text())
            stats = parse_test_log(log.read_text(errors="ignore"))
            if not stats:
                continue
            dw = int(gc.full_spec(man.get("spec_config") or {})["data_width"])
            entry = kernel_entry(stats, dw, _clk_ns(man.get("app_env")))
            entry["source"] = str(log)
            out[_bundle_key(man, man.get("config") or man_path.parent.name)] = entry
    return out


def find_kernel(stats: dict, man: dict, config: str) -> dict | None:
    """The bundle's kernel entry; without an exact match, the only one of its app + config."""
    key = _bundle_key(man, config)
    if key in stats:
        return stats[key]
    same = [e for k, e in stats.items() if k[:2] == key[:2]]
    return same[0] if len(same) == 1 else None


def load_kernel_stats(path: Path) -> dict[tuple[str, str, str], dict]:
    keys = ("app", "config", "created")
    return {tuple(e.get(k, "") for k in keys): {k: v for k, v in e.items() if k not in keys}
            for e in json.loads(Path(path).read_text())}


def dump_kernel_stats(stats: dict[tuple[str, str, str], dict], path: Path) -> None:
    rows = [{"app": a, "config": c, "created": t, **e} for (a, c, t), e in sorted(stats.items())]
    Path(path).write_text(json.dumps(rows, indent=1))


# ---- tile areas ------------------------------------------------------------------------


def tile_areas(tree: _Tree, config: str) -> dict:
    """Genus synth area of the workspace's own tiles: {tile: (cell um2, SRAM macro um2)}."""
    out = {}
    for tile in TILES:
        rpt = _step_file(tree, config, tile, "cadence-genus-synthesis", "results_syn/final_gates.rpt")
        if rpt:
            g = _gates(rpt)
            out[tile] = (g["syn_cell"], g["syn_sram"])
    return out


def sweep_areas(path: Path | None) -> dict[str, dict]:
    """{config: tile_sweep row} of a tile_sweep CSV (lake THESIS.pipeline.tile_sweep)."""
    if not path:
        return {}
    with open(path, newline="") as f:
        return {r["config"]: r for r in csv.DictReader(f)}


# ---- rows -> results ---------------------------------------------------------------


def pond_id(pond_spec) -> str:
    """'builtin' = garnet's default pond (no --pond-spec), 'default' = lake's default
    spec pond ({}), else the spec's keys, e.g. 'dims6_storage_capacity128'."""
    if pond_spec is None:
        return "builtin"
    if not pond_spec:
        return "default"
    return "_".join(f"{k}{v}" for k, v in sorted(pond_spec.items()))


def cell_dir(design_id: str, mode: str, fabric: str, pond: str) -> str:
    tag = "" if pond in ("builtin", "default") else f"__pond-{pond}"
    return f"{design_id}__{mode}__{fabric}{tag}"


def _manifest(tree: _Tree, config: str, variant: str) -> dict | None:
    for tile in TILES:
        t = _step_file(tree, config, tile, f"application-{variant}", "outputs/manifest.json")
        if t:
            return json.loads(t)
    return None


def _run_meta(tree: _Tree, config: str) -> dict:
    for tile in TILES:
        t = tree.read(f"{config}/{tile}/run_meta.json")
        if t:
            return json.loads(t)
    return {}


def build_results(tree: _Tree, *, kernel_stats=None, sweep=None, apps=None,
                  keep_unmapped=False, source=None) -> tuple[list[tuple[str, dict]], list[str]]:
    """([(cell dir/app_id, results dict)], [skip notes]) for every cgra_power.csv row."""
    text = tree.read("cgra_power.csv")
    if text is None:
        raise SystemExit(f"*** no cgra_power.csv in {tree.path}")
    kernel_stats, sweep, apps = kernel_stats or {}, sweep or {}, apps or gc.app_map()
    out, notes, areas = [], [], {}
    for r in csv.DictReader(io.StringIO(text)):
        config, variant = r["config"], r["variant"]
        man = _manifest(tree, config, variant)
        if man is None:
            parsed = gc.parse_config_name(config)
            man = {"spec_config": parsed["spec"], "mode": parsed["mode"],
                   "thesis": parsed["mode"] if parsed["thesis"] else None, "app": r.get("app")}
            notes.append(f"{config}/{variant}: no manifest in the source; spec from the config name")
        spec, mode = man["spec_config"], man["mode"]
        try:
            named = gc.parse_config_name(config)
        except ValueError:
            named = None
        if named and (gc.geometry(named["spec"]) != gc.geometry(spec) or named["mode"] != mode):
            notes.append(f"skip {config}/{variant}: the manifest's spec / mode is not the config name's")
            continue
        fabric = "thesis" if man.get("thesis") else "stock"
        app = gc.app_basename(man.get("app") or r.get("app") or variant)
        app_id = apps.get(app)
        designs = gc.designs_for_spec(spec, DESIGNS_SINGLE_LEVEL)
        if not designs or app_id is None:
            why = ", ".join(w for w, bad in (("no design point with this geometry", not designs),
                                             (f"app {app} not in registry.APPS / --app-map", app_id is None)) if bad)
            if not keep_unmapped:
                notes.append(f"skip {config}/{variant}: {why}")
                continue
            notes.append(f"keep {config}/{variant} under garnet names: {why}")
        design_ids = [d.id for d in designs] or [gc.config_name({k: v for k, v in spec.items()
                                                                 if k in gc.GEOMETRY_KEYS})]
        app_id = app_id or app
        meta = _run_meta(tree, config)
        clock_mhz = _float(meta.get("clock_mhz"))
        if config not in areas:
            areas[config] = tile_areas(tree, config)
        dw = int(gc.full_spec(spec)["data_width"])
        kernel = man.get("kernel") or find_kernel(kernel_stats, man, config)
        kernel = kernel_entry(kernel, dw) if kernel else None

        n = {f"{t}_{role}": _int(r.get(f"{t}_{role}_n")) or 0 for t in TILES for role in ("placed", "route")}
        array_tiles = man.get("array_tiles") or {}
        res = {
            "app_id": app_id, "source": "cgra_app_power", "source_path": source or str(tree.path),
            "config": config, "variant": variant, "garnet_app": man.get("app"),
            "mode": mode, "fabric": fabric, "spec_config": spec, "spec_options": gc.spec_options(spec),
            "pond_spec": man.get("pond_spec"), "pond_id": pond_id(man.get("pond_spec")),
            "garnet": meta.get("garnet"), "cgra_width": man.get("width"), "cgra_height": man.get("height"),
            "tiles_replayed": _int(r.get("tiles_replayed")), "tiles_exact": _int(r.get("tiles_exact")),
            "power_level": "synth",
            "synth_power_w": _float(r.get("app_W")), "array_power_w": _float(r.get("array_W")),
            "idle_power_w": _float(r.get("idle_W")), "sram_power_w": _float(r.get("sram_W")),
            "app_energy_j": _float(r.get("app_J")), "array_energy_j": _float(r.get("array_J")),
            "window_s": _float(r.get("window_s")),
            "clock_mhz": clock_mhz, "clock_period_ps": 1e6 / clock_mhz if clock_mhz else None,
            "mem_tiles_placed": n["Tile_MemCore_placed"], "mem_tiles_route": n["Tile_MemCore_route"],
            "pe_tiles_placed": n["Tile_PE_placed"], "pe_tiles_route": n["Tile_PE_route"],
            "array_mem_tiles": array_tiles.get("Tile_MemCore"), "array_pe_tiles": array_tiles.get("Tile_PE"),
            "tile_count": n["Tile_MemCore_placed"],
            "utilization": None,
        }
        res["sim_status"] = ("PASS" if res["tiles_replayed"] and res["tiles_exact"] == res["tiles_replayed"]
                             else "FAIL")
        if res["window_s"] and clock_mhz:
            res["window_cycles"] = int(round(res["window_s"] * clock_mhz * 1e6))
        if res["array_mem_tiles"]:
            res["mem_tile_utilization"] = res["tile_count"] / res["array_mem_tiles"]
        if kernel:
            res.update(kernel_ns=kernel.get("kernel_ns"), kernel_cycles=kernel.get("cycles"),
                       kernel_clk_period_ns=kernel.get("clk_period_ns"), output_bytes=kernel.get("output_bytes"),
                       output_words=kernel.get("output_words"), kernel_source=kernel.get("source", "manifest"))
            res["total_cycles"] = res["kernel_cycles"]
            if res["output_words"] and res["kernel_cycles"]:
                res["output_per_cycle"] = res["output_words"] / res["kernel_cycles"]

        # tile areas: the workspace's own synthesis, the MemTile from --tile-sweep if given
        a = dict(areas[config])
        sw = sweep.get(config)
        if sw and _float(sw.get("syn_cell")) is not None:
            a["Tile_MemCore"] = (_float(sw["syn_cell"]), _float(sw.get("syn_sram")) or 0.0)
            res["area_source"] = "tile_sweep"
            if _float(sw.get("pnr_cell")) is not None:
                res["mem_tile_pnr_area_um2"] = _float(sw["pnr_cell"])
        elif a:
            res["area_source"] = "cgra_app_power synthesis"
        if "Tile_MemCore" in a:
            res["mem_tile_area_um2"], res["mem_tile_sram_area_um2"] = a["Tile_MemCore"]
        if "Tile_PE" in a:
            res["pe_tile_area_um2"] = a["Tile_PE"][0]
        if "Tile_MemCore" in a and "Tile_PE" in a:
            mem_n = n["Tile_MemCore_placed"] + n["Tile_MemCore_route"]
            pe_n = n["Tile_PE_placed"] + n["Tile_PE_route"]
            total = mem_n * a["Tile_MemCore"][0] + pe_n * a["Tile_PE"][0]
            storage = mem_n * a["Tile_MemCore"][1]
            res.update(synth_total_area_um2=total, synth_storage_area_um2=storage,
                       synth_logic_area_um2=total - storage)
        for design_id in design_ids:
            res_d = {"design_id": design_id, "in_design_set": bool(designs), **res}
            out.append((f"{cell_dir(design_id, mode, fabric, res['pond_id'])}/{app_id}", res_d))
        if len(designs) > 1:
            notes.append(f"{config}/{variant}: same spec as design points {', '.join(design_ids)}; written under each")
    return out, notes


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("src", nargs="?", type=Path, help="cgra_app_power.py --out-dir, or its --zip")
    ap.add_argument("--bundles", nargs="+", type=Path, default=[],
                    help="app-bundle roots: kernel time + output size from each bundle's logs/test.log")
    ap.add_argument("--kernel-stats", nargs="+", type=Path, default=[],
                    help="JSON from --write-kernel-stats (e.g. made on /aha)")
    ap.add_argument("--write-kernel-stats", type=Path, help="write the collected kernel stats here")
    ap.add_argument("--tile-sweep", type=Path, help="tile_sweep CSV for MemTile areas (by config)")
    ap.add_argument("--app-map", nargs="+", default=[], metavar="APP=ID",
                    help="extra Halide app (basename or path) -> lake app id, e.g. matmul_rvtry3=matmul_agg")
    ap.add_argument("--keep-unmapped", action="store_true",
                    help="write rows without a lake design point / app under their garnet names")
    ap.add_argument("--results-root", type=Path, default=RESULTS_ROOT)
    ap.add_argument("--dry-run", action="store_true", help="print what would be written")
    args = ap.parse_args(argv)

    stats = collect_kernel_stats(args.bundles)
    for p in args.kernel_stats:
        stats.update(load_kernel_stats(p))
    if args.write_kernel_stats:
        dump_kernel_stats(stats, args.write_kernel_stats)
        print(f"{len(stats)} bundles -> {args.write_kernel_stats}", file=sys.stderr)
    if args.src is None:
        if not args.write_kernel_stats:
            ap.error("give a cgra_app_power out-dir/zip, or --write-kernel-stats")
        return 0
    extra = {}
    for item in args.app_map:
        k, sep, v = item.partition("=")
        if not sep:
            ap.error(f"--app-map {item!r}: want APP=ID")
        extra[k] = v
    results, notes = build_results(_Tree(args.src), kernel_stats=stats, sweep=sweep_areas(args.tile_sweep),
                                   apps=gc.app_map(extra), keep_unmapped=args.keep_unmapped)
    for note in notes:
        print(f"  {note}", file=sys.stderr)
    for rel, res in results:
        path = args.results_root / rel / "results.json"
        cyc = res.get("total_cycles")
        print(f"{'(dry) ' if args.dry_run else ''}{path}  [{res['sim_status']}] "
              f"{res['config']}/{res['variant']}  cycles {cyc if cyc is not None else '-'}")
        if not args.dry_run:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(json.dumps(res, indent=2))
    no_kernel = sorted({f"{r['config']}/{r['variant']}" for _, r in results if r.get("kernel_cycles") is None})
    if no_kernel:
        print(f"  no kernel cycles (give --bundles / --kernel-stats) for: {', '.join(no_kernel)}", file=sys.stderr)
    print(f"{len(results)} results.json {'to write' if args.dry_run else 'written'}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
