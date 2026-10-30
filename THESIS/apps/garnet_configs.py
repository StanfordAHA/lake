"""garnet sweep config names <-> lake design points (``design_points.py``).

garnet names each tile build after its lake spec, runtime mode and fabric
(``mflowgen/sweep_specs.py`` ``_config_name``):
``fw4_dw16_sc8192_sp_in2_out2_vc2`` (stock fabric, static),
``..._rv`` (stock, RV), ``..._sharectr_portcg_thesis`` (thesis static fabric
with the user's MemTile options), ``..._nofp_portcg_thesis_rv`` (thesis RV).
App bundles (``gen_app_bundle.py --config``) and ``cgra_app_power.py`` use
the same names. Lake names a design point by experiment and geometry
(``port_fw4_dw16_sc8k_sp2x2``). This module converts between the two:

    python3 -m THESIS.apps.garnet_configs              # the map, per design point
    python3 -m THESIS.apps.garnet_configs --commands   # bundle + app-power commands

A design point is its geometry (``GEOMETRY_KEYS``). The fabric options
(``OPTION_KEYS``: clock gating, shared counter, filter path) belong to the
fabric, not the design point. Every thesis-fabric build uses
``THESIS_OPT_KNOBS``, garnet's ``sweep_specs.THESIS_OPT_KNOBS`` (user
2026-10-22).
"""

from __future__ import annotations

import argparse
import re
import shlex
import sys

from .design_points import DESIGNS_SINGLE_LEVEL, DesignPoint
from .registry import APPS

# build_spec / build_spec_rv defaults (garnet sweep_specs.SPEC_DEFAULTS)
SPEC_DEFAULTS = dict(storage_capacity=4096, data_width=16, vec_width=4, dims=6,
                     in_ports=2, out_ports=2, dual_port=False, vec_capacity=2,
                     max_extent=None, max_sequence_width=None,
                     filter_fifo_depth=None, share_clk_ctr=False,
                     clock_gate_ports=False, add_filter_path=True)
GEOMETRY_KEYS = ("storage_capacity", "data_width", "vec_width", "in_ports", "out_ports",
                 "dual_port", "vec_capacity", "dims", "max_extent", "max_sequence_width")
OPTION_KEYS = ("filter_fifo_depth", "share_clk_ctr", "clock_gate_ports", "add_filter_path")
THESIS_OPT_KNOBS = {"static": dict(share_clk_ctr=True, clock_gate_ports=True),
                    "rv": dict(clock_gate_ports=True, add_filter_path=False)}
MODES = ("static", "rv")
FABRICS = ("thesis", "stock")

_NAME_RE = re.compile(
    r"fw(?P<vec_width>\d+)_dw(?P<data_width>\d+)_sc(?P<storage_capacity>\d+)_(?P<port>sp|dp)"
    r"_in(?P<in_ports>\d+)_out(?P<out_ports>\d+)"
    r"(?:_vc(?P<vec_capacity>\d+))?(?:_dim(?P<dims>\d+))?(?:_me(?P<max_extent>\d+))?"
    r"(?:_msw(?P<max_sequence_width>\d+))?(?:_ff(?P<filter_fifo_depth>\d+))?"
    r"(?P<nofp>_nofp)?(?P<sharectr>_sharectr)?(?P<portcg>_portcg)?(?P<thesis>_thesis)?"
    r"(?:_(?P<pond>nopond|specpond))?(?P<rv>_rv)?")


def full_spec(spec: dict) -> dict:
    """spec with build_spec's defaults filled in."""
    return {**SPEC_DEFAULTS, **{k: v for k, v in spec.items() if k in SPEC_DEFAULTS}}


def geometry(spec: dict) -> tuple:
    """The design-point identity of a spec (defaults filled in)."""
    s = full_spec(spec)
    return tuple(bool(s[k]) if k == "dual_port" else s[k] for k in GEOMETRY_KEYS)


def config_name(spec: dict, mode: str = "static", thesis: bool = False) -> str:
    """garnet sweep_specs._config_name of a lake spec (no pond / stock knobs)."""
    s = full_spec(spec)
    parts = [f"fw{s['vec_width']}_dw{s['data_width']}_sc{s['storage_capacity']}",
             "dp" if s["dual_port"] else "sp", f"in{s['in_ports']}_out{s['out_ports']}"]
    if s["vec_width"] > 1:
        parts.append(f"vc{s['vec_capacity']}")
    if s["dims"] != 6:
        parts.append(f"dim{s['dims']}")
    for key, tag in (("max_extent", "me"), ("max_sequence_width", "msw"), ("filter_fifo_depth", "ff")):
        if s[key] is not None:
            parts.append(f"{tag}{s[key]}")
    if s["add_filter_path"] is False:
        parts.append("nofp")
    if s["share_clk_ctr"]:
        parts.append("sharectr")
    if s["clock_gate_ports"]:
        parts.append("portcg")
    if thesis:
        parts.append("thesis")
    if mode == "rv":
        parts.append("rv")
    return "_".join(parts)


def parse_config_name(name: str) -> dict:
    """{"spec": build_spec kwargs (non-default keys only), "mode", "thesis", "pond"}
    of a garnet config name; ValueError if it is not one."""
    m = _NAME_RE.fullmatch(name)
    if not m:
        raise ValueError(f"not a garnet lake-spec config name: {name!r}")
    g = m.groupdict()
    spec = {k: int(g[k]) for k in ("storage_capacity", "data_width", "vec_width", "in_ports", "out_ports")}
    if g["port"] == "dp":
        spec["dual_port"] = True
    for k in ("vec_capacity", "dims", "max_extent", "max_sequence_width", "filter_fifo_depth"):
        if g[k] is not None:
            spec[k] = int(g[k])
    if g["nofp"]:
        spec["add_filter_path"] = False
    if g["sharectr"]:
        spec["share_clk_ctr"] = True
    if g["portcg"]:
        spec["clock_gate_ports"] = True
    spec = {k: v for k, v in spec.items() if SPEC_DEFAULTS[k] != v}
    return {"spec": spec, "mode": "rv" if g["rv"] else "static", "thesis": bool(g["thesis"]),
            "pond": {"nopond": "none", "specpond": "spec"}.get(g["pond"])}


def design_spec(dp: DesignPoint) -> dict:
    """build_spec kwargs of a lake design point (vec_capacity and dims at their
    defaults: DesignPoint has no such fields)."""
    return dict(storage_capacity=dp.storage_cap_bytes, data_width=dp.data_width,
                vec_width=dp.fetch_width, in_ports=dp.in_ports, out_ports=dp.out_ports,
                dual_port=bool(dp.dual_port))


def unsupported(spec: dict, mode: str) -> str | None:
    """Why garnet cannot build spec in mode (sweep_specs._unsupported_reason, geometry part)."""
    s = full_spec(spec)
    if mode == "rv" and s["vec_width"] > 1 and s["vec_capacity"] > 2:
        return "build_spec_rv supports vec_capacity <= 2 for vec_width > 1"
    return None


def garnet_config(dp: DesignPoint, mode: str = "static", fabric: str = "thesis") -> str | None:
    """The garnet config name of a design point in mode on fabric (None if garnet
    cannot build it). The thesis fabric carries THESIS_OPT_KNOBS."""
    spec = design_spec(dp)
    if unsupported(spec, mode):
        return None
    if fabric == "thesis":
        return config_name({**spec, **THESIS_OPT_KNOBS[mode]}, mode, thesis=True)
    return config_name(spec, mode)


def designs_for_spec(spec: dict, designs: list[DesignPoint] | None = None) -> list[DesignPoint]:
    """Every design point whose geometry is spec's."""
    key = geometry(spec)
    return [dp for dp in (DESIGNS_SINGLE_LEVEL if designs is None else designs)
            if geometry(design_spec(dp)) == key]


def spec_options(spec: dict) -> str:
    """Non-default fabric options of a spec, e.g. 'clock_gate_ports,share_clk_ctr'."""
    s = full_spec(spec)
    return ",".join(k for k in OPTION_KEYS if s[k] != SPEC_DEFAULTS[k])


def duplicate_designs(designs: list[DesignPoint] | None = None) -> list[list[str]]:
    """Groups of design-point ids that are the same garnet spec."""
    groups: dict[tuple, list[str]] = {}
    for dp in DESIGNS_SINGLE_LEVEL if designs is None else designs:
        groups.setdefault(geometry(design_spec(dp)), []).append(dp.id)
    return [ids for ids in groups.values() if len(ids) > 1]


def app_basename(app: str) -> str:
    """Last component of a Halide app path (bundles may run a private copy,
    e.g. ``thesis_x/gaussian`` for ``apps/gaussian``)."""
    return app.rstrip("/").rsplit("/", 1)[-1]


def app_map(extra: dict[str, str] | None = None) -> dict[str, str]:
    """{name: lake app id} for matching a cgra_app_power row to an app: every
    registry.APPS id (the variant / bundle-dir names ``--commands`` uses), the
    Halide app basename of each app no other app shares (bundles may run a
    private copy), and extra (e.g. another set's variant name ``mmpond``; a
    path key also counts as its basename)."""
    out = {a.id: a.id for a in APPS}
    base: dict[str, list[str]] = {}
    for a in APPS:
        base.setdefault(app_basename(a.halide_app_dir), []).append(a.id)
    out.update({b: ids[0] for b, ids in base.items() if len(ids) == 1 and b not in out})
    for k, v in (extra or {}).items():
        out[k] = out[app_basename(k)] = v
    return out


# ---- commands ------------------------------------------------------------------

# A private aha tree (garnet mflowgen/CLAUDE.md "App bundles"): symlinks to every
# /aha directory, but its own garnet clone at $GARNET_REV (garnet.py --verilog rewrites
# $GARNET_HOME's GLB/GLC headers), lake = the $LAKE snapshot (aha map/pnr's LAKE_PATH,
# as PYTHONPATH has it), and real MetaMapper / clockwork dirs with symlinked contents and
# real *_header.json copies (hw_support `make map` copies the app's headers there;
# through a symlink they would land in the shared /aha). aha uses only its --dir's
# directories, so loose files at the top of /aha (notes, logs, anything private) stay
# out of the trees.
MAKE_TREE = r"""make_tree() {  # make_tree <tree>
  local t=$1 e n d h
  [ -d "$t/garnet" ] && return
  mkdir -p "$t"
  for e in "$AHA"/* "$AHA"/.[a-z]*; do
    n=$(basename "$e")
    [ -d "$e" ] || continue  # directories only
    case $n in garnet|MetaMapper|clockwork|lake) ;; *) [ -e "$t/$n" ] || ln -s "$e" "$t/$n" ;; esac
  done
  ln -s "$LAKE" "$t/lake"
  git clone -q "$AHA/garnet" "$t/garnet" && git -C "$t/garnet" checkout -q "$GARNET_REV"
  for d in MetaMapper clockwork; do
    mkdir -p "$t/$d"
    for e in "$AHA/$d"/* "$AHA/$d"/.[a-z]*; do ln -s "$e" "$t/$d/$(basename "$e")"; done
    for h in "$AHA/$d"/*_header.json; do
      [ -e "$h" ] && rm "$t/$d/$(basename "$h")" && cp "$h" "$t/$d/"
    done
  done
}"""
# A gen_app_bundle.py run again on a bigger array when its PnR failed on the default
# 16x16 one (gen_app_bundle exits "*** pnr... FAILED"); any other failure stands.
RETRY_BIGGER = r"""retry_bigger() {  # retry_bigger <width> <height> <gen_app_bundle.py command ...>
  local w=$1 h=$2 log rc
  shift 2
  log=$(mktemp)
  "$@" 2>&1 | tee "$log"
  rc=${PIPESTATUS[0]}
  if [ "$rc" != 0 ] && grep -q '^\*\*\* pnr[^ ]* FAILED' "$log"; then
    echo "=== PnR failed on the default array: again on ${w}x${h}"
    "$@" --width "$w" --height "$h"
    rc=$?
  fi
  rm -f "$log"
  return $rc
}"""


def bundle_commands(designs, apps, modes, fabric, bundle_root, tree_root, pond_spec):
    """({job: [gen_app_bundle.py line per app x config]}, comment lines for the
    skipped (design, mode) pairs). apps are registry AppSpecs; each records into
    <bundle_root>/<app id>. Apps that share a Halide app dir (the two matmuls) share
    a job, so no two runs use one app dir at once; each job has its own private aha
    tree (<tree_root>/<job>) and runs its lines one after another. A bundle_blocked
    app's lines are commented out; a bundle_retry_array app's run through
    retry_bigger; an app with bundle_modes / bundle_in_ports records only those
    configs. RV records balanced on every fabric (garnet >= 5fab9324: no PnR
    pipelining, aha pnr latency-matches), so no line passes --rv-balance."""
    configs, notes = [], []
    for dp in designs:
        for mode in modes:
            cfg = garnet_config(dp, mode, fabric)
            if cfg is None:
                notes.append(f"# {dp.id} {mode}: {unsupported(design_spec(dp), mode)}")
            elif cfg in [c for c, _, _ in configs]:
                notes.append(f"# {dp.id} {mode}: same config as an earlier design point ({cfg})")
            else:
                configs.append((cfg, mode, dp))
    jobs: dict[str, list[str]] = {}
    for app in apps:
        job = app_basename(app.halide_app_dir)
        tree = f"{tree_root}/{job}"
        env = "".join(f" --env {shlex.quote(kv)}" for kv in shlex.split(app.bundle_env))
        lines = jobs.setdefault(job, [])
        if app.bundle_blocked:
            lines.append(f"# {app.id} BLOCKED: {app.bundle_blocked}")
        app_modes = app.bundle_modes.split(",") if app.bundle_modes else MODES
        only = [(c, m) for c, m, dp in configs
                if m in app_modes and app.bundle_in_ports in (0, dp.in_ports)]
        if len(only) < len(configs):
            ports = f" on {app.bundle_in_ports}-input points" if app.bundle_in_ports else ""
            lines.append(f"# {app.id}: {'/'.join(app_modes)} only{ports}")
        for cfg, mode in only:
            cmd = (f"python3 {tree}/garnet/mflowgen/common/application/gen_app_bundle.py "
                   f"--config {cfg} --app {app.halide_app_dir} --bundle-dir {bundle_root}/{app.id} "
                   f"--aha-dir {tree} --cgra-tiles --pond-spec {pond_spec}{env}")
            if app.bundle_retry_array:
                cmd = f"retry_bigger {app.bundle_retry_array.replace('x', ' ')} {cmd}"
            lines.append(f"# {cmd}" if app.bundle_blocked else cmd)
    return jobs, notes


def bundle_script(designs, apps, modes, fabric, bundle_root, tree_root, pond_spec):
    """The /aha part as a bash script: one background job per Halide app dir."""
    jobs, notes = bundle_commands(designs, apps, modes, fabric, bundle_root, tree_root, pond_spec)
    lines = ["#!/bin/bash",
             "# /aha: aha venv active, xrun on PATH (module load xcelium).",
             "# Set LAKE and GARNET_REV (the build machine's garnet commit, >= 35e23a53).",
             "# LAKE: a snapshot of lake at the pushed THESIS commit the build machine's rtl",
             "# step will use, not the shared /aha/lake (other sessions edit it). It goes on",
             "# PYTHONPATH and is each tree's lake. E.g. (ORIGIN.txt: gen_app_bundle's lake_git):",
             "#   git -C /aha/lake fetch -q origin THESIS && mkdir -p /aha/sweep_out/lake_snapshot &&",
             "#   git -C /aha/lake archive FETCH_HEAD | tar x -C /aha/sweep_out/lake_snapshot &&",
             "#   git -C /aha/lake rev-parse FETCH_HEAD > /aha/sweep_out/lake_snapshot/ORIGIN.txt",
             "set -u",
             'AHA=${AHA:-/aha}',
             ': "${LAKE:?set LAKE}" "${GARNET_REV:?set GARNET_REV}"',
             'LAKE=$(cd "$LAKE" && pwd)',
             'export PYTHONPATH="$LAKE${PYTHONPATH:+:$PYTHONPATH}"',
             MAKE_TREE, RETRY_BIGGER,
             f"mkdir -p {bundle_root} && echo '{{}}' > {pond_spec}", *notes]
    for job, job_lines in jobs.items():
        if all(c.startswith("#") for c in job_lines):
            lines += job_lines  # every app of this job is blocked
            continue
        lines.append(f"make_tree {tree_root}/{job}")
        body = [c if c.startswith("#") else c + " ;" for c in job_lines]
        lines.append("(\n  " + "\n  ".join(body) + f"\n) > {bundle_root}/{job}.log 2>&1 &")
    lines.append("wait")
    return lines


def power_command(apps, bundle_root, out_dir, fabric, clocks_from):
    """cgra_app_power.py over every app that can be recorded, one variant per app id. On
    the thesis fabric the static PE is timed with registered operands, as the thesis
    tile set (garnet >= 0bc94ef9 --pe-registered-operands; RV/MemCore unchanged)."""
    variants = ",".join(f"{a.id}={bundle_root}/{a.id}" for a in apps if not a.bundle_blocked)
    clock = (f"--clocks-from {clocks_from} --pe-registered-operands" if fabric == "thesis"
             else "--clock-mhz 400")
    return (f"./mflowgen/cgra_app_power.py --any-config --variants {variants} {clock} "
            f"--replay-jobs 4 --out-dir {out_dir} --zip")


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--commands", action="store_true",
                    help="print the /aha bundle script and the build-machine app-power command")
    ap.add_argument("--fabric", choices=FABRICS, default="thesis")
    ap.add_argument("--modes", default="static,rv")
    ap.add_argument("--designs", nargs="+", default=["all"], help="design ids (default all)")
    ap.add_argument("--apps", nargs="+", default=["all"], help="registry.APPS ids (default all)")
    ap.add_argument("--bundle-root", default="/aha/sweep_out/app_bundles_thesis_dp")
    ap.add_argument("--tree-root", default="/aha/sweep_out/app_bundles_thesis_dp_trees",
                    help="private aha trees, one per app (keep them out of --bundle-root)")
    ap.add_argument("--pond-spec", default="/aha/sweep_out/app_bundles_thesis_dp/pond_default.json",
                    help="pond spec JSON ({} = lake's default spec pond, both modes)")
    ap.add_argument("--power-bundle-root", default="/nsim/mstrange/BUILDS/app_bundles_thesis_dp",
                    help="where the bundles are on the build machine")
    ap.add_argument("--power-out-dir", default="/nsim/mstrange/BUILDS/cgra_app_power_thesis_dp")
    ap.add_argument("--clocks-from", default="<OUT>/thesis_tiles_pe/thesis_clocks.json",
                    help="the clock table of the thesis tile set (a fresh thesis_fmax run since "
                         "lassen 0f44a74 made every PE probe stale)")
    args = ap.parse_args(argv)
    designs = (DESIGNS_SINGLE_LEVEL if args.designs == ["all"]
               else [d for d in DESIGNS_SINGLE_LEVEL if d.id in set(args.designs)])
    modes = [m for m in args.modes.split(",") if m]
    if args.commands:
        apps = APPS if args.apps == ["all"] else [a for a in APPS if a.id in set(args.apps)]
        print("\n".join(bundle_script(designs, apps, modes, args.fabric, args.bundle_root,
                                      args.tree_root, args.pond_spec)))
        print("\n# build machine, from /sim/mstrange/BUILD_CGRA/garnet at GARNET_REV (>= 35e23a53), after copying the "
              f"bundles to {args.power_bundle_root}:")
        print("# " + power_command(apps, args.power_bundle_root, args.power_out_dir, args.fabric,
                                   args.clocks_from))
        return 0
    cols = [(f, m) for f in FABRICS for m in MODES]
    print("design_id\t" + "\t".join(f"{f} {m}" for f, m in cols))
    for dp in designs:
        print(dp.id + "\t" + "\t".join(garnet_config(dp, m, f) or "-" for f, m in cols))
    for ids in duplicate_designs(designs):
        print(f"# same garnet spec: {', '.join(ids)}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
