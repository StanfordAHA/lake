"""Standalone lakespec stimulus from an app run on the CGRA (an app bundle).

garnet's gen_app_bundle.py records, next to the tile-port VCD, every MEM
tile's lake controller ports (core.vcd: the lakespec_mem_flat instance in
Tile_MemCore, core_ports.json). The tile's lakespec takes its program on a
parallel config_memory bus (config_passthru) whose bit layout is the standalone
lakespec's serial bitstream (same spec -> same gen_bitstream), so one MEM tile
of the app replays into the standalone design as:

    bitstream.app.bs     the tile's config_memory value (hex, like gen_bitstream)
    input_data.app.hex   each input port's value at every clock edge from the
                         first cycle out of flush (port-major, 4 tb ports)
    PARGS.app.txt        +static=1 +power_only=1, max_time = app window
    comp_args.app.txt    CONFIG_MEMORY_SIZE / NUMBER_PORTS / DATA_WIDTH /
                         INPUT_STREAM_LEN for tb.sv
    app_stimulus.json    tile, window, flush release, and every output port's
                         (cycle, value) on valid -- the replay check

Alignment: edge 0 = the first rising edge where the design sees flush low.
tb.sv (synopsys-vcs-sim-power) reads its stream index THIS_CYC_COUNT, a
nonblocking counter, in the time step it increments it, so word k reaches the
design at edge k+1 (word 0 at edges 0 and 1). Word k is therefore the tile's
input at edge k+1 (--input-shift 1); then the standalone outputs equal the
tile's lakespec outputs in value and cycle (conv_3_3 line buffer, 2x4096 taps;
shift 0 gets 3471 of 4096 wrong). Only edge 0 sees a different input word.
comp_args.app.txt raises tb.sv's MAX_DATA_SIZE to the largest output count.

With the spec flags (the graph's spec params) the bundle's spec must match
this standalone spec (build_spec defaults filled), or the step fails.

Static spec only (the standalone flow has no ready-valid lakespec).

    python gen_app_stimulus.py --bundle <bundle dir> [--tile Tile_X07_Y05] --outdir outputs \\
        [--storage_capacity 4096 --fetch_width 4 ...]
"""

import argparse
import json
import os
import re
import sys

TB_PORTS = 4
# lake build_spec's defaults (garnet sweep_specs.SPEC_DEFAULTS): a spec JSON
# may leave any of them out.
SPEC_DEFAULTS = dict(storage_capacity=4096, data_width=16, vec_width=4, dims=6,
                     in_ports=2, out_ports=2, dual_port=False, vec_capacity=2,
                     max_extent=None, max_sequence_width=None)
# this script's flag -> build_spec key
SPEC_FLAGS = dict(storage_capacity="storage_capacity", data_width="data_width",
                  fetch_width="vec_width", dimensionality="dims", in_ports="in_ports",
                  out_ports="out_ports", dual_port="dual_port", vec_capacity="vec_capacity",
                  max_extent="max_extent", max_sequence_width="max_sequence_width")


def read_vcd(path, scope_suffixes):
    """{scope: {signal: [(time, value_int_or_None)]}} for the scopes whose
    dotted path ends with one of scope_suffixes (x/z -> None)."""
    want = {}          # vcd id -> list of (scope, name)
    scopes, stack = {}, []
    changes = {}
    with open(path) as f:
        it = iter(f)
        for line in it:
            tok = line.split()
            if not tok:
                continue
            if tok[0] == "$scope":
                stack.append(tok[2])
            elif tok[0] == "$upscope":
                stack.pop()
            elif tok[0] == "$var":
                scope = ".".join(stack)
                if any(scope.endswith(s) for s in scope_suffixes):
                    vid, name = tok[3], tok[4]
                    want.setdefault(vid, []).append((scope, name))
                    scopes.setdefault(scope, {})[name] = []
            elif tok[0] == "$enddefinitions":
                break
        t = 0
        for line in it:
            if not line or line[0] in "$\n":
                continue
            c = line[0]
            if c == "#":
                t = int(line[1:])
                continue
            if c in "bB":
                val, vid = line[1:].split()
            elif c in "01xXzZ":
                val, vid = c, line[1:].strip()
            else:
                continue          # real / string values: not on these ports
            if vid not in want:
                continue
            v = None if re.search(r"[xXzZ]", val) else int(val, 2)
            for scope, name in want[vid]:
                scopes[scope][name].append((t, v))
    return scopes


def sample_at_rising_edges(sig, clk):
    """Value of sig just before each rising edge of clk (what a flop samples)."""
    edges = [t for (t, v), (_, pv) in zip(clk[1:], clk[:-1]) if v == 1 and pv == 0]
    if clk and clk[0][1] == 1:
        pass
    out, i, cur = [], 0, None
    for e in edges:
        while i < len(sig) and sig[i][0] < e:
            cur = sig[i][1]
            i += 1
        out.append(cur)
    return edges, out


def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--bundle", required=True)
    p.add_argument("--tile", default=None,
                   help="MEM tile (Tile_X..._Y...); default: the programmed lakespec tile "
                        "with the most output traffic")
    p.add_argument("--outdir", default="outputs")
    p.add_argument("--input-shift", type=int, default=1,
                   help="word k = the tile's input at edge k+SHIFT (1 = tb.sv's timing)")
    p.add_argument("--tail", type=int, default=16,
                   help="cycles simulated after the last output")
    for flag in SPEC_FLAGS:
        p.add_argument(f"--{flag}", default=None,
                       help="this standalone spec's value; checked against the bundle's spec")
    args = p.parse_args()

    b = args.bundle
    manifest = json.load(open(os.path.join(b, "manifest.json")))
    if manifest.get("mode") != "static":
        sys.exit("*** the standalone lakespec flow is static-only; bundle mode is "
                 f"{manifest.get('mode')}")
    core = json.load(open(os.path.join(b, "core_ports.json")))
    spec = manifest["spec_config"]
    given = {key: getattr(args, flag) for flag, key in SPEC_FLAGS.items()
             if getattr(args, flag) not in (None, "")}
    if given:
        def norm(v):
            return {"true": True, "false": False}.get(str(v).lower(), int(v) if str(v).isdigit() else v)
        ours = dict(SPEC_DEFAULTS, **{k: norm(v) for k, v in given.items()})
        theirs = dict(SPEC_DEFAULTS, **spec)
        bad = {k: (theirs.get(k), ours[k]) for k in ours if theirs.get(k) != ours[k]}
        if bad:
            sys.exit(f"*** app bundle spec does not match this standalone spec "
                     f"(key: bundle, here): {bad}")
    dw = spec.get("data_width", 16)
    tiles = []
    for line in open(os.path.join(b, "tiles_Tile_MemCore.list")):
        f = line.strip().split(",")
        if len(f) >= 3:
            tiles.append(f"Tile_X{f[-2]}_Y{f[-1]}")
    suffixes = [f"{t}.{core['path']}" for t in tiles]
    vcd = read_vcd(os.path.join(b, "core.vcd"), suffixes)

    in_ports = sorted((n for n, _ in core["inputs"] if re.fullmatch(r"port_\d+_f_", n)),
                      key=lambda n: int(n.split("_")[1]))
    out_ports = sorted((n for n, _ in core["outputs"] if re.fullmatch(r"port_\d+_f_", n)),
                       key=lambda n: int(n.split("_")[1]))
    cfg_name = next(n for n, _ in core["inputs"] if n.endswith("config_memory"))
    cfg_width = dict(core["inputs"])[cfg_name]

    per_tile = {}
    for tile, suffix in zip(tiles, suffixes):
        scope = next((s for s in vcd if s.endswith(suffix)), None)
        if scope is None:
            continue
        sig = vcd[scope]
        edges, flush = sample_at_rising_edges(sig["flush"], sig["clk"])
        # first edge out of flush after the (last) flush pulse
        hi = [k for k, v in enumerate(flush) if v == 1]
        if not hi:
            continue
        start = hi[-1] + 1
        _, cfg = sample_at_rising_edges(sig[cfg_name], sig["clk"])
        config = cfg[start] if start < len(cfg) else None
        outs = {}
        for o in out_ports:
            _, data = sample_at_rising_edges(sig[o], sig["clk"])
            _, valid = sample_at_rising_edges(sig[o.replace("_f_", "_valid_f_")], sig["clk"])
            outs[o] = [(k - start, data[k] & ((1 << dw) - 1)) for k in range(start, len(edges))
                       if valid[k] == 1 and data[k] is not None]
        per_tile[tile] = dict(scope=scope, start=start, edges=edges, config=config, outs=outs)

    programmed = {t: d for t, d in per_tile.items() if d["config"]}
    if args.tile:
        if args.tile not in per_tile:
            sys.exit(f"*** {args.tile} not in core.vcd ({sorted(per_tile)})")
        tile = args.tile
    else:
        if not programmed:
            sys.exit("*** no MEM tile with a programmed lakespec in this bundle")
        tile = max(programmed, key=lambda t: sum(len(v) for v in programmed[t]["outs"].values()))
    d = per_tile[tile]
    if not d["config"]:
        sys.exit(f"*** {tile}'s lakespec is not programmed (config_memory 0): not in lakespec mode")
    sig = vcd[d["scope"]]
    start = d["start"]
    last = max([c for v in d["outs"].values() for c, _ in v] or [0])
    window = min(last + args.tail, len(d["edges"]) - start)
    n = window

    streams = {}
    for i, name in enumerate(in_ports):
        _, data = sample_at_rising_edges(sig[name], sig["clk"])
        streams[i] = [(data[start + k + args.input_shift] or 0) & ((1 << dw) - 1)
                      if 0 <= start + k + args.input_shift < len(data) else 0 for k in range(n)]

    os.makedirs(args.outdir, exist_ok=True)
    o = args.outdir
    with open(os.path.join(o, "bitstream.app.bs"), "w") as f:
        f.write(f"{d['config']:x}")
    digits = max(1, -(-dw // 4))
    with open(os.path.join(o, "input_data.app.hex"), "w") as f:
        for pt in range(TB_PORTS):
            for w in streams.get(pt, [0] * n):
                f.write(f"{w:0{digits}x}\n")
    with open(os.path.join(o, "PARGS.app.txt"), "w") as f:
        for i in range(TB_PORTS):
            f.write(f"+w{i}_num_data={n if i < len(in_ports) else 0}\n")
        for i in range(TB_PORTS):
            cnt = len(d["outs"][out_ports[i]]) if i < len(out_ports) else 0
            f.write(f"+r{i}_num_data={cnt}\n")
        f.write("+static=1\n+power_only=1\n")
        f.write(f"+max_time={window}\n")
    with open(os.path.join(o, "comp_args.app.txt"), "w") as f:
        f.write(f"+define+CONFIG_MEMORY_SIZE={cfg_width}\n")
        f.write(f"+define+NUMBER_PORTS={len(in_ports) + len(out_ports)}\n")
        f.write(f"+define+DATA_WIDTH={dw}\n")
        f.write(f"+define+INPUT_STREAM_LEN={n}\n")
        f.write(f"+define+MAX_DATA_SIZE={max([4096] + [len(v) for v in d['outs'].values()])}\n")
    json.dump({"bundle": os.path.abspath(b), "app": manifest.get("app"), "tile": tile,
               "spec_config": spec, "window": window, "flush_release_edge": start,
               "in_ports": in_ports, "out_ports": out_ports,
               "programmed_tiles": sorted(programmed),
               "outputs": {k: v for k, v in d["outs"].items()}},
              open(os.path.join(o, "app_stimulus.json"), "w"))
    print(f"OK: {tile} of {manifest.get('app')}: window {window} cycles, outputs "
          f"{ {k: len(v) for k, v in d['outs'].items()} }, programmed MEM tiles "
          f"{sorted(programmed)} -> {o}", file=sys.stderr)


if __name__ == "__main__":
    main()
