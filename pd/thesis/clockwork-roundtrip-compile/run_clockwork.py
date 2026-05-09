#!/usr/bin/env python3
"""Compile conv_3_3 against a lake collateral, then split the resulting CoreIR
JSON into per-tile clockwork configs for the downstream round-trip sim steps.

Wraps ``aha.util.sweep_thesis_collateral.run_clockwork_compilation`` for the
clockwork invocation, then walks the produced ``<testname>.json`` (CoreIR top)
to extract each memory-tile instance's lake config. Each tile's config is
written as ``<out>/tile_<idx>.json`` in the format
``Spec._convert_clockwork_to_port_config`` accepts (top-level keys
``in2agg_*``/``agg2sram_*``/``sram2tb_*``/``tb2out_*`` or ``mem_*``).

Exit code is non-zero on any failure so the mflowgen step is marked failed
(strict policy: a build that reaches synthesis is expected to compile cleanly).
"""

import argparse
import json
import os
import re
import sys


# Tile config keys recognized by Spec._is_clockwork_format / _convert_clockwork_to_port_config.
_TILE_KEY_PREFIXES = ('in2agg_', 'agg2sram_', 'sram2tb_', 'tb2out_',
                      'mem_', 'in2regfile_', 'regfile2out_')

# Some clockwork emissions write keys without an index suffix (e.g. ``agg2sram``
# instead of ``agg2sram_0``). Normalize those so the converter can consume them.
_NORMALIZE_RE = re.compile(r'^(in2agg|agg2sram|sram2tb|tb2out|mem_in|mem_out)$')


def _looks_like_tile_config(d):
    if not isinstance(d, dict):
        return False
    return any(any(str(k).startswith(p) or _NORMALIZE_RE.match(str(k)) for p in _TILE_KEY_PREFIXES)
               for k in d)


def _normalize_keys(cfg):
    """Add ``_0`` suffix to bare ``agg2sram``/``sram2tb`` etc. keys."""
    out = {}
    for k, v in cfg.items():
        ks = str(k)
        if _NORMALIZE_RE.match(ks):
            out[f"{ks}_0"] = v
        else:
            out[ks] = v
    return out


def _extract_inst_config(inst):
    """Pull the lake config dict out of one CoreIR instance.

    Modern clockwork emits ``cgralib.Mem_amber`` instances with the config at
    ``inst['metadata']['config']``. Older formats put it under
    ``inst['modargs']['config']`` (sometimes wrapped in a list-of-dicts).
    Returns the first dict that looks like a tile config, or None.
    """
    if not isinstance(inst, dict):
        return None

    candidates = []
    for top_key in ('metadata', 'modargs'):
        sub = inst.get(top_key)
        if isinstance(sub, dict) and 'config' in sub:
            candidates.append(sub['config'])

    for cfg in candidates:
        if isinstance(cfg, str):
            # Sometimes serialized as a JSON string.
            try:
                import json as _json
                cfg = _json.loads(cfg)
            except Exception:
                continue
        if isinstance(cfg, list):
            for x in cfg:
                if isinstance(x, dict):
                    cfg = x
                    break
        if isinstance(cfg, dict) and _looks_like_tile_config(cfg):
            return cfg

    return None


def extract_tile_configs(coreir_json_path):
    """Walk a clockwork-produced CoreIR JSON and return one tile config per
    memory-tile instance."""
    with open(coreir_json_path) as f:
        j = json.load(f)
    modules = j.get('namespaces', {}).get('global', {}).get('modules', {})

    tile_configs = []
    for mod_name, mod in modules.items():
        if '_ub' not in mod_name or 'instances' not in mod:
            continue
        for inst_name, inst in mod['instances'].items():
            cfg = _extract_inst_config(inst)
            if cfg is None:
                continue
            tile_configs.append({
                'module': mod_name,
                'instance': inst_name,
                'config': _normalize_keys(cfg),
            })
    return tile_configs


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--collateral', required=True)
    ap.add_argument('--app-dir', required=True)
    ap.add_argument('--clockwork-path', required=True)
    ap.add_argument('--testname', required=True)
    ap.add_argument('--out', required=True, help="output dir for tile_<idx>.json")
    ap.add_argument('--manifest', required=True, help="output manifest path")
    args = ap.parse_args()

    os.makedirs(args.out, exist_ok=True)

    # Lazy import: aha.util.sweep_thesis_collateral pulls in lake spec modules
    # which take a few seconds to import. Skip if we're just listing.
    from aha.util.sweep_thesis_collateral import run_clockwork_compilation

    print(f"[roundtrip-compile] running clockwork against {args.collateral}", flush=True)
    success, err = run_clockwork_compilation(
        os.path.abspath(args.collateral), args.app_dir, args.clockwork_path)

    coreir_path = os.path.join(args.app_dir, "bin", "map_result",
                               args.testname, f"{args.testname}.json")

    manifest = {
        "collateral": os.path.abspath(args.collateral),
        "app_dir": args.app_dir,
        "testname": args.testname,
        "clockwork_success": bool(success),
        "clockwork_error": err,
        "tiles": [],
    }

    if not success:
        with open(args.manifest, 'w') as f:
            json.dump(manifest, f, indent=2)
        print(f"[roundtrip-compile] FAIL: clockwork did not succeed: {err}", flush=True)
        sys.exit(1)

    if not os.path.exists(coreir_path):
        with open(args.manifest, 'w') as f:
            json.dump(manifest, f, indent=2)
        print(f"[roundtrip-compile] FAIL: expected {coreir_path} missing", flush=True)
        sys.exit(2)

    # Forensics — copy the raw CoreIR alongside the per-tile splits.
    import shutil
    shutil.copy2(coreir_path, os.path.join(args.out, f"{args.testname}.json"))
    mm_path = os.path.join(os.path.dirname(coreir_path),
                           f"{args.testname}_to_metamapper.json")
    if os.path.exists(mm_path):
        shutil.copy2(mm_path, os.path.join(args.out, f"{args.testname}_to_metamapper.json"))

    tile_configs = extract_tile_configs(coreir_path)
    if not tile_configs:
        with open(args.manifest, 'w') as f:
            json.dump(manifest, f, indent=2)
        print(f"[roundtrip-compile] FAIL: no memory-tile configs found in {coreir_path}",
              flush=True)
        sys.exit(3)

    for idx, tc in enumerate(tile_configs):
        tile_path = os.path.join(args.out, f"tile_{idx}.json")
        with open(tile_path, 'w') as f:
            json.dump(tc['config'], f, indent=2)
        manifest['tiles'].append({
            "idx": idx,
            "module": tc['module'],
            "instance": tc['instance'],
            "path": os.path.relpath(tile_path, os.path.dirname(args.manifest)),
            "keys": sorted(tc['config'].keys()),
        })
        print(f"[roundtrip-compile] tile_{idx}: {tc['instance']} ({len(tc['config'])} keys)",
              flush=True)

    with open(args.manifest, 'w') as f:
        json.dump(manifest, f, indent=2)
    print(f"[roundtrip-compile] OK: {len(tile_configs)} tile(s) extracted", flush=True)


if __name__ == "__main__":
    main()
