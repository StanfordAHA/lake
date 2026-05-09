#!/usr/bin/env python3
"""Per-tile round-trip sim driver, shared between
``clockwork-roundtrip-sim-rtl`` and ``clockwork-roundtrip-sim-synth``.

For each ``inputs/map_results/tile_<idx>.json`` the upstream
``clockwork-roundtrip-compile`` step produced:

  1. Stage a per-tile sim dir ``cfg_<idx>/`` with copies of design.v / sram.v /
     tb.sv / Makefile / test_comparison.py.
  2. Generate ``inputs/{bitstream.bs, comp_args.txt, PARGS.txt}`` and
     ``inputs/gold/`` via ``lake.utils.clockwork_roundtrip.write_roundtrip_artifacts``.
  3. Run ``make sim`` (compile + run + compare). The Makefile's ``compare``
     target invokes ``test_comparison.py`` which in turn calls
     ``verify_gold(dir, mflowgen=True)``.
  4. Tee per-tile log into the step's ``mflowgen-run.log`` so the
     ``'PASS' in mflowgen-run.log`` postcondition fires only when every tile
     passed.

After the loop, write ``outputs/roundtrip_results.json`` summarizing per-tile
status. Echo "Test FAILED for tile_<idx>" for any failure so the
``'FAIL' not in mflowgen-run.log`` postcondition trips.
"""

import argparse
import json
import os
import shutil
import subprocess
import sys
import time


def stage_cfg_dir(cfg_dir, design_v, sram_v, tb_sv, makefile, test_comparison_py,
                  run_sim_tcl=None):
    """Set up a per-tile sim build dir with all the static files."""
    inputs = os.path.join(cfg_dir, "inputs")
    outputs = os.path.join(cfg_dir, "outputs")
    os.makedirs(inputs, exist_ok=True)
    os.makedirs(outputs, exist_ok=True)
    shutil.copy2(design_v, os.path.join(inputs, "design.v"))
    shutil.copy2(sram_v, os.path.join(inputs, "sram.v"))
    shutil.copy2(tb_sv, os.path.join(cfg_dir, "tb.sv"))
    shutil.copy2(makefile, os.path.join(cfg_dir, "Makefile"))
    shutil.copy2(test_comparison_py, os.path.join(cfg_dir, "test_comparison.py"))
    if run_sim_tcl is not None and os.path.exists(run_sim_tcl):
        shutil.copy2(run_sim_tcl, os.path.join(cfg_dir, "run_sim.tcl"))


def run_make_sim(cfg_dir, log_path, dump_vcd=0, timeout=1800):
    """Run ``make sim`` in cfg_dir; tee combined stdout/stderr to log_path.

    Returns (rc, ran_to_completion). The Makefile's compare target prints
    "Test PASSED!" on success and "Test FAILED..." on mismatch.
    """
    env = os.environ.copy()
    env["DUMP_VCD"] = str(dump_vcd)
    with open(log_path, 'a') as logfh:
        proc = subprocess.Popen(
            ["make", "sim"], cwd=cfg_dir, env=env,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
            text=True, bufsize=1,
        )
        try:
            for line in proc.stdout:
                logfh.write(line)
                logfh.flush()
            proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            proc.kill()
            logfh.write(f"\n[run_roundtrip_sim] TIMEOUT after {timeout}s\n")
            return 124, False
    return proc.returncode, True


def parse_pass_fail(log_path):
    """Read tail of log to determine PASS/FAIL. test_comparison.py prints
    'Test PASSED!' or 'Test FAILED...' as the last meaningful line."""
    if not os.path.exists(log_path):
        return False, "log missing"
    with open(log_path) as f:
        text = f.read()
    if "Test PASSED!" in text:
        return True, None
    # Try to surface a useful error excerpt
    lines = [ln for ln in text.splitlines() if ln.strip()]
    tail = " | ".join(lines[-5:])
    return False, tail[-300:] if tail else "no output"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--map-results', required=True,
                    help="dir of tile_<idx>.json from roundtrip-compile")
    ap.add_argument('--design-v',    required=True, help="design.v to sim against")
    ap.add_argument('--sram-v',      required=True, help="sram.v from gen_sram")
    ap.add_argument('--tb-sv',       required=True, help="testbench.sv (from rtl)")
    ap.add_argument('--makefile',    required=True, help="Makefile (sim recipe)")
    ap.add_argument('--test-comparison',
                    default=None,
                    help="test_comparison.py path (defaults next to Makefile)")
    ap.add_argument('--variant',     required=True, choices=['rtl', 'synth'])
    ap.add_argument('--params',      required=True,
                    help="JSON of spec_factory_kwargs")
    ap.add_argument('--results-out', required=True)
    ap.add_argument('--log',         required=True, help="combined PASS/FAIL log")
    ap.add_argument('--dump-vcd',    type=int, default=0)
    args = ap.parse_args()

    if args.test_comparison is None:
        args.test_comparison = os.path.join(os.path.dirname(args.makefile),
                                            "test_comparison.py")

    spec_kwargs = json.loads(args.params)

    # Defer this import: it pulls in lake spec modules and is slow.
    from lake.utils.clockwork_roundtrip import write_roundtrip_artifacts

    tile_jsons = sorted(
        os.path.join(args.map_results, f)
        for f in os.listdir(args.map_results)
        if f.startswith("tile_") and f.endswith(".json")
    )
    if not tile_jsons:
        with open(args.log, 'a') as f:
            f.write("[roundtrip-sim] FAIL: no tile_*.json found\n"
                    "Test FAILED — no tiles to run\n")
        with open(args.results_out, 'w') as f:
            json.dump({"status": "no_tiles", "tiles": []}, f, indent=2)
        sys.exit(1)

    results = []
    overall_pass = True

    for tile_path in tile_jsons:
        idx = os.path.splitext(os.path.basename(tile_path))[0].split("_", 1)[1]
        cfg_dir = os.path.abspath(f"cfg_{idx}")
        os.makedirs(cfg_dir, exist_ok=True)

        with open(args.log, 'a') as f:
            f.write(f"\n[tile_{idx}] === starting ({args.variant}) ===\n")

        stage_cfg_dir(cfg_dir, args.design_v, args.sram_v, args.tb_sv,
                      args.makefile, args.test_comparison)

        t0 = time.time()
        artifact_result = write_roundtrip_artifacts(spec_kwargs, tile_path, cfg_dir)
        if artifact_result.get("status") != "ok":
            msg = (f"[tile_{idx}] artifact gen FAILED: "
                   f"{artifact_result.get('status')}: "
                   f"{artifact_result.get('error', '')}")
            with open(args.log, 'a') as f:
                f.write(msg + "\n")
                f.write(f"Test FAILED for tile_{idx}: artifact gen\n")
            results.append({"tile": idx, "status": "artifact_failed",
                            "error": artifact_result.get("error")})
            overall_pass = False
            continue

        rc, ran = run_make_sim(cfg_dir, args.log, dump_vcd=args.dump_vcd)
        elapsed = time.time() - t0
        passed, err = parse_pass_fail(args.log)
        if not passed or rc != 0:
            with open(args.log, 'a') as f:
                f.write(f"Test FAILED for tile_{idx}: rc={rc}, err={err}\n")
            results.append({"tile": idx, "status": "sim_failed",
                            "rc": rc, "elapsed_s": elapsed, "error": err})
            overall_pass = False
        else:
            with open(args.log, 'a') as f:
                f.write(f"[tile_{idx}] PASS (elapsed {elapsed:.1f}s)\n")
            results.append({"tile": idx, "status": "ok",
                            "elapsed_s": elapsed})

    summary = {
        "variant": args.variant,
        "overall": "PASS" if overall_pass else "FAIL",
        "tiles": results,
    }
    with open(args.results_out, 'w') as f:
        json.dump(summary, f, indent=2)

    if overall_pass:
        with open(args.log, 'a') as f:
            f.write(f"\nAll round-trip tiles PASS ({args.variant})\n")
        sys.exit(0)
    else:
        with open(args.log, 'a') as f:
            f.write(f"\nRound-trip sim FAILED for at least one tile ({args.variant})\n")
        sys.exit(1)


if __name__ == "__main__":
    main()
