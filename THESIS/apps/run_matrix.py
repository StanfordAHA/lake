#!/usr/bin/env python3
"""Top-level driver for the app-mapping harness.

Cross-iterates ``design_points.DESIGNS_SINGLE_LEVEL × registry.APPS``,
invokes the round-trip flow per cell, and drops one ``results.json`` at
``THESIS/data/apps/<design_id>/<app_id>/results.json`` containing:

    {
      "design_id": "...",
      "app_id": "...",
      "sim_status": "PASS" | "FAIL" | "SKIP",
      "cycles": <int>,
      "max_time_ns": <float>,
      "active_cycles": <int>,        # from tb.sv util counter (TODO)
      "total_cycles": <int>,
      "synth_area_um2": <float>,     # from extract_power_area
      "pnr_area_um2": <float>,
      "synth_power_w": <float>,      # from ptpx-synth (roundtrip SAIF)
      "pnr_power_w": <float>,        # from ptpx-gl (roundtrip SAIF)
      "clock_period_ps": <float>,
      "tile_count": {"mem": <int>, "pe": <int>},
    }

**This file is a skeleton.** The functions raise NotImplementedError
where the mflowgen / round-trip plumbing still needs to be
parameterized. See the milestone plan in ``THESIS/apps/README.md``.

Usage (once implemented):
    python3 -m THESIS.apps.run_matrix --apps all --designs single_level
    python3 -m THESIS.apps.run_matrix --apps matmul_agg --designs port_exp_baseline
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

from .design_points import DESIGNS_SINGLE_LEVEL, DesignPoint
from .registry import APPS, AppSpec, unverified

REPO_ROOT = Path(__file__).resolve().parents[2]
RESULTS_ROOT = REPO_ROOT / "THESIS" / "data" / "apps"


def run_one_cell(design: DesignPoint, app: AppSpec, dry_run: bool = False) -> dict:
    """Run one (design, app) cell end-to-end.

    Concrete steps this needs to do (all TODO):
      1. Ensure the design's mflowgen build dir has run through synth
         (``make 7`` in ``$THESIS_BUILDS/<design.experiment>/<...>``).
      2. Invoke ``run_clockwork.py`` with ``--app-dir=<app.halide_app_dir>``
         and ``--testname=<app.testname>`` to produce per-tile
         ``bitstream.bs`` / ``gold`` / ``PARGS`` for THIS app.
         (Currently hardcoded to conv_3_3 in
         ``pd/thesis/clockwork-roundtrip-compile/configure.yml`` and
         ``ASPLOS_EXP/run_roundtrip_sweep.sh:51``.)
      3. Run the round-trip sim step, extract ``cycles`` +
         ``active_cycles`` + ``total_cycles`` from ``outputs/util.txt``
         (TODO: add the counter block to
         ``pd/thesis/synopsys-vcs-sim-rtl/tb.sv``).
      4. Run ``synopsys-ptpx-synth`` on the round-trip SAIF (already
         wired in ``pd/thesis/construct-commercial-full.py`` — needs
         per-app dispatch).
      5. Parse PPA from the extractor + tile count from the clockwork
         manifest, aggregate, return the results dict.
    """
    if dry_run:
        return {
            "design_id": design.id,
            "app_id": app.id,
            "sim_status": "SKIP",
            "note": "dry run — harness not implemented yet",
        }
    raise NotImplementedError(
        "run_one_cell: mflowgen dispatch not yet parameterized per app. "
        "See milestone plan in THESIS/apps/README.md §6."
    )


def write_result(design: DesignPoint, app: AppSpec, result: dict) -> Path:
    outdir = RESULTS_ROOT / design.id / app.id
    outdir.mkdir(parents=True, exist_ok=True)
    outpath = outdir / "results.json"
    outpath.write_text(json.dumps(result, indent=2))
    return outpath


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--apps", nargs="+", default=["all"],
                    help="App IDs (see registry.APPS) or 'all'.")
    ap.add_argument("--designs", nargs="+", default=["all"],
                    help="Design IDs (see design_points.DESIGNS_SINGLE_LEVEL) or 'all'.")
    ap.add_argument("--dry-run", action="store_true",
                    help="Skip actual dispatch; write SKIP results (useful for wiring test).")
    args = ap.parse_args(argv)

    if unverified():
        print("error: some AppSpec fields still contain '??' — pin them down before running:",
              file=sys.stderr)
        for a in unverified():
            print(f"  - {a.id}: {a.halide_app_dir}", file=sys.stderr)
        return 2

    if not DESIGNS_SINGLE_LEVEL:
        print("error: DESIGNS_SINGLE_LEVEL is empty — populate design_points.py first",
              file=sys.stderr)
        return 2

    apps = APPS if args.apps == ["all"] else [a for a in APPS if a.id in set(args.apps)]
    designs = (DESIGNS_SINGLE_LEVEL if args.designs == ["all"]
               else [d for d in DESIGNS_SINGLE_LEVEL if d.id in set(args.designs)])

    for design in designs:
        for app in apps:
            print(f"→ {design.id} × {app.id}", file=sys.stderr)
            result = run_one_cell(design, app, dry_run=args.dry_run)
            outpath = write_result(design, app, result)
            print(f"  wrote {outpath}", file=sys.stderr)

    return 0


if __name__ == "__main__":
    sys.exit(main())
