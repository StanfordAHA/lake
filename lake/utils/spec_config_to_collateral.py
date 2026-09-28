"""Generate a lake collateral JSON from a single spec_config.json.

This is the ONE canonical ``spec params -> lake_collateral.json`` path, shared
by every context that needs the collateral for a given memory spec:

  * a CGRA build with a lake-spec MemCore (garnet's gen_rtl emits it alongside
    ``design.v``), and
  * a standalone lake spec (the round-trip / thesis power flow).

Because the collateral is a *pure, deterministic function of the spec params*
(``build_spec(**cfg).save_compiler_information`` writes it with
``sort_keys=True``), two independent runs on the same ``spec_config.json`` are
byte-identical. The ``verify-lake-collateral`` mflowgen node relies on that:
it regenerates the collateral in both contexts and asserts they match, turning
"the CGRA and standalone views of the spec agree" into an enforced invariant
(catches lake version skew between the garnet container and the build machine,
or a spec_config that got mis-threaded on one side).

Usage:
    python -m lake.utils.spec_config_to_collateral \
        --spec spec_config.json -o lake_collateral.json
"""
import argparse
import inspect
import json
import sys

from lake.spec.spec_memory_controller import build_spec


def _spec_kwargs(cfg):
    """Keep only keys build_spec accepts, so an enriched spec_config.json
    (extra bookkeeping keys) does not raise a TypeError."""
    accepted = set(inspect.signature(build_spec).parameters)
    kwargs = {k: v for k, v in cfg.items() if k in accepted}
    dropped = sorted(set(cfg) - accepted)
    if dropped:
        print(f"[spec->collateral] ignoring non-build_spec keys: {dropped}",
              file=sys.stderr)
    return kwargs


def generate(spec_path, out_path):
    with open(spec_path) as f:
        cfg = json.load(f)
    spec = build_spec(**_spec_kwargs(cfg))
    # save_compiler_information dumps extract_compiler_information() with
    # indent=2, sort_keys=True -> deterministic, diff-able output.
    spec.save_compiler_information(out_path)
    print(f"[spec->collateral] wrote {out_path} from {spec_path}")
    return out_path


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--spec", required=True,
                    help="path to spec_config.json (build_spec kwargs)")
    ap.add_argument("-o", "--output", required=True,
                    help="path to write lake_collateral.json")
    args = ap.parse_args()
    generate(args.spec, args.output)


if __name__ == "__main__":
    main()
