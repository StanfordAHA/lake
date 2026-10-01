"""Generate the regfile-level (PE-tile pond) lake collateral JSON for clockwork.

The pond analogue of ``spec_config_to_collateral``: the pond spec JSON (the file
garnet takes as ``--lake-pond-spec-config``) -> ``build_cgra_pond`` (the exact
pond garnet hosts) -> ``save_compiler_information(level="regfile")``. Clockwork
reads it as its "regfile" memory-hierarchy level via
``LAKE_COLLATERAL_JSON_REGFILE`` (``aha map --pond-collateral``), next to the
MEM collateral in ``LAKE_COLLATERAL_JSON_MEM`` - one collateral per level.

``--rv`` selects the ready-valid pond (``build_pond_rv``, what garnet builds in
``--lake-spec-mode rv``); default is the static ``build_pond``. Deterministic
(sort_keys) like the MEM collateral.

Usage:
    python -m lake.utils.pond_collateral [--pond-spec pond.json] [--rv] -o pond_collateral.json
"""
import argparse
import json

from lake.spec.spec_memory_controller import build_cgra_pond


def generate(out_path, pond_spec_path=None, rv=False):
    params = {}
    if pond_spec_path:
        with open(pond_spec_path) as f:
            params = json.load(f)
    spec = build_cgra_pond(params, rv=rv)
    spec.save_compiler_information(out_path, level="regfile")
    print(f"[pond->collateral] wrote {out_path} ({'rv' if rv else 'static'} pond, spec {params or 'default'})")
    return out_path


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--pond-spec", default=None,
                    help="pond spec JSON (storage_capacity, dims, data_width, ...); default geometry if omitted")
    ap.add_argument("--rv", action="store_true", help="ready-valid pond (build_pond_rv)")
    ap.add_argument("-o", "--output", required=True, help="path to write the collateral JSON")
    args = ap.parse_args()
    generate(args.output, args.pond_spec, args.rv)


if __name__ == "__main__":
    main()
