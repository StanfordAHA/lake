"""Cross-check two lake collateral JSONs and emit the canonical one.

Used by the ``verify-lake-collateral`` mflowgen node. Given the collateral
generated in two independent contexts for the SAME memory spec -- a CGRA build
with a lake-spec MemCore, and a standalone lake spec -- this asserts they
describe the same memory and writes the agreed collateral as the node's output.

The two are expected to be byte-identical (collateral is a deterministic
function of the spec, dumped with sort_keys=True), but we compare *parsed* JSON
so incidental whitespace never trips the check. A mismatch exits non-zero (so
the mflowgen step fails) and prints the differing keys.

Usage:
    python -m lake.utils.verify_collateral \
        --a lake_collateral_cgra.json --b lake_collateral_standalone.json \
        -o lake_collateral.json
"""
import argparse
import json
import sys


def _load(path):
    with open(path) as f:
        return json.load(f)


def _diff_keys(a, b, prefix=""):
    """Yield human-readable descriptions of where two JSON values differ."""
    if isinstance(a, dict) and isinstance(b, dict):
        for k in sorted(set(a) | set(b)):
            p = f"{prefix}.{k}" if prefix else str(k)
            if k not in a:
                yield f"{p}: missing in A (B={b[k]!r})"
            elif k not in b:
                yield f"{p}: missing in B (A={a[k]!r})"
            else:
                yield from _diff_keys(a[k], b[k], p)
    elif a != b:
        yield f"{prefix}: A={a!r} != B={b!r}"


def verify(a_path, b_path, out_path):
    a, b = _load(a_path), _load(b_path)
    diffs = list(_diff_keys(a, b))
    if diffs:
        print(f"[verify-collateral] MISMATCH between {a_path} (A) and "
              f"{b_path} (B):", file=sys.stderr)
        for d in diffs:
            print(f"  {d}", file=sys.stderr)
        print(f"[verify-collateral] {len(diffs)} difference(s) -- the CGRA and "
              f"standalone views of this spec disagree.", file=sys.stderr)
        return False
    # Agreed: write the canonical collateral (A == B, either is fine).
    with open(out_path, "w") as f:
        json.dump(a, f, indent=2, sort_keys=True)
    print(f"[verify-collateral] OK: collateral matches; wrote {out_path}")
    return True


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--a", required=True, help="collateral from context A (CGRA)")
    ap.add_argument("--b", required=True,
                    help="collateral from context B (standalone lake)")
    ap.add_argument("-o", "--output", required=True,
                    help="path to write the agreed canonical collateral")
    args = ap.parse_args()
    if not verify(args.a, args.b, args.output):
        sys.exit(1)


if __name__ == "__main__":
    main()
