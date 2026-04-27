#!/usr/bin/env python3
"""Sanity-check the generated lake_collateral.json against per-config mflowgen parameters.

Run from inside the rtl step's working dir; fails (non-zero exit) if any
field disagrees, which fails the step and surfaces in synth-pool failure logs.
"""
import argparse
import json
import sys


def _parse_bool(s):
    if s in ('1', 'True', 'true'):
        return True
    if s in ('0', 'False', 'false'):
        return False
    raise argparse.ArgumentTypeError(f"expected boolean, got {s!r}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--collateral', required=True)
    ap.add_argument('--storage_capacity', type=int, required=True, help='bytes')
    ap.add_argument('--data_width',       type=int, required=True, help='bits')
    ap.add_argument('--fetch_width',      type=int, required=True)
    ap.add_argument('--dimensionality',   type=int, required=True)
    ap.add_argument('--in_ports',         type=int, required=True)
    ap.add_argument('--out_ports',        type=int, required=True)
    ap.add_argument('--dual_port',        type=_parse_bool, required=True,
                    help='True/False or 1/0')
    args = ap.parse_args()

    with open(args.collateral) as f:
        c = json.load(f)

    errs = []

    if c.get('fetch_width') != args.fetch_width:
        errs.append(f"fetch_width: collateral={c.get('fetch_width')} vs config={args.fetch_width}")

    if c.get('iteration_level') != args.dimensionality:
        errs.append(f"iteration_level: collateral={c.get('iteration_level')} vs config={args.dimensionality}")

    if c.get('interconnect_in_num') != args.in_ports:
        errs.append(f"interconnect_in_num: collateral={c.get('interconnect_in_num')} vs config={args.in_ports}")

    if c.get('interconnect_out_num') != args.out_ports:
        errs.append(f"interconnect_out_num: collateral={c.get('interconnect_out_num')} vs config={args.out_ports}")

    if c.get('dual_port_sram') != args.dual_port:
        errs.append(f"dual_port_sram: collateral={c.get('dual_port_sram')} vs config={args.dual_port}")

    # capacity in collateral is entries; convert back to bytes per controller
    # (entries * word_width * data_width_bytes) and require config bytes appears.
    data_width_bytes = args.data_width // 8
    cap_bytes_per_ctrl = {
        cname: c['capacity'][cname] * c['word_width'].get(cname, 1) * data_width_bytes
        for cname in c.get('capacity', {})
    }
    if args.storage_capacity not in cap_bytes_per_ctrl.values():
        errs.append(
            f"storage_capacity: config={args.storage_capacity} bytes not in per-ctrl bytes "
            f"{cap_bytes_per_ctrl} (raw entries={dict(c.get('capacity', {}))}, "
            f"word_width={dict(c.get('word_width', {}))}, data_width_bytes={data_width_bytes})"
        )

    if errs:
        print("COLLATERAL VALIDATION FAILED:")
        for e in errs:
            print(f"  - {e}")
        sys.exit(1)

    print("COLLATERAL VALIDATION PASSED")
    print(f"  fetch_width={c['fetch_width']}, iteration_level={c['iteration_level']}, "
          f"in/out_num={c['interconnect_in_num']}/{c['interconnect_out_num']}, "
          f"dual_port_sram={c['dual_port_sram']}, capacity_bytes={cap_bytes_per_ctrl}")


if __name__ == '__main__':
    main()
