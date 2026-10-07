"""Generate idle + active-power bitstreams and input data for a single Lake spec.

For a given spec (built via tests.test_spec.thesis_sweep.build_four_port_wide_fetch),
emit two bitstreams and one shared random input stream. Programs and data come
from lake.utils.power_test_programs, the same module garnet's Tile_MemCore
power tests (mflowgen/common/memtile-power-test-gen) use, so the standalone and
CGRA-tile numbers are the same workload on the same data:

- Idle: the empty application -- every port controller cleared, nothing fires.
- Active: the most traffic the spec can sustain -- every port streams one
  element per cycle where the memory keeps up, SRAM accesses slotted so the
  memory port(s) are busy every cycle, readers re-reading what their writers
  stored (power_test_programs.static_active_program).

Both variants get the SAME stimulus: a fresh random data_width-bit word on
every input port every cycle after flush release (input_data.hex), valids and
readies high. Only the bitstream differs, so active minus idle is the memory's
own work.

Outputs written to --outdir (default: outputs/):

    bitstream.idle.bs        # hex-format bitstream for the idle case
    bitstream.active.bs      # hex-format bitstream for the active case
    PARGS.idle.txt           # runtime plusargs (identical stimulus to active)
    PARGS.active.txt
    input_data.hex           # 4 x INPUT_STREAM_LEN words, port-major (w0..w3)
    comp_args.txt            # +define+CONFIG_MEMORY_SIZE / NUMBER_PORTS /
                             #  DATA_WIDTH / INPUT_STREAM_LEN
"""

import argparse
import importlib.util
import os
import sys

from lake.utils.power_test_programs import (active_program, idle_program, input_streams,
                                            stream_length)

# tb.sv wires port_w0..w3 / port_r0..r3.
TB_PORTS = 4


# tests/test_spec/thesis_sweep.py is a runnable script, not an installed
# package (no __init__.py under lake/tests/). Load it by path so this
# script works from any CWD — in particular from the mflowgen build dir
# where the workspace is not on sys.path.
def _load_thesis_sweep():
    lake_root = os.environ.get('LAKE_PATH', '/aha/lake')
    ts_path = os.path.join(lake_root,
                           'tests', 'test_spec', 'thesis_sweep.py')
    if not os.path.exists(ts_path):
        raise RuntimeError(
            f"thesis_sweep.py not found at {ts_path}. "
            f"Set LAKE_PATH to the lake repo root if it's elsewhere.")
    spec = importlib.util.spec_from_file_location('thesis_sweep', ts_path)
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


def emit_bitstream(spec, application, path):
    """Run spec.gen_bitstream() and write the hex string to `path`."""
    bs_int = spec.gen_bitstream(application, over=True)
    hex_string = hex(bs_int)[2:]  # strip '0x'
    with open(path, 'w') as f:
        f.write(hex_string)


def write_pargs(path, in_ports, out_ports, n_data, window):
    """Plusargs for tb.sv (synopsys-vcs-sim-power[-gl]); the same for both
    variants. Valids/readies stay high on every used port for the window.

    +power_only=1 skips tb.sv's end-of-run "Not enough data" / "Still seeing
    data" $finish paths: they need a matching gold count, and the idle
    program by definition moves none of the data offered to it.
    +static=1: Runtime.STATIC, which build_four_port_wide_fetch produces."""
    with open(path, 'w') as f:
        for i in range(TB_PORTS):
            f.write(f"+w{i}_num_data={n_data if i < in_ports else 0}\n")
        for i in range(TB_PORTS):
            f.write(f"+r{i}_num_data={n_data if i < out_ports else 0}\n")
        f.write("+static=1\n")
        f.write("+power_only=1\n")
        f.write(f"+max_time={window}\n")


def write_input_data(path, streams, n, data_width):
    """$readmemh image of the per-port streams, port-major: line p*n + t is
    port_w<p>'s word for cycle t after flush release (zeros for unused tb
    ports)."""
    digits = max(1, -(-data_width // 4))
    with open(path, 'w') as f:
        for p in range(TB_PORTS):
            words = streams.get(f"port_w{p}", [0] * n)
            for wd in words:
                f.write(f"{wd:0{digits}x}\n")


def write_comp_args(path, spec, data_width, n):
    """The +defines thesis_sweep.py writes, plus the tb's data width (its
    default is 16) and the input stream length."""
    with open(path, 'w') as f:
        f.write(f"+define+CONFIG_MEMORY_SIZE={spec.get_total_config_size()}\n")
        f.write(f"+define+NUMBER_PORTS={spec.get_num_ports()}\n")
        f.write(f"+define+DATA_WIDTH={data_width}\n")
        f.write(f"+define+INPUT_STREAM_LEN={n}\n")


def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--storage_capacity", type=int, default=8192)
    p.add_argument("--data_width",       type=int, default=16)
    p.add_argument("--fetch_width",      type=int, default=4)
    p.add_argument("--dimensionality",   type=int, default=6)
    p.add_argument("--in_ports",         type=int, default=2)
    p.add_argument("--out_ports",        type=int, default=2)
    p.add_argument("--dual_port",        action="store_true")
    p.add_argument("--vec_capacity",     type=int, default=2)
    p.add_argument("--max_extent",       type=int, default=None)
    p.add_argument("--max_sequence_width", type=int, default=None)
    p.add_argument("--sim_cycles",       type=int, default=1000,
                   help="measurement window (shortened if the spec cannot keep its "
                        "ports busy that long)")
    p.add_argument("--seed",             type=int, default=1,
                   help="input data seed (the CGRA-tile flow uses the same streams "
                        "for the same seed)")
    p.add_argument("--outdir",           type=str, default="outputs")
    p.add_argument("--physical",         action="store_true",
                   help="Pass through to build_four_port_wide_fetch so the "
                        "spec ships a GF_Tech_Map that matches the rtl step.")
    args = p.parse_args()

    os.makedirs(args.outdir, exist_ok=True)

    thesis_sweep = _load_thesis_sweep()
    spec = thesis_sweep.build_four_port_wide_fetch(
        storage_capacity=args.storage_capacity,
        data_width=args.data_width,
        vec_width=args.fetch_width,
        dims=args.dimensionality,
        in_ports=args.in_ports,
        out_ports=args.out_ports,
        dual_port=args.dual_port,
        vec_capacity=args.vec_capacity,
        max_extent=args.max_extent,
        max_sequence_width=args.max_sequence_width,
        physical=args.physical,
    )
    # generate_hardware() populates the internal generator so gen_bitstream,
    # get_total_config_size, get_num_ports and the port limits are valid.
    spec.generate_hardware()

    active = active_program(spec, args.in_ports, args.out_ports, args.fetch_width,
                            args.data_width, args.storage_capacity, args.dual_port,
                            args.sim_cycles)
    window = active["window"]
    n = stream_length(window)

    # Build the active program before the idle one: gen_bitstream's
    # clear_configuration() resets state between calls either way.
    emit_bitstream(spec, active["app"], os.path.join(args.outdir, "bitstream.active.bs"))
    emit_bitstream(spec, idle_program(), os.path.join(args.outdir, "bitstream.idle.bs"))
    for variant in ("idle", "active"):
        write_pargs(os.path.join(args.outdir, f"PARGS.{variant}.txt"),
                    args.in_ports, args.out_ports, n, window)
    write_input_data(os.path.join(args.outdir, "input_data.hex"),
                     input_streams(args.seed, args.in_ports, n, args.data_width),
                     n, args.data_width)
    write_comp_args(os.path.join(args.outdir, "comp_args.txt"), spec, args.data_width, n)

    print(f"OK: idle+active bitstreams, window {window} cycles, {n}-word random "
          f"input streams (seed {args.seed}) -> {args.outdir}", file=sys.stderr)


if __name__ == "__main__":
    main()
