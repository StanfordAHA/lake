"""Generate per-tile round-trip artifacts (bitstream/gold/comp_args/PARGS) from a
clockwork memory-tile config JSON.

The single public entry point ``write_roundtrip_artifacts`` is the inverse of the
collateral path: given the same lake spec parameters that were used to build the
collateral, plus one tile's worth of clockwork config, it writes the four files
the existing RTL-sim harness expects (``bitstream.bs``, ``gold/``,
``comp_args.txt``, ``PARGS.txt``).

This is the lake-side half of the lake → compiler → lake round trip.
"""

import json
import os

from aha.util.sweep_thesis_collateral import build_spec
from lake.utils.util import get_data_sizes


# Output-side clockwork key prefixes. Each represents one read-port stream:
# wide-fetch ``tb2out_X``, regfile ``regfile2out_X``, fw=1 ``mem_out_X``.
_OUT_KEY_PREFIXES = ('tb2out_', 'regfile2out_', 'mem_out_')


def _write_pargs(pargs_path, pargs):
    """Mirrors TestPrepper.add_pargs but writes to a known path without the
    side effects of TestPrepper.prepare_hw_test (which would copy testbench
    files we already have)."""
    with open(pargs_path, 'a') as f:
        for parg_name, parg_value in pargs:
            f.write(f"+{parg_name}={parg_value}\n")


def _enumerate_affine(dim, extents, strides, offset):
    """Enumerate an affine sequence in row-major (innermost-first) order.

    Mirrors ``generate_affine_sequence`` in lake/utils/util.py but yields
    rather than allocating up-front, since clockwork schedules can be large
    (4096+ events per port for conv_3_3).
    """
    seq_size = 1
    for i in range(dim):
        seq_size *= extents[i]
    it = [0] * dim
    for _ in range(seq_size):
        v = offset
        for j in range(dim):
            v += it[j] * strides[j]
        yield v
        for j in range(dim):
            it[j] += 1
            if it[j] == extents[j]:
                it[j] = 0
            else:
                break


def _compute_clockwork_gold(tile_json, in_ports, load_latency=0, data_width=16):
    """Compute gold (data, time) for each output port of a clockwork tile.

    Models the lake memory hierarchy as a flat FIFO: input port writes
    sequential values 0, 1, 2, ... and each output port reads them out in
    the order determined by its tb2out / mem_out / regfile2out schedule.
    This is correct when ``agg2sram_X`` and ``sram2tb_X`` (or ``mem_in_X``
    and ``mem_out_X`` / ``in2regfile_X`` and ``regfile2out_X``) have matching
    address patterns (every word written is read back in the same order),
    which is the case for streaming conv-style schedules. Schedules with
    reuse / non-matching write/read patterns are not modeled here.

    ``load_latency`` is added to every emit cycle for the ``regfile2out_*``
    and ``mem_out_*`` (fw=1) patterns, where clockwork's cycle_starting_addr
    encodes the read-fire cycle and data appears load_latency cycles later.
    The wide-fetch ``tb2out_*`` path does not need this because the tb2out
    schedule already represents the data-out cycle (TB drains immediately).

    Returns ``{port_idx: {'time': [...], 'data': [...], 'name': 'port_rN'}}``
    where ``port_idx`` matches ``Spec._convert_clockwork_to_port_config``'s
    convention (``in_ports + idx``).
    """
    out = {}
    for key in sorted(tile_json):
        ks = str(key)
        for prefix in _OUT_KEY_PREFIXES:
            if not ks.startswith(prefix):
                continue
            try:
                idx = int(ks.split('_')[-1])
            except ValueError:
                break  # not an indexed key — skip
            entry = tile_json[key]
            if not isinstance(entry, dict):
                break
            dim = entry.get('dimensionality', 1)
            extents = list(entry.get('extent', [1] * dim))[:dim]
            strides = list(entry.get('cycle_stride', [0] * dim))[:dim]
            cs = entry.get('cycle_starting_addr', [0])
            offset = cs[0] if isinstance(cs, list) and cs else (cs or 0)
            if len(extents) < dim:
                extents = extents + [1] * (dim - len(extents))
            if len(strides) < dim:
                strides = strides + [0] * (dim - len(strides))
            # Apply load_latency offset only to fw=1 paths (regfile/mem_out).
            # tb2out's cycle_starting_addr already encodes the data-out cycle.
            extra = load_latency if prefix in ('regfile2out_', 'mem_out_') else 0
            times = list(_enumerate_affine(dim, extents, strides, offset + extra))
            mask = (1 << data_width) - 1
            data = [i & mask for i in range(len(times))]
            port_idx = in_ports + idx
            out[port_idx] = {
                'time': times,
                'data': data,
                'name': f'port_r{idx}',
            }
            break
    return out


def write_roundtrip_artifacts(spec_factory_kwargs, tile_clockwork_json_path,
                              output_dir):
    """Write bitstream.bs / gold / comp_args.txt / PARGS.txt for one clockwork tile.

    Args:
        spec_factory_kwargs: dict of kwargs passed to ``build_spec`` (must
            include at minimum the same params that produced the collateral:
            ``storage_capacity``, ``data_width``, ``vec_width``, ``dims``,
            ``in_ports``, ``out_ports``, ``dual_port``).
        tile_clockwork_json_path: path to the per-tile clockwork JSON
            extracted by ``clockwork-roundtrip-compile``.
        output_dir: destination dir; ``inputs/`` and ``gold/`` are created.

    Returns:
        dict with at least ``status`` ('ok' or a failure tag) and, on success,
        ``max_time`` and ``num_outputs``. Failures are reported in-band rather
        than raised so the caller can keep iterating across tiles.
    """
    inputs_dir = os.path.join(output_dir, "inputs")
    # verify_gold(mflowgen=True) reads from <dir>/inputs/gold/, so the gold
    # dir must live inside inputs/, not at the top level.
    gold_dir = os.path.join(inputs_dir, "gold")
    outputs_dir = os.path.join(output_dir, "outputs")
    os.makedirs(inputs_dir, exist_ok=True)
    os.makedirs(gold_dir, exist_ok=True)
    os.makedirs(outputs_dir, exist_ok=True)

    spec = build_spec(**spec_factory_kwargs)
    spec.generate_hardware()

    with open(tile_clockwork_json_path) as f:
        tile_json = json.load(f)

    try:
        port_config = spec._convert_clockwork_to_port_config(tile_json)
    except Exception as e:
        return {"status": "convert_failed", "error": f"{type(e).__name__}: {e}"}

    # Empty port config: the converter found no recognizable port entries.
    # This usually means the tile JSON is malformed or uses key names
    # the converter doesn't understand.
    int_port_keys = [k for k in port_config if isinstance(k, int)]
    if not int_port_keys:
        return {"status": "convert_failed",
                "error": "converter produced no port entries"}

    in_ports = spec_factory_kwargs.get("in_ports", 2)

    # Clockwork-aware gold: model lake's hierarchy as a FIFO from input port
    # to each output port. The simulator (calculate_read_out_vec) doesn't
    # model TB-bank wrap-around or the parallel sram2tb / tb2out interleave
    # the way the hardware does, and produces wrong gold for conv-style
    # schedules. This direct model walks each output port's tb2out / mem_out
    # schedule and assigns sequential output values, matching what the hw
    # actually does when the input is sequential 0, 1, 2, ...
    # Pull load_latency from the spec's collateral so fw=1 regfile/mem_out
    # schedules get the right hardware-emit time in gold (cycle_starting_addr
    # is the read-fire cycle; data lands load_latency cycles later).
    try:
        coll = spec.extract_compiler_information()
        load_latency = int(coll.get('load_latency', 0))
    except Exception:
        load_latency = 0

    try:
        gold_streams = _compute_clockwork_gold(
            tile_json, in_ports,
            load_latency=load_latency,
            data_width=spec_factory_kwargs.get("data_width", 16),
        )
    except Exception as e:
        return {"status": "gold_failed", "error": f"{type(e).__name__}: {e}"}

    if not gold_streams:
        return {"status": "gold_failed",
                "error": "no output ports found in tile JSON"}

    max_time = 0
    for pnum, stream in gold_streams.items():
        port_name = stream['name']
        times = stream['time']
        datas = stream['data']
        if times and times[-1] > max_time:
            max_time = times[-1]
        with open(os.path.join(gold_dir, f"{port_name}_data.txt"), 'w') as fh:
            for d in datas:
                fh.write(f"{d}\n")
        with open(os.path.join(gold_dir, f"{port_name}_time.txt"), 'w') as fh:
            for t in times:
                fh.write(f"{t}\n")

    try:
        # Pass the already-converted port_config with over=True so gen_bitstream
        # skips its internal conversion path — we already converted to compute gold.
        bs = spec.gen_bitstream(port_config, over=True)
    except Exception as e:
        return {"status": "bitstream_failed",
                "error": f"{type(e).__name__}: {e}"}

    with open(os.path.join(inputs_dir, "bitstream.bs"), 'w') as fh:
        fh.write(hex(bs)[2:])

    config_size = spec.get_total_config_size()
    num_ports = spec.get_num_ports()
    data_width = spec_factory_kwargs.get("data_width", 16)
    with open(os.path.join(inputs_dir, "comp_args.txt"), 'w') as fh:
        fh.write(f"+define+CONFIG_MEMORY_SIZE={config_size}\n")
        fh.write(f"+define+NUMBER_PORTS={num_ports}\n")
        fh.write(f"+define+DATA_WIDTH={data_width}\n")

    pargs_path = os.path.join(inputs_dir, "PARGS.txt")
    open(pargs_path, 'w').close()
    out_ports = spec_factory_kwargs.get("out_ports", 2)
    data_sizes = get_data_sizes(port_config, num_ports=in_ports + out_ports)
    _write_pargs(pargs_path, data_sizes)
    _write_pargs(pargs_path, [("max_time", max_time + 15), ("static", 1)])

    return {
        "status": "ok",
        "max_time": max_time,
        "num_outputs": len(gold_streams),
    }
