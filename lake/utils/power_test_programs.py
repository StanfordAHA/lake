"""Idle / active power-test programs and input data for a lake spec.

Single source for both power-test flows, so the same spec + seed gives the
same port programs and the same input words in each:

- standalone: the bare `lakespec` (pd/thesis/power-test-gen/
  gen_power_bitstreams.py -> synopsys-vcs-sim-power[-gl]/tb.sv)
- CGRA tile: the whole Tile_MemCore (garnet mflowgen/common/
  memtile-power-test-gen/gen_memtile_power_tests.py)

idle   = the empty application: every port controller cleared, nothing fires.
active = the most traffic the spec can sustain (static_active_program /
         rv_active_program).
active, program="full_rate" (opt-in, the THESIS module-characterization
         sweeps): every measured port's controllers fire on every cycle of the
         window (rv_active_program full_rate, static_single_port_program).
Both variants get the SAME input stimulus (input_streams: a fresh random word
per input port per cycle, valids high), so active minus idle is the memory's
own work. Programs are Spec.gen_bitstream(app, over=True) inputs.
"""

import random

# full_rate RV: cycles kept off the stall-free stretch on top of the readers'
# lag (lake SpecSim, fw4 2x2: all ports fire from the readers' first word to
# margin + 2 (multi-dim) / + 4 (1-D) cycles before a writer's first row ends;
# the tile testbench opens the window on its output pins, a few cycles later).
FULL_RATE_GUARD = 10
# full_rate static: schedule offset of the single writer, and the testbench's
# window start (cycles after the first counted cycle), so the window opens
# after the writer's first fire.
STATIC_FULL_RATE_T0 = 4


def port_limits(spec):
    """Per spec-port (index) iteration limits read off the generated HW
    (call after spec.generate_hardware())."""
    out = {}
    for idx in range(spec.get_num_ports()):
        p = spec.get_port_from_idx(idx)
        pid, _, psg = spec.get_port_controllers(port=p)
        sw = getattr(psg, "stride_width", 16)
        out[idx] = {"dims": pid.get_dimensionality(),
                    "ext_cap": 2 ** pid.get_extent_width(),
                    "stride_cap": 2 ** sw - 1}
    return out


def linear_domain(n, dims, ext_cap, cycle_step, stride_cap):
    """Extents / address strides / schedule strides for a linear walk of up to
    n iterations, split over as few dimensions as the spec's limits allow.

    ext_cap: largest extent the iteration domain takes (2**extent_width).
    cycle_step: cycles between consecutive fires (0 = no schedule, RV).
    stride_cap: largest schedule stride the SG register holds.
    Returns (extents, addr_strides, sched_strides, covered_iterations).
    """
    extents, addr, sched = [], [], []
    covered = 1
    for _ in range(dims):
        if covered >= n:
            break
        step = cycle_step * covered
        if cycle_step and step > stride_cap:
            break
        e = min(ext_cap, -(-n // covered))
        if e < 2:
            break
        extents.append(e)
        addr.append(covered)
        sched.append(step)
        covered *= e
    if not extents:
        raise ValueError(f"cannot build a linear domain: n={n} dims={dims} "
                         f"ext_cap={ext_cap} stride_cap={stride_cap}")
    return extents, addr, sched, covered


def idle_program():
    return {"constraints": []}


def static_active_program(spec, in_ports, out_ports, fw, dual_port, words, window, limits):
    """Maximum sustainable static traffic: every port moves one SRAM word per
    period P and streams its fw elements at one per cycle, with every port's
    SRAM access in its own slot of P so the memory port(s) are busy every
    cycle without contention. P = fw when the memory keeps up (all ports
    stream every cycle), else the port count sharing a memory port.
    Each reader re-reads what its writer wrote LAG words earlier, so data
    moves end to end (inputs -> SIPO -> SRAM -> PISO -> outputs).

    Timing within a word follows lake's static wide-fetch linear test
    (tests/test_spec/four_port_single_mp_wide_fetch.py get_linear_test):
    SIPO fill from t, SRAM write at t+fw; SRAM read at r, PISO fill at r+1,
    output from r+2.

    (The first standalone program gave the SIPO/PISO schedules overlapping
    element times -- extents [fw, N], strides [1, 1 or 2] -- which stalls the
    output PISOs after a few elements while the SRAM side fires every cycle
    regardless of data.)

    words: SRAM words. Returns (app, cycles the program keeps every port busy
    after start-up, period)."""
    from lake.utils.spec_enum import Direction
    if dual_port:
        period = max(fw, in_ports, out_ports, 1)
        w_slot = list(range(in_ports))
        r_slot = list(range(out_ports))
    else:
        period = max(fw, in_ports + out_ports, 1)
        w_slot = list(range(in_ports))
        r_slot = [in_ports + j for j in range(out_ports)]
    first_write = 2 * period          # >= fw: first SIPO word complete
    lag = 2                           # words between a write and its read
    startup = first_write + lag * period + period + fw
    n = -(-(window + startup) // period) + 2   # words per port
    pairs = max(min(in_ports, out_ports), 1)
    region = max(words // pairs, 1)
    app = {"constraints": []}
    covered = []

    def domain(idx):
        lim = limits[idx]
        ext, addr, sched, cov = linear_domain(n, lim["dims"], lim["ext_cap"], period,
                                              lim["stride_cap"])
        if fw > 1 and len(ext) > 1:
            # The SIPO/PISO controllers add one dimension (the fw elements);
            # every wide-fetch thesis spec holds the window in 1-D.
            ext, addr, sched = ext[:1], addr[:1], sched[:1]
            cov = ext[0]
        covered.append(cov * period - startup)
        return ext, addr, sched

    def sram_cfg(ext, addr, sched, base, t0):
        return {'dimensionality': len(ext), 'extents': ext,
                'address': {'strides': addr, 'offset': base},
                'schedule': {'strides': sched, 'offset': t0}}

    def elem_cfg(ext, sched, t0):
        # fw elements per word, one per cycle (SIPO/PISO element addresses).
        return {'dimensionality': 1 + len(ext), 'extents': [fw] + ext,
                'address': {'strides': [1, fw], 'offset': 0},
                'schedule': {'strides': [1] + sched, 'offset': t0}}

    for i in range(in_ports):
        idx = spec.port_name_to_int(f"port_w{i}")
        ext, addr, sched = domain(idx)
        t_w = first_write + w_slot[i]
        cfg = sram_cfg(ext, addr, sched, (i % pairs) * region, t_w)
        port = {'type': Direction.IN, 'name': f'port_w{i}', 'config': cfg,
                'vec_constraints': []}
        if fw > 1:
            port['vec_in_config'] = elem_cfg(ext, sched, t_w - fw)
            port['vec_out_config'] = sram_cfg(ext, [1] + addr[1:], sched, 0, t_w)
        else:
            port['vec_in_config'] = port['vec_out_config'] = cfg
        app[idx] = port

    for j in range(out_ports):
        idx = spec.port_name_to_int(f"port_r{j}")
        ext, addr, sched = domain(idx)
        t_r = first_write + lag * period + r_slot[j]
        cfg = sram_cfg(ext, addr, sched, (j % pairs) * region, t_r)
        port = {'type': Direction.OUT, 'name': f'port_r{j}', 'config': cfg,
                'vec_constraints': []}
        if fw > 1:
            port['vec_in_config'] = sram_cfg(ext, [1] + addr[1:], sched, 0, t_r + 1)
            port['vec_out_config'] = elem_cfg(ext, sched, t_r + 2)
        else:
            port['vec_in_config'] = port['vec_out_config'] = cfg
        app[idx] = port

    return app, min(covered) if covered else 0, period


def rv_active_program(spec, in_ports, out_ports, fw, words, window, limits, full_rate=False):
    """Free-flowing RV streams: writer i writes linear addresses, reader i
    reads them back (tests/spec_unit_tests/test_spec_rv_programs.py).

    Domains of >= 2 dimensions: `stream` -- the reader trails its writer by
    the zero-lag margin clockwork uses (2*fw*vec_capacity + 4), dependence at
    level 0; both run concurrently for the whole window.
    1-dimensional domains: gen_bitstream refuses a non-barrier dependence at
    the top level of a power-of-2-dimension domain (the RV comparison
    network's wrap fixup is wrong there), and without one the reader races
    ahead over unwritten words. So `barrier` instead -- the reader starts once
    its writer has finished -- with both phases sized to fit the window (all
    ports share the memory port(s) at fw=1, which every 1-D thesis spec has).

    full_rate (opt-in): every port fires every cycle of the window. 1-D
    domains stream as well (the barrier above predates lake 8748240d; lake
    SpecSim RTL sims, 2026-11-17: a 1-D stream reads back correctly at one
    word per cycle). The window is the stretch where every port fires: from
    the readers' first word (the tile testbench opens the window there,
    window_start "outputs") until a writer reaches the end of its first row.
    Then a multi-dimensional reader stalls until its writer has finished
    (the level-0 dependence across the row wrap), and a 1-D writer is done.
    So the window is the writers' first-row extent minus the readers' lag
    (margin) and FULL_RATE_GUARD, in whole memory-port rounds, at most
    `window`: one row for max_extent 64 / 256, `window` from 1024 up.

    words: elements. Returns (app, window)."""
    from lake.utils.spec_enum import LFComparisonOperator
    LT = LFComparisonOperator.LT.value
    BARRIER = 16383
    pairs = min(in_ports, out_ports)
    region = words // max(pairs, 1)
    region -= region % max(fw, 1)
    margin = 2 * fw * (2 if fw > 1 else 1) + 4
    phased = not full_rate and min(lim["dims"] for lim in limits.values()) < 2
    n = -(-window // max(in_ports + out_ports, 1)) if phased else window + 64
    app = {"constraints": []}
    covered, rows = [], []

    def port(name, base):
        idx = spec.port_name_to_int(name)
        lim = limits[idx]
        ext, addr, _, cov = linear_domain(n, lim["dims"], lim["ext_cap"], 0, 0)
        covered.append(cov)
        if name.startswith("port_w"):
            rows.append(ext[0])
        c = spec.get_base_port_config(name)
        c["config"] = {"dimensionality": len(ext), "extents": ext,
                       "address": {"strides": addr, "offset": base},
                       "schedule": {}, "filter": None}
        c["vec_in_config"], c["vec_out_config"], c["vec_constraints"] = {}, {}, []
        app[idx] = c
        return idx

    for i in range(in_ports):
        port(f"port_w{i}", (i % max(pairs, 1)) * region)
    for j in range(out_ports):
        r_idx = port(f"port_r{j}", (j % max(pairs, 1)) * region)
        if j < in_ports:
            w_idx = spec.port_name_to_int(f"port_w{j}")
            app["constraints"].append((r_idx, 0, w_idx, 0, LT, BARRIER if phased else margin))
    cov = min(covered) if covered else 0
    if phased:
        # Writers, then readers, each phase sharing the memory port(s).
        return app, min(window, (in_ports + out_ports) * cov + 16)
    if full_rate:
        rounds = max(fw, in_ports + out_ports, 1)
        stall_free = min(rows) - margin - FULL_RATE_GUARD if rows else cov
        return app, max(rounds, min(window, stall_free) // rounds * rounds)
    # Free flow moves at most one item per cycle per port.
    return app, min(window, cov)


def static_single_port_program(spec, fw, window, limits):
    """Static full rate (opt-in): one port's controllers fire every cycle.

    A static port steps once per SRAM word, and the ports of a memory port
    share its cycles, so with all ports streaming none fires every cycle
    (static_active_program: once per period P). Here only port_w0 is
    configured: at fw 1 it writes one word every cycle, and every other port
    stays cleared (clock-gated with clock_gate_ports). So each block's active
    power is one port's ID / SG / AG firing every cycle; the reader side and
    the data outputs stay quiet. fw > 1 fills a word in fw cycles, so no
    static port fires every cycle there.

    Returns (app, window, window_start): the window opens window_start
    cycles into the testbench's count, after the writer's first fire."""
    from lake.utils.spec_enum import Direction
    if fw != 1:
        raise ValueError(f"static full-rate program: fw={fw}; only fw 1 fires a static port every cycle")
    idx = spec.port_name_to_int("port_w0")
    lim = limits[idx]
    t0 = STATIC_FULL_RATE_T0
    ext, addr, sched, cov = linear_domain(window + t0 + 8, lim["dims"], lim["ext_cap"], 1,
                                          lim["stride_cap"])
    cfg = {'dimensionality': len(ext), 'extents': ext,
           'address': {'strides': addr, 'offset': 0},
           'schedule': {'strides': sched, 'offset': t0}}
    port = {'type': Direction.IN, 'name': 'port_w0', 'config': cfg, 'vec_constraints': [],
            'vec_in_config': cfg, 'vec_out_config': cfg}
    return {"constraints": [], idx: port}, min(window, cov - 2 * t0), t0


def active_program(spec, in_ports, out_ports, fw, data_width, storage_capacity,
                   dual_port, window, program="default"):
    """The active program for this spec (static or RV, from the generated HW).

    Returns {"app", "window": cycles to measure (<= window: shortened when
    the spec cannot keep its ports busy that long), "min_output_changes": the
    least value changes a word-wide output must show in the window (half the
    elements the program delivers; a coarse floor for RV)}.

    program="full_rate" (opt-in): every measured port fires on every cycle
    of the window (RV: rv_active_program full_rate, every port; static:
    static_single_port_program, port_w0 alone). The result then also has
    "window_start": "outputs" (RV: the testbench opens the window when every
    routed output has delivered a word) or a cycle count (static), and RV
    outputs must change on 90% of the window's cycles. Only the CGRA tile
    testbench (gen_memtile_power_tests.py) honors window_start."""
    limits = port_limits(spec)
    elements = storage_capacity * 8 // data_width
    if program not in ("default", "full_rate"):
        raise ValueError(f"active_program: unknown program {program!r}")
    if program == "full_rate":
        if spec.any_rv_sg:
            app, w = rv_active_program(spec, in_ports, out_ports, fw, elements, window, limits,
                                       full_rate=True)
            return {"app": app, "window": w, "min_output_changes": max(1, w * 9 // 10),
                    "window_start": "outputs"}
        app, w, start = static_single_port_program(spec, fw, window, limits)
        return {"app": app, "window": w, "min_output_changes": 0, "window_start": start}
    if spec.any_rv_sg:
        app, w = rv_active_program(spec, in_ports, out_ports, fw, elements, window, limits)
        return {"app": app, "window": w, "min_output_changes": max(1, w // 16)}
    app, cov, period = static_active_program(spec, in_ports, out_ports, fw, dual_port,
                                             elements // max(fw, 1), window, limits)
    w = min(window, cov)
    return {"app": app, "window": w,
            "min_output_changes": max(1, w * fw // period // 2)}


def stream_length(window):
    """Input words per port: the window plus slack for the programs'
    start-up (a stream index past this wraps)."""
    return window + 64


def input_streams(seed, in_ports, n, data_width):
    """{"port_w<i>": [n random data_width-bit words]}: a fresh word every
    cycle from flush release. Deterministic in (seed, port name) and
    prefix-stable in n, so both flows see identical words per cycle."""
    out = {}
    for i in range(in_ports):
        rng = random.Random(f"lake-power-test:{seed}:port_w{i}")
        out[f"port_w{i}"] = [rng.getrandbits(data_width) for _ in range(n)]
    return out
