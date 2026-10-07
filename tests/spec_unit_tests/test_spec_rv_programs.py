"""Spec-level tests: ready-valid port programs on whole lake MEM specs
(build_spec_rv thesis shapes and garnet's default build_four_port_wide_fetch_rv,
physical=False, remote storage modelled by spec_rtl.SpecSim).

These exercise the units together (Port incl. wide-fetch SIPO/PISO and the
opt_rv write-combining / read-prefetch paths, ID / AG / RV SG, the RV
comparison network, memory-port arbitration) through their function:

  * stream:  writer writes N items (N not a multiple of the fetch width),
             reader trails at level 0 by the zero-lag margin the clockwork
             converter uses (2 * fw * vec_capacity + 4)      -> reads 0..N-1
  * rowlag:  W x H image, row pitch padded to the fetch width, reader one row
             behind the writer (row-level RAW, scalar 1)       -> reads 0..WH-1
  * barrier: reader re-reads the N-item buffer K times, only after the writer
             finished (scalar 16383 never passes in range)     -> reads k % N
  * sweep:   accumulator (resnet conv bank): init writer w_i writes N items,
             then S read-modify-write sweeps r_u -> PE(+add) -> w_u over the
             same N addresses (sweep RAW [1, 1, -1], barrier on the init),
             then a final read after the last sweep            -> a + S*add
Every pattern runs on independent port pairs at disjoint addresses at once
(so ports contend for the shared memory port), free-flow and under stress.
"""
import math

import pytest

from lake.spec.spec_memory_controller import build_spec_rv, build_four_port_wide_fetch_rv
from lake.utils.spec_enum import LFComparisonOperator
from rtl_harness import requires_xrun
from spec_rtl import SpecSim

LT = LFComparisonOperator.LT.value
BARRIER = 16383

SPECS = {
    "fw1_dp_in1_out1": (build_spec_rv, dict(storage_capacity=1024, data_width=16, vec_width=1,
                                            dual_port=True, in_ports=1, out_ports=1)),
    "fw2_sp_in1_out1": (build_spec_rv, dict(storage_capacity=2048, data_width=16, vec_width=2,
                                            in_ports=1, out_ports=1)),
    "fw2_dp_in2_out2": (build_spec_rv, dict(storage_capacity=2048, data_width=16, vec_width=2,
                                            dual_port=True, in_ports=2, out_ports=2)),
    "fw4_sp_in2_out2": (build_spec_rv, dict(storage_capacity=4096, data_width=16, vec_width=4,
                                            in_ports=2, out_ports=2)),
    "garnet_default": (build_four_port_wide_fetch_rv, dict(storage_capacity=4096, data_width=16, vec_width=4)),
}
_SIMS = {}


def sim_for(name):
    if name not in _SIMS:
        builder, kw = SPECS[name]
        _SIMS[name] = SpecSim(builder(physical=False, **kw))
    return _SIMS[name]


def spec_shape(name):
    kw = SPECS[name][1]
    return kw.get("vec_width", 4), kw.get("in_ports", 2), kw.get("out_ports", 2), kw["storage_capacity"] // 2


def port(spec, name, extents, strides, offset):
    c = spec.get_base_port_config(name)
    c["config"] = {"dimensionality": len(extents), "extents": list(extents),
                   "address": {"strides": list(strides), "offset": offset}, "schedule": {}, "filter": None}
    c["vec_in_config"], c["vec_out_config"], c["vec_constraints"] = {}, {}, []
    return c


class Program:
    def __init__(self, spec):
        self.spec = spec
        self.cfg = {"constraints": []}
        self.sizes, self.loopback, self.expect = {}, {}, {}

    def add(self, name, extents, strides, offset):
        self.cfg[self.spec.port_name_to_int(name)] = port(self.spec, name, extents, strides, offset)
        self.sizes[name] = math.prod(extents)

    def dep(self, this, this_lvl, on, on_lvl, scalar):
        s = self.spec
        self.cfg["constraints"].append((s.port_name_to_int(this), this_lvl, s.port_name_to_int(on), on_lvl, LT, scalar))


def build(name, pattern):
    sim = sim_for(name)
    fw, n_in, n_out, words = spec_shape(name)
    prog = Program(sim.spec)
    pairs = min(n_in, n_out)
    if pattern == "sweep":
        pairs //= 2
    region = words // max(pairs, 1)
    region -= region % max(fw, 1)
    add = 5
    for p in range(pairs):
        base = p * region
        if pattern == "stream":
            n = min(203, region)
            w, r = f"port_w{p}", f"port_r{p}"
            prog.add(w, [n], [1], base)
            prog.add(r, [n], [1], base)
            prog.dep(r, 0, w, 0, 2 * fw * (2 if fw > 1 else 1) + 4)
            prog.expect[r] = list(range(n))
        elif pattern == "rowlag":
            W, H = 30, max(2, min(12, region // 32))
            pitch = -(-W // fw) * fw
            w, r = f"port_w{p}", f"port_r{p}"
            prog.add(w, [W, H], [1, pitch], base)
            prog.add(r, [W, H], [1, pitch], base)
            prog.dep(r, 1, w, 1, 1)
            prog.expect[r] = list(range(W * H))
        elif pattern == "barrier":
            n, K = min(37, region), 3
            w, r = f"port_w{p}", f"port_r{p}"
            prog.add(w, [n], [1], base)
            prog.add(r, [n, K], [1, 0], base)
            prog.dep(r, 1, w, 0, BARRIER)
            prog.expect[r] = [k % n for k in range(n * K)]
        elif pattern == "sweep":
            n, S = min(24, region), 4
            wi, wu = f"port_w{2 * p}", f"port_w{2 * p + 1}"
            ru, rf = f"port_r{2 * p}", f"port_r{2 * p + 1}"
            prog.add(wi, [n], [1], base)
            prog.add(wu, [n, S], [1, 0], base)
            prog.add(ru, [n, S], [1, 0], base)
            prog.add(rf, [n], [1], base)
            prog.dep(ru, 1, wu, 1, -1)          # sweep s once w_u is in sweep s
            prog.dep(ru, 1, wi, 0, BARRIER)     # after the init
            prog.dep(rf, 0, wu, 1, BARRIER)     # after the last sweep
            prog.loopback[wu] = ru
            prog.expect[ru] = [(a + s * add) & 0xffff for s in range(S) for a in range(n)]
            prog.expect[rf] = [(a + S * add) & 0xffff for a in range(n)]
        prog.add_value = add
    return sim, prog


CASES = [(s, p) for s in SPECS for p in ("stream", "rowlag", "barrier")] + \
        [(s, "sweep") for s in SPECS if min(spec_shape(s)[1:3]) >= 2]


@requires_xrun
@pytest.mark.parametrize("stress", [False, True])
@pytest.mark.parametrize("spec_name,pattern", CASES)
def test_spec_rv_program(spec_name, pattern, stress):
    sim, prog = build(spec_name, pattern)
    bs = sim.spec.gen_bitstream(prog.cfg, over=True)
    # stall writer 0 early and reader 0 mid-way through the program
    outs, done, cycles = sim.run(bs, prog.sizes, loopback=prog.loopback, stress=stress, seed=3,
                                 name=f"{pattern}_{int(stress)}", lb_add=prog.add_value,
                                 writer_stall=(20, 120), reader_stall=(150, 350))
    got = {r: len(outs.get(r, [])) for r in prog.expect}
    assert done, f"{spec_name}/{pattern}: timeout after {cycles} cycles, outputs {got}"
    for r, exp in prog.expect.items():
        bad = [(i, g, e) for i, (g, e) in enumerate(zip(outs[r], exp)) if g != e]
        assert len(outs[r]) == len(exp) and not bad, f"{spec_name}/{pattern} {r}: first mismatches {bad[:5]}"
