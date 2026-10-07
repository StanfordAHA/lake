"""Functional tests for lake.spec.address_generator.AddressGenerator (the
address of a spec Port: one per Port, driven by the Port's IterationDomain
(mux_sel / restart / finished / iterators) and stepped with it, see spec.py).

Configuration: gen_bitstream(address_map={'offset', 'strides'}, extents,
dimensionality); addr_width = clog2(sum of the memory ports' num_addrs) +
clog2(width_mult).

Function: for the Port's current iteration (the ID's iterators)
    addr_out = (offset + sum_i strides[i] * iterators[i]) mod 2**addr_width
  * recurrence=True (what the spec builders use): addr_out is a register kept
    incrementally: on a cycle with step & ~finished (& clk_en), restart ->
    offset, else += delta[mux_sel], delta[j] = strides[j] - sum_{k<j}
    (extent_k - 1) * strides[k] (the address change when level j increments
    and every lower level wraps); gen_bitstream precomputes the deltas.
    flush (sync, over clk_en) -> offset; rst_n -> 0, i.e. like the whole spec
    the unit needs a flush after configuration (the CGRA / lake tb flush);
  * recurrence=False: addr_out is combinational in the iterators input.
"""
import random

import pytest

from lake.spec.address_generator import AddressGenerator
from lake.spec.iteration_domain import IterationDomain
from rtl_harness import pack, pack_config, requires_xrun, run_vectors
from test_iteration_domain import IterationDomainModel, id_stimulus


class MemPortStub:
    """AddressGenerator.gen_hardware only asks its memory ports for their
    number of addresses."""

    def __init__(self, num_addrs):
        self.num_addrs = num_addrs

    def get_num_addrs(self):
        return self.num_addrs


def affine(offset, strides, it, aw):
    return (offset + sum(s * i for s, i in zip(strides, it))) % (1 << aw)


def level_deltas(strides, extents):
    """Address change when level j increments and all lower levels wrap."""
    return [strides[j] - sum((extents[k] - 1) * strides[k] for k in range(j)) for j in range(len(strides))]


class AddressGeneratorModel:
    """recurrence=True register: reset 0, flush -> offset, step -> restart /
    += delta[mux_sel] (unconfigured levels: delta 0)."""

    def __init__(self, aw, offset, deltas, dims):
        self.aw = aw
        self.offset = offset
        self.deltas = list(deltas) + [0] * (dims - len(deltas))
        self.addr = 0

    def cycle(self, c):
        if not c.get("rst_n", 1):
            self.addr = 0
            return self.addr
        out = self.addr
        if c.get("flush", 0):
            self.addr = self.offset
        elif c.get("clk_en", 1) and c.get("step", 0) and not c.get("finished", 0):
            if c.get("restart", 0):
                self.addr = self.offset
            else:
                self.addr = (self.addr + self.deltas[c.get("mux_sel", 0)]) % (1 << self.aw)
        return out


def make_ag(dims, ew, num_addrs, width_mult, recurrence):
    idg = IterationDomain(dimensionality=dims, extent_width=ew)
    idg.gen_hardware()
    ag = AddressGenerator(dimensionality=dims, recurrence=recurrence)
    if width_mult != 1:
        ag.set_width_mult(width_mult)
    ag.gen_hardware(memports=[MemPortStub(num_addrs)], id=idg)
    return ag


def id_driven_cycles(dims, ew, dim, extents, seed):
    """AG inputs from an IterationDomainModel under random step / clk_en /
    flush / reset (flush in cycle 0 and after the reset pulse, as the unit
    requires). Returns (cycles, iterators-per-cycle, flushed-per-cycle)."""
    steps = id_stimulus(IterationDomainModel(dims, ew, dim, extents), seed=seed, ctrl=True)
    steps = [{"step": 0, "flush": 1}] + steps
    for k in range(len(steps) - 1):
        if not steps[k].get("rst_n", 1):
            steps[k + 1]["flush"] = 1
    m = IterationDomainModel(dims, ew, dim, extents)
    cycles, its, flushed = [], [], []
    init = False
    for s in steps:
        o = m.cycle(s.get("step", 0), s.get("flush", 0), s.get("clk_en", 1), s.get("rst_n", 1))
        if not s.get("rst_n", 1):
            init = False
        cycles.append(dict(s, mux_sel=o["mux_sel"], restart=o["restart"], finished=o["finished"],
                           iterators=pack(o["iterators"], ew)))
        its.append(o["iterators"])
        flushed.append(init)
        if s.get("flush", 0) and s.get("rst_n", 1):
            init = True
    return cycles, its, flushed


# (dims, extent_width, num_addrs, width_mult, dimensionality, extents, offset, strides)
CONFIGS = [
    (1, 4, 64, 1, 1, [7], 5, [3]),
    (3, 4, 128, 1, 2, [4, 3], 2, [1, 4]),                        # row major, dim < dims
    (3, 4, 128, 1, 3, [2, 3, 2], 10, [5, 1, 20]),                 # full, transposed inner levels
    (4, 5, 256, 1, 4, [3, 2, 2, 3], 200, [-1, -3, 7, -50]),       # full, negative strides
    (3, 3, 64, 1, 3, [2, 2, 2], 0, [1, 2, 4]),                    # minimum extents
    (2, 4, 16, 4, 2, [4, 3], 40, [1, 6]),                         # width_mult 4 (opt_rv word addressing)
    (6, 3, 512, 1, 6, [2, 3, 2, 2, 2, 2], 17, [1, 2, 6, 12, 24, 48]),
    (2, 3, 32, 1, 2, [8, 2], 1, [2, 16]),                         # extent 2**extent_width
]


def _cfg_id(c):
    return f"D{c[0]}_w{c[1]}_n{c[2]}_wm{c[3]}_dim{c[4]}_" + "x".join(map(str, c[5]))


def test_address_generator_width():
    for num_addrs, wm, want in ((64, 1, 6), (100, 1, 7), (16, 4, 6), (512, 2, 10)):
        assert make_ag(2, 4, num_addrs, wm, True).get_address_width() == want


@pytest.mark.parametrize("cfg", CONFIGS, ids=_cfg_id)
def test_address_generator_model_matches_affine_map(cfg):
    """The incremental (recurrence) model and the affine address agree over a
    whole run of the loop nest."""
    dims, ew, n, wm, dim, extents, offset, strides = cfg
    aw = make_ag(dims, ew, n, wm, True).get_address_width()
    cycles, its, flushed = id_driven_cycles(dims, ew, dim, extents, seed=3)
    m = AddressGeneratorModel(aw, offset, level_deltas(strides, extents), dims)
    for c, it, init in zip(cycles, its, flushed):
        got = m.cycle(c)
        assert got == (affine(offset, strides, it, aw) if init else 0)


@requires_xrun
@pytest.mark.parametrize("recurrence", [True, False], ids=["recurrence", "explicit"])
@pytest.mark.parametrize("cfg", CONFIGS, ids=_cfg_id)
def test_address_generator_rtl_follows_iteration_domain(cfg, recurrence):
    """Driven by the iteration domain (random step gating, clk_en low, flush
    mid-run and after finished, reset + flush, steps past the end): every cycle
    addr_out = offset + strides . iterators (recurrence: 0 between reset and
    the first flush)."""
    dims, ew, n, wm, dim, extents, offset, strides = cfg
    ag = make_ag(dims, ew, n, wm, recurrence)
    aw = ag.get_address_width()
    config = pack_config(ag.gen_bitstream({"offset": offset, "strides": strides}, extents, dim))
    cycles, its, flushed = id_driven_cycles(dims, ew, dim, extents, seed=dims * 5 + dim)
    exp = [{"addr_out": affine(offset, strides, it, aw) if (init or not recurrence) else 0}
           for it, init in zip(its, flushed)]
    res = run_vectors(ag, cycles, exp, config=config)
    assert res.passed, res


def random_controls(dims, dim, ew, n, seed):
    rng = random.Random(seed)
    cycles = [{"flush": 1}]
    for i in range(n):
        c = {"step": int(rng.random() < 0.7), "mux_sel": rng.randrange(max(dim, 1)),
             "restart": int(rng.random() < 0.1), "finished": int(rng.random() < 0.15),
             "clk_en": int(rng.random() > 0.15), "flush": int(rng.random() < 0.03),
             "iterators": rng.getrandbits(ew * dims)}
        if i == n // 2:
            c["rst_n"] = 0
        cycles.append(c)
    return cycles


@requires_xrun
@pytest.mark.parametrize("cfg", [CONFIGS[0], CONFIGS[3], CONFIGS[5], CONFIGS[6]], ids=_cfg_id)
def test_address_generator_rtl_random_controls(cfg):
    """recurrence=True with arbitrary (not loop-consistent) mux_sel / restart /
    finished / step / clk_en / flush / reset: the register semantics."""
    dims, ew, n, wm, dim, extents, offset, strides = cfg
    ag = make_ag(dims, ew, n, wm, True)
    aw = ag.get_address_width()
    config = pack_config(ag.gen_bitstream({"offset": offset, "strides": strides}, extents, dim))
    cycles = random_controls(dims, dim, ew, 300, seed=dims)
    m = AddressGeneratorModel(aw, offset, level_deltas(strides, extents), dims)
    exp = [{"addr_out": m.cycle(c)} for c in cycles]
    res = run_vectors(ag, cycles, exp, config=config)
    assert res.passed, res


@requires_xrun
@pytest.mark.parametrize("cfg", [CONFIGS[1], CONFIGS[3], CONFIGS[6]], ids=_cfg_id)
def test_address_generator_rtl_explicit_random_iterators(cfg):
    """recurrence=False: addr_out is the affine map of whatever the iterators
    input holds (any value of the extent_width field), step etc. irrelevant."""
    dims, ew, n, wm, dim, extents, offset, strides = cfg
    ag = make_ag(dims, ew, n, wm, False)
    aw = ag.get_address_width()
    config = pack_config(ag.gen_bitstream({"offset": offset, "strides": strides}, extents, dim))
    cycles = random_controls(dims, dim, ew, 200, seed=dims + 1)
    exp = []
    for c in cycles:
        it = [(c.get("iterators", 0) >> (ew * i)) & ((1 << ew) - 1) for i in range(dims)]
        exp.append({"addr_out": affine(offset, strides[:dim], it[:dim], aw)})
    res = run_vectors(ag, cycles, exp, config=config)
    assert res.passed, res
