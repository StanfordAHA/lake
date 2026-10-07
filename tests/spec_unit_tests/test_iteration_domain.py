"""Functional tests for lake.spec.iteration_domain.IterationDomain (the loop
nest of a spec Port: one per Port, stepped by the Port's schedule generator
(static) or memory-port grant (ready-valid); its mux_sel / restart / finished /
iterators drive the Port's address and schedule generators, see spec.py).

Configuration: gen_bitstream(dimensionality, extents) with each extent encoded
as extent - 2, so an extent is in [2, 2**extent_width + 1].

Function:
  * iterators[0..dim-1] enumerate the loop nest in lexicographic order, level 0
    innermost (fastest); levels >= dimensionality stay 0; the iteration only
    changes at a posedge of a cycle with step (and clk_en);
  * mux_sel (while step, else 0) = the level that increments on this step: the
    lowest level not at its last value; every lower level wraps to 0;
  * last_iter = the current iteration is the last one of the nest;
  * restart = step on the last iteration: every level wraps to 0 and the
    domain becomes finished;
  * finished (registered): set by that step, held until flush; further steps
    do not move the iterators (they stay 0; last_iter stays 1, so restart
    follows step and mux_sel is 0);
  * extents_out = the configured (encoded) extents;
  * rst_n (async) and flush (sync, over clk_en) return to iteration 0, not
    finished; clk_en = 0 freezes the state (the combinational outputs still
    follow step).
"""
import itertools
import random

import pytest

from lake.spec.iteration_domain import IterationDomain
from rtl_harness import pack, pack_config, requires_xrun, run_vectors


class IterationDomainModel:
    """Cycle model of the loop nest (not of the RTL's max_value flags)."""

    def __init__(self, dims, extent_width, dimensionality, extents):
        assert len(extents) == dimensionality <= dims
        self.dims = dims
        self.extent_width = extent_width
        self.dim = dimensionality
        self.extents = list(extents)
        self.reset()

    def reset(self):
        self.it = [0] * self.dims
        self.finished = False

    def level(self):
        """Level that increments on the next step; None on the last iteration."""
        if self.finished:
            return None
        for i in range(self.dim):
            if self.it[i] < self.extents[i] - 1:
                return i
        return None

    def outputs(self, step):
        lvl = self.level()
        return {"iterators": list(self.it),
                "mux_sel": lvl if (step and lvl is not None) else 0,
                "restart": int(bool(step) and lvl is None),
                "last_iter": int(lvl is None),
                "finished": int(self.finished)}

    def tick(self, step, flush=0, clk_en=1):
        """Posedge at the end of a cycle."""
        if flush:
            self.reset()
            return
        if not (clk_en and step):
            return
        lvl = self.level()
        if lvl is None:
            self.it = [0] * self.dims
            self.finished = True
        else:
            self.it[lvl] += 1
            for k in range(lvl):
                self.it[k] = 0

    def cycle(self, step, flush=0, clk_en=1, rst_n=1):
        """Outputs seen during a cycle with these inputs, then its posedge."""
        if not rst_n:
            self.reset()
            return self.outputs(step)
        out = self.outputs(step)
        self.tick(step, flush, clk_en)
        return out


def iteration_sequence(extents):
    """Loop-nest iterations in execution order, level 0 fastest."""
    return [list(reversed(t)) for t in itertools.product(*[range(e) for e in reversed(extents)])]


def make_id(dims, extent_width, dimensionality, extents, rv=False):
    dut = IterationDomain(dimensionality=dims, extent_width=extent_width)
    dut.gen_hardware()
    cfg = pack_config(dut.gen_bitstream(dimensionality, list(extents), rv))
    return dut, cfg


def id_stimulus(model, seed, step_p=0.6, ctrl=False, past_end=8):
    """Per-cycle {'step', ...} inputs: random step gating until the domain
    finishes, then steps past the end. With ctrl, also random clk_en low and:
    a flush mid-run (on a stepping cycle: flush wins), run to finished, a flush
    after finished, part of a run, an async reset pulse, run to finished.
    ``model`` (a fresh IterationDomainModel) only tracks when a run finished."""
    rng = random.Random(seed)
    n_iter = len(iteration_sequence(model.extents))
    cycles = []

    def add(c):
        model.cycle(c.get("step", 0), c.get("flush", 0), c.get("clk_en", 1), c.get("rst_n", 1))
        cycles.append(c)

    def rnd():
        c = {"step": int(rng.random() < step_p)}
        if ctrl:
            c["clk_en"] = int(rng.random() > 0.2)
        return c

    def run_to_finished():
        while not model.finished:
            add(rnd())
        for _ in range(past_end):
            add(dict(rnd(), step=1))

    if ctrl:
        for _ in range(n_iter // 2 + 2):
            add(rnd())
        add({"step": 1, "flush": 1})
        run_to_finished()
        add({"step": 1, "flush": 1, "clk_en": 0})
        for _ in range(n_iter // 2 + 2):
            add(rnd())
        add({"step": 1, "rst_n": 0})
    run_to_finished()
    return cycles


def expected(model, cycles, dims, extent_width, enc_extents):
    exp = []
    for c in cycles:
        o = model.cycle(c.get("step", 0), c.get("flush", 0), c.get("clk_en", 1), c.get("rst_n", 1))
        o["iterators"] = pack(o["iterators"], extent_width)
        o["extents_out"] = pack(enc_extents + [0] * (dims - len(enc_extents)), extent_width)
        exp.append(o)
    return exp


# (dims, extent_width, dimensionality, extents)
CONFIGS = [
    (1, 4, 1, [5]),
    (3, 5, 1, [2]),                  # minimum extent, dimensionality < dims
    (3, 5, 2, [3, 2]),
    (3, 5, 3, [2, 3, 4]),            # full dimensionality, non-power-of-2 dims
    (4, 6, 4, [3, 2, 2, 3]),         # full dimensionality, power-of-2 dims
    (5, 6, 3, [4, 2, 3]),
    (6, 4, 6, [2, 2, 2, 2, 2, 2]),   # full 6-D, all minimum extents
    (2, 3, 2, [8, 2]),               # extent 2**extent_width: largest whose indices fit
    (3, 5, 0, []),                   # 0-D: one (empty) iteration
]


def _cfg_id(c):
    return f"D{c[0]}_w{c[1]}_dim{c[2]}_" + "x".join(map(str, c[3]))


@pytest.mark.parametrize("cfg", CONFIGS, ids=_cfg_id)
def test_iteration_domain_model_enumerates_loop_nest(cfg):
    dims, ew, dim, extents = cfg
    m = IterationDomainModel(dims, ew, dim, extents)
    seq = iteration_sequence(extents)
    for k, want in enumerate(seq):
        assert m.it[:dim] == want and not any(m.it[dim:])
        o = m.outputs(1)
        assert o["last_iter"] == (k == len(seq) - 1) and not o["finished"]
        assert o["restart"] == (k == len(seq) - 1)
        if k + 1 < len(seq):
            nxt = seq[k + 1]
            changed = [i for i in range(dim) if nxt[i] != want[i]]
            assert o["mux_sel"] == max(changed) and nxt[o["mux_sel"]] == want[o["mux_sel"]] + 1
            assert all(nxt[i] == 0 for i in range(o["mux_sel"]))
        m.tick(1)
    for _ in range(3):      # past the end
        o = m.outputs(1)
        assert o["finished"] and o["last_iter"] and o["iterators"] == [0] * dims
        m.tick(1)


def test_iteration_domain_bitstream_rejects_unencodable_extents():
    dut = IterationDomain(dimensionality=2, extent_width=4)
    dut.gen_hardware()
    for bad in (1, 0, (1 << 4) + 1, (1 << 4) + 2):
        with pytest.raises(ValueError):
            dut.gen_bitstream(2, [3, bad])
    dut.gen_bitstream(2, [2, 1 << 4])           # both ends of the accepted range


@requires_xrun
@pytest.mark.parametrize("cfg", CONFIGS, ids=_cfg_id)
def test_iteration_domain_rtl(cfg):
    dims, ew, dim, extents = cfg
    dut, config = make_id(dims, ew, dim, extents)
    cycles = id_stimulus(IterationDomainModel(dims, ew, dim, extents), seed=len(extents) * 31 + dims)
    model = IterationDomainModel(dims, ew, dim, extents)
    exp = expected(model, cycles, dims, ew, [e - 2 for e in extents])
    assert any(e["finished"] for e in exp), "stimulus does not finish the domain"
    res = run_vectors(dut, cycles, exp, config=config)
    assert res.passed, res


@requires_xrun
@pytest.mark.parametrize("cfg,rv", [(CONFIGS[3], False), (CONFIGS[4], True), (CONFIGS[6], False)],
                         ids=lambda x: _cfg_id(x) if isinstance(x, tuple) else f"rv{int(x)}")
def test_iteration_domain_rtl_flush_clk_en_reset(cfg, rv):
    """Mid-run flush (also on a stepping cycle), flush after finished, clk_en
    low for random cycles (with and without step), an async reset pulse; the
    rv flag of gen_bitstream must not change the loop nest."""
    dims, ew, dim, extents = cfg
    dut, config = make_id(dims, ew, dim, extents, rv=rv)
    cycles = id_stimulus(IterationDomainModel(dims, ew, dim, extents), seed=7 + dims, ctrl=True)
    model = IterationDomainModel(dims, ew, dim, extents)
    exp = expected(model, cycles, dims, ew, [e - 2 for e in extents])
    assert sum(1 for a, b in zip(exp, exp[1:]) if b["finished"] and not a["finished"]) >= 2
    res = run_vectors(dut, cycles, exp, config=config)
    assert res.passed, res


def test_iteration_domain_accepted_extents_fit_iterators():
    """Regression: gen_bitstream accepted extent 2**extent_width + 1, whose last
    index wraps to 0 on the extent_width-bit iterators port."""
    ew = 3
    dut = IterationDomain(dimensionality=1, extent_width=ew)
    dut.gen_hardware()
    extent = (1 << ew) + 1
    try:
        dut.gen_bitstream(1, [extent])
    except ValueError:
        return          # rejecting the extent would also resolve it
    iter_width = dut.ports.iterators.width
    assert extent - 1 < (1 << iter_width), \
        f"extent {extent} accepted but index {extent - 1} does not fit the {iter_width}-bit iterators port"
