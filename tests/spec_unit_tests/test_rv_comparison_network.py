"""Functional tests for lake.modules.rv_comparison_network.RVComparisonNetwork
(the ready-valid dependency network of a spec: one LFCompBlock per (gated port,
opposite-direction port) pair; a port's RV schedule generator steps when all of
its ``comparisons`` bits are 1).

Function:
  * ports: writers are indices 0..W-1, readers W..W+R-1; each port supplies its
    ID iterators, extents (true extents: the RV SG adds back the ID's -2) and
    finished; write_i_comparisons has bit j for reader j, read_j_comparisons
    has bit i for writer i;
  * gen_bitstream(constraints), constraint (p1, p1_level, p2, p2_level, op,
    scalar) = "p1 (gated) may step when its iterator at p1_level compared
    against p2's iterator at p2_level passes" (LFCompBlock: LT leader + scalar
    < follower, GT leader < follower + scalar, forced 1 when either port is
    finished);
  * one-level wrap fixup: if p1's iterator at p1_level + 1 differs from p2's at
    p2_level + 1 (a missing level above counts as 0), an extent is added to the
    WRITER's counter:
      writer-gated block: leader = writer iter + the writer's extent at p1_level
      reader-gated block: follower = writer iter + the READER's extent at p1_level
    (this assumes the writer is at most one outer iteration ahead and never
    behind; a reader that runs into the next outer iteration ahead of the
    writer, possible with a negative RAW scalar, gets the extent added to the
    wrong side -- the known single-row limitation behind LAKE_RV_ROW_RAW);
  * pairs without a constraint never block (enable_comparison = 0 -> 1).
The network only reads the SGs' iterator widths and dimensionality, so the
tests hand it stub SGs.
"""
import random
from types import SimpleNamespace as NS

import pytest

from lake.modules.rv_comparison_network import RVComparisonNetwork
from lake.utils.spec_enum import LFComparisonOperator as Op
from rtl_harness import pack, pack_config, requires_xrun, run_vectors
from test_lf_comp_block import lfc_model

LT, GT = Op.LT.value, Op.GT.value
_uid = [0]


class StubSG:
    def __init__(self, dims, width):
        self.dims, self.width = dims, width

    def get_dimensionality(self):
        return self.dims

    def get_iterator_intf(self):
        return {k: NS(width=self.width) for k in ("iterators", "extents", "finished", "comparisons")}


def make_net(w_dims, r_dims, width=16):
    _uid[0] += 1
    net = RVComparisonNetwork(name=f"rvcn_t{_uid[0]}",
                              writes=[StubSG(d, width) for d in w_dims],
                              reads=[StubSG(d, width) for d in r_dims])
    net.gen_hardware()
    return net


def block_model(W, cons, state, p1, p2):
    """Output of the block gating p1 against p2. state[p] = (iters, extents, finished)."""
    if (p1, p2) not in cons:
        return 1
    l1, l2, op, scalar = cons[(p1, p2)]
    it1, ex1, f1 = state[p1]
    it2, ex2, f2 = state[p2]
    up1 = it1[l1 + 1] if l1 + 1 < len(it1) else 0
    up2 = it2[l2 + 1] if l2 + 1 < len(it2) else 0
    fix = ex1[l1] if up1 != up2 else 0
    if p1 < W:      # writer-gated: the writer (leader) gets its own extent
        leader, follower = it1[l1] + fix, it2[l2]
    else:           # reader-gated: the writer (follower) gets the reader's extent
        leader, follower = it1[l1], it2[l2] + fix
    return lfc_model(op, scalar, leader, follower, f1, f2)


def net_model(W, R, constraints, state):
    cons = {(c[0], c[2]): (c[1], c[3], c[4], c[5]) for c in constraints}
    out = {}
    for i in range(W):
        out[f"write_{i}_comparisons"] = pack([block_model(W, cons, state, i, W + j) for j in range(R)], 1)
    for j in range(R):
        out[f"read_{j}_comparisons"] = pack([block_model(W, cons, state, W + j, i) for i in range(W)], 1)
    return out


def state_inputs(W, R, state, width):
    c = {}
    for p, (its, exs, fin) in enumerate(state):
        nm = f"write_{p}" if p < W else f"read_{p - W}"
        c[f"{nm}_iterators"] = pack(its, width)
        c[f"{nm}_extents"] = pack(exs, width)
        c[f"{nm}_finished"] = fin
    return c


def net_case(w_dims, r_dims, constraints, states, width=16):
    net = make_net(w_dims, r_dims, width)
    W, R = len(w_dims), len(r_dims)
    cfg = pack_config(net.gen_bitstream(constraints=constraints))
    inputs = [state_inputs(W, R, s, width) for s in states]
    expect = [net_model(W, R, constraints, s) for s in states]
    return net, inputs, expect, cfg


# ---------------------------------------------------------------- random states
SCALARS = [0, 1, -1, 2, -2, 3, -3, 5, -5, 8, 36, -36, 1000, -1000, 16383, 32767, -32768]


def random_constraints(rng, W, R, dims, max_level=None, p_cfg=0.7):
    cons = []
    for p1 in range(W + R):
        others = range(W, W + R) if p1 < W else range(W)
        for p2 in others:
            if rng.random() > p_cfg:
                continue
            d1, d2 = dims[p1], dims[p2]
            top1 = d1 - 1 if max_level is None else min(max_level, d1 - 1)
            top2 = d2 - 1 if max_level is None else min(max_level, d2 - 1)
            op = (GT if p1 < W else LT) if rng.random() < 0.8 else (LT if p1 < W else GT)
            cons.append((p1, rng.randint(0, top1), p2, rng.randint(0, top2), op, rng.choice(SCALARS)))
    return cons


def random_states(rng, dims, n, max_ext=20):
    """Per-port iterators correlated across ports (shared value per level with
    small offsets) so outer levels are often equal / off by one and inner
    counters sit near the comparison thresholds; extents fixed per port."""
    P = len(dims)
    exts = [[rng.randint(3, max_ext) for _ in range(d)] for d in dims]
    states = []
    for _ in range(n):
        base = [rng.randrange(max_ext) for _ in range(max(dims))]
        st = []
        for p in range(P):
            its = []
            for lvl in range(dims[p]):
                r = rng.random()
                v = base[lvl] if r < 0.55 else base[lvl] + rng.choice([-1, 1]) if r < 0.85 else rng.randrange(max_ext)
                its.append(min(max(v, 0), exts[p][lvl] - 1))
            st.append((its, exts[p], int(rng.random() < 0.1)))
        states.append(st)
    return states


def random_case(seed, w_dims=(3, 3), r_dims=(3, 3), width=16, max_level=None, n=300):
    rng = random.Random(seed)
    dims = list(w_dims) + list(r_dims)
    cons = random_constraints(rng, len(w_dims), len(r_dims), dims, max_level)
    states = random_states(rng, dims, n)
    return net_case(w_dims, r_dims, cons, states, width)


@requires_xrun
@pytest.mark.parametrize("seed", [1, 2, 3, 4])
def test_rvcn_rtl_random_2w2r(seed):
    net, inputs, expect, cfg = random_case(seed)
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


@requires_xrun
def test_rvcn_rtl_random_1w3r_width11():
    net, inputs, expect, cfg = random_case(10, w_dims=(3,), r_dims=(3, 3, 3), width=11)
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


@requires_xrun
def test_rvcn_rtl_random_pond_dims4_levels_below_top():
    """build_pond_rv(dims=4) geometry (1 writer + flush writer, 2 readers,
    11-bit iterators) with constraints on levels 0..2 (what the pond programs
    use); the top level of power-of-2 dims is covered by the xfail test."""
    net, inputs, expect, cfg = random_case(20, w_dims=(4, 4), r_dims=(4, 4), width=11, max_level=2)
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


@requires_xrun
def test_rvcn_rtl_unconfigured_never_blocks():
    """No constraints: every comparisons bit is 1 whatever the iterators."""
    rng = random.Random(5)
    states = random_states(rng, [3, 3, 3, 3], 100)
    net, inputs, expect, cfg = net_case((3, 3), (3, 3), [], states)
    assert cfg == 0 and all(v == 3 for e in expect for v in e.values())
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


def only_one_pair_case():
    """One constraint (reader 1 gated on writer 0); the iterators would fail
    every block if all were enabled; only read_1_comparisons bit 0 may be 0."""
    W, R = 2, 2
    cons = [(W + 1, 0, 0, 0, LT, 0)]
    blocked = ([5, 0, 0], [8, 8, 8], 0)          # everyone at column 5, same rows
    states = [[blocked] * 4, [([5, 0, 0], [8, 8, 8], 0)] * 3 + [([4, 0, 0], [8, 8, 8], 0)]]
    net, inputs, expect, cfg = net_case((3, 3), (3, 3), cons, states)
    assert expect[0] == {"write_0_comparisons": 3, "write_1_comparisons": 3,
                         "read_0_comparisons": 3, "read_1_comparisons": 2}
    assert expect[1]["read_1_comparisons"] == 3    # reader 1 at 4 < writer 0 at 5
    return net, inputs, expect, cfg


@requires_xrun
def test_rvcn_rtl_constraint_lands_on_its_block():
    net, inputs, expect, cfg = only_one_pair_case()
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


def fixup_directed_case():
    """Hand-computed wrap-fixup cycles (independent of the model), 1 writer
    (port 0, level-0 extent 10) and 1 reader (port 1, level-0 extent 7), both
    at level 0 with the fixup on level 1:
      RAW (1, 0, 0, 0, LT, 5): reader col + 5 < writer col (+ READER extent 7 if rows differ)
      WAR (0, 0, 1, 0, GT, 6): writer col (+ WRITER extent 10 if rows differ) < reader col + 6
    Returns (net, inputs, expect, cfg); expect = (write_0_comparisons, read_0_comparisons)."""
    cons = [(1, 0, 0, 0, LT, 5), (0, 0, 1, 0, GT, 6)]

    def st(w_col, w_row, r_col, r_row, wf=0, rf=0):
        return [([w_col, w_row, 0], [10, 4, 2], wf), ([r_col, r_row, 0], [7, 4, 2], rf)]
    rows = [
        # same row: plain compare
        (st(9, 0, 3, 0), 0, 1),    # WAR 9 < 9 no; RAW 8 < 9 yes
        (st(8, 0, 3, 0), 1, 0),    # WAR 8 < 9 yes; RAW 8 < 8 no
        # writer one row ahead: writer counter + extent
        (st(2, 1, 5, 0), 0, 0),    # WAR 2+10=12 < 11 no;  RAW 10 < 2+7=9 no  (writer extent would give 12: yes)
        (st(0, 1, 5, 0), 1, 0),    # WAR 10 < 11 yes;      RAW 10 < 7 no
        (st(4, 1, 5, 0), 0, 1),    # WAR 14 < 11 no;       RAW 10 < 11 yes
        # both in row 1 again: no fixup
        (st(4, 1, 5, 1), 1, 0),    # WAR 4 < 11 yes; RAW 10 < 4 no
        # finished flags force 1 on both blocks
        (st(9, 0, 3, 0, wf=1), 1, 1),
        (st(2, 1, 5, 0, rf=1), 1, 1),
    ]
    net, inputs, expect, cfg = net_case((3,), (3,), cons, [r[0] for r in rows])
    for (s, w, r), e in zip(rows, expect):
        assert (e["write_0_comparisons"], e["read_0_comparisons"]) == (w, r), (s, e)
    return net, inputs, expect, cfg


@requires_xrun
def test_rvcn_rtl_fixup_extent_sides():
    net, inputs, expect, cfg = fixup_directed_case()
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


# ---------------------------------------------------------------- closed loop
def closed_loop_case(extents, level, s_raw, cap, seed, n=400, p_w=0.7, p_r=0.7):
    """1 writer + 1 reader traverse the same domain ``extents`` (row-major),
    dims 3 hardware. RAW (1, level, 0, level, LT, s_raw), WAR (0, level, 1,
    level, GT, cap). Each cycle a port steps iff all its comparisons are 1 and
    a random external valid/ready is high. Returns the case and the model-level
    invariant checks: with the writer at most one outer iteration ahead and the
    reader never ahead (s_raw >= 0, cap <= extent at ``level``), the fixed-up
    comparison equals the comparison of the positions linearized at ``level``."""
    rng = random.Random(seed)
    dims = 3
    ex = list(extents) + [2] * (dims - len(extents))
    cons = [(1, level, 0, level, LT, s_raw), (0, level, 1, level, GT, cap)]
    ports = [{"it": [0] * dims, "fin": 0} for _ in range(2)]

    def lin(it):    # position at `level`, linearized with the level above
        return it[level] + ex[level] * it[level + 1]

    def step(p):
        it = p["it"]
        for lvl in range(len(extents)):
            it[lvl] += 1
            if it[lvl] < ex[lvl]:
                return
            it[lvl] = 0
        p["fin"] = 1            # last iteration: the ID clears its counters and sets finished

    states, reads, writes = [], 0, 0
    for _ in range(n):
        st = [(list(p["it"]), ex, p["fin"]) for p in ports]
        states.append(st)
        out = net_model(1, 1, cons, st)
        w_ok, r_ok = out["write_0_comparisons"] == 1, out["read_0_comparisons"] == 1
        wl, rl = lin(st[0][0]), lin(st[1][0])
        if not ports[0]["fin"] and not ports[1]["fin"]:
            assert 0 <= wl - rl <= ex[level], "writer ran more than one outer iteration ahead / reader ahead"
            assert r_ok == (rl + s_raw < wl), "fixup != linear RAW comparison"
            assert w_ok == (wl < rl + cap), "fixup != linear WAR comparison"
        do_w = w_ok and not ports[0]["fin"] and rng.random() < p_w
        do_r = r_ok and not ports[1]["fin"] and rng.random() < p_r
        if do_w:
            step(ports[0])
            writes += 1
        if do_r:
            step(ports[1])
            reads += 1
    net, inputs, expect, cfg = net_case((dims,), (dims,), cons, states)
    return net, inputs, expect, cfg, reads, writes


CLOSED_LOOP = {
    # (extents, level, s_raw, cap): line buffer on columns, row fixup
    "col_level_lag0": ([6, 5], 0, 0, 6),
    "col_level_lag2_cap4": ([7, 6], 0, 2, 4),
    # row-level constraints (LAKE_RV_ROW_RAW style) with the plane fixup
    "row_level_3d": ([3, 4, 5], 1, 1, 3),
}


@pytest.mark.parametrize("name", list(CLOSED_LOOP))
def test_rvcn_model_closed_loop_linear(name):
    """Model-level: in a closed writer/reader loop both ports make progress and
    the fixup reproduces the linearized comparison (asserted per cycle)."""
    extents, level, s_raw, cap = CLOSED_LOOP[name]
    *_, reads, writes = closed_loop_case(extents, level, s_raw, cap, seed=1, n=600)
    total = 1
    for e in extents:
        total *= e
    assert writes == total and reads == total, (reads, writes)


@requires_xrun
@pytest.mark.parametrize("name", list(CLOSED_LOOP))
def test_rvcn_rtl_closed_loop(name):
    extents, level, s_raw, cap = CLOSED_LOOP[name]
    net, inputs, expect, cfg, _, _ = closed_loop_case(extents, level, s_raw, cap, seed=2)
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


# ---------------------------------------------------------------- suspected bugs
@pytest.mark.parametrize("dims", [1, 2, 4])
def test_rvcn_top_level_constraint_pow2_dims_rejected(dims):
    """The RTL computes the fixup level as sel + 1 in clog2(dims) bits: at the
    top level of power-of-2 dims it wraps to level 0 (dims 1: the one-entry mux
    ignores the select), so the extent is added whenever the level-0 iterators
    differ. E.g. dims 4, RAW (r,3,w,3,LT,0), extents 8, writer top 2 / level0
    0, reader top 2 / level0 7: function 2<2 -> 0, RTL 2<2+8 -> 1 (reader
    steps early). gen_bitstream refuses such constraints (RTL fix kept as a
    patch: session scratchpad patches/lake_rvcn_outer_level_rtlfix.patch)."""
    net = make_net((dims,), (dims,), width=11)
    top = dims - 1
    for bad in [(1, top, 0, top, LT, 0), (0, top, 1, top, GT, 2)]:
        with pytest.raises(ValueError):
            net.gen_bitstream(constraints=[bad])
    # a barrier cannot be opened by a spurious fixup: still allowed
    net.gen_bitstream(constraints=[(1, top, 0, top, LT, 16383)])
    if dims > 1:
        net.gen_bitstream(constraints=[(1, top - 1, 0, top - 1, LT, 0)])


def test_rvcn_reader_more_dims_than_writer_builds():
    """Regression: reader->writer blocks sized read_i_to_write_j_sel_in with the
    LAST writer's dimensionality (leaked from the writer loop), so a reader with
    more levels than that writer failed inline_multiplexer's sel-width assert."""
    make_net((2,), (4,))
