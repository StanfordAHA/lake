"""Functional tests for the trimmed RV comparators (Spec / build_spec_rv /
build_pond_rv rv_comparator_trim; RVComparisonNetwork one_sided /
scalar_width / barrier_bit / max_level; LFCompBlock one_sided_op /
scalar_width / barrier_bit).

Function (same as the full network on every constraint the trimmed one
accepts):
  * one-sided block: its direction's op is built in (reader-gated LT,
    writer-gated GT); LT(s) = GT(-s) on the integers, so a constraint written
    with the other op is programmed with the scalar negated;
  * scalar_width: the scalar is a signed scalar_width-bit register; a scalar
    outside it raises at bitstream time;
  * barrier_bit: a barrier constraint (LT scalar >= 16383, the converter's
    barrier) sets the block's barrier bit instead: comparison 0 unless either
    port is finished, whatever the levels;
  * max_level L: each port's level select covers levels 0..L-1 (the wrap
    fixup reads level sel + 1 <= L); a deeper non-barrier constraint raises.
The reference models are the full block's / network's (test_lf_comp_block,
test_rv_comparison_network).
"""
import random

import pytest

from lake.modules.lf_comp_block import LFCompBlock
from lake.modules.rv_comparison_network import (RVComparisonNetwork, RV_BARRIER_SCALAR,
                                                RV_CMP_TRIM_DEFAULT, resolve_rv_comparator_trim)
from lake.utils.spec_enum import LFComparisonOperator as Op
from rtl_harness import pack_config, requires_xrun, run_vectors
from test_lf_comp_block import lfc_model, stimulus
from test_rv_comparison_network import StubSG, net_model, random_states, state_inputs

LT, GT = Op.LT.value, Op.GT.value
_uid = [0]


def make_block(in_width=11, out_width=11, **trim):
    _uid[0] += 1
    blk = LFCompBlock(name=f"lfct_{_uid[0]}", in_width=in_width, out_width=out_width, **trim)
    blk.gen_hardware()
    return blk


def make_net(w_dims, r_dims, width=11, **trim):
    _uid[0] += 1
    net = RVComparisonNetwork(name=f"rvcnt_{_uid[0]}",
                              writes=[StubSG(d, width) for d in w_dims],
                              reads=[StubSG(d, width) for d in r_dims], **trim)
    net.gen_hardware()
    return net


# ---------------------------------------------------------------- option parsing
def test_resolve_trim():
    assert resolve_rv_comparator_trim(None) == {}
    assert resolve_rv_comparator_trim(False) == {}
    assert resolve_rv_comparator_trim(True) == RV_CMP_TRIM_DEFAULT
    assert resolve_rv_comparator_trim({"scalar_width": 8}) == {"scalar_width": 8}
    with pytest.raises(ValueError, match="unknown keys"):
        resolve_rv_comparator_trim({"scalar_bits": 8})
    with pytest.raises(ValueError):
        resolve_rv_comparator_trim("yes")


def test_block_option_checks():
    with pytest.raises(ValueError, match="LT or GT"):
        LFCompBlock(name="x", in_width=11, out_width=11, one_sided_op=Op.EQ)
    with pytest.raises(ValueError, match="scalar_width"):
        LFCompBlock(name="x", in_width=11, out_width=11, scalar_width=17)


# ---------------------------------------------------------------- block bitstreams
def test_block_bitstream_range_and_barrier():
    blk = make_block(one_sided_op=Op.GT, scalar_width=8, barrier_bit=True)
    blk.gen_bitstream(GT, 127)
    blk.gen_bitstream(GT, -128)
    blk.gen_bitstream(LT, 127)          # programmed as GT -127
    with pytest.raises(ValueError, match="does not fit"):
        blk.gen_bitstream(GT, 128)
    with pytest.raises(ValueError, match="does not fit"):
        blk.gen_bitstream(LT, -128)     # GT +128
    blk.gen_bitstream(LT, 0, barrier=True)
    plain = make_block(scalar_width=8)
    with pytest.raises(ValueError, match="barrier_bit"):
        plain.gen_bitstream(LT, 0, barrier=True)
    full = make_block()
    with pytest.raises(ValueError, match="barrier_bit"):
        full.gen_bitstream(LT, 0, barrier=True)


def test_one_sided_negates_other_op():
    a = make_block(one_sided_op=Op.GT, scalar_width=8)
    b = make_block(one_sided_op=Op.GT, scalar_width=8)
    assert pack_config(a.gen_bitstream(LT, 5)) == pack_config(b.gen_bitstream(GT, -5))
    assert pack_config(a.gen_bitstream(LT, -7)) == pack_config(b.gen_bitstream(GT, 7))


# ---------------------------------------------------------------- block RTL
BLOCK_CASES = [
    # (trim kwargs, in_width, out_width, [(op, scalar), ...])
    (dict(one_sided_op=Op.LT, scalar_width=8), 11, 11, [(LT, -127), (LT, 36), (GT, 8), (GT, -1)]),
    (dict(one_sided_op=Op.GT, scalar_width=8, barrier_bit=True), 11, 12, [(GT, 8), (GT, -128), (LT, -63), (LT, 0)]),
    (dict(scalar_width=8), 12, 11, [(LT, 127), (GT, -128), (LT, -1)]),
    (dict(one_sided_op=Op.LT), 16, 16, [(LT, 1000), (GT, -1000), (LT, -32767)]),
    (dict(barrier_bit=True), 11, 11, [(LT, -5), (GT, 3)]),
]


@requires_xrun
@pytest.mark.parametrize("ci", range(len(BLOCK_CASES)))
def test_trimmed_block_rtl(ci):
    trim, iw, ow, programs = BLOCK_CASES[ci]
    blk = make_block(iw, ow, **trim)
    for k, (op, scalar) in enumerate(programs):
        cfg = pack_config(blk.gen_bitstream(comparator=op, scalar=scalar))
        cycles = stimulus(op, scalar, 150, seed=10 * ci + k, in_width=iw, out_width=ow)
        inputs = [{"leader_count": l_, "follower_count": f_, "leader_finished": lf, "follower_finished": ff}
                  for l_, f_, lf, ff in cycles]
        expect = [{"comparison": lfc_model(op, scalar, l_, f_, lf, ff)} for l_, f_, lf, ff in cycles]
        res = run_vectors(blk, inputs, expect, config=cfg)
        assert res.passed, (op, scalar, res)


@requires_xrun
@pytest.mark.parametrize("one_sided", [Op.LT, Op.GT, None])
def test_barrier_bit_rtl(one_sided):
    blk = make_block(11, 11, one_sided_op=one_sided, scalar_width=8, barrier_bit=True)
    cfg = pack_config(blk.gen_bitstream(comparator=LT, scalar=0, barrier=True))
    rng = random.Random(7)
    cycles = [(rng.randrange(2048), rng.randrange(2048), int(rng.random() < 0.3), int(rng.random() < 0.3))
              for _ in range(200)] + [(0, 2047, 0, 0), (2047, 0, 0, 0), (5, 5, 0, 0)]
    inputs = [{"leader_count": l_, "follower_count": f_, "leader_finished": lf, "follower_finished": ff}
              for l_, f_, lf, ff in cycles]
    expect = [{"comparison": int(lf or ff)} for _, _, lf, ff in cycles]
    res = run_vectors(blk, inputs, expect, config=cfg)
    assert res.passed, res


# ---------------------------------------------------------------- network
SCALARS8 = [0, 1, -1, 2, -2, 3, -3, 5, -5, 8, 36, -36, 63, -63, 100, -100, 127, -127]


def trim_constraints(rng, W, R, dims, max_level, barrier_bit=True, p_cfg=0.75, p_barrier=0.3):
    """Random constraints the trimmed network accepts: non-barrier levels below
    max_level, scalars in 8 bits (either op), barriers at any level with the
    barrier bit (below max_level without it: then they use the level select)."""
    cons = []
    for p1 in range(W + R):
        others = range(W, W + R) if p1 < W else range(W)
        for p2 in others:
            if rng.random() > p_cfg:
                continue
            d1, d2 = dims[p1], dims[p2]
            if rng.random() < p_barrier:
                t1, t2 = (d1, d2) if barrier_bit else (min(d1, max_level), min(d2, max_level))
                cons.append((p1, rng.randrange(t1), p2, rng.randrange(t2), LT, RV_BARRIER_SCALAR))
                continue
            op = (GT if p1 < W else LT) if rng.random() < 0.7 else (LT if p1 < W else GT)
            cons.append((p1, rng.randrange(min(d1, max_level)), p2, rng.randrange(min(d2, max_level)),
                         op, rng.choice(SCALARS8)))
    return cons


def trim_case(seed, w_dims, r_dims, max_level=3, width=11, n=300, **extra):
    rng = random.Random(seed)
    dims = list(w_dims) + list(r_dims)
    trim = dict(one_sided=True, scalar_width=8, barrier_bit=True, max_level=max_level)
    trim.update(extra)
    net = make_net(w_dims, r_dims, width, **trim)
    W, R = len(w_dims), len(r_dims)
    cons = trim_constraints(rng, W, R, dims, max_level, trim["barrier_bit"])
    states = random_states(rng, dims, n)
    cfg = pack_config(net.gen_bitstream(constraints=cons))
    inputs = [state_inputs(W, R, s, width) for s in states]
    # the full network's model: a barrier (leader + 16383 < follower) never
    # holds on these small counters, so it is "wait for finished" there too
    expect = [net_model(W, R, cons, s) for s in states]
    return net, inputs, expect, cfg


@requires_xrun
@pytest.mark.parametrize("seed", [1, 2, 3])
def test_trimmed_net_rtl_mem_shape(seed):
    """The thesis RV MemTile shape: 2 writers, 2 readers, 6 dims, 11-bit IDs."""
    net, inputs, expect, cfg = trim_case(seed, (6, 6), (6, 6))
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


@requires_xrun
def test_trimmed_net_rtl_pond_shape():
    """The RV pond shape (data writer + clear writer, 2 readers), 4 dims, 12-bit IDs."""
    net, inputs, expect, cfg = trim_case(11, (4, 4), (4, 4), width=12)
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


@requires_xrun
@pytest.mark.parametrize("max_level,w_dims,r_dims", [(3, (6, 3), (2, 6)), (1, (3,), (3, 3)), (2, (4, 2), (6,))])
def test_trimmed_net_rtl_mixed_dims(max_level, w_dims, r_dims):
    """Ports at, below and above max_level in one network (untrimmed and
    trimmed selects side by side; max_level 1 = no level mux at all)."""
    net, inputs, expect, cfg = trim_case(20 + max_level, w_dims, r_dims, max_level=max_level)
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


@requires_xrun
def test_level_trim_only_rtl():
    """max_level alone: 16-bit two-sided blocks, no barrier bit (barriers as 16383)."""
    net, inputs, expect, cfg = trim_case(31, (6, 6), (6, 6), one_sided=False, scalar_width=16, barrier_bit=False)
    res = run_vectors(net, inputs, expect, config=cfg)
    assert res.passed, res


def test_trimmed_net_structure():
    net = make_net((6, 6), (6, 6), **RV_CMP_TRIM_DEFAULT)
    full = make_net((6, 6), (6, 6))
    assert len(net.lfcs) == len(full.lfcs) == 8
    # per block: enable 1 + scalar 8 + barrier 1 + two 2-bit selects (was 1 + 2 + 16 + 3 + 3)
    assert net.get_config_size() == 8 * 14
    assert full.get_config_size() == 8 * 25
    assert all(v == (3, 3) for v in net.levels_map.values())


def test_trimmed_net_bitstream_checks():
    net = make_net((6, 6), (6, 6), **RV_CMP_TRIM_DEFAULT)
    net.gen_bitstream([(2, 2, 0, 2, LT, -1)])                    # reader 0 waits on writer 0, levels 2/2
    net.gen_bitstream([(2, 5, 1, 4, LT, RV_BARRIER_SCALAR)])     # barrier: levels are free
    with pytest.raises(ValueError, match="level select"):
        net.gen_bitstream([(2, 3, 0, 3, LT, -1)])
    with pytest.raises(ValueError, match="level select"):
        net.gen_bitstream([(0, 1, 2, 3, GT, 8)])
    with pytest.raises(ValueError, match="does not fit"):
        net.gen_bitstream([(2, 0, 0, 0, LT, -200)])
    lvl_only = make_net((6, 6), (6, 6), max_level=3)
    lvl_only.gen_bitstream([(2, 0, 0, 0, LT, -200)])             # 16-bit scalar still fits
    with pytest.raises(ValueError, match="level select"):
        lvl_only.gen_bitstream([(2, 4, 0, 4, LT, RV_BARRIER_SCALAR)])   # no barrier bit: levels count


# ---------------------------------------------------------------- spec builders
def test_spec_builders_take_the_trim():
    from lake.spec.spec_memory_controller import build_spec_rv, build_cgra_pond
    mem = build_spec_rv(storage_capacity=4096, data_width=16, vec_width=4, in_ports=2, out_ports=2,
                        vec_capacity=2, add_filter_path=False, physical=False, comply_17=False,
                        rv_comparator_trim=True)
    assert mem.rv_comparator_trim == RV_CMP_TRIM_DEFAULT
    pond = build_cgra_pond({"rv_comparator_trim": {"max_level": 3}}, rv=True, comply_17=False)
    assert pond.rv_comparator_trim == {"max_level": 3}
    with pytest.raises(ValueError, match="unsupported pond spec keys"):
        build_cgra_pond({"rv_comparator_trim": True}, rv=False)


def test_wide_row_falls_back_to_row_level_raw():
    """A line buffer whose rows are too wide for an 8-bit column-level RAW scalar
    (-(W - 1)) gets the row-level RAW on a trimmed spec (one row of latency); the
    full network keeps the column-level one."""
    import copy
    from lake.spec.spec_memory_controller import build_spec_rv
    from test_convert_rv_capacity import line_buffer

    def raw(trim, W):
        sp = build_spec_rv(storage_capacity=8192, data_width=16, vec_width=1, dual_port=True,
                           in_ports=1, out_ports=1, physical=False, rv_comparator_trim=trim)
        sp.generate_hardware()
        app = sp.rewrite_app_json(copy.deepcopy(line_buffer(W, 20, W, 2)))
        conf = sp.convert_app_json_to_config(app)
        (c,) = [c for c in conf["constraints"] if c[4] == LT]
        sp.rv_comparison_network.gen_bitstream(sp._network_constraints(conf["constraints"]))
        return c[1], c[3], c[5]
    assert raw(None, 200) == (0, 0, -199)
    assert raw(True, 200) == (1, 1, -1)
    assert raw(True, 100) == raw(None, 100) == (0, 0, -99)


@requires_xrun
@pytest.mark.parametrize("stress", [False, True])
def test_wide_row_line_buffer_rtl(stress):
    """The wide line buffer above on the trimmed spec (row-level RAW from the
    converter), clockwork's RV format through Spec.gen_bitstream, whole-spec RTL
    with writer and reader stalls: reader iteration i returns the writer's
    iteration i - 2 rows."""
    from lake.spec.spec_memory_controller import build_spec_rv
    from spec_rtl import SpecSim
    from test_convert_rv_capacity import line_buffer
    W, H, lag = 200, 6, 2
    sim = SpecSim(build_spec_rv(storage_capacity=8192, data_width=16, vec_width=1, dual_port=True,
                                in_ports=1, out_ports=1, physical=False, rv_comparator_trim=True))
    bs = sim.spec.gen_bitstream(line_buffer(W, H, W, lag))
    n = W * H
    outs, done, cycles = sim.run(bs, {"port_w0": n, "port_r0": n}, stress=stress, seed=5,
                                 name=f"wide_row_{int(stress)}", writer_stall=(20, 400), reader_stall=(500, 900))
    assert done, f"timeout after {cycles} cycles, {len(outs.get('port_r0', []))} outputs"
    got = outs["port_r0"]
    assert len(got) == n
    bad = [(i, g, i - lag * W) for i, g in enumerate(got) if i >= lag * W and g != i - lag * W]
    assert not bad, bad[:5]
