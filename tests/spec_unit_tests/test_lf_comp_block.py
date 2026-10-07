"""Functional tests for lake.modules.lf_comp_block.LFCompBlock (one
leader/follower comparison of the RV comparison network; the network builds one
block per (gated port, other-direction port) pair and a port's SG step is the
AND of its blocks' outputs).

Function:
  * leader = the gated port's selected counter (unsigned in_width bits),
    follower = the other port's (unsigned out_width bits), scalar = signed
    16-bit config value; the comparison is on the mathematical integers:
      LT (RAW, a reader may step):  comparison = leader + scalar < follower
      GT (WAR, a writer may step):  comparison = leader < follower + scalar
  * the output is forced to 1 when the block is not enabled (unconfigured pairs
    never block) or when leader_finished or follower_finished is 1;
  * EQ / LTE exist in LFComparisonOperator but are commented out in the RTL: an
    enabled block with those ops never lets the port step (output 0 unless a
    finished flag is set).
Configured only through gen_bitstream(comparator, scalar), packed with
rtl_harness.pack_config.
"""
import random

import pytest

from lake.modules.lf_comp_block import LFCompBlock
from lake.utils.spec_enum import LFComparisonOperator as Op
from rtl_harness import pack_config, requires_xrun, run_vectors

LT, GT, EQ, LTE = Op.LT.value, Op.GT.value, Op.EQ.value, Op.LTE.value
_uid = [0]


def make_block(in_width=16, out_width=16, tag="lfc"):
    _uid[0] += 1
    blk = LFCompBlock(name=f"{tag}_{_uid[0]}", in_width=in_width, out_width=out_width)
    blk.gen_hardware()
    return blk


def lfc_model(op, scalar, leader, follower, leader_finished, follower_finished, enabled=True):
    """Expected comparison output (scalar is a Python int in [-2**15, 2**15))."""
    if not enabled or leader_finished or follower_finished:
        return 1
    if op == LT:
        return int(leader + scalar < follower)
    if op == GT:
        return int(leader < follower + scalar)
    return 0


def _fits(op, scalar, leader, follower):
    """The operand the scalar is added to stays <= 2**16 - 1 (see the
    overflow xfail test; only reachable with 16-bit counters above 2**15)."""
    return (leader + scalar if op == LT else follower + scalar) <= 0xFFFF


def stimulus(op, scalar, n, seed, in_width=16, out_width=16, fin_rate=0.15):
    """Cycles of (leader, follower, leader_finished, follower_finished)."""
    rng = random.Random(seed)
    lmax, fmax = (1 << in_width) - 1, (1 << out_width) - 1
    cycles = []
    # directed: the exact threshold and one either side, without finished flags
    base = [(100, 100 + scalar), (100, 100 + scalar + 1), (100, 100 + scalar - 1),
            (100 + scalar, 100), (100 + scalar + 1, 100), (100 + scalar - 1, 100),
            (0, 0), (lmax, fmax), (0, fmax), (lmax, 0)]
    for l_, f_ in base:
        l_, f_ = min(max(l_, 0), lmax), min(max(f_, 0), fmax)
        if _fits(op, scalar, l_, f_):
            cycles.append((l_, f_, 0, 0))
    while len(cycles) < n:
        kind = rng.random()
        if kind < 0.45:     # around the threshold
            d = rng.randint(-2, 2)
            if op == LT:
                l_ = rng.randint(0, lmax)
                f_ = l_ + scalar + d
            else:
                f_ = rng.randint(0, fmax)
                l_ = f_ + scalar + d
        elif kind < 0.65:   # boundary values
            l_ = rng.choice([0, 1, lmax - 1, lmax])
            f_ = rng.choice([0, 1, fmax - 1, fmax])
        else:
            l_, f_ = rng.randint(0, lmax), rng.randint(0, fmax)
        if not (0 <= l_ <= lmax and 0 <= f_ <= fmax) or not _fits(op, scalar, l_, f_):
            continue
        lf = int(rng.random() < fin_rate)
        ff = int(rng.random() < fin_rate)
        cycles.append((l_, f_, lf, ff))
    return cycles


def lfc_case(op, scalar, n=200, seed=0, in_width=16, out_width=16, enabled=True):
    blk = make_block(in_width, out_width)
    cfg = pack_config(blk.gen_bitstream(comparator=op, scalar=scalar)) if enabled else 0
    cycles = stimulus(op, scalar, n, seed, in_width, out_width)
    inputs = [{"leader_count": l_, "follower_count": f_, "leader_finished": lf, "follower_finished": ff}
              for l_, f_, lf, ff in cycles]
    expect = [{"comparison": lfc_model(op, scalar, l_, f_, lf, ff, enabled)} for l_, f_, lf, ff in cycles]
    return blk, inputs, expect, cfg


def test_lfc_bitstream_encoding():
    """gen_bitstream -> pack_config puts enable=1, the op, and the scalar as
    16-bit two's complement into the block's config fields."""
    blk = make_block()
    cmap = blk.get_cfg_map()
    for op, scalar in [(LT, -1), (GT, 5), (LT, -32768), (GT, 32767)]:
        cfg = pack_config(blk.gen_bitstream(comparator=op, scalar=scalar))

        def field(name):
            hi, lo = cmap[name]
            return (cfg >> lo) & ((1 << (hi - lo + 1)) - 1)
        assert field("enable_comparison") == 1
        assert field("comparison_op") == op
        assert field("comparison_scalar") == scalar & 0xFFFF


def test_lfc_model_semantics():
    """The reference model on the documented RAW / WAR examples."""
    # RAW: a reader at 5 with scalar 0 may step only once the writer is past 5
    assert lfc_model(LT, 0, 5, 5, 0, 0) == 0
    assert lfc_model(LT, 0, 5, 6, 0, 0) == 1
    # negative RAW scalar lets the reader run ahead of the writer by |scalar|
    assert lfc_model(LT, -3, 7, 5, 0, 0) == 1
    assert lfc_model(LT, -3, 8, 5, 0, 0) == 0
    # WAR: a writer may be at most scalar - 1 ahead of the reader
    assert lfc_model(GT, 4, 13, 10, 0, 0) == 1
    assert lfc_model(GT, 4, 14, 10, 0, 0) == 0
    # finished / disabled never block
    assert lfc_model(LT, 0, 5, 5, 1, 0) == 1
    assert lfc_model(GT, 0, 5, 0, 0, 1) == 1
    assert lfc_model(LT, 0, 5, 5, 0, 0, enabled=False) == 1


SCALARS = [0, 1, -1, 100, -100, 32767, -32768]


@requires_xrun
@pytest.mark.parametrize("op", [LT, GT], ids=["LT", "GT"])
@pytest.mark.parametrize("scalar", SCALARS)
def test_lfc_rtl(op, scalar):
    blk, inputs, expect, cfg = lfc_case(op, scalar, seed=1000 * op + scalar)
    res = run_vectors(blk, inputs, expect, config=cfg)
    assert res.passed, res


@requires_xrun
@pytest.mark.parametrize("in_width,out_width,op,scalar", [(11, 16, LT, -3), (16, 11, GT, 5), (11, 11, LT, 2)])
def test_lfc_rtl_mixed_widths(in_width, out_width, op, scalar):
    """The network builds blocks with the two ports' iterator widths."""
    blk, inputs, expect, cfg = lfc_case(op, scalar, seed=7, in_width=in_width, out_width=out_width)
    res = run_vectors(blk, inputs, expect, config=cfg)
    assert res.passed, res


@requires_xrun
def test_lfc_rtl_unconfigured_never_blocks():
    """config 0 (enable_comparison = 0): the comparison is 1 whatever the
    counters, so pairs without a constraint never block a port."""
    blk, inputs, expect, cfg = lfc_case(LT, 0, seed=3, enabled=False)
    assert cfg == 0 and all(e["comparison"] == 1 for e in expect)
    res = run_vectors(blk, inputs, expect, config=cfg)
    assert res.passed, res


@requires_xrun
@pytest.mark.parametrize("op", [EQ, LTE], ids=["EQ", "LTE"])
def test_lfc_rtl_unimplemented_ops_block(op):
    """EQ / LTE are commented out in calculate_comparison: an enabled block
    with them outputs 0 unless a finished flag is set (clockwork/spec never
    emit them; this guards against them silently passing)."""
    blk, inputs, expect, cfg = lfc_case(op, 0, seed=11)
    res = run_vectors(blk, inputs, expect, config=cfg)
    assert res.passed, res


def overflow_case():
    """LT, scalar 32767, 16-bit counters: leader + scalar = 65535 is the largest
    sum that fits; 32769 + 32767 = 65536 and above wrap in the RTL."""
    blk = make_block(16, 16)
    scalar = 32767
    cfg = pack_config(blk.gen_bitstream(comparator=LT, scalar=scalar))
    pts = [(40000, 1000), (0xFFFF, 0), (32769, 5), (32768, 0xFFFF), (100, 200)]
    inputs = [{"leader_count": l_, "follower_count": f_} for l_, f_ in pts]
    expect = [{"comparison": lfc_model(LT, scalar, l_, f_, 0, 0)} for l_, f_ in pts]
    return blk, inputs, expect, cfg


@requires_xrun
@pytest.mark.xfail(strict=True, reason="l_plus_s / f_plus_s are max(width,16)+1 = 17 bits signed: with 16-bit "
                   "counters, leader + scalar > 65535 wraps negative, e.g. LT scalar=32767 leader=40000 "
                   "follower=1000 -> RTL 1 (reader may step), function 0")
def test_lfc_rtl_sum_overflow(tmp_path):
    blk, inputs, expect, cfg = overflow_case()
    res = run_vectors(blk, inputs, expect, config=cfg, workdir=str(tmp_path))
    assert res.passed, res


@pytest.mark.xfail(strict=True, raises=AttributeError,
                   reason="hardened-op constructor path (comparisonOp=..., hard_scalar=...) never defines "
                   "_scalar_reg_signed, so gen_hardware raises AttributeError (path unused by the network)")
def test_lfc_hardened_op_builds():
    blk = LFCompBlock(name="lfc_hard", in_width=16, out_width=16, comparisonOp=Op.LT, hard_scalar=3)
    blk.gen_hardware()
