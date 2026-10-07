"""Functional tests for lake.spec.reg_fifo.RegFIFO (the spec register FIFO:
the skid buffer of a DYNAMIC MemoryInterfaceDecoder read path and the FIFOs in
spec Ports; not lake/modules/reg_fifo.py, which has tests/test_reg_fifo.py).

Function (depth a power of 2, standard mode):
  * a FIFO of up to ``depth`` entries of width_mult x data_width;
  * push writes data_in only while not full: a push while full is dropped,
    even when a pop happens in the same cycle (full is the registered count);
  * pop removes the head only while not empty: a pop while empty does
    nothing, and a push+pop on an empty FIFO only pushes (no pass-through);
  * data_out is the head (don't care while empty); valid = ~empty;
    empty = (count == 0), full = (count == depth),
    almost_full = (count >= depth - almost_full_diff);
  * rd_ptr_out (break_out_rd_ptr) = number of pops mod depth;
  * flush (synchronous, higher priority than clk_en) empties the FIFO;
    clk_en = 0 freezes it (push/pop ignored);
  * depth 0 is a combinational pass-through: data_out = data_in, valid = push,
    empty = ~push, full = almost_full = ~pop.
Parallel-load mode (parallel=True) is not tested: no spec code uses it.
"""
import random
from collections import deque

import pytest

from lake.spec.reg_fifo import RegFIFO
from rtl_harness import pack, requires_xrun, run_vectors

_uid = [0]


def make_fifo(data_width, width_mult, depth, afd, break_out_rd_ptr=False):
    _uid[0] += 1
    return RegFIFO(data_width, width_mult, depth, almost_full_diff=afd,
                   break_out_rd_ptr=break_out_rd_ptr, mod_name_suffix=f"_t{_uid[0]}")


class FifoModel:
    def __init__(self, depth, afd):
        self.depth, self.afd = depth, afd
        self.items = deque()
        self.rd_ptr = 0

    def outputs(self):
        n = len(self.items)
        return {"empty": int(n == 0), "full": int(n == self.depth),
                "almost_full": int(n >= self.depth - self.afd), "valid": int(n > 0),
                "data_out": self.items[0] if n else None, "rd_ptr_out": self.rd_ptr}

    def clock(self, push, pop, data, flush=0, clk_en=1):
        if flush:
            self.items.clear()
            self.rd_ptr = 0
            return
        if not clk_en:
            return
        n = len(self.items)
        write = push and n < self.depth
        read = pop and n > 0
        if read:
            self.items.popleft()
            self.rd_ptr = (self.rd_ptr + 1) % self.depth
        if write:
            self.items.append(data)


def check_invariants(depth, afd, outs):
    for o in outs:
        assert not (o["empty"] and o["full"]) or depth == 0
        assert o["valid"] == 1 - o["empty"]
        if o["full"]:
            assert o["almost_full"], "full but not almost_full"


def stimulus(depth, n, seed, width_bits, ctrl=True):
    """[(push, pop, data, flush, clk_en)]: fill past full, drain past empty,
    simultaneous push/pop at empty/mid/full, random traffic, flush and
    clk_en = 0 cycles."""
    rng = random.Random(seed)
    mask = (1 << width_bits) - 1
    cyc = []

    def d():
        return rng.getrandbits(width_bits) & mask

    cyc += [(1, 1, d(), 0, 1)]                              # push+pop on empty -> push only
    cyc += [(1, 0, d(), 0, 1) for _ in range(depth + 2)]    # fill, then 2 dropped pushes
    cyc += [(1, 1, d(), 0, 1) for _ in range(2)]            # push+pop while full -> pop only
    cyc += [(0, 1, 0, 0, 1) for _ in range(depth + 2)]      # drain, then pops on empty
    cyc += [(1, 0, d(), 0, 1), (1, 1, d(), 0, 1), (1, 1, d(), 0, 1)]  # push+pop mid
    if ctrl:
        cyc += [(1, 0, d(), 0, 0), (0, 1, 0, 0, 0)]         # clk_en low: frozen
        cyc += [(1, 0, d(), 0, 1), (1, 1, d(), 1, 1)]       # flush wins over push/pop
        cyc += [(0, 1, 0, 0, 1)]                            # pop on flushed FIFO
    while len(cyc) < n:
        mode = (len(cyc) // 40) % 3                         # bursts biased to fill / drain / balanced
        p_push, p_pop = [(0.8, 0.3), (0.3, 0.8), (0.5, 0.5)][mode]
        push = int(rng.random() < p_push)
        pop = int(rng.random() < p_pop)
        flush = int(ctrl and rng.random() < 0.02)
        clk_en = int(not ctrl or rng.random() > 0.05)
        cyc.append((push, pop, d(), flush, clk_en))
    return cyc


def fifo_case(data_width, width_mult, depth, afd, n=300, seed=0, break_out_rd_ptr=False, ctrl=True):
    dut = make_fifo(data_width, width_mult, depth, afd, break_out_rd_ptr)
    width_bits = data_width * width_mult
    cyc = stimulus(depth, n, seed, width_bits, ctrl)
    model = FifoModel(depth, afd)
    inputs, expect, outs = [], [], []
    for push, pop, data, flush, clk_en in cyc:
        o = model.outputs()
        outs.append(o)
        inp = {"push": push, "pop": pop, "data_in": data}
        if ctrl:
            inp.update(flush=flush, clk_en=clk_en)
        inputs.append(inp)
        e = {k: o[k] for k in ("empty", "full", "almost_full", "valid", "data_out")}
        if break_out_rd_ptr:
            e["rd_ptr_out"] = o["rd_ptr_out"]
        expect.append(e)
        model.clock(push, pop, data, flush, clk_en)
    check_invariants(depth, afd, outs)
    return dut, inputs, expect


CONFIGS = [  # (data_width, width_mult, depth, almost_full_diff)
    (16, 1, 2, 1),      # MID skid buffer, read delay 0
    (16, 1, 4, 2),      # MID skid buffer, read delay 1
    (16, 1, 2, 2),      # Port input FIFO (default afd: almost_full always 1)
    (8, 2, 8, 2),       # packed width_mult 2
    (16, 1, 8, 3),
]


@pytest.mark.parametrize("cfg", CONFIGS, ids=[f"w{a}x{b}_d{c}_afd{d}" for a, b, c, d in CONFIGS])
def test_fifo_model_invariants(cfg):
    fifo_case(*cfg, n=500, seed=1)


def test_fifo_model_order():
    """Everything popped is what was accepted, in order; dropped pushes are the
    ones that arrived while full."""
    m = FifoModel(4, 2)
    for v in range(6):
        m.clock(1, 0, v)
    assert list(m.items) == [0, 1, 2, 3]
    m.clock(1, 1, 99)           # full: pop succeeds, push dropped
    assert list(m.items) == [1, 2, 3]
    m.clock(1, 1, 7)
    assert list(m.items) == [2, 3, 7]


@requires_xrun
@pytest.mark.parametrize("cfg", CONFIGS, ids=[f"w{a}x{b}_d{c}_afd{d}" for a, b, c, d in CONFIGS])
def test_fifo_rtl(cfg):
    dut, inputs, expect = fifo_case(*cfg, seed=sum(cfg))
    res = run_vectors(dut, inputs, expect)
    assert res.passed, res


@requires_xrun
def test_fifo_rtl_rd_ptr_out():
    dut, inputs, expect = fifo_case(16, 1, 8, 2, seed=5, break_out_rd_ptr=True)
    res = run_vectors(dut, inputs, expect)
    assert res.passed, res


@requires_xrun
def test_fifo_rtl_width_mult_lanes():
    """width_mult lanes travel together: lane 0 in the LSBs of the flat vector."""
    dut = make_fifo(8, 4, 4, 2)
    vals = [[1, 2, 3, 4], [0xAA, 0xBB, 0xCC, 0xDD], [0xFF, 0, 0x80, 0x7F]]
    inputs = [{"push": 1, "data_in": pack(v, 8)} for v in vals] + [{"pop": 1}] * 3
    expect = [{"empty": 1}, {"data_out": pack(vals[0], 8)}, {"data_out": pack(vals[0], 8)},
              {"data_out": pack(vals[0], 8)}, {"data_out": pack(vals[1], 8)}, {"data_out": pack(vals[2], 8)}]
    res = run_vectors(dut, inputs, expect)
    assert res.passed, res


def passthru_case(n=100, seed=9):
    dut = make_fifo(16, 1, 0, 2)
    rng = random.Random(seed)
    inputs, expect = [], []
    for _ in range(n):
        push, pop, data = rng.getrandbits(1), rng.getrandbits(1), rng.getrandbits(16)
        inputs.append({"push": push, "pop": pop, "data_in": data})
        expect.append({"data_out": data, "valid": push, "empty": 1 - push, "full": 1 - pop,
                       "almost_full": 1 - pop})
    return dut, inputs, expect


@requires_xrun
def test_fifo_rtl_depth0_passthrough():
    dut, inputs, expect = passthru_case()
    res = run_vectors(dut, inputs, expect)
    assert res.passed, res
