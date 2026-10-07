"""Functional tests for lake.modules.memory_interface_decoder.MemoryInterfaceDecoder
(MID: one per spec Port; maps the Port's accesses onto the MemoryPort(s) it is
connected to, stacked by address range, and returns read data to the Port).

Function (memports stacked in order: memport k owns port addresses
[base_k, base_k + num_addrs_k)):
  * mp_en_k = en & (addr in range k); mp_addr_k = addr - base_k;
  * IN: mp_data_k = data; mp_clear_k = mp_en_k on a flush_mem memport;
    resource_ready = 1;
  * DYNAMIC: grant = OR of the memports' mp_grant_k (the arbiters' grants);
  * OUT, STATIC or DYNAMIC+opt_rv: data = the read data of the memport decoded
    ``delay`` cycles earlier (with one memport: mp_data_0 directly);
    data_valid = en (STATIC) or en & grant (opt_rv) delayed by ``delay`` =
    the memports' worst read delay; resource_ready = 1;
  * OUT, DYNAMIC (not opt_rv): a skid FIFO of depth
    round_up_pow2(2 + delay), almost_full at depth - (delay + 1):
    resource_ready = ~almost_full; a request is accepted iff en & grant &
    resource_ready, and its read data (mp_data_0 ``delay`` cycles later) is
    pushed; data / data_valid are the FIFO head / ~empty; data_ready pops.
    Every accepted request's data is delivered exactly once, in order (the skid
    depth covers the in-flight reads, so nothing is pushed into a full FIFO).
Notes on the RTL (tested as such, not bugs by themselves): STATIC OUT
data_valid follows the port en, not the decoded enable, so an out-of-range
address still yields data_valid; with delay 0 the DYNAMIC skid FIFO is depth 2
with almost_full at 1 entry, so it accepts at most every other cycle.
Only single-memport decoders can be built (see the xfail test), which is what
every spec builder uses.
"""
import random
from collections import deque
from types import SimpleNamespace as NS

import pytest

from _kratos.exception import VarException
from lake.modules.memory_interface_decoder import MemoryInterfaceDecoder
from lake.spec.memory_port import MemoryPort
from lake.utils.spec_enum import Direction, MemoryPortType, Runtime
from rtl_harness import requires_xrun, run_vectors

_uid = [0]
DW = 16


def clog2(n):
    return max(0, (n - 1).bit_length())


def make_mid(direction, runtime, opt_rv=False, delay=0, num_addrs=(8,), clear=False, addr_width=None):
    mps = []
    for n in num_addrs:
        mp = MemoryPort(data_width=DW, mptype=MemoryPortType.W if direction == Direction.IN else MemoryPortType.R,
                        delay=delay, flush_mem=clear)
        mp.set_num_addrs(n)
        mps.append(mp)
    aw = addr_width if addr_width is not None else clog2(sum(num_addrs))
    _uid[0] += 1
    mid = MemoryInterfaceDecoder(name=f"mid_t{_uid[0]}", port_type=direction,
                                 port_intf={'addr': NS(width=aw), 'data': NS(width=DW)},
                                 memports=mps, runtime=runtime, opt_rv=opt_rv)
    mid.gen_hardware()
    return mid, aw


def decode(addr, en, num_addrs, aw):
    """[(mp_en_k, mp_addr_k)] for each memport."""
    out, base = [], 0
    for n in num_addrs:
        out.append((int(bool(en) and base <= addr < base + n), (addr - base) % (1 << clog2(n))))
        base += n
    return out


def rup2(x):
    return 1 if x < 1 else 1 << (x - 1).bit_length()


# ---------------------------------------------------------------- IN side
def in_case(runtime, num_addrs=(8,), clear=False, n=200, seed=0):
    mid, aw = make_mid(Direction.IN, runtime, num_addrs=num_addrs, clear=clear)
    rng = random.Random(seed)
    inputs, expect = [], []
    for i in range(n):
        addr = i % (1 << aw) if i < 2 * (1 << aw) else rng.randrange(1 << aw)   # sweep every address first
        en, data, g = rng.getrandbits(1), rng.getrandbits(DW), rng.getrandbits(1)
        c = {"addr": addr, "en": en, "data": data}
        e = {"resource_ready": 1}
        for k, (men, maddr) in enumerate(decode(addr, en, num_addrs, aw)):
            e.update({f"mp_en_{k}": men, f"mp_addr_{k}": maddr, f"mp_data_{k}": data})
            if clear:
                e[f"mp_clear_{k}"] = men
        if runtime == Runtime.DYNAMIC:
            c["mp_grant_0"] = g
            e["grant"] = g
        inputs.append(c)
        expect.append(e)
    return mid, inputs, expect


IN_CONFIGS = {
    "static_n8": dict(runtime=Runtime.STATIC, num_addrs=(8,)),
    "static_n6": dict(runtime=Runtime.STATIC, num_addrs=(6,)),        # addresses 6, 7 out of range
    "dynamic_n6_clear": dict(runtime=Runtime.DYNAMIC, num_addrs=(6,), clear=True),
    "dynamic_n16": dict(runtime=Runtime.DYNAMIC, num_addrs=(16,)),
}


@requires_xrun
@pytest.mark.parametrize("name", list(IN_CONFIGS))
def test_mid_in_rtl(name):
    mid, inputs, expect = in_case(**IN_CONFIGS[name], seed=len(name))
    res = run_vectors(mid, inputs, expect)
    assert res.passed, res


# ---------------------------------------------------------------- OUT side
class OutModel:
    """OUT-direction MID with one memport. ``mp_data`` of a cycle is supplied by
    the test's storage emulation (the token of the read issued ``delay``
    cycles earlier)."""

    def __init__(self, runtime, opt_rv, delay, num_addrs):
        self.runtime, self.opt_rv, self.delay, self.num_addrs = runtime, opt_rv, delay, num_addrs
        self.fifo_mode = runtime == Runtime.DYNAMIC and not opt_rv
        self.depth = rup2(2 + delay)
        self.afd = delay + 1
        self.pipe = deque([0] * delay)      # valid / push shift register (oldest on the right)
        self.fifo = deque()
        self.accepted, self.delivered = [], []

    def resource_ready(self):
        return int(not (self.fifo_mode and len(self.fifo) >= self.depth - self.afd))

    def req_bit(self, en, grant):
        if self.fifo_mode:
            return int(en and grant and self.resource_ready())
        if self.runtime == Runtime.DYNAMIC:
            return int(en and grant)
        return int(en)

    def outputs(self, c, mp_data):
        aw = clog2(self.num_addrs)
        men, maddr = decode(c["addr"], c["en"], (self.num_addrs,), aw)[0]
        head = self.pipe[-1] if self.delay else self.req_bit(c["en"], c.get("mp_grant_0", 0))
        e = {"mp_en_0": men, "mp_addr_0": maddr, "resource_ready": self.resource_ready()}
        if self.runtime == Runtime.DYNAMIC:
            e["grant"] = c["mp_grant_0"]
        if self.fifo_mode:
            e["data_valid"] = int(bool(self.fifo))
            e["data"] = self.fifo[0] if self.fifo else None
        else:
            e["data_valid"] = head
            e["data"] = mp_data
        return e, head

    def clock(self, c, mp_data, head):
        """``head`` = the push reaching the FIFO this cycle (req delayed)."""
        req = self.req_bit(c["en"], c.get("mp_grant_0", 0))
        if self.fifo_mode:
            if req:
                self.accepted.append(c["_token"])
            n_before = len(self.fifo)          # full / empty are the registered count
            if head:
                assert n_before < self.depth, "skid FIFO overflow: accepted read data dropped"
            if c.get("data_ready", 0) and n_before:
                self.delivered.append(self.fifo.popleft())
            if head and n_before < self.depth:
                self.fifo.append(mp_data)
        if self.delay:
            self.pipe.appendleft(req)
            self.pipe.pop()


def out_case(runtime, opt_rv=False, delay=0, num_addrs=8, n=300, seed=0):
    mid, aw = make_mid(Direction.OUT, runtime, opt_rv=opt_rv, delay=delay, num_addrs=(num_addrs,))
    model = OutModel(runtime, opt_rv, delay, num_addrs)
    rng = random.Random(seed)
    issued = deque([None] * delay)      # storage emulation: token of the read issued `delay` cycles ago
    inputs, expect = [], []
    for i in range(n):
        phase = (i // 50) % 3           # bursts: ready mostly high / mostly low / random
        p_ready = [0.9, 0.2, 0.5][phase]
        addr = rng.randrange(1 << aw)
        c = {"addr": addr, "en": int(rng.random() < 0.7), "data_ready": int(rng.random() < p_ready)}
        if runtime == Runtime.DYNAMIC:
            c["mp_grant_0"] = int(rng.random() < 0.8)
        token = (i * 0x9E37 + addr) & 0xFFFF
        c["_token"] = token
        # the storage answers a read that went out `delay` cycles ago (mp_en & grant),
        # anything else on the bus is junk
        if delay:
            prev = issued[-1]
            mp_data = prev if prev is not None else rng.getrandbits(DW)
        else:
            mp_data = token
        c["mp_data_0"] = mp_data
        e, head = model.outputs(c, mp_data)
        model.clock(c, mp_data, head)
        granted = c["en"] and (runtime != Runtime.DYNAMIC or c["mp_grant_0"])
        if delay:
            issued.appendleft(token if granted else None)
            issued.pop()
        inputs.append({k: v for k, v in c.items() if k != "_token"})
        expect.append(e)
    if model.fifo_mode:
        # function: everything delivered was accepted, in order, none lost
        assert model.delivered == model.accepted[:len(model.delivered)]
        assert len(model.accepted) - len(model.delivered) == len(model.fifo) + sum(model.pipe)
    return mid, inputs, expect, model


OUT_CONFIGS = {
    "static_d0_n6": dict(runtime=Runtime.STATIC, delay=0, num_addrs=6),
    "static_d1_n8": dict(runtime=Runtime.STATIC, delay=1, num_addrs=8),
    "opt_rv_d0": dict(runtime=Runtime.DYNAMIC, opt_rv=True, delay=0, num_addrs=8),
    "opt_rv_d1": dict(runtime=Runtime.DYNAMIC, opt_rv=True, delay=1, num_addrs=16),
    "fifo_d0": dict(runtime=Runtime.DYNAMIC, delay=0, num_addrs=8),
    "fifo_d1": dict(runtime=Runtime.DYNAMIC, delay=1, num_addrs=8),
    "fifo_d2": dict(runtime=Runtime.DYNAMIC, delay=2, num_addrs=6),
}


@pytest.mark.parametrize("name", [k for k in OUT_CONFIGS if k.startswith("fifo")])
def test_mid_out_fifo_model_delivers_in_order(name):
    """Model-level: the skid FIFO never overflows and delivers every accepted
    request's data in order (asserted inside out_case)."""
    _, _, _, model = out_case(**OUT_CONFIGS[name], n=2000, seed=3)
    assert len(model.delivered) > 100


@requires_xrun
@pytest.mark.parametrize("name", list(OUT_CONFIGS))
def test_mid_out_rtl(name):
    mid, inputs, expect, _ = out_case(**OUT_CONFIGS[name], seed=len(name))
    res = run_vectors(mid, inputs, expect)
    assert res.passed, res


# ---------------------------------------------------------------- multi-memport
def multi_case(direction):
    """Two 4-address memports behind one 3-bit Port address: 0..3 -> memport 0,
    4..7 -> memport 1 (STATIC, delay 0)."""
    mid, aw = make_mid(direction, Runtime.STATIC, num_addrs=(4, 4))
    rng = random.Random(8)
    inputs, expect = [], []
    for i in range(64):
        addr, en = i % 8 if i < 16 else rng.randrange(8), 1 if i < 16 else rng.getrandbits(1)
        c, e = {"addr": addr, "en": en}, {}
        dec = decode(addr, en, (4, 4), aw)
        for k, (men, maddr) in enumerate(dec):
            e.update({f"mp_en_{k}": men, f"mp_addr_{k}": maddr})
        if direction == Direction.IN:
            c["data"] = rng.getrandbits(DW)
            e.update({"mp_data_0": c["data"], "mp_data_1": c["data"]})
        else:
            c.update({"mp_data_0": rng.getrandbits(DW), "mp_data_1": rng.getrandbits(DW)})
            sel = [k for k, (men, _) in enumerate(dec) if men]
            e.update({"data_valid": en, "data": c[f"mp_data_{sel[0]}"] if sel else 0})
        inputs.append(c)
        expect.append(e)
    return mid, inputs, expect


@requires_xrun
@pytest.mark.xfail(strict=True, raises=VarException,
                   reason="mp_addr_k (clog2(num_addrs_k) bits) = addr - base_k (Port address, "
                   "clog2(sum num_addrs) bits): kratos width mismatch, so a Port spanning >1 MemoryPort "
                   "cannot be built (no spec builder does this today)")
@pytest.mark.parametrize("direction", [Direction.IN, Direction.OUT], ids=["IN", "OUT"])
def test_mid_two_memports_rtl(direction):
    mid, inputs, expect = multi_case(direction)
    res = run_vectors(mid, inputs, expect)
    assert res.passed, res
