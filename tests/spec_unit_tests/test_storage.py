"""Functional tests for lake.spec.storage.SingleBankStorage (non-remote,
simulatable single bank: the storage a spec builds when remote=False, e.g. the
filter-path storage of the RV MEM specs).

Function:
  * one bank of ``capacity`` bytes, seen by each MemoryPort as words of its own
    width w: word a of a port covers bytes [a*w/8, (a+1)*w/8) (little-endian,
    byte 0 in the LSBs), so a 64-bit read of word b returns 16-bit words
    4b..4b+3 with 4b in the LSBs; num_addrs = capacity // (w/8);
  * W port: at the clock edge, if clk_en and write_en: word[write_addr] = write_data;
  * W port with flush_mem (clear): if clear, the WHOLE bank becomes 0 at the
    edge; clear wins over a same-cycle write and, like flush elsewhere in
    lake, is not gated by clk_en;
  * R port, delay 0: combinational, read_data = word[read_addr] if read_en else
    0; it sees the bank before this cycle's writes (writes land at the edge);
  * R port, delay 1: at the edge, if clk_en and read_en, read_data <= word[read_addr]
    (the bank before this cycle's writes, i.e. read-first); otherwise it holds;
  * RW port (always registered, delay 1): at the edge, if clk_en: write_en ->
    write; else read_en -> read_data <= word[addr]; a write+read cycle only
    writes and read_data holds.
Unwritten data is X in the RTL and a don't-care here.
"""
import random

import pytest

from _kratos.exception import StmtException
from lake.spec.memory_port import MemoryPort
from lake.spec.storage import SingleBankStorage
from lake.utils.spec_enum import MemoryPortType as T
from rtl_harness import requires_xrun, run_vectors


def make_storage(capacity, ports):
    """ports: [(type, width, delay, clear)] -> (storage, [MemoryPort])."""
    mps = [MemoryPort(data_width=w, mptype=t, delay=d, flush_mem=c) for t, w, d, c in ports]
    stg = SingleBankStorage(capacity=capacity)
    stg.gen_hardware(memory_ports=mps)
    return stg, mps


class StorageModel:
    def __init__(self, capacity, ports):
        self.bytes = [None] * capacity
        self.ports = ports
        self.rdata = [None] * len(ports)     # registered read data (delay-1 R, RW)

    def nwords(self, p):
        return len(self.bytes) // (self.ports[p][1] // 8)

    def read(self, p, addr):
        nb = self.ports[p][1] // 8
        chunk = self.bytes[addr * nb:(addr + 1) * nb]
        if any(b is None for b in chunk):
            return None
        return sum(b << (8 * i) for i, b in enumerate(chunk))

    def write(self, p, addr, data):
        nb = self.ports[p][1] // 8
        for i in range(nb):
            self.bytes[addr * nb + i] = (data >> (8 * i)) & 0xFF

    def outputs(self, cyc):
        out = {}
        for p, (t, w, d, c) in enumerate(self.ports):
            if t == T.R and d == 0:
                if cyc.get(f"memory_port_{p}_read_en", 0):
                    out[f"memory_port_{p}_read_data"] = self.read(p, cyc[f"memory_port_{p}_read_addr"])
                else:
                    out[f"memory_port_{p}_read_data"] = 0
            elif t in (T.R, T.RW):
                out[f"memory_port_{p}_read_data"] = self.rdata[p]
        return out

    def clock(self, cyc):
        clk_en = cyc.get("clk_en", 1)
        # registered reads sample the bank before this edge's writes
        for p, (t, w, d, c) in enumerate(self.ports):
            if not clk_en:
                continue
            if t == T.R and d >= 1 and cyc.get(f"memory_port_{p}_read_en", 0):
                self.rdata[p] = self.read(p, cyc[f"memory_port_{p}_read_addr"])
            if t == T.RW and not cyc.get(f"memory_port_{p}_write_en", 0) and cyc.get(f"memory_port_{p}_read_en", 0):
                self.rdata[p] = self.read(p, cyc[f"memory_port_{p}_addr"])
        for p, (t, w, d, c) in enumerate(self.ports):
            if t == T.W and c and cyc.get(f"memory_port_{p}_clear", 0):
                self.bytes = [0] * len(self.bytes)
            elif t == T.W and clk_en and cyc.get(f"memory_port_{p}_write_en", 0):
                self.write(p, cyc[f"memory_port_{p}_write_addr"], cyc[f"memory_port_{p}_write_data"])
            elif t == T.RW and clk_en and cyc.get(f"memory_port_{p}_write_en", 0):
                self.write(p, cyc[f"memory_port_{p}_addr"], cyc[f"memory_port_{p}_write_data"])


def stimulus(capacity, ports, n, seed, ctrl=True):
    rng = random.Random(seed)
    nwords = [capacity // (w // 8) for _, w, _, _ in ports]
    writers = [p for p, (t, _, _, _) in enumerate(ports) if t in (T.W, T.RW)]
    cycles = []
    last_w = {}
    # sweep-write every word of the first writer so most reads are defined
    wp = writers[0]
    t0, w0 = ports[wp][0], ports[wp][1]
    for a in range(nwords[wp]):
        c = {f"memory_port_{wp}_{'write_addr' if t0 == T.W else 'addr'}": a,
             f"memory_port_{wp}_write_data": rng.getrandbits(w0), f"memory_port_{wp}_write_en": 1}
        cycles.append(c)
    while len(cycles) < n:
        c = {}
        for p, (t, w, d, clr) in enumerate(ports):
            nw = nwords[p]
            if t == T.W:
                if clr and rng.random() < 0.03:
                    c[f"memory_port_{p}_clear"] = 1
                c[f"memory_port_{p}_write_en"] = int(rng.random() < 0.5)
                c[f"memory_port_{p}_write_addr"] = a = rng.randrange(nw)
                c[f"memory_port_{p}_write_data"] = rng.getrandbits(w)
                last_w[p] = a
            elif t == T.R:
                c[f"memory_port_{p}_read_en"] = int(rng.random() < 0.6)
                # bias toward the address just (or being) written, scaled to this port's words
                if last_w and rng.random() < 0.4:
                    q = rng.choice(list(last_w))
                    a = last_w[q] * (ports[q][1] // 8) // (w // 8)
                else:
                    a = rng.randrange(nw)
                c[f"memory_port_{p}_read_addr"] = a
            else:   # RW
                c[f"memory_port_{p}_write_en"] = int(rng.random() < 0.4)
                c[f"memory_port_{p}_read_en"] = int(rng.random() < 0.5)
                c[f"memory_port_{p}_addr"] = a = rng.randrange(nw)
                c[f"memory_port_{p}_write_data"] = rng.getrandbits(w)
                last_w[p] = a
        if ctrl:
            c["clk_en"] = int(rng.random() > 0.08)
        cycles.append(c)
    return cycles


def storage_case(capacity, ports, n=300, seed=0, ctrl=True, cycles=None):
    stg, _ = make_storage(capacity, ports)
    cycles = cycles if cycles is not None else stimulus(capacity, ports, n, seed, ctrl)
    model = StorageModel(capacity, ports)
    expect = []
    for c in cycles:
        expect.append(model.outputs(c))
        model.clock(c)
    return stg, cycles, expect


R0, R1 = (T.R, 16, 0, False), (T.R, 16, 1, False)
W16 = (T.W, 16, 1, False)
W16C = (T.W, 16, 1, True)
CONFIGS = {
    "w_r0": (32, [W16, R0]),
    "w_r1": (32, [W16, R1]),
    "w_r0_r1": (32, [W16, R0, R1]),
    "rw64": (32, [(T.RW, 64, 1, False)]),
    "rw64_r64": (32, [(T.RW, 64, 1, False), (T.R, 64, 1, False)]),   # spec dual_port: [RW, R]
    "w16_r64": (32, [W16, (T.R, 64, 1, False)]),                      # wide-fetch read view
    "w64_r16": (32, [(T.W, 64, 1, False), R0]),
    "wclr_r0": (16, [W16C, R0]),
    "wclr_r1_r0": (16, [W16C, R1, R0]),
}


def test_storage_model_word_view():
    m = StorageModel(16, [W16, (T.R, 64, 1, False)])
    for a, v in enumerate([0x1111, 0x2222, 0x3333, 0x4444]):
        m.write(0, a, v)
    assert m.read(1, 0) == 0x4444333322221111
    assert m.read(1, 1) is None


def test_storage_model_read_first():
    ports = [W16, R0, R1]
    m = StorageModel(8, ports)
    c = {"memory_port_0_write_en": 1, "memory_port_0_write_addr": 2, "memory_port_0_write_data": 5,
         "memory_port_1_read_en": 1, "memory_port_1_read_addr": 2,
         "memory_port_2_read_en": 1, "memory_port_2_read_addr": 2}
    assert m.outputs(c)["memory_port_1_read_data"] is None    # comb read sees the old (unwritten) word
    m.clock(c)
    assert m.rdata[2] is None                                  # registered read is read-first
    assert m.outputs(c)["memory_port_1_read_data"] == 5


@requires_xrun
@pytest.mark.parametrize("name", list(CONFIGS))
def test_storage_rtl(name):
    capacity, ports = CONFIGS[name]
    stg, cycles, expect = storage_case(capacity, ports, seed=len(name))
    res = run_vectors(stg, cycles, expect)
    assert res.passed, res


def raw_timing_case():
    """Hand-written read-after-write timeline (independent of the model):
    W16 + comb R (delay 0) + registered R (delay 1), all on word 3."""
    ports = [W16, R0, R1]
    stg, _ = make_storage(8, ports)

    def cyc(wen=0, wd=0, ren=1):
        return {"memory_port_0_write_en": wen, "memory_port_0_write_addr": 3, "memory_port_0_write_data": wd,
                "memory_port_1_read_en": ren, "memory_port_1_read_addr": 3,
                "memory_port_2_read_en": ren, "memory_port_2_read_addr": 3}
    cycles = [cyc(1, 0xA), cyc(), cyc(1, 0xB), cyc(), cyc(ren=0), cyc(1, 0xC, ren=0), cyc(ren=0), cyc()]
    r0 = "memory_port_1_read_data"
    r1 = "memory_port_2_read_data"
    expect = [
        {},                     # 0: write A (bank still X)
        {r0: 0xA},              # 1: comb read sees A; registered read sampled X at edge 0
        {r0: 0xA, r1: 0xA},     # 2: write B this cycle: comb still A; reg = A (sampled edge 1)
        {r0: 0xB, r1: 0xA},     # 3: B visible comb; reg sampled A at edge 2 (read-first)
        {r0: 0, r1: 0xB},       # 4: read_en 0: comb read 0, reg = B (sampled edge 3)
        {r0: 0, r1: 0xB},       # 5: reg holds (read_en 0 at edge 4); write C
        {r0: 0, r1: 0xB},       # 6: holds
        {r0: 0xC, r1: 0xB},     # 7: C visible comb; reg still B until edge 7
    ]
    return stg, cycles, expect


@requires_xrun
def test_storage_rtl_read_after_write_timing():
    stg, cycles, expect = raw_timing_case()
    res = run_vectors(stg, cycles, expect)
    assert res.passed, res


def clear_case():
    """clear zeroes the whole bank, wins over a same-cycle write, and is not
    gated by clk_en; a write with clk_en = 0 is dropped."""
    ports = [W16C, R0]
    stg, _ = make_storage(8, ports)     # 4 words

    def w(a, d, clear=0, clk_en=1):
        return {"memory_port_0_write_en": 1, "memory_port_0_write_addr": a, "memory_port_0_write_data": d,
                "memory_port_0_clear": clear, "clk_en": clk_en}

    def r(a, clear=0, clk_en=1):
        return {"memory_port_1_read_en": 1, "memory_port_1_read_addr": a, "memory_port_0_clear": clear,
                "clk_en": clk_en}
    rd = "memory_port_1_read_data"
    cycles = [w(0, 1), w(1, 2), w(2, 3), w(3, 4), r(0), r(3),
              w(1, 0x55, clear=1),                   # clear + write: clear wins
              r(0), r(1), r(2), r(3),
              w(2, 0x66), r(2),
              w(2, 0x77, clk_en=0), r(2),            # write dropped while clk_en = 0
              r(1, clear=1, clk_en=0), r(2)]         # clear with clk_en = 0 still clears
    expect = [{}, {}, {}, {}, {rd: 1}, {rd: 4},
              {},
              {rd: 0}, {rd: 0}, {rd: 0}, {rd: 0},
              {}, {rd: 0x66},
              {}, {rd: 0x66},
              {rd: 0}, {rd: 0}]
    return stg, cycles, expect


@requires_xrun
def test_storage_rtl_clear():
    stg, cycles, expect = clear_case()
    res = run_vectors(stg, cycles, expect)
    assert res.passed, res


def two_writer_case():
    """build_pond_rv's memory-port set [W, R, R, W(flush_mem)] on one bank."""
    ports = [W16, R0, R0, (T.W, 16, 0, True)]
    capacity = 16
    rng = random.Random(4)
    cycles = []
    for i in range(120):
        c = {}
        a0 = rng.randrange(8)
        c.update({"memory_port_0_write_en": int(rng.random() < 0.5), "memory_port_0_write_addr": a0,
                  "memory_port_0_write_data": rng.getrandbits(16)})
        a3 = (a0 + 1 + rng.randrange(7)) % 8          # never the same word as port 0
        c.update({"memory_port_3_write_en": int(rng.random() < 0.3), "memory_port_3_write_addr": a3,
                  "memory_port_3_write_data": rng.getrandbits(16),
                  "memory_port_3_clear": int(rng.random() < 0.03)})
        for p in (1, 2):
            c.update({f"memory_port_{p}_read_en": 1, f"memory_port_{p}_read_addr": rng.randrange(8)})
        cycles.append(c)
    return storage_case(capacity, ports, cycles=cycles)


@requires_xrun
@pytest.mark.xfail(strict=True, raises=StmtException,
                   reason="every W/RW port wires its own register array onto data_array, so two write "
                   "ports (build_pond_rv's [W, R, R, W(flush_mem)] set) give 'data_array has multiple "
                   "driver' at Verilog emission; only remote pond storage can be generated")
def test_storage_rtl_two_write_ports(tmp_path):
    stg, cycles, expect = two_writer_case()
    res = run_vectors(stg, cycles, expect, workdir=str(tmp_path))
    assert res.passed, res
