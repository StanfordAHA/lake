"""Functional tests for lake.spec.schedule_generator: ScheduleGenerator (static
schedule of a spec Port) and ReadyValidScheduleGenerator (RV Port). One per
Port; its step drives the Port's IterationDomain and AddressGenerator, whose
mux_sel / restart / finished / iterators come back as inputs (spec.py).

ScheduleGenerator, configured by gen_bitstream(schedule_map={'offset',
'strides'}, extents, dimensionality) (which also sets enable):
  * a counter counts cycles from flush (clk_en cycles; with external_count,
    cycles with clk_en & external_count);
  * step = enable & ~flush & ~finished & (counter == offset + sum_i strides[i]
    * iterators[i]) for the current iteration of the domain it steps. For a
    strictly increasing schedule, the k-th iteration of the loop nest is
    therefore stepped exactly when the counter reaches T_k = offset + strides .
    it_k, i.e. (clk_en = 1) in the cycle T_k after the flush cycle; with clk_en
    low the counter and the domain hold, so step stays up;
  * recurrence=True keeps the target in a register updated like the address
    generator (flush / restart -> offset, step -> += delta[mux_sel]); with
    external_count only on a cycle with external_count; recurrence=False
    computes it from the iterators input;
  * unconfigured (enable = 0) it never steps; rst_n resets the counter and the
    target, the unit needs a flush after configuration (step during rst_n is a
    don't-care).
ReadyValidScheduleGenerator (gen_bitstream only sets enable), stateless:
  * step = enable & ~flush & ~finished & AND(comparisons) (the RV comparison
    network's per-constraint go signals);
  * passes iterators and finished through, and extents decoded (encoded + 2)
    to the comparison network. extents_out_lcl is extent_width bits, so it is
    tested for extents up to 2**extent_width - 1 (larger ones wrap; the network
    adds extents to iterators in extent_width bits anyway).
"""
import random

import pytest

from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ReadyValidScheduleGenerator, ScheduleGenerator
from rtl_harness import pack, pack_config, requires_xrun, run_vectors
from test_iteration_domain import IterationDomainModel, iteration_sequence


def schedule_times(offset, strides, extents):
    return [offset + sum(s * i for s, i in zip(strides, it)) for it in iteration_sequence(extents)]


class StaticScheduleLoop:
    """The static SG in a loop with the IterationDomain it steps: closed form
    target offset + strides . iterators (not the RTL's delta register)."""

    def __init__(self, dims, ew, dim, extents, offset, strides, external_count=False, enable=True):
        self.id = IterationDomainModel(dims, ew, dim, extents)
        self.ew = ew
        self.offset, self.strides = offset, list(strides)
        self.external_count = external_count
        self.enable = enable
        self.ctr = 0
        self.init = False       # flushed since the last reset
        self.steps = 0          # domain steps since the last flush / reset

    def target(self):
        return self.offset + sum(s * i for s, i in zip(self.strides, self.id.it))

    def cycle(self, c):
        """(expected step or None, the ID outputs the SG sees) of one cycle."""
        flush, clk_en = c.get("flush", 0), c.get("clk_en", 1)
        ext = c.get("external_count", 1) if self.external_count else 1
        if not c.get("rst_n", 1):
            self.id.reset()
            self.ctr, self.init, self.steps = 0, False, 0
            return None, self.id.outputs(0)
        if not (self.init or flush):
            step = None
        else:
            step = int(bool(self.init and self.enable and not flush and not self.id.finished
                            and self.ctr == self.target()))
        id_step = bool(step) and bool(ext)
        o = self.id.outputs(id_step)
        if flush:
            self.id.reset()
            self.ctr, self.init, self.steps = 0, True, 0
        elif clk_en:
            if ext:
                self.ctr += 1
            self.steps += id_step
            self.id.tick(id_step)
        return step, o


def sg_stimulus(loop, seed, ctrl=True, ext=False, past_end=12):
    """flush in cycle 0, then random clk_en (and external_count); a flush
    mid-run, run to finished, reset + flush, run to finished, steps past the
    end. ``loop`` (a fresh StaticScheduleLoop) tracks progress."""
    rng = random.Random(seed)
    cycles = []

    def add(c):
        loop.cycle(c)
        cycles.append(c)

    def rnd():
        c = {}
        if ctrl:
            c["clk_en"] = int(rng.random() > 0.2)
        if ext:
            c["external_count"] = int(rng.random() < 0.6)
        return c

    def run(until_steps=None):
        n = 0
        while not loop.id.finished and (until_steps is None or loop.steps < until_steps):
            add(rnd())
            n += 1
            assert n < 5000, "schedule never finishes"
        if until_steps is None:
            for _ in range(past_end):
                add(rnd())

    add({"flush": 1})
    if ctrl:
        run(until_steps=max(1, len(iteration_sequence(loop.id.extents)) // 2))
        add({"flush": 1, "clk_en": 0})
        run()
        add({"rst_n": 0})
        add({"flush": 1})
    run()
    return cycles


def sg_vectors(loop, cycles, ew):
    ins, exp = [], []
    for c in cycles:
        step, o = loop.cycle(c)
        d = dict(c, mux_sel=o["mux_sel"], restart=o["restart"], finished=o["finished"],
                 iterators=pack(o["iterators"], ew))
        ins.append(d)
        exp.append({"step": step})
    return ins, exp


def make_sg(dims, ew, sw, recurrence, external_count=False):
    idg = IterationDomain(dimensionality=dims, extent_width=ew)
    idg.gen_hardware()
    sg = ScheduleGenerator(dimensionality=dims, stride_width=sw, recurrence=recurrence,
                           external_count=external_count,
                           name=f"sg_{dims}_{ew}_{sw}_{int(recurrence)}_{int(external_count)}")
    sg.gen_hardware(id=idg)
    return sg


# (dims, extent_width, stride_width, dimensionality, extents, offset, strides)
CONFIGS = [
    (1, 4, 6, 1, [5], 3, [2]),
    (3, 4, 8, 2, [3, 2], 0, [1, 5]),                              # offset 0: steps right after flush
    (3, 4, 8, 3, [2, 3, 2], 4, [1, 3, 10]),                       # full, gaps between rows / planes
    (4, 5, 8, 4, [2, 2, 2, 2], 1, [1, 2, 4, 8]),                  # full, back-to-back steps
    (6, 4, 10, 6, [2, 2, 2, 2, 2, 2], 7, [1, 2, 4, 8, 16, 32]),   # full 6-D, minimum extents
    (2, 3, 8, 2, [8, 2], 2, [1, 9]),                              # extent 2**extent_width
    (3, 5, 6, 3, [3, 2, 2], 63, [2, 7, 20]),                      # largest offset for stride_width 6
    (3, 4, 8, 1, [2], 5, [3]),                                    # 1-D minimum extent in a 3-D unit
]


def _cfg_id(c):
    return f"D{c[0]}_w{c[1]}_s{c[2]}_dim{c[3]}_" + "x".join(map(str, c[4]))


@pytest.mark.parametrize("cfg", CONFIGS, ids=_cfg_id)
def test_schedule_model_steps_at_schedule_times(cfg):
    """Free running (clk_en = 1): step exactly in the cycles 1 + T_k (cycle 0
    is the flush), once per iteration, in loop order; nothing after."""
    dims, ew, sw, dim, extents, offset, strides = cfg
    times = schedule_times(offset, strides, extents)
    assert all(b > a for a, b in zip(times, times[1:])), "test schedule must be strictly increasing"
    loop = StaticScheduleLoop(dims, ew, dim, extents, offset, strides)
    seq = iteration_sequence(extents)
    steps = []
    for cyc in range(times[-1] + 30):
        step, o = loop.cycle({"flush": int(cyc == 0)})
        if step:
            assert o["iterators"][:dim] == seq[len(steps)]
            steps.append(cyc)
    assert steps == [1 + t for t in times]


def test_schedule_generator_bitstream_range():
    """starting_cycle and the (transformed) strides are stride_width registers;
    gen_bitstream refuses values that do not fit rather than truncating."""
    sg = make_sg(2, 4, 6, True)
    sg.gen_bitstream({"offset": 63, "strides": [1, 63]}, [2, 2], 2)       # delta 62 fits
    with pytest.raises(ValueError):
        sg.gen_bitstream({"offset": 64, "strides": [1, 4]}, [2, 2], 2)
    with pytest.raises(ValueError):
        sg.gen_bitstream({"offset": 0, "strides": [1, 66]}, [2, 2], 2)    # delta 65


@requires_xrun
@pytest.mark.parametrize("recurrence", [True, False], ids=["recurrence", "explicit"])
@pytest.mark.parametrize("cfg", CONFIGS, ids=_cfg_id)
def test_schedule_generator_rtl(cfg, recurrence):
    """In a loop with its iteration domain: flush, random clk_en low, flush
    mid-run (with clk_en low), run to finished and past the end, reset +
    flush, run again: step exactly when counter == offset + strides . it."""
    dims, ew, sw, dim, extents, offset, strides = cfg
    sg = make_sg(dims, ew, sw, recurrence)
    config = pack_config(sg.gen_bitstream({"offset": offset, "strides": strides}, extents, dim))
    cycles = sg_stimulus(StaticScheduleLoop(dims, ew, dim, extents, offset, strides), seed=dims * 3 + dim)
    ins, exp = sg_vectors(StaticScheduleLoop(dims, ew, dim, extents, offset, strides), cycles, ew)
    assert sum(1 for e in exp if e["step"]) >= 2 * len(iteration_sequence(extents))
    res = run_vectors(sg, ins, exp, config=config)
    assert res.passed, res


@requires_xrun
@pytest.mark.parametrize("recurrence", [True, False], ids=["recurrence", "explicit"])
@pytest.mark.parametrize("cfg", [CONFIGS[2], CONFIGS[6]], ids=_cfg_id)
def test_schedule_generator_rtl_external_count(cfg, recurrence):
    """external_count (the Port filter's SG): the counter counts
    external_count cycles; step is a level (not gated by external_count); the
    domain advances on step & external_count. (Schedules with gaps: with a
    back-to-back schedule step is simply high throughout, whatever is counted.)"""
    dims, ew, sw, dim, extents, offset, strides = cfg
    sg = make_sg(dims, ew, sw, recurrence, external_count=True)
    config = pack_config(sg.gen_bitstream({"offset": offset, "strides": strides}, extents, dim))
    mk = lambda: StaticScheduleLoop(dims, ew, dim, extents, offset, strides, external_count=True)
    cycles = sg_stimulus(mk(), seed=11 + dims, ext=True)
    ins, exp = sg_vectors(mk(), cycles, ew)
    res = run_vectors(sg, ins, exp, config=config)
    assert res.passed, res


@requires_xrun
@pytest.mark.parametrize("recurrence", [True, False], ids=["recurrence", "explicit"])
def test_schedule_generator_rtl_unconfigured_never_steps(recurrence):
    """config 0 (enable = 0, offset = strides = 0): counter == target right
    after the flush, but no step."""
    dims, ew, sw, dim, extents = 3, 4, 8, 3, [2, 2, 2]
    sg = make_sg(dims, ew, sw, recurrence)
    loop = StaticScheduleLoop(dims, ew, dim, extents, 0, [0, 0, 0], enable=False)
    cycles = [{"flush": 1}] + [{} for _ in range(30)]
    ins, exp = sg_vectors(loop, cycles, ew)
    assert all(e["step"] == 0 for e in exp)
    res = run_vectors(sg, ins, exp, config=0)
    assert res.passed, res


# ---------------------------------------------------------------- ready-valid

class RVScheduleLoop:
    """RV SG in a loop with its IterationDomain (stepped by the RV SG's step,
    as an IN/OUT Port's ID is by the grant of a lone, ready memory port)."""

    def __init__(self, dims, ew, dim, extents, enable=True):
        self.id = IterationDomainModel(dims, ew, dim, extents)
        self.dims, self.ew = dims, ew
        self.enc = [e - 2 for e in extents] + [0] * (dims - len(extents))
        self.enable = enable

    def cycle(self, c, n_comp):
        flush, clk_en = c.get("flush", 0), c.get("clk_en", 1)
        if not c.get("rst_n", 1):
            self.id.reset()
        comps = c.get("comparisons", 0)
        step = int(self.enable and not flush and not self.id.finished and comps == (1 << n_comp) - 1)
        o = self.id.outputs(step)
        ins = dict(c, finished=o["finished"], mux_sel=o["mux_sel"], restart=o["restart"],
                   iterators=pack(o["iterators"], self.ew), extents=pack(self.enc, self.ew))
        exp = {"step": step, "finished_out": o["finished"], "iterators_out_lcl": pack(o["iterators"], self.ew),
               "extents_out_lcl": pack([(e + 2) % (1 << self.ew) for e in self.enc], self.ew)}
        if c.get("rst_n", 1):
            self.id.tick(step, flush, clk_en)
        return ins, exp


def rv_stimulus(dims, ew, dim, extents, n_comp, seed):
    """Random comparisons (each bit 1 with p .85) and clk_en, a flush in cycle
    20; each time the domain finished: all comparisons true past the end, then
    a reset (1st time) or a flush (2nd). Returns (inputs, expected)."""
    rng = random.Random(seed)
    shadow = RVScheduleLoop(dims, ew, dim, extents)
    ones = (1 << n_comp) - 1
    cycles, finishes = [], 0

    def add(c):
        shadow.cycle(c, n_comp)
        cycles.append(c)

    while finishes < 2:
        was = shadow.id.finished
        add({"comparisons": sum(int(rng.random() < 0.85) << b for b in range(n_comp)),
             "clk_en": int(rng.random() > 0.15), "flush": int(len(cycles) == 20)})
        if shadow.id.finished and not was:
            finishes += 1
            for _ in range(6):
                add({"comparisons": ones})
            add({"comparisons": ones, "rst_n": 0} if finishes == 1 else {"comparisons": ones, "flush": 1})
        assert len(cycles) < 3000
    loop = RVScheduleLoop(dims, ew, dim, extents)
    ins, exp = zip(*[loop.cycle(c, n_comp) for c in cycles])
    return list(ins), list(exp)


# (dims, extent_width, n_comparisons, dimensionality, extents)
RV_CONFIGS = [
    (1, 4, 1, 1, [5]),
    (3, 5, 2, 3, [2, 3, 4]),
    (4, 6, 3, 2, [7, 3]),
    (6, 4, 4, 6, [2, 2, 2, 2, 2, 3]),
    (2, 3, 2, 2, [7, 2]),               # largest extent extents_out_lcl can carry
]


def _rv_id(c):
    return f"D{c[0]}_w{c[1]}_c{c[2]}_dim{c[3]}_" + "x".join(map(str, c[4]))


def make_rv_sg(dims, ew, n_comp):
    idg = IterationDomain(dimensionality=dims, extent_width=ew)
    idg.gen_hardware()
    sg = ReadyValidScheduleGenerator(dimensionality=dims, name=f"rv_sg_{dims}_{ew}_{n_comp}")
    sg.gen_hardware(id=idg, num_comparisons=n_comp)
    return sg


@requires_xrun
@pytest.mark.parametrize("cfg", RV_CONFIGS, ids=_rv_id)
def test_rv_schedule_generator_rtl(cfg):
    dims, ew, n_comp, dim, extents = cfg
    sg = make_rv_sg(dims, ew, n_comp)
    config = pack_config(sg.gen_bitstream({"offset": 0, "strides": [0] * dim}, extents, dim))
    ins, exp = rv_stimulus(dims, ew, dim, extents, n_comp, seed=dims + n_comp)
    assert sum(1 for a, b in zip(exp, exp[1:]) if b["finished_out"] and not a["finished_out"]) == 2
    res = run_vectors(sg, ins, exp, config=config)
    assert res.passed, res


@requires_xrun
def test_rv_schedule_generator_rtl_unconfigured_never_steps():
    dims, ew, n_comp, dim, extents = RV_CONFIGS[1]
    sg = make_rv_sg(dims, ew, n_comp)
    rng = random.Random(5)
    loop = RVScheduleLoop(dims, ew, dim, extents, enable=False)
    ins, exp = zip(*[loop.cycle({"comparisons": rng.getrandbits(n_comp) | 1}, n_comp) for _ in range(60)])
    assert all(e["step"] == 0 for e in exp)
    res = run_vectors(sg, list(ins), list(exp), config=0)
    assert res.passed, res
