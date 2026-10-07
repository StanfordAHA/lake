"""Integration of one spec Port's controllers: IterationDomain +
AddressGenerator + ScheduleGenerator (static) or ReadyValidScheduleGenerator,
wired the way Spec.gen_hardware (spec.py) wires them for a Port whose memory
port it has alone (fw = 1, no wide-fetch Port logic):

  * ID mux_sel / restart / finished / iterators -> AG and SG;
  * static: SG step -> ID step and AG step;
  * RV (IN Port): ID/AG step = grant of the lone memory port = SG step & the
    Port's valid (resource_ready = 1); ID extents_out -> SG extents; the SG's
    comparisons come from the RV comparison network (driven here directly).

Configured only through each unit's gen_bitstream, placed at the wrapper's
child config bases like Spec.configure. Function: after a flush the Port
visits its loop nest in order, stepping at the schedule times (static) or
whenever all comparisons and valid allow (RV), and addr_out = offset +
strides . iterators of the current iteration, every cycle.
"""
import random

import kratos as kts
import pytest

from lake.spec.address_generator import AddressGenerator
from lake.spec.component import Component
from lake.spec.iteration_domain import IterationDomain
from lake.spec.schedule_generator import ReadyValidScheduleGenerator, ScheduleGenerator
from rtl_harness import pack, requires_xrun, run_vectors, pack_config
from test_address_generator import MemPortStub, affine
from test_iteration_domain import IterationDomainModel, iteration_sequence
from test_schedule_generator import StaticScheduleLoop, sg_stimulus


class PortControllers(Component):

    def __init__(self, dims, ew, sw, num_addrs, rv=False, n_comp=1, ag_recurrence=True):
        super().__init__(name=f"port_ctrl_{dims}_{ew}_{sw}_{num_addrs}_{int(rv)}_{n_comp}_{int(ag_recurrence)}")
        self.dims, self.num_addrs, self.rv, self.n_comp = dims, num_addrs, rv, n_comp
        self.pid = IterationDomain(dimensionality=dims, extent_width=ew)
        self.pag = AddressGenerator(dimensionality=dims, recurrence=ag_recurrence)
        if rv:
            self.psg = ReadyValidScheduleGenerator(dimensionality=dims, name=f"rv_sg_{dims}_{ew}_{n_comp}")
        else:
            self.psg = ScheduleGenerator(dimensionality=dims, stride_width=sw, name=f"sg_{dims}_{ew}_{sw}")

    def gen_hardware(self):
        pid, pag, psg = self.pid, self.pag, self.psg
        pid.gen_hardware()
        pag.gen_hardware(memports=[MemPortStub(self.num_addrs)], id=pid)
        if self.rv:
            psg.gen_hardware(id=pid, num_comparisons=self.n_comp)
        else:
            psg.gen_hardware(id=pid)
        for inst, child in (("port_id", pid), ("port_ag", pag), ("port_sg", psg)):
            self.add_child(inst, child, clk=self._clk, rst_n=self._rst_n, clk_en=self._clk_en, flush=self._flush)
        for sig in ("mux_sel", "restart", "finished", "iterators"):
            self.wire(pid.ports[sig], pag.ports[sig])
            self.wire(pid.ports[sig], psg.ports[sig])
        if self.rv:
            self.wire(pid.ports.extents_out, psg.ports.extents)
            self.wire(self.input("comparisons", self.n_comp), psg.ports.comparisons)
            step = self.var("id_step", 1)
            self.wire(step, psg.ports.step & self.input("valid", 1))
        else:
            step = psg.ports.step
        self.wire(pag.ports.step, step)
        self.wire(pid.ports.step, step)
        self.wire(self.output("step", 1), step)
        self.wire(self.output("addr_out", pag.get_address_width()), pag.ports.addr_out)
        self.wire(self.output("iterators", pid.get_extent_width(), size=self.dims, packed=True,
                              explicit_array=True), pid.ports.iterators)
        self.wire(self.output("finished", 1), pid.ports.finished)
        self.config_space_fixed = True
        self._assemble_cfg_memory_input()

    def gen_bitstream(self, dimensionality, extents, address_map, schedule_map):
        cfg = []
        for child, bs in ((self.pid, self.pid.gen_bitstream(dimensionality, extents, self.rv)),
                          (self.pag, self.pag.gen_bitstream(address_map, extents=extents, dimensionality=dimensionality)),
                          (self.psg, self.psg.gen_bitstream(schedule_map, extents=extents, dimensionality=dimensionality))):
            base = self.child_cfg_bases[child]
            cfg += [((u + base, l + base), v) for (u, l), v in bs]
        return cfg


def static_expected(cfg, cycles, check_iterators=True, ag_recurrence=True):
    """Per cycle: step (don't care during rst_n), finished, iterators and
    addr_out = affine(iterators) (recurrence AG: its reset value 0 until the
    first flush after a reset)."""
    dims, ew, sw, n, dim, extents, soff, sstr, aoff, astr = cfg
    aw = (n - 1).bit_length()
    loop = StaticScheduleLoop(dims, ew, dim, extents, soff, sstr)
    exp = []
    for c in cycles:
        was_init = loop.init
        it = list(loop.id.it) if c.get("rst_n", 1) else [0] * dims
        step, o = loop.cycle(c)
        live = (was_init and c.get("rst_n", 1)) or not ag_recurrence
        e = {"step": step, "finished": o["finished"],
             "addr_out": affine(aoff, astr, it, aw) if live else 0}
        if check_iterators:
            e["iterators"] = pack(o["iterators"], ew)
        exp.append(e)
    return exp


# (dims, ew, stride_width, num_addrs, dim, extents, sched offset, sched strides, addr offset, addr strides)
STATIC_CONFIGS = [
    (1, 4, 6, 16, 1, [5], 2, [2], 3, [2]),
    (3, 4, 8, 64, 3, [2, 3, 2], 4, [1, 3, 10], 5, [1, 2, 6]),
    (4, 5, 8, 256, 4, [3, 2, 2, 3], 0, [1, 4, 9, 20], 200, [-1, -3, 7, -50]),
    (6, 3, 10, 512, 6, [2, 3, 2, 2, 2, 2], 7, [1, 2, 6, 12, 24, 48], 17, [1, 2, 6, 12, 24, 48]),
]


def _sid(c):
    return f"D{c[0]}_w{c[1]}_dim{c[4]}_" + "x".join(map(str, c[5]))


def build(cfg, rv=False, n_comp=1, ag_recurrence=True):
    dims, ew, sw, n, dim, extents, soff, sstr, aoff, astr = cfg
    dut = PortControllers(dims, ew, sw, n, rv=rv, n_comp=n_comp, ag_recurrence=ag_recurrence)
    dut.gen_hardware()
    config = pack_config(dut.gen_bitstream(dim, extents, {"offset": aoff, "strides": astr},
                                           {"offset": soff, "strides": sstr}))
    assert dut.pag.get_address_width() == (n - 1).bit_length()
    return dut, config


@requires_xrun
@pytest.mark.parametrize("cfg", STATIC_CONFIGS, ids=_sid)
def test_static_port_controllers_rtl(cfg):
    dims, ew, sw, n, dim, extents, soff, sstr, aoff, astr = cfg
    dut, config = build(cfg)
    cycles = sg_stimulus(StaticScheduleLoop(dims, ew, dim, extents, soff, sstr), seed=dims)
    exp = static_expected(cfg, cycles)
    assert sum(1 for e in exp if e["step"]) >= 2 * len(iteration_sequence(extents))
    res = run_vectors(dut, cycles, exp, config=config)
    assert res.passed, res


def rv_vectors(cfg, n_comp, seed):
    """Random comparisons / valid / clk_en; flush in cycle 0 and mid-run; once
    finished: 6 cycles past the end, then reset + flush (1st time) or the end
    (2nd). The ID model is stepped by SG step & valid."""
    dims, ew, sw, n, dim, extents, soff, sstr, aoff, astr = cfg
    aw = (n - 1).bit_length()
    rng = random.Random(seed)
    m = IterationDomainModel(dims, ew, dim, extents)
    ins, exp, finishes, init = [], [], 0, False
    script = []             # scripted controls of the next cycles; "end" stops
    while script[:1] != ["end"]:
        k = len(ins)
        c = {"comparisons": sum(int(rng.random() < 0.8) << b for b in range(n_comp)),
             "valid": int(rng.random() < 0.75), "clk_en": int(rng.random() > 0.15),
             "flush": int(k in (0, 25))}
        if script:
            c.update(script.pop(0))
        if not c.get("rst_n", 1):
            m.reset()
            init = False
        step = int(not c["flush"] and not m.finished and c["valid"] and c["comparisons"] == (1 << n_comp) - 1)
        exp.append({"step": step, "finished": int(m.finished), "iterators": pack(m.it, ew),
                    "addr_out": affine(aoff, astr, m.it, aw) if init else 0})
        ins.append(c)
        if c.get("rst_n", 1):
            was = m.finished
            m.tick(step, c["flush"], c["clk_en"])
            init = init or bool(c["flush"])
            if m.finished and not was:
                finishes += 1
                script = [{}] * 6 + ([{"rst_n": 0}, {"flush": 1}] if finishes == 1 else ["end"])
        assert k < 3000
    return ins, exp


@requires_xrun
@pytest.mark.parametrize("cfg,n_comp", [(STATIC_CONFIGS[1], 2), (STATIC_CONFIGS[2], 1)],
                         ids=lambda x: _sid(x) if isinstance(x, tuple) else f"c{x}")
def test_rv_port_controllers_rtl(cfg, n_comp):
    dut, config = build(cfg, rv=True, n_comp=n_comp)
    ins, exp = rv_vectors(cfg, n_comp, seed=n_comp)
    res = run_vectors(dut, ins, exp, config=config)
    assert res.passed, res


# ID extent_width 3: largest extent gen_bitstream accepts is 2**3 = 8.
MAX_EXT_CFG = (2, 3, 8, 32, 2, [8, 2], 1, [1, 9], 0, [1, 8])


@requires_xrun
def test_recurrent_ag_at_max_encodable_extent():
    """Largest accepted extent (2**extent_width): recurrence AG / SG."""
    dims, ew, sw, n, dim, extents, soff, sstr, aoff, astr = MAX_EXT_CFG
    dut, config = build(MAX_EXT_CFG)
    cycles = sg_stimulus(StaticScheduleLoop(dims, ew, dim, extents, soff, sstr), seed=4)
    exp = static_expected(MAX_EXT_CFG, cycles, check_iterators=False)
    assert max(e["addr_out"] for e in exp) == 15
    res = run_vectors(dut, cycles, exp, config=config)
    assert res.passed, res


@requires_xrun
def test_nonrecurrent_ag_at_max_encodable_extent():
    """Regression: at extent 2**extent_width + 1 (formerly accepted) the last
    index wrapped to 0 and a recurrence=False AG emitted offset + 0; at the
    largest accepted extent every index fits the iterators."""
    dims, ew, sw, n, dim, extents, soff, sstr, aoff, astr = MAX_EXT_CFG
    dut, config = build(MAX_EXT_CFG, ag_recurrence=False)
    cycles = sg_stimulus(StaticScheduleLoop(dims, ew, dim, extents, soff, sstr), seed=4)
    exp = static_expected(MAX_EXT_CFG, cycles, check_iterators=False, ag_recurrence=False)
    res = run_vectors(dut, cycles, exp, config=config)
    assert res.passed, res
