"""Opt-in Spec power options (2026-10-14), both off by default.

share_clk_ctr: every static ScheduleGenerator (the ports' and the SIPO/PISO
ones inside vectorized ports) reads one shared cycle counter instead of a
clk_ctr each. The counter is as wide as the widest SG and each SG takes its
low bits, which equal its own counter (all clear on flush and count every
clk_en cycle). RV specs have no free-running SG, so it changes nothing there.

clock_gate_ports: each port's ID gets a `port_enable` config bit, set by
IterationDomain.gen_bitstream (i.e. for every port the application
configures), and the port's logic runs on clk gated by it.

RTL equivalence (gold sims, the spec unit tests with each option forced on,
garnet tile power tests) is recorded in lake CLAUDE.md §5.6.
"""
import contextlib
import io
import os
import re
import subprocess
import sys
from pathlib import Path

from lake.spec.spec_memory_controller import build_spec, build_spec_rv

ROOT = Path(__file__).resolve().parents[2]

KW = dict(storage_capacity=4096, data_width=16, vec_width=4, in_ports=2, out_ports=2,
          vec_capacity=2, physical=False)


def built(builder, **kw):
    with contextlib.redirect_stdout(io.StringIO()):
        spec = builder(**dict(KW, **kw))
        spec.generate_hardware()
    return spec


def rtl(spec, out_dir):
    out_dir.mkdir(parents=True, exist_ok=True)
    with contextlib.redirect_stdout(io.StringIO()):
        spec.get_verilog(output_dir=str(out_dir))
    return next(out_dir.glob("*.sv")).read_text()


def static_sgs(spec):
    """Every static SG: each port's, plus the SIPO/PISO SG inside vectorized ports."""
    sgs = []
    for port in spec.get_in_ports() + spec.get_out_ports():
        sgs.append(spec.get_port_controllers(port)[2])
        for attr in ("_sg_sipo_in", "_sg_piso_out"):
            if hasattr(port, attr):
                sgs.append(getattr(port, attr))
    return sgs


def test_share_clk_ctr_static(tmp_path):
    base = rtl(built(build_spec), tmp_path / "base")
    spec = built(build_spec, share_clk_ctr=True)
    shared = rtl(spec, tmp_path / "shared")
    sgs = static_sgs(spec)
    assert len(sgs) == 8 and all(sg.shared_clk_ctr for sg in sgs)
    width = max(sg.total_cycle_width for sg in sgs)
    assert re.search(r"\bclk_ctr <= ", base)
    assert not re.search(r"(?<!shared_)\bclk_ctr <= ", shared)
    assert len(re.findall(r"shared_clk_ctr <= shared_clk_ctr \+", shared)) == 1
    assert f"logic [{width - 1}:0] shared_clk_ctr;" in shared


RV_PROBE = r"""
import contextlib, hashlib, io, pathlib, sys, tempfile
from lake.spec.spec_memory_controller import build_spec_rv
out = pathlib.Path(tempfile.mkdtemp())
with contextlib.redirect_stdout(io.StringIO()), contextlib.redirect_stderr(io.StringIO()):
    spec = build_spec_rv(**dict(KW, share_clk_ctr=SHARE))
    spec.generate_hardware()
    spec.get_verilog(output_dir=str(out))
print(hashlib.sha256(next(out.glob("*.sv")).read_bytes()).hexdigest())
"""


def rv_rtl_hash(share):
    # One process per build: two builds in one process differ anyway (kratos
    # numbers generated module names process-wide).
    env = dict(os.environ, PYTHONHASHSEED="0",
               PYTHONPATH=os.pathsep.join([str(ROOT)] + [p for p in os.environ.get("PYTHONPATH", "").split(os.pathsep) if p]))
    src = RV_PROBE.replace("KW", repr(KW)).replace("SHARE", repr(share))
    out = subprocess.run([sys.executable, "-c", src], env=env, cwd=ROOT, check=True,
                         capture_output=True, text=True).stdout
    return out.strip().splitlines()[-1]


def test_share_clk_ctr_is_a_noop_on_rv():
    assert rv_rtl_hash(True) == rv_rtl_hash(False)


def test_clock_gate_ports_enable_bit():
    for builder in (build_spec, build_spec_rv):
        spec = built(builder, clock_gate_ports=True)
        for port in spec.get_in_ports() + spec.get_out_ports():
            port_id = spec.get_port_controllers(port)[0]
            assert port_id.has_port_enable
            field = port_id.get_cfg_map()["port_enable"]
            assert (field, 1) in port_id.gen_bitstream(2, [4, 4])
        plain = built(builder)
        assert not plain.get_port_controllers(plain.get_in_ports()[0])[0].has_port_enable
