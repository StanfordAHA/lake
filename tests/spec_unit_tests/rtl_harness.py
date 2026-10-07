"""Cycle-vector RTL harness for lake unit tests.

A test builds a kratos generator (a lake spec Component or module), a list of
per-cycle input values, and the expected output values of each cycle (from a
Python reference model of the unit's function). ``run_vectors`` emits the
generator to Verilog, writes a SystemVerilog testbench and runs it on xrun:

  * reset: rst_n low for ``reset_cycles`` posedges (flush 0, clk_en 1,
    config_memory = ``config``); inputs not driven by a test hold 0.
  * cycle i: inputs[i] are applied at the negedge; outputs are sampled 1 ns
    before the next posedge (after combinational settling) and compared
    against expect[i]; that posedge ends cycle i.
  * flush / clk_en / rst_n stay at 0 / 1 / 1 after reset unless some cycle of
    ``inputs`` names them; then they are driven per cycle like any other input
    (cycles that do not name them get 0 / 1 / 1). rst_n is asynchronous, so a
    cycle with rst_n = 0 shows reset values at its sample point.

Values are plain ints; a packed-array port is one flat vector (element 0 in the
LSBs, see ``pack``). An expectation of None (or a missing key) is a don't-care.
Tests are skipped when xrun is unavailable.
"""
import os
import re
import shutil
import subprocess
import tempfile

import kratos as kts
import pytest

XRUN_SETUP = os.environ.get(
    "LAKE_XRUN_SETUP",
    "source /cad/modules/tcl/init/bash && module load base xcelium" if os.path.isdir("/cad/modules") else "")


def xrun_available():
    cmd = (XRUN_SETUP + " && " if XRUN_SETUP else "") + "command -v xrun"
    return subprocess.run(["bash", "-c", cmd], capture_output=True).returncode == 0


requires_xrun = pytest.mark.skipif(not xrun_available(), reason="xrun (Xcelium) not available")


def pack(values, elem_width):
    """Pack a list (element 0 first) into one int, element 0 in the LSBs."""
    out = 0
    for i, v in enumerate(values):
        out |= (int(v) & ((1 << elem_width) - 1)) << (i * elem_width)
    return out


def unpack(value, elem_width, n):
    return [(value >> (i * elem_width)) & ((1 << elem_width) - 1) for i in range(n)]


def pack_config(configuration):
    """[((upper, lower), value), ...] (Component.gen_bitstream) -> int."""
    cfg = 0
    for (upper, lower), value in configuration:
        cfg |= (int(value) & ((1 << (upper - lower + 1)) - 1)) << lower
    return cfg


def _dims_width(dims):
    w = 1
    for hi, lo in re.findall(r"\[(\d+):(\d+)\]", dims or ""):
        w *= int(hi) - int(lo) + 1
    return w


def module_ports(verilog, top):
    """{name: (direction, total_bits)} of module ``top`` in ``verilog``."""
    start = verilog.index(f"module {top} (")
    header = verilog[start:verilog.index(");", start)]
    ports = {}
    for d, dims, name in re.findall(r"(input|output)\s+logic\s*((?:\[\d+:\d+\]\s*)*)(\w+)", header):
        ports[name] = (d, _dims_width(dims))
    return ports


def _hex(v, bits):
    return format(int(v) & ((1 << bits) - 1), "x")


class SimResult:
    def __init__(self, passed, mismatches, log, workdir):
        self.passed = passed
        self.mismatches = mismatches
        self.log = log
        self.workdir = workdir

    def __repr__(self):
        return f"SimResult(passed={self.passed}, mismatches={self.mismatches[:10]}, workdir={self.workdir})"


def run_vectors(gen, inputs, expect, config=None, reset_cycles=2, workdir=None, keep=False):
    """Simulate ``gen`` cycle by cycle; see the module docstring."""
    assert len(inputs) == len(expect)
    top = gen.name
    own = workdir is None
    workdir = workdir or tempfile.mkdtemp(prefix=f"lake_unit_{top}_")
    os.makedirs(workdir, exist_ok=True)
    dut = os.path.join(workdir, "dut.sv")
    kts.verilog(gen, filename=dut)
    ports = module_ports(open(dut).read(), top)
    n = len(inputs)
    ins = [p for p, (d, _) in ports.items() if d == "input"]
    outs = [p for p, (d, _) in ports.items() if d == "output"]
    fixed = {"clk", "rst_n", "flush", "clk_en", "config_memory"}
    for cyc in inputs:
        bad = set(cyc) - set(ins)
        assert not bad, f"not input ports of {top}: {bad}"
    for cyc in expect:
        bad = set(cyc) - set(outs)
        assert not bad, f"not output ports of {top}: {bad}"

    # Control inputs a test drives per cycle (default when a cycle omits them).
    ctrl_default = {"flush": 0, "clk_en": 1, "rst_n": 1}
    ctrl = [p for p in ctrl_default if p in ports and any(p in c for c in inputs)]
    driven = [p for p in ins if p not in fixed] + ctrl
    for p in driven:
        with open(os.path.join(workdir, f"in_{p}.hex"), "w") as f:
            f.write("\n".join(_hex(c.get(p, ctrl_default.get(p, 0)), ports[p][1]) for c in inputs) + "\n")
    checked = [p for p in outs if any(c.get(p) is not None for c in expect)]
    for p in checked:
        with open(os.path.join(workdir, f"exp_{p}.hex"), "w") as f:
            f.write("\n".join(_hex(c.get(p) or 0, ports[p][1]) for c in expect) + "\n")
        with open(os.path.join(workdir, f"msk_{p}.hex"), "w") as f:
            f.write("\n".join("1" if c.get(p) is not None else "0" for c in expect) + "\n")

    sv = ["`timescale 1ns/1ps", "module tb;", "  logic clk = 1'b0;", "  always #5 clk = ~clk;",
          f"  integer errors = 0;", f"  localparam N = {n};"]
    for p, (d, w) in ports.items():
        if p != "clk":
            sv.append(f"  logic [{w - 1}:0] {p};")
    for p in driven:
        sv.append(f"  logic [{ports[p][1] - 1}:0] in_{p} [0:N-1];")
    for p in checked:
        sv.append(f"  logic [{ports[p][1] - 1}:0] exp_{p} [0:N-1];")
        sv.append(f"  logic msk_{p} [0:N-1];")
    sv.append(f"  {top} dut(" + ", ".join(f".{p}({p})" for p in ports) + ");")
    sv.append("  initial begin")
    for p in driven:
        sv.append(f'    $readmemh("{workdir}/in_{p}.hex", in_{p});')
    for p in checked:
        sv.append(f'    $readmemh("{workdir}/exp_{p}.hex", exp_{p});')
        sv.append(f'    $readmemb("{workdir}/msk_{p}.hex", msk_{p});')
    if "rst_n" in ports:
        sv.append("    rst_n = 1'b0;")
    if "flush" in ports:
        sv.append("    flush = 1'b0;")
    if "clk_en" in ports:
        sv.append("    clk_en = 1'b1;")
    if "config_memory" in ports:
        sv.append(f"    config_memory = {ports['config_memory'][1]}'h{_hex(config or 0, ports['config_memory'][1])};")
    for p in driven:
        if p not in ctrl:
            sv.append(f"    {p} = '0;")
    sv.append(f"    repeat ({reset_cycles}) @(posedge clk);")
    sv.append("    @(negedge clk);")
    if "rst_n" in ports:
        sv.append("    rst_n = 1'b1;")
    sv.append("    for (int i = 0; i < N; i++) begin")
    for p in driven:
        sv.append(f"      {p} = in_{p}[i];")
    sv.append("      #4;")
    for p in checked:
        sv.append(f"      if (msk_{p}[i] && ({p} !== exp_{p}[i])) begin")
        sv.append(f'        if (errors < 20) $display("MISMATCH cycle %0d {p} got %h expected %h", i, {p}, exp_{p}[i]);')
        sv.append("        errors++;")
        sv.append("      end")
    sv.append("      @(negedge clk);")
    sv.append("    end")
    sv.append('    $display("UNIT_RESULT %s errors=%0d cycles=%0d", errors == 0 ? "PASS" : "FAIL", errors, N);')
    sv.append("    $finish;")
    sv.append("  end")
    sv.append("endmodule")
    with open(os.path.join(workdir, "tb.sv"), "w") as f:
        f.write("\n".join(sv) + "\n")

    cmd = (XRUN_SETUP + " && " if XRUN_SETUP else "") + \
        f"cd {workdir} && xrun -sv -64bit -q -top tb tb.sv dut.sv -l xrun.log > /dev/null 2>&1"
    subprocess.run(["bash", "-c", cmd])
    log = open(os.path.join(workdir, "xrun.log")).read() if os.path.exists(os.path.join(workdir, "xrun.log")) else ""
    m = re.search(r"UNIT_RESULT (PASS|FAIL) errors=(\d+)", log)
    mismatches = re.findall(r"MISMATCH cycle (\d+) (\w+) got (\w+) expected (\w+)", log)
    passed = m is not None and m.group(1) == "PASS"
    if m is None:
        mismatches = [("sim", "did not finish", "", "\n".join(log.splitlines()[-15:]))]
    if passed and own and not keep:
        shutil.rmtree(workdir, ignore_errors=True)
    return SimResult(passed, mismatches, log, workdir)
