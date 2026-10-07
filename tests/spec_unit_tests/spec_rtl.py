"""Spec-level RTL harness for lake tests: run a ready-valid port program on a
whole generated lake Spec (e.g. build_spec_rv / build_pond_rv, physical=False)
in Xcelium, with the spec's remote storage modelled in the testbench.

The testbench is generated from the spec's top-level port list:
  * writers port_w<i> (RTL port_<i>) drive their stream index k (0..n-1) as
    data; a loopback writer (``loopback={"port_w1": "port_r0"}``) instead gets
    every output of that reader + ``loopback_add`` (a read-modify-write PE);
    dangling ports (no RTL port) are skipped;
  * readers port_r<j> (RTL port_<n_in + j>) record every output;
  * external memory ports: RW (addr/read_en/write_en), R, W and a clearing W
    port (``clear`` zeroes the whole storage); all delay 1 unless the spec
    says 0 (combinational read) — read-old on a same-cycle write;
  * stress: random valid/ready (25% low), writer 0 stalled for cycles
    500..1499 and reader 0 for cycles 2000..5999.
Reset: rst_n low 5 cycles with flush high, config loaded, flush released.
"""
import contextlib
import io
import os
import re
import subprocess
import tempfile

from rtl_harness import XRUN_SETUP


def _mp_delays(spec):
    delays = {}
    for mp in getattr(spec, "_memport_map", {}):
        try:
            delays[mp.name] = mp.get_port_delay()
        except AttributeError:
            pass
    return delays


class SpecSim:
    """Generate a spec's RTL + testbench once; run many programs on it."""

    def __init__(self, spec, workdir=None, top=None, max_data=1 << 16):
        self.spec = spec
        self.workdir = workdir or tempfile.mkdtemp(prefix="lake_spec_rtl_")
        os.makedirs(os.path.join(self.workdir, "rtl"), exist_ok=True)
        with contextlib.redirect_stdout(io.StringIO()):
            spec.generate_hardware()
            spec.get_verilog(output_dir=os.path.join(self.workdir, "rtl"))
        self.rtl_files = [os.path.join(self.workdir, "rtl", f)
                          for f in os.listdir(os.path.join(self.workdir, "rtl")) if f.endswith(".sv")]
        rtl = "".join(open(f).read() for f in self.rtl_files)
        tops = [m for m in re.findall(r"^module (\w+) \(", rtl, re.M) if m.startswith("lakespec")]
        self.top = top or tops[0]
        self.n_in, self.n_out = spec.get_num_in_ports(), spec.get_num_out_ports()
        head = rtl[rtl.index(f"module {self.top} ("):]
        head = head[:head.index(");")]
        self.ports = {n: (d, int(w) + 1 if w else 1)
                      for d, w, n in re.findall(r"(input|output) logic (?:\[(\d+):0\] )?(\w+)", head)}
        self.max_data = max_data
        self._emit_tb(_mp_delays(spec))
        self._compile()

    # lake port name -> RTL data port (None if dangling)
    def rtl_port(self, name):
        k = int(name[len("port_w"):]) if name.startswith("port_w") else self.n_in + int(name[len("port_r"):])
        return f"port_{k}" if f"port_{k}" in self.ports else None

    def _emit_tb(self, delays):
        P = self.ports
        mps = {}
        for n in P:
            m = re.match(r"memory_port_(\w+?)_(?:in|out)_(MemoryPort_\d+)$", n)
            if m:
                mps.setdefault(m.group(2), {})[m.group(1)] = n
        aw = max([P[s][1] for mp in mps.values() for r, s in mp.items() if "addr" in r] + [1])
        dw = max([P[s][1] for mp in mps.values() for r, s in mp.items() if "data" in r] + [16])
        writers = [i for i in range(self.n_in) if f"port_{i}" in P]
        readers = [j for j in range(self.n_out) if f"port_{self.n_in + j}" in P]
        L = ["`timescale 1ns/1ps", "module spec_tb;", "  logic clk = 1'b0;", "  always #5 clk = ~clk;",
             f"  logic [{dw - 1}:0] mem [0:{(1 << aw) - 1}];",
             "  string test_dir; integer cyc = 0, stress = 0, max_time = 100000, fd, lb_add = 1;",
             "  integer sw_lo = 500, sw_hi = 1500, sr_lo = 2000, sr_hi = 6000;",
             "  logic running = 1'b0;"]
        ports = dict(P)
        for n in ("rst_n", "flush"):
            ports.setdefault(n, ("input", 1))
        for n, (d, w) in ports.items():
            if n != "clk":
                L.append(f"  logic [{w - 1}:0] {n};")
        L.append(f"  {self.top} dut(" + ", ".join(f".{n}({n})" for n in P) + ");")
        comb_driven = set()
        for mp, sig in mps.items():
            d = delays.get(mp, 1)
            if "addr" in sig:
                L.append(f"  always @(posedge clk) begin if ({sig['write_en']}) mem[{sig['addr']}] <= {sig['write_data']}; "
                         f"else if ({sig['read_en']}) {sig['read_data']} <= mem[{sig['addr']}]; end")
            if "read_addr" in sig and d == 0:
                L.append(f"  assign {sig['read_data']} = {sig['read_en']} ? mem[{sig['read_addr']}] : '0;")
                comb_driven.add(sig["read_data"])
            elif "read_addr" in sig:
                L.append(f"  always @(posedge clk) if ({sig['read_en']}) {sig['read_data']} <= mem[{sig['read_addr']}];")
            if "write_addr" in sig and "clear" in sig:
                L.append(f"  always @(posedge clk) begin if ({sig['clear']}) begin for (int a = 0; a < {1 << aw}; a++) mem[a] <= '0; end "
                         f"else if ({sig['write_en']}) mem[{sig['write_addr']}] <= {sig['write_data']}; end")
            elif "write_addr" in sig:
                L.append(f"  always @(posedge clk) if ({sig['write_en']}) mem[{sig['write_addr']}] <= {sig['write_data']};")
        for i in writers:
            L += [f"  integer w{i}_n = 0, w{i}_idx = 0, lb_w{i} = -1;", f"  logic [15:0] lbq{i}[$];"]
        for j in readers:
            L += [f"  integer r{j}_n = 0, r{j}_idx = 0, fd_r{j};"]
        L += ["  initial begin",
              '    if (!$value$plusargs("TEST_DIR=%s", test_dir)) $fatal(1, "no +TEST_DIR");',
              '    void\'($value$plusargs("stress=%d", stress)); void\'($value$plusargs("max_time=%d", max_time));',
              '    void\'($value$plusargs("lb_add=%d", lb_add));',
              '    void\'($value$plusargs("sw_lo=%d", sw_lo)); void\'($value$plusargs("sw_hi=%d", sw_hi));',
              '    void\'($value$plusargs("sr_lo=%d", sr_lo)); void\'($value$plusargs("sr_hi=%d", sr_hi));']
        for i in writers:
            L.append(f'    void\'($value$plusargs("w{i}_n=%d", w{i}_n)); void\'($value$plusargs("lb_w{i}=%d", lb_w{i}));')
        for j in readers:
            L.append(f'    void\'($value$plusargs("r{j}_n=%d", r{j}_n)); '
                     f'fd_r{j} = $fopen({{test_dir, "/port_r{j}.txt"}}, "w");')
        L.append('    fd = $fopen({test_dir, "/bitstream.hex"}, "r"); void\'($fscanf(fd, "%h", config_memory)); $fclose(fd);')
        L.append("    rst_n = 1'b0; flush = 1'b1;")
        for n, v in (("config_memory_wen", "1'b0"), ("clk_en", "1'b1")):
            if n in P:
                L.append(f"    {n} = {v};")
        for i in writers:
            L.append(f"    port_{i} = '0; port_{i}_valid = 1'b0;")
        for j in readers:
            L.append(f"    port_{self.n_in + j}_ready = 1'b0;")
        for sig in mps.values():
            for r, s in sig.items():
                if P[s][0] == "input" and s not in comb_driven:
                    L.append(f"    {s} = '0;")
        L.append("    repeat (5) @(posedge clk); rst_n <= 1'b1; repeat (5) @(posedge clk);")
        if "config_memory_wen" in P:
            L.append("    config_memory_wen <= 1'b1; @(posedge clk); config_memory_wen <= 1'b0;")
        L += ["    repeat (5) @(posedge clk); flush <= 1'b0; running <= 1'b1;", "  end",
              "  function automatic logic gate(input integer lo, input integer hi);",
              "    gate = (stress == 0) || (($urandom_range(0, 3) != 0) && !(cyc >= lo && cyc < hi));",
              "  endfunction",
              "  always @(posedge clk) if (running) begin", "    cyc++;"]
        for j in readers:
            p = f"port_{self.n_in + j}"
            L.append(f"    if ({p}_valid && {p}_ready) begin")
            L.append(f"      $fdisplay(fd_r{j}, \"%h\", {p}[15:0]); r{j}_idx++;")
            for i in writers:
                L.append(f"      if (lb_w{i} == {j}) lbq{i}.push_back({p}[15:0] + lb_add[15:0]);")
            L.append("    end")
            L.append(f"    {p}_ready <= gate({'sr_lo' if j == 0 else -1}, {'sr_hi' if j == 0 else -1});")
        for i in writers:
            p = f"port_{i}"
            L.append(f"    if ({p}_valid && {p}_ready) begin w{i}_idx++; if (lb_w{i} >= 0) void'(lbq{i}.pop_front()); end")
            L.append(f"    if ((lb_w{i} >= 0 ? (lbq{i}.size() > 0) : 1'b1) && w{i}_idx < w{i}_n && "
                     f"gate({'sw_lo' if i == 0 else -1}, {'sw_hi' if i == 0 else -1})) begin")
            L.append(f"      {p}_valid <= 1'b1; {p} <= {{1'b0, (lb_w{i} >= 0) ? lbq{i}[0] : w{i}_idx[15:0]}};")
            L.append(f"    end else {p}_valid <= 1'b0;")
        done = " && ".join([f"(r{j}_idx >= r{j}_n)" for j in readers] or ["1'b1"])
        L.append(f"    if (({done}) || cyc >= max_time) begin")
        for j in readers:
            L.append(f"      $fclose(fd_r{j});")
        L.append(f"      if ({done}) $display(\"SPEC_RESULT DONE cycles=%0d\", cyc); else $display(\"SPEC_RESULT TIMEOUT cycles=%0d\", cyc);")
        L += ["      $finish;", "    end", "  end", "endmodule"]
        with open(os.path.join(self.workdir, "spec_tb.sv"), "w") as f:
            f.write("\n".join(L) + "\n")

    def _xrun(self, args, log):
        cmd = (XRUN_SETUP + " && " if XRUN_SETUP else "") + f"cd {self.workdir} && xrun {args} -l {log} > /dev/null 2>&1"
        subprocess.run(["bash", "-c", cmd])
        return open(os.path.join(self.workdir, log)).read() if os.path.exists(os.path.join(self.workdir, log)) else ""

    def _compile(self):
        files = " ".join(os.path.relpath(f, self.workdir) for f in self.rtl_files)
        log = self._xrun(f"-sv -64bit -elaborate -top spec_tb spec_tb.sv {files}", "compile.log")
        if re.search(r"\*[EF],", log):
            raise RuntimeError("spec tb compile failed:\n" + "\n".join(l for l in log.splitlines() if "*E" in l or "*F" in l))

    def run(self, bitstream, sizes, loopback=None, stress=False, seed=1, max_time=None, name="t", lb_add=1,
            writer_stall=(500, 1500), reader_stall=(2000, 6000)):
        """sizes: {lake port name: items}; loopback: {writer: reader};
        under stress writer 0 / reader 0 are stalled for the given cycle windows.
        Returns ({reader: [values or None]}, finished, cycles)."""
        tdir = os.path.join(self.workdir, name)
        os.makedirs(tdir, exist_ok=True)
        with open(os.path.join(tdir, "bitstream.hex"), "w") as f:
            f.write(format(bitstream, "x") + "\n")
        args = [f"+TEST_DIR={tdir}", f"+stress={int(stress)}", f"+lb_add={lb_add}",
                f"+sw_lo={writer_stall[0]}", f"+sw_hi={writer_stall[1]}",
                f"+sr_lo={reader_stall[0]}", f"+sr_hi={reader_stall[1]}",
                f"+max_time={max_time or 12 * sum(sizes.values()) + 20000}"]
        for p, n in sizes.items():
            if self.rtl_port(p) is not None:
                args.append(f"+{p.replace('port_', '')}_n={n}")
        for w, r in (loopback or {}).items():
            args.append(f"+lb_{w.replace('port_', '')}={int(r.replace('port_r', ''))}")
        log = self._xrun(f"-R -64bit -svseed {seed} " + " ".join(args), f"{name}.log")
        m = re.search(r"SPEC_RESULT (DONE|TIMEOUT) cycles=(\d+)", log)
        outs = {}
        for p in sizes:
            if p.startswith("port_r"):
                path = os.path.join(tdir, f"{p}.txt")
                vals = []
                if os.path.exists(path):
                    for line in open(path):
                        line = line.strip()
                        if line:
                            vals.append(None if any(c in line.lower() for c in "xz") else int(line, 16))
                outs[p] = vals
        return outs, bool(m and m.group(1) == "DONE"), int(m.group(2)) if m else None
