"""Port iteration order must not depend on the process's string-hash seed.

kratos' Generator.get_port_names() comes back as a Python set, so iterating it
directly follows PYTHONHASHSEED (random per process). extract_top_config()
order decides the order of garnet's read-only (pass-through) config registers
on the core's read_config_data mux, so before the fix garnet.v's PE readback
mux took reduce_pe_cluster O2/O3/O4 in a different order on every run, and RTL
(garnet.py --verilog) and bitstream (aha pnr) came from different processes.
"""
import json
import os
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]

PROBE = r"""
import json
import kratos as kts
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.top.extract_tile_info import extract_top_config, get_interface

g = kts.Generator("probe")
for n in ["mode", "O4", "O2", "O3", "enable", "stride_0", "ro_status", "zeta"]:
    p = g.output(n, 4) if n.startswith(("O", "ro_")) else g.input(n, 4)
    p.add_attribute(ConfigRegAttr(n, read_only=n.startswith(("O", "ro_"))))
for n in ["data_in_1", "data_in_0", "valid_out", "ready_in"]:
    p = g.input(n, 16)
    p.add_attribute(ControlSignalAttr(is_control=False))
print(json.dumps({"cfg": [c.port_name for c in extract_top_config(g)],
                  "ro": [c.port_name for c in extract_top_config(g) if c.read_only],
                  "intf": [i.port_name for i in get_interface(g)]}))
"""


def probe(seed):
    env = dict(os.environ, PYTHONHASHSEED=str(seed),
               PYTHONPATH=os.pathsep.join([str(ROOT)] + [p for p in os.environ.get("PYTHONPATH", "").split(os.pathsep) if p]))
    out = subprocess.run([sys.executable, "-c", PROBE], env=env, cwd=ROOT, check=True,
                         capture_output=True, text=True).stdout
    return json.loads(out.strip().splitlines()[-1])


def test_extract_order_is_seed_independent():
    runs = [probe(seed) for seed in (0, 1, 2, 3, 4)]
    assert all(r == runs[0] for r in runs), runs
    first = runs[0]
    assert first["cfg"] == sorted(first["cfg"])
    assert first["ro"] == ["O2", "O3", "O4", "ro_status"]
    assert first["intf"] == sorted(first["intf"])
