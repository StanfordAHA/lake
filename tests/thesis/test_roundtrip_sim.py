"""Unit tests for pd/thesis/clockwork-roundtrip-common/run_roundtrip_sim.py.

Focus on parse_util_txt — the pure-Python helper that slurps tb.sv's
utilization output. Other functions in that module (stage_cfg_dir,
run_make_sim) are integration-level and require a real VCS toolchain.
"""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
MODULE_PATH = (REPO_ROOT / "pd" / "thesis" / "clockwork-roundtrip-common"
               / "run_roundtrip_sim.py")


@pytest.fixture(scope="module")
def rrs():
    """Load run_roundtrip_sim.py by path — it isn't part of any package."""
    spec = importlib.util.spec_from_file_location("run_roundtrip_sim", MODULE_PATH)
    mod = importlib.util.module_from_spec(spec)
    sys.modules["run_roundtrip_sim"] = mod
    spec.loader.exec_module(mod)
    return mod


def _write_util(cfg_dir: Path, contents: str) -> None:
    (cfg_dir / "outputs").mkdir(parents=True, exist_ok=True)
    (cfg_dir / "outputs" / "util.txt").write_text(contents)


def test_parse_util_txt_happy_path(rrs, tmp_path):
    _write_util(tmp_path, "77 100\n")
    r = rrs.parse_util_txt(tmp_path)
    assert r == {"active_cycles": 77, "total_cycles": 100, "utilization": 0.77}


def test_parse_util_txt_missing_file_returns_empty(rrs, tmp_path):
    assert rrs.parse_util_txt(tmp_path) == {}


def test_parse_util_txt_missing_outputs_dir(rrs, tmp_path):
    # outputs/ doesn't exist at all
    assert rrs.parse_util_txt(tmp_path) == {}


def test_parse_util_txt_zero_total_no_division(rrs, tmp_path):
    _write_util(tmp_path, "0 0\n")
    r = rrs.parse_util_txt(tmp_path)
    assert r == {"active_cycles": 0, "total_cycles": 0}
    assert "utilization" not in r


@pytest.mark.parametrize("bad", [
    "",              # empty file
    "\n",            # just a newline
    "abc def",       # non-numeric
    "50",            # only one field
    "50 not_int",    # second field bad
])
def test_parse_util_txt_malformed_returns_empty(rrs, tmp_path, bad):
    _write_util(tmp_path, bad)
    assert rrs.parse_util_txt(tmp_path) == {}


def test_parse_util_txt_extra_whitespace_ok(rrs, tmp_path):
    _write_util(tmp_path, "   42     84   \n\n")
    r = rrs.parse_util_txt(tmp_path)
    assert r["active_cycles"] == 42
    assert r["total_cycles"] == 84
    assert r["utilization"] == 0.5


def test_parse_util_txt_extra_tokens_ignored(rrs, tmp_path):
    """Later fields (e.g. per-port breakdown someday) don't break the parser."""
    _write_util(tmp_path, "100 200 extra fields here\n")
    r = rrs.parse_util_txt(tmp_path)
    assert r["active_cycles"] == 100
    assert r["total_cycles"] == 200
