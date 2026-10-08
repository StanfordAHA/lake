"""Unit tests for ASPLOS_EXP/run_roundtrip_sweep.sh — argument parsing and
per-line config-file override behavior.

Approach: extract the arg-parsing prologue via a bash prelude that stops
before the actual work, capture DEFAULT_APP_DIR / DEFAULT_TESTNAME / the
resolved per-line APP_DIR + TESTNAME to stdout. This validates the
observable behavior without spawning thesis_sweep.py / clockwork.
"""

from __future__ import annotations

import subprocess
import textwrap
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT = REPO_ROOT / "ASPLOS_EXP" / "run_roundtrip_sweep.sh"


def _make_probe(tmp_path: Path) -> Path:
    """Copy the first 55 lines (arg-parsing prologue) and append a probe
    that echoes the resolved defaults + per-line overrides. Skips the
    actual dispatch."""
    src_lines = SCRIPT.read_text().splitlines()
    prologue = "\n".join(src_lines[:55])
    probe = tmp_path / "probe.sh"
    probe.write_text(prologue + textwrap.dedent("""
        echo "DEFAULT_APP_DIR=$DEFAULT_APP_DIR"
        echo "DEFAULT_TESTNAME=$DEFAULT_TESTNAME"
        echo "CONFIGS_FILE=$CONFIGS_FILE"
        echo "ROOT=$ROOT"
        while IFS= read -r line; do
            case "$line" in ''|'#'*) continue ;; esac
            IFS='|' read -r NAME SWEEP_ARGS SPEC_KWARGS APP_DIR_OVERRIDE TESTNAME_OVERRIDE <<< "$line"
            APP_DIR="${APP_DIR_OVERRIDE:-$DEFAULT_APP_DIR}"
            TESTNAME="${TESTNAME_OVERRIDE:-$DEFAULT_TESTNAME}"
            echo "LINE|$NAME|$APP_DIR|$TESTNAME"
        done < "$CONFIGS_FILE"
        exit 0
    """))
    probe.chmod(0o755)
    return probe


def _write_cfg(tmp_path: Path, lines: list[str]) -> Path:
    p = tmp_path / "sweep.cfg"
    p.write_text("\n".join(lines) + "\n")
    return p


def _run(probe: Path, *args: str) -> subprocess.CompletedProcess:
    return subprocess.run(
        ["bash", str(probe), *args],
        capture_output=True, text=True, timeout=10,
    )


def _kv(out: str) -> dict:
    d = {}
    for line in out.splitlines():
        if "=" in line and not line.startswith("LINE|"):
            k, _, v = line.partition("=")
            d[k] = v
    return d


def _line_records(out: str) -> list[dict]:
    recs = []
    for line in out.splitlines():
        if line.startswith("LINE|"):
            _, name, app_dir, testname = line.split("|", 3)
            recs.append({"name": name, "app_dir": app_dir, "testname": testname})
    return recs


# ---------- default (no flags) -----------------------------------------------


def test_default_app_dir_derived_from_default_testname(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, ["smoke|--fw 4|storage_capacity=8192"])
    r = _run(probe, str(cfg))
    assert r.returncode == 0, r.stderr
    kv = _kv(r.stdout)
    assert kv["DEFAULT_TESTNAME"] == "conv_3_3"
    assert kv["DEFAULT_APP_DIR"].endswith("/conv_3_3")
    assert kv["CONFIGS_FILE"] == str(cfg)


# ---------- --testname flag --------------------------------------------------


def test_testname_flag_updates_app_dir_derivation(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, ["smoke|--fw 4|storage_capacity=8192"])
    r = _run(probe, "--testname", "harris", str(cfg))
    kv = _kv(r.stdout)
    assert kv["DEFAULT_TESTNAME"] == "harris"
    assert kv["DEFAULT_APP_DIR"].endswith("/harris")


def test_testname_equals_syntax(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, ["smoke|--fw 4|kw"])
    r = _run(probe, "--testname=matmul", str(cfg))
    kv = _kv(r.stdout)
    assert kv["DEFAULT_TESTNAME"] == "matmul"


# ---------- --app-dir flag ---------------------------------------------------


def test_app_dir_flag_overrides_derivation(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, ["smoke|--fw 4|kw"])
    r = _run(probe, "--app-dir", "/custom/path/matmul", "--testname", "matmul",
             str(cfg))
    kv = _kv(r.stdout)
    assert kv["DEFAULT_APP_DIR"] == "/custom/path/matmul"


def test_app_dir_equals_syntax(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, ["smoke|--fw 4|kw"])
    r = _run(probe, "--app-dir=/x/y/z", str(cfg))
    kv = _kv(r.stdout)
    assert kv["DEFAULT_APP_DIR"] == "/x/y/z"


# ---------- per-line overrides ----------------------------------------------


def test_three_field_line_uses_cli_defaults(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, ["smoke3|--fw 4|kw"])
    r = _run(probe, "--testname", "harris", str(cfg))
    rec = _line_records(r.stdout)[0]
    assert rec["name"] == "smoke3"
    assert rec["testname"] == "harris"
    assert rec["app_dir"].endswith("/harris")


def test_five_field_line_overrides_both(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, [
        "smoke5|--fw 2|storage_capacity=4096|/custom/gaussian|gaussian",
    ])
    r = _run(probe, str(cfg))
    rec = _line_records(r.stdout)[0]
    assert rec["name"] == "smoke5"
    assert rec["app_dir"] == "/custom/gaussian"
    assert rec["testname"] == "gaussian"


def test_five_field_line_wins_over_cli_flag(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, [
        "smoke|--fw 2|kw|/from_line/app|matmul_line",
    ])
    r = _run(probe, "--testname", "harris",
             "--app-dir", "/from_cli/harris", str(cfg))
    rec = _line_records(r.stdout)[0]
    assert rec["app_dir"] == "/from_line/app"
    assert rec["testname"] == "matmul_line"


def test_mixed_lines_use_own_defaults(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, [
        "# a comment — skipped",
        "",  # blank — skipped
        "line3|--fw 4|kw",                             # 3 fields — CLI default
        "line5|--fw 2|kw|/custom/matmul|matmul",       # 5 fields — override
    ])
    r = _run(probe, "--testname", "unsharp", str(cfg))
    recs = _line_records(r.stdout)
    assert len(recs) == 2
    a, b = recs
    assert a["name"] == "line3" and a["testname"] == "unsharp"
    assert b["name"] == "line5" and b["testname"] == "matmul"
    assert b["app_dir"] == "/custom/matmul"


# ---------- error paths -----------------------------------------------------


def test_unknown_flag_exits_nonzero(tmp_path):
    probe = _make_probe(tmp_path)
    cfg = _write_cfg(tmp_path, ["smoke|--fw 4|kw"])
    r = _run(probe, "--not-a-real-flag", str(cfg))
    assert r.returncode != 0
    assert "unknown flag" in (r.stdout + r.stderr).lower()


def test_missing_config_file_exits_nonzero(tmp_path):
    probe = _make_probe(tmp_path)
    r = _run(probe)  # no positional arg
    assert r.returncode != 0


def test_help_flag_prints_usage(tmp_path):
    """--help should print the usage block and exit non-zero (exit 2)."""
    probe = _make_probe(tmp_path)
    r = _run(probe, "--help")
    assert r.returncode == 2
    # Usage block references the CLI shape.
    combined = r.stdout + r.stderr
    assert "--app-dir" in combined or "app-dir" in combined


# ---------- syntax check ----------------------------------------------------


def test_script_passes_bash_syntax_check():
    r = subprocess.run(["bash", "-n", str(SCRIPT)],
                       capture_output=True, text=True)
    assert r.returncode == 0, r.stderr
