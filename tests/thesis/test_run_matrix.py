"""Unit tests for THESIS/apps/run_matrix.py — the (design x app) cell driver."""

from __future__ import annotations

import json
import os
import stat
from pathlib import Path

import pytest

from THESIS.apps import run_matrix as rm
from THESIS.apps.design_points import DESIGNS_SINGLE_LEVEL, DesignPoint
from THESIS.apps.registry import APPS, AppSpec


# ---------- fixtures --------------------------------------------------------


@pytest.fixture
def sp_design() -> DesignPoint:
    """A single-port design (dual_port=False)."""
    return next(d for d in DESIGNS_SINGLE_LEVEL if not d.dual_port)


@pytest.fixture
def dp_design() -> DesignPoint:
    """A dual-port design (dual_port=True)."""
    return next(d for d in DESIGNS_SINGLE_LEVEL if d.dual_port)


@pytest.fixture
def any_app() -> AppSpec:
    return APPS[0]


@pytest.fixture
def results_root(tmp_path, monkeypatch):
    """Point run_matrix.RESULTS_ROOT at a tmp dir for the duration of the test."""
    root = tmp_path / "results"
    monkeypatch.setattr(rm, "RESULTS_ROOT", root)
    return root


def _write_fake_sweep_script(tmp_path: Path, *, tiles: list[tuple[int, int]] | None,
                             verdict: str = "PASS", exit_code: int = 0) -> Path:
    """Write a bash script that mimics run_roundtrip_sweep.sh's outputs.

    ``tiles`` is a list of ``(active_cycles, total_cycles)`` — one util.txt
    per tile. Pass ``None`` to skip tile output entirely (e.g. compile-phase
    failure).
    """
    lines = [
        "#!/usr/bin/env bash",
        'CFG="$1"; ROOT="$2"',
        "mkdir -p \"$ROOT\"",
        'NAME=$(head -1 "$CFG" | cut -d"|" -f1)',
    ]
    if tiles is not None:
        for i, (a, t) in enumerate(tiles):
            lines += [
                f'mkdir -p "$ROOT/$NAME/cfg_{i}_sim/outputs"',
                f'echo "{a} {t}" > "$ROOT/$NAME/cfg_{i}_sim/outputs/util.txt"',
            ]
    lines += [
        'echo "=== $NAME ===" > "$ROOT/summary.txt"',
        f'echo "$NAME: {verdict}" >> "$ROOT/summary.txt"',
        f'exit {exit_code}',
    ]
    script = tmp_path / "fake_sweep.sh"
    script.write_text("\n".join(lines) + "\n")
    script.chmod(script.stat().st_mode | stat.S_IEXEC)
    return script


# ---------- sweep-args / spec-kwargs / app-dir composition ------------------


def test_sweep_args_sp_has_no_dual_port_flag(sp_design):
    s = rm._sweep_args_from_design(sp_design)
    assert f"--fetch_width {sp_design.fetch_width}" in s
    assert f"--data_width {sp_design.data_width}" in s
    assert f"--storage_capacity {sp_design.storage_cap_bytes}" in s
    assert f"--in_ports {sp_design.in_ports}" in s
    assert f"--out_ports {sp_design.out_ports}" in s
    assert "--dual_port" not in s


def test_sweep_args_dp_appends_dual_port_flag(dp_design):
    assert "--dual_port" in rm._sweep_args_from_design(dp_design)


def test_spec_kwargs_carries_dual_port_bool(sp_design, dp_design):
    assert "dual_port=False" in rm._spec_kwargs_from_design(sp_design)
    assert "dual_port=True" in rm._spec_kwargs_from_design(dp_design)


def test_spec_kwargs_is_python_dict_body(sp_design):
    body = rm._spec_kwargs_from_design(sp_design)
    ns: dict = {}
    exec(f"d = dict({body})", ns)
    d = ns["d"]
    assert d["storage_capacity"] == sp_design.storage_cap_bytes
    assert d["fetch_width"] == sp_design.fetch_width
    assert d["dual_port"] is False


def test_app_dir_from_spec_joins_aha_root(any_app):
    path = rm._app_dir_from_spec(any_app, "/aha")
    assert path.startswith("/aha/Halide-to-Hardware/apps/hardware_benchmarks/")
    assert path.endswith(any_app.halide_app_dir.lstrip("/"))


def test_app_dir_from_spec_handles_trailing_slash(any_app):
    a = rm._app_dir_from_spec(any_app, "/aha/")
    b = rm._app_dir_from_spec(any_app, "/aha")
    assert a == b


def test_compose_sweep_line_has_five_pipe_fields(sp_design, any_app):
    line = rm.compose_sweep_line(sp_design, any_app, "/aha")
    parts = line.split("|")
    assert len(parts) == 5
    name, sw, kw, ad, tn = parts
    assert name == sp_design.id
    assert "--fetch_width" in sw and "storage_capacity=" in kw
    assert ad.startswith("/aha/") and tn == any_app.testname


# ---------- summary.txt parsing --------------------------------------------


def test_parse_sim_status_missing_file():
    assert rm._parse_sim_status(Path("/no/such/path.txt"), "x") == "MISSING"


def test_parse_sim_status_accepts_str_and_path(tmp_path):
    p = tmp_path / "summary.txt"
    p.write_text("foo: PASS\n")
    assert rm._parse_sim_status(p, "foo") == "PASS"
    assert rm._parse_sim_status(str(p), "foo") == "PASS"


def test_parse_sim_status_finds_matching_row(tmp_path):
    p = tmp_path / "summary.txt"
    p.write_text("=== foo ===\n  4 tile(s)\nfoo: PASS\nbar: FAIL\nbaz: FAIL_compile\n")
    assert rm._parse_sim_status(p, "foo") == "PASS"
    assert rm._parse_sim_status(p, "bar") == "FAIL"
    assert rm._parse_sim_status(p, "baz") == "FAIL_compile"


def test_parse_sim_status_unknown_when_name_absent(tmp_path):
    p = tmp_path / "summary.txt"
    p.write_text("other: PASS\n")
    assert rm._parse_sim_status(p, "not_present") == "UNKNOWN"


def test_parse_sim_status_ignores_leading_context_lines(tmp_path):
    p = tmp_path / "summary.txt"
    p.write_text("=== port_baseline (app=matmul_hw) ===\n"
                 "  tile 0: PASS\n"
                 "  tile 1: PASS\n"
                 "port_baseline: PASS\n")
    assert rm._parse_sim_status(p, "port_baseline") == "PASS"


# ---------- util.txt reader + aggregator -----------------------------------


def test_read_util_txt_happy_path(tmp_path):
    p = tmp_path / "util.txt"
    p.write_text("77 100\n")
    u = rm._read_util_txt(p)
    assert u == {"active_cycles": 77, "total_cycles": 100, "utilization": 0.77}


def test_read_util_txt_missing_file(tmp_path):
    assert rm._read_util_txt(tmp_path / "nope.txt") == {}


def test_read_util_txt_no_divide_by_zero(tmp_path):
    p = tmp_path / "util.txt"
    p.write_text("0 0\n")
    u = rm._read_util_txt(p)
    assert u == {"active_cycles": 0, "total_cycles": 0}
    assert "utilization" not in u


@pytest.mark.parametrize("bad", ["", "not numbers", "50", "50 abc"])
def test_read_util_txt_malformed_returns_empty(tmp_path, bad):
    p = tmp_path / "util.txt"
    p.write_text(bad)
    assert rm._read_util_txt(p) == {}


def test_aggregate_util_empty_run_dir(tmp_path):
    agg = rm._aggregate_util(tmp_path)
    assert agg == {"tile_count": 0, "per_tile_util": []}


def test_aggregate_util_sums_across_tiles(tmp_path):
    for i, (a, t) in enumerate([(60, 100), (75, 100), (40, 100)]):
        d = tmp_path / f"cfg_{i}_sim" / "outputs"
        d.mkdir(parents=True)
        (d / "util.txt").write_text(f"{a} {t}\n")
    agg = rm._aggregate_util(tmp_path)
    assert agg["tile_count"] == 3
    assert agg["active_cycles"] == 175
    assert agg["total_cycles"] == 300
    assert agg["utilization"] == pytest.approx(175 / 300)
    assert len(agg["per_tile_util"]) == 3
    assert agg["per_tile_util"][0]["tile"] == "cfg_0_sim"


def test_aggregate_util_ignores_tiles_missing_util_txt(tmp_path):
    """cfg_0 has util, cfg_1 doesn't. Aggregation should skip cfg_1."""
    (tmp_path / "cfg_0_sim" / "outputs").mkdir(parents=True)
    (tmp_path / "cfg_0_sim" / "outputs" / "util.txt").write_text("50 100\n")
    (tmp_path / "cfg_1_sim" / "outputs").mkdir(parents=True)  # no util.txt
    agg = rm._aggregate_util(tmp_path)
    assert agg["tile_count"] == 2   # both dirs counted
    assert agg["active_cycles"] == 50
    assert agg["total_cycles"] == 100
    assert len(agg["per_tile_util"]) == 1


# ---------- _lookup_ppa ----------------------------------------------------


def test_lookup_ppa_missing_root_returns_empty(sp_design, tmp_path):
    assert rm._lookup_ppa(sp_design, tmp_path / "no" / "such") == {}


def test_lookup_ppa_returns_empty_when_none(sp_design):
    assert rm._lookup_ppa(sp_design, None) == {}


# ---------- dry-run -------------------------------------------------------


def test_dry_run_writes_config_but_no_dispatch(results_root, sp_design, any_app):
    r = rm.run_one_cell(sp_design, any_app, dry_run=True)
    assert r["sim_status"] == "SKIP"
    assert r["design_id"] == sp_design.id
    assert r["app_id"] == any_app.id
    assert r["sweep_config"] == rm.compose_sweep_line(sp_design, any_app, rm.DEFAULT_AHA_ROOT)
    cfg = results_root / sp_design.id / any_app.id / "sweep.cfg"
    assert cfg.is_file()
    assert cfg.read_text().strip() == r["sweep_config"]


def test_dry_run_never_launches_subprocess(results_root, sp_design, any_app,
                                           monkeypatch):
    """Explicit guard: dry_run must not call subprocess.run."""
    def _boom(*a, **k):
        raise AssertionError("subprocess.run was called during dry_run")
    monkeypatch.setattr(rm.subprocess, "run", _boom)
    r = rm.run_one_cell(sp_design, any_app, dry_run=True)
    assert r["sim_status"] == "SKIP"


# ---------- real dispatch (fake sweep script) -----------------------------


def test_missing_sweep_script_returns_fail(results_root, sp_design, any_app):
    r = rm.run_one_cell(sp_design, any_app, dry_run=False,
                        sweep_script="/no/such/script.sh")
    assert r["sim_status"] == "FAIL"
    assert "not found" in r["error"]


def test_dispatch_pass_aggregates_util_and_writes_log(results_root, tmp_path,
                                                     sp_design, any_app):
    fake = _write_fake_sweep_script(tmp_path, tiles=[(100, 200), (50, 200)])
    r = rm.run_one_cell(sp_design, any_app, dry_run=False, sweep_script=fake,
                        builds_root=tmp_path / "no_builds")
    assert r["sim_status"] == "PASS"
    assert r["active_cycles"] == 150
    assert r["total_cycles"] == 400
    assert r["utilization"] == pytest.approx(0.375)
    assert r["tile_count"] == 2
    assert r["roundtrip_returncode"] == 0
    # PPA fields absent because builds_root missing — must NOT crash.
    assert "synth_total_area_um2" not in r
    # Log file was created (may be empty if fake script writes no stdout).
    log = results_root / sp_design.id / any_app.id / "roundtrip.log"
    assert log.is_file()


def test_dispatch_fail_propagates_verdict(results_root, tmp_path, sp_design, any_app):
    fake = _write_fake_sweep_script(tmp_path, tiles=None,
                                    verdict="FAIL", exit_code=1)
    r = rm.run_one_cell(sp_design, any_app, dry_run=False, sweep_script=fake,
                        builds_root=tmp_path / "no_builds")
    assert r["sim_status"] == "FAIL"
    assert r["roundtrip_returncode"] == 1
    assert r["tile_count"] == 0
    assert "utilization" not in r


def test_dispatch_missing_summary_returns_missing(results_root, tmp_path,
                                                  sp_design, any_app):
    """Sweep script exits 0 without writing summary.txt at all."""
    script = tmp_path / "no_summary.sh"
    script.write_text("#!/usr/bin/env bash\nexit 0\n")
    script.chmod(0o755)
    r = rm.run_one_cell(sp_design, any_app, dry_run=False, sweep_script=script,
                        builds_root=tmp_path / "no_builds")
    assert r["sim_status"] == "MISSING"


def test_dispatch_timeout_reports_rc_124(results_root, tmp_path, sp_design, any_app):
    slow = tmp_path / "slow.sh"
    slow.write_text("#!/usr/bin/env bash\nsleep 10\n")
    slow.chmod(0o755)
    r = rm.run_one_cell(sp_design, any_app, dry_run=False, sweep_script=slow,
                        builds_root=tmp_path / "no_builds",
                        timeout_s=1)
    assert r["roundtrip_returncode"] == 124


# ---------- CLI main() ----------------------------------------------------


def test_cli_dry_run_writes_results_json_per_cell(results_root, monkeypatch, tmp_path):
    # Restrict the matrix so the test is fast.
    d0 = DESIGNS_SINGLE_LEVEL[0]
    a0 = APPS[0]
    rc = rm.main([
        "--dry-run",
        "--designs", d0.id,
        "--apps", a0.id,
        "--skip-unverified-check",
    ])
    assert rc == 0
    result = json.loads((results_root / d0.id / a0.id / "results.json").read_text())
    assert result["sim_status"] == "SKIP"
    assert result["design_id"] == d0.id


def test_cli_rejects_unverified_apps_unless_flag_set(results_root, capsys, tmp_path):
    """The registry has '??' markers on some apps; the guard must trip."""
    from THESIS.apps import registry
    if not registry.unverified():
        pytest.skip("registry has no unverified apps — guard cannot be tested")
    rc = rm.main([
        "--dry-run",
        "--designs", DESIGNS_SINGLE_LEVEL[0].id,
        "--apps", "all",
    ])
    assert rc == 2
    err = capsys.readouterr().err
    assert "??" in err


def test_cli_errors_on_unknown_app(results_root, capsys):
    rc = rm.main([
        "--dry-run",
        "--apps", "nope_not_a_real_app",
        "--designs", DESIGNS_SINGLE_LEVEL[0].id,
        "--skip-unverified-check",
    ])
    assert rc == 2
    assert "no apps matched" in capsys.readouterr().err


def test_cli_errors_on_unknown_design(results_root, capsys):
    rc = rm.main([
        "--dry-run",
        "--apps", APPS[0].id,
        "--designs", "not_a_design_id",
        "--skip-unverified-check",
    ])
    assert rc == 2
    assert "no designs matched" in capsys.readouterr().err
