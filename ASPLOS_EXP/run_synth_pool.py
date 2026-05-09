#!/usr/bin/env python3
"""
run_synth_pool.py — parallel synthesis runner for the thesis sweep

Drives cadence-genus-synthesis across all configs produced by
all_experiments_thesis.sh. Dynamically discovers the synthesis step
number from `make list`; never hardcodes step indices.

Usage:
  # Dry-run discovery — print plan, touch nothing
  python ASPLOS_EXP/run_synth_pool.py --phase discover --dry-run

  # Real discovery — materialize graphs, write configs.json
  python ASPLOS_EXP/run_synth_pool.py --phase discover

  # Pilot: 1 config
  python ASPLOS_EXP/run_synth_pool.py --filter 'fw_4_inp_2_outp_2$' --limit 1 --jobs 1

  # Full sweep
  python ASPLOS_EXP/run_synth_pool.py --jobs 4 --step-timeout-min 30
"""

import argparse
import collections
import concurrent.futures
import dataclasses
import datetime
import fcntl
import json
import os
import re
import subprocess
import sys
import threading
import time
from typing import Dict, List, Optional

# ── ANSI / log helpers ────────────────────────────────────────────────────────

ANSI_RE = re.compile(r'\x1b\[[0-9;]*[a-zA-Z]')

# Matches the make-echoed command line that precedes each mflowgen step banner.
# build_orchestrator.py:720 emits:  .../mflowgen-letters -c -t <step_name>
BANNER_RE = re.compile(r'mflowgen-letters\s+(?:-c\s+)?-t\s+([\w\-]+)')

# Matches lines from `make status` output:  - done -> 7   : cadence-genus-synthesis
STATUS_RE = re.compile(r'^\s*-\s*(done|build)\s*->\s*(\d+)\s*:\s*(\S+)')

# Matches lines from `make list` output:  -   7 : cadence-genus-synthesis
LIST_SYNTH_RE = re.compile(r'^\s*-\s*(\d+)\s*:\s*cadence-genus-synthesis\s*$')
LIST_SIM_RE   = re.compile(r'^\s*-\s*(\d+)\s*:\s*synopsys-vcs-sim-rtl\s*$')
# New parallel sim/round-trip step resolvers — see plan 2026-05-06.
LIST_VCS_SIM_SYNTH_RE = re.compile(r'^\s*-\s*(\d+)\s*:\s*synopsys-vcs-sim-synth\s*$')
LIST_RT_COMPILE_RE    = re.compile(r'^\s*-\s*(\d+)\s*:\s*clockwork-roundtrip-compile\s*$')
LIST_RT_SIM_RTL_RE    = re.compile(r'^\s*-\s*(\d+)\s*:\s*clockwork-roundtrip-sim-rtl\s*$')
LIST_RT_SIM_SYNTH_RE  = re.compile(r'^\s*-\s*(\d+)\s*:\s*clockwork-roundtrip-sim-synth\s*$')


def strip_ansi(s: str) -> str:
    return ANSI_RE.sub('', s)


def utcnow() -> str:
    return datetime.datetime.utcnow().isoformat() + 'Z'


def tail_file(path: str, n: int = 80) -> str:
    try:
        with open(path) as f:
            return ''.join(collections.deque(f, maxlen=n))
    except Exception as e:
        return f"(could not read {path}: {e})"


def atomic_write(path: str, content: str):
    tmp = path + '.tmp'
    with open(tmp, 'w') as f:
        f.write(content)
    os.replace(tmp, path)


def atomic_write_json(path: str, obj):
    atomic_write(path, json.dumps(obj, indent=2))


def fmt_duration(seconds: Optional[float]) -> str:
    if seconds is None:
        return '--'
    m, s = divmod(int(seconds), 60)
    return f"{m:02d}:{s:02d}"


# ── Data structures ───────────────────────────────────────────────────────────

@dataclasses.dataclass
class Config:
    config_id: str           # e.g. PORT_EXP/thesis_sweep_700/storage_cap_...
    build_dir: str           # absolute path to dir containing Makefile
    parent_build_root: str   # e.g. /sim/mstrange/THESIS_BUILDS/PORT_EXP
    sh_invocation_idx: int   # which .sh line produced this config


@dataclasses.dataclass
class RunState:
    config: Config
    state: str = 'pending'   # pending|running|done|failed|excluded|skipped
    synth_step: Optional[int] = None
    current_step: Optional[str] = None
    started_at: Optional[str] = None
    ended_at: Optional[str] = None
    duration_s: Optional[float] = None
    step_transitions: list = dataclasses.field(default_factory=list)
    exit_code: Optional[int] = None
    failed_step: Optional[str] = None
    category: Optional[str] = None
    investigation: Optional[dict] = None
    make_log_path: Optional[str] = None

    def to_dict(self) -> dict:
        return {
            'config_id': self.config.config_id,
            'build_dir': self.config.build_dir,
            'state': self.state,
            'synth_step': self.synth_step,
            'current_step': self.current_step,
            'started_at': self.started_at,
            'ended_at': self.ended_at,
            'duration_s': self.duration_s,
            'step_transitions': self.step_transitions,
            'exit_code': self.exit_code,
            'failed_step': self.failed_step,
            'category': self.category,
            'investigation': self.investigation,
            'make_log_path': self.make_log_path,
        }


# ── mflowgen helpers ──────────────────────────────────────────────────────────

def resolve_synth_step(build_dir: str) -> int:
    """Run `make list` and return the step number for cadence-genus-synthesis."""
    result = subprocess.run(
        ['make', 'list'], cwd=build_dir, capture_output=True, text=True
    )
    output = strip_ansi(result.stdout + result.stderr)
    for line in output.splitlines():
        m = LIST_SYNTH_RE.match(line)
        if m:
            return int(m.group(1))
    raise RuntimeError(
        f"cadence-genus-synthesis not found in 'make list' at {build_dir}\n"
        f"Output (first 500 chars):\n{output[:500]}"
    )


def resolve_sim_step(build_dir: str) -> int:
    """Run `make list` and return the step number for synopsys-vcs-sim-rtl."""
    result = subprocess.run(
        ['make', 'list'], cwd=build_dir, capture_output=True, text=True
    )
    output = strip_ansi(result.stdout + result.stderr)
    for line in output.splitlines():
        m = LIST_SIM_RE.match(line)
        if m:
            return int(m.group(1))
    raise RuntimeError(
        f"synopsys-vcs-sim-rtl not found in 'make list' at {build_dir}\n"
        f"Output (first 500 chars):\n{output[:500]}"
    )


def _resolve_step_by_regex(build_dir: str, regex, step_label: str) -> int:
    result = subprocess.run(
        ['make', 'list'], cwd=build_dir, capture_output=True, text=True
    )
    output = strip_ansi(result.stdout + result.stderr)
    for line in output.splitlines():
        m = regex.match(line)
        if m:
            return int(m.group(1))
    raise RuntimeError(
        f"{step_label} not found in 'make list' at {build_dir}\n"
        f"Output (first 500 chars):\n{output[:500]}"
    )


def resolve_vcs_sim_synth_step(build_dir: str) -> int:
    return _resolve_step_by_regex(build_dir, LIST_VCS_SIM_SYNTH_RE,
                                  'synopsys-vcs-sim-synth')


def resolve_roundtrip_compile_step(build_dir: str) -> int:
    return _resolve_step_by_regex(build_dir, LIST_RT_COMPILE_RE,
                                  'clockwork-roundtrip-compile')


def resolve_roundtrip_sim_rtl_step(build_dir: str) -> int:
    return _resolve_step_by_regex(build_dir, LIST_RT_SIM_RTL_RE,
                                  'clockwork-roundtrip-sim-rtl')


def resolve_roundtrip_sim_synth_step(build_dir: str) -> int:
    return _resolve_step_by_regex(build_dir, LIST_RT_SIM_SYNTH_RE,
                                  'clockwork-roundtrip-sim-synth')


def is_step_done(build_dir: str, step_num: int) -> bool:
    result = subprocess.run(
        ['make', 'status'], cwd=build_dir, capture_output=True, text=True
    )
    output = strip_ansi(result.stdout + result.stderr)
    for line in output.splitlines():
        m = STATUS_RE.match(line)
        if m and int(m.group(2)) == step_num:
            return m.group(1) == 'done'
    return False


def is_synth_step_done(build_dir: str, synth_step: int) -> bool:
    return is_step_done(build_dir, synth_step)


def is_pipeline_done(build_dir: str, *step_nums: int) -> bool:
    """All listed steps must be stamped done for the pipeline to be considered complete."""
    return all(is_step_done(build_dir, n) for n in step_nums)


def find_failing_step_from_status(build_dir: str) -> Optional[str]:
    """Re-run `make status` after failure; return the first step in 'build' state."""
    try:
        result = subprocess.run(
            ['make', 'status'], cwd=build_dir, capture_output=True, text=True
        )
        output = strip_ansi(result.stdout + result.stderr)
        for line in output.splitlines():
            m = STATUS_RE.match(line)
            if m and m.group(1) == 'build':
                return f"{m.group(2)}-{m.group(3)}"
    except Exception:
        pass
    return None


def find_step_dir(build_dir: str, step_num: int) -> Optional[str]:
    """Return the path to the numbered step directory, e.g. 7-cadence-genus-synthesis."""
    for name in sorted(os.listdir(build_dir)):
        if re.match(rf'^{step_num}-', name):
            return os.path.join(build_dir, name)
    return None


def find_step_dir_by_name(build_dir: str, step_name: str) -> Optional[str]:
    """Return the path to the step directory whose suffix matches step_name, e.g.
    'synopsys-vcs-sim-rtl' -> '8-synopsys-vcs-sim-rtl'."""
    try:
        for name in sorted(os.listdir(build_dir)):
            m = re.match(r'^(\d+)-(.+)$', name)
            if m and m.group(2) == step_name:
                return name
    except Exception:
        pass
    return None


# ── QoR parser ────────────────────────────────────────────────────────────────

def parse_qor(qor_path: str) -> dict:
    qor: dict = {'slack_ps': None, 'tns_ps': None, 'failing_paths': None, 'cell_area': None}
    if not os.path.isfile(qor_path):
        return qor
    try:
        with open(qor_path) as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                m = re.match(r'Slack\s*\(ps\)\s*[:\s]\s*([-\d.]+)', line)
                if m:
                    qor['slack_ps'] = float(m.group(1))
                    continue
                m = re.match(r'TNS\s*\(ps\)\s*[:\s]\s*([-\d.]+)', line)
                if m:
                    qor['tns_ps'] = float(m.group(1))
                    continue
                m = re.match(r'Failing\s+Paths\s*[:\s]\s*(\d+)', line)
                if m:
                    qor['failing_paths'] = int(m.group(1))
                    continue
                m = re.match(r'Cell\s+Area\s*[:\s]\s*([-\d.]+)', line)
                if m:
                    qor['cell_area'] = float(m.group(1))
    except Exception:
        pass
    return qor


# ── Failure investigation ─────────────────────────────────────────────────────

# ── gen_sram_macro health check ───────────────────────────────────────────────

def check_gen_sram_macro(build_dir: str, sram_step_num: int) -> dict:
    """Validate that gen_sram_macro ran correctly and emitted a real SRAM module.

    Returns a dict with keys: ok (bool), sram_module_name (str|None), issues (list[str]).
    """
    result: dict = {'ok': True, 'sram_module_name': None, 'issues': []}
    sram_dir = find_step_dir(build_dir, sram_step_num)
    if sram_dir is None:
        result['ok'] = False
        result['issues'].append(f"gen_sram_macro step dir not found (step {sram_step_num})")
        return result

    execstamp = os.path.join(sram_dir, '.execstamp')
    if not os.path.exists(execstamp):
        result['ok'] = False
        result['issues'].append("gen_sram_macro .execstamp missing — step did not complete")

    sram_v = os.path.join(sram_dir, 'outputs', 'sram.v')
    if not os.path.isfile(sram_v):
        result['ok'] = False
        result['issues'].append(f"sram.v not found at {sram_v}")
        return result

    # Extract the primary module name from sram.v
    try:
        with open(sram_v) as f:
            for line in f:
                m = re.match(r'^\s*module\s+(IN12LP_\S+)\b', line)
                if m:
                    result['sram_module_name'] = m.group(1)
                    break
    except Exception as e:
        result['issues'].append(f"Could not parse sram.v: {e}")

    if result['sram_module_name'] is None:
        result['ok'] = False
        result['issues'].append(
            "No 'module IN12LP_...' definition found in sram.v — macro may be invalid"
        )

    # Check the required output files exist
    required = ['sram.v', 'sram_tt.lib', 'sram_tt.db', 'sram.lef', 'sram.gds']
    outputs_dir = os.path.join(sram_dir, 'outputs')
    for fname in required:
        if not os.path.isfile(os.path.join(outputs_dir, fname)):
            result['ok'] = False
            result['issues'].append(f"gen_sram_macro output missing: {fname}")

    return result


# ── Black box check ───────────────────────────────────────────────────────────

def check_black_boxes(build_dir: str, synth_step: int, sram_module_name: Optional[str]) -> dict:
    """Check synthesized outputs for black-boxed modules.

    Returns a dict with keys:
      ok (bool), black_box_lines (list[str]), sram_instantiated (bool|None),
      sram_instance_count (int), issues (list[str]).
    """
    result: dict = {
        'ok': True,
        'black_box_lines': [],
        'sram_instantiated': None,
        'sram_instance_count': 0,
        'issues': [],
    }
    synth_dir = find_step_dir(build_dir, synth_step)
    if synth_dir is None:
        result['ok'] = False
        result['issues'].append(f"synthesis step dir not found (step {synth_step})")
        return result

    # ── Scan genus.log for black-box warnings ─────────────────────────────────
    genus_log = os.path.join(synth_dir, 'logs', 'genus.log')
    if os.path.isfile(genus_log):
        try:
            with open(genus_log) as f:
                for line in f:
                    # Genus emits: "Black-boxing module 'X'" or "black box" in various forms
                    if re.search(r'black.box|black-box|blackbox', line, re.IGNORECASE):
                        stripped = line.strip()
                        if stripped:
                            result['black_box_lines'].append(stripped)
        except Exception as e:
            result['issues'].append(f"Could not read genus.log: {e}")

        if result['black_box_lines']:
            result['ok'] = False
            result['issues'].append(
                f"{len(result['black_box_lines'])} black-box reference(s) in genus.log"
            )
    else:
        result['issues'].append("genus.log not found — synthesis may not have run")

    # ── Check SRAM macro instantiation in synthesized netlist ─────────────────
    netlist = os.path.join(synth_dir, 'outputs', 'design.v')
    if os.path.isfile(netlist) and sram_module_name:
        try:
            with open(netlist) as f:
                content = f.read()
            count = content.count(sram_module_name)
            result['sram_instance_count'] = count
            result['sram_instantiated'] = count > 0
            if count == 0:
                result['ok'] = False
                result['issues'].append(
                    f"SRAM macro '{sram_module_name}' not found in design.v — "
                    "may have been black-boxed or optimized away"
                )
        except Exception as e:
            result['issues'].append(f"Could not read design.v: {e}")
    elif sram_module_name is None:
        result['sram_instantiated'] = None
        result['issues'].append("SRAM module name unknown — skipping netlist check")
    elif not os.path.isfile(netlist):
        result['ok'] = False
        result['issues'].append("design.v not found — synthesis output missing")

    return result


TRIAGE_NOTES: Dict[str, str] = {
    'elab_error': (
        "RTL elaboration failed — check 4-rtl/TEST/.../lakespec.sv for unresolved module refs."
    ),
    'elab_error:no_valid_gf_macro': (
        "No GF macro for this (depth, width) combo — expected, see .sh comments. "
        "Consider excluding from sweep."
    ),
    'genus_error': (
        "Internal Genus error. Check full logs/genus.log. "
        "May be tech-lib path issue or corrupted ADK."
    ),
    'license_failure': (
        "Genus license timeout — reduce --jobs or retry with --rerun-failed license_failure."
    ),
    'missing_input': (
        "Input file not found. Did gen_sram_macro complete? Run `make status`."
    ),
    'black_boxes': (
        "Synthesis completed but Genus black-boxed one or more modules. "
        "Check genus.log for 'black-box' lines and verify SRAM lib views are correct."
    ),
    'sram_not_instantiated': (
        "Synthesis completed but the SRAM macro is absent from design.v. "
        "Verify gen_sram_macro outputs and that the RTL references the correct macro name."
    ),
    'gen_sram_macro_incomplete': (
        "gen_sram_macro step output is missing or incomplete. "
        "Re-run `make 6` manually and check logs."
    ),
    'incomplete': (
        "Process crashed mid-run. Check for OOM (dmesg) or timeout. "
        "Retry with --step-timeout-min."
    ),
    'timing_violation': (
        "Synthesis closed but has negative slack. "
        "Relax --frequency or review final_time.rpt."
    ),
    'pre_genus_failure': (
        "Synthesis step never launched. Check mflowgen-run.log for upstream dep failure."
    ),
    'postcondition_mismatch': (
        "All postconditions passed but make returned non-zero. Manual inspection needed."
    ),
    'sim_license_failure': (
        "Xcelium (xrun) could not check out a license. "
        "Retry with --rerun-failed sim_license_failure when licenses free."
    ),
    'sim_test_failure': (
        "RTL simulation completed but gold-data comparison reported Test FAILED. "
        "Inspect 8-synopsys-vcs-sim-rtl/outputs/ vs inputs/gold/ to find the divergence."
    ),
    'sim_failure': (
        "synopsys-vcs-sim-rtl step failed (compile/run error, or postcondition mismatch). "
        "See the step's mflowgen-run.log for the actual error."
    ),
    'vcs_sim_synth_failure': (
        "Handcrafted test failed on synthesized netlist (synopsys-vcs-sim-synth). "
        "If synopsys-vcs-sim-rtl PASSed for this same config, synthesis broke "
        "the simple handcrafted pattern."
    ),
    'roundtrip_compile_failure': (
        "clockwork-roundtrip-compile could not schedule conv_3_3 against this "
        "spec's collateral. Check the step's mflowgen-run.log for the clockwork error."
    ),
    'roundtrip_sim_rtl_failure': (
        "Round-trip sim failed on un-synthesized RTL (clockwork-roundtrip-sim-rtl). "
        "Bug is in spec/compiler/converter — synthesis is not implicated."
    ),
    'roundtrip_sim_synth_failure': (
        "Round-trip sim failed only on synthesized netlist "
        "(clockwork-roundtrip-sim-synth) while the rtl variant PASSed. "
        "Synthesis broke something for the round-trip pattern."
    ),
}


def investigate_failure(
    build_dir: str,
    config_run_dir: str,
    synth_step: int,
    failed_step_name: Optional[str],
) -> dict:
    inv: dict = {
        'postcondition_failures': [],
        'category': 'failed',
        'genus_log_errors': [],
        'genus_normal_exit': None,
        'qor': {'slack_ps': None, 'tns_ps': None, 'failing_paths': None, 'cell_area': None},
        'key_excerpt_path': None,
        'triage_note': None,
    }
    excerpt_lines: List[str] = []

    # ── Sim / round-trip step failure short-circuit ───────────────────────────
    # If the failure is in any of the sim or round-trip steps, the genus logs
    # are irrelevant. Categorize directly from the failing step's run log.
    # Order matters: we want the most specific match first (e.g.
    # 'clockwork-roundtrip-sim-rtl' before 'synopsys-vcs-sim-rtl').
    SIM_STEP_CATEGORY_MAP = [
        ('clockwork-roundtrip-compile',  'roundtrip_compile_failure'),
        ('clockwork-roundtrip-sim-rtl',  'roundtrip_sim_rtl_failure'),
        ('clockwork-roundtrip-sim-synth','roundtrip_sim_synth_failure'),
        ('synopsys-vcs-sim-synth',       'vcs_sim_synth_failure'),
        ('synopsys-vcs-sim-rtl',         None),  # routed through legacy logic
    ]
    matched_sim = None
    matched_default = None
    if failed_step_name:
        for needle, default_cat in SIM_STEP_CATEGORY_MAP:
            if needle in failed_step_name:
                matched_sim = needle
                matched_default = default_cat
                break

    if matched_sim is not None:
        sim_log_path = os.path.join(build_dir, failed_step_name, 'mflowgen-run.log')
        sim_content = ''
        if os.path.isfile(sim_log_path):
            try:
                with open(sim_log_path) as f:
                    sim_content = f.read()
            except Exception as e:
                excerpt_lines.append(f"(could not read sim log: {e})")

        has_sim_license = any(x in sim_content for x in
                              ['NOLICN', 'Unable to checkout license', 'lic_error LMF-'])
        has_sim_test_fail = 'Test FAILED' in sim_content

        if matched_default is not None:
            # New (round-trip / vcs-sim-synth) steps use their own categories
            # straight from the map. License-failure check still wins for the
            # license category since it's actionable separately.
            if has_sim_license:
                inv['category'] = 'sim_license_failure'
            else:
                inv['category'] = matched_default
        else:
            # Legacy synopsys-vcs-sim-rtl path — preserve existing categorization.
            if has_sim_license:
                inv['category'] = 'sim_license_failure'
            elif has_sim_test_fail:
                inv['category'] = 'sim_test_failure'
            else:
                inv['category'] = 'sim_failure'

        excerpt_lines += [
            f'=== {failed_step_name}/mflowgen-run.log (last 60 lines) ===',
            tail_file(sim_log_path, 60),
        ]
        inv['triage_note'] = TRIAGE_NOTES.get(inv['category'], "Sim failed.")

        excerpt_path = os.path.join(config_run_dir, 'investigation_excerpt.log')
        try:
            atomic_write(excerpt_path, '\n'.join(str(x) for x in excerpt_lines))
            inv['key_excerpt_path'] = excerpt_path
        except Exception as e:
            inv['key_excerpt_path'] = f"(write failed: {e})"

        return inv

    synth_step_dir = find_step_dir(build_dir, synth_step)

    # ── QoR (parse regardless of failure mode) ────────────────────────────────
    if synth_step_dir:
        qor_path = os.path.join(synth_step_dir, 'results_syn', 'final_qor.rpt')
        inv['qor'] = parse_qor(qor_path)

    # ── Postconditions re-run ─────────────────────────────────────────────────
    if synth_step_dir:
        postcond = os.path.join(synth_step_dir, 'mflowgen-check-postconditions.py')
        if os.path.isfile(postcond):
            r = subprocess.run(
                [sys.executable, '-m', 'pytest', '-q', '--tb=short',
                 '--no-header', '--color=no', postcond],
                cwd=synth_step_dir, capture_output=True, text=True
            )
            pc_output = r.stdout + r.stderr
            failed_tests = re.findall(r'FAILED\s+\S+::(test_\w+)', pc_output)
            inv['postcondition_failures'] = failed_tests
            excerpt_lines += ['=== Postcondition output ===', pc_output[:2000]]

    # ── Genus log analysis ────────────────────────────────────────────────────
    genus_content: Optional[str] = None
    genus_log_path: Optional[str] = None
    has_unresolved = has_no_macro = has_license = has_missing = False

    if synth_step_dir:
        candidate = os.path.join(synth_step_dir, 'logs', 'genus.log')
        if os.path.isfile(candidate):
            genus_log_path = candidate
            try:
                with open(genus_log_path) as f:
                    genus_content = f.read()
                inv['genus_normal_exit'] = 'Normal exit.' in genus_content
                error_lines = [l.strip() for l in genus_content.splitlines()
                               if 'Error   :' in l]
                inv['genus_log_errors'] = list(dict.fromkeys(error_lines))[:20]
                has_unresolved = 'Cannot resolve reference' in genus_content
                has_no_macro = ('No valid macros' in genus_content or
                                'assert len(options_)' in genus_content)
                has_license = any(x in genus_content for x in
                                  ['Cdslmd timeout', 'license checkout failed',
                                   'license checkout'])
                has_missing = any(x in genus_content for x in
                                  ['cannot open', 'No such file'])
            except Exception as e:
                excerpt_lines.append(f"(could not read genus.log: {e})")
        else:
            inv['genus_normal_exit'] = False

    # ── RTL-gen log check (for 4-rtl failures) ────────────────────────────────
    rtl_gen_content = ''
    rtl_failed = failed_step_name and 'rtl' in (failed_step_name or '')
    if rtl_failed:
        failed_dir = os.path.join(build_dir, failed_step_name) if failed_step_name else None
        if failed_dir and os.path.isdir(failed_dir):
            for root, _, files in os.walk(failed_dir):
                for fname in files:
                    if fname == 'rtl_gen.log':
                        rtl_log = os.path.join(root, fname)
                        try:
                            with open(rtl_log) as f:
                                rtl_gen_content = f.read()
                        except Exception:
                            pass
                        excerpt_lines += [
                            f'=== rtl_gen.log (last 80 lines) ===',
                            tail_file(rtl_log, 80),
                        ]
                        break
                if rtl_gen_content:
                    break
        # Also check mflowgen-run.log for the failed step
        if failed_step_name:
            mfr = os.path.join(build_dir, failed_step_name, 'mflowgen-run.log')
            if os.path.isfile(mfr):
                excerpt_lines += [
                    f'=== {failed_step_name}/mflowgen-run.log (last 40 lines) ===',
                    tail_file(mfr, 40),
                ]

    # ── Decision tree ─────────────────────────────────────────────────────────
    failures = inv['postcondition_failures']
    genus_launched = genus_log_path is not None

    if not genus_launched:
        # Synthesis step never ran — upstream dependency failed
        inv['category'] = 'pre_genus_failure'
        # Check for the known GF macro issue
        if ('No valid macros' in rtl_gen_content or
                'assert len(options_)' in rtl_gen_content or
                has_no_macro):
            inv['category'] = 'elab_error:no_valid_gf_macro'
        # Fall back to mflowgen-run.log if we haven't grabbed it yet
        if failed_step_name and not rtl_failed:
            mfr = os.path.join(build_dir, failed_step_name, 'mflowgen-run.log')
            if os.path.isfile(mfr):
                excerpt_lines += [
                    f'=== {failed_step_name}/mflowgen-run.log (last 40 lines) ===',
                    tail_file(mfr, 40),
                ]

    elif 'test_4_' in failures or has_unresolved:
        inv['category'] = 'elab_error'
        if genus_content:
            refs = re.findall(r'Cannot resolve reference[^\n]*', genus_content)[:5]
            excerpt_lines += ['=== Cannot resolve reference (first 5) ==='] + refs

    elif 'test_5_' in failures or inv['genus_log_errors']:
        if has_no_macro:
            inv['category'] = 'elab_error:no_valid_gf_macro'
        elif has_license:
            inv['category'] = 'license_failure'
        elif has_missing:
            inv['category'] = 'missing_input'
        else:
            inv['category'] = 'genus_error'
        excerpt_lines += ['=== Genus error lines ==='] + inv['genus_log_errors'][:10]
        if genus_log_path:
            excerpt_lines += ['=== genus.log tail (40 lines) ===',
                              tail_file(genus_log_path, 40)]

    elif any(t in failures for t in ('test_0_', 'test_1_', 'test_2_', 'test_3_')):
        inv['category'] = 'incomplete'
        if genus_log_path:
            excerpt_lines += ['=== genus.log tail (40 lines) ===',
                              tail_file(genus_log_path, 40)]

    elif not failures and inv.get('genus_normal_exit'):
        inv['category'] = 'postcondition_mismatch'

    else:
        inv['category'] = 'failed'
        if genus_log_path:
            excerpt_lines += ['=== genus.log tail (40 lines) ===',
                              tail_file(genus_log_path, 40)]

    inv['triage_note'] = TRIAGE_NOTES.get(
        inv['category'], "Unknown failure — manual investigation needed."
    )

    # ── Write excerpt ─────────────────────────────────────────────────────────
    excerpt_path = os.path.join(config_run_dir, 'investigation_excerpt.log')
    try:
        atomic_write(excerpt_path, '\n'.join(str(x) for x in excerpt_lines))
        inv['key_excerpt_path'] = excerpt_path
    except Exception as e:
        inv['key_excerpt_path'] = f"(write failed: {e})"

    return inv


# ── Dashboard renderer ────────────────────────────────────────────────────────

def render_dashboard(states: Dict[str, RunState], run_dir: str, start_time: float):
    counts: Dict[str, int] = collections.Counter(s.state for s in states.values())  # type: ignore
    total = len(states)
    elapsed = fmt_duration(time.time() - start_time)

    lines = [
        f"# Synth Pool — {datetime.datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')}",
        '',
        (f"Jobs: {counts.get('running', 0)}  Total: {total}  "
         f"Done: {counts.get('done', 0)}  Failed: {counts.get('failed', 0)}  "
         f"Excluded: {counts.get('excluded', 0)}  "
         f"Skipped: {counts.get('skipped', 0)}  "
         f"Pending: {counts.get('pending', 0)}  "
         f"Wall: {elapsed}"),
        '',
    ]

    running = [s for s in states.values() if s.state == 'running']
    if running:
        lines += ['## Currently running',
                  '| Config | Step | Elapsed |', '|---|---|---|']
        for s in running:
            age = '--'
            if s.started_at:
                try:
                    st = datetime.datetime.fromisoformat(s.started_at.rstrip('Z'))
                    age = fmt_duration((datetime.datetime.utcnow() - st).total_seconds())
                except Exception:
                    pass
            lines.append(f"| `{s.config.config_id}` | {s.current_step or '?'} | {age} |")
        lines.append('')

    failed = [s for s in states.values() if s.state in ('failed', 'excluded')]
    if failed:
        lines += ['## Failed / Excluded',
                  '| Config | Category | Failing step |', '|---|---|---|']
        for s in failed:
            cat = (s.investigation or {}).get('category', s.category or '?')
            lines.append(f"| `{s.config.config_id}` | {cat} | {s.failed_step or '?'} |")
        lines.append('')

    done = [s for s in states.values() if s.state == 'done']
    if done:
        lines += [f"## Done ({len(done)})",
                  '| Config | Duration | Slack (ps) | SRAM instances | Black boxes |',
                  '|---|---|---|---|---|']
        for s in done:
            inv = s.investigation or {}
            slack = inv.get('qor', {}).get('slack_ps', '--')
            sram_n = inv.get('black_box_check', {}).get('sram_instance_count', '--')
            bb_n = len(inv.get('black_box_check', {}).get('black_box_lines', []))
            bb_str = f"**{bb_n}**" if bb_n > 0 else str(bb_n)
            lines.append(
                f"| `{s.config.config_id}` | {fmt_duration(s.duration_s)} "
                f"| {slack} | {sram_n} | {bb_str} |"
            )
        lines.append('')

    pending = [s for s in states.values() if s.state == 'pending']
    if pending:
        lines.append(f"## Pending ({len(pending)})")
        for s in pending[:10]:
            lines.append(f"- `{s.config.config_id}`")
        if len(pending) > 10:
            lines.append(f"- … and {len(pending) - 10} more")
        lines.append('')

    try:
        atomic_write(os.path.join(run_dir, 'dashboard.md'), '\n'.join(lines))
        atomic_write_json(os.path.join(run_dir, 'dashboard.json'), {
            'timestamp': utcnow(),
            'counts': dict(counts),
            'total': total,
            'states': {k: v.to_dict() for k, v in states.items()},
        })
    except Exception as e:
        print(f"[dashboard] write failed: {e}", file=sys.stderr)


# ── .sh parser ────────────────────────────────────────────────────────────────

def parse_sh_file(sh_path: str) -> List[dict]:
    """Parse all_experiments_thesis.sh; return one dict per non-commented invocation."""
    with open(sh_path) as f:
        raw = f.read()

    # Join line-continuations
    joined = raw.replace('\\\n', ' ')
    invocations = []
    for idx, line in enumerate(joined.splitlines()):
        line = line.strip()
        if not line or line.startswith('#'):
            continue
        if 'create_mflowgen_experiments.py' not in line:
            continue
        # Strip flags that control execution (we want graph-only)
        line = re.sub(r'--run_builds\b', '', line)
        line = re.sub(r'--synth_only\b', '', line)
        line = re.sub(r'\s+', ' ', line).strip()
        m = re.search(r'--build_dir\s+(\S+)', line)
        build_dir = m.group(1) if m else None
        invocations.append({
            'idx': len(invocations),
            'line': line,
            'build_dir': build_dir,
        })
    return invocations


# ── Discovery ─────────────────────────────────────────────────────────────────

def walk_config_dirs(build_dir: str) -> List[str]:
    """Return all mflowgen config dirs exactly 2 levels deep under build_dir."""
    configs = []
    if not os.path.isdir(build_dir):
        return configs
    for group in sorted(os.listdir(build_dir)):
        group_path = os.path.join(build_dir, group)
        if not os.path.isdir(group_path):
            continue
        for name in sorted(os.listdir(group_path)):
            config_path = os.path.join(group_path, name)
            if os.path.isdir(config_path) and os.path.isfile(
                    os.path.join(config_path, 'Makefile')):
                configs.append(config_path)
    return configs


def make_config_id(build_dir: str, config_path: str) -> str:
    """Return a stable relative ID, e.g. PORT_EXP/thesis_sweep_700/storage_cap_..."""
    # build_dir = /sim/.../THESIS_BUILDS/PORT_EXP
    # parent    = /sim/.../THESIS_BUILDS
    parent = os.path.dirname(build_dir)
    return os.path.relpath(config_path, parent)


def discover_configs(
    invocations: List[dict],
    run_dir: str,
    dry_run: bool,
    lake_root: str,
    filter_re: Optional[re.Pattern],  # type: ignore
    limit: Optional[int],
    build_root_filter: Optional[List[str]],
) -> List[Config]:

    discover_dir = os.path.join(run_dir, 'discover')
    if not dry_run:
        os.makedirs(discover_dir, exist_ok=True)

    filtered_invocations = [
        inv for inv in invocations
        if build_root_filter is None or inv['build_dir'] in build_root_filter
    ]

    if dry_run:
        print(f"[dry-run] Would invoke {len(filtered_invocations)} "
              f"create_mflowgen_experiments.py calls:")
        for inv in filtered_invocations:
            print(f"  [{inv['idx']}] {inv['line'][:120]}")
        print(f"[dry-run] Build roots: "
              f"{sorted(set(i['build_dir'] for i in filtered_invocations))}")
        return []

    print(f"\n{'='*60}")
    print(f"DISCOVERY: {len(filtered_invocations)} graph-generation invocations")
    print(f"{'='*60}\n")

    for inv in filtered_invocations:
        log_path = os.path.join(
            discover_dir,
            f"{inv['idx']}-{os.path.basename(inv['build_dir'] or 'unknown')}.log",
        )
        print(f"[{inv['idx']+1}/{len(filtered_invocations)}] "
              f"{inv['line'][:80].rstrip()}  ...")
        print(f"  log → {log_path}")
        with open(log_path, 'w') as lf:
            r = subprocess.run(
                inv['line'], shell=True,
                stdout=lf, stderr=subprocess.STDOUT,
                cwd=lake_root,
            )
        if r.returncode != 0:
            print(f"  ERROR (exit {r.returncode}) — see {log_path}", file=sys.stderr)
            print(tail_file(log_path, 20), file=sys.stderr)
        else:
            print(f"  OK")

    # Walk build dirs
    print(f"\nWalking build dirs for configs...")
    seen_paths: set = set()
    all_configs: List[Config] = []
    for inv in filtered_invocations:
        bd = inv['build_dir']
        if not bd:
            continue
        for cpath in walk_config_dirs(bd):
            if cpath in seen_paths:
                continue
            seen_paths.add(cpath)
            cid = make_config_id(bd, cpath)
            if filter_re and not filter_re.search(cid):
                continue
            all_configs.append(Config(
                config_id=cid,
                build_dir=cpath,
                parent_build_root=bd,
                sh_invocation_idx=inv['idx'],
            ))

    if limit:
        all_configs = all_configs[:limit]

    print(f"Discovered {len(all_configs)} configs.\n")
    return all_configs


# ── Per-config worker ─────────────────────────────────────────────────────────

def run_one(config: Config, run_dir: str, args, dispatcher: 'Dispatcher') -> RunState:
    config_id_safe = re.sub(r'[/ ]+', '__', config.config_id)
    config_run_dir = os.path.join(run_dir, 'configs', config_id_safe)
    os.makedirs(config_run_dir, exist_ok=True)

    state = RunState(config=config)

    def save_state():
        atomic_write_json(os.path.join(config_run_dir, 'status.json'), state.to_dict())

    # ── Resolve synthesis + sim steps dynamically ─────────────────────────────
    try:
        synth_step = resolve_synth_step(config.build_dir)
        state.synth_step = synth_step
    except Exception as e:
        state.state = 'failed'
        state.category = 'pre_genus_failure'
        state.investigation = {'triage_note': f"Could not resolve synthesis step: {e}"}
        save_state()
        dispatcher.on_transition(config.config_id, state)
        print(f"[STEP-RESOLVE FAILED] {config.config_id}: {e}", file=sys.stderr)
        return state

    try:
        sim_step = resolve_sim_step(config.build_dir)
    except Exception as e:
        state.state = 'failed'
        state.category = 'pre_genus_failure'
        state.investigation = {'triage_note': f"Could not resolve sim step: {e}"}
        save_state()
        dispatcher.on_transition(config.config_id, state)
        print(f"[STEP-RESOLVE FAILED] {config.config_id}: {e}", file=sys.stderr)
        return state

    # Resolve the new sim/round-trip steps. If any are missing, treat as
    # pre_genus_failure — they should always exist in the updated graph.
    try:
        vcs_sim_synth_step      = resolve_vcs_sim_synth_step(config.build_dir)
        roundtrip_compile_step  = resolve_roundtrip_compile_step(config.build_dir)
        roundtrip_sim_rtl_step  = resolve_roundtrip_sim_rtl_step(config.build_dir)
        roundtrip_sim_synth_step= resolve_roundtrip_sim_synth_step(config.build_dir)
    except Exception as e:
        state.state = 'failed'
        state.category = 'pre_genus_failure'
        state.investigation = {
            'triage_note': f"Could not resolve round-trip / sim-synth step: {e}"
        }
        save_state()
        dispatcher.on_transition(config.config_id, state)
        print(f"[STEP-RESOLVE FAILED] {config.config_id}: {e}", file=sys.stderr)
        return state

    all_step_nums = [synth_step, sim_step, vcs_sim_synth_step,
                     roundtrip_compile_step, roundtrip_sim_rtl_step,
                     roundtrip_sim_synth_step]

    make_log_path = os.path.join(config_run_dir, f"make{synth_step}.log")
    state.make_log_path = make_log_path

    # ── Skip-done check ───────────────────────────────────────────────────────
    force_this = False
    if args.force is not None:
        if args.force == '':
            force_this = True
        else:
            try:
                force_this = bool(re.search(args.force, config.config_id))
            except re.error:
                force_this = args.force in config.config_id

    if not force_this:
        try:
            if is_pipeline_done(config.build_dir, *all_step_nums):
                state.state = 'skipped'
                save_state()
                dispatcher.on_transition(config.config_id, state)
                print(f"[SKIP] {config.config_id} (steps {all_step_nums} already done)")
                return state
        except Exception as e:
            print(f"[WARN] make status failed for {config.config_id}: {e} — proceeding")

    # ── Lock ──────────────────────────────────────────────────────────────────
    lock_path = os.path.join(config.build_dir, '.synth_pool.lock')
    try:
        lock_fd = open(lock_path, 'w')
        fcntl.flock(lock_fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        state.state = 'failed'
        state.category = 'pre_genus_failure'
        state.investigation = {
            'triage_note': f"Lock busy — another process is running in {config.build_dir}"
        }
        save_state()
        dispatcher.on_transition(config.config_id, state)
        return state

    # ── Launch ────────────────────────────────────────────────────────────────
    state.state = 'running'
    state.started_at = utcnow()
    state.current_step = 'starting'
    save_state()
    dispatcher.on_transition(config.config_id, state)

    env = {**os.environ, 'TERM': 'dumb'}
    last_banner_step: Optional[str] = None
    exit_code = -1

    try:
        with open(make_log_path, 'w') as log_fh:
            # mflowgen DAG runs the rtl-level sims in parallel with synth and
            # the synth-level sims after synth completes. Listing all leaf
            # targets does not serialize — make+mflowgen handle the order.
            make_targets = ['make'] + [str(n) for n in all_step_nums]
            proc = subprocess.Popen(
                make_targets,
                cwd=config.build_dir,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                bufsize=1,
                text=True,
                env=env,
            )

            deadline = (
                time.time() + args.step_timeout_min * 60
                if args.step_timeout_min else None
            )

            for line in (proc.stdout or []):
                log_fh.write(line)

                m = BANNER_RE.search(line)
                if m:
                    step_name = m.group(1)
                    now_ts = utcnow()
                    if last_banner_step and last_banner_step != step_name:
                        # Close the previous transition
                        for t in reversed(state.step_transitions):
                            if t.get('step') == last_banner_step and 'ended_at' not in t:
                                t['ended_at'] = now_ts
                                break
                    state.step_transitions.append({
                        'step': step_name, 'started_at': now_ts
                    })
                    last_banner_step = step_name
                    state.current_step = step_name
                    save_state()
                    dispatcher.on_transition(config.config_id, state)
                    print(f"[{config.config_id[-60:]}] → {step_name}")

                if deadline and time.time() > deadline:
                    proc.terminate()
                    time.sleep(30)
                    if proc.poll() is None:
                        proc.kill()
                    state.category = 'timeout'
                    break

            proc.wait()
            exit_code = proc.returncode

    except Exception as e:
        exit_code = -1
        print(f"[EXCEPTION] {config.config_id}: {e}", file=sys.stderr)
    finally:
        try:
            fcntl.flock(lock_fd, fcntl.LOCK_UN)
            lock_fd.close()
        except Exception:
            pass

    # ── Finalize state ────────────────────────────────────────────────────────
    state.exit_code = exit_code
    state.ended_at = utcnow()
    if state.started_at:
        try:
            start_dt = datetime.datetime.fromisoformat(state.started_at.rstrip('Z'))
            end_dt = datetime.datetime.fromisoformat(state.ended_at.rstrip('Z'))
            state.duration_s = (end_dt - start_dt).total_seconds()
        except Exception:
            pass

    if exit_code == 0:
        state.state = 'done'
        synth_dir = find_step_dir(config.build_dir, synth_step)

        # Find gen_sram_macro step number dynamically from make list output
        sram_step_num: Optional[int] = None
        try:
            r = subprocess.run(['make', 'list'], cwd=config.build_dir,
                               capture_output=True, text=True)
            for line in strip_ansi(r.stdout).splitlines():
                m = re.match(r'^\s*-\s*(\d+)\s*:\s*gen_sram_macro\s*$', line)
                if m:
                    sram_step_num = int(m.group(1))
                    break
        except Exception:
            pass

        # Check gen_sram_macro
        sram_check = (check_gen_sram_macro(config.build_dir, sram_step_num)
                      if sram_step_num is not None
                      else {'ok': False, 'sram_module_name': None,
                            'issues': ['gen_sram_macro step not found in make list'],
                            'sram_instance_count': 0})
        sram_module_name = sram_check.get('sram_module_name')

        # Check for black boxes in synthesis
        bb_check = (check_black_boxes(config.build_dir, synth_step, sram_module_name)
                    if synth_dir else
                    {'ok': False, 'black_box_lines': [], 'sram_instantiated': None,
                     'sram_instance_count': 0, 'issues': ['synth step dir not found']})

        # QoR
        qor = (parse_qor(os.path.join(synth_dir, 'results_syn', 'final_qor.rpt'))
               if synth_dir else
               {'slack_ps': None, 'tns_ps': None, 'failing_paths': None, 'cell_area': None})

        inv: dict = {
            'qor': qor,
            'category': 'done',
            'triage_note': None,
            'gen_sram_macro': sram_check,
            'black_box_check': bb_check,
        }

        # Escalate issues — priority order: timing > black_boxes > sram > gen_sram_macro
        issues: List[str] = []
        if qor['slack_ps'] is not None and qor['slack_ps'] < 0:
            inv['category'] = 'timing_violation'
            inv['triage_note'] = TRIAGE_NOTES['timing_violation']
            state.state = 'failed'
            state.category = 'timing_violation'
            issues.append(f"timing violation: slack={qor['slack_ps']} ps")

        if bb_check['black_box_lines']:
            if inv['category'] == 'done':
                inv['category'] = 'black_boxes'
                inv['triage_note'] = TRIAGE_NOTES['black_boxes']
                state.state = 'failed'
                state.category = 'black_boxes'
            issues.append(
                f"{len(bb_check['black_box_lines'])} black-box warning(s) in genus.log"
            )

        if bb_check.get('sram_instantiated') is False:
            if inv['category'] == 'done':
                inv['category'] = 'sram_not_instantiated'
                inv['triage_note'] = TRIAGE_NOTES['sram_not_instantiated']
                state.state = 'failed'
                state.category = 'sram_not_instantiated'
            issues.append(
                f"SRAM macro '{sram_module_name}' absent from design.v"
            )

        if not sram_check['ok']:
            if inv['category'] == 'done':
                inv['category'] = 'gen_sram_macro_incomplete'
                inv['triage_note'] = TRIAGE_NOTES['gen_sram_macro_incomplete']
                # Don't mark failed — synthesis passed; warn instead
            issues += sram_check.get('issues', [])

        if issues:
            inv['post_synth_issues'] = issues
            if state.state == 'failed':
                state.failed_step = f"{synth_step}-cadence-genus-synthesis"
                dispatcher.on_failure(config.config_id, state)

        state.investigation = inv
    else:
        # Identify failing step
        # Prefer the step that was actively running when make died (last_banner_step)
        # over the first 'build' step in `make status` — the latter returns whatever
        # is earliest in topo order with no stamp (e.g. .PHONY 3-info), not the actual
        # failing step. Map the bare step name back to its numbered directory.
        failed_step: Optional[str] = None
        if last_banner_step:
            failed_step = find_step_dir_by_name(config.build_dir, last_banner_step) or last_banner_step
        if not failed_step:
            failed_step = find_failing_step_from_status(config.build_dir)
        state.failed_step = failed_step
        if state.category != 'timeout':
            state.investigation = investigate_failure(
                config.build_dir, config_run_dir, synth_step, failed_step
            )
            cat = state.investigation.get('category', 'failed')
            state.category = cat
        else:
            state.investigation = {
                'category': 'timeout',
                'triage_note': TRIAGE_NOTES['incomplete'],
                'qor': {'slack_ps': None, 'tns_ps': None,
                        'failing_paths': None, 'cell_area': None},
            }
        # excluded = known-bad GF macro configs
        cat = state.category or 'failed'
        state.state = 'excluded' if cat == 'elab_error:no_valid_gf_macro' else 'failed'
        dispatcher.on_failure(config.config_id, state)

    save_state()
    dispatcher.on_transition(config.config_id, state)
    return state


# ── Dispatcher ────────────────────────────────────────────────────────────────

class Dispatcher:
    def __init__(self, run_dir: str, start_time: float, no_dashboard: bool = False):
        self.run_dir = run_dir
        self.start_time = start_time
        self.no_dashboard = no_dashboard
        self.states: Dict[str, RunState] = {}
        self._lock = threading.Lock()
        self._failures_path = os.path.join(run_dir, 'failures.md')
        with open(self._failures_path, 'w') as f:
            f.write(f"# Synthesis Failures — {utcnow()}\n\n")

    def register(self, states: Dict[str, RunState]):
        with self._lock:
            self.states.update(states)
            self._render()

    def on_transition(self, config_id: str, state: RunState):
        with self._lock:
            self.states[config_id] = state
            if not self.no_dashboard:
                self._render()

    def on_failure(self, config_id: str, state: RunState):
        inv = state.investigation or {}
        category = inv.get('category', state.category or 'failed')
        triage = inv.get('triage_note', '')
        excerpt_path = inv.get('key_excerpt_path', '')
        excerpt = tail_file(excerpt_path, 80) if excerpt_path and os.path.isfile(excerpt_path) else ''

        sep = '=' * 70
        msg = [
            f'\n{sep}',
            f'=== FAILURE: {config_id} ===',
            f'  Category    : {category}',
            f'  Failing step: {state.failed_step or "?"}',
            f'  Exit code   : {state.exit_code}',
            f'  Triage      : {triage}',
        ]
        if excerpt:
            msg += ['', '--- Excerpt ---', excerpt, '--- End excerpt ---']
        msg.append(sep)
        print('\n'.join(msg), file=sys.stderr)

        with self._lock:
            with open(self._failures_path, 'a') as f:
                f.write(f'\n## {config_id}\n\n')
                f.write(f'- **Category**: {category}\n')
                f.write(f'- **Failing step**: {state.failed_step or "?"}\n')
                f.write(f'- **Exit code**: {state.exit_code}\n')
                f.write(f'- **Triage**: {triage}\n')
                if excerpt_path:
                    f.write(f'- **Log**: `{excerpt_path}`\n')
                if excerpt:
                    f.write(f'\n```\n{excerpt[:3000]}\n```\n')
                f.write('\n')

    def _render(self):
        render_dashboard(self.states, self.run_dir, self.start_time)

    def write_investigation_summary(self):
        failed = [s for s in self.states.values() if s.state in ('failed', 'excluded')]
        timing_warn = [
            s for s in self.states.values()
            if s.state == 'done' and
               (s.investigation or {}).get('qor', {}).get('slack_ps', 0) is not None and
               (s.investigation or {}).get('qor', {}).get('slack_ps', 0) < 0
        ]

        path = os.path.join(self.run_dir, 'investigation.md')
        cats: collections.Counter = collections.Counter(
            (s.investigation or {}).get('category', s.category or 'failed')
            for s in failed
        )

        lines = [f'# Synthesis Failure Investigation — {utcnow()}', '']
        lines += ['## Summary', f'- Total failures/excluded: {len(failed)}']
        for cat, count in cats.most_common():
            lines.append(f'  - {cat}: {count}')
        lines.append('')

        if timing_warn:
            lines += ['## Timing violations in passed runs',
                      '| Config | Slack (ps) | TNS (ps) | Failing Paths |',
                      '|---|---|---|---|']
            for s in timing_warn:
                qor = (s.investigation or {}).get('qor', {})
                lines.append(
                    f"| `{s.config.config_id}` | {qor.get('slack_ps')} | "
                    f"{qor.get('tns_ps')} | {qor.get('failing_paths')} |"
                )
            lines.append('')

        if failed:
            lines += ['## Failure detail', '']
            for s in failed:
                inv = s.investigation or {}
                cat = inv.get('category', s.category or '?')
                lines += [f'### {s.config.config_id}', '']
                lines += [
                    f'- **Category**: {cat}',
                    f'- **Failing step**: {s.failed_step or "?"}',
                    f'- **Exit code**: {s.exit_code}',
                ]
                if inv.get('postcondition_failures'):
                    lines.append(
                        f"- **Postcondition failures**: "
                        f"{', '.join(inv['postcondition_failures'])}"
                    )
                if inv.get('triage_note'):
                    lines.append(f"- **Triage**: {inv['triage_note']}")
                errs = inv.get('genus_log_errors', [])
                if errs:
                    lines += ['', '**Genus errors:**', '```']
                    lines += errs[:10]
                    lines += ['```']
                if inv.get('key_excerpt_path'):
                    lines.append(f"- **Excerpt log**: `{inv['key_excerpt_path']}`")
                lines.append('')

        atomic_write(path, '\n'.join(lines))
        print(f"\nInvestigation report → {path}")


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    lake_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    default_sh = os.path.join(lake_root, 'ASPLOS_EXP', 'all_experiments_thesis.sh')
    pool_runs_dir = os.path.join(lake_root, 'ASPLOS_EXP', 'synth_pool_runs')

    parser = argparse.ArgumentParser(
        description='Parallel synthesis runner for the thesis sweep.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Dry-run discovery — parse .sh, print plan, touch nothing
  python ASPLOS_EXP/run_synth_pool.py --phase discover --dry-run

  # Real discovery — materialize graphs, write configs.json (~5 min)
  python ASPLOS_EXP/run_synth_pool.py --phase discover

  # Pilot: skip already-done configs, run 1
  python ASPLOS_EXP/run_synth_pool.py --filter 'fw_4_inp_2_outp_2$' --limit 1 --jobs 1

  # Force re-run a specific config
  python ASPLOS_EXP/run_synth_pool.py --filter 'fw_4_inp_2_outp_2$' --force ''

  # Retry all license failures from a prior run
  python ASPLOS_EXP/run_synth_pool.py --rerun-failed license_failure \\
      --prev-run ASPLOS_EXP/synth_pool_runs/latest

  # Full sweep
  python ASPLOS_EXP/run_synth_pool.py --jobs 4 --step-timeout-min 30

NOTE: Discovery invokes create_mflowgen_experiments.py which does `rm -rf`
  the design folder under gf_physical_design/NEW/ on each run. Build-dir
  synthesis results are NOT affected, but local edits to those folders will
  be lost. Use --phase run --prev-run <dir> to skip re-discovery.
        """,
    )

    # Discovery
    g = parser.add_argument_group('Discovery')
    g.add_argument('--sh-script', default=default_sh, metavar='PATH',
                   help='Path to all_experiments_thesis.sh')
    g.add_argument('--build-root', action='append', dest='build_roots', metavar='PATH',
                   help='Restrict to this build root (repeatable); default: all in .sh')
    g.add_argument('--filter', metavar='REGEX',
                   help='Keep only configs whose config_id matches this regex')
    g.add_argument('--limit', type=int, metavar='N',
                   help='After filter, cap to first N configs')

    # Execution
    g = parser.add_argument_group('Execution')
    g.add_argument('--jobs', type=int, default=4, metavar='N',
                   help='Concurrent synthesis runs (default: 4)')
    g.add_argument('--phase',
                   choices=['discover', 'run', 'both', 'investigate'],
                   default='both',
                   help='Phase(s) to execute (default: both)')
    g.add_argument('--dry-run', action='store_true',
                   help='Print plan; do not call make or create_mflowgen_experiments.py')
    g.add_argument('--force', nargs='?', const='', metavar='PATTERN',
                   help='Re-run even if step is done. '
                        'Optional PATTERN is a regex over config_id (default: all)')
    g.add_argument('--rerun-failed', metavar='CATEGORIES',
                   help='Comma-separated categories to retry '
                        '(e.g. failed,timeout,license_failure). Use "all" to include excluded.')
    g.add_argument('--prev-run', metavar='PATH',
                   help='Prior run dir (required with --phase run/investigate or --rerun-failed)')
    g.add_argument('--step-timeout-min', type=float, metavar='N',
                   help='Kill synthesis step after N minutes (off by default; 30 recommended)')

    # Output
    g = parser.add_argument_group('Output')
    g.add_argument('--run-dir', metavar='PATH',
                   help='Override output dir (default: synth_pool_runs/<UTC-ts>/)')
    g.add_argument('--no-dashboard', action='store_true',
                   help='Skip dashboard.md rendering')
    g.add_argument('--quiet', action='store_true',
                   help='Suppress per-step progress on console (logs still written)')

    args = parser.parse_args()

    # Validate
    if not os.path.isfile(args.sh_script):
        parser.error(f"--sh-script not found: {args.sh_script}")

    filter_re = None
    if args.filter:
        try:
            filter_re = re.compile(args.filter)
        except re.error as e:
            parser.error(f"--filter regex error: {e}")

    # Run dir
    if args.run_dir:
        run_dir = args.run_dir
    else:
        ts = datetime.datetime.utcnow().strftime('%Y%m%dT%H%M%SZ')
        run_dir = os.path.join(pool_runs_dir, ts)

    if not args.dry_run:
        os.makedirs(run_dir, exist_ok=True)
        latest = os.path.join(pool_runs_dir, 'latest')
        try:
            if os.path.islink(latest):
                os.unlink(latest)
            os.symlink(run_dir, latest)
        except Exception:
            pass

    print(f"Run dir: {run_dir}")
    start_time = time.time()

    # Parse .sh
    invocations = parse_sh_file(args.sh_script)
    print(f"Parsed {len(invocations)} invocations from {os.path.basename(args.sh_script)}")

    # ── Phase: investigate ────────────────────────────────────────────────────
    if args.phase == 'investigate':
        if not args.prev_run:
            parser.error("--phase investigate requires --prev-run <path>")
        prev_cfgs = os.path.join(args.prev_run, 'configs.json')
        if not os.path.isfile(prev_cfgs):
            parser.error(f"No configs.json at {args.prev_run}")
        with open(prev_cfgs) as f:
            configs_data = json.load(f)
        dispatcher = Dispatcher(run_dir=args.prev_run, start_time=start_time,
                                no_dashboard=args.no_dashboard)
        for cd in configs_data:
            cid_safe = re.sub(r'[/ ]+', '__', cd['config_id'])
            status_path = os.path.join(args.prev_run, 'configs', cid_safe, 'status.json')
            if not os.path.isfile(status_path):
                continue
            with open(status_path) as f:
                st = json.load(f)
            if st.get('state') not in ('failed', 'excluded'):
                continue
            config = Config(**{k: cd[k] for k in dataclasses.fields(Config)
                               if k in cd})
            config_run_dir = os.path.dirname(status_path)
            synth_step = st.get('synth_step') or resolve_synth_step(config.build_dir)
            inv = investigate_failure(
                config.build_dir, config_run_dir,
                synth_step, st.get('failed_step')
            )
            st['investigation'] = inv
            atomic_write_json(status_path, st)
            print(f"  {cd['config_id']} → {inv.get('category')}")
        dispatcher.states = {}  # just for the summary writer
        dispatcher.write_investigation_summary()
        return

    # ── Discovery ─────────────────────────────────────────────────────────────
    if args.phase in ('discover', 'both'):
        configs = discover_configs(
            invocations=invocations,
            run_dir=run_dir,
            dry_run=args.dry_run,
            lake_root=lake_root,
            filter_re=filter_re,
            limit=args.limit,
            build_root_filter=args.build_roots,
        )
        if not args.dry_run:
            configs_json_path = os.path.join(run_dir, 'configs.json')
            atomic_write_json(configs_json_path, [
                dataclasses.asdict(c) for c in configs
            ])
            print(f"Wrote {configs_json_path} ({len(configs)} configs)")
        if args.phase == 'discover' or args.dry_run:
            return

    elif args.phase == 'run':
        if not args.prev_run:
            parser.error("--phase run requires --prev-run <path>")
        with open(os.path.join(args.prev_run, 'configs.json')) as f:
            configs_data = json.load(f)
        configs = [Config(**cd) for cd in configs_data]
        if filter_re:
            configs = [c for c in configs if filter_re.search(c.config_id)]
        if args.limit:
            configs = configs[:args.limit]
        print(f"Loaded {len(configs)} configs from {args.prev_run}")

    # ── --rerun-failed filter ─────────────────────────────────────────────────
    if args.rerun_failed:
        if not args.prev_run:
            parser.error("--rerun-failed requires --prev-run")
        categories = set(args.rerun_failed.split(','))
        include_excluded = 'all' in categories
        filtered: List[Config] = []
        for c in configs:
            cid_safe = re.sub(r'[/ ]+', '__', c.config_id)
            sp = os.path.join(args.prev_run, 'configs', cid_safe, 'status.json')
            if not os.path.isfile(sp):
                continue
            with open(sp) as f:
                st = json.load(f)
            s_state = st.get('state', '')
            cat = st.get('category') or (st.get('investigation') or {}).get('category', '')
            want = (
                (s_state == 'failed' and (include_excluded or cat in categories)) or
                (s_state == 'excluded' and include_excluded)
            )
            if want:
                filtered.append(c)
        configs = filtered
        print(f"Filtered to {len(configs)} configs for --rerun-failed={args.rerun_failed}")
        if args.force is None:
            args.force = ''

    # ── Run ───────────────────────────────────────────────────────────────────
    if not configs:
        print("No configs to run.")
        return

    print(f"\n{'='*60}")
    print(f"RUNNING synthesis on {len(configs)} configs with --jobs={args.jobs}")
    if args.step_timeout_min:
        print(f"Step timeout: {args.step_timeout_min} min")
    print(f"{'='*60}\n")

    dispatcher = Dispatcher(run_dir=run_dir, start_time=start_time,
                            no_dashboard=args.no_dashboard)
    initial_states = {c.config_id: RunState(config=c) for c in configs}
    dispatcher.register(initial_states)

    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
        futures = {
            executor.submit(run_one, c, run_dir, args, dispatcher): c
            for c in configs
        }
        try:
            for future in concurrent.futures.as_completed(futures):
                c = futures[future]
                try:
                    s = future.result()
                    print(f"[COMPLETE] {c.config_id} → {s.state} "
                          f"({fmt_duration(s.duration_s)})")
                except Exception as e:
                    print(f"[EXCEPTION] {c.config_id}: {e}", file=sys.stderr)
        except KeyboardInterrupt:
            print('\nInterrupted — pending jobs cancelled.', file=sys.stderr)
            for fut in futures:
                fut.cancel()

    # Final outputs
    dispatcher._render()  # force final dashboard
    dispatcher.write_investigation_summary()

    counts: collections.Counter = collections.Counter(
        s.state for s in dispatcher.states.values()
    )
    print(f"\n{'='*60}")
    print(f"DONE  done={counts['done']}  failed={counts['failed']}  "
          f"excluded={counts['excluded']}  skipped={counts['skipped']}")
    print(f"Wall time: {fmt_duration(time.time() - start_time)}")
    print(f"Run dir  : {run_dir}")
    print(f"Dashboard: {os.path.join(run_dir, 'dashboard.md')}")
    if counts['failed'] + counts['excluded']:
        print(f"Failures : {os.path.join(run_dir, 'failures.md')}")
        print(f"Investig : {os.path.join(run_dir, 'investigation.md')}")
    print(f"{'='*60}\n")


if __name__ == '__main__':
    main()
