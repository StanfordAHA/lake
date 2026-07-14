#!/usr/bin/env python3
"""Walk a top-level THESIS_BUILDS-style directory and collect synth + PnR
area / power numbers for every build it finds.

Layout assumed:

    <top>/<EXPERIMENT>/<SWEEP_GROUP>/<sweep_name>/...

For example:

    THESIS_BUILDS/AFFINE_PATTERN_GENERATOR_EXP/thesis_sweep_700/
        storage_cap_8192_data_width_16_ccw_64_dim_1_fw_1_msw_1024/
            7-cadence-genus-synthesis/results_syn/final_area.rpt
            13-synopsys-ptpx-synth/outputs/power.hier
            21-cadence-innovus-signoff/reports/signoff.area.rpt
            32-synopsys-ptpx-gl/reports/lakespec.power.hier.rpt

Reports are picked up if present and quietly skipped if missing, so partial
flows (synth-only) are handled.

Output is a CSV (or stdout). Schema:
    experiment, sweep_group, sweep_name, frequency_mhz,
    <one column per parsed sweep parameter>,
    synth_cell_area_um2, synth_total_area_um2,
    pnr_total_area_um2, pnr_macro_area_um2,
    synth_power_w, pnr_power_w,
    build_dir
"""

from __future__ import annotations

import argparse
import csv
import json
import re
import sys
from pathlib import Path

REPORT_PATHS = {
    "synth_area_rpt": Path("7-cadence-genus-synthesis/results_syn/final_area.rpt"),
    "synth_power_rpt": Path("13-synopsys-ptpx-synth/outputs/power.hier"),
    "pnr_area_rpt": Path("21-cadence-innovus-signoff/reports/signoff.area.rpt"),
    "pnr_power_rpt": Path("32-synopsys-ptpx-gl/reports/lakespec.power.hier.rpt"),
    "synth_qor_rpt": Path("7-cadence-genus-synthesis/results_syn/final_qor.rpt"),
    "synth_time_rpt": Path("7-cadence-genus-synthesis/results_syn/final_time.rpt"),
}

NUMERIC_RE = re.compile(r"^-?\d+(?:\.\d+)?$")


def parse_sweep_name(name: str) -> dict[str, int | float]:
    """Parse `key_words_<num>_key_words_<num>...` directory names.

    e.g. ``storage_cap_8192_data_width_16_ccw_64_dim_1_fw_1_msw_1024`` ->
    ``{'storage_cap': 8192, 'data_width': 16, 'ccw': 64, 'dim': 1,
       'fw': 1, 'msw': 1024}``.
    """
    tokens = name.split("_")
    params: dict[str, int | float] = {}
    key_buf: list[str] = []
    for tok in tokens:
        if NUMERIC_RE.match(tok) and key_buf:
            key = "_".join(key_buf)
            val: int | float = float(tok) if "." in tok else int(tok)
            params[key] = val
            key_buf = []
        else:
            key_buf.append(tok)
    if key_buf:
        params["_".join(key_buf)] = ""  # trailing label with no value
    return params


def _first_lakespec_row(rpt: Path) -> list[str] | None:
    """Return the first whitespace-split line beginning with ``lakespec``."""
    if not rpt.is_file():
        return None
    try:
        with rpt.open() as f:
            for line in f:
                stripped = line.lstrip()
                if stripped.startswith("lakespec"):
                    return stripped.split()
    except OSError:
        return None
    return None


def extract_synth_area(rpt: Path) -> tuple[float | None, float | None]:
    """Return (cell_area, total_area) in um^2 from genus final_area.rpt.

    Columns: Instance Module Cell-Count Cell-Area Net-Area Total-Area
    """
    row = _first_lakespec_row(rpt)
    if row is None or len(row) < 6:
        return None, None
    try:
        return float(row[3]), float(row[5])
    except (ValueError, IndexError):
        return None, None


def extract_pnr_area(rpt: Path) -> tuple[float | None, float | None]:
    """Return (total_area, macro_area) from innovus signoff.area.rpt.

    Matches the columns the existing collect_power_area.sh script reads:
    field 3 = total area, field 10 = macro area.
    """
    row = _first_lakespec_row(rpt)
    if row is None:
        return None, None
    total = macro = None
    if len(row) >= 3:
        try:
            total = float(row[2])
        except ValueError:
            pass
    if len(row) >= 10:
        try:
            macro = float(row[9])
        except ValueError:
            pass
    return total, macro


def extract_total_power(rpt: Path) -> float | None:
    """Return total power (W) for the top-level lakespec row.

    Matches collect_power_area.sh: field 5 of the first lakespec row.
    """
    row = _first_lakespec_row(rpt)
    if row is None or len(row) < 5:
        return None
    try:
        return float(row[4])
    except ValueError:
        return None


def extract_synth_timing(qor_rpt: Path) -> tuple[float | None, float | None]:
    """Return (clock_period_ps, worst_slack_ps) from genus final_qor.rpt.

    File has two ``ideal_clock`` rows in different tables:

        Clock    Period
        -------------------
        ideal_clock 1428.0                  ← 2 tokens → period row

                Cost              Critical         Violating
               Group             Path Slack  TNS     Paths
        ---------------------------------------------------------
        ideal_clock       495.3   0.0          0    ← 4 tokens → slack row

    Disambiguate by token count on the ``ideal_clock`` line — cleaner than
    tracking table state across the two-line header of the slack table.
    """
    if not qor_rpt.is_file():
        return None, None
    period = slack = None
    try:
        with qor_rpt.open() as f:
            for line in f:
                stripped = line.strip()
                if not stripped.startswith("ideal_clock"):
                    continue
                parts = stripped.split()
                try:
                    val = float(parts[1])
                except (IndexError, ValueError):
                    continue
                if len(parts) == 2 and period is None:
                    period = val
                elif len(parts) >= 4 and slack is None:
                    slack = val
    except OSError:
        pass
    return period, slack


_TIME_START_RE = re.compile(r"^\s*Start-point\s*:\s*(.+?)\s*$")
_TIME_END_RE = re.compile(r"^\s*End-point\s*:\s*(.+?)\s*$")


def extract_crit_path_endpoints(time_rpt: Path) -> tuple[str | None, str | None]:
    """Return (startpoint, endpoint) of the first (i.e. worst) path in
    genus final_time.rpt. Genus emits paths in decreasing criticality."""
    if not time_rpt.is_file():
        return None, None
    start = end = None
    try:
        with time_rpt.open() as f:
            for line in f:
                if start is None:
                    m = _TIME_START_RE.match(line)
                    if m:
                        start = m.group(1)
                        continue
                if end is None:
                    m = _TIME_END_RE.match(line)
                    if m:
                        end = m.group(1)
                if start is not None and end is not None:
                    break
    except OSError:
        pass
    return start, end


def find_builds(top: Path) -> list[Path]:
    """Find every sweep build under ``top`` by locating the synth area report.

    The marker has multiple path components, so step up that many parents to
    reach the sweep directory itself.
    """
    marker = REPORT_PATHS["synth_area_rpt"]
    up = len(marker.parts)  # parents[up-1] strips the marker tail
    builds = sorted({p.parents[up - 1] for p in top.glob(f"*/*/*/{marker}")})
    return builds


def collect(top: Path) -> tuple[list[dict], list[str]]:
    rows: list[dict] = []
    param_keys: set[str] = set()
    for build in find_builds(top):
        rel = build.relative_to(top)
        if len(rel.parts) < 3:
            continue
        experiment, sweep_group, sweep_name = rel.parts[0], rel.parts[1], rel.parts[2]

        params = parse_sweep_name(sweep_name)
        param_keys.update(params.keys())

        # Pull frequency from params.json when available — directory name typically
        # only carries the architectural sweep params.
        freq = None
        params_json = build / "params.json"
        if params_json.is_file():
            try:
                pj = json.loads(params_json.read_text())
                freq = pj.get("frequency")
            except (OSError, json.JSONDecodeError):
                pass

        synth_cell, synth_total = extract_synth_area(build / REPORT_PATHS["synth_area_rpt"])
        pnr_total, pnr_macro = extract_pnr_area(build / REPORT_PATHS["pnr_area_rpt"])
        synth_pwr = extract_total_power(build / REPORT_PATHS["synth_power_rpt"])
        pnr_pwr = extract_total_power(build / REPORT_PATHS["pnr_power_rpt"])
        clk_period, wns = extract_synth_timing(build / REPORT_PATHS["synth_qor_rpt"])
        crit_start, crit_end = extract_crit_path_endpoints(build / REPORT_PATHS["synth_time_rpt"])
        crit_delay = None
        if clk_period is not None and wns is not None:
            crit_delay = clk_period - wns

        rows.append({
            "experiment": experiment,
            "sweep_group": sweep_group,
            "sweep_name": sweep_name,
            "frequency_mhz": freq,
            **params,
            "synth_cell_area_um2": synth_cell,
            "synth_total_area_um2": synth_total,
            "pnr_total_area_um2": pnr_total,
            "pnr_macro_area_um2": pnr_macro,
            "synth_power_w": synth_pwr,
            "pnr_power_w": pnr_pwr,
            "clock_period_ps": clk_period,
            "wns_ps": wns,
            "crit_path_delay_ps": crit_delay,
            "crit_path_startpoint": crit_start,
            "crit_path_endpoint": crit_end,
            "build_dir": str(build),
        })

    fieldnames = (
        ["experiment", "sweep_group", "sweep_name", "frequency_mhz"]
        + sorted(param_keys)
        + [
            "synth_cell_area_um2",
            "synth_total_area_um2",
            "pnr_total_area_um2",
            "pnr_macro_area_um2",
            "synth_power_w",
            "pnr_power_w",
            "clock_period_ps",
            "wns_ps",
            "crit_path_delay_ps",
            "crit_path_startpoint",
            "crit_path_endpoint",
            "build_dir",
        ]
    )
    return rows, fieldnames


def write_csv(rows: list[dict], fieldnames: list[str], out) -> None:
    writer = csv.DictWriter(out, fieldnames=fieldnames, extrasaction="ignore")
    writer.writeheader()
    for r in rows:
        writer.writerow(r)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("top", type=Path, help="Top-level builds directory (e.g. THESIS_BUILDS).")
    ap.add_argument("-o", "--output", type=Path, help="CSV output path. Defaults to stdout.")
    args = ap.parse_args(argv)

    if not args.top.is_dir():
        print(f"error: {args.top} is not a directory", file=sys.stderr)
        return 2

    rows, fieldnames = collect(args.top)
    if not rows:
        print(f"warning: no builds found under {args.top}", file=sys.stderr)

    if args.output:
        with args.output.open("w", newline="") as f:
            write_csv(rows, fieldnames, f)
        print(f"wrote {len(rows)} rows to {args.output}", file=sys.stderr)
    else:
        write_csv(rows, fieldnames, sys.stdout)
    return 0


if __name__ == "__main__":
    sys.exit(main())
