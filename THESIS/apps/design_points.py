"""Single-level and (eventually) two-level memory design points.

Each ``DesignPoint`` maps to one row in ``tab:ul_design_points`` and to
one ``THESIS_BUILDS/*/thesis_sweep_700/<config>`` directory that the
extractor already reads. The harness cross-iterates
``DESIGNS_SINGLE_LEVEL × APPS`` and drops one ``results.json`` per cell
under ``THESIS/data/apps/<design_id>/<app_id>/``.

The 8 baseline points below are the round-trip-validated set from
``THESIS_ROUNDTRIP_PROGRESS.md`` (2026-05-08 snapshot). Table generators
in ``pipeline/generators.py`` look up per-design PPA by matching
(experiment, storage_cap, data_width, fw, inp, outp) against the
extractor DataFrame — ``sweep_name`` is kept for provenance but not
required to match a specific dir string.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional


@dataclass(frozen=True)
class DesignPoint:
    id: str
    display: str
    # Experiment dir under THESIS_BUILDS/ that owns this build.
    experiment: str          # "PORT_EXP" | "MEMORY_EXP" | ...
    sweep_group: str         # e.g. "thesis_sweep_700"
    # Architectural parameters used both to identify the build and to
    # render the design-points table.
    fetch_width: int
    storage_cap_bytes: int
    data_width: int
    in_ports: int
    out_ports: int
    # ``dual_port`` is not encoded in the sweep-dir name — the
    # per-build params.json doesn't emit it either. Set from the
    # roundtrip notes; None means "not verified from artifact."
    dual_port: Optional[bool] = None
    roundtrip_validated: bool = False


# 8 configs already round-trip-validated on conv_3_3 (see
# THESIS_ROUNDTRIP_PROGRESS.md rows 1-8). Populated 2026-07-13.
DESIGNS_SINGLE_LEVEL: list[DesignPoint] = [
    DesignPoint(
        id="port_fw4_dw16_sc8k_sp2x2",
        display="PORT fw4 dw16 8KB SP 2x2 (baseline)",
        experiment="PORT_EXP", sweep_group="thesis_sweep_700",
        fetch_width=4, storage_cap_bytes=8192, data_width=16,
        in_ports=2, out_ports=2, dual_port=False, roundtrip_validated=True,
    ),
    DesignPoint(
        id="port_fw2_dw32_sc8k_dp2x2",
        display="PORT fw2 dw32 8KB DP 2x2 (BW-required DP)",
        experiment="PORT_EXP", sweep_group="thesis_sweep_700",
        fetch_width=2, storage_cap_bytes=8192, data_width=32,
        in_ports=2, out_ports=2, dual_port=True, roundtrip_validated=True,
    ),
    DesignPoint(
        id="mem_fw2_dw16_sc2k_sp1x1",
        display="MEM fw2 dw16 2KB SP 1x1 (small SP)",
        experiment="MEMORY_EXP", sweep_group="thesis_sweep_700",
        fetch_width=2, storage_cap_bytes=2048, data_width=16,
        in_ports=1, out_ports=1, dual_port=False, roundtrip_validated=True,
    ),
    DesignPoint(
        id="mem_fw1_dw16_sc4k_dp1x1",
        display="MEM fw1 dw16 4KB DP 1x1 (fw=1 regfile)",
        experiment="MEMORY_EXP", sweep_group="thesis_sweep_700",
        fetch_width=1, storage_cap_bytes=4096, data_width=16,
        in_ports=1, out_ports=1, dual_port=True, roundtrip_validated=True,
    ),
    DesignPoint(
        id="port_fw4_dw8_sc8k_sp2x2",
        display="PORT fw4 dw8 8KB SP 2x2 (narrow dw)",
        experiment="PORT_EXP", sweep_group="thesis_sweep_700",
        fetch_width=4, storage_cap_bytes=8192, data_width=8,
        in_ports=2, out_ports=2, dual_port=False, roundtrip_validated=True,
    ),
    DesignPoint(
        id="port_fw4_dw32_sc8k_sp2x2",
        display="PORT fw4 dw32 8KB SP 2x2 (wide dw)",
        experiment="PORT_EXP", sweep_group="thesis_sweep_700",
        fetch_width=4, storage_cap_bytes=8192, data_width=32,
        in_ports=2, out_ports=2, dual_port=False, roundtrip_validated=True,
    ),
    DesignPoint(
        id="mem_fw2_dw16_sc4k_dp2x2",
        display="MEM fw2 dw16 4KB DP 2x2",
        experiment="MEMORY_EXP", sweep_group="thesis_sweep_700",
        fetch_width=2, storage_cap_bytes=4096, data_width=16,
        in_ports=2, out_ports=2, dual_port=True, roundtrip_validated=True,
    ),
    DesignPoint(
        id="mem_fw4_dw16_sc8k_sp2x2",
        display="MEM fw4 dw16 8KB SP 2x2",
        experiment="MEMORY_EXP", sweep_group="thesis_sweep_700",
        fetch_width=4, storage_cap_bytes=8192, data_width=16,
        in_ports=2, out_ports=2, dual_port=False, roundtrip_validated=True,
    ),
]


# Two-level = single-level upper tier × 3 Pond lower-tier points.
# TODO: needs a Pond spec factory (see PIPELINE.md TODO #6). Empty until
# the Pond mflowgen construct is stood up.
DESIGNS_TWO_LEVEL: list[DesignPoint] = []
