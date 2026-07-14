"""Compose per-tile PPA into per-app CGRA-level PPA.

The exploration figures (single_level_*, two_level_*) need per-app cost
at the *chip* level, not the per-memtile level. That means:

    chip_area = N_mem_tiles × mem_tile_area
              + N_pe_tiles  × pe_tile_area
              + interconnect_overhead

per the 3:1 PE:Mem ratio and 8×16 grid budget in the thesis narrative
(§ around line 2019 of main_thesis.tex).

**Skeleton.** ``compose_one`` returns a placeholder dict shaped exactly
like the final CGRA-level result so downstream generators can be written
against the schema now.

TODO:
  1. Get authoritative PE-tile PPA numbers (currently no ``pd/thesis/pe-tile/``
     mflowgen construct exists — need to stand one up or borrow from the
     upstream AHA build).
  2. Read per-app tile counts from the clockwork manifest that
     ``run_clockwork.py`` already produces.
  3. Decide how to charge interconnect (fraction of switchbox from the
     existing CGRA build, or lumped constant).
"""

from __future__ import annotations

from pathlib import Path

# TODO: replace with PPA numbers from a real pd/thesis/pe-tile/ build.
PE_TILE_AREA_UM2_PLACEHOLDER = 2500.0
PE_TILE_POWER_W_PLACEHOLDER = 0.0012

# TODO: interconnect share — 0 until we have the SB PPA numbers referenced by
# fig:interconnect_fork_ready_valid_data.
INTERCONNECT_OVERHEAD_FRAC = 0.0


def compose_one(memtile_result: dict, tile_count_mem: int, tile_count_pe: int) -> dict:
    """Turn a per-memtile result dict into a per-app CGRA-level dict.

    ``memtile_result`` is one ``results.json`` from ``run_matrix.py``.
    Returns a dict with the same keys plus ``chip_*`` roll-ups.
    """
    mem_area = memtile_result.get("pnr_area_um2") or memtile_result.get("synth_area_um2") or 0.0
    mem_power = memtile_result.get("pnr_power_w") or memtile_result.get("synth_power_w") or 0.0

    chip_area = (
        tile_count_mem * mem_area
        + tile_count_pe * PE_TILE_AREA_UM2_PLACEHOLDER
    )
    chip_area *= (1 + INTERCONNECT_OVERHEAD_FRAC)

    chip_power = (
        tile_count_mem * mem_power
        + tile_count_pe * PE_TILE_POWER_W_PLACEHOLDER
    )
    chip_power *= (1 + INTERCONNECT_OVERHEAD_FRAC)

    return {
        **memtile_result,
        "tile_count_mem": tile_count_mem,
        "tile_count_pe": tile_count_pe,
        "chip_area_um2": chip_area,
        "chip_power_w": chip_power,
        # perf/util pass through unchanged — they're already app-level.
    }
