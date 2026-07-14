"""Registry of every figure + table in ``main_thesis.tex``.

Each entry maps a stable ``id`` to:
  - ``label``: the LaTeX ``\\label{...}`` string (fig: or tab: prefix)
  - ``output``: path *relative to* ``THESIS/output/`` that ``\\includegraphics``
    or ``\\input`` in the tex points at. Must match the tex exactly.
  - ``kind``:  "figure" | "table"
  - ``source``: one-line description of what data the artifact needs
  - ``status``: "real" | "bogus" | "manual" — see below
  - ``generator``: callable(ctx, outpath) or None
  - ``notes``: TODOs / open questions

Status semantics:
  - real   → generator writes a real artifact from data. If it raises
             MissingDataError, orchestrator falls back to BOGUS + logs.
  - bogus  → intentionally BOGUS (data doesn't exist yet). Orchestrator
             always writes a red-watermark placeholder + notes it.
  - manual → hand-authored (block diagrams from a drawing tool, curated
             tables). Orchestrator SKIPS these — they must live in the
             tex source area already. Registry records them so nothing
             is silently forgotten.

Add a new figure by appending an ENTRY here. The comment above each
grouping documents which chapter/section the artifact belongs to.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Callable, Optional

from . import generators as g

Generator = Callable[["g.GenContext", "object"], None]


@dataclass
class Entry:
    id: str
    label: str
    output: str
    kind: str
    source: str
    status: str
    generator: Optional[Generator] = None
    notes: str = ""


# ---------------------------------------------------------------------------
# Ch. 1–3 background: hand-drawn block diagrams (manual, live in tex tree).
# ---------------------------------------------------------------------------
BACKGROUND_MANUAL = [
    Entry("comp_systems_old", "fig:accelerator_evolution_old",
          "figures/comp_systems_old.pdf", "figure",
          "hand-drawn block diagram (early computer system w/ expansion cards)",
          "manual"),
    Entry("comp_systems_modern", "fig:accelerator_evolution",
          "figures/comp_systems_modern.pdf", "figure",
          "hand-drawn block diagram (mid-modern SoC w/ MMU + cache)", "manual"),
    Entry("comp_systems_new", "fig:accelerator_evolution",
          "figures/comp_systems_new.pdf", "figure",
          "hand-drawn block diagram (modern SoC w/ ML accelerator)", "manual"),
    Entry("end_to_end_gen_system", "fig:bg_end_to_end_system",
          "figures/end_to_end_gen_system.pdf", "figure",
          "hand-drawn block diagram (compiler-in-the-loop generator)", "manual"),
    Entry("basic_cgra", "fig:basic_cgra",
          "figures/basic_cgra.pdf", "figure",
          "hand-drawn CGRA fabric diagram", "manual"),
    Entry("basic_cgra_interconnections", "fig:cgra_topologies",
          "figures/basic_cgra_interconnections.pdf", "figure",
          "hand-drawn topologies (2D/toroidal/hierarchical mesh)", "manual"),
    Entry("basic_cgra_SB", "fig:cgra_sb_pipeline_regs",
          "figures/basic_cgra_SB.pdf", "figure",
          "hand-drawn Wilton switch box diagram", "manual"),
    Entry("basic_cgra_tile", "fig:cgra_tile",
          "figures/basic_cgra_tile.pdf", "figure",
          "hand-drawn CGRA tile layout", "manual"),
    Entry("cgra_software_flow", "fig:cgra_software_flow",
          "figures/cgra_software_flow.pdf", "figure",
          "hand-drawn software flow diagram", "manual"),
    Entry("cgra_interconnect_fork_ready_valid", "fig:interconnect_fork_ready_valid",
          "figures/cgra_interconnect_fork_ready_valid.pdf", "figure",
          "hand-drawn ready/valid forking diagram", "manual"),
    Entry("lake_software_flow", "fig:tool_flow",
          "figures/lake_software_flow.pdf", "figure",
          "hand-drawn Halide→Lake tool flow", "manual"),
    Entry("lake_system", "fig:lake_system",
          "figures/lake_system.pdf", "figure",
          "hand-drawn Lake system overview", "manual"),
    Entry("lake_simple_dual_port", "fig:simple_dual_port_code_and_diagram",
          "figures/lake_simple_dual_port.pdf", "figure",
          "hand-drawn dual-port streaming memory block diagram", "manual"),
    Entry("lake_components_iteration_domain", "fig:lake_components_iteration_domain",
          "figures/lake_components_iteration_domain.pdf", "figure",
          "hand-drawn IterationDomain schematic", "manual"),
    Entry("lake_components_address_generator", "fig:lake_components_address_generator",
          "figures/lake_components_address_generator.pdf", "figure",
          "hand-drawn AddressGenerator schematic (MAC form)", "manual"),
    Entry("lake_components_address_generator_custom", "fig:lake_components_address_generator_custom",
          "figures/lake_components_address_generator_custom.pdf", "figure",
          "hand-drawn AddressGenerator schematic (recurrence form)", "manual"),
    Entry("lake_components_schedule_generator", "fig:lake_components_schedule_generator",
          "figures/lake_components_schedule_generator.pdf", "figure",
          "hand-drawn ScheduleGenerator schematic", "manual"),
    Entry("lake_components_storage", "fig:lake_components_storage",
          "figures/lake_components_storage.pdf", "figure",
          "hand-drawn Storage schematic", "manual"),
    Entry("lake_components_port_vectorized_write", "fig:lake_components_port_vectorized_write",
          "figures/lake_components_port_vectorized_write.pdf", "figure",
          "hand-drawn vectorized-write Port schematic", "manual"),
    Entry("lake_components_port_vectorized_read", "fig:lake_components_port_vectorized_read",
          "figures/lake_components_port_vectorized_read.pdf", "figure",
          "hand-drawn vectorized-read Port schematic", "manual"),
    Entry("lake_strides_versus_deltas", "fig:strides_vs_deltas",
          "figures/lake_strides_versus_deltas.pdf", "figure",
          "hand-drawn strides-vs-deltas visualization", "manual"),
    Entry("lake_memory_hierarchy", "fig:memory_hierarchy_implementation_location",
          "figures/lake_memory_hierarchy.pdf", "figure",
          "hand-drawn CGRA two-level memory hierarchy diagram", "manual"),
]

# ---------------------------------------------------------------------------
# Ch. 4 Lake Component characterization (real — from THESIS_BUILDS synth).
# ---------------------------------------------------------------------------
COMPONENT_CHARACTERIZATION = [
    # Port
    Entry("port_area_vs_data_width", "fig:port_characterization_interface_width_area",
          "figures/port_characterization/PORT_EXP__synth_total_area_um2_vs_data_width.pdf",
          "figure", "PORT_EXP: synth area vs data_width, hue fw/vc",
          "real", g.port_area_vs_data_width),
    Entry("port_power_vs_data_width", "fig:port_characterization_interface_width_power",
          "figures/port_characterization/PORT_EXP__synth_power_w_vs_data_width.pdf",
          "figure", "PORT_EXP: synth power vs data_width",
          "real", g.port_power_vs_data_width,
          notes="TODO: needs power flow (ptpx-synth) — will fall back to BOGUS until run."),
    Entry("port_area_vs_vc", "fig:port_characterization_vectorization_area",
          "figures/port_characterization/PORT_EXP__synth_total_area_um2_vs_vc.pdf",
          "figure", "PORT_EXP: synth area vs vc",
          "real", g.port_area_vs_vc),
    Entry("port_power_vs_vc", "fig:port_characterization_vectorization_power",
          "figures/port_characterization/PORT_EXP__synth_power_w_vs_vc.pdf",
          "figure", "PORT_EXP: synth power vs vc",
          "real", g.port_power_vs_vc,
          notes="TODO: needs power flow."),

    # IterationDomain
    Entry("iter_dom_area_vs_dim", "fig:iteration_domain_characterization_dimensionality_area",
          "figures/iteration_domain_characterization_dimensionality_area.pdf",
          "figure", "ITERATION_DOMAIN_EXP: synth area vs dim, hue me",
          "real", g.iter_dom_area_vs_dim),
    Entry("iter_dom_power_vs_dim", "fig:iteration_domain_characterization_dimensionality_power",
          "figures/iteration_domain_characterization_dimensionality_power.pdf",
          "figure", "ITERATION_DOMAIN_EXP: synth power vs dim",
          "real", g.iter_dom_power_vs_dim,
          notes="TODO: needs power flow."),
    Entry("iter_dom_area_vs_max_extent", "fig:iteration_domain_characterization_max_extent_area",
          "figures/iteration_domain_characterization_max_extent_area.pdf",
          "figure", "ITERATION_DOMAIN_EXP: synth area vs me, hue dim",
          "real", g.iter_dom_area_vs_max_extent),
    Entry("iter_dom_power_vs_max_extent", "fig:iteration_domain_characterization_max_extent_power",
          "figures/iteration_domain_characterization_max_extent_power.pdf",
          "figure", "ITERATION_DOMAIN_EXP: synth power vs me",
          "real", g.iter_dom_power_vs_max_extent,
          notes="TODO: needs power flow."),

    # Affine Pattern Generator
    Entry("affine_area_vs_dim", "fig:affine_pattern_generator_characterization_dimensionality_area",
          "figures/affine_pattern_generator_characterization_dimensionality_area.pdf",
          "figure", "AFFINE_PATTERN_GENERATOR_EXP: synth area vs dim, hue msw",
          "real", g.affine_area_vs_dim),
    Entry("affine_power_vs_dim", "fig:affine_pattern_generator_characterization_dimensionality_power",
          "figures/affine_pattern_generator_characterization_dimensionality_power.pdf",
          "figure", "AFFINE_PATTERN_GENERATOR_EXP: synth power vs dim",
          "real", g.affine_power_vs_dim,
          notes="TODO: needs power flow."),
    Entry("affine_area_vs_max_value", "fig:affine_pattern_generator_characterization_max_value_area",
          "figures/affine_pattern_generator_characterization_max_value_area.pdf",
          "figure", "AFFINE_PATTERN_GENERATOR_EXP: synth area vs msw, hue dim",
          "real", g.affine_area_vs_max_value),
    Entry("affine_power_vs_max_value", "fig:affine_pattern_generator_characterization_max_value_power",
          "figures/affine_pattern_generator_characterization_max_value_power.pdf",
          "figure", "AFFINE_PATTERN_GENERATOR_EXP: synth power vs msw",
          "real", g.affine_power_vs_max_value,
          notes="TODO: needs power flow."),
    # Stride / offset sweeps are called out in tex but AFFINE_EXP doesn't
    # yet vary them independently — reuse msw as the width bound and
    # BOGUS until a dedicated sweep is added.
    Entry("affine_area_vs_stride", "fig:affine_pattern_generator_characterization_stride_area",
          "figures/affine_pattern_generator_characterization_stride_area.pdf",
          "figure", "AFFINE_PATTERN_GENERATOR_EXP: area vs max stride",
          "bogus", None,
          notes="TODO: add a per-stride sweep (currently msw bounds both stride+offset)."),
    Entry("affine_power_vs_stride", "fig:affine_pattern_generator_characterization_stride_power",
          "figures/affine_pattern_generator_characterization_stride_power.pdf",
          "figure", "AFFINE_PATTERN_GENERATOR_EXP: power vs max stride",
          "bogus", None,
          notes="TODO: per-stride sweep + power flow."),
    Entry("affine_area_vs_offset", "fig:affine_pattern_generator_characterization_offset_area",
          "figures/affine_pattern_generator_characterization_offset_area.pdf",
          "figure", "AFFINE_PATTERN_GENERATOR_EXP: area vs max offset",
          "bogus", None,
          notes="TODO: add per-offset sweep."),
    Entry("affine_power_vs_offset", "fig:affine_pattern_generator_characterization_offset_power",
          "figures/affine_pattern_generator_characterization_offset_power.pdf",
          "figure", "AFFINE_PATTERN_GENERATOR_EXP: power vs max offset",
          "bogus", None,
          notes="TODO: per-offset sweep + power flow."),

    # Schedule Generator (commented out in tex — kept in registry for future).
    Entry("sched_gen_dim", "fig:schedule_generator_characterization_dimensionality",
          "figures/schedule_generator_characterization_dimensionality.pdf",
          "figure", "ScheduleGenerator area/power vs dim",
          "bogus", None,
          notes="TODO: dedicated SG sweep — currently rolled into affine PG."),
    Entry("sched_gen_max_extent", "fig:schedule_generator_characterization_max_extent",
          "figures/schedule_generator_characterization_max_extent.pdf",
          "figure", "ScheduleGenerator area/power vs max extent",
          "bogus", None,
          notes="TODO: dedicated SG sweep."),

    # Memory Port + Storage
    Entry("memory_port_area_vs_interface_width",
          "fig:memory_port_characterization_interface_width_area",
          "figures/memory_port_characterization_interface_width_area.pdf",
          "figure", "MEMORY_EXP: synth area vs fw (interface width), hue storage_cap/data_width",
          "real", g.memport_area_vs_fw),
    Entry("memory_port_power_vs_interface_width",
          "fig:memory_port_characterization_interface_width_power",
          "figures/memory_port_characterization_interface_width_power.pdf",
          "figure", "MEMORY_EXP: synth power vs fw",
          "real", g.memport_power_vs_fw,
          notes="TODO: needs power flow."),
    Entry("storage_area_vs_capacity", "fig:storage_characterization_capacity_area",
          "figures/storage_characterization_capacity_area.pdf",
          "figure", "MEMORY_EXP: synth area vs storage_cap",
          "real", g.storage_area_vs_capacity),
    Entry("storage_power_vs_capacity", "fig:storage_characterization_capacity_power",
          "figures/storage_characterization_capacity_power.pdf",
          "figure", "MEMORY_EXP: synth power vs storage_cap",
          "real", g.storage_power_vs_capacity,
          notes="TODO: needs power flow."),
]

# ---------------------------------------------------------------------------
# Ch. 5 Application exploration figures — BOGUS until app-mapping data lands.
# ---------------------------------------------------------------------------
EXPLORATION_APPS = [
    Entry("single_level_power", "fig:single_level_power",
          "figures/single_level_power.pdf", "figure",
          "per-app power across single-level design points",
          "bogus", None,
          notes="TODO: needs an app-mapping harness that runs each of the "
                "10 apps in tab:exploration_applications on each single-level "
                "design point from tab:ul_design_points and reports power."),
    Entry("single_level_performance", "fig:single_level_performance",
          "figures/single_level_performance.pdf", "figure",
          "per-app perf across single-level design points", "bogus", None,
          notes="TODO: needs app-mapping harness."),
    Entry("single_level_area", "fig:single_level_area",
          "figures/single_level_area.pdf", "figure",
          "per-app area across single-level design points", "bogus", None,
          notes="TODO: area is per-design not per-app — clarify what chart shows."),
    Entry("single_level_utilization", "fig:single_level_utilization",
          "figures/single_level_utilization.pdf", "figure",
          "per-app utilization on single-level designs", "bogus", None,
          notes="TODO: define utilization metric (compute cycles / total cycles?)."),
    Entry("single_level_energy_efficiency", "fig:single_level_energy_efficiency",
          "figures/single_level_energy_efficiency.pdf", "figure",
          "per-app energy efficiency on single-level designs", "bogus", None,
          notes="TODO: perf/watt — needs perf + power both landed."),
    Entry("two_level_power", "fig:two_level_power",
          "figures/two_level_power.pdf", "figure",
          "per-app power across two-level design points", "bogus", None,
          notes="TODO: two-level design points aren't enumerated yet."),
    Entry("two_level_performance", "fig:two_level_performance",
          "figures/two_level_performance.pdf", "figure",
          "per-app perf across two-level design points", "bogus", None),
    Entry("two_level_area", "fig:two_level_area",
          "figures/two_level_area.pdf", "figure",
          "per-app area on two-level designs", "bogus", None),
    Entry("two_level_utilization", "fig:two_level_utilization",
          "figures/two_level_utilization.pdf", "figure",
          "per-app utilization on two-level designs", "bogus", None),
    Entry("two_level_energy_efficiency", "fig:two_level_energy_efficiency",
          "figures/two_level_energy_efficiency.pdf", "figure",
          "per-app energy efficiency on two-level designs", "bogus", None),

    # Latency-insensitive variants
    Entry("single_level_power_LI", "fig:single_level_power_LI",
          "figures/single_level_power_LI.pdf", "figure",
          "per-app power on single-level LI designs", "bogus", None,
          notes="TODO: LI variant requires ready/valid overlay app runs."),
    Entry("single_level_performance_LI", "fig:single_level_performance_LI",
          "figures/single_level_performance_LI.pdf", "figure",
          "per-app perf on single-level LI designs", "bogus", None),
    Entry("single_level_area_LI", "fig:single_level_area_LI",
          "figures/single_level_area_LI.pdf", "figure",
          "per-app area on single-level LI designs", "bogus", None),
    Entry("single_level_utilization_LI", "fig:single_level_utilization_LI",
          "figures/single_level_utilization_LI.pdf", "figure",
          "per-app utilization on single-level LI designs", "bogus", None),
    Entry("single_level_energy_efficiency_LI", "fig:single_level_energy_efficiency_LI",
          "figures/single_level_energy_efficiency_LI.pdf", "figure",
          "per-app energy efficiency on single-level LI designs", "bogus", None),
]

# ---------------------------------------------------------------------------
# LI ScheduleGenerator + interconnect characterization — BOGUS until sweeps run.
# ---------------------------------------------------------------------------
LI_CHARACTERIZATION = [
    Entry("li_sched_ports_area", "fig:latency_insensitive_schedule_generator_characterization_ports_area",
          "figures/latency_insensitive_schedule_generator_characterization_ports_area.pdf",
          "figure", "LI SG area vs port count", "bogus", None,
          notes="TODO: needs LI_SCHED_EXP sweep (build_dir not yet in THESIS_BUILDS)."),
    Entry("li_sched_ports_power", "fig:latency_insensitive_schedule_generator_characterization_ports_power",
          "figures/latency_insensitive_schedule_generator_characterization_ports_power.pdf",
          "figure", "LI SG power vs port count", "bogus", None),
    Entry("li_sched_loc_area", "fig:latency_insensitive_schedule_generator_characterization_loc_area",
          "figures/latency_insensitive_schedule_generator_characterization_loc_area.pdf",
          "figure", "LI SG area vs levels of comparison", "bogus", None),
    Entry("li_sched_loc_power", "fig:latency_insensitive_schedule_generator_characterization_loc_power",
          "figures/latency_insensitive_schedule_generator_characterization_loc_power.pdf",
          "figure", "LI SG power vs levels of comparison", "bogus", None),
    Entry("interconnect_fork_ready_valid_data", "fig:interconnect_fork_ready_valid_data",
          "figures/cgra_interconnect_fork_ready_valid_data.pdf", "figure",
          "SB PPA before/after ready/valid forking logic", "bogus", None,
          notes="TODO: needs pre/post SB PPA numbers from CGRA builds."),
]

# ---------------------------------------------------------------------------
# Tables — curated in tex today; registered so nothing is silently forgotten.
# ---------------------------------------------------------------------------
TABLES = [
    Entry("hardware_generator_tools", "tab:hardware_generator_tools",
          "tables/hardware_generator_tools.tex", "table",
          "curated list of hardware generators", "manual",
          notes="Content lives inline in main_thesis.tex; migrate to \\input{} to enable programmatic edits."),
    Entry("lake_interfaces", "tab:lake_interfaces",
          "tables/lake_interfaces.tex", "table",
          "Lake Component interface summary", "manual"),
    Entry("compiler_info", "tab:compiler_info",
          "tables/compiler_info.tex", "table",
          "compiler↔component metadata handshake", "manual"),
    Entry("PE_operations_support", "tab:PE_operations_support",
          "tables/PE_operations_support.tex", "table",
          "PE ALU supported operations", "manual"),
    Entry("memtile_model_coeff", "tab:memtile_model_coeff",
          "tables/memtile_model_coeff.tex", "table",
          "Memory Tile regression-model coefficients", "bogus", None,
          notes="TODO: fit a regression on the extracted characterization data + emit."),
    Entry("memtile_model_verif", "tab:memtile_model_verif",
          "tables/memtile_model_verif.tex", "table",
          "Memory Tile predicted vs actual area/power", "bogus", None,
          notes="TODO: after regression, hold out a set of configs and report residuals."),
    Entry("exploration_applications", "tab:exploration_applications",
          "tables/exploration_applications.tex", "table",
          "app suite × memory-usage summary", "manual"),
    Entry("ul_design_points", "tab:ul_design_points",
          "tables/ul_design_points.tex", "table",
          "single-level memory design points", "manual"),
    Entry("ul_perf", "tab:ul_perf",
          "tables/ul_perf.tex", "table",
          "per-app perf + memory usage on single-level designs", "bogus", None,
          notes="TODO: fill from the same app-mapping harness that feeds single_level_* figs."),
    Entry("ul_ppa_summary", "tab:ul_ppa_summary",
          "tables/ul_ppa_summary.tex", "table",
          "PPA summary for single-level design points", "bogus", None,
          notes="TODO: aggregate synth+PnR PPA per design point from THESIS_BUILDS."),
]


REGISTRY: list[Entry] = (
    BACKGROUND_MANUAL
    + COMPONENT_CHARACTERIZATION
    + EXPLORATION_APPS
    + LI_CHARACTERIZATION
    + TABLES
)


def by_id(id_: str) -> Entry:
    for e in REGISTRY:
        if e.id == id_:
            return e
    raise KeyError(id_)
