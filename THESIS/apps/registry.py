"""Application registry for Ch. 5 exploration.

Each ``AppSpec`` names one app × schedule variant, mapping to a Halide
app dir + testname that the round-trip flow (`ASPLOS_EXP/run_roundtrip_sweep.sh`)
can consume.

The 6 apps come from ``tab:exploration_applications`` in main_thesis.tex
(lines ~1885-1898); matrix multiply has a single- and a two-level entry
(2026-10-25). ``halide_app_dir`` is the ``--app`` path garnet's
``gen_app_bundle.py`` records (``THESIS/apps/CGRA_APP_POWER.md``); the
matmul and resnet dirs are the ones the garnet app bundles and thesis-fabric
runs used. Which apps have "aggressive" vs "conservative" schedule variants
is still open.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class AppSpec:
    """One benchmark configuration.

    Attributes:
        id: stable slug used as a directory / result-key name.
        display: human-readable name for figure legends / tables.
        halide_app_dir: path relative to the Halide-to-Hardware apps root
            (verify against your checkout — the harness driver joins it
            with ``$AHA_ROOT/apps/hardware_benchmarks/``).
        testname: Halide "testname" the round-trip flow passes to
            ``run_clockwork.py`` via ``--testname``.
        schedule_variant: "aggressive" | "conservative" (matches the
            column split in ``tab:ul_perf``).
        memory_notes: one-line access-pattern description for the thesis
            narrative — kept in-code so it's easy to keep the tex table
            in sync with the harness.
        level: "single" (MEM tiles only) | "two" (also maps onto the PE-tile
            pond); the single_level_* figures plot only "single" apps.
        bundle_env: extra env for garnet gen_app_bundle.py (``--env``), as
            KEY=VAL shell words (quote a value with spaces), beyond aha's
            application_parameters.json.
        bundle_blocked: why the app cannot be recorded yet ("" = it can);
            ``garnet_configs --commands`` prints its lines commented out.
        bundle_retry_array: "WxH": record again on that array when PnR fails
            on the default 16x16 one ("" = never).
    """

    id: str
    display: str
    halide_app_dir: str
    testname: str
    schedule_variant: str
    memory_notes: str
    level: str = "single"
    bundle_env: str = ""
    bundle_blocked: str = ""
    bundle_retry_array: str = ""


# TODO: verify each halide_app_dir / testname against the local
# Halide-to-Hardware checkout. Marked "??" where the exact directory name
# isn't confirmed. The harness driver should refuse to run with any "??"
# fields.
APPS: list[AppSpec] = [
    AppSpec("gaussian_agg", "Gaussian",
            "apps/gaussian", "gaussian_hw",
            "aggressive",
            "2D separable stencil; line-buffer holds ~2 rows, 1-write/N-read reuse per pixel"),
    AppSpec("unsharp_agg", "Unsharp",
            "apps/unsharp", "unsharp_hw",
            "aggressive",
            "stencil + downstream accumulation stage; adds a small accum RF"),
    AppSpec("harris_agg", "Harris (color)",
            "apps/harris_color", "harris_color_hw",
            "aggressive",
            "multi-stage color→gradient→corner stencils; chained line buffers"),
    # Has passed only on a 28x16 array (16+ IO tiles); user 2026-10-26: record it on a
    # bigger array if 16x16 does not place (whole-CGRA power replays single tiles).
    AppSpec("camera_pipeline_agg", "Camera Pipeline 2x2",
            "apps/camera_pipeline_2x2", "camera_pipeline_2x2_hw",
            "aggressive",
            "Bayer→RGB pipeline w/ 2x2 mosaic access; strided reads on short history",
            bundle_retry_array="28x16"),
    # Matrix multiply in both hierarchies (user 2026-10-25): the n=16 matmul the
    # garnet app bundles run (tests/matmul_rvtry3), with the reduction unrolled
    # into a PE adder tree (single level) or accumulated in the PE-tile pond
    # (two level). aha's application_parameters.json has no entry for it, so the
    # size comes from the env: without HALIDE_GEN_ARGS n=16 the generator builds
    # n=64 while process (MATMUL_N, -DMATMUL_N in the app Makefile) runs 16x16, and
    # halide stops at "accessed at 63, which is beyond the max (15)" (/aha, 2026-10-26).
    AppSpec("matmul_agg", "Matrix Multiply",
            "tests/matmul_rvtry3", "matmul_rvtry3",
            "aggressive",
            "GEMM; reduction unrolled into a PE adder tree, MEM tiles buffer the operands",
            bundle_env="HL_TARGET=host-x86-64 'HALIDE_GEN_ARGS=sched=1 n=16' MATMUL_N=16",
            bundle_blocked="does not map on 16x16: static hits clockwork's "
                           "garnet_single_port_ram_schedule assert, RV (23 PEs) does not route "
                           "(garnet mflowgen/CLAUDE.md 'No-pond matmul')"),
    AppSpec("matmul_pond_agg", "Matrix Multiply (pond)",
            "tests/matmul_rvtry3", "matmul_rvtry3",
            "aggressive",
            "GEMM; partial sums accumulate in the PE-tile pond, MEM tiles buffer the operands",
            level="two",
            bundle_env="HL_TARGET=host-x86-64-enable_ponds HALIDE_GEN_ARGS=n=16 MATMUL_N=16"),
    # The resnet that ran end to end on both thesis fabrics (garnet mflowgen/CLAUDE.md
    # "Thesis fabrics"): a copy of apps/resnet with the output channel innermost; stock
    # apps/resnet stops in parse_design_meta. In H2H thesis since 163a591a.
    AppSpec("resnet_agg", "ResNet block",
            "pondspec_e2e/resnet", "resnet",
            "aggressive",
            "3×3 conv block w/ weight reuse; ping-pong tile buffer for activations",
            bundle_env="EXT=mat"),

    # TODO: conservative-schedule variants. tab:ul_perf splits every app
    # into aggressive/conservative columns. Confirm whether the schedule
    # variant is a separate Halide testname or a compile flag; add rows
    # once known.
]


def by_id(app_id: str) -> AppSpec:
    for a in APPS:
        if a.id == app_id:
            return a
    raise KeyError(app_id)


def unverified() -> list[AppSpec]:
    """Return apps that still have a ``??`` in any field — the harness
    should refuse to run these until they're pinned down."""
    return [a for a in APPS if "??" in " ".join(a.__dict__.values())]
