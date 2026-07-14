"""Application registry for Ch. 5 exploration.

Each ``AppSpec`` names one app × schedule variant, mapping to a Halide
app dir + testname that the round-trip flow (`ASPLOS_EXP/run_roundtrip_sweep.sh`)
can consume.

**Skeleton with TODOs.** The 6 apps come from ``tab:exploration_applications``
in main_thesis.tex (lines ~1885-1898). The exact
``/aha/Halide-to-Hardware/apps/hardware_benchmarks/...`` path per app
needs verification against the local Halide-to-Hardware checkout — I
haven't confirmed which apps have "aggressive" vs "conservative"
schedule variants and which need an im2col preprocessing pass.
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
    """

    id: str
    display: str
    halide_app_dir: str
    testname: str
    schedule_variant: str
    memory_notes: str


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
    AppSpec("camera_pipeline_agg", "Camera Pipeline 2x2",
            "apps/camera_pipeline_2x2", "camera_pipeline_2x2_hw",
            "aggressive",
            "Bayer→RGB pipeline w/ 2x2 mosaic access; strided reads on short history"),
    AppSpec("matmul_agg", "Matrix Multiply",
            "apps/matmul", "matmul_hw",
            "aggressive",
            "GEMM output-stationary; partial-sum RF + weight buffer"),
    AppSpec("resnet_agg", "ResNet block",
            "apps/resnet_stage", "resnet_stage_hw",  # TODO: verify testname
            "aggressive",
            "3×3 conv block w/ weight reuse; ping-pong tile buffer for activations"),

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
