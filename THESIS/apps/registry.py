"""Application registry for Ch. 5 exploration.

Each ``AppSpec`` names one app × schedule variant, mapping to a Halide
app dir + testname that the round-trip flow (`ASPLOS_EXP/run_roundtrip_sweep.sh`)
can consume.

``tab:exploration_applications`` is rendered from this list
(``tables.emit_exploration_applications``): each app's ``computes`` and
``memory_classes``, and the Memory Tiles it places in the final run. Matrix
multiply has a single- and a two-level entry (2026-10-25), and resnet a
two-level one where the single-level one cannot map (2026-10-29). ``halide_app_dir`` is the ``--app`` path garnet's
``gen_app_bundle.py`` records (``THESIS/apps/CGRA_APP_POWER.md``); the
matmul and resnet dirs are the ones the garnet app bundles and thesis-fabric
runs used. Every app is the aggressive schedule: the final run has no
conservative ones (user 2026-10-29).
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
        schedule_variant: "aggressive" | "conservative" (every app is
            aggressive: tab:ul_perf has no conservative rows).
        computes: what the app computes, as it ran in the final run (the
            generator at ``halide_app_dir`` and its ``bundle_env`` args):
            LaTeX, written into tab:exploration_applications as is. The array
            it ran on, when not 16x16, is added from the app results.
        memory_classes: the memory classes it uses, from the three the thesis
            defines (MEMORY_CLASSES), comma-separated; " (pond)" after a class
            means the PE-tile pond holds it.
        level: "single" (MEM tiles only) | "two" (also maps onto the PE-tile
            pond); the single_level_* figures plot only "single" apps.
        bundle_env: extra env for garnet gen_app_bundle.py (``--env``), as
            KEY=VAL shell words (quote a value with spaces), beyond aha's
            application_parameters.json.
        bundle_blocked: why the app cannot be recorded yet ("" = it can);
            ``garnet_configs --commands`` prints its lines commented out.
        bundle_retry_array: "WxH": record again on that array when PnR fails
            on the default 16x16 one ("" = never).
        bundle_modes: the runtime modes it records in, comma-separated ("" =
            every mode).
        bundle_in_ports: record only on design points with this many input
            ports (0 = every point).
        single_level_app: a two-level app's single-level counterpart (the same
            computation without the pond), which the two_level_* figures pair it
            with on each design point ("" = none).
    """

    id: str
    display: str
    halide_app_dir: str
    testname: str
    schedule_variant: str
    computes: str
    memory_classes: str
    level: str = "single"
    bundle_env: str = ""
    bundle_blocked: str = ""
    bundle_retry_array: str = ""
    bundle_modes: str = ""
    bundle_in_ports: int = 0
    single_level_app: str = ""


# The memory classes of the thesis (sec. "Memory classes"); AppSpec.memory_classes
# names only these.
MEMORY_CLASSES = ("Line Buffer", "Double Buffer", "Accumulation")


# TODO: verify each halide_app_dir / testname against the local
# Halide-to-Hardware checkout. Marked "??" where the exact directory name
# isn't confirmed. The harness driver should refuse to run with any "??"
# fields.
APPS: list[AppSpec] = [
    AppSpec("gaussian_agg", "Gaussian",
            "apps/gaussian", "gaussian_hw",
            "aggressive",
            r"3$\times$3 Gaussian blur of a grayscale image, two output pixels per cycle",
            "Line Buffer"),
    AppSpec("unsharp_agg", "Unsharp",
            "apps/unsharp", "unsharp_hw",
            "aggressive",
            r"Unsharp mask of an RGB image: a grayscale copy and its 3$\times$3 blur give a "
            r"sharpening ratio that scales all three channels",
            "Line Buffer"),
    AppSpec("harris_agg", "Harris (color)",
            "apps/harris_color", "harris_color_hw",
            "aggressive",
            r"Harris corner detector on an RGB image: grayscale, 3$\times$3 Sobel gradients, "
            r"3$\times$3 sums of the gradient products, the corner response, and 3$\times$3 "
            r"non-maximum suppression, so its line buffers are chained",
            "Line Buffer"),
    # Has passed only on a 28x16 array (16+ IO tiles); user 2026-10-26: record it on a
    # bigger array if 16x16 does not place (whole-CGRA power replays single tiles).
    AppSpec("camera_pipeline_agg", "Camera Pipeline 2x2",
            "apps/camera_pipeline_2x2", "camera_pipeline_2x2_hw",
            "aggressive",
            r"Raw Bayer image to RGB: hot-pixel suppression, demosaicking, color correction, "
            r"and a gamma and contrast curve, a 2$\times$2 block of pixels per cycle",
            "Line Buffer",
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
            r"16$\times$16 product $AB$ of an 8-bit matrix $A$ with $B = A + 1$ (elementwise), "
            r"computed on the CGRA; each output's 16-term sum is a PE adder tree",
            "Double Buffer",
            bundle_env="HL_TARGET=host-x86-64 'HALIDE_GEN_ARGS=sched=1 n=16' MATMUL_N=16",
            bundle_blocked="does not map on 16x16: static hits clockwork's "
                           "garnet_single_port_ram_schedule assert, RV (23 PEs) does not route "
                           "(garnet mflowgen/CLAUDE.md 'No-pond matmul')"),
    AppSpec("matmul_pond_agg", "Matrix Multiply (pond)",
            "tests/matmul_rvtry3", "matmul_rvtry3",
            "aggressive",
            r"16$\times$16 product $AB$ of an 8-bit matrix $A$ with $B = A + 1$ (elementwise), "
            r"computed on the CGRA; each output's 16-term sum accumulates in a PE Tile pond",
            "Double Buffer, Accumulation (pond)",
            level="two",
            bundle_env="HL_TARGET=host-x86-64-enable_ponds HALIDE_GEN_ARGS=n=16 MATMUL_N=16",
            single_level_app="matmul_agg"),
    # The resnet that ran end to end on both thesis fabrics (garnet mflowgen/CLAUDE.md
    # "Thesis fabrics"): a copy of apps/resnet with the output channel innermost; stock
    # apps/resnet stops in parse_design_meta. In H2H thesis since 163a591a.
    AppSpec("resnet_agg", "ResNet block",
            "pondspec_e2e/resnet", "resnet",
            "aggressive",
            r"One 3$\times$3 convolution layer with 4 input channels, 3 output channels, and a "
            r"28$\times$28 output; the 9 kernel positions are the outermost loop, so the partial "
            r"sums stay in Memory Tiles",
            "Double Buffer, Accumulation",
            bundle_env="EXT=mat"),
    # Resnet where resnet_agg cannot map (two MEM writers, one input port): the same
    # app with the reduction innermost, accumulated in the PE-tile pond, the output
    # channels a loop and HL_TARGET -enable_ponds (its Makefile), at the default
    # tile=28. addon2 recorded it RV on the two 1-input points (garnet mflowgen/CLAUDE.md
    # "Add-ons"); static outputs all zeros there, so those two cells are gaps (user
    # 2026-10-29). RV on the five 2-input points too, so every point where resnet_agg
    # maps has both levels for the two_level_* figures (user 2026-10-30); static there
    # is untried; RV fw4_dw32 sp fails in clockwork and is a gap (addon4, 2026-10-31).
    # Recorded from an identical copy (manifest app probe_1fa8/resnet_pond);
    # in H2H thesis since ff1435830. No app env: its bundles record app_env {} and ran
    # with hardware_targets.mk's default EXT=png, bit-accurate (unlike resnet_agg's
    # EXT=mat; /aha manifests, aha-08 2026-10-29).
    AppSpec("resnet_pond_agg", "ResNet block (pond)",
            "pondspec_e2e/resnet_pond", "resnet",
            "aggressive",
            r"The same layer with the reduction innermost, so each output pixel accumulates in a "
            r"PE Tile pond",
            "Double Buffer, Accumulation (pond)",
            level="two",
            bundle_modes="rv",
            single_level_app="resnet_agg"),

]


def by_id(app_id: str) -> AppSpec:
    for a in APPS:
        if a.id == app_id:
            return a
    raise KeyError(app_id)


def unverified() -> list[AppSpec]:
    """Return apps that still have a ``??`` in any field — the harness
    should refuse to run these until they're pinned down."""
    return [a for a in APPS if "??" in " ".join(map(str, a.__dict__.values()))]
