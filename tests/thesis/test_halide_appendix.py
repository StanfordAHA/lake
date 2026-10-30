"""THESIS/pipeline/halide_appendix.py: each listing's generator args are the
ones its app bundles record (THESIS/apps/registry.py)."""

from __future__ import annotations

from THESIS.apps.registry import by_id
from THESIS.pipeline import halide_appendix as ha


def test_apps_are_registry_entries():
    for app_id, _, _, _ in ha.APPS:
        assert not by_id(app_id).bundle_blocked


def test_bundle_env_args_win():
    params = {"tests/matmul_rvtry3": {"default": {"HALIDE_GEN_ARGS": "n=64"}}}
    assert ha.gen_args("matmul_pond_agg", params) == ("n=16", "host-x86-64-enable_ponds")


def test_application_parameters_otherwise():
    params = {"apps/gaussian": {"default": {"HALIDE_GEN_ARGS": "mywidth=62 schedule=3"}}}
    assert ha.gen_args("gaussian_agg", params) == ("mywidth=62 schedule=3", "")
    assert ha.gen_args("gaussian_agg", {}) == ("", "")


_GEN = """#include "Halide.h"

namespace {
class Mm : public Halide::Generator<Mm> {
public:
    GeneratorParam<int> sched{"sched", 0};
    void generate() {
        out(x) = in(x);

        /* THE SCHEDULE */
        if (get_target().has_feature(Target::Clockwork)) {
            if (sched == 0) {
                out.compute_root();
            } else if (sched == 1) {
                out.unroll(x);
            }
        } else {
            out.parallel(x);
        }
    }
};
}
"""


def test_sched_param_picks_its_branch():
    default = "\n".join(ha.listing(_GEN, "n=16"))
    assert "compute_root" in default and "unroll" not in default and "parallel" not in default
    one = "\n".join(ha.listing(_GEN, "sched=1 n=16"))
    assert "unroll" in one and "compute_root" not in one


_CHAIN = """#include "Halide.h"

namespace {
class Mm : public Halide::Generator<Mm> {
public:
    GeneratorParam<int> sched{"sched", 0};
    void generate() {
        out(x) = in(x);

        /* THE SCHEDULE */
        if (get_target().has_feature(Target::Clockwork)) {
          out.compute_root();
          if ((int)sched == 1) {
            out.unroll(x);
          } else if ((int)sched == 2) {
            out.vectorize(x);
          }
          in.stream_to_accelerator();
        } else {
          out.parallel(x);
        }
    }
};
}
"""


def test_sched_chain_inside_the_shared_schedule():
    default = ha.listing(_CHAIN, "n=16")
    text = "\n".join(default)
    assert "compute_root" in text and "stream_to_accelerator" in text
    assert "unroll" not in text and "vectorize" not in text
    assert "          // the sched == 1, 2 variants are omitted (sched = 0)" in default
    two = "\n".join(ha.listing(_CHAIN, "sched=2"))
    assert "vectorize" in two and "unroll" not in two and "stream_to_accelerator" in two


def test_prose_with_a_semicolon_is_kept():
    assert not ha._code_comment("// ROM tiles; the lake spec MemCore cannot")
    assert not ha._code_comment("// scratch: output tile per accelerator invocation (28 = whole image);")
    assert ha._code_comment("//int k_z = 8;")
    assert ha._code_comment("// out.unroll(x);")
    assert ha._code_comment("//hw_input.unroll(x, 3);   // unroll x")
