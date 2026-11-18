"""lake/utils/power_test_programs: the opt-in full_rate active programs (every
measured port fires on every cycle of the window) and the default program left
as it was. Pure Python (program shape and windows); the RTL behind the window
lengths was checked with lake SpecSim (garnet mflowgen/CLAUDE.md, 2026-11-17)."""
import contextlib
import io

import pytest

from lake.spec.spec_memory_controller import build_spec, build_spec_rv
from lake.utils import power_test_programs as ptp

RV = dict(storage_capacity=8192, data_width=16, vec_width=4, in_ports=2, out_ports=2,
          vec_capacity=2, add_filter_path=False)
STATIC_FW1 = dict(storage_capacity=8192, data_width=16, vec_width=1, in_ports=2, out_ports=2)
MARGIN = 2 * 4 * 2 + 4      # the readers' zero-lag margin at fw 4


def _spec(builder, **kw):
    spec = builder(physical=False, **kw)
    with contextlib.redirect_stdout(io.StringIO()):
        spec.generate_hardware()
    return spec


@pytest.mark.parametrize("dims,max_extent,window", [(1, 64, 32), (2, 64, 32), (2, 256, 224),
                                                    (1, 1024, 992), (6, 4096, 1000)])
def test_rv_full_rate(dims, max_extent, window):
    spec = _spec(build_spec_rv, dims=dims, max_extent=max_extent, **RV)
    act = ptp.active_program(spec, 2, 2, 4, 16, 8192, False, 1000, program="full_rate")
    # one row of the writers' domain minus the readers' lag, in whole 4-cycle rounds
    assert act["window"] == window
    assert act["window_start"] == "outputs"
    assert act["min_output_changes"] == window * 9 // 10
    # every reader streams behind its writer at level 0, 1-D included (no barrier)
    cons = act["app"]["constraints"]
    assert len(cons) == 2 and all(c[1] == 0 and c[3] == 0 and c[5] == MARGIN for c in cons)


def test_rv_default_unchanged():
    spec = _spec(build_spec_rv, dims=1, max_extent=1024, **RV)
    act = ptp.active_program(spec, 2, 2, 4, 16, 8192, False, 1000)
    assert "window_start" not in act
    # the default still runs 1-D domains phased (barrier dependence)
    assert all(c[5] == 16383 for c in act["app"]["constraints"])


@pytest.mark.parametrize("dims,max_extent,window,extents", [(2, 64, 1000, [64, 16]), (1, 64, 56, [64]),
                                                            (6, 4096, 1000, [1012])])
def test_static_full_rate_single_port(dims, max_extent, window, extents):
    spec = _spec(build_spec, dims=dims, max_extent=max_extent, **STATIC_FW1)
    act = ptp.active_program(spec, 2, 2, 1, 16, 8192, False, 1000, program="full_rate")
    app = act["app"]
    w0 = spec.port_name_to_int("port_w0")
    assert [k for k in app if k != "constraints"] == [w0]
    cfg = app[w0]["config"]
    assert cfg["extents"] == extents
    assert cfg["schedule"]["strides"][0] == 1          # fires every cycle
    assert cfg["schedule"]["offset"] == ptp.STATIC_FULL_RATE_T0
    assert act["window"] == window and act["window_start"] == ptp.STATIC_FULL_RATE_T0
    assert act["min_output_changes"] == 0


def test_static_full_rate_needs_fw1():
    spec = _spec(build_spec, storage_capacity=8192, data_width=16, vec_width=4, in_ports=2, out_ports=2,
                 vec_capacity=2)
    with pytest.raises(ValueError, match="fw=4"):
        ptp.active_program(spec, 2, 2, 4, 16, 8192, False, 1000, program="full_rate")


def test_unknown_program():
    spec = _spec(build_spec, **STATIC_FW1)
    with pytest.raises(ValueError, match="unknown program"):
        ptp.active_program(spec, 2, 2, 1, 16, 8192, False, 1000, program="fastest")
