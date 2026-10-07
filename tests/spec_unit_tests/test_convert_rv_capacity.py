"""Spec.convert_app_json_to_config (clockwork RV program -> lake config) must
respect the memory's capacity:

  * a line buffer's WAR placeholder (clockwork emits null) lets the writer run
    at most min(8, R - D - c) rows ahead of the reader, R = capacity // row
    pitch rows held (addresses wrap), D the reader's precursor row lag, c = 1
    if it also lags by columns; with no row of slack left the reader's RAW
    moves to the row level, and it raises only if R - c < 3;
  * a reuse buffer with explicit [level, level, scalar] constraints (barrier /
    sweep) must be resident: addresses past the memory raise.
"""
import copy

import pytest

from lake.spec.spec_memory_controller import build_spec_rv
from lake.utils.spec_enum import LFComparisonOperator

GT = LFComparisonOperator.GT.value
LT = LFComparisonOperator.LT.value

_SPECS = {}


def spec(capacity_bytes):
    if capacity_bytes not in _SPECS:
        s = build_spec_rv(storage_capacity=capacity_bytes, data_width=16, vec_width=1, dual_port=True,
                          in_ports=1, out_ports=1, physical=False)
        s.generate_hardware()
        _SPECS[capacity_bytes] = s
    return _SPECS[capacity_bytes]


def line_buffer(W, H, pitch, lag_rows, lag_cols=0):
    return {
        "port_mappings": {"wr": "m.data_in_0", "rd": "m.data_out_0"},
        "domain": {p: {"dimensionality": [2], "extents": [W, H]} for p in ("wr", "rd")},
        "access_map": {p: {"dimensionality": [2], "address_offset": [0], "address_stride": [1, pitch]}
                       for p in ("wr", "rd")},
        "precursor_deltas": {"rd": [[0, lag_cols], [1, lag_rows]]},
        "dep_values": {"rd___DEPTO___wr": [0, 0], "wr___DEPTO___rd": None},
    }


def war_scalar(sp, app):
    conf = sp.convert_app_json_to_config(sp.rewrite_app_json(copy.deepcopy(app)))
    wars = [c for c in conf["constraints"] if c[4] == GT]
    assert len(wars) == 1
    return wars[0][5]


@pytest.mark.parametrize("cap,W,pitch,lag,cols,expected", [
    (4096, 64, 64, 2, 0, 8),      # 2048 words = 32 rows: default 8 rows
    (1024, 64, 64, 2, 0, 6),      # 512 words = 8 rows: 8 - 2
    (1024, 64, 64, 2, 1, 5),      # column lag costs a row
    (1024, 100, 128, 1, 1, 2),    # 4 rows of pitch 128
])
def test_line_buffer_war_bounded_by_capacity(cap, W, pitch, lag, cols, expected):
    assert war_scalar(spec(cap), line_buffer(W, 20, pitch, lag, cols)) == expected


def constraints(sp, app):
    return sp.convert_app_json_to_config(sp.rewrite_app_json(copy.deepcopy(app)))["constraints"]


@pytest.mark.parametrize("W,pitch,lag", [
    (64, 64, 7),     # 8 rows held, lag 7 rows + cols
    (68, 68, 6),     # unsharp gray tap on a 512-word memory: 7 rows held, lag 6 rows + cols
])
def test_line_buffer_without_spare_row_uses_row_level_raw(W, pitch, lag):
    """No spare row (R - D - c = 0): the default column-level RAW would
    deadlock (the writer may not enter the reader's row), so the reader's RAW
    moves to the row level (scalar -D + 1) and the writer trails the reader's
    row counter (WAR scalar 0)."""
    cons = constraints(spec(1024), line_buffer(W, 20, pitch, lag, 1))
    (war,) = [c for c in cons if c[4] == GT]
    (raw,) = [c for c in cons if c[4] == LT]
    assert war[1] == war[3] == 1 and war[5] == 0
    assert raw[1] == raw[3] == 1 and raw[5] == -lag + 1


def test_line_buffer_too_small_rejected():
    # 2 rows of pitch 256 held, lag 1 row + cols: even row-level constraints
    # leave the writer no row to be in
    with pytest.raises(ValueError, match="does not fit"):
        war_scalar(spec(1024), line_buffer(200, 20, 256, 1, 1))


def reuse_buffer(n):
    return {
        "port_mappings": {"wr": "m.data_in_0", "rd": "m.data_out_0"},
        "domain": {"wr": {"dimensionality": [1], "extents": [n]}, "rd": {"dimensionality": [2], "extents": [n, 3]}},
        "access_map": {"wr": {"dimensionality": [1], "address_offset": [0], "address_stride": [1]},
                       "rd": {"dimensionality": [2], "address_offset": [0], "address_stride": [1, 0]}},
        "dep_values": {"rd___DEPTO___wr": [1, 0, 16383]},
    }


def test_resident_reuse_buffer_fits():
    sp = spec(1024)
    sp.convert_app_json_to_config(sp.rewrite_app_json(reuse_buffer(512)))


def test_resident_reuse_buffer_too_big_rejected():
    sp = spec(1024)
    with pytest.raises(ValueError, match="must be resident"):
        sp.convert_app_json_to_config(sp.rewrite_app_json(reuse_buffer(513)))


def test_dependence_across_storages_rejected():
    """A 1-input spec's second input is the filter path, with its own storage:
    mapping an accumulator's second writer there (resnet conv banks on
    in1/out1 specs) must be refused, not silently dropped."""
    sp = spec(1024)
    app = reuse_buffer(64)
    app["port_mappings"] = {"wr": "m.data_in_1", "rd": "m.data_out_0"}   # data_in_1 = filter path
    with pytest.raises(ValueError, match="different storages"):
        sp.convert_app_json_to_config(sp.rewrite_app_json(app))
