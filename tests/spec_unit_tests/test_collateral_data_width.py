"""The compiler collateral carries the spec's data_width at every level.

garnet builds the CGRA at the lake spec's data_width (tracks of data_width + 1 bits),
and Halide, clockwork and MetaMapper take the word width from this collateral's
`data_width` (LAKE_COLLATERAL_JSON_MEM / _REGFILE), so the two always match.
"""
import json

import pytest

from lake.spec.spec_memory_controller import build_pond, build_pond_rv, build_spec, build_spec_rv
from lake.utils import pond_collateral, spec_config_to_collateral

WIDTHS = (8, 16, 32, 64)


@pytest.mark.parametrize("width", WIDTHS)
@pytest.mark.parametrize("builder,kwargs", [
    (build_spec, dict(storage_capacity=8192, vec_width=4, in_ports=2, out_ports=2)),
    (build_spec, dict(storage_capacity=8192, vec_width=2, in_ports=2, out_ports=2, dual_port=True)),
    (build_spec_rv, dict(storage_capacity=8192, vec_width=4, in_ports=2, out_ports=2)),
], ids=("static", "static_dp", "rv"))
def test_mem_level(builder, kwargs, width):
    if width == 64:
        kwargs = dict(kwargs, vec_width=2)  # dw64 x fw4 has no SRAM macro; the sweep uses fw2
    info = builder(data_width=width, **kwargs).extract_compiler_information(level="mem")
    assert info["data_width"] == width


@pytest.mark.parametrize("width", WIDTHS)
@pytest.mark.parametrize("builder", (build_pond, build_pond_rv), ids=("static", "rv"))
def test_regfile_level(builder, width):
    info = builder(data_width=width).extract_compiler_information(level="regfile")
    assert info["data_width"] == width


@pytest.mark.parametrize("width", WIDTHS)
def test_spec_config_to_collateral(tmp_path, width):
    # what `aha map` runs when LAKE_SPEC_CONFIG is set and no --collateral is given
    spec = tmp_path / "spec_config.json"
    spec.write_text(json.dumps(dict(data_width=width, storage_capacity=8192, vec_width=4,
                                    in_ports=2, out_ports=2)))
    out = tmp_path / "lake_collateral_mem.json"
    spec_config_to_collateral.generate(str(spec), str(out))
    assert json.loads(out.read_text())["data_width"] == width


@pytest.mark.parametrize("width", (16, 32))
@pytest.mark.parametrize("rv", (False, True), ids=("static", "rv"))
def test_pond_collateral(tmp_path, width, rv):
    spec = tmp_path / "pond_spec.json"
    spec.write_text(json.dumps(dict(data_width=width)))
    out = tmp_path / "lake_collateral_regfile.json"
    pond_collateral.generate(str(out), str(spec), rv=rv)
    assert json.loads(out.read_text())["data_width"] == width


def test_default_collateral_is_16(tmp_path):
    out = tmp_path / "lake_collateral_regfile.json"
    pond_collateral.generate(str(out))
    assert json.loads(out.read_text())["data_width"] == 16
