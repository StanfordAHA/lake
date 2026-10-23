"""build_spec / build_spec_rv check their (remote) storage against the GF SRAM library the
way the tile's CoreCombiner picks the macro: one full-width column if the library has it,
else the word split over several columns. Before, only the full width was tried, so RV
fw2 dw64 dual-port (a 128-bit SDPB word: no such macro, but 2 x 64-bit) asserted
"No valid macros" although the tile builds."""
import pytest

from lake.spec.spec_memory_controller import build_spec_rv
from lake.top.tech_maps import GF_Tech_Map, GF_Tech_Map_any_columns


@pytest.mark.parametrize("depth,width,dual_port", [(512, 64, False), (512, 64, True), (1024, 32, False),
                                                   (2048, 16, True)])
def test_full_width_macro_unchanged(depth, width, dual_port):
    assert GF_Tech_Map_any_columns(depth, width, dual_port) == GF_Tech_Map(depth, width, dual_port)


def test_dual_port_128_bit_word_splits_into_64_bit_columns():
    with pytest.raises(AssertionError):
        GF_Tech_Map(depth=512, width=128, dual_port=True)
    m = GF_Tech_Map_any_columns(depth=512, width=128, dual_port=True)
    assert m["width"] == 64 and m["depth"] == 512 and "SDPB" in m["name"]


def test_no_macro_at_any_column_count_still_raises():
    with pytest.raises(AssertionError):
        GF_Tech_Map_any_columns(depth=3, width=7, dual_port=False)


def test_build_spec_rv_dw64_fw2_dual_port():
    spec = build_spec_rv(storage_capacity=8192, data_width=64, vec_width=2, in_ports=2, out_ports=2,
                         dual_port=True)
    assert spec is not None
