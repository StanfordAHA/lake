"""Hand-written RV MEM/pond configs (spec/hack_rv_mem_pond_bitstream.py) are picked
per tile by its instance name (node_name), which garnet passes down
MemoryTileBuilder.get_bitstream -> SpecMemoryController.get_bitstream ->
Spec.gen_bitstream -> hack_rv_config.

Covers: per-tile dispatch for a multi-tile app (vector_reduction_fp: a filter MEM
feeding two accumulation ponds), that the name reaches the spec controller and only
the spec controller, and that each tile's config encodes on garnet's RV MEM / pond.
"""
import types

import pytest

from lake.spec.hack_rv_mem_pond_bitstream import (hack_rv_config, get_filter_mem_two_streams,
                                                  get_vec_accum_pond)
from lake.spec.spec_memory_controller import (SpecMemoryController, build_cgra_pond,
                                              build_four_port_wide_fetch_rv)
from lake.top.memtile_builder import MemoryTileBuilder

VEC_RED_ARGS = "vec_width=512 vec_height=4 glb_i=8 glb_o=1 tree_stages=3"


@pytest.fixture
def vector_reduction_env(monkeypatch):
    monkeypatch.setenv("TEST_NAME_FOR_HACKING_CHECK", "vector_reduction_fp")
    monkeypatch.setenv("HALIDE_GEN_ARGS", VEC_RED_ARGS)


def test_hack_rv_config_dispatches_on_node_name(vector_reduction_env):
    # 512 / 8 = 64 partial sums per vector, 4 vectors; each pond takes half
    assert hack_rv_config("vector_reduction_fp", node_name="vector_reduction_fp_filter_mem") == \
        get_filter_mem_two_streams(input_stream_size=64 * 4)
    for i in range(2):
        assert hack_rv_config("vector_reduction_fp", node_name=f"vector_reduction_fp_accum_pond_{i}") == \
            get_vec_accum_pond(num_partial_reduction=32, num_output_pixels=4)
    with pytest.raises(ValueError):
        hack_rv_config("vector_reduction_fp", node_name="some_other_tile")
    with pytest.raises(ValueError):
        hack_rv_config("vector_reduction_fp")


class _Ctrl:
    def __init__(self, mode, takes_node_name):
        self.mode, self.calls = mode, []
        if takes_node_name:
            self.get_bitstream = lambda cfg, node_name=None: self.calls.append(node_name) or []
        else:
            self.get_bitstream = lambda cfg: self.calls.append("no-kwarg") or []

    def get_config_mode_str(self):
        return self.mode

    def __str__(self):
        return self.mode


def _fake_tile(controllers):
    tile = types.SimpleNamespace(controllers=controllers, num_modes=len(controllers),
                                 num_chopped_cfg=0, config_mapping={}, allowed_reg_size=32)
    tile.get_mode_map = lambda: MemoryTileBuilder.get_mode_map(tile)
    return tile


def test_memtile_builder_forwards_node_name_to_spec_controller_only():
    spec_ctrl, rom_ctrl = _Ctrl("lakespec", True), _Ctrl("ROM", False)
    tile = _fake_tile([spec_ctrl, rom_ctrl])
    MemoryTileBuilder.get_bitstream(tile, {"mode": "UB", "config": {}}, node_name="m_filter_mem")
    MemoryTileBuilder.get_bitstream(tile, {"mode": "lakespec"}, node_name="m_accum_pond_0")
    MemoryTileBuilder.get_bitstream(tile, {"mode": "lakespec"})
    MemoryTileBuilder.get_bitstream(tile, {"mode": "ROM"}, node_name="m_rom")
    assert spec_ctrl.calls == ["m_filter_mem", "m_accum_pond_0", None]
    assert rom_ctrl.calls == ["no-kwarg"]


def test_spec_memory_controller_forwards_node_name():
    seen = []
    fake = types.SimpleNamespace(spec=types.SimpleNamespace(
        gen_bitstream=lambda cfg, node_name=None: seen.append(node_name) or 0))
    assert SpecMemoryController.get_bitstream(fake, {}, node_name="p3_path_balance_pond") == \
        [('config_memory', 0)]
    SpecMemoryController.get_bitstream(fake, {})
    assert seen == ["p3_path_balance_pond", None]


def test_vector_reduction_tiles_encode(vector_reduction_env):
    """Each tile gets its own program and it encodes on the spec garnet builds for it
    (filter MEM: Zircon build_four_port_wide_fetch_rv(4096, 16, 4); ponds: default RV pond)."""
    mem = build_four_port_wide_fetch_rv(storage_capacity=4096, data_width=16, vec_width=4)
    mem.generate_hardware()
    pond = build_cgra_pond(None, rv=True, comply_17=True)
    pond.generate_hardware()
    mem.gen_bitstream({}, node_name="vector_reduction_fp_filter_mem")
    pond_bs = [pond.gen_bitstream({}, node_name=f"vector_reduction_fp_accum_pond_{i}") for i in range(2)]
    assert pond_bs[0] == pond_bs[1]
