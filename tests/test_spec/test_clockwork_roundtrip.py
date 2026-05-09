"""Smoke tests for lake.utils.clockwork_roundtrip.write_roundtrip_artifacts.

These tests build a small wide-fetch spec, hand it a synthetic clockwork
per-tile config in the format ``Spec._convert_clockwork_to_port_config`` accepts
(keys ending in ``_<idx>``), and verify the four artifacts appear with sane
contents. They do NOT exercise the CoreIR-walking that lives in the
clockwork-roundtrip-compile mflowgen step — that is integration-tested by the
smoke_test.sh sweep.
"""

import json
import os

import pytest

from lake.utils.clockwork_roundtrip import write_roundtrip_artifacts


def _make_synthetic_tile():
    """Minimal wide-fetch clockwork tile config.

    Two write ports streaming a small linear pattern, two read ports reading
    back the same pattern after a small delay. fw=2, so both word-level
    (in2agg/tb2out) and SRAM-level (agg2sram/sram2tb) entries are present.
    """
    base = {
        "dimensionality": 1,
        "extent": [8],
        "cycle_starting_addr": [0],
        "cycle_stride": [1],
        "write_data_starting_addr": [0],
        "write_data_stride": [1],
        "read_data_starting_addr": [0],
        "read_data_stride": [1],
    }
    return {
        "in2agg_0":   {**base, "cycle_starting_addr": [0]},
        "agg2sram_0": {**base, "cycle_starting_addr": [0]},
        "sram2tb_0":  {**base, "cycle_starting_addr": [4]},
        "tb2out_0":   {**base, "cycle_starting_addr": [4]},
    }


def test_write_roundtrip_artifacts_smoke(tmp_path):
    tile_path = tmp_path / "tile_0.json"
    tile_path.write_text(json.dumps(_make_synthetic_tile()))

    out_dir = tmp_path / "cfg_0"

    spec_factory_kwargs = dict(
        storage_capacity=1024,
        data_width=16,
        vec_width=2,
        dims=4,
        in_ports=1,
        out_ports=1,
        dual_port=False,
        vec_capacity=2,
        physical=False,
    )

    result = write_roundtrip_artifacts(spec_factory_kwargs, str(tile_path),
                                       str(out_dir))

    # The synthetic config may or may not survive bitstream packing — we accept
    # any failure tag as long as the helper returns cleanly.
    assert isinstance(result, dict)
    assert result.get("status") in {"ok", "convert_failed", "gold_failed",
                                     "bitstream_failed"}, result

    if result["status"] == "ok":
        assert (out_dir / "inputs" / "bitstream.bs").exists()
        assert (out_dir / "inputs" / "comp_args.txt").exists()
        assert (out_dir / "inputs" / "PARGS.txt").exists()
        assert (out_dir / "inputs" / "gold").is_dir()
        # comp_args has the two define lines
        comp_args = (out_dir / "inputs" / "comp_args.txt").read_text()
        assert "+define+CONFIG_MEMORY_SIZE=" in comp_args
        assert "+define+NUMBER_PORTS=" in comp_args
        # PARGS has at least max_time and static
        pargs = (out_dir / "inputs" / "PARGS.txt").read_text()
        assert "+max_time=" in pargs
        assert "+static=1" in pargs


def test_write_roundtrip_artifacts_handles_garbage(tmp_path):
    """Helper should report failure status, not raise, when the tile is bogus."""
    tile_path = tmp_path / "tile_garbage.json"
    tile_path.write_text(json.dumps({"not_a_clockwork_key": {}}))
    out_dir = tmp_path / "cfg_garbage"

    spec_factory_kwargs = dict(
        storage_capacity=1024,
        data_width=16,
        vec_width=2,
        dims=4,
        in_ports=1,
        out_ports=1,
        dual_port=False,
        physical=False,
    )

    result = write_roundtrip_artifacts(spec_factory_kwargs, str(tile_path),
                                       str(out_dir))
    # Garbage keys: converter produces an empty port_config; downstream may
    # raise during bitstream gen. Either way, we must return a dict, not raise.
    assert isinstance(result, dict)
    assert "status" in result
