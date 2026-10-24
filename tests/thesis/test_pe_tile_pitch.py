"""fig:pe_tile_height: the PE tile as the unit tile and each Memory Tile's height
(THESIS/pipeline/tile_figures.py pe_tile_pitch)."""

from __future__ import annotations

import pytest

from THESIS.pipeline import tile_figures as tf
from THESIS.pipeline.errors import MissingDataError


@pytest.mark.parametrize("rel,k", [(0.8, 1), (1.5, 1), (1.51, 2), (2.1, 2), (3.0, 2), (3.2, 3), (6.0, 4)])
def test_mem_tile_height(rel, k):
    assert tf.mem_tile_height(rel) == k


def test_figure_from_csv(tmp_path, monkeypatch):
    csv = tmp_path / "tiles.csv"
    csv.write_text("fabric,tile,area_um2\nstatic,pe,100\nstatic,mem,210\nrv,pe,130\nrv,mem,225\n")
    monkeypatch.setattr(tf, "PE_TILE_CSV", csv)
    out = tmp_path / "pe_tile_height.pdf"
    tf.pe_tile_pitch(None, out)
    assert out.stat().st_size > 1000


def test_missing_data(tmp_path, monkeypatch):
    monkeypatch.setattr(tf, "PE_TILE_CSV", tmp_path / "none.csv")
    with pytest.raises(MissingDataError):
        tf.pe_tile_pitch(None, tmp_path / "x.pdf")
    csv = tmp_path / "tiles.csv"
    csv.write_text("fabric,tile,area_um2\nstatic,pe,100\nstatic,mem,1000\n")   # 10 PE tiles: > 4 high
    monkeypatch.setattr(tf, "PE_TILE_CSV", csv)
    with pytest.raises(MissingDataError, match="unit heights"):
        tf.pe_tile_pitch(None, tmp_path / "x.pdf")
