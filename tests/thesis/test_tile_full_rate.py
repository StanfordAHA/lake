"""Full-rate sweep (garnet --recipe thesis_id_fullrate): tile_sweep routes it to
full_rate.csv, not latest.csv, and the figures that need full-rate power read it."""

from __future__ import annotations

import pandas as pd
import pytest

from THESIS.pipeline import tile_figures as tf
from THESIS.pipeline import tile_sweep


def test_figure_source_default_sweep_goes_to_latest():
    assert tile_sweep.figure_source(pd.DataFrame({"config": ["a"]})) == tile_sweep.LATEST
    df = pd.DataFrame({"config": ["a", "b"], "power_program": ["default", None]})
    assert tile_sweep.figure_source(df) == tile_sweep.LATEST


def test_figure_source_full_rate_sweep_goes_to_full_rate():
    df = pd.DataFrame({"config": ["a", "b"], "power_program": ["full_rate"] * 2})
    assert tile_sweep.figure_source(df) == tile_sweep.FULL_RATE


def test_figure_source_refuses_a_mixed_sweep():
    df = pd.DataFrame({"config": ["a", "b"], "power_program": ["full_rate", "default"]})
    with pytest.raises(ValueError, match="1 of 2"):
        tile_sweep.figure_source(df)


def _rows(mode, vec_width, power, experiments):
    rows = []
    for dims in (1, 2):
        for me in (64, 256):
            rows.append({
                "config": f"fw{vec_width}_d{dims}_me{me}_{mode}", "mode": mode, "status": "PASS",
                "syn_cell": 1.0, "data_width": 16, "vec_width": vec_width, "in_ports": 2,
                "out_ports": 2, "dual_port": False, "dims": dims, "max_extent": me,
                "experiments": experiments,
                **{f"blk_{b}": 10.0 * dims for b in ("c_sg0", "c_id0", "c_rvnet", "c_sg")},
                **{f"pw_synth_active_{b}": power * dims for b in ("c_sg0", "c_id0", "c_rvnet", "c_sg")},
                **{f"pw_synth_idle_{b}": 1e-8 for b in ("c_sg0", "c_id0", "c_rvnet", "c_sg")},
            })
    return rows


@pytest.fixture
def sweeps(tmp_path, monkeypatch):
    """latest.csv: the ID experiment at the default program's rate (fw1, 1e-6 W);
    full_rate.csv: static fw1 points and RV fw4 points (no experiment), 3e-6 W."""
    latest, full = tmp_path / "latest.csv", tmp_path / "full_rate.csv"
    pd.DataFrame(_rows("static", 1, 1e-6, "ITERATION_DOMAIN_EXP") +
                 _rows("rv", 1, 1e-6, "ITERATION_DOMAIN_EXP")).to_csv(latest, index=False)
    pd.DataFrame(_rows("static", 1, 3e-6, "ITERATION_DOMAIN_EXP") +
                 _rows("rv", 4, 3e-6, "")).to_csv(full, index=False)
    monkeypatch.setattr(tile_sweep, "LATEST", latest)
    monkeypatch.setattr(tile_sweep, "FULL_RATE", full)
    return latest, full


def test_load_full_rate_source(sweeps):
    df = tf._load("rv", source="full_rate")
    assert set(df["vec_width"]) == {4}
    assert set(tf._load("rv")["vec_width"]) == {1}


@pytest.mark.parametrize("fig", [
    tf.iter_dom_power_vs_dim, tf.iter_dom_power_vs_max_extent,
    tf.sched_gen_area_vs_dim, tf.sched_gen_power_vs_dim,
    tf.sched_gen_area_vs_max_extent, tf.sched_gen_power_vs_max_extent,
    tf.li_sched_loc_area, tf.li_sched_loc_power,
])
def test_full_rate_figures_draw(sweeps, tmp_path, fig):
    out = tmp_path / f"{fig.__name__}.png"
    fig(None, out)
    assert out.stat().st_size > 0


def test_full_rate_figures_read_full_rate_csv(sweeps, tmp_path, monkeypatch):
    """The plotted values are the full-rate ones (3e-6 W), and the RV figure keeps
    the fw4 tiles that match no experiment."""
    seen = []
    real = tf._metric

    def spy(df, blocks, metric, what):
        out = real(df, blocks, metric, what)
        seen.append(out)
        return out
    monkeypatch.setattr(tf, "_metric", spy)
    tf.li_sched_loc_power(None, tmp_path / "li.png")
    tf.iter_dom_power_vs_dim(None, tmp_path / "id.png")
    rv, st = seen
    assert set(rv["vec_width"]) == {4} and len(rv) == 4
    assert rv["y"].min() == pytest.approx(2 * 3e-6)  # c_rvnet + c_sg at dims 1
    assert st["y"].min() == pytest.approx(3e-6)


def test_full_rate_figures_need_full_rate_csv(tmp_path, monkeypatch):
    from THESIS.pipeline.errors import MissingDataError
    monkeypatch.setattr(tile_sweep, "FULL_RATE", tmp_path / "missing.csv")
    with pytest.raises(MissingDataError, match="missing.csv"):
        tf.sched_gen_area_vs_dim(None, tmp_path / "x.png")


def test_reference_off_scale_at_several_steps_goes_to_the_legend():
    """sched_gen_power_vs_dim: y ∝ x is off scale at 4 of 5 dims steps. Four per-step
    notes one dimension apart ran together; they become one legend note."""
    import matplotlib.pyplot as plt
    fig, pax = plt.subplots()
    xs = [1, 2, 3, 4, 5, 6]
    tf._pct_line(pax, xs, [10, 10.5, 11, 11.4, 11.8, 12.1])
    tf._pct_line(pax, xs, [1, 1.2, 1.3, 1.4, 1.5, 1.8])  # steps up to +20%
    tf._pct_axis(pax, xs)
    assert not [t for t in pax.texts if t.get_text()[:1] in "↑↓"]
    assert tf._REF_OFF_SCALE[pax] == "y ∝ x, off scale:\n+25% to +100% per step"
    plt.close(fig)
