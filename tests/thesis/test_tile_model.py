"""The Memory Tile model behind tab:memtile_model_coeff / tab:memtile_model_verif
(THESIS/pipeline/regression.py, tile-sweep part)."""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from THESIS.pipeline import regression as r
from THESIS.pipeline.errors import MissingDataError


def sweep(n=60, seed=0):
    """A tile-sweep-shaped DataFrame whose non-SRAM costs are exactly linear
    in the model's features (static and RV)."""
    rng = np.random.default_rng(seed)
    rows = []
    for mode in ("static", "rv"):
        for i in range(n):
            fw = int(rng.choice([1, 2, 4, 8]))
            ports = int(rng.choice([1, 2, 4]))
            row = dict(config=f"c{i}_{mode}", mode=mode, dims=int(rng.integers(1, 7)),
                       vec_width=fw, vec_capacity=int(rng.choice([2, 4, 8])),
                       in_ports=ports, out_ports=ports, dual_port=bool(rng.integers(0, 2)),
                       max_extent=float(rng.choice([64, 256, 1024, 4096, np.nan])),
                       max_sequence_width=np.nan,
                       storage_capacity=int(rng.choice([2048, 4096, 8192, 16384, 32768])),
                       data_width=16, sweep="test_sweep",
                       pnr_role="validate" if i < 2 else "")
            log2_me = np.log2(row["max_extent"]) if row["max_extent"] == row["max_extent"] else 11
            base = (100 * row["dims"] + 500 * np.log2(fw) + 300 * 2 * ports +
                    50 * log2_me + 40 * np.log2(row["storage_capacity"]) + 2000)
            row["syn_std"] = base
            row["pw_synth_idle_total"], row["pw_synth_idle_sram"] = (base / 10 + 7) * 1e-6, 7e-6
            row["pw_synth_active_total"], row["pw_synth_active_sram"] = (base / 4 + 30) * 1e-6, 30e-6
            rows.append(row)
    return pd.DataFrame(rows)


def test_frame_drops_the_sram_and_fills_spec_defaults():
    f = r.tile_model_frame(sweep())
    assert list(r.TILE_FEATURES) == [c for c in f.columns if c in r.TILE_FEATURES]
    nan_me = sweep()["max_extent"].isna()
    assert (f.loc[nan_me, "log2_me"] == 11).all() and (f["log2_msw"] == 16).all()
    assert np.allclose(f["idle"], f["area"] / 10) and np.allclose(f["active"], f["area"] / 4)


def test_fit_recovers_a_linear_cost():
    fit = r.fit_tile(r.tile_model_frame(sweep()), "static", "area")
    assert fit.coef["ports"] == pytest.approx(300, rel=0.02)   # per port, in or out
    assert fit.coef["log2_fw"] == pytest.approx(500, rel=0.02)
    assert fit.coef["log2_dw"] == 0          # constant in the sweep: no coefficient
    assert fit.mape < 0.01


def test_tables_render():
    df = sweep()
    coef = r.render_tile_coef_table(r.fit_tile_models(r.tile_model_frame(df)))
    assert r"\multicolumn{3}{c}{Static}" in coef and r"\multicolumn{3}{c}{RV}" in coef
    assert "Ports (in + out)" in coef and coef.count(r"\\") == 3 + len(r.TILE_FEATURES) + 3
    rows = r.tile_holdout_errors(r.tile_model_frame(df))
    assert {x["test"] for x in rows} >= {"Held-out builds", "Capacity 32\\,KB (extrapolated)"}
    held = [x for x in rows if x["test"] == "Held-out builds"]
    assert [x["n"] for x in held] == [2, 2]
    assert all(x["area"][1] < 0.02 for x in held)   # exact linear data: tiny error
    assert "mean / max" in r.render_tile_verif_table(rows)


def test_too_few_configs():
    with pytest.raises(MissingDataError):
        r.fit_tile(r.tile_model_frame(sweep(n=5)), "static", "area")
