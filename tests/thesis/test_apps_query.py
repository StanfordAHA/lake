"""Unit tests for THESIS/pipeline/apps_query.py — the loader that turns the
app-mapping harness's per-cell results.json files into a tidy DataFrame."""

from __future__ import annotations

import json
from pathlib import Path

import pandas as pd
import pytest

from THESIS.pipeline import apps_query


def _write_result(root: Path, design: str, app: str, payload: dict) -> Path:
    d = root / design / app
    d.mkdir(parents=True, exist_ok=True)
    p = d / "results.json"
    p.write_text(json.dumps(payload))
    return p


def test_load_returns_empty_df_when_tree_missing(tmp_path):
    df = apps_query.load_app_results_df(tmp_path / "does" / "not" / "exist")
    assert isinstance(df, pd.DataFrame)
    assert df.empty


def test_load_returns_empty_df_when_tree_has_no_results(tmp_path):
    (tmp_path / "d1" / "a1").mkdir(parents=True)  # empty subdir, no results.json
    df = apps_query.load_app_results_df(tmp_path)
    assert df.empty


def test_load_reads_every_results_json(tmp_path):
    _write_result(tmp_path, "d1", "a1", {"design_id": "d1", "app_id": "a1",
                                         "sim_status": "PASS", "total_cycles": 100})
    _write_result(tmp_path, "d1", "a2", {"design_id": "d1", "app_id": "a2",
                                         "sim_status": "PASS", "total_cycles": 200})
    _write_result(tmp_path, "d2", "a1", {"design_id": "d2", "app_id": "a1",
                                         "sim_status": "PASS", "total_cycles": 300})
    df = apps_query.load_app_results_df(tmp_path)
    assert len(df) == 3
    assert set(df["design_id"]) == {"d1", "d2"}
    assert set(df["app_id"]) == {"a1", "a2"}
    assert set(df["total_cycles"]) == {100, 200, 300}


def test_pass_only_default_filters_failed_cells(tmp_path):
    _write_result(tmp_path, "d1", "ok",   {"design_id": "d1", "app_id": "ok",
                                           "sim_status": "PASS"})
    _write_result(tmp_path, "d1", "bad",  {"design_id": "d1", "app_id": "bad",
                                           "sim_status": "FAIL"})
    _write_result(tmp_path, "d1", "miss", {"design_id": "d1", "app_id": "miss",
                                           "sim_status": "MISSING"})
    df = apps_query.load_app_results_df(tmp_path)
    assert list(df["sim_status"]) == ["PASS"]
    assert list(df["app_id"]) == ["ok"]


def test_pass_only_false_keeps_everything(tmp_path):
    _write_result(tmp_path, "d1", "ok",  {"design_id": "d1", "app_id": "ok",
                                          "sim_status": "PASS"})
    _write_result(tmp_path, "d1", "bad", {"design_id": "d1", "app_id": "bad",
                                          "sim_status": "FAIL"})
    df = apps_query.load_app_results_df(tmp_path, pass_only=False)
    assert set(df["sim_status"]) == {"PASS", "FAIL"}


def test_malformed_json_is_skipped_not_raised(tmp_path):
    good = _write_result(tmp_path, "d1", "a1", {"design_id": "d1", "app_id": "a1",
                                                "sim_status": "PASS"})
    bad = tmp_path / "d1" / "corrupt" / "results.json"
    bad.parent.mkdir()
    bad.write_text("{ not valid json ")
    df = apps_query.load_app_results_df(tmp_path)
    assert len(df) == 1
    assert df.iloc[0]["app_id"] == "a1"


def test_list_and_dict_fields_are_dropped_from_scalar_columns(tmp_path):
    """per_tile_util (list) and hypothetical dicts must not become DataFrame
    columns — callers who need them read the JSON directly."""
    _write_result(tmp_path, "d1", "a1", {
        "design_id": "d1", "app_id": "a1", "sim_status": "PASS",
        "total_cycles": 100,
        "per_tile_util": [{"tile": "cfg_0", "active_cycles": 50}],
        "nested": {"a": 1},
    })
    df = apps_query.load_app_results_df(tmp_path)
    assert "per_tile_util" not in df.columns
    assert "nested" not in df.columns
    assert "total_cycles" in df.columns


def test_source_column_is_relative_path(tmp_path):
    _write_result(tmp_path, "d1", "a1", {"design_id": "d1", "app_id": "a1",
                                         "sim_status": "PASS"})
    df = apps_query.load_app_results_df(tmp_path)
    assert df.iloc[0]["_source"] == "d1/a1/results.json"


def test_default_root_used_when_none_passed(tmp_path, monkeypatch):
    """Passing None falls back to apps_query.DEFAULT_RESULTS_ROOT."""
    monkeypatch.setattr(apps_query, "DEFAULT_RESULTS_ROOT", tmp_path)
    _write_result(tmp_path, "d1", "a1", {"design_id": "d1", "app_id": "a1",
                                         "sim_status": "PASS"})
    df = apps_query.load_app_results_df()  # no arg
    assert len(df) == 1
