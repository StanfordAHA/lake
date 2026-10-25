"""Unit tests for THESIS/apps/cgra_power_ingest.py: garnet cgra_app_power.py
output (cgra_power.csv + workspace manifests / run_meta / Genus reports) ->
results.json. The fixture is a made-up out-dir with the same layout."""

from __future__ import annotations

import csv
import json
import shutil
from pathlib import Path

import pytest

from THESIS.apps import cgra_power_ingest as ing

STATIC = "fw4_dw16_sc8192_sp_in2_out2_vc2_sharectr_portcg_thesis"
RV = "fw4_dw16_sc8192_sp_in2_out2_vc2_nofp_portcg_thesis_rv"
UNMAPPED = "fw4_dw16_sc4096_sp_in2_out2_vc2_sharectr_portcg_thesis"  # not in DESIGNS_SINGLE_LEVEL


def _gates(cell: float, sram: float) -> str:
    """A Genus final_gates.rpt with the lines tile_sweep._gates reads (made-up numbers)."""
    macro = f"IN12LP_S1DB_FAKE_H        2  {sram:.3f}  lib\n" if sram else ""
    sram_row = f"timing_model              2  {sram:.3f}  10.0\n" if sram else ""
    return (f"  Gate  Instances  Area  Library\n{macro}\n"
            "                 Type   Instances   Area   Area %\n"
            f"{sram_row}sequential  10  {cell / 4:.3f}  25.0\ntotal  100  {cell:.3f}  100.0\n")


def _manifest(app, config, mode, created, pond=None):
    spec = ing.gc.parse_config_name(config)["spec"]  # the config's build_spec kwargs
    return {"app": app, "mode": mode, "thesis": mode, "width": 16, "height": 16,
            "spec_config": spec, "pond_spec": {} if pond is None else pond, "config": config,
            "array_tiles": {"Tile_MemCore": 64, "Tile_PE": 192}, "created": created, "app_env": {}}


ROW = dict(tiles_replayed=30, tiles_exact=30, Tile_MemCore_placed_n=4, Tile_MemCore_placed_W=0.004,
           Tile_MemCore_route_n=1, Tile_MemCore_route_W=0.001, Tile_PE_placed_n=20,
           Tile_PE_placed_W=0.02, Tile_PE_route_n=5, Tile_PE_route_W=0.002, app_W=0.027,
           app_J=1.6e-7, sram_W=0.002, window_s=6e-6, idle_W=0.05, array_W=0.077, array_J=4.6e-7)


def make_out_dir(root: Path, cells: list[tuple[str, str, str, dict]]) -> tuple[Path, Path]:
    """cells: (config, variant, app, row overrides). -> (out-dir, bundle root)."""
    out, bundles = root / "cgra_app_power", root / "bundles"
    rows = []
    for i, (config, variant, app, over) in enumerate(cells):
        mode = "rv" if config.endswith("_rv") else "static"
        created = f"2026-10-2{i} 00:00:00"
        man = _manifest(app, config, mode, created, over.pop("pond", None))
        for tile, step, cell, sram in (("Tile_MemCore", 14, 10000.0, 4000.0), ("Tile_PE", 9, 5000.0, 0.0)):
            ws = out / config / tile
            (ws / f"3-application-{variant}" / "outputs").mkdir(parents=True, exist_ok=True)
            (ws / f"3-application-{variant}" / "outputs" / "manifest.json").write_text(json.dumps(man))
            syn = ws / f"{step}-cadence-genus-synthesis" / "results_syn"
            syn.mkdir(parents=True, exist_ok=True)
            (syn / "final_gates.rpt").write_text(_gates(cell, sram))
            (ws / "run_meta.json").write_text(json.dumps(
                {"config": config, "tile": tile, "clock_mhz": 500.0, "garnet": "abc123",
                 "variants": {variant: f"/nsim/b/{variant}/{config}"}}))
        b = bundles / variant / config
        (b / "logs").mkdir(parents=True, exist_ok=True)
        (b / "manifest.json").write_text(json.dumps(man))
        (b / "logs" / "test.log").write_text(
            "[gaussian] kernel start at 100.00 ns\n"
            "[gaussian] It takes 4000.00 ns total time to run kernel.\n"
            "[gaussian] The size of output is 7200 Byte.\ncomparison passed\n")
        rows.append({"config": config, "variant": variant, "app": app, **ROW, **over})
    with open(out / "cgra_power.csv", "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]))
        w.writeheader()
        w.writerows(rows)
    return out, bundles


def _by_dir(results):
    return {rel: r for rel, r in results}


def test_static_row_maps_to_both_duplicate_design_points(tmp_path):
    out, bundles = make_out_dir(tmp_path, [(STATIC, "gauss", "thesis_x/gaussian", {})])
    results, notes = ing.build_results(ing._Tree(out), kernel_stats=ing.collect_kernel_stats([bundles]))
    got = _by_dir(results)
    assert set(got) == {"port_fw4_dw16_sc8k_sp2x2__static__thesis/gaussian_agg",
                        "mem_fw4_dw16_sc8k_sp2x2__static__thesis/gaussian_agg"}
    assert any("same spec as design points" in n for n in notes)
    r = got["port_fw4_dw16_sc8k_sp2x2__static__thesis/gaussian_agg"]
    assert r["sim_status"] == "PASS" and r["mode"] == "static" and r["fabric"] == "thesis"
    assert r["pond_id"] == "default" and r["spec_options"] == "share_clk_ctr,clock_gate_ports"
    assert r["synth_power_w"] == pytest.approx(0.027)
    assert r["clock_period_ps"] == pytest.approx(2000.0)
    assert r["window_cycles"] == 3000                     # 6 us at 500 MHz
    assert r["kernel_cycles"] == r["total_cycles"] == 4000  # 4000 ns at aha test's 1 ns clock
    assert r["output_words"] == 3600                      # 7200 B of 16-bit words
    assert r["output_per_cycle"] == pytest.approx(0.9)
    assert r["tile_count"] == 4 and r["mem_tile_utilization"] == pytest.approx(4 / 64)
    # area the app touches: (4 + 1) MEM + (20 + 5) PE tiles
    assert r["mem_tile_area_um2"] == 10000.0 and r["pe_tile_area_um2"] == 5000.0
    assert r["synth_total_area_um2"] == pytest.approx(5 * 10000 + 25 * 5000)
    assert r["synth_storage_area_um2"] == pytest.approx(5 * 4000)
    assert r["synth_logic_area_um2"] == pytest.approx(5 * 6000 + 25 * 5000)
    assert r["utilization"] is None


def test_zip_reads_like_the_dir(tmp_path):
    out, bundles = make_out_dir(tmp_path, [(STATIC, "gauss", "apps/gaussian", {}),
                                           (RV, "gauss", "apps/gaussian", {})])
    zpath = Path(shutil.make_archive(str(tmp_path / "z"), "zip", tmp_path, out.name))
    stats = ing.collect_kernel_stats([bundles])
    a = _by_dir(ing.build_results(ing._Tree(out), kernel_stats=stats)[0])
    b = _by_dir(ing.build_results(ing._Tree(zpath), kernel_stats=stats)[0])
    assert set(a) == set(b) and len(a) == 4
    for k in a:
        assert {**a[k], "source_path": ""} == {**b[k], "source_path": ""}
    assert "port_fw4_dw16_sc8k_sp2x2__rv__thesis/gaussian_agg" in a


def test_unmapped_design_and_app_are_skipped_unless_kept(tmp_path):
    out, _ = make_out_dir(tmp_path, [(UNMAPPED, "gauss", "apps/gaussian", {}),
                                     (STATIC, "conv", "tests/conv_3_3", {})])
    results, notes = ing.build_results(ing._Tree(out))
    assert results == []
    assert any("no design point with this geometry" in n for n in notes)
    assert any("app conv_3_3 not in registry.APPS" in n for n in notes)
    kept = _by_dir(ing.build_results(ing._Tree(out), keep_unmapped=True)[0])
    assert "fw4_dw16_sc4096_sp_in2_out2_vc2__static__thesis/gaussian_agg" in kept
    assert kept["fw4_dw16_sc4096_sp_in2_out2_vc2__static__thesis/gaussian_agg"]["in_design_set"] is False
    assert "port_fw4_dw16_sc8k_sp2x2__static__thesis/conv_3_3" in kept
    mapped = ing.build_results(ing._Tree(out), apps=ing.gc.app_map({"conv_3_3": "conv"}))[0]
    assert {rel.split("/")[1] for rel, _ in mapped} == {"conv"}


def test_row_whose_manifest_disagrees_with_its_config_name_is_skipped(tmp_path):
    out, _ = make_out_dir(tmp_path, [(STATIC, "gauss", "apps/gaussian", {})])
    p = out / STATIC / "Tile_MemCore" / "3-application-gauss" / "outputs" / "manifest.json"
    man = json.loads(p.read_text())
    man["spec_config"]["storage_capacity"] = 2048
    for tile in ("Tile_MemCore", "Tile_PE"):
        (out / STATIC / tile / "3-application-gauss" / "outputs" / "manifest.json").write_text(json.dumps(man))
    results, notes = ing.build_results(ing._Tree(out))
    assert results == [] and any("not the config name's" in n for n in notes)


def test_variant_names_tell_the_two_matmuls_apart(tmp_path):
    out, bundles = make_out_dir(tmp_path, [(STATIC, "matmul_agg", "tests/matmul_rvtry3", {}),
                                           (STATIC, "matmul_pond_agg", "pond_x/matmul_rvtry3", {}),
                                           (RV, "mmpond", "tests/matmul_rvtry3", {})])
    got = {(r["variant"], r["design_id"]): r for _, r in ing.build_results(ing._Tree(out))[0]}
    assert got[("matmul_agg", "port_fw4_dw16_sc8k_sp2x2")]["app_id"] == "matmul_agg"
    assert got[("matmul_agg", "port_fw4_dw16_sc8k_sp2x2")]["level"] == "single"
    assert got[("matmul_pond_agg", "port_fw4_dw16_sc8k_sp2x2")]["level"] == "two"
    assert not any(v == "mmpond" for v, _ in got)  # basename matmul_rvtry3 is ambiguous
    kept = ing.build_results(ing._Tree(out), apps=ing.gc.app_map({"mmpond": "matmul_pond_agg"}))[0]
    assert {r["app_id"] for _, r in kept if r["variant"] == "mmpond"} == {"matmul_pond_agg"}


def test_replay_mismatch_and_pond_spec(tmp_path):
    out, _ = make_out_dir(tmp_path, [(STATIC, "mm", "apps/gaussian",
                                      {"tiles_exact": 29, "pond": {"storage_capacity": 128, "dims": 6}})])
    rel, r = ing.build_results(ing._Tree(out))[0][0]
    assert r["sim_status"] == "FAIL"
    assert r["pond_id"] == "dims6_storage_capacity128"
    assert rel.startswith("port_fw4_dw16_sc8k_sp2x2__static__thesis__pond-dims6_storage_capacity128/")
    assert "total_cycles" not in r  # no kernel stats given


def test_kernel_stats_keep_variants_of_one_app_apart(tmp_path):
    out, bundles = make_out_dir(tmp_path, [(RV, "A", "cgra_ab_A/gaussian", {}),
                                           (RV, "B", "cgra_ab_B/gaussian", {})])
    log = bundles / "B" / RV / "logs" / "test.log"
    log.write_text(log.read_text().replace("4000.00 ns", "5000.00 ns"))
    stats = ing.collect_kernel_stats([bundles])
    assert len(stats) == 2
    ks = tmp_path / "ks.json"
    ing.dump_kernel_stats(stats, ks)
    assert ing.load_kernel_stats(ks) == stats
    got = {r["variant"]: r["kernel_cycles"] for _, r in ing.build_results(ing._Tree(out), kernel_stats=stats)[0]}
    assert got == {"A": 4000, "B": 5000}


def test_manifest_kernel_entry_wins(tmp_path):
    out, _ = make_out_dir(tmp_path, [(STATIC, "gauss", "apps/gaussian", {})])
    p = out / STATIC / "Tile_MemCore" / "3-application-gauss" / "outputs" / "manifest.json"
    man = json.loads(p.read_text())
    man["kernel"] = {"kernel_ns": 2000.0, "clk_period_ns": 2.0, "output_bytes": 100}
    p.write_text(json.dumps(man))
    r = ing.build_results(ing._Tree(out))[0][0][1]
    assert r["kernel_cycles"] == 1000 and r["output_words"] == 50 and r["kernel_source"] == "manifest"


def test_parse_test_log_sums_kernels():
    text = ("[a] It takes 10.00 ns total time to run kernel.\n[a] The size of output is 8 Byte.\n"
            "[b] It takes 5.50 ns total time to run kernel.\n[b] The size of output is 4 Byte.\n")
    assert ing.parse_test_log(text) == {"kernel_ns": 15.5, "output_bytes": 12, "n_kernels": 2}
    assert ing.parse_test_log("no kernel here") == {}
    assert ing.kernel_entry({"kernel_ns": 15.5, "output_bytes": 12}, 32)["output_words"] == 3


def test_tile_sweep_area_overrides_the_mem_tile(tmp_path):
    out, _ = make_out_dir(tmp_path, [(STATIC, "gauss", "apps/gaussian", {})])
    sweep = {STATIC: {"config": STATIC, "syn_cell": "12000", "syn_sram": "4000", "pnr_cell": "13000"}}
    r = ing.build_results(ing._Tree(out), sweep=sweep)[0][0][1]
    assert r["area_source"] == "tile_sweep"
    assert r["mem_tile_area_um2"] == 12000.0 and r["mem_tile_pnr_area_um2"] == 13000.0
    assert r["synth_total_area_um2"] == pytest.approx(5 * 12000 + 25 * 5000)


def test_cli_writes_results_tree(tmp_path, capsys):
    out, bundles = make_out_dir(tmp_path, [(STATIC, "gauss", "apps/gaussian", {})])
    ks = tmp_path / "ks.json"
    assert ing.main(["--bundles", str(bundles), "--write-kernel-stats", str(ks)]) == 0
    root = tmp_path / "apps"
    assert ing.main([str(out), "--kernel-stats", str(ks), "--results-root", str(root)]) == 0
    files = sorted(p.relative_to(root).as_posix() for p in root.rglob("results.json"))
    assert files == ["mem_fw4_dw16_sc8k_sp2x2__static__thesis/gaussian_agg/results.json",
                     "port_fw4_dw16_sc8k_sp2x2__static__thesis/gaussian_agg/results.json"]
    assert json.loads((root / files[0]).read_text())["total_cycles"] == 4000
