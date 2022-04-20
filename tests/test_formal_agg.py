from lake.top.lake_top import *
import fault
import pytest
import tempfile


@pytest.mark.skip
def test_formal_agg():
    lt_dut, n, u, t = get_formal_module("agg")

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.tile_en = 1
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1
    tester.eval()

    config = {}
    config["strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_starting_addr"] = 0
    config["strg_ub_agg_only_loops_in2buf_0_dimensionality"] = 1
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_enable"] = 1
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_starting_addr"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_dimensionality"] = 1
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_0"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_1"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_2"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_3"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_4"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_5"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_starting_addr"] = 4
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_0_starting_addr"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_1_starting_addr"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_dimensionality"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_0"] = 4
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_0_strides_0"] = 1
    config["strg_ub_agg_only_agg_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_1_strides_0"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_1_strides_1"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_1_strides_2"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_1_strides_5"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_1_strides_0"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_0_enable"] = 1
    config["strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_starting_addr"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_enable"] = 0
    config["strg_ub_agg_only_loops_in2buf_0_ranges_0"] = 782
    config["strg_ub_agg_only_loops_in2buf_0_ranges_1"] = 0
    config["strg_ub_agg_only_loops_in2buf_0_ranges_2"] = 0
    config["strg_ub_agg_only_loops_in2buf_0_ranges_3"] = 0
    config["strg_ub_agg_only_loops_in2buf_0_ranges_4"] = 0
    config["strg_ub_agg_only_loops_in2buf_0_ranges_5"] = 0
    config["strg_ub_agg_only_agg_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_0"] = 194
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_1"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_2"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_3"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_4"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_5"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_1_enable"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_agg_only_loops_in2buf_1_ranges_0"] = 0
    config["strg_ub_agg_only_loops_in2buf_1_ranges_1"] = 0
    config["strg_ub_agg_only_loops_in2buf_1_ranges_2"] = 0
    config["strg_ub_agg_only_loops_in2buf_1_ranges_3"] = 0
    config["strg_ub_agg_only_loops_in2buf_1_ranges_4"] = 0
    config["strg_ub_agg_only_loops_in2buf_1_ranges_5"] = 0
    config["strg_ub_agg_only_loops_in2buf_1_dimensionality"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_0_strides_0"] = 1
    config["strg_ub_agg_only_agg_write_addr_gen_0_strides_1"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_0_strides_2"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_agg_only_agg_write_addr_gen_0_starting_addr"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_5"] = 0

    for f1 in config:
        setattr(tester.circuit, f1, config[f1])

    for i in range(785):
        tester.circuit.data_in_0 = i

        # check agg_data_out (internal signal) or formal_agg_data_out (top level)

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_formal_agg()
