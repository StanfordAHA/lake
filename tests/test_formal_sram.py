from lake.top.lake_top import *
import fault
import pytest
import tempfile


@pytest.mark.skip
def test_formal_sram():
    lt_dut, n, u, t = get_formal_module("sram")

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

    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_0"] = 4
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_enable"] = 1
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_0"] = 769
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_1"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_2"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_3"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_4"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_5"] = 0
    config["strg_ub_sram_only_input_addr_gen_0_strides_0"] = 117
    config["strg_ub_sram_only_input_addr_gen_0_strides_1"] = 511
    config["strg_ub_sram_only_input_addr_gen_0_strides_2"] = 511
    config["strg_ub_sram_only_input_addr_gen_0_strides_3"] = 511
    config["strg_ub_sram_only_input_addr_gen_0_strides_4"] = 511
    config["strg_ub_sram_only_input_addr_gen_0_strides_5"] = 511
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_dimensionality"] = 0
    config["strg_ub_sram_only_output_addr_gen_1_starting_addr"] = 374
    config["strg_ub_sram_tb_shared_output_sched_gen_1_enable"] = 1
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_dimensionality"] = 1
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_0"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_1"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_2"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_3"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_4"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_5"] = 0
    config["strg_ub_sram_only_output_addr_gen_1_strides_0"] = 117
    config["strg_ub_sram_only_output_addr_gen_1_strides_1"] = 511
    config["strg_ub_sram_only_output_addr_gen_1_strides_2"] = 511
    config["strg_ub_sram_only_output_addr_gen_1_strides_3"] = 511
    config["strg_ub_sram_only_output_addr_gen_1_strides_4"] = 511
    config["strg_ub_sram_only_output_addr_gen_1_strides_5"] = 511
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_starting_addr"] = 53
    config["strg_ub_sram_tb_shared_output_sched_gen_0_enable"] = 1
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_0"] = 4
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_sram_only_output_addr_gen_0_starting_addr"] = 67
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_0"] = 180
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_1"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_2"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_3"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_4"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_5"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_0"] = 4
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_starting_addr"] = 0
    config["strg_ub_sram_only_input_addr_gen_0_starting_addr"] = 67
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_enable"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_starting_addr"] = 51
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_0"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_starting_addr"] = 4
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_0"] = 180
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_1"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_2"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_3"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_4"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_5"] = 0
    config["strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_dimensionality"] = 1
    config["strg_ub_sram_only_input_addr_gen_1_starting_addr"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_dimensionality"] = 1
    config["strg_ub_sram_only_output_addr_gen_0_strides_0"] = 117
    config["strg_ub_sram_only_output_addr_gen_0_strides_1"] = 511
    config["strg_ub_sram_only_output_addr_gen_0_strides_2"] = 511
    config["strg_ub_sram_only_output_addr_gen_0_strides_3"] = 511
    config["strg_ub_sram_only_output_addr_gen_0_strides_4"] = 511
    config["strg_ub_sram_only_output_addr_gen_0_strides_5"] = 511
    config["strg_ub_sram_only_input_addr_gen_1_strides_0"] = 0
    config["strg_ub_sram_only_input_addr_gen_1_strides_1"] = 0
    config["strg_ub_sram_only_input_addr_gen_1_strides_2"] = 0
    config["strg_ub_sram_only_input_addr_gen_1_strides_3"] = 0
    config["strg_ub_sram_only_input_addr_gen_1_strides_4"] = 0
    config["strg_ub_sram_only_input_addr_gen_1_strides_5"] = 0

    for f1 in config:
        setattr(tester.circuit, f1, config[f1])

    counter = 0
    curr = 4
    for i in range(785):
        if i == curr:
            curr += 4
            for i in range(4):
                setattr(tester.circuit, f"agg_data_out_top_0_{i}", counter)
                counter += 1

        # check formal_mem_data at top level

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_formal_sram()
