from lake.top.lake_top import *
import fault
import pytest
import tempfile


@pytest.mark.skip
def test_formal_tb():
    lt_dut, n, u, t = get_formal_module("tb")

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
    '''config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 65535
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 65535
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 65535
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 65535
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 65535
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 56
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 65535
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 65535
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 65535
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 65535
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 65535
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_0"] = 9
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_1"] = 15
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_3"] = 15
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_4"] = 15
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_5"] = 15
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_dimensionality"] = 1
    config["strg_ub_sram_tb_shared_output_sched_gen_0_enable"] = 1
    config["strg_ub_tb_only_loops_buf2out_read_1_dimensionality"] = 1
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_0"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_1"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_2"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_3"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_4"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_5"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_0"] = 180
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_1"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_2"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_3"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_4"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_5"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_dimensionality"] = 1
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_0"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_1"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_2"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_3"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_4"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_5"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_0_starting_addr"] = 14
    config["strg_ub_tb_only_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_0"] = 4
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_1"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_2"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_3"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_4"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_5"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_0"] = 180
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_1"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_2"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_3"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_4"] = 65535
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_5"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_starting_addr"] = 53
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_0"] = 9
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_1"] = 15
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_2"] = 15
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_3"] = 15
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_4"] = 15
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_5"] = 15
    config["strg_ub_tb_only_tb_read_sched_gen_1_enable"] = 1
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 56
    config["strg_ub_sram_tb_shared_output_sched_gen_1_enable"] = 1
    config["strg_ub_tb_only_tb_read_addr_gen_0_starting_addr"] = 8
    config["strg_ub_tb_only_tb_read_sched_gen_0_enable"] = 1
    config["strg_ub_tb_only_tb_read_addr_gen_1_starting_addr"] = 12
    config["strg_ub_tb_only_tb_write_addr_gen_1_starting_addr"] = 15
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_0"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_1"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_2"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_3"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_4"] = 65535
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_5"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_0"] = 4
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_1"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_2"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_3"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_4"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_5"] = 65535
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_starting_addr"] = 51
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_0"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_1"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_2"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_3"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_4"] = 15
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_5"] = 15'''

    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_starting_addr"] = 56
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_0"] = 1
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_0"] = 1
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_only_tb_read_addr_gen_1_strides_5"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_dimensionality"] = 1
    config["strg_ub_sram_tb_shared_output_sched_gen_0_enable"] = 1
    config["strg_ub_tb_only_loops_buf2out_read_1_dimensionality"] = 1
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_0"] = 766
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_1"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_2"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_3"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_4"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_1_ranges_5"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_0"] = 180
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_1"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_2"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_3"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_4"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_5"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_dimensionality"] = 1
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_0"] = 1
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_0_starting_addr"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_0_dimensionality"] = 1
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_0"] = 4
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_1"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_2"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_3"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_4"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_5"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_0"] = 180
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_1"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_2"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_3"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_4"] = 0
    config["strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_5"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_starting_addr"] = 53
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_0"] = 1
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_1"] = 0
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_2"] = 0
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_3"] = 0
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_4"] = 0
    config["strg_ub_tb_only_tb_read_addr_gen_0_strides_5"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_1_enable"] = 1
    config["strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_starting_addr"] = 56
    config["strg_ub_sram_tb_shared_output_sched_gen_1_enable"] = 1
    config["strg_ub_tb_only_tb_read_addr_gen_0_starting_addr"] = 0
    config["strg_ub_tb_only_tb_read_sched_gen_0_enable"] = 1
    config["strg_ub_tb_only_tb_read_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_1_starting_addr"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_0"] = 766
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_1"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_2"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_3"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_4"] = 0
    config["strg_ub_tb_only_loops_buf2out_read_0_ranges_5"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_0"] = 4
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_1"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_2"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_3"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_4"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_5"] = 0
    config["strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_starting_addr"] = 51
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_0"] = 1
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_1"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_2"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_3"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_4"] = 0
    config["strg_ub_tb_only_tb_write_addr_gen_1_strides_5"] = 0
    for f1 in config:
        setattr(tester.circuit, f1, config[f1])

    counter = 0
    start_cycle = 0
    for i in range(785):
        if i > start_cycle and i % 2 == 0:
            for i in range(4):
                setattr(tester.circuit, f"formal_mem_data_0_0_{i}", counter)
                counter += 1

        # check data_out at top level
        # with the input data put in, data out should increase
        # by 1 and go back and forth between the two output ports
        # for conv_3_3 for the right solution

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_formal_tb()
