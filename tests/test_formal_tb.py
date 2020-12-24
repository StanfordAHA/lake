from lake.top.lake_top import *
import fault
import pytest
import tempfile


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

    for f1 in config:
        setattr(tester.circuit, f1, config[f1])

    counter = 0
    start_cycle = 0
    for i in range(785):
        if i > start_cycle:
            for i in range(4):
                setattr(tester.circuit, f"formal_mem_data_{i}", counter)
                counter += 1

        # check data_out at top level

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_formal_tb()