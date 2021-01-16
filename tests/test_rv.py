import fault
import pytest
import tempfile
from lake.modules.rv_top import SchedGenTop
import kratos as kts

def test_rv():
    lt_dut = SchedGenTop()

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1
    tester.eval()

    reg_vals = [16, 16, 16, 0, 4, 8]
    config = {}
    count = 0
    for reg in ("sched_gen_range_product", "sched_gen_starting_addr"):
        for mem in ("agg", "sram", "tb"):
            config_reg = mem + "_" + reg
            config[config_reg] = reg_vals[count]
            count += 1

    for f1 in config:
        print(f1, config[f1])
        setattr(tester.circuit, f1, config[f1])

    for i in range(16):
        tester.circuit.valid_in = 1

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "rv"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    test_rv()
