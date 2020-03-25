from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.passes.passes import lift_config_reg
from lake.models.lake_top_model import LakeTopModel
from lake.modules.strg_fifo import StrgFIFO


def test_storage_fifo(data_width=16,
                      banks=2,
                      memory_width=64,
                      rw_same_cycle=False,
                      read_delay=1):

    fw_int = int(memory_width / data_width)

    new_config = {}
    new_config["fifo_depth"] = 100

    ### DUT
    stg_fifo_dut = StrgFIFO(data_width=16,
                            banks=2,
                            memory_width=64,
                            rw_same_cycle=False,
                            read_delay=1)

    # Run the config reg lift
    lift_config_reg(stg_fifo_dut.internal_generator)

    magma_dut = kts.util.to_magma(stg_fifo_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###
    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    data_in = 0
    push = 1
    pop = 0
    data_from_strg = 0

    for i in range(40):
        data_in += 1

        if i == 10:
            pop = 1

        tester.circuit.data_in = data_in
        tester.circuit.push = push
        tester.circuit.pop = pop

        tester.eval()

        # Now check the outputs

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_storage_fifo()
