import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.utils.util import transform_strides_and_ranges, generate_pond_api
from lake.collateral2compiler.pond_dsl import *


def test_pond():

    hw = pond.test_magma_lake()
    magma_dut = kts.util.to_magma(hw,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    for start_addr_rd in range(16):
        # Ranges, Strides, Dimensionality, Starting Addr, Starting Addr - Schedule
        ctrl_rd = [[16, 1], [1, 0], 2, start_addr_rd, 16]
        ctrl_wr = [[16, 1], [1, 1], 2, 0, 0]
        pond_config = generate_pond_api(ctrl_rd, ctrl_wr, True)

        for key, value in pond_config.items():
            setattr(tester.circuit, key, value)

        rand.seed(0)
        tester.circuit.clk = 0
        tester.circuit.clk_en = 1
        tester.circuit.tile_en = 1
        tester.circuit.rst_n = 0
        tester.step(1)
        tester.circuit.rst_n = 1
        tester.step(1)
        tester.circuit.clk_en = 1

        interconnect_input_ports = 1

        data_in_pond = [0] * interconnect_input_ports
        valid_in = [0] * interconnect_input_ports
        for i in range(32 - start_addr_rd):
            # Incrementing Data
            data_in_pond[0] = data_in_pond[0] + 1

            if interconnect_input_ports == 1:
                tester.circuit.data_in = data_in_pond[0]
            else:
                for j in range(interconnect_input_ports):
                    setattr(tester.circuit, f"data_in_{j}", data_in_pond[j])

            if i >= 16:
                tester.circuit.data_out.expect(i - 15 + start_addr_rd)


            tester.eval()
            tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir="pond_formal"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    test_pond()
