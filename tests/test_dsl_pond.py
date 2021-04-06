import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.utils.util import transform_strides_and_ranges
from lake.dsl.dsl_examples.pond import *


# setup for pond tests - only call once
hw = pond.test_magma_lake()
magma_dut = kts.util.to_magma(hw,
                              flatten_array=True,
                              check_multiple_driver=False,
                              optimize_if=False,
                              check_flip_flop_always_ff=False)

tester = fault.Tester(magma_dut, magma_dut.clk)


def test_pond_basic():

    # Ranges, Strides, Dimensionality, Starting Addr
    # Starting Addr (schedule), Ranges (schedule)
    ctrl_rd = [[16, 1], [1, 1], 2, 0, 16, [1, 1]]
    ctrl_wr = [[16, 1], [1, 1], 2, 0, 0, [1, 1]]
    pond_config = hw.generate_pond_api(ctrl_rd, ctrl_wr)

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

    data_in_pond = [0]
    valid_in = [0]
    for i in range(32):
        # Incrementing Data
        data_in_pond[0] = data_in_pond[0] + 1

        tester.circuit.data_in = data_in_pond[0]

        if i >= 16:
            tester.circuit.data_out.expect(i - 15)

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


def test_pond_strided_read():

    # Ranges, Strides, Dimensionality, Starting Addr
    # Starting Addr (schedule), Ranges (schedule)
    ctrl_rd = [[8, 1], [2, 0], 1, 0, 16, [1, 0]]
    ctrl_wr = [[16, 1], [1, 1], 1, 0, 0, [1, 1]]
    pond_config = hw.generate_pond_api(ctrl_rd, ctrl_wr)

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

    data_in_pond = [0]
    valid_in = [0]
    for i in range(24):
        # Incrementing Data
        data_in_pond[0] = data_in_pond[0] + 1

        tester.circuit.data_in = data_in_pond[0]

        if i >= 16:
            tester.circuit.data_out.expect((i - 16) * 2 + 1)

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


def test_pond_b2b_read():

    # Ranges, Strides, Dimensionality, Starting Addr
    # Starting Addr (schedule), Ranges (schedule)
    ctrl_rd = [[16, 1], [1, 1], 2, 0, 16, [1, 1]]
    ctrl_wr = [[16, 1], [1, 1], 2, 0, 0, [1, 1]]
    pond_config = hw.generate_pond_api(ctrl_rd, ctrl_wr)

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

    data_in_pond = [0]
    valid_in = [0]
    for i in range(32):
        # Incrementing Data
        data_in_pond[0] = data_in_pond[0] + 1

        tester.circuit.data_in = data_in_pond[0]

        if i >= 16:
            tester.circuit.data_out.expect(i - 15)

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_pond_strided_read()
