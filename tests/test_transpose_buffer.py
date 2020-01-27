from lake.modules.transpose_buffer import *
import fault
import tempfile
from kratos import *
import pytest

#@pytest.mark.skip
def test_transpose_buffer():
    word_width = 1
    fetch_width = 4
    stencil_height = 3
    max_range_value = 5
    num_tb = 1
    max_stencil_height = 3
    dut = TransposeBuffer(word_width,
                        fetch_width,
                        num_tb,
                        stencil_height,
                        max_range_value,
                        max_stencil_height)
    magma_dut = kratos.util.to_magma(dut, flatten_array=True)
    verilog(dut, filename="transposebuffer.sv")
    tester = fault.Tester(magma_dut, magma_dut.clk)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    data = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0,
            0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1,
            1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0,
            1, 1, 1, 1]
    for i in range(64):
        tester.circuit.stencil_height_input = 3
        for j in range(fetch_width):
            setattr(tester.circuit, f"input_data_{j}", data[(i * 4 + fetch_width - 1 - j) % len(data)])
        tester.circuit.range_outer = 5
        tester.circuit.range_inner = 3
        tester.circuit.stride = 2
        tester.circuit.stencil_height_input = 3
        if i == 0 or i == 1 or i == 2:
            tester.circuit.valid_data = 1
        elif i == 3 or i == 4:
            tester.circuit.valid_data = 0
        else:
            if (i-5) % 6 < 3:
                tester.circuit.valid_data = 1
            else:
                tester.circuit.valid_data = 0

        for j in range(max_range_value):
            setattr(tester.circuit, f"indices_{j}", j)
        tester.eval()
        tester.step(2)

        with tempfile.TemporaryDirectory() as tempdir:
            tempdir="/nobackupkiwi/skavya/lake_/lake/tests/temp"
            tester.compile_and_run(target="verilator",
                                directory=tempdir,
                                flags=["-Wno-fatal", "--trace"],
                                magma_output="verilog")

test_transpose_buffer()

