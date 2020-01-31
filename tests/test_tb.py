from lake.models.tb_model import TBModel
from lake.modules.transpose_buffer import TransposeBuffer
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.skip
def test_tb(word_width=1,
            fetch_width=4,
            num_tb=1,
            tb_height=1,
            max_range=5):

    model_tb = TBModel(word_width,
                       fetch_width,
                       num_tb,
                       tb_height,
                       max_range)

    new_config = {}
    new_config["range_outer"] = 5
    new_config["range_inner"] = 3
    new_config["stride"] = 2
    new_config["indices"] = [0, 1, 2]

    model_tb.set_config(new_config=new_config)

    dut = TransposeBuffer(word_width,
                          fetch_width,
                          num_tb,
                          tb_height,
                          max_range)

    magma_dut = k.util.to_magma(dut, flatten_array=True)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    data = [0, 0, 0, 1, 5, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0,
            0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1,
            1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0,
            1, 1, 1, 1]

    data = []
    for i in range(64):
        data.append(i)

    for i in range(32):
        print("i: ", i)
        for j in range(fetch_width):
            # set formula for this as well as model eventually
            setattr(tester.circuit, f"input_data_{j}", data[(i * fetch_width + j) % 50])
        tester.circuit.range_outer = 5
        tester.circuit.range_inner = 3
        tester.circuit.stride = 2

        if i == 0 or i == 1 or i == 2:
            valid_data = 0
        elif i % 3 == 0:
            valid_data = 1
        else:
            valid_data = 0

        tester.circuit.valid_data = valid_data

        for j in range(max_range):
            setattr(tester.circuit, f"indices_{j}", j)

        input_data = data[i * fetch_width % 50:(i * fetch_width + 4) % 50]

        if i == 0:
            ack_in = 0

        tester.circuit.ack_in = ack_in
        if len(input_data) != fetch_width:
            input_data = data[0:4]
        print("input data: ", input_data)
        model_data, model_valid, model_rdy_to_arbiter = \
            model_tb.transpose_buffer(input_data, valid_data, ack_in)
        tester.eval()

        tester.circuit.output_valid.expect(model_valid)
        tester.circuit.rdy_to_arbiter.expect(model_rdy_to_arbiter)
        print("model rdy: ", model_rdy_to_arbiter)
        print("model output valid: ", model_valid)
        print(model_data[0])
        if model_valid:
            tester.circuit.col_pixels.expect(model_data[0])

        tester.step(2)

        if model_rdy_to_arbiter:
            ack_in = 1
        else:
            ack_in = 0

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
