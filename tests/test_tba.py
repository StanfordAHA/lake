from lake.models.tba_model import TBAModel
from lake.modules.transpose_buffer_aggregation import TransposeBufferAggregation
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


def test_tba(word_width=16,
            fetch_width=4,
            num_tb=1,
            tb_height=1,
            max_range=5):

    model_tba = TBAModel(word_width,
                        fetch_width,
                        num_tb,
                        tb_height,
                        max_range)

    new_config = {}
    new_config["range_outer"] = 5
    new_config["range_inner"] = 3
    new_config["stride"] = 2
    new_config["indices"] = [0, 1, 2]

    model_tba.set_config(new_config=new_config)

    dut = TransposeBufferAggregation(word_width,
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

    # configuration registers
    tester.circuit.indices_0 = 0
    tester.circuit.indices_1 = 1
    tester.circuit.indices_2 = 2

    tester.circuit.range_outer = 5
    tester.circuit.range_inner = 3
    tester.circuit.stride = 2

    rand.seed(0)

    num_iters = 5
    for i in range(num_iters):
        print()
        print("i: ", i)

        data = []
        for j in range(fetch_width):
            data.append(rand.randint(0, 2**word_width - 1))

        for j in range(fetch_width):
            setattr(tester.circuit, f"SRAM_to_tb_data_{j}", data[j])

        valid_data = 1
        tester.circuit.valid_data = valid_data

        tb_index_for_data = 0
        tester.circuit.tb_index_for_data = tb_index_for_data

        ack_in = valid_data
        tester.circuit.ack_in = ack_in

        model_data, model_valid = \
                model_tba.tba_main(data, valid_data, ack_in, tb_index_for_data)

        tester.eval()
        tester.circuit.tb_to_interconnect_valid.expect(model_valid)
        if model_valid:
            tester.circuit.tb_to_interconnect_data.expect(model_data[0])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "top_dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])

test_tba()
