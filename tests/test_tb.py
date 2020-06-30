from lake.models.tb_model import TBModel
from lake.modules.transpose_buffer import TransposeBuffer
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.parametrize("start_addr", [0, 1])
def test_tb(start_addr,
            word_width=16,
            fetch_width=4,
            num_tb=1,
            max_tb_height=1,
            max_range=5,
            max_range_inner=5,
            max_stride=15,
            tb_iterator_support=2):

    model_tb = TBModel(word_width,
                       fetch_width,
                       num_tb,
                       max_tb_height,
                       max_range,
                       max_range_inner)

    new_config = {}
    new_config["range_outer"] = 5
    new_config["range_inner"] = 3
    new_config["stride"] = 2
    new_config["indices"] = [0, 1, 2]
    new_config["tb_height"] = 1
    new_config["dimensionality"] = 2
    new_config["starting_addr"] = start_addr

    model_tb.set_config(new_config=new_config)

    dut = TransposeBuffer(word_width,
                          fetch_width,
                          num_tb,
                          max_tb_height,
                          max_range,
                          max_range_inner,
                          max_stride,
                          tb_iterator_support)

    magma_dut = k.util.to_magma(dut, flatten_array=True, check_flip_flop_always_ff=False)
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
    tester.circuit.tb_height = 1
    tester.circuit.dimensionality = 2
    tester.circuit.starting_addr = start_addr

    rand.seed(0)

    num_iters = 300
    for i in range(num_iters):
        data = []
        for j in range(fetch_width):
            data.append(rand.randint(0, 2**word_width - 1))

        for j in range(fetch_width):
            setattr(tester.circuit, f"input_data_{j}", data[j])

        if i % fetch_width == 0:
            valid_data = 1
        else:
            valid_data = 0

        valid_data = rand.randint(0, 1)
        tester.circuit.valid_data = valid_data

        input_data = data

        mem_valid_data = rand.randint(0, 1)
        tester.circuit.mem_valid_data = mem_valid_data

        ack_in = valid_data
        tester.circuit.ack_in = ack_in

        ren = 1
        tester.circuit.ren = ren

        model_data, model_valid, model_rdy_to_arbiter = \
            model_tb.interact(input_data, valid_data, ack_in, ren, mem_valid_data)

        # print("i: ", i, " model valid ", model_valid, " model data ", model_data)
        tester.eval()

        tester.circuit.output_valid.expect(model_valid)
        if model_valid:
            tester.circuit.col_pixels.expect(model_data[0])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


def test_id(word_width=16,
            fetch_width=4,
            num_tb=1,
            max_tb_height=1,
            max_range=12,
            max_range_inner=5,
            max_stride=15,
            tb_iterator_support=2):

    model_tb = TBModel(word_width,
                       fetch_width,
                       num_tb,
                       max_tb_height,
                       max_range,
                       max_range_inner)

    new_config = {}
    new_config["range_outer"] = 12
    new_config["range_inner"] = 3
    new_config["stride"] = 1
    new_config["indices"] = [0, 1, 2]
    new_config["tb_height"] = 1
    new_config["dimensionality"] = 1
    new_config["starting_addr"] = 0

    model_tb.set_config(new_config=new_config)

    dut = TransposeBuffer(word_width,
                          fetch_width,
                          num_tb,
                          max_tb_height,
                          max_range,
                          max_range_inner,
                          max_stride,
                          tb_iterator_support)

    magma_dut = k.util.to_magma(dut, flatten_array=True)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1

    # configuration registers

    # dimensionality = 1 version
    tester.circuit.indices_0 = 0
    tester.circuit.indices_1 = 1
    tester.circuit.indices_2 = 2

    tester.circuit.range_outer = 12
    tester.circuit.range_inner = 3
    tester.circuit.stride = 1
    tester.circuit.tb_height = 1
    tester.circuit.dimensionality = 1
    tester.circuit.starting_addr = 0

    rand.seed(0)

    num_iters = 300
    for i in range(num_iters):
        # print()
        # print("i: ", i)

        data = []
        for j in range(fetch_width):
            data.append(rand.randint(0, 2**word_width - 1))

        for j in range(fetch_width):
            setattr(tester.circuit, f"input_data_{j}", data[j])

        valid_data = rand.randint(0, 1)
        tester.circuit.valid_data = valid_data

        input_data = data

        mem_valid_data = rand.randint(0, 1)
        tester.circuit.mem_valid_data = mem_valid_data

        ack_in = valid_data
        tester.circuit.ack_in = ack_in

        ren = 1
        tester.circuit.ren = ren

        model_data, model_valid, model_rdy_to_arbiter = \
            model_tb.interact(input_data, valid_data, ack_in, ren, mem_valid_data)

        tester.eval()

        tester.circuit.output_valid.expect(model_valid)
        if model_valid:
            tester.circuit.col_pixels.expect(model_data[0])
        # print("model data ", model_data, " model_valid ", model_valid)

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


def test_fw1(word_width=16,
             fetch_width=1,
             num_tb=1,
             max_tb_height=1,
             max_range=5,
             max_range_inner=5,
             max_stride=15,
             tb_iterator_support=2):

    model_tb = TBModel(word_width,
                       fetch_width,
                       num_tb,
                       max_tb_height,
                       max_range,
                       max_range_inner)

    new_config = {}
    new_config["range_outer"] = 5
    new_config["range_inner"] = 3
    new_config["stride"] = 1
    new_config["indices"] = [0, 1, 2]
    new_config["tb_height"] = 1
    new_config["dimensionality"] = 1
    new_config["starting_addr"] = 0

    model_tb.set_config(new_config=new_config)

    dut = TransposeBuffer(word_width,
                          fetch_width,
                          num_tb,
                          max_tb_height,
                          max_range,
                          max_range_inner,
                          max_stride,
                          tb_iterator_support)

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
    tester.circuit.stride = 1
    tester.circuit.tb_height = 1
    tester.circuit.dimensionality = 1
    tester.circuit.starting_addr = 0

    rand.seed(0)

    data = 0
    num_iters = 300
    for i in range(num_iters):
        # print()
        # print("i: ", i)

        data = rand.randint(0, 2**word_width - 1)
        tester.circuit.input_data = data

        valid_data = rand.randint(0, 1)
        tester.circuit.valid_data = valid_data

        input_data = data

        mem_valid_data = rand.randint(0, 1)
        tester.circuit.mem_valid_data = mem_valid_data

        ack_in = valid_data
        tester.circuit.ack_in = ack_in

        ren = 1
        tester.circuit.ren = ren

        model_data, model_valid, model_rdy_to_arbiter = \
            model_tb.interact(input_data, valid_data, ack_in, ren, mem_valid_data)

        # print("i: ", i, " model valid ", model_valid, " model data ", model_data)
        tester.eval()

        tester.circuit.output_valid.expect(model_valid)
        if model_valid:
            tester.circuit.col_pixels.expect(model_data[0])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_tb()
    # test_id()
    # test_fw1()
