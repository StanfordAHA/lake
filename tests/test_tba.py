from lake.models.tba_model import TBAModel
from lake.modules.transpose_buffer_aggregation import TransposeBufferAggregation
from lake.passes.passes import lift_config_reg
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
             max_range=5,
             max_range_inner=5):

    model_tba = TBAModel(word_width,
                         fetch_width,
                         num_tb,
                         tb_height,
                         max_range,
                         max_range_inner)

    new_config = {}
    new_config["range_outer"] = 5
    new_config["range_inner"] = 3
    new_config["stride"] = 2
    new_config["indices"] = [0, 1, 2]
    new_config["dimensionality"] = 2
    new_config["tb_height"] = 1
    new_config["starting_addr"] = 0

    model_tba.set_config(new_config=new_config)

    dut = TransposeBufferAggregation(word_width,
                                     fetch_width,
                                     num_tb,
                                     tb_height,
                                     max_range,
                                     max_range_inner,
                                     max_stride=5,
                                     tb_iterator_support=2)

    lift_config_reg(dut.internal_generator)

    magma_dut = k.util.to_magma(dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    # configuration registers
    tester.circuit.tb_0_indices_0 = 0
    tester.circuit.tb_0_indices_1 = 1
    tester.circuit.tb_0_indices_2 = 2

    tester.circuit.tb_0_range_outer = 5
    tester.circuit.tb_0_range_inner = 3
    tester.circuit.tb_0_stride = 2
    tester.circuit.tb_0_dimensionality = 2
    tester.circuit.tb_0_tb_height = 1
    tester.circuit.tb_0_starting_addr = 0

    tester.circuit.clk = 0
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.tba_ren = 1
    tester.circuit.rst_n = 1

    rand.seed(0)

    num_iters = 100
    for i in range(num_iters):

        data = []
        for j in range(fetch_width):
            data.append(rand.randint(0, 2**word_width - 1))

        for j in range(fetch_width):
            setattr(tester.circuit, f"SRAM_to_tb_data_{j}", data[j])

        valid_data = rand.randint(0, 1)
        tester.circuit.valid_data = valid_data

        mem_valid_data = rand.randint(0, 1)
        tester.circuit.mem_valid_data = mem_valid_data

        tb_index_for_data = 0
        tester.circuit.tb_index_for_data = tb_index_for_data

        ack_in = valid_data
        tester.circuit.ack_in = ack_in

        model_data, model_valid = \
            model_tba.tba_main(data, valid_data, ack_in, tb_index_for_data, 1, mem_valid_data)

        tester.eval()
        tester.circuit.tb_to_interconnect_valid.expect(model_valid)
        if model_valid:
            tester.circuit.tb_to_interconnect_data.expect(model_data[0])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_tba()
