from lake.models.prefetcher_model import PrefetcherModel
from lake.modules.prefetcher import Prefetcher
from lake.passes.passes import lift_config_reg
import _kratos
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.skip
def test_prefetcher_basic(input_latency=10,
                          max_prefetch=64,
                          fetch_width=32,
                          data_width=16):

    assert input_latency < max_prefetch, "Input latency must be smaller than fifo"

    fw_int = int(fetch_width / data_width)

    # Set up model..
    model_pf = PrefetcherModel(fetch_width=fetch_width,
                               data_width=data_width,
                               max_prefetch=max_prefetch)
    new_config = {}
    new_config['input_latency'] = input_latency

    model_pf.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = Prefetcher(fetch_width=fetch_width,
                     data_width=data_width,
                     max_prefetch=max_prefetch)
    lift_config_reg(dut.internal_generator)
    magma_dut = k.util.to_magma(dut, flatten_array=True,
                                check_multiple_driver=False,
                                check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.circuit.data_in = 0
    tester.circuit.valid_read = 0
    tester.circuit.tba_rdy_in = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    # Seed for posterity
    rand.seed(0)

    data_in = [0 for i in range(fw_int)]

    for i in range(1000):
        # Gen random data
        print(i)
        for j in range(fw_int):
            data_in[j] = rand.randint(0, 2 ** data_width - 1)
        tba_rdy_in = rand.randint(0, 1)
        valid_read = rand.randint(0, 1)
        mem_valid_data = rand.randint(0, 1)

        (model_d, model_v, model_stp, model_mem_valid) = \
            model_pf.interact(data_in, valid_read, tba_rdy_in, mem_valid_data)

        for j in range(fw_int):
            setattr(tester.circuit, f"data_in_{j}", data_in[j])
        tester.circuit.valid_read = valid_read
        tester.circuit.tba_rdy_in = tba_rdy_in

        tester.eval()

        # Check the step
        tester.circuit.prefetch_step.expect(model_stp)
        tester.circuit.valid_out.expect(model_v)
        if (model_v):
            for j in range(fw_int):
                getattr(tester.circuit, f"data_out_{j}").expect(model_d[j])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_prefetcher_basic()
