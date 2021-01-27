from lake.models.agg_buff_model import AggBuffModel
from lake.modules.aggregation_buffer import AggregationBuffer
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.skip
def test_agg_buff_basic(agg_height=4,
                        data_width=16,
                        mem_width=64,
                        max_agg_schedule=64):

    # Set up model...
    model_ab = AggBuffModel(agg_height=agg_height,
                            data_width=data_width,
                            mem_width=mem_width,
                            max_agg_schedule=max_agg_schedule)
    new_config = {}
    new_config['in_sched_0'] = 0
    new_config['in_sched_1'] = 1
    new_config['in_sched_2'] = 2
    new_config['in_sched_3'] = 3
    new_config['out_sched_0'] = 0
    new_config['out_sched_1'] = 1
    new_config['out_sched_2'] = 2
    new_config['out_sched_3'] = 3
    new_config['in_period'] = 4
    new_config['out_period'] = 4

    model_ab.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = AggregationBuffer(agg_height=agg_height,
                            data_width=data_width,
                            mem_width=mem_width,
                            max_agg_schedule=max_agg_schedule)

    magma_dut = k.util.to_magma(dut, flatten_array=True,
                                check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    num_per_agg = int(mem_width / data_width)

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    rand.seed(0)
    rand.randint(0, 1)

    data_in = 0
    valid_in = 0

    write_act = 0

    for i in range(1000):

        valid_in = rand.randint(0, 1)
        data_in = rand.randint(0, 2 ** 16 - 1)
        align = rand.randint(0, 1)
        write_act = 0

        # Circuit
        tester.circuit.valid_in = valid_in
        tester.circuit.data_in = data_in
        tester.circuit.align = align
        # Model
        (mod_dat, mod_val) = model_ab.interact(data_in, valid_in, write_act, align)
        if mod_val:
            write_act = 1
        tester.circuit.write_act = write_act

        tester.eval()

        tester.circuit.valid_out.expect(mod_val)
        if(mod_val == 1):
            # Check the data on the output...
            for j in range(num_per_agg):
                getattr(tester.circuit,
                        f"data_out_chop_{j}").expect(mod_dat[j])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_agg_buff_basic()
