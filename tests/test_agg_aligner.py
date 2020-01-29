from lake.models.agg_aligner_model import AggAlignerModel
from lake.modules.agg_aligner import AggAligner
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand


def test_agg_aligner_basic(data_width=16,
                           max_line_length=2048):

    # Set up model...
    model_al = AggAlignerModel(data_width=data_width, max_line_length=max_line_length)
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

    magma_dut = k.util.to_magma(dut, flatten_array=True)
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

    for i in range(2000):
        v_out_model = model_ab.get_valid_out()
        tester.circuit.valid_out.expect(v_out_model)
        tester.circuit.write_act = 0

        if(v_out_model == 1):
            # Check the data on the output...
            mod_dat_out = model_ab.get_item()
            for j in range(num_per_agg):
                getattr(tester.circuit,
                        f"data_out_chop_{j}").expect(mod_dat_out[j])

            tester.circuit.write_act = 1

        valid_next = rand.randint(0, 1)
        if(valid_next == 1):
            data_in = rand.randint(0, 2**30)
            # Circuit
            tester.circuit.valid_in = 1
            tester.circuit.data_in = data_in
            # Model
            model_ab.insert(data_in, 1)

        else:
            tester.circuit.valid_in = 0
            model_ab.insert(0, 0)

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
