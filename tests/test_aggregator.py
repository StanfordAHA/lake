from lake.modules.aggregator import *
from lake.models.agg_model import AggModel
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand

def test_aggregator_basic(word_width, mem_word_width):

    model_agg = AggModel(num_elts=mem_word_width)
    # No actual configuration to be set
    model_agg.set_config()

    # get verilog file that needs to be copied to agg_dump directory before running verilator
    dut = Aggregator(word_width=word_width, mem_word_width=mem_word_width)
    magma_dut = k.util.to_magma(dut, flatten_array=True)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    num_per_piece = int(mem_word_width / word_width)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    rand.seed(0)
    rand.randint(0,1)

    data_in = 0

    for i in range(200):
        v_out_model = model_agg.get_valid_out()
        tester.circuit.valid_out.expect(v_out_model)

        if(v_out_model == 1):
            # Check the data on the output...
            mod_dat_out = model_agg.get_data_out()
            for i in range(mem_word_width):
                getattr(tester.circuit, f"agg_out_{i}").expect(mod_dat_out[i])

        valid_next = rand.randint(0,1)
        #data_next = rand.randint(0, 50)
        if(valid_next == 1):
            # Circuit
            tester.circuit.valid_in = 1
            tester.circuit.in_pixels = data_in
            # Model
            model_agg.insert(data_in, 1)

            data_in = data_in + 1
        else:
            tester.circuit.valid_in = 0
            model_agg.insert(0, 0)
        
        tester.eval()
        tester.step(2)


    with tempfile.TemporaryDirectory() as tempdir:
        tempdir="agg_dump"
        tester.compile_and_run(target="verilator",
                            directory=tempdir,
                            magma_output="verilog",
                            flags=["-Wno-fatal", "--trace"])

test_aggregator_basic(word_width=16, mem_word_width=4)