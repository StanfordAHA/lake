from lake.models.demux_reads_model import DemuxReadsModel
from lake.modules.demux_reads import DemuxReads
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand


def test_demux_reads_basic(fetch_width=32,
                           banks=2,
                           int_out_ports=2):

    # Set up model...
    model_dr = DemuxReadsModel(fetch_width=fetch_width,
                               banks=banks,
                               int_out_ports=int_out_ports)

    new_config = {}
    model_dr.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = DemuxReads(fetch_width=fetch_width,
                     banks=banks,
                     int_out_ports=int_out_ports)

    magma_dut = k.util.to_magma(dut, flatten_array=True)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    rand.seed(0)

    data_in = [0] * banks
    valid_in = [0] * banks
    port_in = [0] * banks
    port_in_hw = [0] * banks

    for z in range(1000):
        # Generate new input
        for i in range(banks):
            valid_in[i] = rand.randint(0, 1)
            port_in[i] = rand.randint(0, int_out_ports - 1)
            # One-Hot encoding in hardware
            port_in_hw[i] = 1 << port_in[i]

        (model_dat, model_val) = model_dr.input_data(data_in, valid_in, port_in)
        for i in range(banks):
            tester.circuit.valid_in[i] = valid_in[i]
            setattr(tester.circuit, f"data_in_{i}", data_in[i])
            setattr(tester.circuit, f"port_in_{i}", port_in_hw[i])
        tester.eval()

        for i in range(int_out_ports):
            getattr(tester.circuit, f"data_out_{i}").expect(model_dat[i])
            tester.circuit.valid_out[i].expect(model_val[i])

        tester.step(2)

        for i in range(banks):
            data_in[i] += 1

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
