from lake.models.demux_reads_model import DemuxReadsModel
from lake.modules.demux_reads import DemuxReads
import magma as m
from magma import *
import fault
import tempfile
from lake.utils.util import *
import kratos as k
import random as rand


def test_demux_reads_basic(fetch_width=32,
                           data_width=16,
                           banks=2,
                           int_out_ports=2):

    fw_int = int(fetch_width / data_width)

    # Set up model...
    model_dr = DemuxReadsModel(fetch_width=fetch_width,
                               data_width=data_width,
                               banks=banks,
                               int_out_ports=int_out_ports)

    new_config = {}
    model_dr.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = DemuxReads(fetch_width=fetch_width,
                     data_width=data_width,
                     banks=banks,
                     int_out_ports=int_out_ports)

    magma_dut = k.util.to_magma(dut, flatten_array=True,
                                check_flip_flop_always_ff=False)
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

    data_in = []
    for i in range(banks):
        row = []
        for j in range(fw_int):
            row.append(0)
        data_in.append(row)

    valid_in = [0] * banks
    mem_valid_data = [0] * banks
    port_in = [0] * banks
    port_in_hw = [0] * banks
    for z in range(1000):
        # Generate new input
        for i in range(banks):
            valid_in[i] = rand.randint(0, 1)
            mem_valid_data[i] = rand.randint(0, 1)
            port_in[i] = rand.randint(0, int_out_ports - 1)
            # One-Hot encoding in hardware
            port_in_hw[i] = 1 << port_in[i]
            for j in range(fw_int):
                data_in[i][j] = rand.randint(0, 2 ** data_width - 1)

        model_dat, model_val, model_mem_valid_data_out = \
            model_dr.interact(data_in, valid_in, port_in_hw, mem_valid_data)

        for i in range(banks):
            tester.circuit.valid_in[i] = valid_in[i]
            tester.circuit.mem_valid_data[i] = mem_valid_data[i]
            setattr(tester.circuit, f"port_in_{i}", port_in_hw[i])
            for j in range(fw_int):
                setattr(tester.circuit, f"data_in_{i}_{j}", data_in[i][j])

        tester.eval()

        for i in range(int_out_ports):
            for j in range(fw_int):
                getattr(tester.circuit, f"data_out_{i}_{j}").expect(model_dat[i][j])
            tester.circuit.valid_out[i].expect(model_val[i])
            tester.circuit.mem_valid_data_out[i].expect(model_mem_valid_data_out[i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
