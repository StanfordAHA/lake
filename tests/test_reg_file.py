from lake.models.register_file_model import RegisterFileModel
from lake.modules.register_file import RegisterFile
import magma as m
from magma import *
import fault
import tempfile
import kratos as kts
import random as rand
import pytest


@pytest.mark.parametrize("data_width", [16, 32])
@pytest.mark.parametrize("depth", [512, 1024])
@pytest.mark.parametrize("width_mult", [1, 2])
@pytest.mark.parametrize("write_ports", [1, 2])
@pytest.mark.parametrize("read_ports", [1, 2])
def test_reg_file_basic(data_width,
                        depth,
                        width_mult,
                        write_ports,
                        read_ports):

    addr_width = kts.clog2(depth)

    # Set up model...
    model_rf = RegisterFileModel(data_width=data_width,
                                 write_ports=write_ports,
                                 read_ports=read_ports,
                                 width_mult=width_mult,
                                 depth=depth)
    new_config = {}
    model_rf.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = RegisterFile(data_width=data_width,
                       write_ports=write_ports,
                       read_ports=read_ports,
                       width_mult=width_mult,
                       depth=depth)

    magma_dut = kts.util.to_magma(dut, flatten_array=True,
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

    for z in range(1000):
        # Generate new input
        wen = []
        wr_addr = []
        wr_data = []
        for i in range(write_ports):
            wen.append(rand.randint(0, 1))
            wr_addr.append(rand.randint(0, depth - 1))
            new_dat = []
            for j in range(width_mult):
                new_dat.append(rand.randint(0, 2 ** data_width - 1))
            wr_data.append(new_dat)

        rd_addr = []
        for i in range(read_ports):
            rd_addr.append(rand.randint(0, depth - 1))

        if write_ports == 1:
            tester.circuit.wr_addr = wr_addr[0]
        else:
            for i in range(write_ports):
                setattr(tester.circuit, f"wr_addr_{i}", wr_addr[i])

        for i in range(write_ports):
            tester.circuit.wen[i] = wen[i]

        if width_mult == 1 and write_ports == 1:
            tester.circuit.data_in = wr_data[0][0]
        elif width_mult == 1:
            for i in range(write_ports):
                setattr(tester.circuit, f"data_in_{i}_0", wr_data[i][0])
        elif write_ports == 1:
            for i in range(width_mult):
                setattr(tester.circuit, f"data_in_{i}", wr_data[0][i])
        else:
            for i in range(write_ports):
                for j in range(width_mult):
                    setattr(tester.circuit, f"data_in_{i}_{j}", wr_data[i][j])

        if read_ports == 1:
            tester.circuit.rd_addr = rd_addr[0]
        else:
            for i in range(read_ports):
                setattr(tester.circuit, f"rd_addr_{i}", rd_addr[i])

        model_dat_out = model_rf.interact(wen, wr_addr, rd_addr, wr_data)

        tester.eval()

        if width_mult == 1 and read_ports == 1:
            tester.circuit.data_out.expect(model_dat_out[0][0])
        elif width_mult == 1:
            for i in range(read_ports):
                getattr(tester.circuit, f"data_out_{i}_0").expect(model_dat_out[i][0])
        elif read_ports == 1:
            for i in range(width_mult):
                getattr(tester.circuit, f"data_out_{i}").expect(model_dat_out[0][i])
        else:
            for i in range(read_ports):
                for j in range(width_mult):
                    getattr(tester.circuit, f"data_out_{i}_{j}").expect(model_dat_out[i][j])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_reg_file_basic()
