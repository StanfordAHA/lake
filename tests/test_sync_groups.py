from lake.models.sync_groups_model import SyncGroupsModel
from lake.modules.sync_groups import SyncGroups
from lake.passes.passes import lift_config_reg
import _kratos
import magma as m
from magma import *
import fault
import tempfile
import kratos as kts
import random as rand
import pytest


@pytest.mark.skip
@pytest.mark.parametrize("int_out_ports", [1, 2, 3])
def test_sync_groups(int_out_ports,
                     fetch_width=32,
                     data_width=16):

    fw_int = int(fetch_width / data_width)

    # Set up model..
    model_sg = SyncGroupsModel(fetch_width=fetch_width,
                               data_width=data_width,
                               int_out_ports=int_out_ports)

    rand.seed(0)
    group_choice = rand.randint(0, 1)

    new_config = {}
    if int_out_ports == 1:
        new_config['sync_group'] = 1
        new_config['sync_group_0'] = 1
    elif group_choice == 1:
        new_config['sync_group_0'] = 1
        new_config['sync_group_1'] = 1
        new_config['sync_group_2'] = 1
    else:
        new_config['sync_group_0'] = 1
        new_config['sync_group_1'] = 1
        new_config['sync_group_2'] = 2

    model_sg.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = SyncGroups(fetch_width=fetch_width,
                     data_width=data_width,
                     int_out_ports=int_out_ports)

    lift_config_reg(dut.internal_generator)
    magma_dut = kts.util.to_magma(dut, flatten_array=True,
                                  check_multiple_driver=False,
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
    # Seed for posterity

    data_in = []
    for i in range(int_out_ports):
        row = []
        for j in range(fw_int):
            row.append(0)
        data_in.append(row)

    for i in range(1000):
        # Gen random data
        ack_in = rand.randint(0, 2 ** int_out_ports - 1)
        ren_in = []
        valid_in = []
        mem_valid_data = []
        for j in range(int_out_ports):
            ren_in.append(rand.randint(0, 1))
            valid_in.append(rand.randint(0, 1))
            mem_valid_data.append(rand.randint(0, 1))
            for k in range(fw_int):
                data_in[j][k] = rand.randint(0, 2 ** data_width - 1)

        # Apply stimulus to dut
        tester.circuit.ack_in = ack_in
        for j in range(int_out_ports):
            tester.circuit.ren_in[j] = ren_in[j]
            tester.circuit.valid_in[j] = valid_in[j]
            tester.circuit.mem_valid_data[j] = mem_valid_data[j]

        for j in range(int_out_ports):
            for k in range(fw_int):
                setattr(tester.circuit, f"data_in_{j}_{k}", data_in[j][k])

        # Interact w/ model
        (model_do, model_vo, model_rd_sync, model_mem_valid) = \
            model_sg.interact(ack_in, data_in, valid_in, ren_in, mem_valid_data)

        tester.eval()

        for j in range(int_out_ports):
            for k in range(fw_int):
                getattr(tester.circuit, f"data_out_{j}_{k}").expect(model_do[j][k])

        for j in range(int_out_ports):
            tester.circuit.valid_out[j].expect(model_vo[j])
            tester.circuit.rd_sync_gate[j].expect(model_rd_sync[j])
            tester.circuit.mem_valid_data_out[j].expect(model_mem_valid[j])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_sync_groups(int_out_ports=2,
                     fetch_width=32,
                     data_width=16)
