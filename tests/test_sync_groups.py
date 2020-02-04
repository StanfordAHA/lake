from lake.models.sync_groups_model import SyncGroupsModel
from lake.modules.sync_groups import SyncGroups
from lake.passes.passes import lift_config_reg
import _kratos
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.parametrize("int_out_ports", [1, 2, 3])
def test_sync_groups(int_out_ports,
                     fetch_width=32):

    # Set up model..
    model_sg = SyncGroupsModel(fetch_width=fetch_width,
                               int_out_ports=int_out_ports)

    rand.seed(0)
    group_choice = rand.randint(0, 1)

    new_config = {}
    if int_out_ports == 1:
        new_config['sync_group'] = 1
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
                     int_out_ports=int_out_ports)

    lift_config_reg(dut.internal_generator)
    magma_dut = k.util.to_magma(dut, flatten_array=True, check_multiple_driver=False)
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

    for i in range(1000):
        # Gen random data
        ack_in = 0
        ren_in = 0
        valid_in = []
        data_in = []
        for j in range(int_out_ports):
            valid_in.append(rand.randint(0, 1))
            data_in.append(rand.randint(0, 2 ** fetch_width - 1))

        # Apply stimulus to dut
        tester.circuit.ack_in = ack_in
        tester.circuit.ren_in = ren_in
        for j in range(int_out_ports):
            tester.circuit.valid_in[j] = valid_in[j]
            # tester.circuit.data_in[j] = data_in[j]
        if(int_out_ports == 1):
            tester.circuit.data_in = data_in[0]
        else:
            for j in range(int_out_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])

        # Interact w/ model
        (model_do, model_vo, model_rd_sync) = model_sg.interact(ack_in,
                                                                data_in,
                                                                valid_in,
                                                                ren_in)

        tester.eval()

        # Check outputs
        if int_out_ports == 1:
            tester.circuit.data_out.expect(model_do[0])
        else:
            for j in range(int_out_ports):
                getattr(tester.circuit, f"data_out_{j}").expect(model_do[j])

        for j in range(int_out_ports):
            tester.circuit.valid_out[j].expect(model_vo[j])
            # tester.circuit.rd_sync_cate.expect(model_rd_sync)

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "sync_group_dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])

# test_sync_groups()
