from lake.models.rw_arbiter_model import RWArbiterModel
from lake.modules.rw_arbiter import RWArbiter
from lake.passes.passes import lift_config_reg
import _kratos
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.parametrize("int_out_ports", [1, 2, 4])
def test_rw_arbiter_basic(int_out_ports,
                          memory_depth=512,
                          fetch_width=32):

    # Set up model..
    model_rwa = RWArbiterModel(fetch_width=fetch_width,
                               memory_depth=memory_depth,
                               int_out_ports=int_out_ports)

    new_config = {}
    model_rwa.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = RWArbiter(fetch_width=fetch_width,
                    memory_depth=memory_depth,
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
    tester.circuit.wen_in = 0
    tester.circuit.wen_en = 0
    tester.circuit.w_data = 0
    tester.circuit.w_addr = 0
    tester.circuit.data_from_mem = 0
    tester.circuit.ren_in = 0
    tester.circuit.ren_en = 0
    if(int_out_ports == 1):
        tester.circuit.rd_addr = 0
    else:
        for i in range(int_out_ports):
            setattr(tester.circuit, f"rd_addr_{i}", 0)
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    # Seed for posterity
    rand.seed(0)

    for i in range(1000):
        # Gen random data
        wen_in = rand.randint(0, 1)
        wen_en = rand.randint(0, 1)
        w_data = rand.randint(0, 2 ** fetch_width - 1)
        w_addr = rand.randint(0, 2 ** 9 - 1)

        data_from_mem = rand.randint(0, 2 ** fetch_width - 1)

        ren_in = rand.randint(0, 2 ** int_out_ports - 1)
        ren_en = rand.randint(0, 1)

        rd_addr = []
        for j in range(int_out_ports):
            rd_addr.append(rand.randint(0, 2 ** 9 - 1))

        # Apply stimulus to dut
        tester.circuit.wen_in = wen_in
        tester.circuit.wen_en = wen_en
        tester.circuit.w_data = w_data
        tester.circuit.w_addr = w_addr
        tester.circuit.data_from_mem = data_from_mem
        tester.circuit.ren_in = ren_in
        tester.circuit.ren_en = ren_en
        if(int_out_ports == 1):
            tester.circuit.rd_addr = rd_addr[0]
        else:
            for j in range(int_out_ports):
                setattr(tester.circuit, f"rd_addr_{j}", rd_addr[j])

        # Interact w/ model
        (model_od, model_op, model_ov, model_cen_mem,
         model_wen_mem, model_mem_data, model_mem_addr,
         model_ack) = model_rwa.interact(wen_in,
                                         wen_en,
                                         w_data,
                                         w_addr,
                                         data_from_mem,
                                         ren_in,
                                         ren_en,
                                         rd_addr)

        tester.eval()

        # Check outputs
        tester.circuit.out_valid.expect(model_ov)
        if(model_ov):
            tester.circuit.out_port.expect(model_op)
            tester.circuit.out_data.expect(model_od)
        tester.circuit.cen_mem.expect(model_cen_mem)
        tester.circuit.wen_mem.expect(model_wen_mem)
        tester.circuit.data_to_mem.expect(model_mem_data)
        tester.circuit.addr_to_mem.expect(model_mem_addr)
        tester.circuit.out_ack.expect(model_ack)

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
