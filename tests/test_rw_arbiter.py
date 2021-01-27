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


@pytest.mark.skip
@pytest.mark.parametrize("int_out_ports", [1, 2, 4])
@pytest.mark.parametrize("fetch_width", [16, 32])
@pytest.mark.parametrize("read_delay", [0, 1])
def test_rw_arbiter_basic(int_out_ports,
                          fetch_width,
                          read_delay,
                          data_width=16,
                          memory_depth=256,
                          int_in_ports=1,
                          strg_wr_ports=1,
                          strg_rd_ports=1,
                          rw_same_cycle=False,
                          separate_addresses=False):

    fw_int = int(fetch_width / data_width)

    # Set up model..
    model_rwa = RWArbiterModel(fetch_width=fetch_width,
                               data_width=data_width,
                               memory_depth=memory_depth,
                               int_out_ports=int_out_ports,
                               read_delay=read_delay)

    new_config = {}
    model_rwa.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = RWArbiter(fetch_width=fetch_width,
                    data_width=data_width,
                    memory_depth=memory_depth,
                    int_in_ports=int_in_ports,
                    int_out_ports=int_out_ports,
                    strg_wr_ports=strg_wr_ports,
                    strg_rd_ports=strg_rd_ports,
                    read_delay=read_delay,
                    rw_same_cycle=rw_same_cycle,
                    separate_addresses=separate_addresses)

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

    ren_in = [0 for i in range(int_out_ports)]
    ren_en = [0 for i in range(int_out_ports)]

    w_data = [0 for i in range(fw_int)]
    data_from_mem = [0 for i in range(fw_int)]

    for i in range(100):
        # Gen random data
        wen_in = rand.randint(0, 1)
        wen_en = 1

        for j in range(fw_int):
            w_data[j] = rand.randint(0, 2 ** data_width - 1)
            data_from_mem[j] = rand.randint(0, 2 ** data_width - 1)
        w_addr = rand.randint(0, 2 ** 9 - 1)

        ren_en_base = rand.randint(0, 1)
        for j in range(int_out_ports):
            ren_in[j] = rand.randint(0, 1)
            ren_en[j] = ren_en_base

        rd_addr = []
        for j in range(int_out_ports):
            rd_addr.append(rand.randint(0, 2 ** 9 - 1))

        mem_valid_data = []
        for j in range(strg_rd_ports):
            mem_valid_data.append(rand.randint(0, 1))

        # Apply stimulus to dut
        tester.circuit.wen_in = wen_in
        tester.circuit.wen_en = wen_en
        tester.circuit.w_addr = w_addr

        if fw_int == 1:
            tester.circuit.w_data_0_0 = w_data[0]
            tester.circuit.data_from_mem_0_0 = data_from_mem[0]
        else:
            for j in range(fw_int):
                setattr(tester.circuit, f"w_data_0_{j}", w_data[j])
                setattr(tester.circuit, f"data_from_mem_0_{j}", data_from_mem[j])

        for j in range(int_out_ports):
            tester.circuit.ren_in[j] = ren_in[j]
            tester.circuit.ren_en[j] = ren_en[j]

        if (int_out_ports == 1):
            tester.circuit.rd_addr = rd_addr[0]
        else:
            for j in range(int_out_ports):
                setattr(tester.circuit, f"rd_addr_{j}", rd_addr[j])

        if (strg_rd_ports == 1):
            tester.circuit.mem_valid_data = mem_valid_data[0]
        else:
            for j in range(strg_rd_ports):
                setattr(tester.circuit, f"mem_valid_data_{j}", mem_valid_data[j])

        # Interact w/ model
        (model_od, model_op, model_ov, model_cen_mem,
         model_wen_mem, model_mem_data, model_mem_addr,
         model_ack, model_out_mem_valid_data) = model_rwa.interact(wen_in,
                                                                   wen_en,
                                                                   w_data,
                                                                   w_addr,
                                                                   data_from_mem,
                                                                   ren_in,
                                                                   ren_en,
                                                                   rd_addr,
                                                                   mem_valid_data)

        tester.eval()

        # Check outputs
        tester.circuit.out_valid.expect(model_ov)
        if(model_ov):
            tester.circuit.out_port.expect(model_op)
            if fw_int == 1:
                tester.circuit.out_data_0_0.expect(model_od[0])
            else:
                for j in range(fw_int):
                    getattr(tester.circuit, f"out_data_0_{j}").expect(model_od[j])

        tester.circuit.cen_mem.expect(model_cen_mem)
        tester.circuit.wen_mem.expect(model_wen_mem)

        if fw_int == 1:
            tester.circuit.data_to_mem_0_0.expect(model_mem_data[0])
        else:
            for j in range(fw_int):
                getattr(tester.circuit, f"data_to_mem_0_{j}").expect(model_mem_data[j])

        tester.circuit.addr_to_mem.expect(model_mem_addr)
        tester.circuit.out_ack.expect(model_ack)

        print(mem_valid_data)
        print(model_out_mem_valid_data)
        if strg_rd_ports == 1:
            tester.circuit.out_mem_valid_data.expect(model_out_mem_valid_data[0])
        else:
            for j in range(strg_rd_ports):
                tester.circuit.out_mem_valid_data[j].expect(model_out_mem_valid_data[j])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_rw_arbiter_basic(int_out_ports=1,
                          fetch_width=16,
                          read_delay=0)
