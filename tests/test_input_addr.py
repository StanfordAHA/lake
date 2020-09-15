from lake.models.input_addr_ctrl_model import InputAddrCtrlModel
from lake.modules.input_addr_ctrl import InputAddrCtrl
from lake.passes.passes import lift_config_reg
import _kratos
import magma as m
from magma import *
from lake.utils.util import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


# This module doesn't really exist as anything useful anymore...
@pytest.mark.skip
@pytest.mark.parametrize("banks", [1, 2, 4])
@pytest.mark.parametrize("interconnect_input_ports", [1, 2])
def test_input_addr_basic(banks,
                          interconnect_input_ports,
                          mem_depth=512,
                          data_width=16,
                          fetch_width=32,
                          iterator_support=4,
                          address_width=16,
                          multiwrite=1,
                          num_tiles=1):

    fw_int = int(fetch_width / data_width)

    # Set up model...
    model_iac = InputAddrCtrlModel(
        interconnect_input_ports=interconnect_input_ports,
        mem_depth=mem_depth,
        banks=banks,
        num_tiles=num_tiles,
        iterator_support=iterator_support,
        max_port_schedule=64,
        address_width=address_width,
        data_width=data_width,
        fetch_width=fetch_width)

    new_config = {}
    new_config['address_gen_0_starting_addr'] = 0
    new_config['address_gen_0_dimensionality'] = 3
    new_config['address_gen_0_strides_0'] = 1
    new_config['address_gen_0_strides_1'] = 3
    new_config['address_gen_0_strides_2'] = 9
    new_config['address_gen_0_ranges_0'] = 3
    new_config['address_gen_0_ranges_1'] = 3
    new_config['address_gen_0_ranges_2'] = 3

    new_config['address_gen_1_starting_addr'] = mem_depth
    new_config['address_gen_1_dimensionality'] = 3
    new_config['address_gen_1_strides_0'] = 1
    new_config['address_gen_1_strides_1'] = 3
    new_config['address_gen_1_strides_2'] = 9
    new_config['address_gen_1_ranges_0'] = 3
    new_config['address_gen_1_ranges_1'] = 3
    new_config['address_gen_1_ranges_2'] = 3

    model_iac.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = InputAddrCtrl(interconnect_input_ports=interconnect_input_ports,
                        mem_depth=mem_depth,
                        banks=banks,
                        num_tiles=num_tiles,
                        iterator_support=iterator_support,
                        address_width=address_width,
                        data_width=16,
                        fetch_width=fetch_width,
                        multiwrite=multiwrite,
                        strg_wr_ports=1,
                        config_width=16)

    lift_config_reg(dut.internal_generator)
    magma_dut = k.util.to_magma(dut, flatten_array=True,
                                check_multiple_driver=False,
                                check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    valid_in = []
    wen_en = []
    for i in range(interconnect_input_ports):
        valid_in.append(0)
        wen_en.append(0)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    for i in range(interconnect_input_ports):
        tester.circuit.wen_en[i] = 1
    tester.step(2)

    rand.seed(0)

    data_in = []
    # Init blank data input
    for i in range(interconnect_input_ports):
        data_in.append([0 for z in range(fw_int)])

    for i in range(1000):
        # Set valid and wen enable
        for j in range(interconnect_input_ports):
            valid_in[j] = rand.randint(0, 1)
            wen_en[j] = rand.randint(0, 1)

        # Deal with data in
        for j in range(interconnect_input_ports):
            for z in range(fw_int):
                data_in[j][z] = rand.randint(0, 2 ** data_width - 1)

        (wen, data_out, addrs, port_out) = model_iac.interact(valid_in, data_in, wen_en)

        for z in range(interconnect_input_ports):
            tester.circuit.valid_in[z] = valid_in[z]
            tester.circuit.wen_en[z] = wen_en[z]

        for z in range(interconnect_input_ports):
            for word in range(fw_int):
                setattr(tester.circuit, f"data_in_{z}_{word}", data_in[z][word])

        tester.eval()

        if(banks == 1):
            tester.circuit.addr_out_0_0.expect(addrs[0])
            tester.circuit.wen_to_sram.expect(wen[0])
        else:
            for z in range(banks):
                getattr(tester.circuit, f"addr_out_{z}_0").expect(addrs[z])
                getattr(tester.circuit, f"wen_to_sram_{z}").expect(wen[z])

        for z in range(banks):
            for word in range(fw_int):
                getattr(tester.circuit, f"data_out_{z}_0_{word}").expect(data_out[z][word])

        for j in range(interconnect_input_ports):
            tester.circuit.port_out[j].expect(port_out[j])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_input_addr_basic(banks=1,
                          interconnect_input_ports=2)
