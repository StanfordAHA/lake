from lake.models.input_addr_ctrl_model import InputAddrCtrlModel
from lake.modules.input_addr_ctrl import InputAddrCtrl
from lake.passes.passes import lift_config_reg
import _kratos
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


#@pytest.mark.parametrize("banks", [2, 4])
def test_input_addr_basic(banks=2,
                          interconnect_input_ports=2,
                          mem_depth=512,
                          iterator_support=6,
                          address_width=16,
                          multiwrite=1):

    # Set up model..
    model_iac = InputAddrCtrlModel(
        interconnect_input_ports=interconnect_input_ports,
        mem_depth=mem_depth,
        banks=banks,
        iterator_support=iterator_support,
        max_port_schedule=64,
        address_width=address_width)
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
                        iterator_support=iterator_support,
                        max_port_schedule=64,
                        address_width=address_width,
                        data_width=16,
                        multiwrite=multiwrite)
    lift_config_reg(dut.internal_generator)
    magma_dut = k.util.to_magma(dut, flatten_array=True, check_multiple_driver=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    valid_in = []
    for i in range(interconnect_input_ports):
        valid_in.append(0)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    # Seed for posterity
    rand.seed(0)

    data_in = 0

    for i in range(1000):
        for j in range(interconnect_input_ports):
            valid_in[j] = rand.randint(0, 1)
        wen = model_iac.get_wen(valid_in)

        for z in range(interconnect_input_ports):
            tester.circuit.valid_in[z] = valid_in[z]

        addrs = model_iac.get_addrs()

        tester.eval()
        tester.step(2)

        for z in range(banks):
            tester.circuit.wen_to_sram[z].expect(wen[z])

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])

test_input_addr_basic()