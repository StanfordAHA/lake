from lake.models.input_addr_ctrl_model import InputAddrCtrlModel
from lake.modules.input_addr_ctrl import InputAddrCtrl
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest

@pytest.mark.parametrize("banks", [4, 6])
def test_input_addr_basic(
                            banks,
                            interconnect_input_ports=2,
                            mem_depth=512,
                            iterator_support=6,
                            address_width=16
                            ):

    ######################### Set up model...

    model_iac = InputAddrCtrlModel(interconnect_input_ports=interconnect_input_ports,
                                    mem_depth=mem_depth,
                                    banks=banks,
                                    iterator_support=iterator_support,
                                    max_port_schedule=64,
                                    address_width=address_width)
    new_config = {}
    new_config['starting_addr_p_0'] = 0
    new_config['dimensionality_0'] = 3
    new_config['stride_p_0_0'] = 1
    new_config['stride_p_0_1'] = 3
    new_config['stride_p_0_2'] = 9
    new_config['range_p_0_0'] = 3
    new_config['range_p_0_1'] = 3
    new_config['range_p_0_2'] = 3

    new_config['starting_addr_p_1'] = mem_depth
    new_config['dimensionality_1'] = 3
    new_config['stride_p_1_0'] = 1
    new_config['stride_p_1_1'] = 3
    new_config['stride_p_1_2'] = 9
    new_config['range_p_1_0'] = 3
    new_config['range_p_1_1'] = 3
    new_config['range_p_1_2'] = 3


    model_iac.set_config(new_config=new_config)
    ###

    #exit()

    ######################### Set up dut...
    dut = InputAddrCtrl(    interconnect_input_ports=interconnect_input_ports,
                            mem_depth=mem_depth,
                            banks=banks,
                            iterator_support=iterator_support,
                            max_port_schedule=64,
                            address_width=address_width
                            )

    magma_dut = k.util.to_magma(dut, flatten_array=True)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    #num_per_agg = int(mem_width / data_width)

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)


    valid_in = []
    for i in range(interconnect_input_ports):
        valid_in.append(0)
   # tester.circuit

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    # Seed for posterity
    rand.seed(0)

    ####
    data_in = 0

    for i in range(2000):
        for j in range(interconnect_input_ports):
            valid_in[j] = rand.randint(0,1)
        wen = model_iac.get_wen(valid_in)

        for z in range(interconnect_input_ports):
            tester.circuit.valid_in[z] = valid_in[z]
        #tester.circuit.valid_in[0] = valid_in[0]
        #tester.circuit.valid_in[1] = valid_in[1]
        #tester.circuit.wen_to_sram[0].expect(wen[0])
        #tester.circuit.wen_to_sram[0].expect(wen[0])

        addrs = model_iac.get_addrs()

        tester.eval()
        tester.step(2)

        for z in range(banks):
            tester.circuit.wen_to_sram[z].expect(wen[z])


    with tempfile.TemporaryDirectory() as tempdir:
        #tempdir="input_addr_dump"
        tester.compile_and_run(target="verilator",
                            directory=tempdir,
                            magma_output="verilog",
                            flags=["-Wno-fatal"])
