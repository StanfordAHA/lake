from lake.models.output_addr_ctrl_model import OutputAddrCtrlModel
from lake.modules.output_addr_ctrl import OutputAddrCtrl
from lake.passes.passes import lift_config_reg
import _kratos
import magma as m
from magma import *
import fault
import tempfile
import kratos as kts
import random as rand
import pytest


# This module doesn't really exist as anything useful anymore...
@pytest.mark.skip
@pytest.mark.parametrize("banks", [1, 2, 4])
@pytest.mark.parametrize("interconnect_output_ports", [1, 2])
@pytest.mark.parametrize("enable_chain_output", [0, 1])
def test_output_addr_basic(banks,
                           interconnect_output_ports,
                           enable_chain_output,
                           mem_depth=512,
                           num_tiles=1,
                           data_width=16,
                           fetch_width=32,
                           iterator_support=4,
                           address_width=16,
                           config_width=16,
                           chain_idx_output=0):

    fw_int = int(fetch_width / data_width)

    # Set up model..
    model_oac = OutputAddrCtrlModel(interconnect_output_ports=interconnect_output_ports,
                                    mem_depth=mem_depth,
                                    banks=banks,
                                    num_tiles=num_tiles,
                                    iterator_support=iterator_support,
                                    address_width=address_width,
                                    data_width=data_width,
                                    fetch_width=fetch_width,
                                    chain_idx_output=chain_idx_output)

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

    model_oac.set_config(new_config=new_config)
    ###

    # Set up dut...
    dut = OutputAddrCtrl(interconnect_output_ports=interconnect_output_ports,
                         mem_depth=mem_depth,
                         num_tiles=num_tiles,
                         banks=banks,
                         iterator_support=iterator_support,
                         address_width=address_width,
                         config_width=config_width)

    lift_config_reg(dut.internal_generator)

    magma_dut = kts.util.to_magma(dut, flatten_array=True,
                                  check_multiple_driver=False,
                                  check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    valid_in = []
    for i in range(interconnect_output_ports):
        valid_in.append(0)

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    # Seed for posterity
    rand.seed(0)

    for i in range(1000):
        for j in range(interconnect_output_ports):
            valid_in[j] = rand.randint(0, 1)
        step_in = rand.randint(0, 2 ** interconnect_output_ports - 1)

        for z in range(interconnect_output_ports):
            tester.circuit.valid_in[z] = valid_in[z]
        tester.circuit.step_in = step_in

        # top level config regs passed down
        tester.circuit.enable_chain_output = enable_chain_output
        tester.circuit.chain_idx_output = chain_idx_output

        (ren, addrs) = model_oac.interact(valid_in, step_in, enable_chain_output)

        tester.eval()

        if(banks == 1):
            for k in range(interconnect_output_ports):
                tester.circuit.ren[k].expect(ren[0][k])
        else:
            for j in range(banks):
                for k in range(interconnect_output_ports):
                    getattr(tester.circuit, f"ren_{j}")[k].expect(ren[j][k])

        if(interconnect_output_ports == 1):
            tester.circuit.addr_out.expect(addrs[0])
        else:
            for j in range(interconnect_output_ports):
                getattr(tester.circuit, f"addr_out_{z}").expect(addrs[z])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_output_addr_basic(banks=2,
                           interconnect_output_ports=3,
                           enable_chain_output=0,
                           mem_depth=512,
                           num_tiles=1,
                           data_width=16,
                           fetch_width=32,
                           iterator_support=4,
                           address_width=16,
                           config_width=16,
                           chain_idx_output=0)
