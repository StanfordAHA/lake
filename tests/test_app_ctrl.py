from lake.models.app_ctrl_model import AppCtrlModel
from lake.modules.app_ctrl import AppCtrl
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
@pytest.mark.parametrize("sprt_stcl_valid", [True, False])
def test_app_ctrl(sprt_stcl_valid,
                  int_in_ports=1,
                  int_out_ports=3,
                  depth_width=16,
                  stcl_cnt_width=16,
                  stcl_iter_support=4):

    # Set up model..
    model_ac = AppCtrlModel(int_in_ports=int_in_ports,
                            int_out_ports=int_out_ports,
                            sprt_stcl_valid=sprt_stcl_valid,
                            stcl_iter_support=stcl_iter_support)

    new_config = {}
    new_config['input_port_0'] = 0
    new_config['input_port_1'] = 0
    new_config['input_port_2'] = 0
    new_config['read_depth_0'] = 196
    new_config['read_depth_1'] = 196
    new_config['read_depth_2'] = 196
    new_config['write_depth_0'] = 196

    for i in range(stcl_iter_support):
        new_config[f'ranges_{i}'] = 4
        new_config[f'threshold_{i}'] = 4

    rand.seed(0)

    prefill = []
    for i in range(int_out_ports):
        prefill_num = rand.randint(0, 1)
        new_config[f'prefill_{i}'] = prefill_num
        prefill.append(prefill_num)

    model_ac.set_config(new_config=new_config)

    # Set up dut...
    dut = AppCtrl(interconnect_input_ports=int_in_ports,
                  interconnect_output_ports=int_out_ports,
                  depth_width=depth_width,
                  sprt_stcl_valid=sprt_stcl_valid,
                  stcl_cnt_width=stcl_cnt_width,
                  stcl_iter_support=stcl_iter_support)

    lift_config_reg(dut.internal_generator)

    magma_dut = kts.util.to_magma(dut, flatten_array=True,
                                  check_multiple_driver=False,
                                  check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###
    tester.zero_inputs()

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    tester.circuit.write_depth = 196

    # initial reset
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    # Seed for posterity

    wen_in = [0] * int_in_ports
    ren_in = [0] * int_out_ports
    tb_valid = [0] * int_out_ports
    ren_update = [0] * int_out_ports

    for i in range(300):
        # Gen random data
        for j in range(int_in_ports):
            wen_in[j] = rand.randint(0, 1)
        ren_in_tmp = rand.randint(0, 1)
        for j in range(int_out_ports):
            tb_valid[j] = rand.randint(0, 1)
            ren_in[j] = ren_in_tmp
            ren_update[j] = rand.randint(0, 1)

        # Apply stimulus to dut
        for j in range(int_in_ports):
            tester.circuit.wen_in[j] = wen_in[j]

        for j in range(int_out_ports):
            tester.circuit.ren_in[j] = ren_in[j]
            tester.circuit.tb_valid[j] = tb_valid[j]
            tester.circuit.ren_update[j] = ren_update[j]
            tester.circuit.prefill[j] = prefill[j]

        # Interact w/ model
        (wen_out,
         ren_out,
         valid_out_data,
         valid_out_stencil) = model_ac.interact(wen_in=wen_in,
                                                ren_in=ren_in,
                                                tb_valid=tb_valid,
                                                ren_update=ren_update)

        tester.eval()

        for j in range(int_in_ports):
            tester.circuit.wen_out[j].expect(wen_out[j])

        for j in range(int_out_ports):
            # tester.circuit.ren_out[j].expect(ren_out[j])
            tester.circuit.valid_out_data[j].expect(valid_out_data[j])
            tester.circuit.valid_out_stencil[j].expect(valid_out_stencil[j])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_app_ctrl(sprt_stcl_valid=True,
                  int_in_ports=1,
                  int_out_ports=3,
                  depth_width=16,
                  stcl_cnt_width=16,
                  stcl_iter_support=4)
