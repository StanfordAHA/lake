from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.passes.passes import lift_config_reg
from lake.models.lake_top_model import LakeTopModel


def test_pond(data_width=16,  # CGRA Params
                 mem_width=16,
                 mem_depth=32,
                 banks=1,
                 input_iterator_support=2,  # Addr Controllers
                 output_iterator_support=2,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 mem_input_ports=2,
                 mem_output_ports=2,
                 use_sram_stub=1,
                 read_delay=0,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=True,  # Does the memory allow r+w in same cycle?
                 agg_height=0,
                 max_agg_schedule=64,
                 input_max_port_sched=64,
                 output_max_port_sched=64,
                 align_input=0,
                 max_line_length=2048,
                 max_tb_height=1,
                 tb_range_max=2048,
                 tb_sched_max=64,
                 max_tb_stride=15,
                 num_tb=0,
                 tb_iterator_support=2,
                 multiwrite=1,
                 max_prefetch=8,
                 config_data_width=16,
                 config_addr_width=8):

    new_config = {}

    # Input addr ctrl
    new_config["input_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["input_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["input_addr_ctrl_address_gen_0_ranges_0"] = 16
    new_config["input_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["input_addr_ctrl_address_gen_0_ranges_1"] = 100
    new_config["input_addr_ctrl_address_gen_0_strides_1"] = 0

    new_config["input_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["input_addr_ctrl_address_gen_1_starting_addr"] = 16
    new_config["input_addr_ctrl_address_gen_1_ranges_0"] = 16
    new_config["input_addr_ctrl_address_gen_1_strides_0"] = 0
    new_config["input_addr_ctrl_address_gen_1_ranges_1"] = 100
    new_config["input_addr_ctrl_address_gen_1_strides_1"] = 0

    # Output addr ctrl
    new_config["output_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["output_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["output_addr_ctrl_address_gen_0_ranges_0"] = 16
    new_config["output_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["output_addr_ctrl_address_gen_0_ranges_1"] = 100
    new_config["output_addr_ctrl_address_gen_0_strides_1"] = 0

    new_config["output_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["output_addr_ctrl_address_gen_1_starting_addr"] = 16
    new_config["output_addr_ctrl_address_gen_1_ranges_0"] = 16
    new_config["output_addr_ctrl_address_gen_1_strides_0"] = 0
    new_config["output_addr_ctrl_address_gen_1_ranges_1"] = 100
    new_config["output_addr_ctrl_address_gen_1_strides_1"] = 0

    # These ports are desynched
    new_config["sync_grp_sync_group_0"] = 1
    new_config["sync_grp_sync_group_1"] = 2

    # model_lt = LakeTopModel(data_width=data_width,
    #                         mem_width=mem_width,
    #                         mem_depth=mem_depth,
    #                         banks=banks,
    #                         input_iterator_support=input_iterator_support,
    #                         output_iterator_support=output_iterator_support,
    #                         interconnect_input_ports=interconnect_input_ports,
    #                         interconnect_output_ports=interconnect_output_ports,
    #                         mem_input_ports=mem_input_ports,
    #                         mem_output_ports=mem_output_ports,
    #                         use_sram_stub=use_sram_stub,
    #                         agg_height=agg_height,
    #                         max_agg_schedule=max_agg_schedule,
    #                         input_max_port_sched=input_max_port_sched,
    #                         output_max_port_sched=output_max_port_sched,
    #                         align_input=align_input,
    #                         max_line_length=max_line_length,
    #                         tb_height=max_tb_height,
    #                         tb_range_max=tb_range_max,
    #                         tb_sched_max=tb_sched_max,
    #                         num_tb=num_tb,
    #                         multiwrite=multiwrite,
    #                         max_prefetch=max_prefetch)

    # model_lt.set_config(new_config=new_config)

    ### DUT
    lt_dut = LakeTop()

    # Run the config reg lift
    lift_config_reg(lt_dut.internal_generator)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    ###
    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    data_in = [0] * interconnect_input_ports
    valid_in = [0] * interconnect_input_ports
    wen_en = 0
    ren_en = 0
    addr_in = 0
    ren = 0

    for i in range(32):
        # Rand data
        # for j in range(interconnect_input_ports):
        #     data_in[j] += 1  # rand.randint(0, 2 ** data_width - 1)
        #     valid_in[j] = 1  # rand.randint(0, 1)
        if i < 12:
            data_in[0] = data_in[0] + 1
            valid_in[0] = 1
            wen_en = 1
            ren_en = 0
            ren = 0
        # elif i < 15:
        #     data_in[0] = data_in[0] + 1
        #     valid_in[0] = 1
        #     # data_in[1] = tester.circuit.data_out_0 + tester.circuit.data_out_1
        #     # valid_in[1] = tester.circuit.valid_out[0]
        #     wen_en = 1
        #     ren_en = 3
        #     ren = 0
        elif i < 13:
            data_in[0] = data_in[0] + 1
            valid_in[0] = 1
            wen_en = 3
            ren_en = 3
            ren = 3
        else:
            data_in[0] = data_in[0] + 1
            valid_in[0] = 1
            data_in[1] = tester.circuit.data_out_0 + tester.circuit.data_out_1
            valid_in[1] = tester.circuit.valid_out[0]
            wen_en = 3
            ren_en = 3
            ren = 3


        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen[j] = valid_in[j]
        tester.circuit.addr_in = addr_in
        tester.circuit.wen_en = wen_en
        tester.circuit.ren_en = ren_en
        tester.circuit.ren = ren

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "pond_dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    test_pond()
