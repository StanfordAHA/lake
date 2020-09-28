from lake.top.lake_top import LakeTop
from lake.top.pond import Pond
import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.models.lake_top_model import LakeTopModel
from lake.utils.util import transform_strides_and_ranges


ctrl_dim = 2
ctrl_ranges = [16, 1]
ctrl_strides = [1, 1]
(tform_ranges, tform_strides) = transform_strides_and_ranges(ctrl_ranges, ctrl_strides, ctrl_dim)
print(tform_ranges, tform_strides)
# Deprecated on old pond...
#@pytest.mark.skip
def test_pond(data_width=16,  # CGRA Params
              mem_depth=32,
              default_iterator_support=2,
              config_data_width=32,
              config_addr_width=8,
              cycle_count_width=16,
              add_clk_enable=True,
              add_flush=True,
              #mem_depth=32,
              #banks=1,
              #input_iterator_support=2,  # Addr Controllers
              #output_iterator_support=2,
              interconnect_input_ports=1,  # Connection to int
              interconnect_output_ports=1):
              #mem_input_ports=1,
              #mem_output_ports=1,
              #use_sram_stub=1,
              #read_delay=0,  # Cycle delay in read (SRAM vs Register File)
              #rw_same_cycle=True,  # Does the memory allow r+w in same cycle?
              #agg_height=0,
              #transpose_height=0,  # new field
              #max_agg_schedule=64,
              #input_max_port_sched=64,
              #output_max_port_sched=64,
              #align_input=0,
              #max_line_length=2048,
              #max_tb_height=1,
              #tb_range_max=2048,
              #tb_range_inner_max=5,  # new field
              #tb_sched_max=64,
              #max_tb_stride=15,
              #num_tb=0,
              #tb_iterator_support=2,
              #multiwrite=1,
              #max_prefetch=64,
              ##config_data_width=16,
              ##config_addr_width=8,
              #remove_tb=True):

    new_config = {}

    # Input addr ctrl
    new_config["rf_read_iter_0_dimensionality"] = 2
    #new_config["strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
    #new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 16
    #new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1
    #new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_1"] = 100
    #new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_1"] = 0

    new_config["rf_write_iter_0_dimensionality_0"] = 2 
    new_config["rf_read_addr_0_starting_addr"] = 2
    new_config["rf_read_addr_0_strides"] = 1
    new_config["rf_read_iter_0_dimensionality"] = 2
    new_config["rf_read_iter_0_ranges"] = 16
    new_config["rf_read_sched_0_sched_addr_gen_starting_addr"] = 0
    new_config["rf_read_sched_0_sched_addr_gen_strides"] = 1
    new_config["rf_write_addr_0_starting_addr"] = 0
    new_config["rf_write_addr_0_strides"] = 1
    new_config["rf_write_iter_0_dimensionality"] = 2
    new_config["rf_write_iter_0_ranges"] = 16
    new_config["rf_write_sched_0_sched_addr_gen_starting_addr"] = 0
    new_config["rf_write_sched_0_sched_addr_gen_strides"] = 1

    #new_config["strg_ub_input_addr_ctrl_address_gen_1_dimensionality"] = 2
    #new_config["strg_ub_input_addr_ctrl_address_gen_1_starting_addr"] = 16
    #new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_0"] = 16
    #new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_0"] = 1
    #new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_1"] = 100
    #new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_1"] = 0

    # Output addr ctrl
    #new_config["strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 2
    #new_config["strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
    #new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 16
    #new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1
    #new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_1"] = 100
    #new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_1"] = 0

    #new_config["strg_ub_output_addr_ctrl_address_gen_1_dimensionality"] = 2
    #new_config["strg_ub_output_addr_ctrl_address_gen_1_starting_addr"] = 16
    #new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_0"] = 16
    #new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_0"] = 1
    #new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_1"] = 100
    #new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_1"] = 0

    # These ports are desynched
    # new_config["strg_ub_sync_grp_sync_group_0"] = 1  # not needed
    # new_config["strg_ub_sync_grp_sync_group_1"] = 2  # not needed

    #new_config["strg_ub_app_ctrl_read_depth_0"] = 16
    #new_config["strg_ub_app_ctrl_read_depth_1"] = 16

    #new_config["strg_ub_app_ctrl_write_depth_0"] = 16
    #new_config["strg_ub_app_ctrl_write_depth_1"] = 16

    #new_config["strg_ub_app_ctrl_coarse_read_depth_0"] = 16
    #new_config["strg_ub_app_ctrl_coarse_read_depth_1"] = 0

    #new_config["strg_ub_app_ctrl_coarse_write_depth_0"] = 16
    #new_config["strg_ub_app_ctrl_coarse_write_depth_1"] = 16

    #new_config["strg_ub_app_ctrl_write_depth_ss_0"] = 16
    #new_config["strg_ub_app_ctrl_write_depth_ss_0"] = 16

    # Don't use the model - this is handwritten for now

    ### DUT
    pond_dut = Pond(data_width=data_width,  # CGRA Params
                    mem_depth=mem_depth,
                    default_iterator_support=default_iterator_support,
                    interconnect_input_ports=interconnect_input_ports,  # Connection to int
                    interconnect_output_ports=interconnect_output_ports,
                    config_data_width=config_data_width,
                    config_addr_width=config_addr_width,
                    cycle_count_width=cycle_count_width,
                    add_clk_enable=add_clk_enable,
                    add_flush=add_flush)

    magma_dut = kts.util.to_magma(pond_dut,
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
    tester.circuit.clk_en = 1
    tester.circuit.tile_en = 1
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1

    data_in = [0] * interconnect_input_ports
    valid_in = [0] * interconnect_input_ports
    wen_en = 0
    ren_en = 0
    addr_in = 0
    ren = 0
    # data_in[1] = 15
    for i in range(32):
        # Incrementing Data
        data_in[0] = data_in[0] + 1
        # data_in[1] = data_in[1] + 1

        wen_en = 3
        wen = 3

        ren_en = 0
        ren = 0

        if i >= 16:
            wen_en = 0
            wen = 0
            ren_en = 3
            ren = 3

        # tester.circuit.addr_in = addr_in
        tester.circuit.wen_en = wen_en
        tester.circuit.wen_in = wen
        tester.circuit.ren_en = ren_en
        tester.circuit.ren_in = ren

        if interconnect_input_ports == 1:
            tester.circuit.data_in = data_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
        # print("data0 and data1", data_in[0], data_in[1])
        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_pond()
