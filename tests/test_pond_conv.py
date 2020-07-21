from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.models.lake_top_model import LakeTopModel


def test_pond(data_width=16,  # CGRA Params
              mem_width=16,
              mem_depth=32,
              banks=1,
              input_iterator_support=2,  # Addr Controllers
              output_iterator_support=2,
              interconnect_input_ports=1,  # Connection to int
              interconnect_output_ports=1,
              mem_input_ports=1,
              mem_output_ports=1,
              use_sram_stub=1,
              read_delay=0,  # Cycle delay in read (SRAM vs Register File)
              rw_same_cycle=True,  # Does the memory allow r+w in same cycle?
              agg_height=0,
              transpose_height=0, # new field 
              max_agg_schedule=64,
              input_max_port_sched=64,
              output_max_port_sched=64,
              align_input=0,
              max_line_length=2048,
              max_tb_height=1,
              tb_range_max=2048,
              tb_range_inner_max=5, # new field
              tb_sched_max=64,
              max_tb_stride=15,
              num_tb=0,
              tb_iterator_support=2,
              multiwrite=1,
              max_prefetch=64,
              config_data_width=16,
              config_addr_width=8,
              remove_tb=True):

    new_config = {}

    # Input addr ctrl
    new_config["strg_ub_input_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 16
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_1"] = 100
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_1"] = 0

    new_config["strg_ub_input_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["strg_ub_input_addr_ctrl_address_gen_1_starting_addr"] = 16
    new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_0"] = 16
    new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_0"] = 1 
    new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_1"] = 100
    new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_1"] = 0

    # Output addr ctrl
    new_config["strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_1"] = 100
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_1"] = 0

    new_config["strg_ub_output_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_1_starting_addr"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_0"] = 1 
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_1"] = 100
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_1"] = 0

    # These ports are desynched
    # new_config["strg_ub_sync_grp_sync_group_0"] = 1  # not needed
    # new_config["strg_ub_sync_grp_sync_group_1"] = 2  # not needed 
    
    new_config["strg_ub_app_ctrl_read_depth_0"]  = 16
    new_config["strg_ub_app_ctrl_read_depth_1"]  = 16 

    new_config["strg_ub_app_ctrl_write_depth_0"] = 16
    new_config["strg_ub_app_ctrl_write_depth_1"] = 16

    new_config["strg_ub_app_ctrl_coarse_read_depth_0"]  = 16
    new_config["strg_ub_app_ctrl_coarse_read_depth_1"]  = 0 

    new_config["strg_ub_app_ctrl_coarse_write_depth_0"] = 16
    new_config["strg_ub_app_ctrl_coarse_write_depth_1"] = 16

    new_config["strg_ub_app_ctrl_write_depth_ss_0"] = 16
    new_config["strg_ub_app_ctrl_write_depth_ss_0"] = 16 

    # Don't use the model - this is handwritten for now

    ### DUT
    lt_dut = LakeTop(data_width=data_width,  # CGRA Params
                     mem_width=mem_width,
                     mem_depth=mem_depth,
                     banks=banks,
                     input_iterator_support=input_iterator_support,  # Addr Controllers
                     output_iterator_support=output_iterator_support,
                     interconnect_input_ports=interconnect_input_ports,  # Connection to int
                     interconnect_output_ports=interconnect_output_ports,
                     mem_input_ports=mem_input_ports,
                     mem_output_ports=mem_output_ports,
                     use_sram_stub=use_sram_stub,
                     read_delay=read_delay,  # Cycle delay in read (SRAM vs Register File)
                     rw_same_cycle=rw_same_cycle,  # Does the memory allow r+w in same cycle?
                     agg_height=agg_height,
                     max_agg_schedule=max_agg_schedule,
                     input_max_port_sched=input_max_port_sched,
                     output_max_port_sched=output_max_port_sched,
                     align_input=align_input,
                     max_line_length=max_line_length,
                     max_tb_height=max_tb_height,
                     tb_range_max=tb_range_max,
                     tb_range_inner_max=tb_range_inner_max, 
                     tb_sched_max=tb_sched_max,
                     max_tb_stride=max_tb_stride,
                     num_tb=num_tb,
                     tb_iterator_support=tb_iterator_support,
                     multiwrite=multiwrite,
                     max_prefetch=max_prefetch,
                     config_data_width=config_data_width,
                     config_addr_width=config_addr_width,
                     remove_tb=remove_tb,
                     fifo_mode=False)

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
    #data_in[1] = 15
    for i in range(32):
        # Incrementing Data
        data_in[0] = data_in[0] + 1
        #data_in[1] = data_in[1] + 1 

        wen_en = 3
        wen    = 3

        ren_en = 0
        ren    = 0
        
        if i >=16:
            wen_en      = 0
            wen         = 0 
            ren_en      = 3 
            ren         = 3 

        #tester.circuit.addr_in = addr_in
        tester.circuit.wen_en = wen_en
        tester.circuit.wen_in = wen
        tester.circuit.ren_en = ren_en
        tester.circuit.ren_in = ren

        if interconnect_input_ports == 1:
            tester.circuit.data_in = data_in[0]
        else:
            for j in range(interconnect_input_ports):
               setattr(tester.circuit, f"data_in_{j}", data_in[j])
        #print("data0 and data1", data_in[0], data_in[1])
        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "pond_conv_dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    test_pond()
