from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.passes.passes import lift_config_reg
from lake.models.lake_top_model import LakeTopModel


def test_ports3_stride1(read_delay=1,
                        data_width=16,
                        mem_width=64,
                        mem_depth=512,
                        banks=1,
                        input_iterator_support=6,
                        output_iterator_support=6,
                        interconnect_input_ports=1,
                        interconnect_output_ports=3,
                        mem_input_ports=1,
                        mem_output_ports=1,
                        use_sram_stub=1,
                        agg_height=8,
                        transpose_height=8,
                        max_agg_schedule=64,
                        input_max_port_sched=64,
                        output_max_port_sched=64,
                        align_input=1,
                        max_line_length=256,
                        max_tb_height=1,
                        tb_range_max=64,
                        max_tb_stride=15,
                        tb_sched_max=64,
                        num_tb=1,
                        tb_iterator_support=2,
                        multiwrite=1,
                        max_prefetch=64):

    new_config = {}

    # Agg align
    new_config["strg_ub_agg_align_0_line_length"] = 64

    # Agg buffer
    new_config["strg_ub_agg_in_0_in_period"] = 1  # I don't actually know
    new_config["strg_ub_agg_in_0_in_sched_0"] = 0
    new_config["strg_ub_agg_in_0_out_period"] = 1
    new_config["strg_ub_agg_in_0_out_sched_0"] = 0

    # Input addr ctrl
    new_config["strg_ub_input_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 16
    new_config["strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_1"] = 100
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_1"] = 16

    # Output addr ctrl
    new_config["strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_2_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_1"] = 6
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_0"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_1"] = 6
    new_config["strg_ub_output_addr_ctrl_address_gen_2_ranges_0"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_2_ranges_1"] = 6
    new_config["strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_starting_addr"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_2_starting_addr"] = 4
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_1"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_1"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_2_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_2_strides_1"] = 2

    # TBA
    # do not matter
    new_config["strg_ub_tba_0_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_0_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_0_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_0_tb_0_range_inner"] = 3

    new_config["strg_ub_tba_0_tb_0_range_outer"] = 8
    new_config["strg_ub_tba_0_tb_0_stride"] = 1
#    new_config["strg_ub_tba_0_tb_0_tb_height"] = 1
    new_config["strg_ub_tba_0_tb_0_dimensionality"] = 1

    # do not matter
    new_config["strg_ub_tba_1_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_1_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_1_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_1_tb_0_range_inner"] = 3

    new_config["strg_ub_tba_1_tb_0_range_outer"] = 8
    new_config["strg_ub_tba_1_tb_0_stride"] = 1
#    new_config["strg_ub_tba_1_tb_0_tb_height"] = 1
    new_config["strg_ub_tba_1_tb_0_dimensionality"] = 1

    # do not matter
    new_config["strg_ub_tba_2_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_2_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_2_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_2_tb_0_range_inner"] = 3

    new_config["strg_ub_tba_2_tb_0_range_outer"] = 8
    new_config["strg_ub_tba_2_tb_0_stride"] = 1
#    new_config["strg_ub_tba_2_tb_0_tb_height"] = 1
    new_config["strg_ub_tba_2_tb_0_dimensionality"] = 1

    # Sets multiwrite
    new_config["strg_ub_input_addr_ctrl_offsets_cfg_0_0"] = 0

    new_config["strg_ub_sync_grp_sync_group_0"] = 1
    new_config["strg_ub_sync_grp_sync_group_1"] = 1
    new_config["strg_ub_sync_grp_sync_group_2"] = 1

    new_config["strg_ub_app_ctrl_input_port_0"] = 0
    new_config["strg_ub_app_ctrl_input_port_1"] = 0
    new_config["strg_ub_app_ctrl_input_port_2"] = 0
    new_config["strg_ub_app_ctrl_read_depth_0"] = 196
    new_config["strg_ub_app_ctrl_read_depth_1"] = 196
    new_config["strg_ub_app_ctrl_read_depth_2"] = 196
    new_config["strg_ub_app_ctrl_write_depth_0"] = 196

    model_lt = LakeTopModel(data_width=data_width,
                            mem_width=mem_width,
                            mem_depth=mem_depth,
                            banks=banks,
                            input_iterator_support=input_iterator_support,
                            output_iterator_support=output_iterator_support,
                            interconnect_input_ports=interconnect_input_ports,
                            interconnect_output_ports=interconnect_output_ports,
                            mem_input_ports=mem_input_ports,
                            mem_output_ports=mem_output_ports,
                            use_sram_stub=use_sram_stub,
                            agg_height=agg_height,
                            max_agg_schedule=max_agg_schedule,
                            input_max_port_sched=input_max_port_sched,
                            output_max_port_sched=output_max_port_sched,
                            align_input=align_input,
                            max_line_length=max_line_length,
                            tb_height=max_tb_height,
                            tb_range_max=tb_range_max,
                            tb_sched_max=tb_sched_max,
                            num_tb=num_tb,
                            multiwrite=multiwrite,
                            max_prefetch=max_prefetch,
                            read_delay=read_delay)

    model_lt.set_config(new_config=new_config)

    ### DUT
    lt_dut = LakeTop(data_width=data_width,
                     mem_width=mem_width,
                     mem_depth=mem_depth,
                     banks=banks,
                     input_iterator_support=input_iterator_support,
                     output_iterator_support=output_iterator_support,
                     interconnect_input_ports=interconnect_input_ports,
                     interconnect_output_ports=interconnect_output_ports,
                     mem_input_ports=mem_input_ports,
                     mem_output_ports=mem_output_ports,
                     use_sram_stub=use_sram_stub,
                     agg_height=agg_height,
                     max_agg_schedule=max_agg_schedule,
                     input_max_port_sched=input_max_port_sched,
                     output_max_port_sched=output_max_port_sched,
                     align_input=align_input,
                     max_line_length=max_line_length,
                     max_tb_height=max_tb_height,
                     tb_range_max=tb_range_max,
                     max_tb_stride=max_tb_stride,
                     tb_sched_max=tb_sched_max,
                     num_tb=num_tb,
                     tb_iterator_support=tb_iterator_support,
                     multiwrite=multiwrite,
                     max_prefetch=max_prefetch,
                     read_delay=read_delay,
                     fifo_mode=banks > 1)

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

    tester.circuit.strg_ub_tba_0_tb_0_tb_height = 1
    tester.circuit.strg_ub_tba_1_tb_0_tb_height = 1
    tester.circuit.strg_ub_tba_2_tb_0_tb_height = 1
    tester.circuit.strg_ub_app_ctrl_write_depth = 196

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    data_in = [0] * interconnect_input_ports
    valid_in = [0] * interconnect_input_ports
    ren = [0] * interconnect_output_ports
    addr_in = 0

    for i in range(500):
        # Rand data
        addr_in = rand.randint(0, 2 ** 16 - 1)
        for j in range(interconnect_input_ports):
            data_in[j] += 1  # rand.randint(0, 2 ** data_width - 1)
            valid_in[j] = 1  # rand.randint(0, 1)
        ren_tmp = rand.randint(0, 1)
        for j in range(interconnect_output_ports):
            ren[j] = ren_tmp
        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen[j] = valid_in[j]
        tester.circuit.addr_in = addr_in
        if interconnect_output_ports == 1:
            tester.circuit.ren = ren[0]
        else:
            for j in range(interconnect_output_ports):
                tester.circuit.ren[j] = ren[j]

        (mod_do, mod_vo) = model_lt.interact(data_in, addr_in, valid_in, ren)

        tester.eval()

        # Now check the outputs
        if(interconnect_output_ports == 1):
            tester.circuit.valid_out.expect(mod_vo[0])
            if mod_vo[0]:
                tester.circuit.data_out.expect(mod_do[0][0])
        else:
            for j in range(interconnect_output_ports):
                tester.circuit.valid_out[j].expect(mod_vo[j])
                if mod_vo[j]:
                    getattr(tester.circuit, f"data_out_{j}").expect(mod_do[j][0])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "ports3_dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal", "--trace"])


if __name__ == "__main__":
    test_ports3_stride1()
