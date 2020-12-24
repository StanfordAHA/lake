from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.passes.passes import lift_config_reg, change_sram_port_names
from lake.models.lake_top_model import LakeTopModel
from lake.utils.sram_macro import SRAMMacroInfo
# from lake.top.lake_chain import LakeChain


# These configurations are deprecated
@pytest.mark.skip
def test_mult_lines_dim1(data_width=16,
                         mem_width=64,
                         mem_depth=512,
                         banks=2,
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
                         tb_range_inner_max=5,
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
    new_config["strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 12
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_1"] = 100
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_1"] = 12

    # Output addr ctrl
    new_config["strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 3
    new_config["strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1

    # TBA

    # NOTE: both these configurations result in equivalent functionality

    # if dimensionality == 1 version
    new_config["strg_ub_tba_0_tb_0_range_outer"] = 9
    new_config["strg_ub_tba_0_tb_0_stride"] = 1
    new_config["strg_ub_tba_0_tb_0_dimensionality"] = 1

    # if dimensionality == 2 version
    new_config["strg_ub_tba_0_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_0_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_0_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_0_tb_0_indices_3"] = 3
    new_config["strg_ub_tba_0_tb_0_range_inner"] = 4
    # new_config["tba_0_tb_0_range_outer"] = 3
    # new_config["tba_0_tb_0_stride"] = 4
    # new_config["tba_0_tb_0_tb_height"] = 1
    # new_config["tba_0_tb_0_dimensionality"] = 2

    # Sets multiwrite
    new_config["strg_ub_input_addr_ctrl_offsets_cfg_0_0"] = 0

    new_config["strg_ub_sync_grp_sync_group_0"] = 1

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
                            tb_range_inner_max=tb_range_inner_max,
                            tb_sched_max=tb_sched_max,
                            num_tb=num_tb,
                            multiwrite=multiwrite,
                            max_prefetch=max_prefetch)

    model_lt.set_config(new_config=new_config)

    # DUT
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
                     tb_range_inner_max=tb_range_inner_max,
                     tb_sched_max=tb_sched_max,
                     num_tb=num_tb,
                     tb_iterator_support=tb_iterator_support,
                     multiwrite=multiwrite,
                     max_prefetch=max_prefetch)

    # Run the config reg lift
    # lift_config_reg(lt_dut.internal_generator)

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

    if interconnect_output_ports == 1:
        tester.circuit.strg_ub_sync_grp_sync_group[0] = 1

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1
    tester.eval()

    data_in = [0] * interconnect_input_ports
    valid_in = [0] * interconnect_input_ports
    wen_en = 1
    ren_in = [0] * interconnect_output_ports
    addr_in = 0
    output_en = 1

    for i in range(300):
        # Rand data
        addr_in = rand.randint(0, 2 ** 16 - 1)
        ren_tmp = rand.randint(0, 1)
        for j in range(interconnect_output_ports):
            ren_in[j] = ren_tmp
        for j in range(interconnect_input_ports):
            data_in[j] += 1  # rand.randint(0, 2 ** data_width - 1)
            valid_in[j] = rand.randint(0, 1)
        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen[j] = valid_in[j]
        tester.circuit.addr_in = addr_in
        for j in range(interconnect_output_ports):
            tester.circuit.ren_in[j] = ren_in[j]
        (mod_do, mod_vo) = model_lt.interact(data_in, addr_in, valid_in, ren_in)

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
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


# These configurations are deprecated
@pytest.mark.skip
@pytest.mark.parametrize("tb0_range_outer", [3, 4])
@pytest.mark.parametrize("tb0_stride", [2, 3])
def test_mult_lines_dim2(tb0_range_outer,
                         tb0_stride,
                         read_delay=0,
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
                         tb_range_inner_max=5,
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
    new_config["strg_ub_input_addr_ctrl_address_gen_0_dimensionality"] = 1
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 2048
    new_config["strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1

    # Output addr ctrl
    new_config["strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_2_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_1"] = 64
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_1"] = 64
    new_config["strg_ub_output_addr_ctrl_address_gen_2_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_2_ranges_1"] = 64
    new_config["strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_starting_addr"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_2_starting_addr"] = 32
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_1"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_1"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_2_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_2_strides_1"] = 16

    # TBA
    new_config["strg_ub_tba_0_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_0_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_0_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_0_tb_0_range_inner"] = 3
    new_config["strg_ub_tba_0_tb_0_range_outer"] = tb0_range_outer
    new_config["strg_ub_tba_0_tb_0_stride"] = tb0_stride
#    new_config["tba_0_tb_0_tb_height"] = 1
    new_config["strg_ub_tba_0_tb_0_dimensionality"] = 2

    new_config["strg_ub_tba_1_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_1_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_1_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_1_tb_0_range_inner"] = 3
    new_config["strg_ub_tba_1_tb_0_range_outer"] = 3
    new_config["strg_ub_tba_1_tb_0_stride"] = 3
#    new_config["tba_1_tb_0_tb_height"] = 1
    new_config["strg_ub_tba_1_tb_0_dimensionality"] = 2

    new_config["strg_ub_tba_2_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_2_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_2_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_2_tb_0_range_inner"] = 3
    new_config["strg_ub_tba_2_tb_0_range_outer"] = 3
    new_config["strg_ub_tba_2_tb_0_stride"] = 3
#    new_config["tba_2_tb_0_tb_height"] = 1
    new_config["strg_ub_tba_2_tb_0_dimensionality"] = 2

    # Sets multiwrite
    new_config["strg_ub_input_addr_ctrl_offsets_cfg_0_0"] = 0

    new_config["strg_ub_sync_grp_sync_group_0"] = 1
    new_config["strg_ub_sync_grp_sync_group_1"] = 1
    new_config["strg_ub_sync_grp_sync_group_2"] = 1

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
                            tb_range_inner_max=tb_range_inner_max,
                            tb_sched_max=tb_sched_max,
                            num_tb=num_tb,
                            multiwrite=multiwrite,
                            max_prefetch=max_prefetch,
                            read_delay=read_delay)

    model_lt.set_config(new_config=new_config)

    # DUT
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
                     tb_range_inner_max=tb_range_inner_max,
                     max_tb_stride=max_tb_stride,
                     tb_sched_max=tb_sched_max,
                     num_tb=num_tb,
                     tb_iterator_support=tb_iterator_support,
                     multiwrite=multiwrite,
                     max_prefetch=max_prefetch,
                     read_delay=read_delay,
                     fifo_mode=read_delay > 0)

    # Run the config reg lift
    # lift_config_reg(lt_dut.internal_generator)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    tester.zero_inputs()
    tester.circuit.clk_en = 1
    tester.eval()
    ###
    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    tester.circuit.strg_ub_tba_0_tb_0_tb_height = 1
    tester.circuit.strg_ub_tba_1_tb_0_tb_height = 1
    tester.circuit.strg_ub_tba_2_tb_0_tb_height = 1

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    data_in = [0] * interconnect_input_ports
    valid_in = [0] * interconnect_input_ports
    ren_in = [0] * interconnect_output_ports
    addr_in = 0

    for i in range(300):
        # Rand data
        addr_in = rand.randint(0, 2 ** 16 - 1)
        for j in range(interconnect_input_ports):
            data_in[j] += 1
            valid_in[j] = rand.randint(0, 1)

        ren_tmp = rand.randint(0, 1)
        for j in range(interconnect_output_ports):
            ren_in[j] = ren_tmp
        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen[j] = valid_in[j]
        tester.circuit.addr_in = addr_in
        for j in range(interconnect_output_ports):
            # ren_en[j] = 1
            tester.circuit.ren_in[j] = ren_in[j]
        (mod_do, mod_vo) = model_lt.interact(data_in, addr_in, valid_in, ren_in)

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
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


# These configurations are deprecated
@pytest.mark.skip
@pytest.mark.parametrize("mem_width", [16, 64])
@pytest.mark.parametrize("use_sram_stub", [0, 1])
@pytest.mark.parametrize("fifo_mode", [0, 1])
def test_sram_port_names_change(mem_width,
                                use_sram_stub,
                                fifo_mode,
                                data_width=16,
                                mem_depth=512,
                                banks=2,
                                input_iterator_support=6,
                                output_iterator_support=6,
                                interconnect_input_ports=1,
                                interconnect_output_ports=3,
                                mem_input_ports=1,
                                mem_output_ports=1,
                                sram_macro_info=SRAMMacroInfo(),
                                read_delay=1,
                                rw_same_cycle=False,
                                agg_height=8,
                                max_agg_schedule=64,
                                input_max_port_sched=64,
                                output_max_port_sched=64,
                                align_input=1,
                                max_line_length=256,
                                max_tb_height=1,
                                tb_range_max=64,
                                tb_range_inner_max=5,
                                tb_sched_max=64,
                                max_tb_stride=15,
                                num_tb=1,
                                tb_iterator_support=2,
                                multiwrite=1,
                                max_prefetch=64,
                                config_data_width=16,
                                config_addr_width=8,
                                remove_tb=False):

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
                     sram_macro_info=sram_macro_info,
                     read_delay=read_delay,
                     rw_same_cycle=rw_same_cycle,
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
                     fifo_mode=fifo_mode)

    change_sram_port_pass = change_sram_port_names(use_sram_stub, sram_macro_info)

    # Run the config reg lift
    # lift_config_reg(lt_dut.internal_generator)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False,
                                  additional_passes={"change_sram_port": change_sram_port_pass})


# 1 port, tested with enable_chain_output = 0 and = 1
@pytest.mark.skip
def test_chain_mult_tile(num_tiles=2,
                         banks=1,
                         interconnect_output_ports=3,
                         interconnect_input_ports=1,
                         mem_depth=4,
                         tb_range_max=512):

    new_config = {}

    for i in range(num_tiles):
        # Agg align
        new_config[f"tile_{i}_strg_ub_agg_align_0_line_length"] = 64

        # Agg buffer
        new_config[f"tile_{i}_strg_ub_agg_in_0_in_period"] = 1  # I don't actually know
        new_config[f"tile_{i}_strg_ub_agg_in_0_in_sched_0"] = 0
        new_config[f"tile_{i}_strg_ub_agg_in_0_out_period"] = 1
        new_config[f"tile_{i}_strg_ub_agg_in_0_out_sched_0"] = 0

        # Input addr ctrl
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_dimensionality"] = 2
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 8
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_ranges_1"] = 100
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_strides_1"] = 8

        # Output addr ctrl
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 1
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 8
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1

        # TBA

        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_range_outer"] = 8
        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_stride"] = 1
        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_dimensionality"] = 1

        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_indices_0"] = 0
        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_indices_1"] = 0
        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_range_inner"] = 2

        # Sets multiwrite
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_offsets_cfg_0_0"] = 0

        new_config[f"tile_{i}_strg_ub_sync_grp_sync_group_0"] = 1

        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_tb_height"] = 1

        new_config[f"tile_{i}_strg_ub_app_ctrl_input_port_0"] = 0
        new_config[f"tile_{i}_strg_ub_app_ctrl_input_port_1"] = 0
        new_config[f"tile_{i}_strg_ub_app_ctrl_input_port_2"] = 0
        new_config[f"tile_{i}_strg_ub_app_ctrl_read_depth_0"] = 32
        new_config[f"tile_{i}_strg_ub_app_ctrl_read_depth_1"] = 32
        new_config[f"tile_{i}_strg_ub_app_ctrl_read_depth_2"] = 32

    new_config["tile_0_strg_ub_app_ctrl_write_depth"] = 32
    new_config["tile_1_strg_ub_app_ctrl_write_depth"] = 32

    # DUT
    lt_dut = LakeChain(num_tiles=num_tiles,
                       banks=banks,
                       interconnect_input_ports=interconnect_input_ports,
                       interconnect_output_ports=interconnect_output_ports,
                       mem_depth=mem_depth,
                       tb_range_max=tb_range_max)

    # config reg lift is part of LakeChain module

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
    ren_in = [1] * interconnect_output_ports
    addr_in = 0

    for i in range(2000):
        # Rand data
        addr_in = rand.randint(0, 2 ** 16 - 1)
        for j in range(interconnect_input_ports):
            data_in[j] += 1  # rand.randint(0, 2 ** data_width - 1)
            data_in[j] = data_in[j] % 32
            valid_in[j] = 1  # rand.randint(0, 1)

        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen[j] = valid_in[j]

        tester.circuit.addr_in = addr_in

        # Chaining
        enable_chain_output = 0
        tester.circuit.enable_chain_output = enable_chain_output

        if interconnect_output_ports == 1:
            tester.circuit.ren_in = ren_in[0]
        else:
            for j in range(interconnect_output_ports):
                tester.circuit.ren_in[j] = ren_in[j]

        tester.eval()

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


# 3 ports, tested with enable_chain_output = 0 and = 1
@pytest.mark.skip
def test_chain_3porttile(num_tiles=2,
                         banks=1,
                         interconnect_output_ports=3,
                         interconnect_input_ports=1,
                         mem_depth=4,
                         tb_range_max=512):

    new_config = {}

    for i in range(num_tiles):
        # Agg align
        new_config[f"tile_{i}_strg_ub_agg_align_0_line_length"] = 64

        # Agg buffer
        new_config[f"tile_{i}_strg_ub_agg_in_0_in_period"] = 1  # I don't actually know
        new_config[f"tile_{i}_strg_ub_agg_in_0_in_sched_0"] = 0
        new_config[f"tile_{i}_strg_ub_agg_in_0_out_period"] = 1
        new_config[f"tile_{i}_strg_ub_agg_in_0_out_sched_0"] = 0

        # Input addr ctrl
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_dimensionality"] = 2
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 8
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_ranges_1"] = 100
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_address_gen_0_strides_1"] = 8

        # Output addr ctrl
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 1
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 8
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1

        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_1_dimensionality"] = 1
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_1_ranges_0"] = 8
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_1_starting_addr"] = 1
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_1_strides_0"] = 1

        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_2_dimensionality"] = 1
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_2_ranges_0"] = 8
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_2_starting_addr"] = 2
        new_config[f"tile_{i}_strg_ub_output_addr_ctrl_address_gen_2_strides_0"] = 1

        # TBA

        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_range_outer"] = 8
        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_stride"] = 1
        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_dimensionality"] = 1

        new_config[f"tile_{i}_strg_ub_tba_1_tb_0_range_outer"] = 8
        new_config[f"tile_{i}_strg_ub_tba_1_tb_0_stride"] = 1
        new_config[f"tile_{i}_strg_ub_tba_1_tb_0_dimensionality"] = 1

        new_config[f"tile_{i}_strg_ub_tba_2_tb_0_range_outer"] = 8
        new_config[f"tile_{i}_strg_ub_tba_2_tb_0_stride"] = 1
        new_config[f"tile_{i}_strg_ub_tba_2_tb_0_dimensionality"] = 1

        # Sets multiwrite
        new_config[f"tile_{i}_strg_ub_input_addr_ctrl_offsets_cfg_0_0"] = 0

        new_config[f"tile_{i}_strg_ub_sync_grp_sync_group_0"] = 1
        new_config[f"tile_{i}_strg_ub_sync_grp_sync_group_1"] = 1
        new_config[f"tile_{i}_strg_ub_sync_grp_sync_group_2"] = 1

        new_config[f"tile_{i}_strg_ub_tba_0_tb_0_tb_height"] = 1
        new_config[f"tile_{i}_strg_ub_tba_1_tb_0_tb_height"] = 1
        new_config[f"tile_{i}_strg_ub_tba_2_tb_0_tb_height"] = 1

        new_config[f"tile_{i}_strg_ub_app_ctrl_input_port_0"] = 0
        new_config[f"tile_{i}_strg_ub_app_ctrl_input_port_1"] = 0
        new_config[f"tile_{i}_strg_ub_app_ctrl_input_port_2"] = 0
        new_config[f"tile_{i}_strg_ub_app_ctrl_read_depth_0"] = 32
        new_config[f"tile_{i}_strg_ub_app_ctrl_read_depth_1"] = 32
        new_config[f"tile_{i}_strg_ub_app_ctrl_read_depth_2"] = 32

    new_config["tile_0_strg_ub_app_ctrl_write_depth"] = 32
    new_config["tile_1_strg_ub_app_ctrl_write_depth"] = 32
    # DUT
    lt_dut = LakeChain(num_tiles=num_tiles,
                       banks=banks,
                       interconnect_input_ports=interconnect_input_ports,
                       interconnect_output_ports=interconnect_output_ports,
                       mem_depth=mem_depth,
                       tb_range_max=tb_range_max)

    # config reg lift is part of Chain module
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
    tester.circuit.clk_en = 1
    tester.eval()

    data_in = [0] * interconnect_input_ports
    valid_in = [0] * interconnect_input_ports
    ren_in = [1] * interconnect_output_ports
    addr_in = 0

    for i in range(2000):
        # Rand data
        addr_in = rand.randint(0, 2 ** 16 - 1)
        for j in range(interconnect_input_ports):
            data_in[j] += 1  # rand.randint(0, 2 ** data_width - 1)
            data_in[j] = data_in[j] % 32
            valid_in[j] = 1  # rand.randint(0, 1)

        ren_tmp = rand.randint(0, 1)

        for j in range(interconnect_output_ports):
            ren_in[j] = ren_tmp

        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen[j] = valid_in[j]

        # Chaining
        enable_chain_output = 0
        tester.circuit.enable_chain_output = enable_chain_output

        if interconnect_output_ports == 1:
            tester.circuit.ren_in = ren_in[0]
        else:
            for j in range(interconnect_output_ports):
                tester.circuit.ren_in[j] = ren_in[j]

        tester.eval()

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


# These configurations are deprecated
@pytest.mark.skip
def test_identity_stream(data_width=16,
                         mem_width=64,
                         mem_depth=512,
                         banks=2,
                         input_iterator_support=6,
                         output_iterator_support=6,
                         input_config_width=16,
                         output_config_width=16,
                         interconnect_input_ports=1,
                         interconnect_output_ports=3,
                         mem_input_ports=1,
                         mem_output_ports=1,
                         use_sram_stub=1,
                         sram_macro_info=SRAMMacroInfo(),
                         read_delay=1,
                         rw_same_cycle=False,
                         agg_height=8,
                         max_agg_schedule=64,
                         input_max_port_sched=64,
                         output_max_port_sched=64,
                         align_input=1,
                         max_line_length=256,
                         max_tb_height=1,
                         tb_range_max=64,
                         tb_range_inner_max=5,
                         tb_sched_max=64,
                         max_tb_stride=15,
                         num_tb=1,
                         tb_iterator_support=2,
                         multiwrite=1,
                         max_prefetch=64,
                         config_data_width=32,
                         config_addr_width=8,
                         num_tiles=1,
                         remove_tb=False,
                         app_ctrl_depth_width=16,
                         fifo_mode=True,
                         add_clk_enable=True,
                         add_flush=True,
                         stcl_valid_iter=4):

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
    new_config["strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 12
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_1"] = 100
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_1"] = 12

    # Output addr ctrl
    new_config["strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 3
    new_config["strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1

    # TBA

    # NOTE: both these configurations result in equivalent functionality

    # if dimensionality == 1 version
    new_config["strg_ub_tba_0_tb_0_range_outer"] = 12
    new_config["strg_ub_tba_0_tb_0_stride"] = 1
    new_config["strg_ub_tba_0_tb_0_dimensionality"] = 1

    # if dimensionality == 2 version
    new_config["strg_ub_tba_0_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_0_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_0_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_0_tb_0_indices_3"] = 3
    new_config["strg_ub_tba_0_tb_0_range_inner"] = 4
    # new_config["tba_0_tb_0_range_outer"] = 3
    # new_config["tba_0_tb_0_stride"] = 4
    # new_config["tba_0_tb_0_tb_height"] = 1
    # new_config["tba_0_tb_0_dimensionality"] = 2

    # Sets multiwrite
    new_config["strg_ub_input_addr_ctrl_offsets_cfg_0_0"] = 0

    new_config["strg_ub_sync_grp_sync_group_0"] = 1

    new_config["strg_ub_app_ctrl_input_port_0"] = 0
    new_config["strg_ub_app_ctrl_input_port_1"] = 0
    new_config["strg_ub_app_ctrl_input_port_2"] = 0
    new_config["strg_ub_app_ctrl_read_depth_0"] = 196
    new_config["strg_ub_app_ctrl_read_depth_1"] = 196
    new_config["strg_ub_app_ctrl_read_depth_2"] = 196
    new_config["strg_ub_app_ctrl_write_depth_0"] = 196

    sram_name = sram_macro_info.name

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
                            sram_name=sram_name,
                            agg_height=agg_height,
                            max_agg_schedule=max_agg_schedule,
                            input_max_port_sched=input_max_port_sched,
                            output_max_port_sched=output_max_port_sched,
                            align_input=align_input,
                            max_line_length=max_line_length,
                            tb_height=max_tb_height,
                            tb_range_max=tb_range_max,
                            tb_range_inner_max=tb_range_inner_max,
                            tb_sched_max=tb_sched_max,
                            num_tb=num_tb,
                            multiwrite=multiwrite,
                            max_prefetch=max_prefetch,
                            num_tiles=num_tiles,
                            stcl_valid_iter=stcl_valid_iter)

    model_lt.set_config(new_config=new_config)

    # DUT
    lt_dut = LakeTop(data_width=data_width,
                     mem_width=mem_width,
                     mem_depth=mem_depth,
                     banks=banks,
                     input_iterator_support=input_iterator_support,
                     output_iterator_support=output_iterator_support,
                     input_config_width=input_config_width,
                     output_config_width=output_config_width,
                     interconnect_input_ports=interconnect_input_ports,
                     interconnect_output_ports=interconnect_output_ports,
                     mem_input_ports=mem_input_ports,
                     mem_output_ports=mem_output_ports,
                     use_sram_stub=use_sram_stub,
                     sram_macro_info=sram_macro_info,
                     read_delay=read_delay,
                     rw_same_cycle=rw_same_cycle,
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
                     num_tiles=num_tiles,
                     remove_tb=remove_tb,
                     app_ctrl_depth_width=app_ctrl_depth_width,
                     fifo_mode=fifo_mode,
                     add_clk_enable=add_clk_enable,
                     add_flush=add_flush,
                     stcl_valid_iter=stcl_valid_iter)

    # Run the config reg lift
    # lift_config_reg(lt_dut.internal_generator)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    tester.zero_inputs()
    tester.circuit.clk_en = 1
    tester.eval()
    ###
    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    tester.circuit.strg_ub_tba_0_tb_0_tb_height = 1
    tester.circuit.strg_ub_app_ctrl_write_depth = 196

    tester.circuit.enable_chain_output = 0
    tester.circuit.chain_idx_input = 0
    tester.circuit.chain_idx_output = 0

    if interconnect_output_ports == 1:
        tester.circuit.strg_ub_sync_grp_sync_group[0] = 1

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    data_in = [0] * interconnect_input_ports
    valid_in = [0] * interconnect_input_ports
    chain_data_in = [0] * interconnect_input_ports
    chain_valid_in = [0] * interconnect_input_ports

    ren_in = [1] * interconnect_output_ports
    addr_in = 0

    for i in range(300):
        # Rand data
        addr_in = rand.randint(0, 2 ** 16 - 1)
        for j in range(interconnect_input_ports):
            data_in[j] += 1  # rand.randint(0, 2 ** data_width - 1)
            valid_in[j] = 1  # rand.randint(0, 1)

        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen[j] = valid_in[j]
        tester.circuit.addr_in = addr_in

        (chain_out, chain_valid_out, mod_do, mod_vo) = \
            model_lt.interact(chain_data_in, chain_valid_in, data_in, addr_in, valid_in, ren_in)

        if interconnect_output_ports == 1:
            tester.circuit.ren_in = ren_in[0]
        else:
            for j in range(interconnect_output_ports):
                tester.circuit.ren_in[j] = ren_in[j]

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
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


# These configurations are deprecated
@pytest.mark.skip
@pytest.mark.parametrize("read_delay", [0, 1])
def test_top(read_delay,
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
             tb_range_inner_max=5,
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
    new_config["strg_ub_input_addr_ctrl_address_gen_0_dimensionality"] = 1
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 2048
    new_config["strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1

    # Output addr ctrl
    new_config["strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_2_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_1"] = 64
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_1"] = 64
    new_config["strg_ub_output_addr_ctrl_address_gen_2_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_2_ranges_1"] = 64
    new_config["strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_starting_addr"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_2_starting_addr"] = 32
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_1"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_1"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_2_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_2_strides_1"] = 16

    # TBA
    new_config["strg_ub_tba_0_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_0_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_0_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_0_tb_0_range_inner"] = 3
    new_config["strg_ub_tba_0_tb_0_range_outer"] = 62
    new_config["strg_ub_tba_0_tb_0_stride"] = 2
#    new_config["tba_0_tb_0_tb_height"] = 1
    new_config["strg_ub_tba_0_tb_0_dimensionality"] = 2

    new_config["strg_ub_tba_1_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_1_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_1_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_1_tb_0_range_inner"] = 3
    new_config["strg_ub_tba_1_tb_0_range_outer"] = 62
    new_config["strg_ub_tba_1_tb_0_stride"] = 2
#    new_config["tba_1_tb_0_tb_height"] = 1
    new_config["strg_ub_tba_1_tb_0_dimensionality"] = 2

    new_config["strg_ub_tba_2_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_2_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_2_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_2_tb_0_range_inner"] = 3
    new_config["strg_ub_tba_2_tb_0_range_outer"] = 62
    new_config["strg_ub_tba_2_tb_0_stride"] = 2
#    new_config["tba_2_tb_0_tb_height"] = 1
    new_config["strg_ub_tba_2_tb_0_dimensionality"] = 2

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
                            tb_range_inner_max=tb_range_inner_max,
                            tb_sched_max=tb_sched_max,
                            num_tb=num_tb,
                            multiwrite=multiwrite,
                            max_prefetch=max_prefetch,
                            read_delay=read_delay)

    model_lt.set_config(new_config=new_config)

    # DUT
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
                     tb_range_inner_max=tb_range_inner_max,
                     max_tb_stride=max_tb_stride,
                     tb_sched_max=tb_sched_max,
                     num_tb=num_tb,
                     tb_iterator_support=tb_iterator_support,
                     multiwrite=multiwrite,
                     max_prefetch=max_prefetch,
                     read_delay=read_delay,
                     fifo_mode=read_delay > 0)

    # Run the config reg lift
    # lift_config_reg(lt_dut.internal_generator)

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
    tester.circuit.clk_en = 1
    tester.eval()

    data_in = [0] * interconnect_input_ports
    valid_in = [0] * interconnect_input_ports
    ren_in = [1] * interconnect_output_ports
    addr_in = 0

    for i in range(1000):
        # Rand data
        addr_in = rand.randint(0, 2 ** 16 - 1)
        for j in range(interconnect_input_ports):
            data_in[j] += 1
            valid_in[j] = rand.randint(0, 1)
        ren_tmp = rand.randint(0, 1)
        for j in range(interconnect_output_ports):
            ren_in[j] = ren_tmp

        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen[j] = valid_in[j]

        if interconnect_output_ports == 1:
            tester.circuit.ren_in = ren_in[0]
        else:
            for j in range(interconnect_output_ports):
                tester.circuit.ren_in[j] = ren_in[j]
        (mod_do, mod_vo) = model_lt.interact(data_in, addr_in, valid_in, ren_in)

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
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


# These configurations are deprecated
@pytest.mark.skip
def test_config_storage(data_width=16,
                        mem_width=64,
                        mem_depth=512,
                        banks=2,
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
                        tb_range_inner_max=5,
                        tb_sched_max=64,
                        num_tb=1,
                        tb_iterator_support=2,
                        multiwrite=1,
                        max_prefetch=64,
                        config_addr_width=8,
                        read_delay=1):

    sets_per_macro = int(mem_depth / (2 ** config_addr_width))
    total_sets = sets_per_macro * banks
    fw_int = int(mem_width / data_width)

    new_config = {}

    # Agg align
    new_config["strg_ub_agg_align_0_line_length"] = 64

    # Agg buffer
    new_config["strg_ub_agg_in_0_in_period"] = 1  # I don't actually know
    new_config["strg_ub_agg_in_0_in_sched_0"] = 0
    new_config["strg_ub_agg_in_0_out_period"] = 1
    new_config["strg_ub_agg_in_0_out_sched_0"] = 0

    # Input addr ctrl
    new_config["strg_ub_input_addr_ctrl_address_gen_0_dimensionality"] = 1
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = 2048
    new_config["strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1

    # Output addr ctrl
    new_config["strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_2_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_1"] = 64
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_1"] = 64
    new_config["strg_ub_output_addr_ctrl_address_gen_2_ranges_0"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_2_ranges_1"] = 64
    new_config["strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_starting_addr"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_2_starting_addr"] = 32
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_1"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_1"] = 16
    new_config["strg_ub_output_addr_ctrl_address_gen_2_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_2_strides_1"] = 16

    # TBA
    new_config["strg_ub_tba_0_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_0_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_0_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_0_tb_0_range_inner"] = 3
    new_config["strg_ub_tba_0_tb_0_range_outer"] = 62
    new_config["strg_ub_tba_0_tb_0_stride"] = 2
#    new_config["tba_0_tb_0_tb_height"] = 1
    # new_config["tba_0_tb_0_dimensionality"] = 2

    new_config["strg_ub_tba_1_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_1_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_1_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_1_tb_0_range_inner"] = 3
    new_config["strg_ub_tba_1_tb_0_range_outer"] = 62
    new_config["strg_ub_tba_1_tb_0_stride"] = 2
#    new_config["tba_1_tb_0_tb_height"] = 1
#    new_config["tba_1_tb_0_dimensionality"] = 2

    new_config["strg_ub_tba_2_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_2_tb_0_indices_1"] = 1
    new_config["strg_ub_tba_2_tb_0_indices_2"] = 2
    new_config["strg_ub_tba_2_tb_0_range_inner"] = 3
    new_config["strg_ub_tba_2_tb_0_range_outer"] = 62
    new_config["strg_ub_tba_2_tb_0_stride"] = 2
#    new_config["tba_2_tb_0_tb_height"] = 1
#    new_config["tba_2_tb_0_dimensionality"] = 2

    # Sets multiwrite
    new_config["strg_ub_input_addr_ctrl_offsets_cfg_0_0"] = 0

    new_config["strg_ub_sync_grp_sync_group_0"] = 1
    new_config["strg_ub_sync_grp_sync_group_1"] = 1
    new_config["strg_ub_sync_grp_sync_group_2"] = 1

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
                            tb_range_inner_max=tb_range_inner_max,
                            tb_sched_max=tb_sched_max,
                            num_tb=num_tb,
                            multiwrite=multiwrite,
                            max_prefetch=max_prefetch,
                            read_delay=read_delay)

    model_lt.set_config(new_config=new_config)

    # DUT
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
                     tb_range_inner_max=tb_range_inner_max,
                     tb_sched_max=tb_sched_max,
                     num_tb=num_tb,
                     tb_iterator_support=tb_iterator_support,
                     multiwrite=multiwrite,
                     max_prefetch=max_prefetch,
                     config_addr_width=config_addr_width,
                     read_delay=read_delay)

    # Run the config reg lift
    # lift_config_reg(lt_dut.internal_generator)

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
    tester.circuit.strg_ub_tba_0_tb_0_dimensionality = 2
    tester.circuit.strg_ub_tba_1_tb_0_dimensionality = 2
    tester.circuit.strg_ub_tba_2_tb_0_dimensionality = 2

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1

    data_in = [0] * interconnect_input_ports
    valid_in = [0] * interconnect_input_ports
    wen_en = 1
    ren_en = 0
    addr_in = 0

    fw_int = int(mem_width / data_width)
    sets_per_macro = int(mem_depth / (2 ** config_addr_width))
    sets = sets_per_macro * banks

    tester.circuit.config_en = 0
    tester.circuit.config_read = 0
    tester.circuit.config_write = 0
    tester.circuit.config_data_in = 0
    tester.circuit.config_addr_in = 0
    for i in range(1000):
        # Pick a set and and addr
        tester.circuit.config_en = 0
        tester.circuit.config_write = 0
        tester.circuit.config_read = 0
        set_to_poke = rand.randint(0, total_sets - 1)
        addr = rand.randint(0, 2 ** config_addr_width - 1)
        data = []
        for j in range(fw_int):
            data.append(rand.randint(0, 2 ** data_width - 1))

        tester.circuit.config_addr_in = addr
        tester.circuit.config_write = 1
        tester.circuit.config_en[set_to_poke] = 1
        # Write it
        for j in range(fw_int):
            tester.circuit.config_data_in = data[j]
            tester.step(2)

        tester.circuit.config_write = 0
        tester.circuit.config_read = 1
        # Read it
        for j in range(fw_int):
            tester.step(2)
            getattr(tester.circuit, f"config_data_out_{set_to_poke}").expect(data[j])

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


# These configurations are deprecated
@pytest.mark.skip
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

    # DUT
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
    # lift_config_reg(lt_dut.internal_generator)

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
    ren_in = [0] * interconnect_output_ports
    addr_in = 0

    tester.circuit.clk_en = 1

    for i in range(1000):
        # Rand data
        addr_in = rand.randint(0, 2 ** 16 - 1)
        for j in range(interconnect_input_ports):
            data_in[j] += 1
            valid_in[j] = rand.randint(0, 1)
        ren_tmp = rand.randint(0, 1)
        for j in range(interconnect_output_ports):
            ren_in[j] = ren_tmp
        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen[j] = valid_in[j]
        tester.circuit.addr_in = addr_in
        if interconnect_output_ports == 1:
            tester.circuit.ren_in = ren_in[0]
        else:
            for j in range(interconnect_output_ports):
                tester.circuit.ren_in[j] = ren_in[j]

        (mod_do, mod_vo) = model_lt.interact(data_in, addr_in, valid_in, ren_in)

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
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


# These configurations are deprecated
@pytest.mark.skip
def test_2ports_idstream(data_width=16,
                         mem_width=64,
                         mem_depth=512,
                         banks=1,
                         input_iterator_support=6,
                         output_iterator_support=6,
                         interconnect_input_ports=2,
                         interconnect_output_ports=2,
                         mem_input_ports=1,
                         mem_output_ports=1,
                         use_sram_stub=1,
                         sram_macro_info=SRAMMacroInfo("default_name"),
                         read_delay=1,
                         rw_same_cycle=False,
                         agg_height=4,
                         max_agg_schedule=16,
                         input_max_port_sched=16,
                         output_max_port_sched=16,
                         align_input=1,
                         max_line_length=128,
                         max_tb_height=1,
                         tb_range_max=1024,
                         tb_range_inner_max=64,
                         tb_sched_max=16,
                         max_tb_stride=15,
                         num_tb=1,
                         tb_iterator_support=2,
                         multiwrite=1,
                         max_prefetch=8,
                         config_data_width=32,
                         config_addr_width=8,
                         num_tiles=2,
                         app_ctrl_depth_width=16,
                         remove_tb=False,
                         fifo_mode=True,
                         add_clk_enable=True,
                         add_flush=True,
                         # core_reset_pos=False,
                         stcl_valid_iter=4):

    fw_int = mem_width / data_width

    new_config = {}

    new_config["strg_ub_app_ctrl_input_port_0"] = 0
    new_config["strg_ub_app_ctrl_read_depth_0"] = 2 * mem_depth
    new_config["strg_ub_app_ctrl_write_depth_wo_0"] = mem_depth
    new_config["strg_ub_app_ctrl_write_depth_ss_0"] = mem_depth
    new_config["strg_ub_app_ctrl_coarse_read_depth_0"] = int(mem_depth / fw_int)
    new_config["strg_ub_app_ctrl_coarse_write_depth_wo_0"] = int(mem_depth / fw_int)
    new_config["strg_ub_app_ctrl_coarse_write_depth_ss_0"] = int(mem_depth / fw_int)

    new_config["strg_ub_input_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_0"] = int(mem_depth / fw_int)
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_1"] = 100
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_2"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_3"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_4"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_ranges_5"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_1"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_2"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_3"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_4"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_0_strides_5"] = 0

    new_config["strg_ub_output_addr_ctrl_address_gen_0_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_0"] = int(mem_depth / fw_int)
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_1"] = 100
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_2"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_3"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_4"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_ranges_5"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_starting_addr"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_1"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_2"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_3"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_4"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_0_strides_5"] = 0

    new_config["strg_ub_tba_0_tb_0_range_outer"] = mem_depth
    new_config["strg_ub_tba_0_tb_0_starting_addr"] = 0
    new_config["strg_ub_tba_0_tb_0_stride"] = 1
    new_config["strg_ub_tba_0_tb_0_dimensionality"] = 2
    new_config["strg_ub_tba_0_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_0_tb_0_indices_1"] = 0
    new_config["strg_ub_tba_0_tb_0_indices_2"] = 0
    new_config["strg_ub_tba_0_tb_0_indices_3"] = 0
    new_config["strg_ub_tba_0_tb_0_range_inner"] = 2
    new_config["strg_ub_tba_0_tb_0_tb_height"] = 1

    new_config["strg_ub_sync_grp_sync_group_0"] = 1
    new_config["tile_en"] = 1
    new_config["fifo_ctrl_fifo_depth"] = 0
    new_config["mode"] = 0
    new_config["flush_reg_sel"] = 1
    new_config["strg_ub_pre_fetch_0_input_latency"] = 4

    # chaining
    new_config["enable_chain_output"] = 0
    new_config["enable_chain_input"] = 0
    new_config["chain_idx_input"] = 0
    new_config["chain_idx_output"] = 0

    new_config["strg_ub_app_ctrl_input_port_1"] = 1
    new_config["strg_ub_app_ctrl_read_depth_1"] = 2 * mem_depth
    new_config["strg_ub_app_ctrl_write_depth_wo_1"] = mem_depth
    new_config["strg_ub_app_ctrl_write_depth_ss_1"] = mem_depth
    new_config["strg_ub_app_ctrl_coarse_read_depth_1"] = int(mem_depth / fw_int)
    new_config["strg_ub_app_ctrl_coarse_write_depth_wo_1"] = int(mem_depth / fw_int)
    new_config["strg_ub_app_ctrl_coarse_write_depth_ss_1"] = int(mem_depth / fw_int)

    new_config["strg_ub_input_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_0"] = int(mem_depth / fw_int)
    new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_1"] = 100
    new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_2"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_3"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_4"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_1_ranges_5"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_1_starting_addr"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_0"] = 1
    new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_1"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_2"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_3"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_4"] = 0
    new_config["strg_ub_input_addr_ctrl_address_gen_1_strides_5"] = 0

    new_config["strg_ub_output_addr_ctrl_address_gen_1_dimensionality"] = 2
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_0"] = int(mem_depth / fw_int)
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_1"] = 100
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_2"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_3"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_4"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_ranges_5"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_starting_addr"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_0"] = 1
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_1"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_2"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_3"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_4"] = 0
    new_config["strg_ub_output_addr_ctrl_address_gen_1_strides_5"] = 0

    new_config["strg_ub_tba_1_tb_0_range_outer"] = mem_depth
    new_config["strg_ub_tba_1_tb_0_starting_addr"] = 0
    new_config["strg_ub_tba_1_tb_0_stride"] = 1
    new_config["strg_ub_tba_1_tb_0_dimensionality"] = 2
    new_config["strg_ub_tba_1_tb_0_indices_0"] = 0
    new_config["strg_ub_tba_1_tb_0_indices_1"] = 0
    new_config["strg_ub_tba_1_tb_0_indices_2"] = 0
    new_config["strg_ub_tba_1_tb_0_indices_3"] = 0
    new_config["strg_ub_tba_1_tb_0_range_inner"] = 2
    new_config["strg_ub_tba_1_tb_0_tb_height"] = 1

    new_config["strg_ub_sync_grp_sync_group_1"] = 1
    new_config["strg_ub_pre_fetch_1_input_latency"] = 4

    # DUT
    lt_dut = LakeTop(data_width=data_width,
                     mem_width=mem_width,
                     banks=banks,
                     input_iterator_support=input_iterator_support,
                     output_iterator_support=output_iterator_support,
                     interconnect_input_ports=interconnect_input_ports,
                     interconnect_output_ports=interconnect_output_ports,
                     mem_input_ports=mem_input_ports,
                     mem_output_ports=mem_output_ports,
                     use_sram_stub=use_sram_stub,
                     sram_macro_info=sram_macro_info,
                     read_delay=read_delay,
                     rw_same_cycle=rw_same_cycle,
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
                     num_tiles=num_tiles,
                     app_ctrl_depth_width=app_ctrl_depth_width,
                     remove_tb=remove_tb,
                     fifo_mode=fifo_mode,
                     add_clk_enable=add_clk_enable,
                     add_flush=add_flush,
                     # core_reset_pos=core_reset_pos,
                     stcl_valid_iter=stcl_valid_iter)

    # Run the config reg lift
    # lift_config_reg(lt_dut.internal_generator)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    tester.zero_inputs()
    tester.circuit.clk_en = 1
    tester.eval()
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
    ren_in = [1] * interconnect_output_ports
    addr_in = 0

    for i in range(3 * mem_depth):
        # Rand data
        addr_in = rand.randint(0, 2 ** 16 - 1)
        for j in range(interconnect_input_ports):
            data_in[j] += 1  # rand.randint(0, 2 ** data_width - 1)
            valid_in[j] = 1  # rand.randint(0, 1)

        if(interconnect_input_ports == 1):
            tester.circuit.data_in = data_in[0]
            tester.circuit.wen_in = valid_in[0]
        else:
            for j in range(interconnect_input_ports):
                setattr(tester.circuit, f"data_in_{j}", data_in[j])
                tester.circuit.wen_in[j] = valid_in[j]

        tester.circuit.addr_in = addr_in

        if interconnect_output_ports == 1:
            tester.circuit.ren_in = ren_in[0]
            tester.circuit.chain_valid_in = 0
            tester.circuit.chain_data_in = 0
        else:
            for j in range(interconnect_output_ports):
                tester.circuit.ren_in[j] = ren_in[j]
                setattr(tester.circuit, f"chain_data_in_{j}", 0)
                tester.circuit.chain_valid_in[j] = 0

        tester.eval()

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    # test_2ports_idstream()
    # test_chain_mult_tile()
    # test_chain_3porttile()
    test_identity_stream()
    # test_mult_lines_dim1()
    # test_mult_lines_dim2(4, 2)
    # test_mult_lines_dim2(3, 3)
    # test_top(0)
    # test_config_storage()
    # test_ports3_stride1()
    # test_sram_port_names_change(16,0,1)
