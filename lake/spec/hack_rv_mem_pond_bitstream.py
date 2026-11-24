from lake.utils.spec_enum import Direction, LFComparisonOperator

import os
import math
import json
import copy

APPS_NEEDING_HACKS = [
    "gelu_pass1_mu_input_fp",
    "gelu_pass2_fp",
    "add_gelu_pass2_fp",
    "maxpooling_dense_rv_fp",
    "maxpooling_dense_rv_mem_buf_fp",
    "tanh_fp",
    "camera_pipeline_2x2_dense_rv",
]


def hack_rv_config(test_name, node_name=None):
    '''
    Main function to hack app. It has to hack config for all MEM/Pond in the apps.
    This function should only return the config for a single MEM/Pond.
    TODO: If Ponds/MEMs use different configs, then we have to feed tile information.
    '''
    print(f"\033[94mApplying hack_rv_config for a MEM/Pond in test_name: {test_name}\033[0m")
    rv_config = {}

    # Extract halide_gen_args dict for config
    HALIDE_GEN_ARGS = os.environ.get("HALIDE_GEN_ARGS", None)
    assert HALIDE_GEN_ARGS is not None, f"HALIDE_GEN_ARGS has to be set for hack_rv_config"
    halide_gen_args_dict = dict(item.split('=') for item in HALIDE_GEN_ARGS.strip().split())


    # Global hack for path balancing with ponds
    if "_path_balance_pond" in node_name:
        pe_id = node_name.split("_path_balance_pond")[0]
        app_path_balancing_json_file = f"/aha/Halide-to-Hardware/apps/hardware_benchmarks/apps/{test_name}/bin/path_balancing.json"
        assert os.path.exists(app_path_balancing_json_file), f"Cannot find path balancing json file: {app_path_balancing_json_file}"
        with open(app_path_balancing_json_file, "r") as f:
            path_balancing_metadata = json.load(f)

        balance_length = path_balancing_metadata["balance_lengths"][pe_id]
        total_stream_length = path_balancing_metadata["total_stream_lengths"][pe_id]
        pe_to_pond = path_balancing_metadata["pe_to_pond"][pe_id][0] # Get boolean value
        print(f"\033[93mINFO: Adding path balancing pond for PE {pe_id} with balance_length: {balance_length}, total_stream_length: {total_stream_length}. PE-to-pond is {pe_to_pond}\033[0m")
        rv_config = get_path_balancing_pond(balance_length=balance_length, total_stream_length=total_stream_length, pe_to_pond=pe_to_pond)

    elif test_name == "maxpooling_dense_rv_fp":
        # Line buffer with two read ports
        in_img = int(halide_gen_args_dict["in_img"])
        n_ic = int(halide_gen_args_dict["n_ic"])
        unroll = int(halide_gen_args_dict["unroll"])
        rv_config = get_mem_line_buffer_dual_port(
            line_size=in_img,
            num_lines=(in_img - 1) * n_ic // unroll,
            offset_r0=-2*in_img,
            offset_r1=-in_img,
        )

    elif test_name == "maxpooling_dense_rv_mem_buf_fp":
        print(f"configure node_name: {node_name}")
        if "mem" in node_name:
            # Line buffer with two read ports
            in_img = int(halide_gen_args_dict["in_img"])
            n_ic = int(halide_gen_args_dict["n_ic"])
            unroll = int(halide_gen_args_dict["unroll"])
            mem_idx = node_name.split("_")[-1]
            assert mem_idx in ["0", "1", "2", "3"], f"Invalid mem_idx: {mem_idx}"
            if mem_idx == "0":
                offset_r0 = -1
                offset_r1 = -2
            elif mem_idx == "1":
                offset_r0 = -in_img
                offset_r1 = -(in_img + 1)
            elif mem_idx == "2":
                offset_r0 = -(in_img + 2)
                offset_r1 = -2 * in_img
            elif mem_idx == "3":
                offset_r0 = -(2 * in_img + 1)
                offset_r1 = -(2 * in_img + 2)
            rv_config = get_mem_line_buffer_dual_port(
                line_size=in_img,
                num_lines=(in_img - 1) * n_ic // unroll,
                offset_r0=offset_r0,
                offset_r1=offset_r1
            )
        elif "_path_balance_pond" in node_name:
            pe_id = node_name.split("_path_balance_pond")[0]
            app_path_balancing_json_file = f"/aha/Halide-to-Hardware/apps/hardware_benchmarks/apps/{test_name}/bin/path_balancing.json"
            assert os.path.exists(app_path_balancing_json_file), f"Cannot find path balancing json file: {app_path_balancing_json_file}"
            with open(app_path_balancing_json_file, "r") as f:
                path_balancing_metadata = json.load(f)
            balance_length = path_balancing_metadata["balance_lengths"][pe_id]
            total_stream_length = path_balancing_metadata["total_stream_lengths"][pe_id]
            pe_to_pond = path_balancing_metadata["pe_to_pond"][pe_id]
            print(f"\033[93mINFO: Adding path balancing pond for PE {pe_id} with balance_length: {balance_length}, total_stream_length: {total_stream_length}. PE-to-pond is {pe_to_pond}\033[0m")
            rv_config = get_path_balancing_pond(balance_length=balance_length, total_stream_length=total_stream_length, pe_to_pond=pe_to_pond)
        else:
            raise ValueError(f"Invalid node name: {node_name}")

    elif test_name in ["gelu_pass1_mu_input_fp", "gelu_pass2_fp", "add_gelu_pass1_mu_input_mem_buf_fp", "add_gelu_pass2_fp"]:
        print(f"configure node_name: {node_name}")
        vec_len = int(halide_gen_args_dict['vec_width'])
        num_vecs = int(halide_gen_args_dict['vec_height'])
        if "input_buffer_mem" in node_name:
            if test_name in ["gelu_pass1_mu_input_fp", "add_gelu_pass1_mu_input_mem_buf_fp"]:
                mu_i = int(halide_gen_args_dict['mu_i'])
                rv_config = get_mem_dual_read(input_stream_size=vec_len * num_vecs // mu_i)
            elif test_name in ["gelu_pass2_fp", "add_gelu_pass2_fp"]:
                glb_i = int(halide_gen_args_dict['glb_i'])
                rv_config = get_mem_dual_read(input_stream_size=vec_len * num_vecs // glb_i)
        elif "mu_buffer_mem" in node_name:
            if test_name in ["add_gelu_pass1_mu_input_mem_buf_fp"]:
                mu_i = int(halide_gen_args_dict['mu_i'])
                rv_config = get_mem_single_read(input_stream_size=vec_len * num_vecs // mu_i)
        elif "_path_balance_pond" in node_name:
            pe_id = node_name.split("_path_balance_pond")[0]
            app_path_balancing_json_file = f"/aha/Halide-to-Hardware/apps/hardware_benchmarks/apps/{test_name}/bin/path_balancing.json"
            assert os.path.exists(app_path_balancing_json_file), f"Cannot find path balancing json file: {app_path_balancing_json_file}"
            with open(app_path_balancing_json_file, "r") as f:
                path_balancing_metadata = json.load(f)
            balance_length = path_balancing_metadata["balance_lengths"][pe_id]
            total_stream_length = path_balancing_metadata["total_stream_lengths"][pe_id]
            pe_to_pond = path_balancing_metadata["pe_to_pond"][pe_id]
            print(f"\033[93mINFO: Adding path balancing pond for PE {pe_id} with balance_length: {balance_length}, total_stream_length: {total_stream_length}. PE-to-pond is {pe_to_pond}\033[0m")
            rv_config = get_path_balancing_pond(balance_length=balance_length, total_stream_length=total_stream_length, pe_to_pond=pe_to_pond)
        else:
            raise ValueError(f"Invalid node name: {node_name}")

    elif test_name == "tanh_fp":
        import re
        print(f"configure node_name: {node_name}")
        vec_len = int(halide_gen_args_dict['vec_len'])
        glb_o = int(halide_gen_args_dict['glb_o'])
        if "output_buffer" in node_name:
            match = re.search(r"_stencil_(\d+)_write", node_name)
            lane_idx = int(match.group(1)) if match else 0
            rv_config = get_mem_single_read(input_stream_size=vec_len // glb_o, rv_stride=glb_o, filter_offset_scalar=lane_idx)
        elif "_path_balance_pond" in node_name:
            pe_id = node_name.split("_path_balance_pond")[0]
            app_path_balancing_json_file = f"/aha/Halide-to-Hardware/apps/hardware_benchmarks/apps/{test_name}/bin/path_balancing.json"
            assert os.path.exists(app_path_balancing_json_file), f"Cannot find path balancing json file: {app_path_balancing_json_file}"
            with open(app_path_balancing_json_file, "r") as f:
                path_balancing_metadata = json.load(f)

            balance_length = path_balancing_metadata["balance_lengths"][pe_id]
            total_stream_length = path_balancing_metadata["total_stream_lengths"][pe_id]
            pe_to_pond = path_balancing_metadata["pe_to_pond"][pe_id]
            print(f"\033[93mINFO: Adding path balancing pond for PE {pe_id} with balance_length: {balance_length}, total_stream_length: {total_stream_length}. PE-to-pond is {pe_to_pond}\033[0m")
            rv_config = get_path_balancing_pond(balance_length=balance_length, total_stream_length=total_stream_length, pe_to_pond=pe_to_pond)
        else:
            raise ValueError(f"Invalid node name: {node_name}")

    elif test_name == "camera_pipeline_2x2_dense_rv":
        print(f"configure node_name: {node_name}")

        # Category 1: line buffer
        if node_name in [
            "hw_input_global_wrapper_global_wrapper_stencil$ub_hw_input_global_wrapper_global_wrapper_stencil_BANK_2_garnet",
            "hw_input_global_wrapper_global_wrapper_stencil$ub_hw_input_global_wrapper_global_wrapper_stencil_BANK_4_garnet",
            "hw_input_global_wrapper_global_wrapper_stencil$ub_hw_input_global_wrapper_global_wrapper_stencil_BANK_6_garnet"
        ]:
            rv_config = get_mem_line_buffer_single_port(line_size=33, num_lines=48, offset_r0=-33)
        elif node_name == "hw_input_global_wrapper_global_wrapper_stencil$ub_hw_input_global_wrapper_global_wrapper_stencil_BANK_5_garnet":
            rv_config = get_mem_line_buffer_single_port(line_size=33, num_lines=48, offset_r0=-67)
        elif node_name in [
            "hw_input_global_wrapper_global_wrapper_stencil$ub_hw_input_global_wrapper_global_wrapper_stencil_BANK_3_garnet",
            "hw_input_global_wrapper_global_wrapper_stencil$ub_hw_input_global_wrapper_global_wrapper_stencil_BANK_7_garnet"
        ]:
            rv_config = get_mem_line_buffer_single_port(line_size=33, num_lines=48, offset_r0=-67)
        elif node_name == "hw_input_global_wrapper_global_wrapper_stencil$ub_hw_input_global_wrapper_global_wrapper_stencil_bank_8_garnet":
            rv_config = get_mem_line_buffer_dual_port(line_size=33, num_lines=48, offset_r0=-33, offset_r1=-67)

        # Category 2: demosaic RGB stencil MEM
        if node_name == "b_b_stencil$ub_b_b_stencil_BANK_0_garnet_dup_1": #m1
            rv_config = get_demosaic_rgb_stencil_mem_single_port(
                    in_line_size=29,
                    in_num_lines=45,
                    out_line_size=33,
                    out_num_lines=49,
                    filter_offset_scalar=102,
                    address_offset_scalar=-135
                )
            return rv_config
        # elif node_name == "r_r_stencil$ub_r_r_stencil_BANK_0_garnet_dup_4": #m48
        #     rv_config = get_demosaic_rgb_stencil_mem_single_port(
        #             in_line_size=29,
        #             in_num_lines=45,
        #             out_line_size=33,
        #             out_num_lines=49,
        #             filter_offset_scalar=102,
        #             address_offset_scalar=-134
        #         )
        #     return rv_config
        elif node_name == "g_r_stencil$ub_g_r_stencil_BANK_0_garnet_dup_1": #m15
            rv_config = get_demosaic_rgb_stencil_mem_single_port(
                    in_line_size=29,
                    in_num_lines=45,
                    out_line_size=33,
                    out_num_lines=49,
                    filter_offset_scalar=102,
                    address_offset_scalar=-135
                )
            return rv_config
        # elif node_name == "r_r_stencil$ub_r_r_stencil_BANK_0_garnet_dup_2": #m46
        #     rv_config = get_demosaic_rgb_stencil_mem_single_port(
        #             in_line_size=29,
        #             in_num_lines=45,
        #             out_line_size=33,
        #             out_num_lines=49,
        #             filter_offset_scalar=102,
        #             address_offset_scalar=-134
        #         )
        #     return rv_config

        for name_pattern in [
            "g_gb_stencil$ub_g_gb_stencil_BANK_0_garnet",
            "g_gr_stencil$ub_g_gr_stencil_BANK_0_garnet"
        ]:
            if name_pattern in node_name:
                rv_config = get_demosaic_rgb_stencil_mem_single_port(
                    in_line_size=30,
                    in_num_lines=46,
                    out_line_size=33,
                    out_num_lines=49,
                    filter_offset_scalar=68,
                    address_offset_scalar=-101
                )
                break

        # for name_pattern in [
        #     "b_b_stencil$ub_b_b_stencil_BANK_0_garnet",
        # ]:
        #     if name_pattern in node_name:
        #         rv_config = get_demosaic_rgb_stencil_mem_single_port(
        #             in_line_size=29,
        #             in_num_lines=45,
        #             out_line_size=33,
        #             out_num_lines=49,
        #             filter_offset_scalar=102,
        #             address_offset_scalar=-134
        #         )
        #         break

        for name_pattern in [
            "g_b_stencil$ub_g_b_stencil_BANK_0_garnet",
            "g_r_stencil$ub_g_r_stencil_BANK_0_garnet",
            "r_r_stencil$ub_r_r_stencil_BANK_0_garnet",
            "b_b_stencil$ub_b_b_stencil_BANK_0_garnet",
        ]:
            if name_pattern in node_name:
                rv_config = get_demosaic_rgb_stencil_mem_single_port(
                    in_line_size=29,
                    in_num_lines=45,
                    out_line_size=33,
                    out_num_lines=49,
                    filter_offset_scalar=102,
                    address_offset_scalar=-135
                )
                break

        if "r_gr_stencil$ub_r_gr_stencil_BANK_0_garnet" in node_name:
            rv_config = get_demosaic_rgb_stencil_mem_single_port(
                in_line_size=28,
                in_num_lines=44,
                out_line_size=33,
                out_num_lines=48,
                filter_offset_scalar=103,
                address_offset_scalar=-136,
                raw_scalar_guard_offset=2
            )

        # Category 3: static cycle dma emulator MEM
        if "static_cycle_dma_emulator_mem_" in node_name:
            clkwrk_idx = int(node_name.split("static_cycle_dma_emulator_mem_")[-1])
            if clkwrk_idx in [0, 3]:
                rv_config = get_static_cycle_dma_emulator_mem(
                    cycle_start_addr=0,
                    cycle_stride_outer=1,
                    input_extent_inner=33,
                    input_extent_outer=48
                )
            elif clkwrk_idx == 1:
                rv_config = get_static_cycle_dma_emulator_mem(
                    cycle_start_addr=33,
                    cycle_stride_outer=1,
                    input_extent_inner=33,
                    input_extent_outer=48
                )
            elif clkwrk_idx == 2:
                rv_config = get_static_cycle_dma_emulator_mem(
                    cycle_start_addr=1,
                    cycle_stride_outer=1,
                    input_extent_inner=33,
                    input_extent_outer=48
                )
            else:
                raise ValueError(f"Invalid node name: {node_name} for static cycle dma emulator MEM tiles")

        elif "_path_balance_pond" in node_name:
            pe_id = node_name.split("_path_balance_pond")[0]
            app_path_balancing_json_file = f"/aha/Halide-to-Hardware/apps/hardware_benchmarks/apps/{test_name}/bin/path_balancing.json"
            assert os.path.exists(app_path_balancing_json_file), f"Cannot find path balancing json file: {app_path_balancing_json_file}"
            with open(app_path_balancing_json_file, "r") as f:
                path_balancing_metadata = json.load(f)

            balance_length = path_balancing_metadata["balance_lengths"][pe_id]
            total_stream_length = path_balancing_metadata["total_stream_lengths"][pe_id]
            pe_to_pond = path_balancing_metadata["pe_to_pond"][pe_id]
            print(f"\033[93mINFO: Adding path balancing pond for PE {pe_id} with balance_length: {balance_length}, total_stream_length: {total_stream_length}. PE-to-pond is {pe_to_pond}\033[0m")
            rv_config = get_path_balancing_pond(balance_length=balance_length, total_stream_length=total_stream_length, pe_to_pond=pe_to_pond)

        if rv_config is None:
            raise ValueError(f"Invalid node name: {node_name}")

    # Global hack for path balancing with ponds
    elif "_path_balance_pond" in node_name:
        pe_id = node_name.split("_path_balance_pond")[0]
        app_path_balancing_json_file = f"/aha/Halide-to-Hardware/apps/hardware_benchmarks/apps/{test_name}/bin/path_balancing.json"
        assert os.path.exists(app_path_balancing_json_file), f"Cannot find path balancing json file: {app_path_balancing_json_file}"
        with open(app_path_balancing_json_file, "r") as f:
            path_balancing_metadata = json.load(f)

        balance_length = path_balancing_metadata["balance_lengths"][pe_id]
        total_stream_length = path_balancing_metadata["total_stream_lengths"][pe_id]
        pe_to_pond = path_balancing_metadata["pe_to_pond"][pe_id]
        print(f"\033[93mINFO: Adding path balancing pond for PE {pe_id} with balance_length: {balance_length}, total_stream_length: {total_stream_length}. PE-to-pond is {pe_to_pond}\033[0m")
        rv_config = get_path_balancing_pond(balance_length=balance_length, total_stream_length=total_stream_length, pe_to_pond=pe_to_pond)

    assert rv_config, f"rv_config is empty for test_name: {test_name}"
    return rv_config


def get_accum_pond(num_partial_reduction=64, num_output_pixels=1):
    '''
    Helper function to create config for reduction pond
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    MEM port mapping: 0: port_w0, 1: port_w1, 2: port_r0, 3: port_r1
    '''

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 1,
            'extents': [num_partial_reduction],
            'address': {
                # read-modify-write the same address
                'strides': [0],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[2] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 1,
            'extents': [num_partial_reduction],
            'address': {
                # read-modify-write the same address
                'strides': [0],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r1',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 1,
            'extents': [num_output_pixels],
            'address': {
                # read-modify-write the same address
                'strides': [0],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    pw = 0
    pr1 = 2
    pr2 = 3

    # Allows the reads to start early to read out the initial zero value in pond
    raw_r1_scalar = -1
    raw_r1_constraint = (pr1, 0, pw, 0, LFComparisonOperator.LT.value, raw_r1_scalar)

    raw_r2_scalar = num_partial_reduction
    raw_r2_constraint = (pr2, 0, pw, 0, LFComparisonOperator.LT.value, raw_r2_scalar)

    # Just have read follow write
    linear_test['constraints'] = [raw_r1_constraint, raw_r2_constraint]

    return linear_test


def get_vec_accum_pond(num_partial_reduction=64, num_output_pixels=4):
    '''
    Helper function to create config for vector reduction pond, where pond has to autorestart after each vector's reduction
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    MEM port mapping: 0: port_w0, 1: port_w1, 2: port_r0, 3: port_r1
    '''

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': [num_partial_reduction, num_output_pixels],
            'address': {
                # read-modify-write the same address
                'strides': [0, 0],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[1] = {
        'name': 'port_w_init',
        'type': Direction.IN,
        'config': {
            'dimensionality': 1,
            'extents': [num_output_pixels],
            'address': {
                # address for the init (flush) port does not matter and it's not consumed
                # this port just OR in the 1 bit enable into the flush
                'strides': [0],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[2] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [num_partial_reduction, num_output_pixels],
            'address': {
                # read-modify-write the same address
                'strides': [0, 0],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r1',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 1,
            'extents': [num_output_pixels],
            'address': {
                # read-modify-write the same address
                'strides': [0],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    pw = 0
    pwinit = 1
    pr1 = 2
    pr2 = 3

    # Let the update port read out a data before write update
    raw_r1_scalar = -1
    raw_r1_constraint = (pr1, 0, pw, 0, LFComparisonOperator.LT.value, raw_r1_scalar)

    # Wait for the write update to finish its last accum write before spill
    raw_r2_scalar = 0
    raw_r2_constraint = (pr2, 0, pw, 1, LFComparisonOperator.LT.value, raw_r2_scalar)

    # Don't let the read_update start until the memory has been cleared
    raw_r1_pwinit_scalar = 0
    raw_r1_pwinit_constraint = (pr1, 1, pwinit, 0, LFComparisonOperator.LT.value, raw_r1_pwinit_scalar)

    # Don't clear the memory until the spill has happened (slip of 1)
    war_pwinit_r2_scalar = 1
    war_winit_r2_constraint = (pwinit, 0, pr2, 0, LFComparisonOperator.GT.value, war_pwinit_r2_scalar)

    # Add all constraints
    linear_test['constraints'] = [raw_r1_constraint, raw_r2_constraint, raw_r1_pwinit_constraint, war_winit_r2_constraint]

    return linear_test


def get_repeat_pond(num_writes=2, num_reads=1024):
    '''
    Pond schedule that holds one value and returns it num_reads times.

    Write port 0 writes every token to word 0; the tokens are copies of one
    value (for example a weight that a GLB IO reads twice). Read port 0
    (port 2, the port that feeds the PE packed into the Pond's tile) reads
    word 0 num_reads times. A read waits until the writer has written more
    tokens than have been read, and runs freely once the writer is finished.
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    '''
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    assert 1 <= num_writes <= MAX_EXTENT
    read_extents = [num_reads]
    while read_extents[0] > MAX_EXTENT:
        assert read_extents[0] % 2 == 0, f"ERROR: cannot split {num_reads} pond reads into extents"
        read_extents = [read_extents[0] // 2, 2 * (read_extents[1] if len(read_extents) > 1 else 1)]
    assert all(e <= MAX_EXTENT for e in read_extents)

    linear_test = {}
    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 1,
            'extents': [num_writes],
            'address': {
                'strides': [0],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }
    linear_test[2] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': len(read_extents),
            'extents': read_extents,
            'address': {
                'strides': [0] * len(read_extents),
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }
    pw = 0
    pr0 = 2
    # read count < write count until the writer finishes, then always true
    raw_constraint = (pr0, 0, pw, 0, LFComparisonOperator.LT.value, 0)
    linear_test['constraints'] = [raw_constraint]
    return linear_test


def get_row_shift_pond(num_groups, num_rows, shift):
    '''
    Pond schedule that shifts a row-major stream by ``shift`` elements within
    each row: for element g of row q it returns element g - shift of row q,
    or 0 for g < shift. A row is num_groups elements and the rows follow
    each other.

    Row q is written to the B Pond words starting at B * q (mod 32), where B
    is the smallest power of two of at least 2 * num_groups - 1, and read
    port 0 (port 2) reads word B * q + g - shift. For g < shift that word is
    one of the last shift words of the previous row's block, which are never
    written; the clear (port 1) zeroes the Pond before the first row (twice,
    as iteration extents are programmed as extent - 2).

    With 4 or more rows in flight (num_groups <= 4), row q is read once it is
    completely written, and the writer is held back once it is 32 / B - 1
    rows ahead. With only 2 rows in flight (5 to 8 groups, B = 16), those
    row guards would hold the writer back while the reader reads the
    previous row, so both guards compare single elements instead: element t
    is read once the writer has written element t, and the writer stays
    fewer than num_groups elements ahead of the reader. The ports are then
    never a whole row apart, so the comparison network's row correction (it
    adds a row to the writer's count when the row counters differ) is exact,
    and a write never reaches a word the reader still needs (that word is
    2 * num_groups - shift elements back).

    The Pond's input ready is tied high, so a held-back write is lost: the
    reader must keep up (it must never wait for a downstream join), and the
    guard only turns an overflow into a hang. Each constraint compares one
    counter level of one port pair; the comparison network holds one
    comparison per pair.
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    '''
    POND_DEPTH = 32
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    assert 1 <= shift < num_groups, f"ERROR: shift {shift} must lie in [1, {num_groups})"
    assert 2 <= num_rows <= MAX_EXTENT, f"ERROR: {num_rows} rows must lie in [2, {MAX_EXTENT}]"
    block = 1
    while block < 2 * num_groups - 1:
        block *= 2
    rows_in_flight = POND_DEPTH // block
    assert rows_in_flight >= 2, f"ERROR: rows of {num_groups} elements need {block} Pond words, more than half the Pond"

    def port(name, direction, extents, strides, offset):
        return {
            'name': name,
            'type': direction,
            'config': {
                'dimensionality': len(extents),
                'extents': extents,
                'address': {'strides': strides, 'offset': offset},
                'schedule': {}
            },
            'vec_in_config': {},
            'vec_out_config': {},
            'vec_constraints': []
        }

    linear_test = {}
    linear_test[0] = port('port_w0', Direction.IN, [num_groups, num_rows], [1, block], 0)
    linear_test[1] = port('port_w_init', Direction.IN, [2], [0], 0)
    linear_test[2] = port('port_r0', Direction.OUT, [num_groups, num_rows], [1, block], POND_DEPTH - shift)
    pw, pwinit, pr = 0, 1, 2
    LT, GT = LFComparisonOperator.LT.value, LFComparisonOperator.GT.value
    if rows_in_flight > 2:
        linear_test['constraints'] = [
            # read row q once the writer has finished it
            (pr, 1, pw, 1, LT, 0),
            # read only after both clears
            (pr, 1, pwinit, 0, LT, 1),
            # overwrite a row's block only after its reads (one row of margin)
            (pw, 1, pr, 1, GT, rows_in_flight - 1),
        ]
    else:
        linear_test['constraints'] = [
            # read element t once the writer has written it
            (pr, 0, pw, 0, LT, 0),
            # read only after both clears
            (pr, 1, pwinit, 0, LT, 1),
            # writer < reader + num_groups: fewer than a row ahead
            (pw, 0, pr, 0, GT, num_groups),
        ]
    return linear_test


def get_col_cumsum_pond(num_groups, num_rows, num_mats):
    '''
    Pond schedule of one lane of a cumsum along dim -2 (down the rows).

    The lane sees column groups m = 0 .. num_groups - 1 of row r, then of row
    r + 1, and so on; num_rows rows form one matrix, and num_mats matrices
    follow each other. Its PE adds the input to the running sum of the same
    column in the row above, read from this Pond, and writes the new sum back:

        y[r, m] = y[r - 1, m] + x[r, m],   y[-1, m] = 0

    The Pond is a ring of its 32 words (addresses wrap). Token t of a matrix
    (t = r * num_groups + m) is written by port_w0 to word t mod 32; port_r0
    (port 2, the tile's PE) reads word (t - num_groups) mod 32, the sum of the
    same column one row up; port_r1 (port 3, the interconnect) reads word
    t mod 32 again, the new sum, which is the cumsum output. The clear (port 1)
    zeroes the Pond once before each matrix (num_mats >= 2, as iteration
    extents are programmed as extent - 2), so the first row reads zeros.

    A matrix of more than 1,024 tokens (the 11-bit extent limit) is counted
    in chunks of up to 512 tokens, as in get_fifo_pond. The token guards
    still compare single tokens: when two ports are in different chunks, the
    comparison network adds the gated port's chunk extent, which is exact
    because the ports stay fewer than POND_DEPTH + num_groups tokens apart,
    less than a chunk. The last chunk of a matrix and the first chunk of the
    next are adjacent in token order too, so the guards also hold across
    matrices.

    Each constraint compares one counter level of one read port and one
    write port (the comparison network has no read-to-read comparison, and
    one comparison per pair and direction):
    - r0 reads token t once w0 has written token t - num_groups;
    - r1 reads token t once w0 has written it;
    - w0 stays fewer than 30 tokens ahead of r1 (ring of 32 words);
    - r0 starts matrix b after the clears of matrix b;
    - the clears of matrix b wait until r1 has finished matrix b - 1.
    The write port cannot hold back its writer (the Pond's input ready is
    tied high), and r1 cannot pace r0, so a held-back write is lost: the
    output must keep up with the loop (r1 must stay fewer than 30 tokens
    behind w0), and the w0 guard only turns an overflow into a hang.
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    '''
    POND_DEPTH = 32
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    tokens = num_groups * num_rows
    assert 1 <= num_groups < POND_DEPTH - 2, f"ERROR: {num_groups} column groups do not fit the Pond ring"
    assert tokens >= 2, f"ERROR: {tokens} tokens per matrix, need at least 2"
    assert 2 <= num_mats <= MAX_EXTENT, f"ERROR: {num_mats} matrices must lie in [2, {MAX_EXTENT}]"
    if tokens <= MAX_EXTENT:
        tok_extents, tok_strides = [tokens], [1]
    else:
        inner = next((e for e in range(MAX_EXTENT // 2, POND_DEPTH + num_groups, -1)
                      if tokens % e == 0 and tokens // e <= MAX_EXTENT), None)
        assert inner is not None, f"ERROR: cannot split {tokens} tokens per matrix into chunks"
        tok_extents, tok_strides = [inner, tokens // inner], [1, inner]
    mat = len(tok_extents)  # level of the matrix loop in w0, r0 and r1

    def port(name, direction, extents, strides, offset):
        return {
            'name': name,
            'type': direction,
            'config': {
                'dimensionality': len(extents),
                'extents': extents,
                'address': {'strides': strides, 'offset': offset},
                'schedule': {}
            },
            'vec_in_config': {},
            'vec_out_config': {},
            'vec_constraints': []
        }

    linear_test = {}
    extents, strides = tok_extents + [num_mats], tok_strides + [0]
    linear_test[0] = port('port_w0', Direction.IN, extents, strides, 0)
    linear_test[1] = port('port_w_init', Direction.IN, [num_mats], [0], 0)
    linear_test[2] = port('port_r0', Direction.OUT, extents, strides, POND_DEPTH - num_groups)
    linear_test[3] = port('port_r1', Direction.OUT, extents, strides, 0)
    pw, pwinit, pr0, pr1 = 0, 1, 2, 3
    LT, GT = LFComparisonOperator.LT.value, LFComparisonOperator.GT.value
    linear_test['constraints'] = [
        # r0 token t - num_groups < w0 count: the sum one row up is written
        (pr0, 0, pw, 0, LT, -num_groups),
        # r1 token t < w0 count: the new sum is written
        (pr1, 0, pw, 0, LT, 0),
        # w0 < r1 + POND_DEPTH - 2: never overwrite an unread sum
        (pw, 0, pr1, 0, GT, POND_DEPTH - 2),
        # r0 matrix b < clears done
        (pr0, mat, pwinit, 0, LT, 0),
        # clear b < r1 matrix + 1: matrix b - 1 has been read out
        (pwinit, 0, pr1, mat, GT, 1),
    ]
    return linear_test



def _pond_port(name, direction, extents, strides, offset):
    return {
        'name': name,
        'type': direction,
        'config': {
            'dimensionality': len(extents),
            'extents': extents,
            'address': {'strides': strides, 'offset': offset},
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }


def get_block_absmax_pond(num_groups, block_rows, num_blocks):
    '''
    Pond schedule of one lane of a block |max| along dim -2 (down the rows).

    In each block the lane sees num_groups columns of row r, then of row r + 1,
    for block_rows rows; num_blocks blocks follow each other. Its PE takes the
    |max| of the input and the running |max| of the same column one row up,
    read from this Pond, and writes the result back:

        m[r, g] = abs_max(m[r - 1, g], x[r, g]),   m[-1, g] = 0

    The Pond is a ring of its 32 words. Token t of a block (t = r * num_groups
    + g) is written by port_w0 to word t mod 32; port_r0 (port 2, the tile's
    PE) reads word (t - num_groups) mod 32. port_r1 (port 3, the
    interconnect) reads only the last row, the block maxima, once the block is
    written. The clear (port 1) zeroes the Pond once before each block, so the
    first row reads zeros.

    Constraints (one per read/write pair and direction):
    - r0 reads token t once w0 has written token t - num_groups;
    - r1 reads block b once w0 has finished block b. This compares the block
      counters: at the token level the counter correction for a writer in the
      next block adds the reader's extent (num_groups, not the block length),
      so the last maximum would never be released;
    - r0 starts block b after the clear of block b;
    - the clear of block b waits until r1 has read block b - 1.
    w0 needs no guard: it can only enter the next block after that block's
    clear, which waits for r1. The Pond's input ready is tied high, so r1 must
    not stall for long (its reader should never back-pressure).
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    '''
    POND_DEPTH = 32
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    tokens = num_groups * block_rows
    assert 2 <= num_groups < POND_DEPTH - 2, f"ERROR: {num_groups} column groups do not fit the Pond ring"
    assert 2 <= block_rows and tokens <= MAX_EXTENT, f"ERROR: {tokens} tokens per block exceed the 11-bit extent limit"
    assert 2 <= num_blocks <= MAX_EXTENT, f"ERROR: {num_blocks} blocks must lie in [2, {MAX_EXTENT}]"

    linear_test = {}
    linear_test[0] = _pond_port('port_w0', Direction.IN, [tokens, num_blocks], [1, 0], 0)
    linear_test[1] = _pond_port('port_w_init', Direction.IN, [num_blocks], [0], 0)
    linear_test[2] = _pond_port('port_r0', Direction.OUT, [tokens, num_blocks], [1, 0], POND_DEPTH - num_groups)
    linear_test[3] = _pond_port('port_r1', Direction.OUT, [num_groups, num_blocks], [1, 0], (tokens - num_groups) % POND_DEPTH)
    pw, pwinit, pr0, pr1 = 0, 1, 2, 3
    LT, GT = LFComparisonOperator.LT.value, LFComparisonOperator.GT.value
    linear_test['constraints'] = [
        # r0 token t - num_groups < w0 count: the maximum one row up is written
        (pr0, 0, pw, 0, LT, -num_groups),
        # r1 block b < w0 blocks done: the block maxima are written
        (pr1, 1, pw, 1, LT, 0),
        # r0 block b < clears done
        (pr0, 1, pwinit, 0, LT, 0),
        # clear b < r1 block + 1: block b - 1 has been read out
        (pwinit, 0, pr1, 1, GT, 1),
    ]
    return linear_test


def get_block_replay_pond(num_groups, block_rows, num_blocks):
    '''
    Pond schedule that replays the num_groups values of each block block_rows
    times, in the token order of the block (row-major, num_groups per row).

    port_w0 writes value g of block b to word (g + num_groups * b) mod 32 (a
    ring of 32 / num_groups blocks). port_r0 (port 2, the tile's PE) reads the
    words of block b once per row, after w0 has finished block b. The w0 guard
    keeps a block from overwriting one that r0 has not finished; the Pond's
    input ready is tied high, so the guard only turns an overflow into a hang,
    and the reader must keep up (it runs one row per block_rows tokens of its
    writer's source, so it only holds the latency gap).
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    '''
    POND_DEPTH = 32
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    ring_blocks = POND_DEPTH // num_groups
    assert 2 <= num_groups <= POND_DEPTH // 2, f"ERROR: {num_groups} values per block do not fit two blocks in the Pond"
    assert 2 <= block_rows <= MAX_EXTENT, f"ERROR: {block_rows} rows must lie in [2, {MAX_EXTENT}]"
    assert 2 <= num_blocks <= MAX_EXTENT, f"ERROR: {num_blocks} blocks must lie in [2, {MAX_EXTENT}]"

    linear_test = {}
    linear_test[0] = _pond_port('port_w0', Direction.IN, [num_groups, num_blocks], [1, num_groups], 0)
    linear_test[2] = _pond_port('port_r0', Direction.OUT, [num_groups, block_rows, num_blocks],
                                [1, 0, num_groups], 0)
    pw, pr0 = 0, 2
    LT, GT = LFComparisonOperator.LT.value, LFComparisonOperator.GT.value
    linear_test['constraints'] = [
        # r0 block b < w0 blocks done
        (pr0, 2, pw, 1, LT, 0),
        # w0 block b < r0 block + ring_blocks - 1: never overwrite an unread block
        (pw, 1, pr0, 2, GT, ring_blocks - 1),
    ]
    return linear_test


def get_fifo_pond(input_stream_size, read_port=2, raw_margin=0):
    '''
    Pond schedule for an elastic FIFO.

    Write port 0 stores the stream in order and one read port returns it in
    order: read port 0 (port 2) feeds the PE of the Pond's tile and read port 1
    (port 3) drives the interconnect. ``read_port`` selects 2 or 3.
    A word is read once it was written; ``raw_margin`` m > 0 waits until m
    more were written (RTL simulation reads correctly with 0, also right
    after the write). The last word is read when the writer is done. A
    stream longer than the 11-bit extent limit is counted in chunks of up
    to 512 words, and both guards still compare single words: when the two
    ports are in different chunks, the comparison network adds the gated
    port's chunk extent to the right counter, which is exact because the
    ports never drift a whole chunk apart. (Guards on whole blocks of 2-8
    words cut the usable depth to 21-24 words at 4,096 words, and
    fused_e8m0_quant_single_IO at 64 x 2048 lost words and hung.)
    The writer is held back once it is 30 words ahead, but the
    Pond's input ready is tied high, so a held-back write is lost: the FIFO
    must never fill (its branch must hold fewer than 30 words), and the
    guard only turns an overflow into a hang.
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    '''
    POND_DEPTH = 32
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    if input_stream_size <= MAX_EXTENT:
        extents, strides = [input_stream_size], [1]
    else:
        # Chunks of at most 512 words keep chunk extent + slack inside the
        # 11-bit counters, and a chunk is longer than the slack.
        inner = next((e for e in range(MAX_EXTENT // 2, POND_DEPTH, -1)
                      if input_stream_size % e == 0 and input_stream_size // e <= MAX_EXTENT), None)
        assert inner is not None, f"ERROR: cannot split a {input_stream_size}-word pond FIFO stream into chunks"
        extents, strides = [inner, input_stream_size // inner], [1, inner]
    level = 0

    linear_test = {}
    assert read_port in (2, 3)
    for port, name, direction in ((0, 'port_w0', Direction.IN), (read_port, f'port_r{read_port - 2}', Direction.OUT)):
        linear_test[port] = {
            'name': name,
            'type': direction,
            'config': {
                'dimensionality': len(extents),
                'extents': extents,
                'address': {
                    'strides': strides,
                    'offset': 0
                },
                'schedule': {}
            },
            'vec_in_config': {},
            'vec_out_config': {},
            'vec_constraints': []
        }

    pw = 0
    pr = read_port
    # read count + raw_margin < write count
    raw_constraint = (pr, level, pw, level, LFComparisonOperator.LT.value, raw_margin)
    # write count < read count + slack: stay inside the 32-word pond
    slack = POND_DEPTH - 2
    war_constraint = (pw, level, pr, level, LFComparisonOperator.GT.value, slack)
    linear_test['constraints'] = [raw_constraint, war_constraint]
    return linear_test


def get_single_mem_transpose(in_img_x=64, in_img_y=32):
    '''
    Helper function to create config for mem transpose
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    MEM port mapping: 0: port_w0, 1: port_w1, 2: port_w2, 3: port_r0, 4: port_r1, 5: port_r2
    '''

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': [in_img_x, in_img_y],
            'address': {
                'strides': [1, in_img_x],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [in_img_y, in_img_x],
            'address': {
                'strides': [in_img_x, 1],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3

    raw_scalar_1 = in_img_x
    raw_1 = (port_data_out_0, 1, port_data_in_0, 1, LFComparisonOperator.LT.value, raw_scalar_1)

    linear_test['constraints'] = [raw_1]

    return linear_test


def get_single_mem_slice(in_img_x=32, in_img_y=64):
    '''
    Helper function to create config for slice MEM
    MEM port mapping: 0: port_w0, 1: port_w1, 2: port_w2, 3: port_r0, 4: port_r1, 5: port_r2
    '''

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': [in_img_x, in_img_y],
            'address': {
                'strides': [1, in_img_x],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [in_img_x, in_img_y // 2],
            'address': {
                'strides': [1, in_img_x],
                'offset': in_img_x * (in_img_y // 2)
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3

    raw_scalar_1 = in_img_x
    raw_1 = (port_data_out_0, 1, port_data_in_0, 1, LFComparisonOperator.LT.value, raw_scalar_1)

    linear_test['constraints'] = [raw_1]

    return linear_test


def get_single_mem_stride(in_img_x=32, in_img_y=64, stride=4):
    '''
    Helper function to create config for strided output MEM
    MEM port mapping: 0: port_w0, 1: port_w1, 2: port_w2, 3: port_r0, 4: port_r1, 5: port_r2
    '''

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': [in_img_x // stride, in_img_y],
            'address': {
                'strides': [1, in_img_x // stride],
                'offset': 0
            },
            'schedule': {},
            'filter': {
                'offset': [0],
                'dimensionality': [2],
                'strides': [stride, stride * in_img_x // stride]
            }
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [in_img_x // stride, in_img_y],
            'address': {
                'strides': [1, in_img_x // stride],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3

    raw_scalar_1 = in_img_x // stride
    raw_1 = (port_data_out_0, 1, port_data_in_0, 1, LFComparisonOperator.LT.value, raw_scalar_1)

    linear_test['constraints'] = [raw_1]

    return linear_test


def get_single_mem_line_buffer(buffer_size=28 * 28, num_lines=2):
    '''
    Helper function to create config for line buffer MEM
    MEM port mapping: 0: port_w0, 1: port_w1, 2: port_w2, 3: port_r0, 4: port_r1, 5: port_r2
    '''

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': [buffer_size, num_lines],
            'address': {
                'strides': [1, buffer_size],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [buffer_size, num_lines],
            'address': {
                'strides': [1, buffer_size],
                'offset': -buffer_size
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[4] = {
        'name': 'port_r1',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [buffer_size, num_lines],
            'address': {
                'strides': [1, buffer_size],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3
    port_data_out_1 = 4

    # Only read out data when buffer_size data has been written
    raw_scalar_1 = -buffer_size
    # This means: rd[0] - buffer_size < wr[0]
    raw_1 = (port_data_out_0, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar_1)

    # Should be 0, but configure a magic number 12 to actually contraint read after write. Needs investigation.
    raw_scalar_2 = 12
    raw_2 = (port_data_out_1, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar_2)

    mem_tile_size = 4 * 1024 // 2 # 4KB's word size

    war_scalar_1 = -((mem_tile_size - buffer_size) // buffer_size)
    war_1 = (port_data_in_0, 1, port_data_out_0, 1, LFComparisonOperator.LT.value, war_scalar_1)

    war_scalar_2 = -(mem_tile_size // buffer_size)
    war_2 = (port_data_in_0, 1, port_data_out_1, 1, LFComparisonOperator.LT.value, war_scalar_2)

    linear_test['constraints'] = [raw_1, raw_2, war_1, war_2]

    return linear_test


def get_broadcast_mem(input_stream_size=128, replicate_factor=4, raw_scalar=4):
    '''
    Input: input_stream_size
    Output: repeatedly read each element replicate_factor times
    Replicate factor should be vec_len // glb_i unroll
    MEM port mapping: 0: port_w0, 3: port_r0
    '''
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    # TODO: tile the loop to reduce extent
    assert input_stream_size <= MAX_EXTENT, f"ERROR: input_stream_size has to be less than or equal to {MAX_EXTENT}"
    assert replicate_factor <= MAX_EXTENT, f"ERROR: replicate_factor has to be less than or equal to {MAX_EXTENT}"

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 1,
            'extents': [input_stream_size],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [replicate_factor, input_stream_size],
            'address': {
                'strides': [0, 1],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3

    raw_scalar_1 = raw_scalar
    raw_1 = (port_data_out_0, 1, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar_1)

    linear_test['constraints'] = [raw_1]

    return linear_test


def get_mem_dual_read(input_stream_size=128, row_size=None):
    '''
    Single write port, dual read ports
    '''
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    port_data_in_0 = 0
    port_data_out_0 = 3
    port_data_out_1 = 4

    dim_1 = input_stream_size
    dim_2 = 1
    while dim_1 > MAX_EXTENT:
        assert dim_1 % 2 == 0, f"ERROR: Dim1 always has to be divisible by 2 when increasing dimensionality."
        dim_1 //= 2
        dim_2 *= 2
        assert dim_2 <= MAX_EXTENT, f"ERROR: Cannot map filter mem to two streams using 3D extents with input_stream_size: {input_stream_size}. Higher dimensionality is required."

    if dim_2 > 1:
        input_dimensionality = 2
        input_extents = [dim_1, dim_2]
        input_strides = [1, dim_1]
        output_dimensionality = 2
        output_extents_0 = [dim_1, dim_2]
        output_extents_1 = [dim_1, dim_2]
        output_strides = [1, dim_1]
    else:
        input_dimensionality = 1
        input_extents = [dim_1]
        input_strides = [1]
        output_dimensionality = 1
        output_extents_0 = [dim_1]
        output_extents_1 = [dim_1]
        output_strides = [1]

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': input_dimensionality,
            'extents': input_extents,
            'address': {
                'strides': input_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': output_dimensionality,
            'extents': output_extents_0,
            'address': {
                'strides': output_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[4] = {
        'name': 'port_r1',
        'type': Direction.OUT,
        'config': {
            'dimensionality': output_dimensionality,
            'extents': output_extents_1,
            'address': {
                'strides': output_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3
    port_data_out_1 = 4

    # The scalar has to be 6 to actually contraint read after write
    raw_scalar = 6
    raw_0 = (port_data_out_0, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar)
    raw_1 = (port_data_out_1, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar)

    mem_tile_size = 4 * 1024 // 2 # 4KB's word size
    war_scalar = -mem_tile_size
    war_0 = (port_data_in_0, 0, port_data_out_0, 0, LFComparisonOperator.LT.value, war_scalar)
    war_1 = (port_data_in_0, 0, port_data_out_1, 0, LFComparisonOperator.LT.value, war_scalar)

    linear_test['constraints'] = [raw_0, raw_1, war_0, war_1]

    if row_size is not None:
        # Bound a streaming circular buffer using row/block counters. The
        # default inner counter wraps before reaching the 2048-word capacity,
        # so its WAR comparison cannot protect a slow second reader.
        assert 0 < row_size <= MAX_EXTENT
        assert input_stream_size % row_size == 0
        rows = input_stream_size // row_size
        assert rows <= MAX_EXTENT
        buffered_rows = mem_tile_size // row_size - 2
        assert buffered_rows >= 2
        for port in (port_data_in_0, port_data_out_0, port_data_out_1):
            config = linear_test[port]['config']
            config['dimensionality'] = 2
            config['extents'] = [row_size, rows]
            config['address']['strides'] = [1, row_size]
        # Leave one extra block for pending SRAM writes. A just-completed
        # block can still have its final four words in the write aggregator.
        linear_test['constraints'] = [
            (reader, 1, port_data_in_0, 1, LFComparisonOperator.LT.value, 1)
            for reader in (port_data_out_0, port_data_out_1)
        ] + [
            (port_data_in_0, 1, reader, 1, LFComparisonOperator.LT.value, -buffered_rows)
            for reader in (port_data_out_0, port_data_out_1)
        ]

    return linear_test

def get_mem_fifo(input_stream_size, row_size):
    """Elastic single-reader FIFO with pending-write and circular-buffer guards."""
    application = get_mem_dual_read(input_stream_size, row_size=row_size)
    del application[4]
    application['constraints'] = [c for c in application['constraints']
                                  if c[0] != 4 and c[2] != 4]
    return application


def get_merge_dual_read_mem(single_input_stream_size, row_size):
    """Two writers, two readers of streams merged in four-word blocks.

    Configuration only: use the existing MEM write aggregators, address
    generators and RAW/WAR comparison network. Each writer owns disjoint
    four-word SRAM blocks; both readers visit w0[0:4], w1[0:4], w0[4:8], ... .
    Contiguous reads avoid repeatedly fetching the same SRAM words into the
    existing two-entry read buffer, preserving streaming bandwidth.
    Block counters guard pending writes and both readers across SRAM wraps.
    ``row_size`` counts output words in a scheduling block, not a tensor row.
    """
    capacity = 2048  # 4 KiB, in BF16 words
    max_extent = 1024  # signed 11-bit loop counter
    if row_size < 8 or row_size % 8:
        raise ValueError("Interleave block size must be a positive multiple of 8")
    if single_input_stream_size < 1 or (2 * single_input_stream_size) % row_size:
        raise ValueError("Both input streams must fill complete interleave blocks")
    blocks = 2 * single_input_stream_size // row_size
    buffered_blocks = capacity // row_size - 2
    if row_size > max_extent or blocks > max_extent or buffered_blocks < 2:
        raise ValueError("Interleave schedule exceeds MEM counter or buffer capacity")

    readers = (3, 4)
    # The taped-out loop controller cannot represent an extent of one.
    # For an eight-word block, omit the redundant four-word-group loop.
    writer_block_dim = 1 if row_size == 8 else 2
    application = {}
    for port in (0, 1) + readers:
        writing = port < 3
        extents = ([4, row_size // 8, blocks] if writing
                   else [row_size, blocks])
        strides = ([1, 8, row_size] if writing
                   else [1, row_size])
        if writing and row_size == 8:
            extents = [4, blocks]
            strides = [1, row_size]
        application[port] = {
            'name': f'port_w{port}' if writing else f'port_r{port - 3}',
            'type': Direction.IN if writing else Direction.OUT,
            'config': {
                'dimensionality': len(extents),
                'extents': extents,
                'address': {'strides': strides, 'offset': 4 * port if writing else 0},
                'schedule': {},
            },
            'vec_in_config': {}, 'vec_out_config': {}, 'vec_constraints': [],
        }
    application['constraints'] = [
        (reader, 1, writer, writer_block_dim, LFComparisonOperator.LT.value, 1)
        for reader in readers for writer in (0, 1)
    ] + [
        (writer, writer_block_dim, reader, 1, LFComparisonOperator.LT.value, -buffered_blocks)
        for writer in (0, 1) for reader in readers
    ]
    return application


def get_deinterleave_blocks_mem(input_stream_size, row_size):
    """Split consecutive four-word blocks between two output streams.

    For the 32-to-16 LayerNorm layout this restores exactly the two partial
    sum streams of the original 16-lane reduction, including rounding order.
    After affine, the same schedule scatters into canonical 32-lane stripes.
    """
    if row_size < 8 or row_size % 8:
        raise ValueError("Deinterleave block size must be a positive multiple of 8")
    application = get_mem_dual_read(input_stream_size, row_size=row_size)
    reader_block_dim = 1 if row_size == 8 else 2
    for reader in (3, 4):
        application[reader]['config'].update({
            'dimensionality': reader_block_dim + 1,
            'extents': ([4, input_stream_size // row_size] if row_size == 8
                        else [4, row_size // 8, input_stream_size // row_size]),
            'address': {'strides': ([1, row_size] if row_size == 8
                                    else [1, 8, row_size]),
                        'offset': 4 * (reader - 3)},
        })
    buffered_blocks = 2048 // row_size - 2
    application['constraints'] = [
        (reader, reader_block_dim, 0, 1, LFComparisonOperator.LT.value, 1)
        for reader in (3, 4)
    ] + [
        (0, 1, reader, reader_block_dim, LFComparisonOperator.LT.value, -buffered_blocks)
        for reader in (3, 4)
    ]
    return application


def get_paired_mem_fifo(input_stream_size, row_size):
    """Two independent FIFOs sharing disjoint four-word SRAM blocks.

    Each stream has one writer, one reader, and half of the MEM capacity.
    Dependencies stay within each stream, so a stalled lane does not impose
    a read-after-write dependency on its partner.
    ``row_size`` counts words per stream in each scheduling block.
    """
    application = get_merge_dual_read_mem(input_stream_size, 2 * row_size)
    for reader, writer in ((3, 0), (4, 1)):
        application[reader]['config'] = copy.deepcopy(application[writer]['config'])
    block_dim = application[0]['config']['dimensionality'] - 1
    buffered_blocks = 2048 // (2 * row_size) - 2
    application['constraints'] = [
        (reader, block_dim, writer, block_dim, LFComparisonOperator.LT.value, 1)
        for reader, writer in ((3, 0), (4, 1))
    ] + [
        (writer, block_dim, reader, block_dim, LFComparisonOperator.LT.value, -buffered_blocks)
        for reader, writer in ((3, 0), (4, 1))
    ]
    return application


def get_mem_single_read(input_stream_size=128, rv_stride=1, filter_offset_scalar=0):
    '''
    Single write port, single read port
    '''
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    port_data_in_0 = 0
    port_data_out_0 = 3

    dim_1 = input_stream_size
    dim_2 = 1
    while dim_1 > MAX_EXTENT:
        assert dim_1 % 2 == 0, f"ERROR: Dim1 always has to be divisible by 2 when increasing dimensionality."
        dim_1 //= 2
        dim_2 *= 2
        assert dim_2 <= MAX_EXTENT, f"ERROR: Cannot map filter mem to two streams using 3D extents with input_stream_size: {input_stream_size}. Higher dimensionality is required."

    if dim_2 > 1:
        input_dimensionality = 2
        input_extents = [dim_1, dim_2]
        input_strides = [1, dim_1]
        output_dimensionality = 2
        output_extents_0 = [dim_1, dim_2]
        output_strides = [1, dim_1]
        # Optionally use filter feature
        filter_offset = [filter_offset_scalar]
        filter_dimensionality = [input_dimensionality]
        filter_strides = [rv_stride, dim_1 * rv_stride]
    else:
        input_dimensionality = 1
        input_extents = [dim_1]
        input_strides = [1]
        output_dimensionality = 1
        output_extents_0 = [dim_1]
        output_strides = [1]
        # Optionally use filter feature
        filter_offset = [filter_offset_scalar]
        filter_dimensionality = [input_dimensionality]
        filter_strides = [rv_stride]


    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': input_dimensionality,
            'extents': input_extents,
            'address': {
                'strides': input_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }
    if rv_stride > 1:
        linear_test[0]['config']['filter'] = {
            'offset': filter_offset,
            'dimensionality': filter_dimensionality,
            'strides': filter_strides
        }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': output_dimensionality,
            'extents': output_extents_0,
            'address': {
                'strides': output_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3

    # The scalar has to be 6 to actually contraint read after write
    raw_scalar = 6
    raw_0 = (port_data_out_0, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar)

    mem_tile_size = 4 * 1024 // 2 # 4KB's word size
    war_scalar = -mem_tile_size
    war_0 = (port_data_in_0, 0, port_data_out_0, 0, LFComparisonOperator.LT.value, war_scalar)

    linear_test['constraints'] = [raw_0, war_0]

    return linear_test


def get_mem_line_buffer_dual_port(line_size=64, num_lines=198, offset_r0=-64, offset_r1=-128):
    '''
    Helper function to create line buffer schedule for sliding window reduction
    Has two read ports: one with one line of delay and the other with two lines of delay
    '''

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': [line_size, num_lines],
            'address': {
                'strides': [1, line_size],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    # Port 0 connects to last three PEs with two lines of delay
    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [line_size, num_lines],
            'address': {
                'strides': [1, line_size],
                'offset': offset_r0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    # Port 1 connects to middle three PEs with one line of delay
    linear_test[4] = {
        'name': 'port_r1',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [line_size, num_lines],
            'address': {
                'strides': [1, line_size],
                'offset': offset_r1
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3
    port_data_out_1 = 4
    # Add a magic number 6 to constraint read after write when offset is very small
    raw_scalar_0 = offset_r0 + 6
    raw_scalar_1 = offset_r1 + 6

    raw_0 = (port_data_out_0, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar_0)
    raw_1 = (port_data_out_1, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar_1)

    linear_test['constraints'] = [raw_0, raw_1]

    return linear_test

def get_mem_line_buffer_single_port(line_size=64, num_lines=198, offset_r0=-64):
    '''
    Helper function to create line buffer schedule for sliding window reduction
    Has two read ports: one with one line of delay and the other with two lines of delay
    '''

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': [line_size, num_lines],
            'address': {
                'strides': [1, line_size],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    # Port 0 connects to last three PEs with two lines of delay
    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [line_size, num_lines],
            'address': {
                'strides': [1, line_size],
                'offset': offset_r0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3
    # Add a magic number 6 to constraint read after write when offset is very small
    raw_scalar_0 = offset_r0 + 6

    raw_0 = (port_data_out_0, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar_0)

    linear_test['constraints'] = [raw_0]

    return linear_test

def get_interleave_mem(single_input_stream_size=512, turn=1):
    '''
    Helper function to interleave two data streams like output GLB IOs.
    Input num 0 first, input num 1 second.
    Auto-splits extents when single_input_stream_size exceeds the extent
    counter width, following the same pattern as get_filter_mem_two_streams.
    turn: values read from one input before switching to the other (1, 2 or 4;
    default 1 = element-wise). The inputs sit in alternate SRAM words, and the
    read port refills its word cache only every other cycle, so turn 1 (a new
    word for every value) reads at most 0.5 values per cycle; turn 2 or 4 keeps
    the read at about 1 value per cycle.
    '''
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)
    SIPO_WORD_WIDTH = 4
    assert turn in (1, 2, 4), f"ERROR: interleave turn must be 1, 2 or 4 values, got {turn}"

    port_data_in_0 = 0
    port_data_in_1 = 1
    port_data_out_0 = 3

    dim_1 = single_input_stream_size // SIPO_WORD_WIDTH
    dim_2 = 1
    while dim_1 > MAX_EXTENT:
        assert dim_1 % 2 == 0, f"ERROR: dim_1 always has to be divisible by 2 when increasing dimensionality."
        dim_1 //= 2
        dim_2 *= 2
        assert dim_2 <= MAX_EXTENT, f"ERROR: Cannot map interleave mem with single_input_stream_size: {single_input_stream_size}. Higher dimensionality is required."

    if dim_2 > 1:
        write_dimensionality = 3
        write_extents = [SIPO_WORD_WIDTH, dim_1, dim_2]
        write_strides = [1, 2 * SIPO_WORD_WIDTH, dim_1 * 2 * SIPO_WORD_WIDTH]
        read_dimensionality = 4
        read_extents = [2, SIPO_WORD_WIDTH, dim_1, dim_2]
        read_strides = [SIPO_WORD_WIDTH, 1, 2 * SIPO_WORD_WIDTH, dim_1 * 2 * SIPO_WORD_WIDTH]
        raw_dim_w = 2
        raw_dim_r = 3
    else:
        write_dimensionality = 2
        write_extents = [SIPO_WORD_WIDTH, dim_1]
        write_strides = [1, 2 * SIPO_WORD_WIDTH]
        read_dimensionality = 3
        read_extents = [2, SIPO_WORD_WIDTH, dim_1]
        read_strides = [SIPO_WORD_WIDTH, 1, 2 * SIPO_WORD_WIDTH]
        raw_dim_w = 1
        raw_dim_r = 2

    if turn > 1:
        # turn values of one input, then turn of the other, then the next turn values of each word:
        # (turn, 1), (2, SIPO_WORD_WIDTH), (SIPO_WORD_WIDTH // turn, turn), then the word-pair loops
        turn_levels = [(turn, 1), (2, SIPO_WORD_WIDTH), (SIPO_WORD_WIDTH // turn, turn)]
        turn_levels = [(e, s) for e, s in turn_levels if e > 1]
        outer = list(zip(read_extents[2:], read_strides[2:]))
        read_extents = [e for e, _ in turn_levels + outer]
        read_strides = [s for _, s in turn_levels + outer]
        read_dimensionality = len(read_extents)
        raw_dim_r = read_dimensionality - 1

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': write_dimensionality,
            'extents': write_extents,
            'address': {
                'strides': write_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[1] = {
        'name': 'port_w1',
        'type': Direction.IN,
        'config': {
            'dimensionality': write_dimensionality,
            'extents': write_extents,
            'address': {
                'strides': write_strides,
                'offset': SIPO_WORD_WIDTH
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': read_dimensionality,
            'extents': read_extents,
            'address': {
                'strides': read_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    raw_scalar_0 = 1
    raw_0 = (port_data_out_0, raw_dim_r, port_data_in_0, raw_dim_w, LFComparisonOperator.LT.value, raw_scalar_0)

    raw_scalar_1 = 1
    raw_1 = (port_data_out_0, raw_dim_r, port_data_in_1, raw_dim_w, LFComparisonOperator.LT.value, raw_scalar_1)

    war_0 = (port_data_in_0, raw_dim_w, port_data_out_0, raw_dim_r, LFComparisonOperator.GT.value, 128)
    war_1 = (port_data_in_1, raw_dim_w, port_data_out_0, raw_dim_r, LFComparisonOperator.GT.value, 128)

    linear_test['constraints'] = [raw_0, raw_1, war_0, war_1]

    return linear_test


def get_filter_scale_mem(img_size, total_channels, mu_OC=32, packed=True):
    '''
    Helper function to align scale data to width of two GLB tiles
    Basicaly img_size should be multiple of 32
    '''

    if packed:
        # Inner // 2 because of data packing
        # Outer // 2 because of block size is 2 * mu_OC
        extents = [img_size // 2, total_channels // mu_OC // 2]
        address_strides = [1, img_size // 2]
        filter_offset = [img_size + 1]
        filter_strides = [2, img_size * 2]
    else:
        extents = [img_size, total_channels // mu_OC // 2]
        address_strides = [1, img_size]
        filter_offset = [img_size]
        filter_strides = [1, img_size * 2]

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': extents,
            'address': {
                'strides': address_strides,
                'offset': 0
            },
            'schedule': {},
            'filter': {
                'offset': filter_offset,
                'dimensionality': [2],
                'strides': filter_strides
            }
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': extents,
            'address': {
                'strides': address_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3
    raw_scalar_0 = 12
    raw_0 = (port_data_out_0, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar_0)

    linear_test['constraints'] = [raw_0]

    return linear_test


def get_filter_mem_two_streams(input_stream_size=512, row_size=None):
    '''
    Helper function to create config for filter mem to demux reduction results into two interleaving streams
    '''

    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10
    CYCLES_PER_DATA = 2
    port_data_in_0 = 0
    port_data_out_0 = 3
    port_data_out_1 = 4

    stream_is_even = (input_stream_size % CYCLES_PER_DATA == 0)

    dim_1 = input_stream_size // CYCLES_PER_DATA
    dim_2 = 1
    while dim_1 > MAX_EXTENT:
        assert dim_1 % 2 == 0, f"ERROR: Dim1 always has to be divisible by 2 when increasing dimensionality."
        dim_1 //= 2
        dim_2 *= 2
        assert dim_2 <= MAX_EXTENT, f"ERROR: Cannot map filter mem to two streams using 3D extents with input_stream_size: {input_stream_size}. Higher dimensionality is required."

    if dim_2 > 1:
        input_dimensionality = 3
        input_extents = [CYCLES_PER_DATA, dim_1, dim_2]
        input_strides = [1, CYCLES_PER_DATA, dim_1]
        output_dimensionality = 2
        output_extents_0 = [dim_1, dim_2]
        output_extents_1 = [dim_1, dim_2]
        output_strides = [CYCLES_PER_DATA, dim_1]
    else:
        if stream_is_even:
            input_dimensionality = 2
            input_extents = [CYCLES_PER_DATA, dim_1]
            input_strides = [1, CYCLES_PER_DATA]
            output_dimensionality = 1
            output_extents_0 = [dim_1]
            output_extents_1 = [dim_1]
            output_strides = [CYCLES_PER_DATA]
        else:
            input_dimensionality = 2
            input_extents = [CYCLES_PER_DATA, dim_1]
            input_strides = [1, CYCLES_PER_DATA]
            output_dimensionality = 1
            output_extents_0 = [dim_1 + 1]
            output_extents_1 = [dim_1]
            output_strides = [CYCLES_PER_DATA]

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': input_dimensionality,
            'extents': input_extents,
            'address': {
                'strides': input_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': output_dimensionality,
            'extents': output_extents_0,
            'address': {
                'strides': output_strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[4] = {
        'name': 'port_r1',
        'type': Direction.OUT,
        'config': {
            'dimensionality': output_dimensionality,
            'extents': output_extents_1,
            'address': {
                'strides': output_strides,
                'offset': 1
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3
    port_data_out_1 = 4

    # The scalar has to be a magic number 3 to actually contraint read after write...
    raw_0 = (port_data_out_0, 0, port_data_in_0, 1, LFComparisonOperator.LT.value, 3)
    raw_1 = (port_data_out_1, 0, port_data_in_0, 1, LFComparisonOperator.LT.value, 3)

    linear_test['constraints'] = [raw_0, raw_1]
    if row_size is not None:
        # Keep reduction blocks contiguous and protect unread partial sums.
        assert 0 < row_size <= MAX_EXTENT and row_size % 2 == 0
        assert input_stream_size % row_size == 0
        rows = input_stream_size // row_size
        assert rows <= MAX_EXTENT
        buffered_rows = 2048 // row_size - 2
        assert buffered_rows >= 2
        linear_test[0]['config'].update(
            dimensionality=3, extents=[2, row_size // 2, rows],
            address={'strides': [1, 2, row_size], 'offset': 0})
        for reader, offset in ((3, 0), (4, 1)):
            linear_test[reader]['config'].update(
                dimensionality=2, extents=[row_size // 2, rows],
                address={'strides': [2, row_size], 'offset': offset})
        # As above, reserve one complete block for pending SRAM writes.
        linear_test['constraints'] = [
            (reader, 1, 0, 2, LFComparisonOperator.LT.value, 1)
            for reader in (3, 4)
        ] + [
            (0, 2, reader, 1, LFComparisonOperator.LT.value, -buffered_rows)
            for reader in (3, 4)
        ]
    return linear_test

def get_filter_mem_transpose(
        X: int,
        Y: int,
        output_glb_bank_idx: int,
        lane_idx_within_bank: int,
        unroll: int=32,
    ):
    '''
    Helper function to create config for filter mem to transpose a 2D tensor
    '''
    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 1,
            'extents': [X * Y // unroll // unroll],
            'address': {
                'strides': [1],
                'offset': 0
            },
            'schedule': {},
            'filter': {
                'offset': [4 * output_glb_bank_idx + lane_idx_within_bank],
                'dimensionality': [1],
                'strides': [unroll]
            }
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 1,
            'extents': [X * Y // unroll // unroll],
            'address': {
                'strides': [1],
                'offset': 0
            },
            "schedule": {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3

    # The scalar has to be a magic number 12 to actually contraint read after write.
    raw_scalar = 12
    raw = (port_data_out_0, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar)

    linear_test['constraints'] = [raw]

    return linear_test

def get_demosaic_rgb_stencil_mem_single_port(
        in_line_size=126,
        in_num_lines=94,
        out_line_size=129,
        out_num_lines=96,
        filter_offset_scalar=130,
        address_offset_scalar=-259,
        raw_scalar_guard_offset=1
    ):
    '''
    Helper function to create config for demosaic RGB stencil MEM
    '''
    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': [in_line_size, in_num_lines],
            'address': {
                'strides': [1, out_line_size],
                'offset': 0
            },
            'schedule': {},
            'filter': {
                'offset': [filter_offset_scalar],
                'dimensionality': [2],
                'strides': [1, out_line_size]
            }
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [out_line_size, out_num_lines],
            'address': {
                'strides': [1, out_line_size],
                'offset': address_offset_scalar
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3

    # Add conservative raw_scalar to guard raw constraints
    # Can be released if run at full rate with path balancing
    raw_scalar_0 = address_offset_scalar // out_line_size + raw_scalar_guard_offset
    raw_0 = (port_data_out_0, 1, port_data_in_0, 1, LFComparisonOperator.LT.value, raw_scalar_0)

    linear_test['constraints'] = [raw_0]

    return linear_test

def get_static_cycle_dma_emulator_mem(
    cycle_start_addr=0,
    cycle_stride_outer=1,
    input_extent_inner=33,
    input_extent_outer=48
):
    '''
    Helper function to create config to emulate static load DMA's cycle controller configs.
    It assumes a 2D configuration with inner cycle stride of 1 and outer cycle stride > 1.
    It generates extra dummy data between outer loop to achieve padding effect.
    So output extent will be larger than input extent.
    '''
    linear_test = {}

    linear_test[0] = linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 2,
            'extents': [input_extent_inner, input_extent_outer],
            'address': {
                'strides': [1, input_extent_inner + cycle_stride_outer - 1],
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[3] = {
        'name': 'port_r0',
        'type': Direction.OUT,
        'config': {
            'dimensionality': 2,
            'extents': [input_extent_inner + cycle_stride_outer - 1, input_extent_outer],
            'address': {
                'strides': [1, input_extent_inner + cycle_stride_outer - 1],
                'offset': -cycle_start_addr
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3

    raw_scalar_0 = 0
    raw_0 = (port_data_out_0, 1, port_data_in_0, 1, LFComparisonOperator.LT.value, raw_scalar_0)

    linear_test['constraints'] = [raw_0]

    return linear_test

def get_path_balancing_pond(balance_length=2, interconnect_fifo_depth=2, total_stream_length=4096, pe_to_pond=True):
    '''
    Helper function to create config for pond behaving as a chain of interconnect FIFOs for path balancing
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    MEM port mapping: 0: port_w0, 1: port_w1, 2: port_r0, 3: port_r1
    '''
    POND_DEPTH = 32
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)  # Counter is signed so max extent is 2^10

    def unique_factors_skip_one(n):
        factors = set()

        i = 1
        while i * i <= n:
            if n % i == 0:
                if i != 1:
                    factors.add(i)
                if n // i != 1:
                    factors.add(n // i)
            i += 1

        return sorted(factors)

    total_fifo_depth = balance_length * interconnect_fifo_depth
    if balance_length > POND_DEPTH:
    # if total_fifo_depth > POND_DEPTH:
        print(f"\033[91mERROR: balance_length {balance_length} is too large to be balanced by a single pond\033[0m")
        assert False

    assert balance_length >= 1, f"ERROR: balance_length has to be at least 1"


    if balance_length > 1:
        assert total_stream_length % balance_length == 0, f"ERROR: total_stream_length has to be divisible by balance_length"
        dim1 = total_stream_length // balance_length
        dim2 = 1
        while dim1 > MAX_EXTENT:
            divisors_to_try = unique_factors_skip_one(dim1)
            # Use smallest divisor >= 2 to keep dim2 small
            divisor = divisors_to_try[0]

            # assert dim1 % 2 == 0, f"ERROR: Dim1 always has to be divisible by 2 when increasing dimensionality."
            assert dim1 % divisor == 0, f"ERROR: Dim1 has to be divisible by divisor {divisor} when increasing dimensionality."
            dim1 //= divisor
            dim2 *= divisor
            assert dim2 <= MAX_EXTENT, f"ERROR: Cannot map path balancing pond using 3D extents with balance_length: {balance_length}, total_stream_length: {total_stream_length}. Higher dimensionality is required."

        if dim2 > 1:
            dimensionality = 3
            extents = [balance_length, dim1, dim2]
            strides = [1, balance_length, dim1]
        else:
            dimensionality = 2
            extents = [balance_length, dim1]
            strides = [1, balance_length]
    else:
        assert total_stream_length <= MAX_EXTENT , f"ERROR: total_stream_length exceeds max extent for 1D pond configuration."
        dimensionality = 1
        extents = [total_stream_length]
        strides = [1]

    port_data_in_0 = 0

    # If PE to pond, need to use pond_output_num1 to drive output onto the swtichbox interconnect
    if pe_to_pond:
        port_data_out_0 = 3
    else:
        port_data_out_0 = 2

    linear_test = {}

    linear_test[port_data_in_0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': dimensionality,
            'extents': extents,
            'address': {
                'strides': strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    linear_test[port_data_out_0] = {
        'name': 'port_r1',
        'type': Direction.OUT,
        'config': {
            'dimensionality': dimensionality,
            'extents': extents,
            'address': {
                'strides': strides,
                'offset': 0
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    # Attempt to keep wr_ptr "balance_lengths" ahead of rd_ptr.
    # NOTE: Since this constraint is on dim1, in reality, the distance between wr_ptr and rd_ptr is [1, ~2*balance_length)
    # The "avg. behavior" is wr_ptr is balance_length ahead of rd_ptr, but it can be as low as 1
    if balance_length > 1:
        raw_1 = (port_data_out_0, 1, port_data_in_0, 1, LFComparisonOperator.LT.value, 0)
    else:
        raw_1 = (port_data_out_0, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, 0)

    # Cannot write more than "total_fifo_depth" ahead of read ("FIFOs" are full)
    war_scalar_1 = total_fifo_depth
    if balance_length > 1:
        two_dim_war_scalar_1 = math.floor(POND_DEPTH / balance_length)
        # war_1 = (port_data_in_0, 1, port_data_out_0, 1, LFComparisonOperator.GT.value, 3)
        war_1 = (port_data_in_0, 1, port_data_out_0, 1, LFComparisonOperator.GT.value, two_dim_war_scalar_1)
    else:
        # war_1 = (port_data_in_0, 0, port_data_out_0, 0, LFComparisonOperator.GT.value, 3)
        # war_1 = (port_data_in_0, 0, port_data_out_0, 0, LFComparisonOperator.GT.value, war_scalar_1 + 1)
        war_1 = (port_data_in_0, 0, port_data_out_0, 0, LFComparisonOperator.GT.value, 31)

    # linear_test['constraints'] = [raw_1, war_1]
    linear_test['constraints'] = [raw_1]

    return linear_test
