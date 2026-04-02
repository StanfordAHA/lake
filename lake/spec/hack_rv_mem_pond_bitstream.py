from lake.utils.spec_enum import Direction, LFComparisonOperator

import os
import math
import json

APPS_NEEDING_HACKS = [
    "scalar_reduction_fp",
    "vector_reduction_fp",
    "scalar_max_fp",
    "stable_softmax_pass1_fp",
    "stable_softmax_pass3_fp",
    "layer_norm_pass1_fp",
    "layer_norm_pass2_fp",
    "rms_norm_pass1_fp",
    "gelu_pass1_mu_input_fp",
    "gelu_pass2_fp",
    "add_gelu_pass1_mu_input_fp",
    "add_gelu_pass2_fp",
    "scalar_avg_fp",
    "mem_transpose_test",
    "mem_slice_test",
    "mem_filter_test",
    "avgpool_layer_fp",
    "mat_vec_mul_fp",
    "get_apply_e8m0_scale_fp",
    "get_e8m0_scale_tree_mu_input",
    "get_e8m0_scale_accum_gb_input",
    "maxpooling_dense_rv_fp",
    "maxpooling_dense_rv_mem_buf_fp",
    "fully_connected_layer_fp",
    "tanh_fp",
    "camera_pipeline_2x2_dense_rv",
    "pe_mem_flush_test",
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

    elif test_name in ["scalar_reduction_fp", "scalar_max_fp", "scalar_avg_fp"]:
        # Only have one Pond
        # "HALIDE_GEN_ARGS" example: "vec_width=256 vec_height=2 glb_i=8 glb_o=1 tree_stages=3"
        vec_len = int(halide_gen_args_dict['vec_width']) * int(halide_gen_args_dict['vec_height'])
        num_partial_reduction = vec_len // int(halide_gen_args_dict['glb_i'])
        rv_config = get_accum_pond(num_partial_reduction=num_partial_reduction,
                                   num_output_pixels=1)

    elif test_name in ["vector_reduction_fp", "stable_softmax_pass1_fp"]:
        # "HALIDE_GEN_ARGS" example: "vec_width=256 vec_height=2 glb_i=8 glb_o=1 tree_stages=3"
        vec_len = int(halide_gen_args_dict['vec_width'])
        num_vecs = int(halide_gen_args_dict['vec_height'])
        num_partial_reduction = vec_len // int(halide_gen_args_dict['glb_i'])
        assert num_partial_reduction % 2 == 0, f"ERROR: num_partial_reduction has to be even for two ponds"
        # Configure filter mem to demux reduction results into two ponds
        if "filter_mem" in node_name:
            rv_config = get_filter_mem_two_streams(input_stream_size=num_partial_reduction * num_vecs)
        # Configure accum pond, each handling half of the reduction results
        elif "accum_pond" in node_name:
            rv_config = get_vec_accum_pond(num_partial_reduction=num_partial_reduction // 2, num_output_pixels=num_vecs)
        else:
            raise ValueError(f"Invalid node name: {node_name}")

    elif test_name == "mem_transpose_test":
        # Only have one MEM
        rv_config = get_single_mem_transpose(
            in_img_x=int(halide_gen_args_dict["in_img_x"]),
            in_img_y=int(halide_gen_args_dict["in_img_y"])
        )

    elif test_name == "mem_slice_test":
        # Only have one MEM
        rv_config = get_single_mem_slice(
            in_img_x=int(halide_gen_args_dict["in_img_x"]),
            in_img_y=int(halide_gen_args_dict["in_img_y"])
        )

    elif test_name == "mem_filter_test":
        # Only have one MEM
        rv_config = get_single_mem_stride(
            in_img_x=int(halide_gen_args_dict["in_img_x"]),
            in_img_y=int(halide_gen_args_dict["in_img_y"]),
            stride=int(halide_gen_args_dict["write_stride"])
        )

    elif test_name == "avgpool_layer_fp":
        # Have accum ponds and output mems
        print(f"configure node_name: {node_name}")

        # Configure accum pond
        # input port: num_0
        # output port: update: num_0, spill: num_1
        avgpool_kernel_reduction = int(halide_gen_args_dict["in_img"]) * int(halide_gen_args_dict["in_img"])
        num_out_channels_per_lane = int(halide_gen_args_dict["n_ic"]) // int(halide_gen_args_dict["glb_i"])
        assert avgpool_kernel_reduction % 2 == 0, f"ERROR: avgpool_kernel_reduction has to be even for two ponds"
        # Configure filter mem to demux reduction results into two ponds
        if "filter_mem" in node_name:
            rv_config = get_filter_mem_two_streams(input_stream_size=avgpool_kernel_reduction * num_out_channels_per_lane)
        elif "accum_pond" in node_name:
            rv_config = get_vec_accum_pond(num_partial_reduction=avgpool_kernel_reduction // 2, num_output_pixels=num_out_channels_per_lane)
        else:
            raise ValueError(f"Invalid node name: {node_name}")

    elif test_name == "mat_vec_mul_fp":
        # Have accum ponds
        print(f"configure node_name: {node_name}")
        matrix_width = int(halide_gen_args_dict['matrix_width'])
        matrix_height = int(halide_gen_args_dict['matrix_height'])
        num_partial_reduction = matrix_width // int(halide_gen_args_dict['glb_i'])
        assert num_partial_reduction % 2 == 0, f"ERROR: num_partial_reduction has to be even for two ponds"
        # Configure filter mem to demux reduction results into two ponds
        if "filter_mem" in node_name:
            rv_config = get_filter_mem_two_streams(input_stream_size=num_partial_reduction * matrix_height)
        # Configure accum pond, each handling half of the reduction results
        elif "accum_pond" in node_name:
            rv_config = get_vec_accum_pond(num_partial_reduction=num_partial_reduction // 2, num_output_pixels=matrix_height)
        else:
            raise ValueError(f"Invalid node name: {node_name}")

    elif test_name == "get_apply_e8m0_scale_fp":
        # Configure mem tiles to buffer 32 channels of all pixels
        # vec_height is pixels per channel and vec_width is total number of channels
        img_size = int(halide_gen_args_dict["vec_height"])
        total_channels = int(halide_gen_args_dict["vec_width"])
        mu_OC = int(halide_gen_args_dict["mu_i"])
        print(f"configure node_name: {node_name}")
        if "mem_mu2tree" in node_name:
            rv_config = get_single_mem_line_buffer(
                buffer_size=img_size,
                num_lines=total_channels // mu_OC
            )
        elif "mem_quantized_output_pair" in node_name:
            # // 2 because of data packing and x2 because of bogus data
            rv_config = get_interleave_mem(single_input_stream_size=img_size * total_channels // mu_OC // 2 * 2)
        elif "mem_scale_output_broadcast" in node_name:
            # Filter scale to only stream valid scales to GLB output IO
            rv_config = get_filter_scale_mem(img_size=img_size, total_channels=total_channels, mu_OC=mu_OC)

    elif test_name == "get_e8m0_scale_tree_mu_input":
        # Configure mem tiles to buffer 32 channels of all pixels
        # vec_height is pixels per channel and vec_width is total number of channels
        img_size = int(halide_gen_args_dict["vec_height"])
        total_channels = int(halide_gen_args_dict["vec_width"])
        mu_OC = int(halide_gen_args_dict["mu_i"])
        print(f"configure node_name: {node_name}")
        if "mem_mu2tree" in node_name:
            rv_config = get_single_mem_line_buffer(
                buffer_size=img_size,
                num_lines=total_channels // mu_OC
            )
        elif "mem_scale_filter" in node_name:
            # Filter scale to only stream valid scales to GLB output IO
            rv_config = get_filter_scale_mem(img_size=img_size, total_channels=total_channels, mu_OC=mu_OC, packed=False)
        else:
            raise ValueError(f"Invalid node name: {node_name}")

    elif test_name == "get_e8m0_scale_accum_gb_input":
        print(f"configure node_name: {node_name}")
        # Configure filter mem and accum pond
        # Configure accum pond
        # input port: num_0
        # output port: update: num_0, spill: num_1
        head_dim = int(halide_gen_args_dict["head_dim"])
        seq_heads_prod = int(halide_gen_args_dict["seq_heads_prod"])
        block_size = 64
        num_out_scales_per_lane = seq_heads_prod // block_size * head_dim // int(halide_gen_args_dict["glb_i"])
        assert block_size % 2 == 0, f"ERROR: num_partial_reduction has to be even for two ponds"
        # Configure filter mem to demux reduction results into two ponds
        if "filter_mem" in node_name:
            rv_config = get_filter_mem_two_streams(input_stream_size=block_size * num_out_scales_per_lane)
        elif "accum_pond" in node_name:
            rv_config = get_vec_accum_pond(num_partial_reduction=block_size // 2, num_output_pixels=num_out_scales_per_lane)
        else:
            raise ValueError(f"Invalid node name: {node_name}")

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

    elif test_name == "fully_connected_layer_fp":
        # Have accum ponds
        print(f"configure node_name: {node_name}")
        matrix_width = int(halide_gen_args_dict['matrix_width'])
        matrix_height = int(halide_gen_args_dict['matrix_height'])
        num_partial_reduction = matrix_width // int(halide_gen_args_dict['glb_i'])
        # Configure filter mem to demux reduction results into two ponds
        if "filter_mem" in node_name:
            rv_config = get_filter_mem_two_streams(input_stream_size=num_partial_reduction * matrix_height)
        # Configure accum pond, each handling half of the reduction results
        elif "accum_pond" in node_name:
            rv_config = get_vec_accum_pond(num_partial_reduction=num_partial_reduction // 2, num_output_pixels=matrix_height)
        else:
            raise ValueError(f"Invalid node name: {node_name}")

    elif test_name in ["stable_softmax_pass3_fp", "layer_norm_pass1_fp", "layer_norm_pass2_fp", "rms_norm_pass1_fp"]:
        print(f"configure node_name: {node_name}")
        vec_len = int(halide_gen_args_dict['vec_width'])
        num_vecs = int(halide_gen_args_dict['vec_height'])
        glb_i = int(halide_gen_args_dict['glb_i'])
        num_partial_reduction = vec_len // glb_i
        inputs_per_lane = vec_len * num_vecs // glb_i
        assert num_partial_reduction % 2 == 0, f"ERROR: num_partial_reduction has to be even for two ponds"
        # Category 1: filter mem
        if "filter_mem" in node_name:
            rv_config = get_filter_mem_two_streams(input_stream_size=num_partial_reduction * num_vecs)
        # Category 2: accum pond
        elif "accum_pond" in node_name:
            rv_config = get_vec_accum_pond(num_partial_reduction=num_partial_reduction // 2, num_output_pixels=num_vecs)
        # Category 3: 1/sum buffer mem
        elif "output_cgra_stencil" in node_name:
            if test_name in ["layer_norm_pass2_fp", "rms_norm_pass1_fp"]:
                raw_scalar = 6
            else:
                raw_scalar = 4
            rv_config = get_broadcast_mem(input_stream_size=num_vecs, replicate_factor=vec_len // glb_i, raw_scalar=raw_scalar)
        # Category 4: input buffer mem
        elif "tile_input_stencil" in node_name:
            rv_config = get_mem_dual_read(input_stream_size=inputs_per_lane)
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

    elif test_name in ["gelu_pass1_mu_input_fp", "gelu_pass2_fp", "add_gelu_pass1_mu_input_fp", "add_gelu_pass2_fp"]:
        print(f"configure node_name: {node_name}")
        vec_len = int(halide_gen_args_dict['vec_width'])
        num_vecs = int(halide_gen_args_dict['vec_height'])
        if "input_buffer_mem" in node_name:
            if test_name in ["gelu_pass1_mu_input_fp", "add_gelu_pass1_mu_input_fp"]:
                mu_i = int(halide_gen_args_dict['mu_i'])
                rv_config = get_mem_dual_read(input_stream_size=vec_len * num_vecs // mu_i)
            elif test_name in ["gelu_pass2_fp", "add_gelu_pass2_fp"]:
                glb_i = int(halide_gen_args_dict['glb_i'])
                rv_config = get_mem_dual_read(input_stream_size=vec_len * num_vecs // glb_i)
        elif "mu_buffer_mem" in node_name:
            if test_name in ["add_gelu_pass1_mu_input_fp"]:
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

    elif test_name == "pe_mem_flush_test":
        print(f"configure node_name: {node_name}")
        input_width = int(halide_gen_args_dict["input_width"])
        input_height = int(halide_gen_args_dict["input_height"])
        unroll = int(halide_gen_args_dict["myunroll"])
        stream_size_per_lane = input_width * input_height // unroll
        assert stream_size_per_lane * 2 // 1024 <= 4, f"ERROR: stream_size_per_lane {stream_size_per_lane} is too large to be mapped to 4KB's MEM tile size"
        rv_config = get_mem_single_read(input_stream_size=stream_size_per_lane)

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


def get_mem_dual_read(input_stream_size=128):
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

    return linear_test

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

def get_interleave_mem(single_input_stream_size=512):
    '''
    Helper function to interleave two data streams like output GLB IOs.
    Input num 0 first, input num 1 second.
    Auto-splits extents when single_input_stream_size exceeds the extent
    counter width, following the same pattern as get_filter_mem_two_streams.
    '''
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH - 1)
    SIPO_WORD_WIDTH = 4

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


def get_filter_mem_two_streams(input_stream_size=512):
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
