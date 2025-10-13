from lake.utils.spec_enum import Direction, LFComparisonOperator

import os
import math
import json

APPS_NEEDING_HACKS = [
    "scalar_reduction_fp",
    "vector_reduction_fp",
    "scalar_max_fp",
    "stable_softmax_pass2_fp",
    "scalar_avg_fp",
    "layer_norm_pass2_fp",
    "mem_transpose_test",
    "mem_slice_test",
    "mem_filter_test",
    "avgpool_layer_fp",
    "mat_vec_mul_fp",
    "get_apply_e8m0_scale_fp",
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

    if test_name in ["scalar_reduction_fp", "scalar_max_fp", "stable_softmax_pass2_fp", "scalar_avg_fp", "layer_norm_pass2_fp"]:
        # Only have one Pond
        # "HALIDE_GEN_ARGS" example: "vec_width=256 vec_height=2 glb_i=8 glb_o=1 tree_stages=3"
        vec_len = int(halide_gen_args_dict['vec_width']) * int(halide_gen_args_dict['vec_height'])
        num_partial_reduction = vec_len // int(halide_gen_args_dict['glb_i'])
        rv_config = get_accum_pond(num_partial_reduction=num_partial_reduction,
                                   num_output_pixels=1)

    elif test_name == "vector_reduction_fp":
        # Only have one Pond
        # "HALIDE_GEN_ARGS" example: "vec_width=256 vec_height=2 glb_i=8 glb_o=1 tree_stages=3"
        vec_len = int(halide_gen_args_dict['vec_width'])
        num_vecs = int(halide_gen_args_dict['vec_height'])
        num_partial_reduction = vec_len // int(halide_gen_args_dict['glb_i'])
        rv_config = get_vec_accum_pond(num_partial_reduction=num_partial_reduction,
                                   num_output_pixels=num_vecs)

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
        if "output_cgra_stencil" in node_name:
            avgpool_kernel_reduction = int(halide_gen_args_dict["in_img"]) * int(halide_gen_args_dict["in_img"])
            num_out_channels_per_lane = int(halide_gen_args_dict["n_ic"]) // int(halide_gen_args_dict["glb_i"])
            rv_config = get_vec_accum_pond(num_partial_reduction=avgpool_kernel_reduction, num_output_pixels=num_out_channels_per_lane)

    elif test_name == "mat_vec_mul_fp":
        # Have accum ponds
        print(f"configure node_name: {node_name}")
        matrix_width = int(halide_gen_args_dict['matrix_width'])
        matrix_height = int(halide_gen_args_dict['matrix_height'])
        num_partial_reduction = matrix_width // int(halide_gen_args_dict['glb_i'])
        rv_config = get_vec_accum_pond(num_partial_reduction=num_partial_reduction,
                                   num_output_pixels=matrix_height)

    elif test_name == "get_apply_e8m0_scale_fp":
        # Configure mem tiles to buffer 32 channels of all pixels
        rv_config = get_single_mem_line_buffer(
            in_size=int(halide_gen_args_dict["vec_height"])
        )

    # Global hack for path balancing with ponds
    elif "_path_balance_pond" in node_name:
        pe_id = node_name.split("_path_balance_pond")[0]
        app_path_balancing_json_file = f"/aha/Halide-to-Hardware/apps/hardware_benchmarks/apps/{test_name}/bin/path_balancing.json"
        assert os.path.exists(app_path_balancing_json_file), f"Cannot find path balancing json file: {app_path_balancing_json_file}"
        with open(app_path_balancing_json_file, "r") as f:
            path_balancing_metadata = json.load(f)

        balance_length = path_balancing_metadata["balance_lengths"][pe_id]
        total_stream_length = path_balancing_metadata["total_stream_lengths"][pe_id]
        print(f"\033[93mINFO: Adding path balancing pond for PE {pe_id} with balance_length: {balance_length}, total_stream_length: {total_stream_length}\033[0m")
        rv_config = get_path_balancing_pond(balance_length=balance_length, total_stream_length=total_stream_length)

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

    pr1_raw_idx = 0
    pw1_raw_idx = 0
    raw_r1_comp = LFComparisonOperator.LT.value
    # Allows the reads to start early to read out the initial zero value in pond
    raw_r1_scalar = -1
    raw_r1_constraint = (pr1, pr1_raw_idx, pw, pw1_raw_idx, raw_r1_comp, raw_r1_scalar)

    pr2_raw_idx = 0
    pw2_raw_idx = 0
    raw_r2_comp = LFComparisonOperator.LT.value
    raw_r2_scalar = num_partial_reduction
    raw_r2_constraint = (pr2, pr2_raw_idx, pw, pw2_raw_idx, raw_r2_comp, raw_r2_scalar)

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


def get_single_mem_line_buffer(in_size=784):
    '''
    Helper function to create config for line buffer MEM
    MEM port mapping: 0: port_w0, 1: port_w1, 2: port_w2, 3: port_r0, 4: port_r1, 5: port_r2
    '''

    linear_test = {}

    linear_test[0] = {
        'name': 'port_w0',
        'type': Direction.IN,
        'config': {
            'dimensionality': 1,
            'extents': [in_size],
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
            'dimensionality': 1,
            'extents': [in_size * 2],
            'address': {
                'strides': [1],
                'offset': -in_size
            },
            'schedule': {}
        },
        'vec_in_config': {},
        'vec_out_config': {},
        'vec_constraints': []
    }

    port_data_in_0 = 0
    port_data_out_0 = 3

    raw_scalar_1 = -in_size
    raw_1 = (port_data_out_0, 0, port_data_in_0, 0, LFComparisonOperator.LT.value, raw_scalar_1)

    linear_test['constraints'] = [raw_1]

    return linear_test



def get_path_balancing_pond(balance_length=2, interconnect_fifo_depth=2, total_stream_length=4096):
    '''
    Helper function to create config for pond behaving as a chain of interconnect FIFOs for path balancing
    Pond port mapping: 0: port_w0, 1: port_init (clear memory) 2: port_r0, 3: port_r1
    MEM port mapping: 0: port_w0, 1: port_w1, 2: port_r0, 3: port_r1
    '''
    POND_DEPTH = 32
    EXTENT_COUNTER_WIDTH = 11
    MAX_EXTENT = 2**(EXTENT_COUNTER_WIDTH-1)  # Counter is signed so max extent is 2^10

    total_fifo_depth = balance_length * interconnect_fifo_depth
    if total_fifo_depth > POND_DEPTH:
        print(f"\033[91mERROR: balance_length {balance_length} is too large to be balanced by a single pond\033[0m")
        assert False

    assert balance_length >= 1, f"ERROR: balance_length has to be at least 1"

    # TODO: If this doesn't work across the board, track min. balance length in path_balancing.json and use that to make the decision
    assert total_stream_length % balance_length == 0, f"ERROR: total_stream_length has to be divisible by balance_length"
    dim1 = total_stream_length // balance_length
    dim2 = 1
    while dim1 > MAX_EXTENT:
        assert dim1 % 2 == 0, f"ERROR: Dim1 always has to be divisible by 2 when increasing dimensionality."
        dim1 //= 2
        dim2 *= 2
        assert dim2 <= MAX_EXTENT, f"ERROR: Cannot map path balancing pond using 3D extents with balance_length: {balance_length}, total_stream_length: {total_stream_length}. Higher dimensionality is required."

    if dim2 > 1:
        dimensionality = 3
        extents = [balance_length, dim1, dim2]
        strides = [1, balance_length, dim1]
    else:
        dimensionality = 2
        extents = [balance_length, dim1]
        strides = [1, balance_length]


    linear_test = {}

    linear_test[0] = {
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


    linear_test[3] = {
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

    port_data_in_0 = 0
    port_data_out_0 = 3

    # TODO: Need to double check these constraints
    # Attempt to keep wr_ptr "balance_lengths" ahead of rd_ptr.
    # NOTE: Since this constraint is on dim1, in reality, the distance between wr_ptr and rd_ptr is [1, ~2*balance_length)
    raw_1 = (port_data_out_0, 1, port_data_in_0, 1, LFComparisonOperator.LT.value, 0)

    # Cannot write more than "total_fifo_depth" ahead of read ("FIFOs" are full)
    war_scalar_1 = total_fifo_depth
    war_1 = (port_data_in_0, 0, port_data_out_0, 0, LFComparisonOperator.GT.value, war_scalar_1)

    linear_test['constraints'] = [raw_1, war_1]

    return linear_test