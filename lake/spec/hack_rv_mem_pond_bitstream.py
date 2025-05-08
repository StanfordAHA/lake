from lake.utils.spec_enum import Direction, LFComparisonOperator

import os

APPS_NEEDING_HACKS = [
    "scalar_reduction_fp",
]


def hack_rv_config(test_name):
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

    if test_name == "scalar_reduction_fp":
        # Only have one Pond
        # "HALIDE_GEN_ARGS" example: "vec_width=256 vec_height=2 glb_i=8 glb_o=1 tree_stages=3"
        vec_len = int(halide_gen_args_dict['vec_width']) * int(halide_gen_args_dict['vec_height'])
        num_partial_reduction = vec_len // int(halide_gen_args_dict['glb_i'])
        rv_config = get_accum_pond(num_partial_reduction=num_partial_reduction,
                                   num_output_pixels=1)

    assert rv_config, f"rv_config is empty for test_name: {test_name}"
    return rv_config


def get_accum_pond(num_partial_reduction=64, num_output_pixels=1):
    '''
    Helper function to create config for reduction pond
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

    linear_test[1] = {
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

    linear_test[2] = {
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
    pr1 = 1
    pr2 = 2

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
