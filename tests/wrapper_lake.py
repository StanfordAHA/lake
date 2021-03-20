import argparse
import sys
import tempfile
import pytest

from lake.utils.util import *
from lake.utils.test_infra import base_lake_tester
from lake.top.lake_top import get_lake_dut

from _kratos import create_wrapper_flatten

def get_lake_wrapper(config_path,
                     stencil_valid,
                     name,
                     in_file_name="",
                     out_file_name="",
                     in_ports=2,
                     out_ports=2):

    lt_dut, need_config_lift, s, t = get_lake_dut(in_ports=in_ports,
                                                  out_ports=out_ports,
                                                  stencil_valid=stencil_valid)

    configs = lt_dut.get_static_bitstream(config_path, in_file_name, out_file_name)
    # prints out list of configs for compiler team
    configs_list = set_configs_sv(lt_dut, "configs.sv", get_configs_dict(configs))

    flattened = create_wrapper_flatten(lt_dut.internal_generator.clone(),
                                       "LakeTop_W")
    inst = Generator("LakeTop_W",
                     internal_generator=flattened)
    verilog(inst, filename="LakeTop_W.v")
    
    generate_lake_config_wrapper(configs_list, "configs.sv", "LakeTop_W.v", name)


def wrapper(config_path_input, stencil_valid, name):
    lc, ls = check_env()
    # we are in the process of transitioning to csvs being in this folder
    # lc = <path to clockwork>/aha_garnet_design/

    config_path = lc + config_path_input
    get_lake_wrapper(config_path=config_path,
                     stencil_valid=stencil_valid,
                     name=name)


def error(usage):
    print(usage)
    sys.exit(2)


# adding this test to ensure wrapper generation is not broken

# have to skip this test for now because LAKE_CONTROLLERS will
# be changing very soon in the future compared to the current
# path as the compiler team changes directories (and eliminates
# complicated csv file names)
@pytest.mark.skip
@pytest.mark.parametrize("stencil_valid", [True, False])
@pytest.mark.parametrize("name", ["LakeWrapper", "LakeConv33"])
def test_wrapper(stencil_valid,
                 name):
    wrapper(conv_3_3_wrapper_path, stencil_valid, name)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='LakeWrapper')
    parser.add_argument("-c",
                        type=str,
                        help="required: csv_file path relative to LAKE_CONTROLLERS environment variable")
    parser.add_argument("-s",
                        type=str,
                        help="optional: True or False indicating whether or not to generate hardware with stencil_valid (default: True)",
                        default="True")
    parser.add_argument("-n",
                        type=str,
                        help="optional: module name for LakeWrapper module (default: LakeWrapper)",
                        default="LakeWrapper")

    args = parser.parse_args()

    usage = "File usage: python wrapper.py [-c / --csv_file] [csv_file path relative to LAKE_CONTROLLERS environment variable]"
    usage += " [-s / --stencil_valid] [True or False indicating whether or not to generate hardware with stencil_valid (default: True)"
    usage += " [-n] [module name for LakeWrapper module (default: LakeWrapper)]"

    if args.s == "False":
        stencil_valid = False
    elif args.s == "True":
        stencil_valid = True
    else:
        print("Invalid option for stencil valid (must be True or False)...defaulting to True")

    if args.c is None:
        error(usage)

    wrapper(args.c, stencil_valid, args.n)

    # Example usage:
    # python tests/wrapper_lake.py -c conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf
