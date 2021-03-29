import argparse
import sys
import tempfile
import pytest

from lake.utils.util import generate_lake_config_wrapper
from lake.utils.util import check_env
from lake.utils.test_infra import base_lake_tester


def get_lake_wrapper(config_path,
                     stencil_valid,
                     name,
                     fw,
                     in_file_name="",
                     out_file_name="",
                     in_ports=2,
                     out_ports=2):

    lt_dut, configs, configs_list, magma_dut, tester = \
        base_lake_tester(config_path=config_path,
                         in_file_name=in_file_name,
                         out_file_name=out_file_name,
                         in_ports=in_ports,
                         out_ports=out_ports,
                         fetch_width=fw,
                         stencil_valid=stencil_valid,
                         get_configs_list=True)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               flags=["-Wno-fatal"])

    generate_lake_config_wrapper(configs_list, "configs.sv", "build/LakeTop_W.v", name)


def wrapper(config_path_input, stencil_valid, name, fw):
    lc, ls = check_env()
    # we are in the process of transitioning to csvs being in this folder
    # lc = <path to clockwork>/aha_garnet_design/

    config_path = lc + config_path_input
    get_lake_wrapper(config_path=config_path,
                     stencil_valid=stencil_valid,
                     name=name,
                     fw=fw)


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
    parser.add_argument("-fw",
                        type=str,
                        help="optional: module name for LakeWrapper module (default: LakeWrapper)",
                        default="4")

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

    fw = int(args.fw)
    wrapper(args.c, stencil_valid, args.n, fw)

    # Example usage:
    # python tests/wrapper_lake.py -c conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf
