import argparse
import sys
import tempfile

from lake.utils.util import generate_lake_config_wrapper
from lake.utils.util import check_env
from test_lake import base_lake_tester


def get_lake_wrapper(config_path,
                     stencil_valid,
                     name,
                     in_file_name="input",
                     out_file_name="output",
                     in_ports=2,
                     out_ports=2):

    lt_dut, configs, configs_list, magma_dut, tester = \
        base_lake_tester(config_path,
                         in_file_name,
                         out_file_name,
                         in_ports,
                         out_ports,
                         stencil_valid)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               flags=["-Wno-fatal"])

    generate_lake_config_wrapper(configs_list, "configs.sv", "build/LakeTop_W.v", name)


def wrapper(config_path_input, stencil_valid, name):
    lc, ls = check_env()
    config_path = lc + config_path_input
    get_lake_wrapper(config_path=config_path,
                     stencil_valid=stencil_valid,
                     name=name)


def error(usage):
    print(usage)
    sys.exit(2)


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
