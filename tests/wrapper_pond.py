import argparse
import sys
import tempfile
import pytest

from lake.utils.util import generate_lake_config_wrapper
from lake.utils.util import check_env
from lake.utils.test_infra import base_pond_tester


def get_pond_wrapper(config_path,
                     name,
                     in_ports=1,
                     out_ports=1): # 1R/1W pond 

    pond_dut, configs, configs_list, magma_dut, tester = \
        base_pond_tester(config_path,
                         in_ports,
                         out_ports,
                         get_configs_list=True)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "build"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])

    generate_lake_config_wrapper(configs_list, "configs.sv", "build/pond_W.v", name)


def wrapper(config_path_input, name):
    lc, ls = check_env()
    # we are in the process of transitioning to csvs being in this folder
    # lc = <path to clockwork>/aha_garnet_design/

    #config_path = lc + config_path_input
    get_pond_wrapper(config_path=config_path_input, name=name)


def error(usage):
        print(usage)
        sys.exit(2)


# adding this test to ensure wrapper generation is not broken
def test_wrapper():
    wrapper()

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='LakeWrapper')
    parser.add_argument("-c",
                        type=str,
                        help="required: csv_file path relative to LAKE_CONTROLLERS environment variable")
    
    parser.add_argument("-n",
                        type=str,
                        help="optional: module name for LakeWrapper module (default: LakeWrapper)",
                        default="LakeWrapper")

    args = parser.parse_args()

    usage = "File usage: python wrapper.py [-c / --csv_file] [csv_file path relative to LAKE_CONTROLLERS environment variable]"
    usage += " [-n] [module name for LakeWrapper module (default: LakeWrapper)]"

    #if args.c is None:
    #    error(usage)

    wrapper(args.c, args.n)

    # Example usage:
    # python tests/wrapper_lake.py -c conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf
