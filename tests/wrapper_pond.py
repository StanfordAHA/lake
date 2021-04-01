import argparse
import sys
import tempfile
import pytest

from lake.utils.util import generate_lake_config_wrapper
from lake.utils.util import check_env
from lake.utils.test_infra import base_pond_tester

from lake.utils.util import *
from _kratos import create_wrapper_flatten

"""
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

    # get flattened module
    flattened = create_wrapper_flatten(lt_dut.internal_generator.clone(),
                                       "LakeTop_W")
    inst = Generator("LakeTop_W",
                     internal_generator=flattened)
    verilog(inst, filename="LakeTop_W.v")

    # get original verilog
    verilog(lt_dut, filename="lt_dut.v")
    # prepend wrapper module to original verilog file
    with open("LakeTop_W.v", "a") as with_flatten:
        with open("lt_dut.v", "r") as lt_dut_file:
            for line in lt_dut_file:
                with_flatten.write(line)

    generate_lake_config_wrapper(configs_list, "configs.sv", "LakeTop_W.v", name)
"""


def get_pond_wrapper(config_path,
                     name,
                     depth,
                     iterator_support,
                     in_ports=2,
                     out_ports=2,
                     mem_in_ports=1,
                     mem_out_ports=1):  # 1R/1W pond

    pond_dut, configs, configs_list, magma_dut, tester = \
        base_pond_tester(config_path,
                         depth,
                         iterator_support,
                         in_ports,
                         out_ports,
                         mem_in_ports,
                         mem_out_ports,
                         get_configs_list=True)

    # get flattened module
    flattened = create_wrapper_flatten(pond_dut.internal_generator.clone(),
                                       "pond_W")
    inst = Generator("pond_W",
                     internal_generator=flattened)
    verilog(inst, filename="pond_W.v")

    # get original verilog
    verilog(pond_dut, filename="pond_dut.v")
    # prepend wrapper module to original verilog file
    with open("pond_W.v", "a") as with_flatten:
        with open("pond_dut.v", "r") as pond_dut_file:
            for line in pond_dut_file:
                with_flatten.write(line)

    generate_lake_config_wrapper(configs_list, "configs.sv", "build/pond_W.v", name)


def get_pond_wrapper_orig(config_path,
                          name,
                          depth,
                          iterator_support,
                          in_ports=2,
                          out_ports=2,
                          mem_in_ports=1,
                          mem_out_ports=1):  # 1R/1W pond

    pond_dut, configs, configs_list, magma_dut, tester = \
        base_pond_tester(config_path,
                         depth,
                         iterator_support,
                         in_ports,
                         out_ports,
                         mem_in_ports,
                         mem_out_ports,
                         get_configs_list=True)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "build"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])

    generate_lake_config_wrapper(configs_list, "configs.sv", "build/pond_W.v", name, "pond")


def wrapper(config_path_input, name, pond_depth, iterator_support):
    lc, ls = check_env()
    # we are in the process of transitioning to csvs being in this folder
    # lc = <path to clockwork>/aha_garnet_design/

    # config_path = lc + config_path_input
    get_pond_wrapper(config_path=config_path_input, name=name, depth=pond_depth, iterator_support=iterator_support)


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
    parser.add_argument("-d",
                        type=int,
                        help="optional: depth for Pond memory",
                        default=32)

    parser.add_argument("-l",
                        type=int,
                        help="optional: iterator support for Pond memory",
                        default=3)
    args = parser.parse_args()

    usage = "File usage: python wrapper.py [-c / --csv_file] [csv_file path relative to LAKE_CONTROLLERS environment variable]"
    usage += " [-n] [module name for LakeWrapper module (default: LakeWrapper)]"

    # if args.c is None:
    #    error(usage)

    wrapper(args.c, args.n, args.d, args.l)

    # Example usage:
    # python tests/wrapper_lake.py -c conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf
