import argparse
import sys
import tempfile
import pytest

from lake.utils.util import *
from lake.top.lake_top import get_lake_dut
from lake.top.pond import get_pond_dut

from _kratos import create_wrapper_flatten


def wrapper(config_path_input,
            stencil_valid,
            name,
            pond,
            pd,
            pl,
            in_file_name="",
            out_file_name=""):
    lc, ls = check_env()
    # we are in the process of transitioning to csvs being in this folder
    # lc = <path to clockwork>/aha_garnet_design/

    config_path = lc + config_path_input

    if pond:
        dut, need_config_lift, s, t = \
            get_pond_dut(depth=depth,
                         iterator_support=iterator_support,
                         in_ports=in_ports,
                         in_ports=2,
                         out_ports=2,
                         mem_in_ports=1,
                         mem_out_ports=1,
                         out_ports=out_ports)
        module_name = "pond"
    else:
        dut, need_config_lift, s, t = \
            get_lake_dut(in_ports=2,
                         out_ports=2,
                         stencil_valid=stencil_valid)
        module_name = "LakeTop"

    configs = dut.get_static_bitstream(config_path)
    # prints out list of configs for compiler team
    configs_list = set_configs_sv(dut, "configs.sv", get_configs_dict(configs))

    # get flattened module
    flattened = create_wrapper_flatten(dut.internal_generator.clone(),
                                       f"{module_name}_W")
    inst = Generator(f"{module_name}_W",
                     internal_generator=flattened)
    verilog(inst, filename=f"{module_name}_W.v")

    # get original verilog
    verilog(dut, filename=f"{module_name}_dut.v")
    # prepend wrapper module to original verilog file
    with open(f"{module_name}_W.v", "a") as with_flatten:
        with open(f"{module_name}_dut.v", "r") as dut_file:
            for line in dut_file:
                with_flatten.write(line)

    generate_lake_config_wrapper(configs_list, "configs.sv", f"{module_name}_W.v", name)


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
    parser.add_argument("-n",
                        type=str,
                        help="optional: module name for LakeWrapper module (default: LakeWrapper)",
                        default="LakeWrapper")
    parser.add_argument("-s",
                        type=str,
                        help="optional: True or False indicating whether or not to generate hardware with stencil_valid (default: True)",
                        default="True")
    parser.add_argument("-p",
                        type=str,
                        help="True for pond wrapper. False for memtile wrapper.",
                        default="False")
    parser.add_argument("-pd",
                        type=int,
                        help="optional: depth for Pond memory",
                        default=32)
    parser.add_argument("-pl",
                        type=int,
                        help="optional: iterator support for Pond memory",
                        default=3)

    args = parser.parse_args()

    usage = "File usage: python wrapper.py [-c / --csv_file] [csv_file path relative to LAKE_CONTROLLERS environment variable]"
    usage += " [-s / --stencil_valid] [True or False indicating whether or not to generate hardware with stencil_valid (default: True)"
    usage += " [-n] [module name for LakeWrapper module (default: LakeWrapper)]"

    stencil_valid = True if args.s == "True" else False
    pond = False if args.s == "False" else True

    if args.c is None:
        error(usage)

    wrapper(args.c, stencil_valid, args.n, pond, args.pd, args.pl)

    # Example usage:
    # python tests/wrapper_lake.py -c conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf
