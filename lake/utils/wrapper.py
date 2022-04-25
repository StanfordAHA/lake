import argparse
import sys
import tempfile
import pytest

from lake.utils.util import *
from lake.top.lake_top import LakeTop
from lake.top.pond import get_pond_dut

from _kratos import create_wrapper_flatten


def get_dut(pond, pd, pl, **dut_kwargs):
    if pond:
        dut, need_config_lift, s, t = \
            get_pond_dut(depth=pd,
                         iterator_support=pl,
                         in_ports=2,
                         out_ports=2,
                         mem_in_ports=1,
                         mem_out_ports=1,
                         **dut_kwargs)
        module_name = dut.name
        iterator_support = pl
    else:
        raise NotImplementedError

    return dut, module_name, iterator_support


def wrapper(dut,
            module_name,
            iterator_support,
            config_path_input,
            name):
    lc, ls = check_env()
    # we are in the process of transitioning to csvs being in this folder
    # lc = <path to clockwork>/aha_garnet_design/

    config_path = config_path_input
    # config_path = lc + config_path_input

    configs = dut.get_static_bitstream(config_path)
    # prints out list of configs for compiler team
    configs_list = set_configs_sv(dut, "configs.sv", get_configs_dict(configs), iterator_support)

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

    generate_lake_config_wrapper(configs_list, "configs.sv", f"{module_name}_W.v", name, module_name)


def error(usage):
    print(usage)
    sys.exit(2)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='LakeWrapper')
    parser.add_argument("-c",
                        type=str,
                        help="required: csv_file path relative to LAKE_CONTROLLERS environment variable")
    parser.add_argument("-wfn",
                        type=str,
                        help="optional: file name for LakeWrapper module (default: lakewrapper.sv)",
                        default="lakewrapper.sv")
    parser.add_argument("-wmn",
                        type=str,
                        help="optional: module name for LakeWrapper module (default: LakeWrapper)",
                        default="LakeWrapper")
    parser.add_argument("-s",
                        action='store_true',
                        help="optional: indicating whether or not to generate memtile with stencil_valid (default: False)")
    parser.add_argument("-p",
                        action='store_true',
                        help="True for pond wrapper. False for memtile wrapper. (default: False)")
    parser.add_argument("-d",
                        type=int,
                        help="optional: depth for memory",
                        default=256)
    parser.add_argument("-pl",
                        type=int,
                        help="optional: iterator support for Pond memory",
                        default=3)

    parser.add_argument("-mw",
                        type=int,
                        help="optional: memory width",
                        default=64)

    parser.add_argument("-dp",
                        action='store_true',
                        help="use dual port sram",
                        default=False)

    parser.add_argument("-v",
                        action='store_true',
                        help='Generate main verilog')

    parser.add_argument("-vmn",
                        type=str,
                        help="optional: module name for Lake module (default: LakeTop)",
                        default="LakeTop")

    parser.add_argument("-vfn",
                        type=str,
                        help="optional: file name for Lake module (default: laketop.sv)",
                        default="laketop.sv")

    parser.add_argument("-ii",
                        type=int,
                        help="input iterator support",
                        default=6)

    parser.add_argument("-oi",
                        type=int,
                        help="output iterator support",
                        default=6)

    parser.add_argument("-rd",
                        type=int,
                        help="memory read delay",
                        default=1)

    parser.add_argument("-a",
                        action='store_true',
                        help="append module wrapper to wrapper file")

    args = parser.parse_args()

    usage = "File usage: python wrapper.py [-c / --csv_file] [csv_file path relative to LAKE_CONTROLLERS environment variable]"
    usage += " [-s / --stencil_valid] [True or False indicating whether or not to generate hardware with stencil_valid (default: True)"
    usage += " [-n] [module name for LakeWrapper module (default: LakeWrapper)]"

    if args.c is None:
        error(usage)

    lake_kwargs = {}

    if args.p is False:
        # Use updated codepath for wrapper generation of laketop
        lake_kwargs['stencil_valid'] = args.s
        lake_kwargs['mem_width'] = args.mw
        lake_kwargs['mem_depth'] = args.d
        lake_kwargs['rw_same_cycle'] = args.dp
        lake_kwargs['input_iterator_support'] = args.ii
        lake_kwargs['output_iterator_support'] = args.oi
        lake_kwargs['read_delay'] = args.rd
        lake_kwargs['name'] = args.vmn
        lt_dut = LakeTop(**lake_kwargs)
        lt_dut.wrapper(wrapper_vlog_filename=args.wfn,
                       wrapper_vlog_modulename=args.wmn,
                       # vlog_extension="sv",
                       config_path=args.c,
                       append=args.a)
        if args.v:
            lt_dut.get_verilog(args.vfn, flattened=True)
        print(lt_dut)

    else:
        pond_kwargs = {}
        pond_kwargs['name'] = args.wmn
        dut, module_name, iterator_support = get_dut(args.p, args.d, args.pl, **pond_kwargs)
        wrapper(dut, module_name, iterator_support, args.c, args.wfn)

    # Example usage:
    # python wrapper_lake.py -c conv_3_3_recipe/buf_inst_input_10_to_buf_inst_output_3_ubuf
