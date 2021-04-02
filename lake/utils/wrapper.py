from lake.utils.util import check_env, generate_lake_config_wrapper, get_configs_dict, set_configs_sv
from _kratos import create_wrapper_flatten
from lake.top.lake_top import get_lake_dut
from kratos import *
import sys


def get_lake_wrapper(config_path="",
                     gen_name="",
                     in_file_name="",
                     out_file_name="",
                     **lake_kwargs):

    lt_dut, need_config_lift, s, t = get_lake_dut(**lake_kwargs)

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

    generate_lake_config_wrapper(configs_list, "configs.sv", "LakeTop_W.v", gen_name)


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
