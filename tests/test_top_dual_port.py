from lake.utils.wrapper import get_lake_wrapper
from lake.passes.passes import change_sram_port_names
from lake.utils.util import extract_formal_annotation, get_configs_dict, set_configs_sv
from lake.top.lake_top import get_formal_module, get_lake_dut
from kratos import *
import argparse as argparse


def test_gen_dual_port():

    lake_gen_kwargs = {
        "interconnect_input_ports": 2,
        "interconnect_output_ports": 2,
        "read_delay": 1,
        "name": "JoeysWorld",
        "rw_same_cycle": True,
        "fifo_mode": False,
        "stencil_valid": False,
        "mem_width": 16
    }

    get_lake_wrapper(config_path="",
                     gen_name="mek",
                     in_file_name="",
                     out_file_name="",
                     **lake_gen_kwargs)

    prefix = ""
    lt_dut, need_config_lift, s, t = get_lake_dut(**lake_gen_kwargs)
    # configs = lt_dut.get_static_bitstream(config_path, in_file_name, out_file_name)
    # configs_list = set_configs_sv(lt_dut, "configs.sv", get_configs_dict(configs))


if __name__ == "__main__":
    test_gen_dual_port()
