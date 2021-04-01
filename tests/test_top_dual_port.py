

from lake.passes.passes import change_sram_port_names
from lake.utils.util import extract_formal_annotation, get_configs_dict, set_configs_sv
from lake.top.lake_top import get_formal_module, get_lake_dut
from kratos import *
import argparse as argparse


def test_gen_dual_port():

    lake_gen_kwargs = {
        "read_delay": 1,
        "name": "JoeysWorld",
        "rw_same_cycle": True,
        "fifo_mode": False,
        "mem_width": 16
    }

    prefix = ""
    lt_dut, need_config_lift, s, t = get_lake_dut(**lake_gen_kwargs)
    # configs = lt_dut.get_static_bitstream(config_path, in_file_name, out_file_name)
    # configs_list = set_configs_sv(lt_dut, "configs.sv", get_configs_dict(configs))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='LakeTop')
    parser.add_argument("-f",
                        help="optional: will generate verilog, annotation file, and dim to strides/range mapping collateral to solve a formal problem. must provide module to solve for")

    args = parser.parse_args()

    need_config_lift = True

    lake_gen_kwargs = {
        "read_delay": 1,
        "name": "JoeysWorld",
        "rw_same_cycle": True,
        "fifo_mode": False
    }

    # normal generation
    if args.f is None:
        prefix = ""
        lake_dut, need_config_lift, use_sram_stub, tsmc_info = get_lake_dut(mem_width=16,
                                                                            **lake_gen_kwargs)
        # extract_formal_annotation(lake_dut, f"lake_top_annotation.txt", "full")
    # optional: to add generator cuts for formal module verilog + annotations
    else:
        module = args.f
        lake_dut, need_config_lift, use_sram_stub, tsmc_info = get_formal_module(module)
        prefix = f"{module}_"

    # config lift happens in all possible cases by this point
    assert not need_config_lift

    sram_port_pass = change_sram_port_names(use_sram_stub=use_sram_stub, sram_macro_info=tsmc_info)
    # generate verilog
    verilog(lake_dut, filename=f"{prefix}_dual_port.sv",
            optimize_if=False,
            additional_passes={"change sram port names": sram_port_pass})
