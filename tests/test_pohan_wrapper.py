from lake.top.pohan_top import PohanTop
from kratos import *
import pytest
import argparse


@pytest.mark.skip
def test_gen_dual_port(config_path="/aha/config.json",
                       base_vlog_filename="default_base",
                       wrapper_vlog_filename="default_wrapper",
                       vlog_extension="v"):

    print(f"Using configuration file at: {config_path}")
    pohan_top = PohanTop()
    pohan_top.get_flat_verilog(filename=f"{base_vlog_filename}.{vlog_extension}")
    pohan_top_wrapper = pohan_top.wrapper(wrapper_vlog_filename=wrapper_vlog_filename,
                                          vlog_extension=vlog_extension,
                                          config_path=config_path)
    print(f"Generated base verilog file : {base_vlog_filename}.{vlog_extension}")
    print(f"Generated wrapper verilog file : {wrapper_vlog_filename}.{vlog_extension}")
    return pohan_top_wrapper


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Parse arguments for PohanTop")
    ap.add_argument("-f",
                    type=str,
                    help="path to configuration json file",
                    default="/aha/config.json")
    ap.add_argument("-b",
                    type=str,
                    help="base verilog filename",
                    default="default_base")
    ap.add_argument("-w",
                    type=str,
                    help="wrapper verilog filename",
                    default="default_wrapper")
    ap.add_argument("-e",
                    type=str,
                    help="verilog extension (v or sv)",
                    default="v")

    args = ap.parse_args()

    test_gen_dual_port(config_path=args.f,
                       base_vlog_filename=args.b,
                       wrapper_vlog_filename=args.w,
                       vlog_extension=args.e)
