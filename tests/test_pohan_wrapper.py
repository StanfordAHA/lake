from lake.top.pohan_top import PohanTop
from kratos import *
import pytest
import argparse


@pytest.mark.skip
def test_gen_dual_port(config_path="/aha/config.json", name="cfg_wrapper"):

    print(f"Using configuration file at: {config_path}")
    pohan_top = PohanTop()
    pohan_top_wrapper = pohan_top.wrapper(vlog_filename=name,
                                          config_path=config_path)
    print(f"Generated verilog file : {name}.sv")
    return pohan_top_wrapper


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Parse arguments for PohanTop")
    ap.add_argument("-f",
                    type=str,
                    help="path to configuration json file",
                    default="/aha/config.json")
    ap.add_argument("-n",
                    type=str,
                    help="suffix for wrapper name",
                    default="cfg_wrapper")
    args = ap.parse_args()

    test_gen_dual_port(config_path=args.f,
                       name=args.n)
