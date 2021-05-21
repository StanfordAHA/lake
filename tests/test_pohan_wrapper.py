from lake.top.pohan_top import PohanTop
from kratos import *
import pytest
import argparse


@pytest.mark.skip
def test_gen_dual_port(config_path="/aha/config.json"):

    print(f"Using configuration file at: {config_path}")
    pohan_top = PohanTop()
    pohan_top_wrapper = pohan_top.wrapper(module_name="joey_mod",
                                          config_path=config_path,
                                          name="pohan_wrapper")
    return pohan_top_wrapper


if __name__ == "__main__":
    ap = argparse.ArgumentParser(description="Parse arguments for PohanTop")
    ap.add_argument("-f",
                    type=str,
                    help="required: path to configuration json file",
                    default="/aha/config.json")
    args = ap.parse_args()

    test_gen_dual_port(config_path=args.f)
