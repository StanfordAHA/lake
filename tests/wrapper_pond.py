import argparse
import sys
import tempfile
import pytest

from lake.utils.util import generate_lake_config_wrapper
from lake.utils.util import check_env
from lake.utils.test_infra import base_pond_tester


def get_pond_wrapper(in_ports=1,
                     out_ports=1): # 1R/1W pond 

    pond_dut, configs, configs_list, magma_dut, tester = \
        base_pond_tester(in_ports,
                         out_ports,
                         get_configs_list=True)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir = "build"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal"])

    generate_lake_config_wrapper(configs_list, "configs.sv", "build/pond_W.v", "Pond")


def wrapper():
    lc, ls = check_env()
    # we are in the process of transitioning to csvs being in this folder
    # lc = <path to clockwork>/aha_garnet_design/

    #config_path = lc + config_path_input
    get_pond_wrapper()


# adding this test to ensure wrapper generation is not broken
def test_wrapper():
    wrapper()


if __name__ == "__main__":
    wrapper()

