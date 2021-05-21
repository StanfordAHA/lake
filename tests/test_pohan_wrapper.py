from lake.top.pohan_top import PohanTop
from lake.utils.wrapper import get_dut, wrapper
from kratos import *
import pytest


@pytest.mark.skip
def test_gen_dual_port():

    config_path = "/nobackup/joeyliu/aha/poly/clockwork/aha_garnet_design_pond/three_level_pond_rolled/lake_collateral/ub_hw_input_stencil_BANK_0/"

    pohan_top = PohanTop()
    pohan_top_wrapper = pohan_top.wrapper(module_name="joey_mod",
                                          config_path=config_path,
                                          name="pohan_wrapper")
    return pohan_top_wrapper

if __name__ == "__main__":
    test_gen_dual_port()