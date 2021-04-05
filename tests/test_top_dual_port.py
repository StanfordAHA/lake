from lake.utils.wrapper import get_lake_wrapper
from lake.utils.util import get_configs_dict, set_configs_sv
from lake.top.lake_top import get_lake_dut
from kratos import *
import pytest


@pytest.mark.skip
def test_gen_dual_port():

    config_path = "/nobackup/joeyliu/aha/poly/clockwork/aha_garnet_design_pond/three_level_pond_rolled/lake_collateral/ub_hw_input_stencil_BANK_0/"
    in_file_name = ""
    out_file_name = ""

    lake_gen_kwargs = {
        "interconnect_input_ports": 2,
        "interconnect_output_ports": 2,
        "read_delay": 1,
        "name": "JoeysWorld",
        "rw_same_cycle": True,
        "fifo_mode": False,
        "stencil_valid": True,
        "mem_width": 16
    }

    get_lake_wrapper(config_path=config_path,
                     gen_name="mek",
                     in_file_name=in_file_name,
                     out_file_name=out_file_name,
                     **lake_gen_kwargs)

    prefix = ""
    lt_dut, need_config_lift, s, t = get_lake_dut(**lake_gen_kwargs)
    configs = lt_dut.get_static_bitstream(config_path, in_file_name, out_file_name)
    configs_list = set_configs_sv(lt_dut, "configs.sv", get_configs_dict(configs))


if __name__ == "__main__":
    test_gen_dual_port()
