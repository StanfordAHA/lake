from lake.utils.wrapper import get_dut, wrapper
from kratos import *
import pytest


@pytest.mark.skip
def test_gen_dual_port():

    config_path = "/nobackup/joeyliu/aha/poly/clockwork/aha_garnet_design_pond/three_level_pond_rolled/lake_collateral/ub_hw_input_stencil_BANK_0/"

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

    # Get the DUT
    lt_dut, module_name, iter_support = get_dut(pond=False,
                                                pd=0,
                                                pl=0,
                                                **lake_gen_kwargs)

    wrapper(dut=lt_dut,
            module_name=module_name,
            iterator_support=iter_support,
            config_path_input=config_path,
            name=lake_gen_kwargs['name'])


if __name__ == "__main__":
    test_gen_dual_port()
