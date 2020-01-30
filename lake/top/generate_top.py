import kratos as kts
from lake.passes.passes import lift_config_reg
from lake.top.lake_top import LakeTop

if __name__ == "__main__":
    lake_dut = LakeTop()
    # Perform pass to move config_reg

    kts.verilog(lake_dut, filename="lake_top.sv",
                check_multiple_driver=False,
                optimize_if=False,
                additional_passes={"lift_config_reg": lift_config_reg})
