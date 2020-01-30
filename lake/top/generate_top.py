import kratos as kts


if __name__ == "__main__":
    lake_dut = LakeTop()
    # Perform pass to move config_reg

    verilog(lake_dut, filename="lake_top.sv",
            check_multiple_driver=False,
            optimize_if=False)
