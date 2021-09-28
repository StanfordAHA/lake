from lake.modules.optimization_test import OptimizationTest
import argparse
from kratos import *
from lake.passes.passes import lift_config_reg

if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser.add_argument("-o", "--opt_lvl", default=0, type=int, help="lvl 0-2 accepted")
    args = parser.parse_args()

    opt_lvl = args.opt_lvl

    opt_dut = OptimizationTest(optimization_lvl=opt_lvl)
    verilog(opt_dut, filename=f"design.v",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})

