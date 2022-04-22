from kratos import *
from lake.top.lake_top import *
import argparse

if __name__ == "__main__":

    # parser = argparse.ArgumentParser(description='LakeTop')
    # parser.add_argument("-f",
    #                     help="optional: will generate verilog, annotation file, and dim to strides/range mapping collateral to solve a formal problem. must provide module to solve for")
    # parser.add_argument("--fetch_width", type=int, default=4)
    # parser.add_argument("--dual_port", action="store_true")

    # args = parser.parse_args()

    configs = [
        {'ports': 'single', 'fetch_width': 1},
        {'ports': 'single', 'fetch_width': 2},
        {'ports': 'single', 'fetch_width': 4},
        {'ports': 'dual', 'fetch_width': 1},
        {'ports': 'dual', 'fetch_width': 2},
        {'ports': 'dual', 'fetch_width': 4},
    ]

    for config in configs:

        fetch_width = config['fetch_width']
        ports = config['ports']

        mem_width = fetch_width * 16

        mem_name = "single"
        rw_same_cycle = False
        if ports == 'dual':
            rw_same_cycle = True
            mem_name = "dual"

        lake_top = LakeTop(data_width=16,
                        mem_width=mem_width,
                        mem_depth=512,
                        banks=1,
                        fifo_mode=True,
                        add_clk_enable=True,
                        add_flush=True,
                        rw_same_cycle=False,
                        read_delay=1,
                        use_sim_sram=True,
                        name=f"LakeTop_width_{fetch_width}_{mem_name}")

        # print(lake_top)

        # config = extract_top_config(lake_top.dut, verbose=True)

        # generate verilog
        verilog(lake_top.dut, filename=f"LakeTop_width_{fetch_width}_{mem_name}.sv",
                optimize_if=False)