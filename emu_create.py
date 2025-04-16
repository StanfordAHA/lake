import os

src_file_dir = "/aha/garnet"
dump_file_dir = "/aha/lake"

concate_file_dir = "/aha/lake/emu_modules"

swap_modules = ["CW_fp_add", "CW_fp_mult", "glb_bank_sram_gen_13"]


def rm_gclk(src_file, dump_file):

    gclk_signals = ["mem_ctrl_intersect_unit_flat_clk", "mem_ctrl_crddrop_flat_clk", "mem_ctrl_Repeat_flat_clk",\
                    "mem_ctrl_RepeatSignalGenerator_flat_clk", "mem_ctrl_reduce_pe_cluster_flat_clk",\
                    "mem_ctrl_stream_arbiter_flat_clk", "mem_ctrl_pass_through_flat_clk", "mem_ctrl_fiber_access_16_flat_clk",\
                    "mem_ctrl_strg_ub_vec_flat_clk", "mem_ctrl_strg_ram_64_512_delay1_flat_clk"]

    with open(src_file, "r") as f:
        lines = f.readlines()
        for i in range(len(lines)):
            if "gclk" in lines[i]:
                if "gclk = clk & tile_en;" in lines[i]:
                    lines[i] = lines[i].replace("gclk = clk & tile_en;", "gclk = clk;")
                else:
                    for gclk_signal in gclk_signals:
                        gclk_signal_exp = gclk_signal + " ="
                        if gclk_signal_exp in lines[i]:
                            new_line = "assign " + gclk_signal + " = gclk;\n"
                            lines[i] = new_line
                            break

        # write the modified lines to the dump file
        with open(dump_file, "w") as dump:
            for line in lines:
                dump.write(line)
            

def create_base_design(src_file, dump_file):

    with open(src_file, "r") as src:
        lines = src.readlines()

        # if the line contains the swap_modules, then modify the line
        for i in range(len(lines)):
            for swap_module in swap_modules:
                if swap_module in lines[i] and "module" not in lines[i]:
                    lines[i] = lines[i].replace(swap_module, swap_module + "_emu")
        
    # write the modified lines to the dump file
    with open(dump_file, "w") as dump:
        for line in lines:
            dump.write(line)

    # concate every swap module into one file
    for swap_module in swap_modules:
        concate_file = os.path.join(concate_file_dir, swap_module + "_emu.sv")
        print(f"concating {swap_module} to {concate_file}")
        
        # concate the file to dump_file
        with open(dump_file, "a") as dump:
            with open(concate_file, "r") as src:
                lines = src.readlines()
                for line in lines:
                    dump.write(line)


file_name = "garnet"

src_file = os.path.join(src_file_dir, file_name + ".v")
dump_file_emu = os.path.join(dump_file_dir, file_name + "_emu.v")
dump_file_gclk = os.path.join(dump_file_dir, file_name + "_gclk.v")


rm_gclk(dump_file_emu, dump_file_gclk)
