import random
import json
import copy


# ===============
# general sg
# ===============
def gen_sg(config,
           addr_width=16,
           max_dim=5,
           max_start_addr=500,
           max_extent=15,
           max_stride=10):

    dimensionality = random.randint(1, max_dim)

    max_addr = pow(2, addr_width)
    start_addr = random.randint(0, max_start_addr - 1)
    start_addr = start_addr % max_addr

    extent = random.choices(range(2, max_extent), weights=range(max_extent, 2, -1), k=dimensionality)
    stride = random.randint(1, max_stride)

    cycle_stride = [stride]
    for dim in range(1, dimensionality):
        range_start = extent[dim - 1] * cycle_stride[dim - 1]
        next_stride = random.randint(range_start, range_start + 500)
        cycle_stride.append(next_stride)
    # print(cycle_stride)

    config["cycle_starting_addr"] = [start_addr]
    config["cycle_stride"] = cycle_stride
    config["dimensionality"] = dimensionality
    config["extent"] = extent

    return config


# ================
# general ag
# ================
def gen_ag(config,
           name,
           agg=False,
           addr_width=9):

    dimensionality = config["dimensionality"]
    extent = config["extent"]

    max_addr = pow(2, addr_width)
    if agg:
        start_addr = 0
        stride = 1
    else:
        max_start_addr = max_addr // extent[0]
        start_addr = random.randint(0, max_start_addr)
        range_end = (max_addr - start_addr) // extent[0]
        stride = random.randint(1, max(1, range_end))

    addr_stride = [stride]
    for dim in range(1, dimensionality):
        range_start = extent[dim - 1] * addr_stride[dim - 1]
        range_start = range_start % max_addr
        if agg:
            # ===============
            # when test random config => 4-to-1 might not follow next_stride = range_start
            # fix config to start new addr = 0 or 4 for in2agg
            # ===============
            if 0 < range_start <= max_addr // 2:
                next_stride = max_addr // 2
            else:
                next_stride = 0
        else:
            range_end = max_addr // extent[dim]
            if range_end > range_start:
                next_stride = random.randint(range_start, range_end)
            else:
                next_stride = random.randint(0, max(0, range_end))
        addr_stride.append(next_stride)
    # print(addr_stride)

    config[f"{name}_data_starting_addr"] = [start_addr]
    config[f"{name}_data_stride"] = addr_stride

    return config


# =============
# generate sched from given sg/ag setting
# =============
def get_sched(start_addr, dimensionality, stride, extent):
    step = []
    for i in range(dimensionality):
        step.append(1)

    sched = []
    while True:
        output = start_addr
        for i in range(dimensionality):
            output += stride[i] * (step[i] - 1)
        sched.append(output)
        if step == extent:
            break
        for i in range(dimensionality):
            if step[i] == extent[i]:
                step[i] = 1
                continue
            step[i] += 1
            break
    return sched


def gen_config(name, delay=False, fifo_depth=8):
    config = {}

    # addr_width = 3 for AGG
    # addr_width = 4 for TB
    # addr_width = 9 for SRAM
    # addr_width = 16 for sched

    if "in2agg" in name:
        config = gen_sg(config, max_dim=3, max_start_addr=100)
        config = gen_ag(config, "write", addr_width=3, agg=True)
    elif "agg2sram" in name:        # only for area_opt = False
        config = gen_sg(config)
        config = gen_ag(config, "write", addr_width=4)      # increase read non-zero possibility ######
        config = gen_ag(config, "read", addr_width=3)
        config["agg_read_padding"] = [random.randint(0, pow(2, 8) - 1)]
    elif "sram2tb" in name:
        config = gen_sg(config)
        # agg2sram share read and write addr => sram2tb should also share if in delay mode
        if delay:
            config = gen_ag(config, "read", addr_width=9)   # agg2sram linearly increase for area_opt=True, delay mode
            config["write_data_starting_addr"] = config["read_data_starting_addr"]
            config["write_data_stride"] = config["read_data_stride"]
        else:
            config = gen_ag(config, "read", addr_width=4)   # increase read non-zero possibility ######
            config = gen_ag(config, "write", addr_width=4)
    elif "tb2out" in name:
        if delay:
            config = gen_sg(config, max_dim=3, max_start_addr=100)
            config = gen_ag(config, "read", addr_width=3, agg=True)
        else:
            config = gen_sg(config)
            config = gen_ag(config, "read", addr_width=4)

    return config


def gen_in2agg(large_config, mode, fifo_depth=8, delay_width=4):
    tb2out = large_config[f"tb2out_{mode-2}"]

    # =============
    # fifo can only store fifo_depth address data
    # in2agg delay can only support to cycle_stride * (fifo_depth -1)
    # delay bit_width = 4
    # =============
    delay = random.randint(0, min(pow(2, delay_width) - 1, tb2out["cycle_stride"][0] * (fifo_depth - 1)))
    new_start_addr = tb2out["cycle_starting_addr"][0] + delay
    config = {}
    config["cycle_starting_addr"] = [new_start_addr]
    for name in ["cycle_stride", "dimensionality", "extent"]:
        config[name] = tb2out[name]
    config["write_data_starting_addr"] = tb2out["read_data_starting_addr"]
    config["write_data_stride"] = tb2out["read_data_stride"]
    return config


# ================
# delay_width only support 4
# area_opt = True
# ================
def gen_agg2sram(large_config, i, mode, addr_width=9, fifo_depth=4, delay_width=4):
    config = {}
    config["mode"] = [mode]
    # ============
    # padding mode
    # ============
    if mode == 0:

        # ===========
        # generate sg
        # ===========
        in2agg = large_config[f"in2agg_{i}"]
        in2agg_extent = in2agg["extent"]
        in2agg_stride = in2agg["cycle_stride"]

        new_stride = copy.deepcopy(in2agg_stride)
        new_stride[0] = in2agg_stride[0] * 4
        config["cycle_stride"] = new_stride

        new_extent = copy.deepcopy(in2agg_extent)
        if in2agg_extent[0] % 4 == 0:
            new_extent[0] = in2agg_extent[0] // 4
            padding = 0
        else:
            new_extent[0] = in2agg_extent[0] // 4 + 1
            padding = (4 - in2agg_extent[0] % 4) * in2agg_stride[0] + 1
        config["extent"] = new_extent
        config["agg_read_padding"] = [padding]
        config["delay"] = [0]
        config["cycle_starting_addr"] = [in2agg_stride[0] * 3 + in2agg["cycle_starting_addr"][0] + 1]
        config["dimensionality"] = in2agg["dimensionality"]

        # ===========
        # generate ag
        # ===========
        max_addr = pow(2, addr_width)
        start_addr = random.randint(0, max_addr // 2)
        start_addr = start_addr * 2         # end with 0
        stride = 1                          # area_opt = True

        write_stride = [stride]
        read_stride = [stride]
        for dim in range(1, config["dimensionality"]):
            next_stride = new_extent[dim - 1] * write_stride[dim - 1]
            next_stride = next_stride % max_addr
            write_stride.append(next_stride)
            read_stride.append(next_stride % 4)

        config[f"write_data_starting_addr"] = [start_addr]
        config[f"read_data_starting_addr"] = [start_addr % 4]
        config[f"write_data_stride"] = write_stride
        config[f"read_data_stride"] = read_stride

    # ============
    # delay mode
    # ============
    else:
        sram2tb = large_config[f"sram2tb_{mode-2}"]
        max_delay = pow(2, delay_width)
        delay = random.randint(1, min(max_delay - 1, (fifo_depth - 1) * sram2tb["cycle_stride"][0]))
        config["cycle_starting_addr"] = [sram2tb["cycle_starting_addr"][0] + delay + 1]
        config["delay"] = [delay]
        config["agg_read_padding"] = [0]
        for name in ["cycle_stride", "dimensionality", "extent"]:
            config[name] = sram2tb[name]
        config["write_data_starting_addr"] = sram2tb["read_data_starting_addr"]
        config["read_data_starting_addr"] = sram2tb["write_data_starting_addr"]
        config["write_data_stride"] = sram2tb["read_data_stride"]
        config["read_data_stride"] = sram2tb["write_data_stride"]
    return config


# ================
# check single port sched
# interpret from controller setting
# still might violate when random config since area_opt controller only use part of config
# ================
def check_invalid(config, port):
    cycle = set()
    for i in range(port):
        for j in ["agg2sram", "sram2tb"]:
            cur_config = config[f"{j}_{i}"]
            sched = get_sched(cur_config["cycle_starting_addr"][0],
                              cur_config["dimensionality"],
                              cur_config["cycle_stride"],
                              cur_config["extent"])
            sched = set(sched)
            if (sched & cycle):   # if sched cycle overlap
                return 1
            else:
                cycle.update(sched)
    ## if no overlap
    return 0


# ================
# make sure sched would not be super long
# ================
def check_toolong(config, port):
    for i in range(port):
        for j in ["agg2sram", "sram2tb", "tb2out"]:
            cur_config = config[f"{j}_{i}"]
            end_cycle = cur_config["cycle_starting_addr"][0] + cur_config["cycle_stride"][-1] * cur_config["extent"][-1]
            if end_cycle > 20000:
                return 1
    return 0


# =================
# call by test.py
# generate random config
# =================
def gen_random_config(name="random", area_opt=True):
    isinvalid = 1
    istoolong = 1
    while (isinvalid | istoolong):
        large_config = {}

        port = random.randint(1, 2)
        if area_opt:
            mode_list = []
            for i in range(port):
                if port == 2:
                    mode = random.choice([0, 2, 3])
                else:
                    mode = random.choice([0, 2])
                mode_list.append(mode)

            if 2 in mode_list:
                large_config[f"tb2out_0"] = gen_config("tb2out", delay=True)
                large_config[f"sram2tb_0"] = gen_config("sram2tb", delay=True)
            if 3 in mode_list:
                large_config[f"tb2out_1"] = gen_config("tb2out", delay=True)
                large_config[f"sram2tb_1"] = gen_config("sram2tb", delay=True)

            for i in range(port):
                if mode_list[i] == 0:
                    large_config[f"in2agg_{i}"] = gen_config("in2agg")
                else:
                    large_config[f"in2agg_{i}"] = gen_in2agg(large_config, mode_list[i])
                large_config[f"agg2sram_{i}"] = gen_agg2sram(large_config, i, mode_list[i], addr_width=4)  # increase non-zero possibility
                for j in ["sram2tb", "tb2out"]:
                    if f"{j}_{i}" not in large_config.keys():
                        large_config[f"{j}_{i}"] = gen_config(j)
        # =============
        # area_opt = False
        # =============
        else:
            for i in range(port):
                for j in ["in2agg", "agg2sram", "sram2tb", "tb2out"]:
                    large_config[f"{j}_{i}"] = gen_config(j)

            for i in range(port):
                if port == 2:
                    mode = random.choice([0, 2, 3])
                else:
                    mode = random.choice([0, 2])
                large_config[f"agg2sram_{i}"]["mode"] = [mode]
                max_delay = pow(2, 4)   # delay_width = 4
                if mode != 0:
                    tb2out = large_config[f"tb2out_{mode-2}"]
                    sram2tb = large_config[f"sram2tb_{mode-2}"]
                    agg2sram_delay = random.randint(1, min(max_delay - 1, 3 * sram2tb["cycle_stride"][0]))
                    in2agg_delay = random.randint(0, min(max_delay - 1, 7 * tb2out["cycle_stride"][0]))
                    large_config[f"in2agg_{i}"]["cycle_starting_addr"] = [tb2out["cycle_starting_addr"][0] + in2agg_delay]
                else:
                    agg2sram_delay = random.randint(1, max_delay)
                large_config[f"agg2sram_{i}"]["delay"] = [agg2sram_delay]
        isinvalid = check_invalid(large_config, port)
        if isinvalid:
            print("bad :(")
        else:
            istoolong = check_toolong(large_config, port)
            if istoolong:
                print("too long")

    wrap_config = {}
    wrap_config["ID"] = ""
    wrap_config["num_inputs"] = port
    wrap_config["num_outputs"] = port
    wrap_config["width"] = 16
    wrap_config["mode"] = "UB"
    wrap_config["config"] = large_config

    out_file = open(f"/aha/lake/tests/test_config/random/{name}_{area_opt}.json", "w")
    json.dump(wrap_config, out_file, indent=4)
    out_file.close()


if __name__ == "__main__":
    gen_random_config(area_opt=True)
    # gen_random_config(area_opt=False)
