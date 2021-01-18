def main():
    valid = {"agg": 1, "sram": 0, "tb": 0, "valid_out": 0}
    ready = {"agg": 1, "sram": 1, "tb": 0}

    # parameters from compiler team
    cycles = 50
    steady_state = 16

    # hardware parameters
    fetch_width = 4
    # double buffer
    tb_height = 2

    # input accessor valid (valid to tile)
    input_ac = 1

    sram_capacity = 512
    tb_capacity = 8
    agg_capacity = 4

    # min_update_size = max(1, read_port_width / write_port_width)
    # max_update_size is in units of write_port_width
    # keep track of input valids
    input_ac_count = 0
    min_agg_update_size = 4
    max_agg_update_size = 4
    # tb has startup + steady state
    tb_no_ss = True
    tb_no_ss_cnt = 0

    # keep track of valid data in SRAM
    valid_sram_cnt = 0
    min_sram_update_size = 1
    max_sram_update_size = 1024

    valid_tb_cnt = 0
    min_tb_update_size = 1
    max_tb_update_size = 2 * 8
    # simplify?
    # keep track of valid data in tb
    valid_tb = [False, False]
    valid_tb_cycles = [0, 0]
    tb_index = 1

    for i in range(cycles):

        # ready to tb
        if i < steady_state:
            ready["tb"] = 0
        else:
            if not valid_tb[0] or not valid_tb[1]:
                ready["tb"] = 1
            else:
                ready["tb"] = 0
            # write buffer is ready whenever there is space in write buffer
            # valid if data in it
            # valid if write buffer is full
            if valid["tb"] and ready["tb"]:
                if not valid_tb[0]:
                    valid_tb[0] = True
                elif not valid_tb[1]:
                    valid_tb[1] = True
                elif tb_no_ss:
                    print("ERROR: at least one tb buffer should not be valid yet...")
                    assert False

            if tb_no_ss:
                if tb_no_ss_cnt == tb_height:
                    tb_no_ss = False
                elif valid["tb"] == 1:
                    tb_no_ss_cnt += 1

            # get valid to valid out
            if not tb_no_ss:
                if valid_tb[tb_index] and valid_tb_cycles[tb_index] < fetch_width:
                    valid_tb_cycles[tb_index] += 1
                elif valid_tb[tb_index] and valid_tb_cycles[tb_index] == fetch_width:
                    valid_tb_cycles[tb_index] = 0
                    valid_tb[tb_index] = False
                    tb_index = 1 - tb_index

        if valid_tb_cnt == max_tb_update_size:
            valid_tb_cnt = 0
        if valid["tb"] == 1:
            valid_tb_cnt += 1

        if i < steady_state:
            ready["tb"] = 0
        elif valid_tb_cnt <= tb_capacity:
            ready["tb"] = 1
        else:
            ready["tb"] = 0

        if i > steady_state and \
                valid_tb_cnt >= min_tb_update_size and valid_tb_cnt <= max_tb_update_size:
            valid["valid_out"] = 1
        else:
            valid["valid_out"] = 0

        # get valid to tb
        if valid_sram_cnt == max_sram_update_size:
            valid_sram_cnt = 0
        if valid["sram"] == 1:
            valid_sram_cnt += 1

        if valid_sram_cnt <= sram_capacity:
            ready["sram"] = 1
        else:
            ready["sram"] = 0

        if valid_sram_cnt >= min_sram_update_size and valid_sram_cnt <= max_sram_update_size:
            # not writing to SRAM, so can read from SRAM
            if not (valid["sram"] and ready["sram"]):
                valid["tb"] = 1
        else:
            valid["tb"] = 0

        # get valid to agg
        valid["agg"] = input_ac

        if input_ac_count == max_agg_update_size:
            input_ac_count = 0
        if valid["agg"] == 1:
            input_ac_count += 1

        if input_ac_count <= agg_capacity:
            ready["agg"] = 1
        else:
            ready["agg"] = 0

        # get valid to SRAM
        if input_ac_count >= min_agg_update_size and input_ac_count <= max_agg_update_size:
            valid["sram"] = 1
        else:
            valid["sram"] = 0

        print("CYCLE: ", i)
        print("VALID: ", valid)
        print("READY: ", ready)
        print("WRITE: ", [valid[key] & ready[key] for key in ready])


if __name__ == "__main__":
    main()
