def main():
    valid = {"agg": 1, "sram": 0, "tb": 0, "valid_out": 0}
    ready = {"agg": 1, "sram": 1, "tb": 0}

    # parameters
    fetch_width = 4
    # double buffer

    tb_height = 2
    cycles = 50 
    steady_state = 16

    # input accessor valid (valid to tile)
    input_ac = 1
    # keep track of input valids
    input_ac_count = 0

    # tb has startup + steady state
    tb_no_ss = True
    tb_no_ss_cnt = 0

    # keep track of valid data in SRAM
    valid_sram_cnt = 0

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
                    valid["valid_out"] = 1 
                elif valid_tb[tb_index] and valid_tb_cycles[tb_index] == fetch_width:
                    valid_tb_cycles[tb_index] = 0
                    valid_tb[tb_index] = False
                    tb_index = 1 - tb_index
                    if valid_tb[tb_index]:
                        valid["valid_out"] = 1
                    else:
                        valid["valid_out"] = 0
            
        # get valid to tb
        # doesn't account for reuse of data in SRAM
        # once written to TB, SRAM data is regarded as invalid
        if valid["sram"] == 1:
            valid_sram_cnt += 1

        if valid_sram_cnt > 0:
            valid["tb"] = 1
            if valid["tb"] and ready["tb"]:
                valid_sram_cnt -= 1
        else:
            valid["tb"] = 0

        # get valid to agg
        valid["agg"] = input_ac

        # get valid to SRAM
        if input_ac_count == fetch_width - 1:
            valid["sram"] = 1
            input_ac_count = 0
        else:
            valid["sram"] = 0
            if valid["agg"]:
                input_ac_count += 1

        print("CYCLE: ", i)
        print("VALID: ", valid)
        print("READY: ", ready)
        print("WRITE: ", [valid[key] & ready[key] for key in ready])
        

if __name__ == "__main__":
    main()
