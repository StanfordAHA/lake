from lake.top.fiber_access import FiberAccess
from lake.top.tech_maps import GF_Tech_Map
import magma as m
from magma import *
import tempfile
# from kratos import *
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.test.test import TIMEOUT
from sam.sim.src.rd_scanner import CompressedCrdRdScan
from sam.sim.src.wr_scanner import ValsWrScan, CompressWrScan


import subprocess
import os
import random
random.seed(15)
import string


def init_module():
    data_width = 16
    mem_depth = 512
    mem_width = 64
    macro_width = 32
    dual_port = False
    pipeline_scanner = True
    fiber_access = FiberAccess(data_width=16,
                        mem_width=64,
                        tb_harness=True,
                        local_memory=False,
                        tech_map=GF_Tech_Map(depth=mem_depth, width=macro_width, dual_port=dual_port),
                        defer_fifos=False,
                        add_flush=True,
                        use_pipelined_scanner=pipeline_scanner,
                        fifo_depth=2,
                        buffet_optimize_wide=True,
                        perf_debug=False)

    k.verilog(fiber_access, filename=f"./modules/Fiber_access.sv", optimize_if=False)

    sparse_helper.update_tcl("fiber_fib2glb_seg_tb")


def create_gold(in_crd, in_ref):
    i_c_cpy = in_crd[:]
    i_r_cpy = in_ref[:]

    # print(in_crd)
    # print(in_ref)
    # print("============")
    # process write
    buf_size = 1000
    fill = 0
    done = False
    time = 0

    wrscan = CompressWrScan(size=buf_size, seg_size=buf_size, fill=fill)

    while not done and time < TIMEOUT:
        if len(in_crd) > 0:
            wrscan.set_input(in_crd.pop(0))

        wrscan.update()

        # print("Timestep", time, "\t WrScan:", wrscan.out_done())

        done = wrscan.out_done()
        time += 1

    print("sam write cycle count: ", time)
    # print(wrscan.get_seg_arr())
    # print(wrscan.get_arr())
    crdscan = CompressedCrdRdScan(seg_arr=wrscan.get_seg_arr(), crd_arr=wrscan.get_arr())
    done = False
    time = 0
    out_crd = []
    out_ref = []

    while not done and time < TIMEOUT:
        if len(in_ref) > 0:
            crdscan.set_in_ref(in_ref.pop(0))

        crdscan.update()

        out_crd.append(crdscan.out_crd())
        out_ref.append(crdscan.out_ref())

        # print("Timestep", time, "\t Crd:", crdscan.out_crd(), "\t Ref:", crdscan.out_ref())

        done = crdscan.done
        time += 1

    print("sam read cycle count: ", time)
    
    out_crd = remove_emptystr(out_crd)
    out_ref = remove_emptystr(out_ref)

    # print(out_crd)
    # print(out_ref)

    out_c = []
    out_r = []
    for i in range(len(out_crd)):
        if out_crd[i] != 'N':
            out_c.append(out_crd[i])
            out_r.append(out_ref[i])
    st = [i_c_cpy, i_r_cpy, out_c, out_r]
    tr_st = [convert_stream_to_onyx_interp(i) for i in st]

    return tr_st


def check_output(gd, coord_out):
    assert len(gd) == len(coord_out), f"Length of gold {len(gd)} didn't match output {len(coord_out)}"
    for i in range(len(gd)):
        assert len(gd[i]) == len(coord_out[i]), f"Length of gold {len(gd[i])} didn't match output {len(coord_out[i])}"
        for j in range(len(gd[i])):
            assert gd[i][j] == coord_out[i][j], f"Gold {gd[i][j]} didn't match output {coord_out[i][j]}"


def load_test_module(test_name):
    if test_name == "direct_l0":
        in_val = [[0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']]
        blk_gold = [[3, 0, 3, 6], [6, 0, 2, 3, 4, 5, 6]]
        in_val_t = [convert_stream_to_onyx_interp(i) for i in in_val]
        return in_val_t, blk_gold

    if test_name == "empty_stop":
        in_val = [['S0', 'D']]
        blk_gold = [[2, 0, 0], [0]]
        in_val_t = [convert_stream_to_onyx_interp(i) for i in in_val]
        return in_val_t, blk_gold

    if test_name == "empty_total":
        in_val = [['D']]
        blk_gold = [[1, 0], [0]]
        in_val_t = [convert_stream_to_onyx_interp(i) for i in in_val]
        return in_val_t, blk_gold

    if test_name == "regular_b2b":
        in_val = [[0, 2, 3, 'S0', 4, 5, 6, 7, 8, 'S1', 'D'], [1, 2, 10, 111, 'S0', 'D']]
        blk_gold = [[3, 0, 3, 8], [8, 0, 2, 3, 4, 5, 6, 7, 8], [2, 0, 4], [4, 1, 2, 10, 111]]
        in_val_t = [convert_stream_to_onyx_interp(i) for i in in_val]
        return in_val_t, blk_gold

    if test_name == "empty_series":
        in_val = [[0, 10, 100, 'S0', 'D'], ['D'], ['D'], [11, 29, 99, 112, 113, 'S1', 'D'], ['D'], [1, 2, 3, 4, 5, 6, 7, 8, 'S2', 'D']]
        blk_gold = [[2, 0, 3], [3, 0, 10, 100], [1, 0], [0], [1, 0], [0], [2, 0, 5], [5, 11, 29, 99, 112, 113], [1, 0], [0], [2, 0, 8], [8, 1, 2, 3, 4, 5, 6, 7, 8]]
        in_val_t = [convert_stream_to_onyx_interp(i) for i in in_val]
        return in_val_t, blk_gold

    if test_name == "all_empty_series":
        in_val = [['S0', 'D'], ['D'], ['D']]
        blk_gold = [[2, 0, 0], [0], [1, 0], [0], [1, 0], [0],]
        in_val_t = [convert_stream_to_onyx_interp(i) for i in in_val]
        return in_val_t, blk_gold

    if test_name == "single_ele_empty":
        in_val = [[0, 'S0', 'D'], ['S0', 'D']]
        blk_gold = [[2, 0, 1], [1, 0], [1, 0], [0],]
        in_val_t = [convert_stream_to_onyx_interp(i) for i in in_val]
        return in_val_t, blk_gold
    
    if test_name == "single_ele_mul":
        in_val = [[1, 'S0', 'D'], [2, 'S0', 'D'], [3, 'S0', 'D']]
        blk_gold = [[2, 0, 1], [1, 1], [2, 0, 1], [1, 2], [2, 0, 1], [1, 3]]
        in_val_t = [convert_stream_to_onyx_interp(i) for i in in_val]
        return in_val_t, blk_gold

    # elif test_name[0:2] == "rd":
    #     size = int(test_name.split("_")[1])
    #     vals = [random.randint(1, 600) for i in range(size)]
    #     v = [size] + vals
    #     return v
        
    else:
        in_val = [[2002, 'S0', 'D']]
        blk_gold = [[2, 0, 1], [1, 2002]]
        in_val_t = [convert_stream_to_onyx_interp(i) for i in in_val]
        return in_val_t, blk_gold


def module_iter_basic(test_name, add_test=""):
    iv, blk_gold = load_test_module(test_name)

    print(iv)
    print(blk_gold)
    TX_NUM = len(blk_gold)
    iv_c = []
    for i in iv:
        iv_c += i
    print(iv_c)
    print(TX_NUM)

    sparse_helper.write_txt("coord_in_0.txt", iv_c)

    sparse_helper.clear_txt("coord_out.txt")

    #run command "make sim" to run the simulation
    sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_fib2glb_seg_tb.sv", "TOP=fiber_fib2glb_seg_tb",\
                            f"TX_NUM_GLB={TX_NUM // 2}", "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)

    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    print(lines[1])

    coord_out = sparse_helper.read_glb("coord_out.txt", tx_num=TX_NUM)
    print(coord_out)

    #compare each element in the output from coord_out.txt with the gold output
    check_output(blk_gold, coord_out)
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["direct_l0", "regular_b2b", "xxx", "regular_b2b", "empty_series", "all_empty_series", "empty_stop", "empty_total", "single_ele_mul"]
    for test in test_list:
        module_iter_basic(test)
