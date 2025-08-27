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

FA_ID = 1885

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

    sparse_helper.update_tcl("fiber_glb2fib_seg_tb")


def create_gold(in_ref, blk_write):
    assert len(blk_write) % 2 == 0, "blk_write should be even"
    assert len(in_ref) == len(blk_write)/2, f"Length of block {len(in_ref)} didn't match reference {len(in_ref)}"
    in_ref_t = [convert_stream_to_onyx_interp(i) for i in in_ref]

    ref_gld = []
    crd_gld = []
    for i in range(len(in_ref)):
        sub_ref_gld = []
        sub_crd_gld = []
        for j in range(len(in_ref[i])):
            v = in_ref[i][j]
            if type(v) != str:
                assert v < blk_write[i*2][0] - 1, f"Value {v} out of range for block {i}"
                start = blk_write[i*2][1+v]
                end = blk_write[i*2][2+v]
                sub_crd_gld += blk_write[i*2+1][start+1:end+1]
                sub_ref_gld += [i for i in range(start, end)]

                if not sparse_helper.is_STOP_sam(in_ref[i][j+1]):
                    sub_crd_gld.append('S0')
                    sub_ref_gld.append('S0')
            elif sparse_helper.is_MAYBE_sam(v):
                if not sparse_helper.is_STOP_sam(in_ref[i][j+1]):
                    sub_crd_gld.append('S0')
                    sub_ref_gld.append('S0')
            elif sparse_helper.is_STOP_sam(v):
                level = int(v[1:]) + 1
                stop_token = f'S{level}'
                sub_crd_gld.append(stop_token)
                sub_ref_gld.append(stop_token)
            else:
                assert v == 'D', f"Value {v} not recognized"
                sub_crd_gld.append('D')
                sub_ref_gld.append('D')
        ref_gld.append(sub_ref_gld)
        crd_gld.append(sub_crd_gld)
    
    ref_gld_t = [convert_stream_to_onyx_interp(i) for i in ref_gld]
    crd_gld_t = [convert_stream_to_onyx_interp(i) for i in crd_gld]
    ref_gld_c = []
    for i in ref_gld_t:
        ref_gld_c += i
    crd_gld_c = []
    for i in crd_gld_t:
        crd_gld_c += i
    
    gld_stream = {"ref": ref_gld_c, "crd": crd_gld_c}
    return in_ref_t, gld_stream


def check_output(gd, stream):
    assert len(gd) == len(stream), f"Length of gold {len(gd)} didn't match output {len(stream)}"
    for i in range(len(gd)):
        assert gd[i] == stream[i], f"Gold {gd[i]} didn't match output {stream[i]}"

def load_test_module(test_name):
    if test_name == "direct_l0":
        in_ref = [[0, 1, 'S0', 'D']]
        blk_write = [[3, 0, 3, 6], [6, 0, 2, 3, 4, 5, 6]]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld

    if test_name == "empty_stop":
        in_ref = [['S0', 'D']]
        blk_write = [[2, 0, 0], [0]]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld

    if test_name == "empty_total":
        in_ref = [['D']]
        blk_write = [[1, 0], [0]]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld

    if test_name == "regular_b2b":
        in_ref = [[0, 1, 1, 'S0', 0, 0, 1, 0, 1, 'S1', 'D'], [0, 0, 0, 'S0', 'D']]
        blk_write = [[3, 0, 3, 8], [8, 0, 2, 3, 4, 5, 6, 7, 8], [2, 0, 4], [4, 1, 2, 10, 111]]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld

    if test_name == "empty_series":
        in_ref = [[0, 0, 'S0', 'D'], ['D'], ['D'], [0, 0, 0, 0, 'S1', 'D'], ['D'], [0, 0, 0, 'S2', 'D']]
        blk_write = [[2, 0, 3], [3, 0, 10, 100], [1, 0], [0], [1, 0], [0], [2, 0, 5], [5, 11, 29, 99, 112, 113], [1, 0], [0], [2, 0, 8], [8, 1, 2, 3, 4, 5, 6, 7, 8]]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld

    if test_name == "empty_read":
        in_ref = [[0, 'S0', 'D'], ['S1', 'D'], [0, 'S0', 1, 'S1', 'D']]
        blk_write = [[2, 0, 1], [1, 15], [10, 0, 2, 4, 6, 8, 10, 12, 14, 16, 18], [18, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17], [3, 0, 8, 10], [10, 1, 2, 3, 4, 5, 6, 7, 8, 19, 20]]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld

    if test_name == "all_empty_series":
        in_ref = [['S0', 'D'], ['N', 'S0', 'D'], ['D']]
        blk_write = [[2, 0, 0], [0], [1, 0], [0], [1, 0], [0],]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld

    if test_name == "single_ele_empty":
        in_ref = [[0, 'S0', 'D'], ['S0', 'D']]
        blk_write = [[2, 0, 1], [1, 0], [1, 0], [0],]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld
    
    if test_name == "single_ele_mul":
        in_ref = [[0, 'S0', 'D'], [0, 'S0', 'D'], [0, 'S0', 'D']]
        blk_write = [[2, 0, 1], [1, 1], [2, 0, 1], [1, 2], [2, 0, 1], [1, 3]]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld

    # elif test_name[0:2] == "rd":
    #     size = int(test_name.split("_")[1])
    #     vals = [random.randint(1, 600) for i in range(size)]
    #     v = [size] + vals
    #     return v
        
    else:
        in_ref = [[0, 'S0', 'D']]
        blk_write = [[2, 0, 1], [1, 2002]]
        in_ref_t, stream_gld = create_gold(in_ref, blk_write)
        return in_ref_t, blk_write, stream_gld


def module_iter_basic(test_name, add_test=""):
    ir, ib, stream_gld = load_test_module(test_name)


    print(ib)
    print(ir)
    TX_NUM = len(ib)
    ib_c = []
    for i in range(len(ib)):
        if i % 2 == 0:
            ib_c.append(FA_ID)
        ib_c += ib[i]

    ir_c = []
    for i in ir:
        ir_c += i
    # print(ib_c)
    print(TX_NUM)

    sparse_helper.write_txt("coord_in_0.txt", ib_c)
    sparse_helper.write_txt("pos_in_0.txt", ir_c)

    sparse_helper.clear_txt("coord_out.txt")
    sparse_helper.clear_txt("pos_out.txt")

    #run command "make sim" to run the simulation
    sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_glb2fib_seg_tb.sv", "TOP=fiber_glb2fib_seg_tb",\
                            f"TX_NUM_GLB={TX_NUM // 2}", "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)

    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    print(lines[1])

    coord_out = sparse_helper.read_txt("coord_out.txt", count=TX_NUM//2)
    pos_out = sparse_helper.read_txt("pos_out.txt", count=TX_NUM//2)
    print(coord_out)
    print(pos_out)

    #compare each element in the output from coord_out.txt with the gold output
    check_output(coord_out, stream_gld["crd"])
    check_output(pos_out, stream_gld["ref"])
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["direct_l0", "empty_stop", "empty_total", "regular_b2b", "empty_series", "empty_read", "all_empty_series", "single_ele_empty", "single_ele_mul", "xxx"]
    for test in test_list:
        module_iter_basic(test)
