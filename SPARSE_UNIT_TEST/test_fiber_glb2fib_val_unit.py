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

    sparse_helper.update_tcl("fiber_glb2fib_val_tb")


def create_gold(in_blk, in_ref):
    assert len(in_blk) == len(in_ref), f"Length of block {len(in_blk)} didn't match reference {len(in_ref)}"
    ir_t = [convert_stream_to_onyx_interp(i) for i in in_ref]

    gld = []
    for i in range(len(in_blk)):
        gld_sub = []
        for j in range(len(in_ref[i])):
            v = in_ref[i][j]
            if type(v) != str:
                assert v < len(in_blk[i]) - 1, f"Value {v} out of range for block {i}"
                gld_sub.append(in_blk[i][v+1])
            elif v == 'N':
                gld_sub.append(0)
            else:
                gld_sub.append(v)
        gld.append(gld_sub)

    gld_t = [convert_stream_to_onyx_interp(i) for i in gld]
    return ir_t, gld_t


def check_output(gd, coord_out):
    assert len(gd) == len(coord_out), f"Length of gold {len(gd)} didn't match output {len(coord_out)}"
    for j in range(len(gd)):
        assert gd[j] == coord_out[j], f"Gold {gd[j]} didn't match output {coord_out[j]}"


def load_test_module(test_name):
    if test_name == "direct_l0":
        ir = [[0, 'S0', 'D']]
        ib = [[6, 77, 2, 3, 4, 5, 6]]
        ir_t, stream_gld = create_gold(ib, ir)
        return ib, ir_t, stream_gld

    if test_name == "direct_l1":
        ir = [[0, 'S1', 'D']]
        ib = [[7, 9, 10, 2, 1, 1, 19, 9]]
        ir_t, stream_gld = create_gold(ib, ir)
        return ib, ir_t, stream_gld

    if test_name == "empty_read":
        ir = [['S0', 'D']]
        ib = [[7, 9, 10, 2, 1, 1, 19, 9]]
        ir_t, stream_gld = create_gold(ib, ir)
        return ib, ir_t, stream_gld

    if test_name == "empty_write_series":
        ir = [['S0', 'D'], [1, 0, 'D'], [0, 1, 2, 'D']]
        ib = [[1, 0], [3, 19, 11, 13], [4, 1, 2, 3, 4]]
        ir_t, stream_gld = create_gold(ib, ir)
        return ib, ir_t, stream_gld

    if test_name == "regular_b2b":
        ir = [[0, 0, 'S0', 'D'], [0, 'D']]
        ib = [[8, 0, 2, 3, 4, 5, 6, 7, 8], [4, 1, 1, 1, 1]]
        ir_t, stream_gld = create_gold(ib, ir)
        return ib, ir_t, stream_gld

    if test_name == "empty_series":
        ir = [[0, 0, 'S0', 'D'], ['D'], [0, 'D'], [2, 0, 1, 'S1', 'D'], [0, 'S0', 'D'], [0, 1, 2, 3, 'S0', 'D']]
        ib = [[3, 1, 1, 1], [6, 1, 2, 3, 4, 5, 6], [3, 1, 11, 111], [5, 11, 29, 9, 7, 3], [7, 18, 1, 21, 77, 2, 3, 4], [8, 1, 2, 3, 4, 5, 6, 7, 8]]
        ir_t, stream_gld = create_gold(ib, ir)
        return ib, ir_t, stream_gld

    if test_name == "single_ele_mul":
        ir = [[0, 0, 'S0', 'D'], ['D'], [0, 'D'], [0, 0, 'S1', 'D']]
        ib = [[1, 1], [1, 2], [1, 3], [1, 4]]
        ir_t, stream_gld = create_gold(ib, ir)
        return ib, ir_t, stream_gld

    # elif test_name[0:2] == "rd":
    #     size = int(test_name.split("_")[1])
    #     vals = [random.randint(1, 600) for i in range(size)]
    #     v = [size] + vals
    #     return v
        
    else:
        ir = [[0, 0, 'S0', 'D']]
        ib = [[1, 2002]]
        ir_t, stream_gld = create_gold(ib, ir)
        return ib, ir_t, stream_gld


def module_iter_basic(test_name, add_test=""):
    ib, ir, stream_gld = load_test_module(test_name)

    print(ib)
    TX_NUM = len(ib)
    ib_c = []
    for i in ib:
        ib_c.append(FA_ID)
        ib_c += i
    ir_c = []
    for i in ir:
        ir_c += i
    print(ib_c)
    print(TX_NUM)
    stream_gld_c = []
    for i in stream_gld:
        stream_gld_c += i

    sparse_helper.write_txt("coord_in_0.txt", ib_c)
    sparse_helper.write_txt("pos_in_0.txt", ir_c)

    sparse_helper.clear_txt("coord_out.txt")

    #run command "make sim" to run the simulation
    sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_glb2fib_val_tb.sv", "TOP=fiber_glb2fib_val_tb",\
                            f"TX_NUM_GLB={TX_NUM}", "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)

    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    print(lines[1])

    coord_out = sparse_helper.read_txt("coord_out.txt", count=TX_NUM)
    print(coord_out)

    #compare each element in the output from coord_out.txt with the gold output
    check_output(stream_gld_c, coord_out)
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    # test_list = ["direct_l0", "direct_l1", "empty_read", "regular_b2b", "empty_series", "single_ele_mul", "xxx"]
    test_list = ["empty_write_series"]
    for test in test_list:
        module_iter_basic(test)
