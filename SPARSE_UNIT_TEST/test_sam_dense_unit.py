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

    sparse_helper.update_tcl("sam_dense_tb")

def create_random_fiber(rate, size, d, f_type = "coord", maybe = 0.0):
    # size = int(size*random.uniform(1.0, 1.0+d))

    s = int(rate*size)
    if f_type == "coord":
        crd = random.sample(range(size), s)
        crd.sort()
        return crd
    elif f_type == "pos":
        pos = random.sample(range(1, size + 1), s)
        pos.sort()
        if maybe != 0.0:
            replace = random.sample(range(0, len(pos)), int(len(pos)*maybe))
            for i in replace:
                pos[i] = 'N'
        return pos
    else:
        val = [random.uniform(0, 2**15-1) for i in range(s)]
        return val


def create_gold(in_ref):
    dense_dim = 7 # a constant the should be changed to match with the case for the tb
    rep = [i for i in range(dense_dim)]
    crd_out = []
    pos_out = []
    for i in range(len(in_ref)):
        if sparse_helper.is_DONE_sam(in_ref[i]):
            crd_out.append(in_ref[i])
            pos_out.append(in_ref[i])
        elif sparse_helper.is_STOP_sam(in_ref[i]):
            crd_out.append('S' + str(int(in_ref[i][-1]) + 1))
            pos_out.append('S' + str(int(in_ref[i][-1]) + 1))
        else:
            crd_out.extend(rep)
            pos_out.extend([i for i in range(int(in_ref[i]) * dense_dim, (int(in_ref[i]) + 1) * dense_dim)])
            if not sparse_helper.is_STOP_sam(in_ref[i+1]):
                crd_out.append('S0')
                pos_out.append('S0')

    # print(crd_out)
    # print(pos_out)

    st = [in_ref, crd_out, pos_out]
    tr_st = [convert_stream_to_onyx_interp(i) for i in st]

    return tr_st


def load_test_module(test_name):
    if test_name == "direct_2d":
        in_ref = [0, 1, 'S0', 'D']
        return create_gold(in_ref)
    
    elif test_name == "direct_1d":
        in_ref = [0, 'D']
        return create_gold(in_ref)

    elif test_name == "in_ref_2d_1":
        in_ref = [0, 'S0', 1, 'S2', 'D']
        return create_gold(in_ref)
    
    elif test_name == "in_ref_2d_2":
        in_ref = [0, 0, 'S0', 0, 'S1', 'D']
        return create_gold(in_ref)

    elif test_name == "in_ref_empty_fiber":
        in_ref = ['S0', 'S0', 1, 0, 'S0', 'S0', 1, 'S1', 'D']
        return create_gold(in_ref)

    elif test_name == "maybe_token":
        in_ref = [0, 'S0', 'D']
        return create_gold(in_ref)

    elif test_name == "empty_root_seq_1":
        in_ref = [0, 0, 'D']
        return create_gold(in_ref)

    elif test_name == "empty_root_seq_3":
        in_ref = ['S0', 5, 5, 0, 'S0', 3, 1, 'S1', 'D']
        return create_gold(in_ref)

    elif test_name == "arr_1":
        in_crd = [0, 1, 'S0', 2, 'S0', 3, 'S1', 'D']
        in_ref = [0, 1, 'N', 2, 'N', 'S0', 'D']
        return create_gold(in_ref)

    elif test_name == "arr_2":
        in_ref = [0, 'N', 1, 'N', 2, 'S0', 'D']
        return create_gold(in_ref)

    elif test_name == "arr_3":
        in_ref = [0, 1, 'N', 'N', 'S0', 2, 3, 'S0', 'N', 'N', 'S0', 4, 'S0', 'N', 'N', 'S1', 'D']
        return create_gold(in_ref)

    elif test_name == "seq_1_1":
        in_ref = ['N', 'N', 'S0', 'D']
        return create_gold(in_ref)

    else:
        in_ref = [0, 0, 'S1', 0, 'S3', 'D']
        return create_gold(in_ref)


def module_iter_basic(test_name, add_test=""):
    [ir, gc, gr] = load_test_module(test_name)

    # if add_test != "" and add_test != "void":
    #     additional_t = load_test_module(add_test)
    #     ic = ic + additional_t[0]
    #     ir = ir + additional_t[1]
    #     gc = gc + additional_t[2]
    #     gr = gr + additional_t[3]

    print(ir)

    sparse_helper.clear_txt("coord_in_0.txt")
    sparse_helper.write_txt("pos_in_0.txt", ir)

    sparse_helper.clear_txt("coord_out.txt")
    sparse_helper.clear_txt("pos_out_0.txt")

    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=sam_dense_tb.sv", "TOP=sam_dense_tb",\
                             "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=sam_dense_tb.sv",\
                             "TOP=sam_dense_tb", "TX_NUM_GLB=2", "TEST_UNIT=Fiber_access.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    # print(lines[0])
    # print(lines[1])

    coord_out = sparse_helper.read_txt("coord_out.txt", addit=add_test != "")
    pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", addit=add_test != "")
    print(coord_out)
    print(pos_out_0)

    # compare each element in the output from coord_out.txt with the gold output
    assert len(coord_out) == len(gc), \
        f"Output length {len(coord_out)} didn't match gold length {len(gc)}"
    for i in range(len(coord_out)):
        assert coord_out[i] == gc[i], \
            f"Output {coord_out[i]} didn't match gold {gc[i]} at index {i}"
    
    #compare each element in the output from pos_out_0.txt with the gold output
    assert len(pos_out_0) == len(gr), \
        f"Output length {len(pos_out_0)} didn't match gold length {len(gr)}"
    for i in range(len(pos_out_0)):
        assert pos_out_0[i] == gr[i], \
            f"Output {pos_out_0[i]} didn't match gold {gr[i]} at index {i}"
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    # test_list = ["direct_1d", "direct_2d", "in_ref_2d_1", "in_ref_2d_2", "in_ref_empty_fiber", "xxx"]
    test_list = ["arr_3"]
    #TODO: enable maybe token
    for test in test_list:
        module_iter_basic(test)
