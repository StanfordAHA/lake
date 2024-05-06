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

    sparse_helper.update_tcl("fiber_glb_crd_tb")


def create_random_stream(rate, size, dim, f_type="seg", allow_empty=False):
    assert rate >= 0 and rate <= 1, "Rate should be between 0 and 1"
    num_elem = size ** dim
    mat_1d = np.random.randint(-1024, 1024, size=num_elem)
    zero_index = random.sample(range(num_elem), int(num_elem * rate))
    for i in zero_index:
        mat_1d[i] = 0
    mat_nd = mat_1d.reshape((-1, size))
    mat_nd = mat_nd.tolist()
    # print(mat_nd)
    # print(mat_1d)

    if f_type == "seg":
        if allow_empty:
            seg = [0]
            cur_seg = 0
            crd = []
            for fiber in mat_nd:
                sub_crd = [e for e in fiber if e != 0]
                cur_seg += len(sub_crd)
                seg.append(cur_seg)
                crd += sub_crd
        else:
            seg = [0]
            for i in range(len(mat_nd)):
                seg.append((i+1)*size)
            crd = [i for i in range(len(mat_1d))]
        seg.insert(0, len(seg))
        crd.insert(0, len(crd))
        # print(seg)
        # print(crd)
        out_stream = seg + crd
    else:
        if allow_empty:
            val = mat_1d.tolist()
        else:
            val = [e for e in mat_1d if e != 0]
        val.insert(0, len(val))
        out_stream = val
    # print(out_stream)
    return out_stream

def check_streams(stream_out, gold):
    assert len(stream_out) == len(gold), "Output length didn't match gold length"
    for i in range(len(stream_out)):
        assert stream_out[i] == gold[i], f"Output value {stream_out[i]} didn't match gold value {gold[i]}"

def load_test_module(test_name):
    if test_name == "direct_l0":
        in_crd = [2, 0, 4, 4, 0, 1, 2, 3]
        return in_crd

    if test_name == "direct_l1":
        in_crd = [1885, 5, 0, 4, 7, 9, 12, 12, 0, 1, 2, 3, 0, 1, 3, 0, 2, 0, 1, 3]
        return in_crd, [1, 2]

    if test_name == "direct_l2":
        in_crd = [1, 19, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,\
                    19, 2, 4, 1, 4, 5, 9, 1, 0, 2, 7, 8, 2, 0, 8, 2, 0, 7, 1, 9]
        return in_crd, [1, 2]
    
    if test_name == "2stream_1":
        in_crd1 = [1, 3, 0, 1, 2, 2, 5, 6]
        in_crd2 = [1885, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10]

        gd = [[800, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10]]

        return in_crd1 + in_crd2, [2, 1], gd

    if test_name == "4stream_1":
        in_crd1 = [1, 3, 0, 1, 2, 2, 5, 6]
        in_crd2 = [1885, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10]
        in_crd3 = [1, 3, 0, 1, 2, 2, 5, 6]
        in_crd4 = [1885, 3, 0, 1, 2, 2, 5, 6]

        gd = [[800, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10], [900, 3, 0, 1, 2, 2, 5, 6]]

        return in_crd1 + in_crd2 + in_crd3 + in_crd4, [4, 2], gd

    if test_name == "diag":
        in_crd = [7, 0, 1, 2, 3, 4, 5, 6, 6, 1, 2, 3, 4, 5, 6]
        return in_crd

    if test_name == "empty":
        in_crd = [2, 0, 0, 0]
        return in_crd

    elif test_name[0:2] == "rd":
        size = int(test_name.split("_")[2])
        f_n = random.randint(1, 64)
        if test_name[3] == "1":
            f = random.sample(range(int(size * 1.5)), size)
            f.sort()
            f = [2, 0, size, size] + f
            return f
        fibers =[]
        size = size // f_n
        if size == 0:
            size = 1 #ensure there is something
        for i in range(f_n):
            crd = random.sample(range(int(size * 1.5)), size)
            crd.sort()
            fibers.append(crd)
        seg = []
        seg.append(len(fibers) + 1)
        seg.append(0)
        cur_ptr = 0
        for f in fibers:
            cur_ptr += len(f)
            seg.append(cur_ptr)
            assert len(f) > 0, "Fiber length is 0"
            assert len(f) == seg[-1] - seg[-2], "Fiber length is not consistent"
        crd = [seg[-1]]
        for f in fibers:
            crd += f
        return seg + crd
        
    else:
        in_crd = [2, 0, 1, 1, 0]
        return in_crd


def module_iter_basic(test_name):
    ic, tx_size, gc = load_test_module(test_name)

    print(ic)
    print(gc)

    sparse_helper.write_txt("stream_in_0.txt", ic)

    sparse_helper.clear_txt("stream_out.txt")

    seg_mode = 1

    #run command "make sim" to run the simulation
    sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_glb_crd_tb.sv", "TOP=fiber_glb_crd_tb",\
                            f"TX_NUM_0={tx_size[0]}", f"TX_NUM_1={tx_size[1]}", f"SEG_MODE={seg_mode}",\
                            "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)

    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    print(lines[1])

    stream_out = sparse_helper.read_glb_stream("stream_out.txt", tx_size[1], seg_mode)
    print(stream_out)

    #compare each element in the output from coord_out.txt with the gold output
    check_streams(stream_out, gc)
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    # test_list = ["direct_l0", "direct_l1", "direct_l2", "diag", "xxx"]
    # test_list = ["4stream_1"]
    test_list = ["2stream_1"]
    for test in test_list:
        module_iter_basic(test)


# def test_iter_random():
#     init_module()
#     for i in range(30):
#         size = random.randint(1, 200)
#         module_iter_basic(f"rd_1d_{size}")


# def test_iter_random():
#     init_module()
#     for i in range(30):
#         size = random.randint(1, 200)
#         module_iter_basic(f"rd_Nd_{size}")


# def test_iter_seq():
#     init_module()
#     module_iter_basic("direct_l0", "direct_l1")
#     module_iter_basic("direct_l2", "diag")


# def test_iter_seq_random():
#     init_module()
#     test_list = ["direct_l0", "direct_l1", "direct_l2", "diag"]
#     for i in range(100):
#         if i % 2 == 0:
#             test_list.append(f"rd_1d_{random.randint(1, 400)}")
#         else:
#             test_list.append(f"rd_Nd_{random.randint(1, 400)}")
#     for i in range(30):
#         rand = random.sample(test_list, 2)
#         module_iter_basic(rand[0], rand[1])
