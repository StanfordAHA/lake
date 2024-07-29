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

    sparse_helper.update_tcl("fiber_glb_tb")


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

def create_gold(ic, FA_ID, base, stride):
    gc = []
    cur_addr = base
    for i in ic:
        if i[0] == FA_ID:
            o_s = i[:]
            o_s[0] = cur_addr
            cur_addr += stride
            gc.append(o_s)
    return gc

def check_streams(stream_out, gold):
    assert len(stream_out) == len(gold), "Output length didn't match gold length"
    for i in range(len(stream_out)):
        assert stream_out[i] == gold[i], f"Output value {stream_out[i]} didn't match gold value {gold[i]}"

def load_test_module(test_name):
    FA_ID = 1885
    base = 800
    stride = 100

    if test_name == "1stream_1_crd":
        in_crd = [[1885, 5, 0, 4, 7, 9, 12, 12, 0, 1, 2, 3, 0, 1, 3, 0, 2, 0, 1, 3]]
        gc = create_gold(in_crd, FA_ID, base, stride)
        seg_mode = 1
        return in_crd, gc, seg_mode

    if test_name == "1stream_1_val":
        in_crd = [[1885, 5, 0, 4, 7, 9, 12]]
        gc = create_gold(in_crd, FA_ID, base, stride)
        seg_mode = 0
        return in_crd, gc, seg_mode
    
    if test_name == "2stream_1_crd":
        in_crd1 = [1, 3, 0, 1, 2, 2, 5, 6]
        in_crd2 = [1885, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10]
        ic = [in_crd1, in_crd2]

        # gc = [[800, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10]]
        gc = create_gold(ic, FA_ID, base, stride)
        seg_mode = 1

        return ic, gc, seg_mode


    if test_name == "2stream_1_val":
        in_crd1 = [1, 3, 0, 1, 2]
        in_crd2 = [1885, 5, 0, 1, 2, 5, 6]
        ic = [in_crd1, in_crd2]

        # gc = [[800, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10]]
        gc = create_gold(ic, FA_ID, base, stride)
        seg_mode = 0

        return ic, gc, seg_mode

    if test_name == "4stream_1_crd":
        in_crd1 = [1, 3, 0, 1, 2, 2, 5, 6]
        in_crd2 = [1885, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10]
        in_crd3 = [1, 3, 0, 1, 2, 2, 5, 6]
        in_crd4 = [1885, 3, 0, 1, 2, 2, 5, 6]
        ic = [in_crd1, in_crd2, in_crd3, in_crd4]

        # gc = [[800, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10], [900, 3, 0, 1, 2, 2, 5, 6]]
        gc = create_gold(ic, FA_ID, base, stride)
        seg_mode = 1

        return ic, gc, seg_mode


    if test_name == "4stream_1_val":
        in_crd1 = [1, 3, 0, 1, 2]
        in_crd2 = [1885, 5, 0, 1, 2, 5, 6]
        in_crd3 = [1, 3, 0, 1, 2]
        in_crd4 = [1885, 3, 0, 1, 2]
        ic = [in_crd1, in_crd2, in_crd3, in_crd4]

        # gc = [[800, 5, 0, 1, 2, 5, 6, 6, 5, 6, 7, 8, 9, 10], [900, 3, 0, 1, 2, 2, 5, 6]]
        gc = create_gold(ic, FA_ID, base, stride)
        seg_mode = 0

        return ic, gc, seg_mode

    elif test_name[0:2] == "rd":
        args = test_name.split("_")
        single = args[1] == "single"
        seg_mode = args[3] == "crd"

        # parameter
        total_st = 16
        if single:
            active_st = total_st
        else:
            active_st = random.randint(1, total_st)
        
        if seg_mode:
            f_type = "seg"
        else:
            f_type = "val"

        rate_range = [0.8, 1.0]
        dim = [1, 2]
        size_range = [1, 8]

        allow_empty = False

        mask_stream = [i for i in range(total_st)]
        mask_stream = random.sample(mask_stream, active_st)
        for i in range(total_st):
            f = create_random_stream(random.uniform(rate_range[0], rate_range[1]), random.randint(size_range[0], size_range[1]), random.choice(dim), f_type, allow_empty)
            if i in mask_stream:
                f.insert(0, FA_ID)
                ic.append(f)
            else:
                id = random.randint(0, 4095)
                while id == FA_ID:
                    id = random.randint(0, 4095)
                f.insert(0, id)
                ic.append(f)
        
        gc = create_gold(ic, FA_ID, base, stride)
        if seg_mode:
            seg_mode = 1
        else:
            seg_mode = 0
        return ic, gc, seg_mode

        
    else:
        raise Exception("Invalid test name")


def module_iter_basic(test_name):
    ic, gc, seg_mode = load_test_module(test_name)

    print(ic)
    print(gc)
    ic_s = []
    for i in ic:
        ic_s = ic_s + i
    print(ic_s)
    print(seg_mode)

    sparse_helper.write_txt("stream_in_0.txt", ic_s)

    sparse_helper.clear_txt("stream_out.txt")

    #crd mode and seg mode are the same. seg mode and val mode are different
    #run command "make sim" to run the simulation
    sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_glb_tb.sv", "TOP=fiber_glb_tb",\
                            f"TX_NUM_0={len(ic)}", f"TX_NUM_1={len(gc)}", f"SEG_MODE={seg_mode}",\
                            "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)

    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    print(lines[1])

    stream_out = sparse_helper.read_glb_stream("stream_out.txt", len(gc), seg_mode)
    print(stream_out)

    #compare each element in the output from coord_out.txt with the gold output
    check_streams(stream_out, gc)
    
    print(test_name, " passed\n")


# def test_iter_crd_basic():
#     init_module()
#     test_list = ["1stream_1_crd", "2stream_1_crd", "4stream_1_crd"]
#     for test in test_list:
#         module_iter_basic(test)


# def test_iter_val_basic():
#     init_module()
#     test_list = ["1stream_1_val", "2stream_1_val", "4stream_1_val"]
#     for test in test_list:
#         module_iter_basic(test)


def test_single_stream_random_crd():
    init_module()
    for test in range(30):
        module_iter_basic("rd_single_crd")


# def test_single_stream_random_val():
#     init_module()
#     for test in range(30):
#         module_iter_basic("rd_single_val")


# def test_multi_stream_random_crd():
#     init_module()
#     for test in range(30):
#         module_iter_basic("rd_multi_crd")


# def test_multi_stream_random_val():
#     init_module()
#     for test in range(30):
#         module_iter_basic("rd_multi_val")
