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


def load_test_module(test_name):
    if test_name == "direct_l0":
        in_crd = [2, 0, 4, 4, 0, 1, 2, 3]
        return in_crd

    if test_name == "direct_l1":
        in_crd = [5, 0, 4, 7, 9, 12, 12, 0, 1, 2, 3, 0, 1, 3, 0, 2, 0, 1, 3]
        return in_crd

    if test_name == "direct_l2":
        in_crd = [19, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,\
                    19, 2, 4, 1, 4, 5, 9, 1, 0, 2, 7, 8, 2, 0, 8, 2, 0, 7, 1, 9]
        return in_crd

    if test_name == "diag":
        in_crd = [7, 0, 1, 2, 3, 4, 5, 6, 6, 1, 2, 3, 4, 5, 6]
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


def module_iter_basic(test_name, add_test=""):
    ic = load_test_module(test_name)

    if add_test != "" and add_test != "void":
        additional_t = load_test_module(add_test)
        ic = ic + additional_t

    print(ic)
    gc = ic[:]

    sparse_helper.write_txt("coord_in_0.txt", ic)

    sparse_helper.clear_txt("coord_out.txt")
    TX_NUM = 2
    if add_test != "":
        TX_NUM = 4

    #run command "make sim" to run the simulation
    sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_glb_crd_tb.sv", "TOP=fiber_glb_crd_tb",\
                            f"TX_NUM_GLB={TX_NUM}", "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)

    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    print(lines[1])

    coord_out_t = sparse_helper.read_glb("coord_out.txt", TX_NUM)
    coord_out = []
    for i in coord_out_t:
        coord_out += i
    # print(coord_out)

    #compare each element in the output from coord_out.txt with the gold output
    assert len(coord_out) == len(gc), \
        f"Output length {len(coord_out)} didn't match gold length {len(gc)}"
    for i in range(len(coord_out)):
        assert coord_out[i] == gc[i], \
            f"Output {coord_out[i]} didn't match gold {gc[i]} at index {i}"
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["direct_l0", "direct_l1", "direct_l2", "diag", "xxx"]
    for test in test_list:
        module_iter_basic(test)


def test_iter_random():
    init_module()
    for i in range(30):
        size = random.randint(1, 200)
        module_iter_basic(f"rd_1d_{size}")


def test_iter_random():
    init_module()
    for i in range(30):
        size = random.randint(1, 200)
        module_iter_basic(f"rd_Nd_{size}")


def test_iter_seq():
    init_module()
    module_iter_basic("direct_l0", "direct_l1")
    module_iter_basic("direct_l2", "diag")


def test_iter_seq_random():
    init_module()
    test_list = ["direct_l0", "direct_l1", "direct_l2", "diag"]
    for i in range(100):
        if i % 2 == 0:
            test_list.append(f"rd_1d_{random.randint(1, 400)}")
        else:
            test_list.append(f"rd_Nd_{random.randint(1, 400)}")
    for i in range(30):
        rand = random.sample(test_list, 2)
        module_iter_basic(rand[0], rand[1])
