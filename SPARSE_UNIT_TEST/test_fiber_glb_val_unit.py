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

    sparse_helper.update_tcl("fiber_glb_val_tb")


def load_test_module(test_name):
    if test_name == "direct_l0":
        in_crd = [2, 0, 4]
        return in_crd

    if test_name == "direct_l1":
        in_crd = [5, 10, 4, 7, 9, 12]
        return in_crd

    if test_name == "direct_l2":
        in_crd = [19, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
        return in_crd

    if test_name == "diag":
        in_crd = [7, 0, 1, 2, 3, 4, 5, 6]
        return in_crd

    if test_name == "empty":
        in_crd = [0]
        return in_crd

    elif test_name[0:2] == "rd":
        size = int(test_name.split("_")[1])
        vals = [random.randint(1, 600) for i in range(size)]
        v = [size] + vals
        return v
        
    else:
        in_crd = [2, 0, 0]
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
    TX_NUM = 1
    if add_test != "":
        TX_NUM = 2

    #run command "make sim" to run the simulation
    sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_glb_val_tb.sv", "TOP=fiber_glb_val_tb",\
                            f"TX_NUM_GLB={TX_NUM}", "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)

    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    print(lines[1])

    coord_out_t = sparse_helper.read_glb("coord_out.txt", tx_num=TX_NUM)
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


# def test_iter_basic():
#     init_module()
#     test_list = ["direct_l0", "direct_l1", "direct_l2", "diag", "xxx"]
#     for test in test_list:
#         module_iter_basic(test)


def test_iter_random():
    init_module()
    for i in range(30):
        size = i + 1
        module_iter_basic(f"rd_{size}")


def test_iter_seq():
    init_module()
    module_iter_basic("direct_l0", "direct_l1")
    module_iter_basic("direct_l2", "diag")


def test_iter_random_sweep():
    init_module()
    for i in range(30):
        size = i + 1
        module_iter_basic(f"rd_{size}", f"rd_{size}")
