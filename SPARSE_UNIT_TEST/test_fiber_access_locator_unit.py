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

    sparse_helper.update_tcl("fiber_access_locator_tb")

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

def create_random(n, rate, size, d1=0, maybe = 0.0): #d1 is the num of the outer level
    if n == 1:
        in_crd1 = create_random_fiber(rate, size, 0.2, "coord", maybe) + ['S0', 'D']
        return in_crd1
    
    elif n == 2:
        if d1 == 0:
            d1 = int(random.uniform(2, int(size**(1/2))))
        d2 = size // d1
        rate = rate**(1/2)
        ret = []
        for i in range(d1):
            ret_t = []
            if random.random() < rate:
                ret_t = ret_t + create_random_fiber(rate, d2, 0.2, "coord", maybe)
            if i == d1 - 1:
                ret = ret + ret_t + ['S1', 'D']
            else:
                ret = ret + ret_t + ['S0']
        return ret
    elif n == 3:
        if d1 == 0:
            d1 = int(random.uniform(2, int(size**(1/3))))
        d2 = size // d1
        d1_ = int(random.uniform(2, int(d2**(1/2))))
        rate = rate**(1/3)
        ret = []
        # print(d1, d2, rate)
        for i in range(d1):
            ret_t = []
            if random.random() < rate:
                ret_t = create_random(2, rate*rate, d2, d1_)
            else:
                ret_t = ret_t + ['S1', 'D']

            if i == d1 - 1:
                ret = ret + ret_t[:-2] + ['S2', 'D']
            else:
                ret = ret + ret_t[:-1]
        return ret


def create_ref(coord, dim, rate, size, maybe = True):
    ran = []
    k = 0
    for i in coord:
        if sparse_helper.is_STOP_sam(i):
            ran.append(k)
            k += 1
    if maybe:
        ran.append('N')
    t = create_random(dim, rate, size)
    for j in range(len(t)):
        if type(t[j]) is int:
            t[j] = ran[int(random.uniform(0, len(ran)))]
    return t


def create_gold(density, dim_size, in_ref):
    i_r_cpy = in_ref[:]

    mem = [0 for i in range(dim_size)]
    non_empty_locations = random.sample(range(0, dim_size), int(density*dim_size)) 

    addr = 0 
    for i in range(0, len(mem)):
        if i in non_empty_locations:
            mem[i] = addr
            addr = addr + 1
        else:
            # no corresponding lower level coordinates
            mem[i] = 65535

    max_addr = len(mem) - 1

    print("Sam assumes 0-delay memory access")

    out_v = []
    for addr in in_ref:
        if sparse_helper.is_MAYBE_sam(addr):
            out_v.append(0)
        elif (not sparse_helper.is_STOP_sam(addr) and not sparse_helper.is_DONE_sam(addr)):
            assert addr <= max_addr
            # if there's no corresponding lower level coordinates, do not output anything 
            if mem[addr] != 65535:
             out_v.append(mem[addr])
        else:
            out_v.append(addr)

    # encode the content of the locator memory into block mode format for the glb
    encoded_mem = [1885, len(mem)]
    encoded_mem.extend(mem)
    st = [encoded_mem, i_r_cpy, out_v]
    tr_st = [convert_stream_to_onyx_interp(i) for i in st]

    return tr_st


def load_test_module(test_name):
    if test_name == "fully_dense_full_scan_1d":
        density = 1.0
        dim_size = 10
        in_ref = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 'S0', 'D']
        return create_gold(density, dim_size, in_ref)

    elif test_name == "sparse_full_scan_1d":
        density = 0.5
        dim_size = 10
        in_ref = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 'S0', 'D']
        return create_gold(density, dim_size, in_ref)

    elif test_name == "fully_dense_random_scan_1d":
        density = 1.0
        dim_size = 10
        in_ref = [0, 2, 4, 6, 8, 'S0', 'D']
        return create_gold(density, dim_size, in_ref)

    elif test_name == "sparse_random_scan_1d":
        density = 0.5
        dim_size = 10
        in_ref = [0, 2, 5, 6, 7, 'S0', 'D'] 
        return create_gold(density, dim_size, in_ref)
    
    elif test_name == "maybe_token_1d":
        density = 0.5
        dim_size = 10
        in_ref = [0, 'N', 2, 'N', 5, 6, 'N', 9, 'S0', 'D']
        return create_gold(density, dim_size, in_ref)
    
    elif test_name == "fully_dense_full_scan_2d":
        density = 1.0
        dim_size = 20
        in_ref = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 'S0', 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 'S1', 'D']
        return create_gold(density, dim_size, in_ref)
    
    elif test_name == "sparse_full_scan_2d":
        density = 0.5
        dim_size = 20
        in_ref = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 'S0', 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 'S1', 'D']
        return create_gold(density, dim_size, in_ref)
    
    elif test_name == "fully_dense_random_scan_2d":
        density = 1.0
        dim_size = 20
        in_ref = [0, 1, 2, 3, 6, 8, 'S0', 10, 12, 15, 16, 17, 19, 'S1', 'D']
        return create_gold(density, dim_size, in_ref)
    
    elif test_name == "sparse_random_scan_2d":
        density = 0.5
        dim_size = 20
        in_ref = [0, 1, 2, 3, 7, 8, 'S0', 11, 13, 15, 16, 17, 19, 'S1', 'D']
        return create_gold(density, dim_size, in_ref)
    
    elif test_name == "maybe_token_2d":
        density = 0.5
        dim_size = 20
        in_ref = [0, 'N', 2, 'N', 5, 6, 'N', 9, 'S0', 'N', 'N', 13, 14, 'N', 18, 'S1', 'D']
        return create_gold(density, dim_size, in_ref)
    
    elif test_name == "empty_fiber":
        density = 0.5
        dim_size = 20
        in_ref = ['S0', 'S0', 0, 1, 2, 3, 4, 9, 'S1', 'D']
        return create_gold(density, dim_size, in_ref)

def module_iter_basic(test_name, add_test=""):
    [ic, ir, gv] = load_test_module(test_name)

    if add_test != "" and add_test != "void":
        additional_t = load_test_module(add_test)
        ic = ic + additional_t[0]
        ir = ir + additional_t[1]
        gv = gv + additional_t[2]

    print(ic)
    print(ir)
    print(gv)

    sparse_helper.write_txt("coord_in_0.txt", ic)
    sparse_helper.write_txt("pos_in_0.txt", ir)

    sparse_helper.clear_txt("coord_out.txt")

    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_access_locator_tb.sv", "TOP=fiber_access_locator_tb",\
                             "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_access_locator_tb.sv",\
                             "TOP=fiber_access_locator_tb", "TX_NUM_GLB=2", "TEST_UNIT=Fiber_access.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    print(lines[1])

    tx_num = 1
    if add_test != "":
        tx_num = 2
    value_out = sparse_helper.read_txt("coord_out.txt", count=tx_num)
    print(value_out)

    #compare each element in the output from coord_out.txt with the gold output
    assert len(value_out) == len(gv), \
        f"Output length {len(value_out)} didn't match gold length {len(gv)}"
    for i in range(len(value_out)):
        assert value_out[i] == gv[i], \
            f"Output {value_out[i]} didn't match gold {gv[i]} at index {i}"
    
    print(test_name, " passed\n")


# def test_1d():
#     init_module()
#     test_list = ["fully_dense_full_scan_1d", 
#                  "sparse_full_scan_1d", 
#                  "fully_dense_random_scan_1d",
#                  "sparse_random_scan_1d",
#                  "maybe_token_1d"]
#     for test in test_list:
#         module_iter_basic(test)

def test_basic_2d():
    init_module()
    test_list = ["fully_dense_full_scan_2d",
                 "sparse_full_scan_2d",
                 "fully_dense_random_scan_2d",
                 "sparse_random_scan_2d",
                 "maybe_token_2d",
                 "empty_fiber"]
    for test in test_list:
        module_iter_basic(test)

def test_seq_rd():
    init_module()
    test_list = ["fully_dense_full_scan_1d", 
                 "sparse_full_scan_1d", 
                 "fully_dense_random_scan_1d",
                 "sparse_random_scan_1d",
                 "maybe_token_1d",
                 "fully_dense_full_scan_2d",
                 "sparse_full_scan_2d",
                 "fully_dense_random_scan_2d",
                 "sparse_random_scan_2d",
                 "maybe_token_2d",
                 "empty_fiber"]
    for i in range(0, 20):
        random_test_cases = []
        random_test_cases.append(random.choice(test_list))
        random_test_cases.append(random.choice(test_list))
        module_iter_basic(random_test_cases[0], random_test_cases[1])


