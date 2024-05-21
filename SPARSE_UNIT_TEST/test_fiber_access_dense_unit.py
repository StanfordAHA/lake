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
from sam.sim.src.rd_scanner import UncompressCrdRdScan
from sam.sim.src.wr_scanner import CompressWrScan


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

    sparse_helper.update_tcl("fiber_access_dense_tb")

def create_random_fiber(rate, size, maybe = 0.0):
    s = int(rate*size)
    pos = random.sample(range(0, size + 1), s)
    pos.sort()
    if maybe != 0.0:
        replace = random.sample(range(0, len(pos)), int(len(pos)*maybe))
        for i in replace:
            pos[i] = 'N'
    return pos

def create_random(n, rate, size, d1=0, maybe=0.0): #d1 is the num of the outer level
    if n == 1:
        in_crd1 = create_random_fiber(rate, size, maybe) + ['S0', 'D']
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
                ret_t = ret_t + create_random_fiber(rate, d2, maybe)
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
        for i in range(d1):
            ret_t = []
            if random.random() < rate:
                ret_t = create_random(2, rate*rate, d2, d1_, maybe)
            else:
                ret_t = ret_t + ['S1', 'D']

            if i == d1 - 1:
                ret = ret + ret_t[:-2] + ['S2', 'D']
            else:
                ret = ret + ret_t[:-1]
        return ret


def create_ref(coord, dim, rate, size, maybe = False):
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


def create_gold(dim_size, in_ref, root=False):
    # overwrite the input ref to 0, 'D' if we are testing root
    if root:
        in_ref = [0, 'D']
    dim_size_cpy = dim_size
    i_r_cpy = in_ref[:]

    print(dim_size_cpy)
    print(i_r_cpy)

    done = False
    time = 0

    crdscan = UncompressCrdRdScan(dim=dim_size_cpy)
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

        done = crdscan.done
        time += 1

    print("sam read cycle count: ", time)

    out_crd = remove_emptystr(out_crd)
    out_ref = remove_emptystr(out_ref)

    out_c = []
    out_r = []
    for i in range(len(out_crd)):
        if out_crd[i] != 'N':
            out_c.append(out_crd[i])
            out_r.append(out_ref[i])
    # now encode the dim size to (2, 0, dim_size) data pair
    encoded_dim_size = [2, 0, dim_size_cpy]
    st = [encoded_dim_size, i_r_cpy, out_c, out_r]
    tr_st = [convert_stream_to_onyx_interp(i) for i in st]

    return tr_st


def load_test_module(test_name, root=False):
    if test_name == "basic_2d":
        dim_size = 10
        in_ref = [0, 1, 'S0', 0, 1, 2, 'D']
        return create_gold(dim_size, in_ref)
    
    elif test_name == "basic_1d":
        dim_size = 6
        in_ref = [0, 1, 'S1', 'D']
        return create_gold(dim_size, in_ref)

    elif test_name == "in_ref_empty_fiber":
        dim_size = 20
        in_ref = ['S0', 'S0', 1, 0, 'S0', 'S0', 1, 'S1', 'D']
        return create_gold(dim_size, in_ref)
    # test case 
    elif test_name == "rd_full_mat_scan":
        t_arg = test_name.split("_")
        dim_size = random.randint(1, 30)
        num_fiber = random.randint(1, 30)
        in_ref = []
        for i in range(0, num_fiber):
            in_ref.append(i)
        in_ref.append("S0")
        in_ref.append("D")
        return create_gold(dim_size, in_ref)
    elif test_name == "rd_pos_input":
        dim_size = random.randint(1, 30)
        n = random.randint(1, 3)
        rate = random.uniform(0, 1)
        size = random.randint(1, 30)
        in_ref = create_random(n, rate, size)
        return create_gold(dim_size, in_ref)
    elif test_name == "maybe_token_1":
        dim_size = 10
        in_ref = [0, 'N', 'S0', 'N', 'N', 2, 'D']
        return create_gold(dim_size, in_ref)
    elif test_name == "maybe_token_2":
        dim_size = 20
        in_ref = ['N', 'S0', 1, 'N', 'S0', 'N', 'S1', 'D']
        return create_gold(dim_size, in_ref)
    elif test_name == "maybe_token_3":
        dim_size = 30
        in_ref = ['S0', 'S0', 'N', 'N', 'S0', 'S0', 1, 'N', 'S1', 'D']
        return create_gold(dim_size, in_ref)
    elif test_name == "rd_pos_input_maybe":
        dim_size = random.randint(1, 30)
        n = random.randint(1, 3)
        size = random.randint(3, 30)
        maybe = random.uniform(0.5, 0.8)
        in_ref = create_random(n, 1.0, size, maybe=maybe)
        return create_gold(dim_size, in_ref)
    elif test_name == "test_root_basic":
        dim_size = random.randint(1, 30)
        return create_gold(dim_size, [], root=True)

def module_iter_basic(test_name, add_test="", root=False):
    [ic, ir, gc, gr] = load_test_module(test_name, root=root)

    if add_test != "" and add_test != "void":
        additional_t = load_test_module(add_test, root=root)
        ic = ic + additional_t[0]
        ir = ir + additional_t[1]
        gc = gc + additional_t[2]
        gr = gr + additional_t[3]

    print(ic)
    print(ir)
    print(gc)
    print(gr)

    sparse_helper.write_txt("coord_in_0.txt", ic)
    sparse_helper.write_txt("pos_in_0.txt", ir)

    sparse_helper.clear_txt("coord_out.txt")
    sparse_helper.clear_txt("pos_out_0.txt")

    #run command "make sim" to run the simulation
    if add_test == "":
        sim_cmd = ["make", "sim", "TEST_TAR=fiber_access_dense_tb.sv", "TOP=fiber_access_dense_tb",\
                   "TEST_UNIT=Fiber_access.sv"]
    else:
        sim_cmd = ["make", "sim", "TEST_TAR=fiber_access_dense_tb.sv",\
                   "TOP=fiber_access_dense_tb", "TX_NUM_GLB=2", "TEST_UNIT=Fiber_access.sv"]

    if root:
        sim_cmd.append("FIBER_ACCESS_ROOT=1")

    sim_result = subprocess.run(sim_cmd, capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    #print(lines[1])

    tx_num = 1
    if add_test != "":
        tx_num = 2

    coord_out = sparse_helper.read_txt("coord_out.txt", count=tx_num)
    pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", count=tx_num)
    # print(coord_out)
    # print(pos_out_0)

    #compare each element in the output from coord_out.txt with the gold output
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
    test_list = ["basic_1d", "basic_2d"]
    for test in test_list:
        module_iter_basic(test)

def test_rd_full_mat_scan():
    init_module()
    for i in range(0, 10):
        module_iter_basic("rd_full_mat_scan")

def test_seq_basic():
    init_module()
    module_iter_basic("basic_1d", "basic_2d")

def test_empty_fiber():
    init_module()
    module_iter_basic("in_ref_empty_fiber")
    

def test_rd_pos_input():
    init_module()
    for i in range(0, 10):
        module_iter_basic("rd_pos_input")

def test_maybe():
    init_module()
    test_list = ["maybe_token_1", "maybe_token_2", "maybe_token_3"]
    for test in test_list:
        module_iter_basic(test)

def test_rd_pos_input_maybe():
    init_module()
    for i in range(0, 10):
        module_iter_basic("rd_pos_input_maybe")

def test_root_basic():
    init_module()
    for i in range(0, 10):
        module_iter_basic("test_root_basic", root=True)

def test_root_basic_seq():
    init_module()
    for i in range(0, 10):
        module_iter_basic("test_root_basic", "test_root_basic", root=True)

def test_seq_rd():
    init_module()
    test_list = ["basic_1d", "basic_2d", "rd_full_mat_scan", "rd_pos_input", "maybe_token_1", "maybe_token_2", "maybe_token_3",
                 "in_ref_empty_fiber", "rd_pos_input_maybe", "test_root_basic"]
    for i in range(0, 20):
        random_test_cases = []
        random_test_cases.append(random.choice(test_list))
        random_test_cases.append(random.choice(test_list))
        module_iter_basic(random_test_cases[0], random_test_cases[1])