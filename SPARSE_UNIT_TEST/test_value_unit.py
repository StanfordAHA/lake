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

    sparse_helper.update_tcl("value_tb")

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


def create_gold(in_val, in_ref):
    i_c_cpy = in_val[:]
    i_r_cpy = in_ref[:]

    mem = [v for v in in_val if (not sparse_helper.is_STOP_sam(v) and not sparse_helper.is_DONE_sam(v))]
    max_addr = len(mem) - 1

    print("Sam assumes 0-delay memory access")

    out_v = []
    for addr in in_ref:
        if sparse_helper.is_MAYBE_sam(addr):
            out_v.append(0)
        elif (not sparse_helper.is_STOP_sam(addr) and not sparse_helper.is_DONE_sam(addr)):
            assert addr <= max_addr
            out_v.append(mem[addr])
        else:
            out_v.append(addr)

    st = [i_c_cpy, i_r_cpy, out_v]
    tr_st = [convert_stream_to_onyx_interp(i) for i in st]

    return tr_st


def load_test_module(test_name):
    if test_name == "direct_2d":
        in_val = [0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_ref = [0, 1, 2, 'S0', 3, 4, 5, 'S1', 'D']
        return create_gold(in_val, in_ref)
    
    elif test_name == "direct_1d":
        in_val = [0, 2, 3, 7, 8, 10] + ['S0', 'D']
        in_ref = [0, 1, 2, 3, 4, 5, 'S0', 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "in_ref_2d_1":
        in_val = [0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_ref = [0, 'S0', 5, 'S1', 'D']
        return create_gold(in_val, in_ref)
    
    elif test_name == "in_ref_2d_2":
        in_val = [0, 2, 3, 7, 8, 10] + ['S0', 'D']
        in_ref = [4, 1, 'S0', 5, 'S1', 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "in_ref_empty_fiber":
        in_val = [0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_ref = ['S0', 'S0', 'S1', 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "arr_4":
        in_val = [1,2,3,4,5, 'S0', 'D']
        # print(len(in_val))
        in_ref = [4, 3, 2, 1, 0, 'S2', 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "arr_5":
        in_val = [6,'S0', 'D']
        # print(len(in_val))
        in_ref = [0, 0, 0, 0, 'S1', 'D']
        return create_gold(in_val, in_ref)
    
    elif test_name == "arr_6":
        in_val = [0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_ref = ['S0', 'S0', 1, 0, 'S0', 'S0', 4, 'S1', 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "arr_1":
        in_val = [0, 1, 'S0', 2, 'S0', 3, 'S1', 'D']
        in_ref = [0, 1, 'S0', 2, 'S0', 3, 'S1', 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "arr_2":
        in_val = [0, 2, 3, 'S0', 0, 'S0', 2, 3, 'S1', 'D']
        in_ref = [0, 1, 2, 'S0', 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "arr_3":
        in_val = [0, 1, 2, 3, 'S0', 4, 'S0', 5, 6, 'S0', 7, 8, 9, 'S0', 10, 'S1', 'D'] 
        in_ref = [0, 1, 'S0', 2, 3, 'S0', 'S0', 4, 'S0', 7, 9, 9, 7, 1, 2, 0, 'S1', 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "seq_1_1":
        in_val = [0, 1, 2, 3, 'S1', 'D']
        in_ref = ['N', 'N', 'S0', 'D']
        return create_gold(in_val, in_ref)
    
    elif test_name == "seq_1_2":
        in_val = [3, 4, 5, 'S0', 'D']
        in_ref = [0, 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "empty":
        in_val = ['S0', 'D']
        in_ref = ['D']
        return create_gold(in_val, in_ref)

    elif test_name == "loop_round_1":
        in_val = [377, 104, 443, 50, 556, 313, 462, 184, 275, 219, 230, 367, 554, 122, 279,\
                263, 229, 532, 351, 394, 433, 341, 77, 307, 198, 37, 323, 262, 97, 5, 534, 'S0', 'D']
        in_ref = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 'D']
        return create_gold(in_val, in_ref)

    elif test_name == "loop_round_2":
        in_val = [185, 208, 268, 44, 226, 488, 369, 91, 460, 302, 28, 133, 236, 282, 132,\
                355, 90, 337, 315, 214, 203, 399, 164, 455, 284, 27, 574, 36, 239, 70, 267, 'S0', 'D']
        in_ref = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 'D']
        return create_gold(in_val, in_ref)

    elif test_name[0:2] == "rd":
        t_arg = test_name.split("_")
        dim1 = int(t_arg[1][0])
        rate1 = float(t_arg[2])
        size1 = int(t_arg[3]) # unlike other tests, the size here is the final size
        t_size = int(size1 / rate1)
        dim2 = int(t_arg[4][0])
        rate2 = float(t_arg[5])
        size2 = int(t_arg[6])
        
        coord = sparse_helper.coord_drop(create_random(dim1, rate1, t_size))

        coord_t = coord[:] #only used for seq

        ref = create_ref(coord, dim2, rate2, size2)
        print(coord)
        print(ref)
        return create_gold(coord, ref)

    else:
        in_val = [0, 'S0', 'D']
        in_ref = [0, 0, 0, 'S0', 'D']
        return create_gold(in_val, in_ref)


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
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=value_tb.sv", "TOP=value_tb",\
                             "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=value_tb.sv",\
                             "TOP=value_tb", "TX_NUM_GLB=2", "TEST_UNIT=Fiber_access.sv"\
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


def test_iter_basic():
    init_module()
    test_list = ["direct_1d", "direct_2d", "in_ref_2d_1", "in_ref_2d_2", "in_ref_empty_fiber", "seq_1_1", "arr_1", "arr_2", "arr_3", "arr_4", "arr_5",  "arr_6", "xxx"]
    for test in test_list:
        module_iter_basic(test)


def test_seq1():
    init_module()
    module_iter_basic("in_ref_empty_fiber", "arr_1")


def test_seq2():
    init_module()
    module_iter_basic("loop_round_1", "loop_round_2")


def test_seq3():
    init_module()
    module_iter_basic("in_ref_empty_fiber", "empty")


def test_random_1d_1d():
    init_module()
    test_list = ["rd_1d_0.1_200_1d_1.0_3", "rd_1d_0.3_200_1d_1.0_3", "rd_1d_0.5_200_1d_1.0_3", "rd_1d_0.8_200_1d_1.0_3", "rd_1d_1.0_200_1d_1.0_3"]
    for test in test_list:
        module_iter_basic(test)


def test_random_2d_1d():
    init_module()
    test_list = ["rd_2d_0.1_100_1d_1.0_3", "rd_2d_0.3_100_1d_1.0_3", "rd_2d_0.5_100_1d_1.0_3", "rd_2d_0.8_100_1d_1.0_3", "rd_2d_1.0_100_1d_1.0_3"]
    for test in test_list:
        module_iter_basic(test)


def test_random_2d_2d():
    init_module()
    test_list = ["rd_2d_0.1_100_2d_0.3_30", "rd_2d_0.3_100_2d_0.3_30", "rd_2d_0.5_100_2d_0.3_30", "rd_2d_0.8_100_2d_0.3_30", "rd_2d_1.0_100_2d_0.3_30"]
    for test in test_list:
        module_iter_basic(test)


def test_seq3():
    init_module()
    test_list =  ["rd_1d_0.1_200_1d_1.0_3", "rd_1d_0.3_200_1d_1.0_3", "rd_1d_0.5_200_1d_1.0_3", "rd_1d_0.8_200_1d_1.0_3", "rd_1d_1.0_200_1d_1.0_3"] +\
                 ["rd_2d_0.1_100_1d_1.0_3", "rd_2d_0.3_100_1d_1.0_3", "rd_2d_0.5_100_1d_1.0_3", "rd_2d_0.8_100_1d_1.0_3", "rd_2d_1.0_100_1d_1.0_3"] +\
                 ["rd_2d_0.1_100_2d_0.3_30", "rd_2d_0.3_100_2d_0.3_30", "rd_2d_0.5_100_2d_0.3_30", "rd_2d_0.8_100_2d_0.3_30", "rd_2d_1.0_100_2d_0.3_30"] +\
                 ["direct_1d", "direct_2d", "in_ref_2d_1", "in_ref_2d_2", "in_ref_empty_fiber", "arr_1", "arr_2", "arr_3", "arr_4", "arr_5", "arr_6", "xxx"]
    for i in range(30):
        rand = random.sample(test_list, 2)
        module_iter_basic(rand[0], rand[1])
