from lake.modules.locator import *
import magma as m
from magma import *
import tempfile
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp

import subprocess
import os
import random
random.seed(15)
import string


def init_module():
    dut = Locator(data_width=16,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)
                    # data_width=16,
                    # use_merger=False,
                    # fifo_depth=2,
                    # defer_fifos=True,
                    # add_flush=False,
                    # perf_debug=perf_debug
    # magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    verilog(dut, filename=f"./modules/Locator.sv",
            optimize_if=False)
    sparse_helper.update_tcl("locator_tb")

def create_random_fiber(rate, size, d, f_type = "coord"):
    # size = int(size*random.uniform(1.0, 1.0+d))
    s = int(rate*size)
    if f_type == "coord":
        crd = random.sample(range(size), s)
        crd.sort()
        return crd
    elif f_type == "pos":
        pos = random.sample(range(1, size + 1), s)
        pos.sort()
        return pos
    else:
        val = [random.uniform(0, 2**15-1) for i in range(s)]
        return val

def create_random(n, rate, size, d1=0):
    if n == 1:
        in_crd1 = create_random_fiber(rate, size, 0.2, "coord") + ['S0', 'D']
        in_ref1 = create_random_fiber(1, len(in_crd1) - 2, 0, "pos") + ['S0', 'D']
        in_crd2 = create_random_fiber(rate, size, 0.2, "coord") + ['S0', 'D']
        in_ref2 = create_random_fiber(1, len(in_crd2) - 2, 0, "pos") + ['S0', 'D']
        return in_crd1, in_crd2, in_ref1, in_ref2
    
    elif n == 2:
        if d1 == 0:
            d1 = int(random.uniform(2, int(size**(1/2))))
        d2 = size // d1
        rate = rate**(1/2)
        ret = [[] for i in range(4)]
        for i in range(d1):
            ret_t = [[] for j in range(4)]
            if random.random() < rate:
                ret_t[0] = create_random_fiber(rate, d2, 0.2, "coord")
                ret_t[1] = create_random_fiber(1, len(ret_t[0]), 0, "pos")
            if random.random() < rate:
                ret_t[2] = create_random_fiber(rate, d2, 0.2, "coord")
                ret_t[3] = create_random_fiber(1, len(ret_t[2]), 0, "pos")

            for j in range(4):
                if i == d1 - 1:
                    ret[j] = ret[j] + ret_t[j] + ['S1', 'D']
                else:
                    ret[j] = ret[j] + ret_t[j] + ['S0']
        return ret[0], ret[1], ret[2], ret[3]
    elif n == 3:
        if d1 == 0:
            d1 = int(random.uniform(2, int(size**(1/3))))
        d2 = size // d1
        d1_ = int(random.uniform(2, int(d2**(1/2))))
        rate = rate**(1/3)
        ret = [[] for i in range(4)]
        # print(d1, d2, rate)
        for i in range(d1):
            # print("i---------: ", i)
            ret_t = [[] for j in range(4)]
            if random.random() < rate:
                ret_t[0], ret_t[1], ret_t[2], ret_t[3] = create_random(2, rate*rate, d2, d1_)
            else:
                for j in range(4):
                    ret_t[j] = ret_t[j] + ['S1', 'D']
            
            for j in range(4):
                if i == d1 - 1:
                    ret[j] = ret[j] + ret_t[j][:-2] + ['S2', 'D']
                else:
                    ret[j] = ret[j] + ret_t[j][:-1]
                # print(j)
                # print(ret_t[j][:-1])
                # print(ret[j])
        return ret[0], ret[1], ret[2], ret[3]
                

def create_gold(in_crd1, in_crd2, l_level, dim):

    num_outer = [x for x in in_crd2 if type(x) is int]
    num_value = len(num_outer)
    stop_inner = [x for x in in_crd1 if type(x) is not int]
    valid_stop_inner = [x for x in stop_inner if type(x) is str and x[0] == 'S' and int(x[1:]) >= l_level]

    assert len(valid_stop_inner) == num_value, "unmatched value pair"
    
    i_c1_cpy = in_crd1[:]
    i_c2_cpy = in_crd2[:]

    out_ref = []

    outer_idx = 0
    inner_idx = 0

    while inner_idx < len(in_crd1):
        if type(in_crd1[inner_idx]) is not int:
            if "S" in in_crd1[inner_idx] and int(in_crd1[inner_idx][1:]) >= l_level:
                outer_idx += 1
            out_ref.append(in_crd1[inner_idx])
            inner_idx += 1
            continue
        else:
            out_ref.append(int(in_crd1[inner_idx]) + dim * in_crd2[outer_idx])
            inner_idx += 1

    print("ideal cycle count: ", len(in_crd1))
    print(out_ref)

    assert len(out_ref) == len(in_crd1)
    st = [i_c1_cpy, i_c2_cpy, out_ref]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))

    tr_st.append(l_level)
    tr_st.append(dim)

    return tr_st


def load_test_module(test_name):
    if test_name == "direct_2level":
        dim = 10
        l_level = 1
        in_crd1 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 0, 1, 2, 'S0', 0, 1, 2, 'S2', 'D']
        in_crd2 = [0, 1, 'S0', 'D']

        return create_gold(in_crd1, in_crd2, l_level, dim)
    
    elif test_name == "direct_1level":
        dim = 10
        l_level = 0
        in_crd1 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']
        in_crd2 = [0, 1, 'S0', 'D']

        return create_gold(in_crd1, in_crd2, l_level, dim)

    elif test_name == "direct_1level":
        dim = 20
        l_level = 1
        in_crd1 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 0, 1, 2, 'S0', 0, 1, 2, 'S2', 'D']
        in_crd2 = ['S0', 'S0', 0, 'S0', 1, 'S1', 'D']

        return create_gold(in_crd1, in_crd2, l_level, dim)

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        outer_l = int(t_arg[1])
        l_level = int(t_arg[2])
        dim = random.randint(1, 400)
        [in_crd1, in_ref1, in_crd2, in_ref2] = create_random(outer_l, l_level)
        return create_gold(in_crd1, in_crd2, l_level, dim)

    else:
        dim = 10
        l_level = 0
        in_crd1 = ['S0', 'D']
        in_crd2 = [1, 'D']

        return create_gold(in_crd1, in_crd2, l_level, dim)


def module_iter_basic(test_name, add_test=""):
    [ic1, ic2, gr, l_level, dim] = load_test_module(test_name)
    if add_test != "":
        additional_t = load_test_module(add_test)
        ic1 = ic1 + additional_t[0]
        ic2 = ic2 + additional_t[1]
        gr = gr + additional_t[2]

    print("ic1", ic1)
    print("ic2", ic2)
    print("gr", gr)

    print("l_level", l_level)
    print("dim", dim)

    sparse_helper.write_txt("coord_in_0.txt", ic1)
    sparse_helper.write_txt("coord_in_1.txt", ic2)

    sparse_helper.clear_txt("pos_out_0.txt")
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=locator_tb.sv", "TOP=locator_tb",\
                             f"L_LEVEL={l_level}", f"DIM={dim}",\
                             "TEST_UNIT=Locator.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=locator_tb.sv",\
                             "TOP=locator_tb", "TX_NUM_GLB=2", f"L_LEVEL={l_level}", f"DIM={dim}",\
                             "TEST_UNIT=Locator.sv"], capture_output=True, text=True)

    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    tx_num = 1
    if add_test != "":
        tx_num = 2

    pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", count=tx_num)
    
    #compare each element in the output from pos_out_0.txt with the gold output
    assert len(pos_out_0) == len(gr), \
        f"Output length {len(pos_out_0)} didn't match gold length {len(gr)}"
    for i in range(len(pos_out_0)):
        assert pos_out_0[i] == gr[i], \
            f"Output {pos_out_0[i]} didn't match gold {gr[i]} at index {i}"
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["direct_1level", "direct_2level", "xxx"]
    for test in test_list:
        module_iter_basic(test)


# def test_seq():
#     init_module()
#     test_list =  ["rd_1d_0.1_400", "rd_1d_0.3_400", "rd_1d_0.5_400", "rd_1d_0.8_400", "rd_1d_1.0_400"] +\
#                  ["rd_2d_0.1_400", "rd_2d_0.3_400", "rd_2d_0.5_400", "rd_2d_0.8_400", "rd_1d_1.0_400"] +\
#                  ["rd_3d_0.1_400", "rd_3d_0.3_400", "rd_3d_0.5_400", "rd_3d_0.8_400", "rd_1d_1.0_400"]
#     for i in range(10):
#         rand = random.sample(test_list, 2)
#         module_iter_basic(rand[0], rand[1])
