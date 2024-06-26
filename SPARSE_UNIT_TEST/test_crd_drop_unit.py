from lake.modules.crddrop import *
import magma as m
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.crd_manager import CrdDrop as CrdDrop_sim
from sam.sim.test.test import TIMEOUT
from sam.sim.src.token import EmptyFiberStknDrop


import subprocess
import os
import random
random.seed(15)
import string


def init_module():
    dut = CrdDrop(data_width=16,
                    fifo_depth=2,
                    lift_config=False,
                    defer_fifos=False,
                    add_flush=True)
    # magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    k.verilog(dut, filename=f"./modules/CrdDrop.sv",
            optimize_if=False)
    sparse_helper.update_tcl("coord_drop_tb")

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


def create_random(n, rate, size, d1=0): # d1 is the total fiber number
    if n == 1:
        in_crd1 = create_random_fiber(rate, size, 0.2, "coord", 0.2) + ['S0', 'D']
        return in_crd1
    
    elif n == 2:
        if d1 == 0:
            d1 = int(random.uniform(2, int(size**(1/2))))
        d2 = max(size // d1, 1)
        rate = rate**(1/2)
        # print("h: ", size, d1, d2, rate)
        ret = []
        for i in range(d1):
            ret_t = []
            r = rate
            if random.random() > rate:
                r = 0
            ret_t = ret_t + create_random_fiber(r, d2, 0.2, "coord", 0.2)
            if i == d1 - 1:
                ret = ret + ret_t + ['S1', 'D']
            else:
                ret = ret + ret_t + ['S0']
        return ret
    elif n == 3:
        if d1 == 0:
            d1 = int(random.uniform(2, int(size**(1/3))))
        total_d = d1
        d2 = size // d1
        rate = rate**(1/3)
        ret = []
        # print(d1, d2, rate, int(size**(1/3)), int(d1**(1/2)))
        while total_d > 0:
            ret_t = []
            dd = int(random.uniform(1, int(d1 ** (1/2))))
            dd = min(total_d, dd)
            # print(dd)
            ret_t = create_random(2, rate*rate, d2, dd)
            total_d -= dd
            ret = ret + ret_t[:-1]
        
        ret = ret[:-1] + ['S2', 'D']
        return ret

def create_inner_fiber(outer_fiber, rate, size):
    ret = []
    num_fiber = len([x for x in outer_fiber if type(x) is int])
    fiber_size = max(1, int(size // num_fiber))
    for idx, token in enumerate(outer_fiber):
        # actual coordinate
        if type(token) is int:
            if random.random() < rate:
                ret = ret + create_random_fiber(rate, fiber_size, 0.2, "coord", 0.2)
            if type(outer_fiber[idx+1]) is int:
                ret = ret + ['S0']
        else: 
            if token == 'D':
                ret = ret + ['D']
            # stop token
            else:
                stkn_level = int(token[1]) + 1
                ret = ret + ['S' + str(stkn_level)]
    return ret

def create_gold(in_crd1, in_crd2): # 1 is outer, 0 is inner

    i_c1_cpy = in_crd1[:]
    i_c2_cpy = in_crd2[:]

    done = False
    time = 0

    cd = CrdDrop_sim()
    stkndrp = EmptyFiberStknDrop()
    out_outer = []
    out_inner = []
    while not done and time < TIMEOUT:
        if len(in_crd2) > 0:
            cd.set_inner_crd(in_crd2.pop(0))
        if len(in_crd1) > 0:
            cd.set_outer_crd(in_crd1.pop(0))

        stkndrp.set_in_stream(cd.out_crd_inner())

        out_outer.append(cd.out_crd_outer())
        out_inner.append(stkndrp.out_val())

        # print("Timestep", time, "\t Done:", cd.out_done(), "\t Out:", cd.out_crd_outer())

        done = cd.out_done() and stkndrp.out_done()
        cd.update()
        stkndrp.update()
        time += 1

    out_outer = remove_emptystr(out_outer)
    out_inner = remove_emptystr(out_inner)

    print("sam cycle count: ", time)

    st = [i_c1_cpy, i_c2_cpy, out_outer, out_inner]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))

    return tr_st, time


def load_test_module(test_name):
    if test_name == "stream_1":
        in_crd_o = [2, 6, 'S0', 'S0', 1, 7, 9, 'S0', 1, 5, 8, 'S0', 'S0', 0, 2, 3, 'S0', 6, 'S0', 2, 'S0', 2, 9, 'S0', 6, 7, 8, 'S1', 'D']
        in_crd_i = ['S0', 2, 'S1', 'S1', 'S0', 'S0', 'S1', 'S0', 'S0', 'S1', 'S1', 'S0', 'S0', 0, 'S1', 'S1', 'S1', 'S0', 'S1', 'S0', 'S0', 'S2', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_2":
        in_crd_o = [0, 1, 'S0', 'D']
        in_crd_i =  [1, 'S0', 1, 'S1', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_3":
        in_crd_o = [0, 1, 2, 3, 'S0', 'D']
        in_crd_i = [1, 'S0', 1, 'S0', 'S0', 1, 'S1', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_4":
        in_crd_o = [1, 'S0', 'D']
        in_crd_i = [1, 2, 'S1', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_5":
        in_crd_o = [1, 2, 3, 'S0', 'S1', 'D']
        in_crd_i = [1, 2, 'S0', 'S0', 'S1', 'S2', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_6":
        in_crd_o = [1, 2, 3, 'S0', 4, 'S1', 'D']
        in_crd_i = [1, 2, 'S0', 'S0', 'S1', 'S2', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_7":
        in_crd_o = [1, 2, 3, 'S0', 4, 5, 'S1', 'D']
        in_crd_i = ['S0', 1, 2, 'S0', 'S1', 'S0', 'S2', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        dim1 = int(t_arg[1][0])
        rate1 = float(t_arg[2])
        size1 = int(t_arg[3])
        dim2 = int(t_arg[4][0])
        rate2 = float(t_arg[5])
        size2 = int(t_arg[6])
        in_crd_o = create_random(dim1, rate1, size1)
        fiber_num = len([i for i in in_crd_o if type(i) is int])
        if fiber_num == 0: # assure something is in the outer
            in_crd_o = [1] + in_crd_o
            fiber_num = len([i for i in in_crd_o if type(i) is int])
        in_crd_i = create_inner_fiber(in_crd_o, rate2, size2)
        return create_gold(in_crd_o, in_crd_i)

    else:
        in_crd_o = [1, 'S0', 'D']
        in_crd_i = ['S1', 'D']

        return create_gold(in_crd_o, in_crd_i)


def module_iter_basic(test_name, add_test=""):
    [ic1, ic2, gc1, gc2], sam_cycs = load_test_module(test_name)
    if add_test != "":
        additional_t, add_sam_cycs = load_test_module(add_test)
        ic1 = ic1 + additional_t[0]
        ic2 = ic2 + additional_t[1]
        gc1 = gc1 + additional_t[2]
        gc2 = gc2 + additional_t[3]
        sam_cycs = sam_cycs + add_sam_cycs

    print("ic1", ic1)
    print("ic2", ic2)
    print("gc1", gc1)
    print("gc2", gc2)

    sparse_helper.write_txt("coord_in_0.txt", ic1)
    sparse_helper.write_txt("coord_in_1.txt", ic2)

    sparse_helper.clear_txt("pos_out_0.txt")
    sparse_helper.clear_txt("pos_out_1.txt") 
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=coord_drop_tb.sv", "TOP=coord_drop_tb",\
                             "TEST_UNIT=CrdDrop.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=coord_drop_tb.sv",\
                             "TOP=coord_drop_tb", "TX_NUM_GLB=2", "TEST_UNIT=CrdDrop.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    assert output.find("Valid signal fails to end") == -1, "Valid signal fails to end"
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])
    hw_cycs = int(cycle_count_line.splitlines()[0].split(":")[1])

    tx_num = 1
    if add_test != "":
        tx_num = 2

    pos_out_1 = sparse_helper.read_txt("pos_out_0.txt", count=tx_num)
    pos_out_0 = sparse_helper.read_txt("pos_out_1.txt", count=tx_num)

    # print(pos_out_0)
    # print(pos_out_1)
    
    #compare each element in the output from pos_out_0.txt with the gold output
    assert len(pos_out_0) == len(gc1), \
        f"Output length {len(pos_out_0)} didn't match gold length {len(gc1)}"
    for i in range(len(pos_out_0)):
        assert pos_out_0[i] == gc1[i], \
            f"Output {pos_out_0[i]} didn't match gold {gc1[i]} at index {i}"
    
    #compare each element in the output from pos_out_1.txt with the gold output
    assert len(pos_out_1) == len(gc2), \
        f"Output length {len(pos_out_1)} didn't match gold length {len(gc2)}"
    for i in range(len(pos_out_1)):
        assert pos_out_1[i] == gc2[i], \
            f"Output {pos_out_1[i]} didn't match gold {gc2[i]} at index {i}"
    
    print(test_name, " passed\n")
    sparse_helper.write_csv("crd_drop", test_name + "_" + add_test, hw_cycs, sam_cycs)


def test_iter_basic():
    init_module()
    test_list = ["stream_1", "stream_2", "stream_3", "stream_4", "stream_5", "stream_6", "stream_7", "xxx"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1d_2d():
    init_module()
    test_list = ["rd_1d_0.3_30_2d_0.1_80", "rd_1d_0.3_30_2d_0.3_80", "rd_1d_0.3_30_2d_0.5_80", "rd_1d_0.3_30_2d_0.8_80", "rd_1d_0.3_30_2d_1.0_80"]
    for test in test_list:
        module_iter_basic(test)


def test_random_2d_3d():
    init_module()
    test_list = ["rd_2d_0.3_30_3d_0.1_80", "rd_2d_0.3_30_3d_0.3_80", "rd_2d_0.3_30_3d_0.5_80", "rd_2d_0.3_30_3d_0.8_80", "rd_2d_0.3_30_3d_1.0_80"]
    for test in test_list:
        module_iter_basic(test)


def test_seq():
    init_module()
    test_list =  ["stream_1", "stream_2", "stream_3", "stream_4", "stream_5", "stream_6", "stream_7", "xxx"] +\
                 ["rd_1d_0.3_30_2d_0.1_80", "rd_1d_0.3_30_2d_0.3_80", "rd_1d_0.3_30_2d_0.5_80", "rd_1d_0.3_30_2d_0.8_80", "rd_1d_0.3_30_2d_1.0_80"] +\
                 ["rd_2d_0.3_30_3d_0.1_80", "rd_2d_0.3_30_3d_0.3_80", "rd_2d_0.3_30_3d_0.5_80", "rd_2d_0.3_30_3d_0.8_80", "rd_2d_0.3_30_3d_1.0_80"]
    for i in range(20):
        rand = random.sample(test_list, 2)
        module_iter_basic(rand[0], rand[1])


def test_eff():
    init_module()
    test_list = ["rd_2d_0.8_80_3d_0.1_200", "rd_2d_0.8_80_3d_0.3_200", "rd_2d_0.8_80_3d_0.5_200", "rd_2d_0.8_80_3d_0.8_200", "rd_2d_0.8_80_3d_1.0_200"]
    for test in test_list:
        module_iter_basic(test)
