from lake.modules.crdhold import *
import magma as m
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.crd_manager import CrdHold as CrdHold_sim
from sam.sim.test.test import TIMEOUT


import subprocess
import os
import random
random.seed(15)
import string


def init_module():
    dut = CrdHold(data_width=16,
                    fifo_depth=2,
                    lift_config=False,
                    defer_fifos=False,
                    add_flush=True)
    # magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    k.verilog(dut, filename=f"./modules/CrdHold.sv",
            optimize_if=False)
    sparse_helper.update_tcl("coord_hold_tb")

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
        d2 = size // d1
        rate = rate**(1/2)
        ret = []
        for i in range(d1):
            ret_t = []
            ret_t = ret_t + create_random_fiber(rate, d2, 0.2, "coord", 0.2)
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
        # print(d1, d2, rate)
        while total_d > 0:
            ret_t = []
            dd = int(random.uniform(2, int(d1 ** (1/2))))
            dd = min(total_d, dd)
            ret_t = create_random(2, rate*rate, d2, dd)
            total_d -= dd
            ret = ret + ret_t[:-1]
        
        ret = ret[:-1] + ['S2', 'D']
        return ret


def create_gold(ocrd, icrd):
    # assert len([i for i in ocrd if type(i) is int]) ==\
    #      len([i for i in icrd if sparse_helper.is_STOP_sam(i)])
    
    i_c2_cpy = icrd[:]
    i_c1_cpy = ocrd[:]

    ch = CrdHold_sim()

    done = False
    time = 0
    out = []
    while not done and time < TIMEOUT:
        if len(icrd) > 0:
            ch.set_inner_crd(icrd.pop(0))
        if len(ocrd) > 0:
            ch.set_outer_crd(ocrd.pop(0))

        ch.update()

        out.append(ch.out_crd_outer())

        # print("Timestep", time, "\t Done:", ch.out_done(), "\t Out:", ch.out_crd_outer())

        done = ch.out_done()
        time += 1

    out = remove_emptystr(out)
    print("sam cycle count: ", time)

    # temp fix for sam
    assert len(out) == len(i_c2_cpy)
    for i in range(len(out)):
        if sparse_helper.is_STOP_sam(out[i]):
            out[i] = i_c2_cpy[i]

    st = [i_c1_cpy, i_c2_cpy, out] #inner and outer
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))

    return tr_st


# Outer stream should not have empty fiber
def load_test_module(test_name):
    if test_name == "stream_1":
        in_crd_o = [0, 1, 2, 'S0', 'D']
        in_crd_i = [0, 2, 'S0', 2, 'S0', 2, 'S1', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_2":
        in_crd_o = [0, 1, 2, 5, 'S0', 'D']
        in_crd_i = [1, 2, 5, 'S0', 2, 'S0', 2, 'S0', 2, 3, 4, 5, 'S1', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_3":
        in_crd_o = [0, 2, 'S0', 3, 'S0', 4, 'S1', 'D']
        in_crd_i = [0, 2, 3, 'S0', 0, 2, 3, 'S1', 0, 'S1', 2, 3, 'S2', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_4":
        in_crd_o = [1, 2, 'S0', 'D']
        in_crd_i = [1, 'S0', 1, 2, 6, 7, 'S1', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_5":
        in_crd_o = [1, 2, 'S1', 'D']
        in_crd_i = [1, 'S1', 1, 2, 6, 7, 'S2', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_6":
        in_crd_o = [1, 2, 'S1', 'D']
        in_crd_i = ['S1', 'S2', 'D']

        return create_gold(in_crd_o, in_crd_i)

    elif test_name == "stream_7":
        in_crd_o = [9, 'S0', 7, 'S1', 'D']
        in_crd_i = ['S1', 'S2', 'D']

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
        in_crd_i = create_random(dim2, rate2, size2, d1=fiber_num)
        return create_gold(in_crd_o, in_crd_i)

    else:
        in_crd_o = [1, 'S0', 'D']
        in_crd_i = ['S1', 'D']

        return create_gold(in_crd_o, in_crd_i)


def module_iter_basic(test_name, add_test=""):
    [ic1, ic2, gc] = load_test_module(test_name)
    if add_test != "":
        additional_t = load_test_module(add_test)
        ic1 = ic1 + additional_t[0]
        ic2 = ic2 + additional_t[1]
        gc = gc + additional_t[2]

    print("ic1", ic1)
    print("ic2", ic2)
    print("gc1", gc)

    sparse_helper.write_txt("coord_in_0.txt", ic1)
    sparse_helper.write_txt("coord_in_1.txt", ic2)

    sparse_helper.clear_txt("pos_out_0.txt")
    sparse_helper.clear_txt("pos_out_1.txt") 
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=coord_hold_tb.sv", "TOP=coord_hold_tb",\
                             "TEST_UNIT=CrdHold.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=coord_hold_tb.sv",\
                             "TOP=coord_hold_tb", "TX_NUM_GLB=2", "TEST_UNIT=CrdHold.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    assert output.find("Valid signal fails to end") == -1, "Valid signal fails to end"
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    tx_num = 1
    if add_test != "":
        tx_num = 2

    pos_out_1 = sparse_helper.read_txt("pos_out_0.txt", count=tx_num)
    pos_out_0 = sparse_helper.read_txt("pos_out_1.txt", count=tx_num)

    print(pos_out_0)
    print(pos_out_1)
    
    #compare each element in the output from pos_out_0.txt with the gold output
    assert len(pos_out_0) == len(gc), \
        f"Output length {len(pos_out_0)} didn't match gold length {len(gc)}"
    for i in range(len(pos_out_0)):
        assert pos_out_0[i] == gc[i], \
            f"Output {pos_out_0[i]} didn't match gold {gc[i]} at index {i}"
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["stream_1", "stream_2", "stream_3", "stream_4", "stream_5", "stream_6", "stream_7", "xxx"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1d_2d():
    init_module()
    test_list = ["rd_1d_0.1_30_2d_0.5_80", "rd_1d_0.3_30_2d_0.5_80", "rd_1d_0.5_30_2d_0.5_80", "rd_1d_0.8_30_2d_0.5_80", "rd_1d_1.0_30_2d_0.5_80"]
    for test in test_list:
        module_iter_basic(test)


def test_random_2d_3d():
    init_module()
    test_list = ["rd_2d_0.1_30_3d_0.5_80", "rd_2d_0.3_30_3d_0.5_80", "rd_2d_0.5_30_3d_0.5_80", "rd_2d_0.8_30_3d_0.5_80", "rd_2d_1.0_30_3d_0.5_80"]
    for test in test_list:
        module_iter_basic(test)


def test_seq():
    init_module()
    test_list =  ["stream_1", "stream_2", "stream_3", "stream_4", "stream_5", "stream_6", "stream_7", "xxx"] +\
                 ["rd_1d_0.1_30_2d_0.5_80", "rd_1d_0.3_30_2d_0.5_80", "rd_1d_0.5_30_2d_0.5_80", "rd_1d_0.8_30_2d_0.5_80", "rd_1d_1.0_30_2d_0.5_80"] +\
                 ["rd_2d_0.1_30_3d_0.5_80", "rd_2d_0.3_30_3d_0.5_80", "rd_2d_0.5_30_3d_0.5_80", "rd_2d_0.8_30_3d_0.5_80", "rd_2d_1.0_30_3d_0.5_80"]
    for i in range(10):
        rand = random.sample(test_list, 2)
        module_iter_basic(rand[0], rand[1])
