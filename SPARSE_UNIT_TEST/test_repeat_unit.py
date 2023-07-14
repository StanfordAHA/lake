from lake.modules.repeat import *
import magma as m
from magma import *
import tempfile
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.repeater import Repeat as Repeat_GLD
from sam.sim.test.test import TIMEOUT


import subprocess
import os
import random
random.seed(15)
import string


def init_module():
    dut = Repeat(data_width=16,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)

    magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    sparse_helper.update_tcl("repeat_tb")

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
        return ret[0], ret[1], ret[2], ret[3]
                

def create_gold(in_ref1, repsig):
    assert (len([x for x in in_ref1 if (type(x) is int or x == 'N')])) == \
        len([repsig[i] for i in range(len(repsig)) if (repsig[i] == 'S' and (i == 0 or repsig[i - 1] == 'R'))])
    assert (in_ref1[-1] == 'D' and repsig[-1] == 'D')
    
    i_r1_cpy = in_ref1[:]
    sig_cpy = repsig[:]

    done = False
    time = 0

    rep = Repeat_GLD()
    out = []

    while not done and time < TIMEOUT:
        if len(in_ref1) > 0:
            rep.set_in_ref(in_ref1.pop(0))
        if len(repsig) > 0:
            rep.set_in_repeat(repsig.pop(0))

        rep.update()

        out.append(rep.out_ref())

        # print("Timestep", time, "\t Done:", rep.out_done(), "\t Repeat Sig:", rep.out_ref())

        done = rep.out_done()
        time += 1

    print("sam cycle count: ", time)
    out = remove_emptystr(out)

    assert len(out) == len(sig_cpy)
    st = [i_r1_cpy, sig_cpy, out]
    for i in range(len(out)):
        if sig_cpy[i] == 'S':
            sig_cpy[i] = out[i]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))

    return tr_st


def load_test_module(test_name):
    if test_name == "arr_1":
        ref = [0, 1, 2, 'S0', 'D']
        repeat = ['R', 'R', 'S', 'R', 'S', 'R', 'S', 'D']
        return create_gold(ref, repeat)

    elif test_name == "arr_2":
        ref = [0, 1, 'S0', 2, 'S0', 3, 'S1', 'D']
        repeat = ['R', 'R', 'R', 'S', 'R', 'R', 'R', 'S', 'R', 'S', 'R', 'R', 'S', 'D']
        return create_gold(ref, repeat)
    
    elif test_name == "arr_3":
        ref = [0, 'S0', 1, 2, 3, 'S1', 'D']
        repeat = ['R', 'R', 'R', 'S', 'R', 'R', 'R', 'S', 'R', 'S', 'R', 'R', 'S', 'D']
        return create_gold(ref, repeat)

    elif test_name == "arr_4":
        ref = [0, 'D']
        repeat = ['R', 'R', 'S', 'D']
        return create_gold(ref, repeat)

    elif test_name == "arr_5":
        ref = [0, 1, 'S0', 'D']
        repeat = ['R', 'R', 'R', 'S'] * 2 + ['D']
        return create_gold(ref, repeat)
    
    elif test_name == "arr_6":
        ref = [0, 'S0', 'S0', 'S0', 'S0', 'S0', 'S0', 'S0', 'S1', 'D']
        repeat = ['R', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'S', 'D']
        return create_gold(ref, repeat)
    
    elif test_name == "arr_7":
        ref = [0, 'S0', 1, 'S1', 'D']
        repeat = ['S', 'R', 'S', 'D']
        return create_gold(ref, repeat)
    
    elif test_name == "arr_8":
        ref = ['N', 0, 1, 'S0', 'D']
        repeat = ['R', 'S', 'R', 'S', 'R', 'S', 'D']
        return create_gold(ref, repeat)

    else:
        ref = [0, 'S0', 'D']
        repeat = ['S','D']
        return create_gold(ref, repeat)


def module_iter_basic(test_name, add_test=""):
    [ir1, rep, gc] = load_test_module(test_name)

    print(ir1)
    print(rep)
    print(gc)

    if add_test != "":
        additional_t = load_test_module(add_test)
        ir1 = ir1 + additional_t[0]
        rep = rep + additional_t[1]
        gc = gc + additional_t[2]

    # print("ir1", ir1)
    # print("rep", rep)
    # print("gc", gc)

    sparse_helper.write_txt("pos_in_0.txt", ir1)
    sparse_helper.write_txt("pos_in_1.txt", rep)

    sparse_helper.clear_txt("pos_out_0.txt")
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=repeat_tb.sv", "TOP=repeat_tb",\
                             "TEST_UNIT=Repeat-kratos.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=repeat_tb.sv",\
                             "TOP=repeat_tb", "TX_NUM_GLB=2", "TEST_UNIT=Repeat-kratos.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", addit=add_test != "")
    
    #compare each element in the output from pos_out_0.txt with the gold output
    print(pos_out_0)
    assert len(pos_out_0) == len(gc), \
        f"Output length {len(pos_out_0)} didn't match gold length {len(gc)}"
    for i in range(len(pos_out_0)):
        assert pos_out_0[i] == gc[i], \
            f"Output {pos_out_0[i]} didn't match gold {gc[i]} at index {i}"
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["arr_1", "arr_2", "arr_3", "arr_4", \
                "arr_5", "arr_6", "arr_7", "arr_8", "empty"]
    for test in test_list:
        module_iter_basic(test)
