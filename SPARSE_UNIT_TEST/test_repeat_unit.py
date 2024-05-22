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

    # magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    verilog(dut, filename=f"./modules/Repeat.sv",
            optimize_if=False)
    sparse_helper.update_tcl("repeat_tb")


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


def create_random(n, rate, size, d1=0):
    if n == 1:
        in_crd1 = create_random_fiber(rate, size, 0.2, "pos", 0.2) + ['S0', 'D']
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
                ret_t = ret_t + create_random_fiber(rate, d2, 0.2, "pos", 0.2)
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
                

def create_repeat_sig(ref, rep_times):
    total_stop = (len([ref[i] for i in range(len(ref)) if (type(ref[i]) is int or ref[i] == 'N') \
        and not sparse_helper.is_STOP_sam(ref[i+1])])) \
        + len([x for x in ref if sparse_helper.is_STOP_sam(x)])
    values = len([ref[i] for i in range(len(ref)) if (type(ref[i]) is int or ref[i] == 'N')])

    assert(values <= total_stop)
    sig = []

    for i in range(total_stop):
        if i < values:
            t = int(random.uniform(0, rep_times))
            sig = sig + ['R' for i in range(t)]
        sig = sig + ['S']
    
    sig = sig + ['D']
    return sig


def create_gold(in_ref1, repsig):
    assert (len([in_ref1[i] for i in range(len(in_ref1)) if (type(in_ref1[i]) is int or in_ref1[i] == 'N') \
        and not sparse_helper.is_STOP_sam(in_ref1[i+1])])) \
        + len([x for x in in_ref1 if sparse_helper.is_STOP_sam(x)]) == \
        len([x for x in repsig if sparse_helper.is_STOP_sam(x)])
    assert (in_ref1[-1] == 'D' and repsig[-1] == 'D')
    
    i_r1_cpy = in_ref1[:]
    sig_cpy = repsig[:]

    # temporal fix
    i = 0
    j = 0
    mod_rep = []
    while i < len(in_ref1) - 1 and j < len(repsig) - 1:
        pre_value = False
        if type(in_ref1[i]) is int or in_ref1[i] == 'N':
            pre_value = True
        rs = []
        while repsig[j] == 'R':
            rs.append('R')
            j += 1
        if pre_value:
            mod_rep += rs
        mod_rep += [repsig[j]]

        i += 1
        j += 1

        if sparse_helper.is_STOP_sam(in_ref1[i]) and pre_value:
            i += 1
    mod_rep.append('D')
    # print(mod_rep)
    
    done = False
    time = 0
    in_ref2 = in_ref1[:]
    rep_t = Repeat_GLD()
    out_t = []
    while not done and time < TIMEOUT:
        if len(in_ref2) > 0:
            rep_t.set_in_ref(in_ref2.pop(0))
        if len(mod_rep) > 0:
            rep_t.set_in_repeat(mod_rep.pop(0))
        rep_t.update()
        out_t.append(rep_t.out_ref())
        done = rep_t.out_done()
        time += 1
    out_t = remove_emptystr(out_t)
    #end temp fix

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

    # print(i_r1_cpy)
    # print(sig_cpy)
    # print(out)
    # print("=============")

    # assert len(out) == len(sig_cpy)

    st = [i_r1_cpy, sig_cpy, out_t]
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

    elif test_name == "arr_9":
        ref = [0, 1, 'S0', 0, 1, 'S1', 'D']
        repeat = ['S', 'R', 'S', 'S', 'R', 'S', 'D']
        return create_gold(ref, repeat)

    elif test_name == "arr_10":
        ref = [1, 2, 'S0', 'S1', 3, 'S0', 4, 'S2', 'D']
        repeat = ['R', 'S', 'R', 'S', 'R', 'R', 'S', 'R', 'S', 'S', 'D']
        return create_gold(ref, repeat)   

    elif test_name == "arr_11":
        ref = [1, 2, 'S0', 'S0', 3, 'S0', 4, 'S1', 'D']
        repeat = ['R', 'S', 'R', 'S', 'S', 'R', 'S', 'S', 'D']
        return create_gold(ref, repeat)   

    elif test_name == "arr_12":
        ref = [1, 2, 'S0', 'S1', 'S1', 3, 'S0', 4, 'S2', 'D']
        repeat = create_repeat_sig(ref, 2)
        return create_gold(ref, repeat) 

    elif test_name == "arr_13":
        ref = ['S1', 1, 2, 'S0', 'S0', 3, 'S0', 4, 'S1', 'D']
        repeat = ['R', 'R', 'S', 'R', 'S', 'R', 'S', 'S', 'R', 'S', 'S', 'D']
        return create_gold(ref, repeat)

    elif test_name == "arr_14":
        ref = ['S1', 1, 2, 'S0', 'S0', 3, 'S0', 4, 'S0','S1', 'D']
        repeat = ['R', 'R', 'S', 'R', 'S', 'R', 'S', 'S', 'R', 'S', 'S', 'R', 'R', 'S', 'D']
        return create_gold(ref, repeat)

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        n = int(t_arg[1][0])
        rate = float(t_arg[2])
        size = int(t_arg[3])
        rep_times = int(t_arg[4])
        ref = create_random(n, rate, size)
        repeat = create_repeat_sig(ref, rep_times)
        return create_gold(ref, repeat)

    else:
        ref = [0, 'S0', 'D']
        repeat = ['S','D']
        return create_gold(ref, repeat)


def module_iter_basic(test_name, add_test=""):
    [ir1, rep, gc] = load_test_module(test_name)

    if add_test != "":
        additional_t = load_test_module(add_test)
        ir1 = ir1 + additional_t[0]
        rep = rep + additional_t[1]
        gc = gc + additional_t[2]

    print("ir1", ir1)
    print("rep", rep)
    print("gc", gc)

    sparse_helper.write_txt("pos_in_0.txt", ir1)
    sparse_helper.write_txt("pos_in_1.txt", rep)

    sparse_helper.clear_txt("pos_out_0.txt")
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=repeat_tb.sv", "TOP=repeat_tb",\
                             "TEST_UNIT=Repeat.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=repeat_tb.sv",\
                             "TOP=repeat_tb", "TX_NUM_GLB=2", "TEST_UNIT=Repeat.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    assert output.find("Valid signal fails to end") == -1, "Valid signal fails to end"
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    tx_num = 1
    if add_test != "":
        tx_num = 2
    pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", count=tx_num)
    
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
                "arr_5", "arr_6", "arr_7", "arr_8", \
                "arr_9", "arr_10", "arr_11", "arr_12", "arr_13", "arr_14", "empty"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1d():
    init_module()
    test_list = ["rd_1d_0.1_200_5", "rd_1d_0.3_200_5", "rd_1d_0.5_200_5", "rd_1d_0.8_200_5", "rd_1d_1.0_200_5"]
    for test in test_list:
        module_iter_basic(test)


def test_random_2d():
    init_module()
    test_list = ["rd_2d_0.1_200_5", "rd_2d_0.3_200_5", "rd_2d_0.5_200_5", "rd_2d_0.8_200_5", "rd_2d_1.0_200_5"]
    for test in test_list:
        module_iter_basic(test)


def test_random_3d():
    init_module()
    test_list = ["rd_3d_0.1_200_5", "rd_3d_0.3_200_5", "rd_3d_0.5_200_5", "rd_3d_0.8_200_5", "rd_3d_1.0_200_5"]
    for test in test_list:
        module_iter_basic(test)


def test_seq():
    init_module()
    test_list = ["arr_1", "arr_2", "arr_3", "arr_4", \
                "arr_5", "arr_6", "arr_7", "arr_8", \
                "arr_9", "arr_10", "arr_11", "arr_12", "arr_13", "arr_14", "empty"]
    for i in range(10):
        rand = random.sample(test_list, 2)
        module_iter_basic(rand[0], rand[1])
