from lake.modules.repeat_signal_generator import *
import magma as m
from magma import *
import tempfile
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.repeater import RepeatSigGen
from sam.sim.test.test import TIMEOUT


import subprocess
import os
import random
random.seed(15)
import string


def init_module():
    dut = RepeatSignalGenerator(data_width=16,
                    passthru=False,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)
    # magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    verilog(dut, filename=f"./modules/RepeatSig.sv",
            optimize_if=False)
    sparse_helper.update_tcl("repeatsig_tb")

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
                ret_t = ret_t + create_random_fiber(rate, d2, 0.2, "coord")
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
                

def create_gold(in_crd1):
    assert (in_crd1[-1] == 'D')
    
    i_c1_cpy = in_crd1[:]

    done = False
    time = 0

    repsig = RepeatSigGen()
    out = []

    while not done and time < TIMEOUT:
        if len(in_crd1) > 0:
            repsig.set_istream(in_crd1.pop(0))

        repsig.update()

        out.append(repsig.out_repeat())

        # print("Timestep", time, "\t Done:", repsig.out_done(), "\t Repeat Sig:", repsig.out_repeat())

        done = repsig.out_done()
        time += 1

    print("sam cycle count: ", time)
    out = remove_emptystr(out)

    assert len(out) == len(i_c1_cpy)
    st = [i_c1_cpy, out]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))
    return tr_st


def load_test_module(test_name):
    if test_name == "direct_2d":
        in_crd1 = [0, 'S0', 0, 1, 2, 'S1', 'D']

        return create_gold(in_crd1)
    
    elif test_name == "direct_1d":
        in1 = 16
        in_crd1 = [x for x in range(in1)] + ['S0', 'D']

        return create_gold(in_crd1)

    elif test_name == "empty_2d":
        in_crd1 = ['S0', 'S0', 'S0', 'S1', 'D']
        return create_gold(in_crd1)

    elif test_name == "simple_3d":
        in_crd1 = [0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S2', 'D']
        return create_gold(in_crd1)

    elif test_name == "arr_1":
        in_crd1 = [0, 1, 'S0', 2, 'S0', 3, 'S1', 'D']
        return create_gold(in_crd1)

    elif test_name == "arr_2":
        in_crd1 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 3, 'S1', 4, 5, 'S2', 'D']
        return create_gold(in_crd1)

    elif test_name == "arr_3":
        in_crd1 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'S1', 4, 5, 'S2', 'D']
        return create_gold(in_crd1)

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        n = int(t_arg[1][0])
        rate = float(t_arg[2])
        size = int(t_arg[3])
        in_crd1 = create_random(n, rate, size)
        return create_gold(in_crd1)

    else:
        in_crd1 = [0, 'S0', 'D']
        return create_gold(in_crd1)


def module_iter_basic(test_name, add_test=""):
    [ic1, gc] = load_test_module(test_name)

    if add_test != "":
        additional_t = load_test_module(add_test)
        ic1 = ic1 + additional_t[0]
        gc = gc + additional_t[1]

    # print("ic1", ic1)
    # print("gc", gc)

    sparse_helper.write_txt("coord_in_0.txt", ic1)

    sparse_helper.clear_txt("coord_out.txt")
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=repeatsig_tb.sv", "TOP=repeatsig_tb",\
                             "TEST_UNIT=RepeatSig.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=repeatsig_tb.sv",\
                             "TOP=repeatsig_tb", "TX_NUM_GLB=2", "TEST_UNIT=RepeatSig.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    tx_num = 1
    if add_test != "":
        tx_num = 2
    coord_out = sparse_helper.read_txt("coord_out.txt", count=tx_num)

    # print(coord_out)

    #compare each element in the output from coord_out.txt with the gold output
    assert len(coord_out) == len(gc), \
        f"Output length {len(coord_out)} didn't match gold length {len(gc)}"
    for i in range(len(coord_out)):
        if sparse_helper.is_DONE(gc[i]):
            assert sparse_helper.is_DONE(coord_out[i]), \
                f"Output {coord_out[i]} didn't match gold {gc[i]} at index {i}"
        elif sparse_helper.is_STOP(gc[i]):
            assert coord_out[i] == ic1[i], \
                f"Output {coord_out[i]} didn't match gold {gc[i]} at index {i}"
        else:
            assert coord_out[i] == gc[i], \
                f"Output {coord_out[i]} didn't match gold {gc[i]} at index {i}"    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["arr_1", "arr_2", "arr_3", "direct_1d", "direct_2d", "xxx", "empty_2d"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1d():
    init_module()
    test_list = ["rd_1d_0.1_400", "rd_1d_0.3_400", "rd_1d_0.5_400", "rd_1d_0.8_400", "rd_1d_1.0_400"]
    for test in test_list:
        module_iter_basic(test)


def test_random_2d():
    init_module()
    test_list = ["rd_2d_0.1_400", "rd_2d_0.3_400", "rd_2d_0.5_400", "rd_2d_0.8_400", "rd_2d_1.0_400"]
    for test in test_list:
        module_iter_basic(test)


def test_seq():
    init_module()
    test_list =  ["rd_1d_0.1_400", "rd_1d_0.3_400", "rd_1d_0.5_400", "rd_1d_0.8_400", "rd_1d_1.0_400"] +\
                 ["rd_2d_0.1_400", "rd_2d_0.3_400", "rd_2d_0.5_400", "rd_2d_0.8_400", "rd_1d_1.0_400"] 
    for i in range(10):
        rand = random.sample(test_list, 2)
        module_iter_basic(rand[0], rand[1])
