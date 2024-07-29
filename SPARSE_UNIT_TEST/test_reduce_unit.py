import magma as m
import tempfile
import kratos as k

import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.accumulator import Reduce
from sam.sim.test.test import TIMEOUT
from hwtypes.modifiers import strip_modifiers
from peak.family import PyFamily
from peak.assembler import Assembler
from lassen.sim import PE_fc as lassen_fc
import lassen.asm as asm

import subprocess
import os
import random
random.seed(15)
import string


def init_module():
    sparse_helper.update_tcl("reduce_tb")

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
        val = [int(random.uniform(0, 2**10-1)) for i in range(s)]
        return val

def create_random(n, rate, size, d1=0):
    if n == 1:
        in_crd1 = create_random_fiber(rate, size, 0.2, "val") + ['S0', 'D']
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
                ret_t = ret_t + create_random_fiber(rate, d2, 0.2, "val")
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
                

def create_gold(in_val):
    assert (in_val[-1] == 'D')
    
    i_v_cpy = in_val[:]

    done = False
    time = 0

    red = Reduce()
    out = []

    while not done and time < TIMEOUT:
        if len(in_val) > 0:
            red.set_in_val(in_val.pop(0))

        red.update()

        out.append(red.out_val())

        # print("Timestep", time, "\t Red:", red.out_val(), "\t Ref1:", )

        done = red.done
        time += 1

    print("sam cycle count: ", time)
    out = remove_emptystr(out)

    st = [i_v_cpy, out]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))
    return tr_st


def load_test_module(test_name):
    if test_name == "direct_2d":
        in_val = [5, 5, 'S0', 5, 'S0', 4, 8, 'S0', 4, 3, 'S0', 4, 3, 'S1', 'D']
        return create_gold(in_val)
    
    elif test_name == "direct_3d":
        in_val = [1, 2, 3, 4, 5, 6, 7, 8, 9, 'S2', 'D']
        return create_gold(in_val)

    elif test_name == "empty_2d":
        in_val = [5, 5, 'S0', 'S0', 4, 8, 'S0', 4, 3, 'S0', 4, 3, 'S1', 'D']
        return create_gold(in_val)

    elif test_name == "empty_end":
        in_val = [5, 5, 'S0', 'S0', 4, 8, 'S0', 4, 3, 'S0', 'S1', 'D']
        return create_gold(in_val)

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        n = int(t_arg[1][0])
        rate = float(t_arg[2])
        size = int(t_arg[3])
        in_val = create_random(n, rate, size)
        return create_gold(in_val)

    else:
        in_val = [0, 'S0', 'D']
        return create_gold(in_val)


def module_iter_basic(test_name, add_test=""):
    [iv, gv] = load_test_module(test_name)

    if add_test != "":
        additional_t = load_test_module(add_test)
        iv = iv + additional_t[0]
        gv = gv + additional_t[1]

    print("iv", iv)
    print("gv", gv)

    sparse_helper.write_txt("coord_in_0.txt", iv)
    sparse_helper.clear_txt("coord_out.txt")

    instr_type = strip_modifiers(lassen_fc.Py.input_t.field_dict['inst'])
    asm_ = Assembler(instr_type)
    # configurate pe in cluster to perform add
    config = hex(int(asm_.assemble(asm.add())))
    num_inputs = 0b011
    print(config)


    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=reduce_tb.sv", "TOP=reduce_tb",\
                             "TEST_UNIT=garnet.v", f"INST=+inst={config}", f"NUM_INPUTS=+num_inputs={num_inputs}"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=reduce_tb.sv",\
                             "TOP=reduce_tb", "TX_NUM_GLB=2", "TEST_UNIT=garnet.v",\
                             f"INST=+inst={config}", f"NUM_INPUTS=+num_inputs={num_inputs}"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    tx_num = 1
    if add_test != "":
        tx_num = 2
    val_out = sparse_helper.read_txt("coord_out.txt", count=tx_num)

    # print(val_out)

    #compare each element in the output from coord_out.txt with the gold output
    assert len(val_out) == len(gv), \
        f"Output length {len(val_out)} didn't match gold length {len(gv)}"
    for i in range(len(val_out)):
        assert val_out[i] == gv[i], \
            f"Output {val_out[i]} didn't match gold {gv[i]} at index {i}" 
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["direct_2d", "direct_3d", "empty_2d", "empty_end", "xxx"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1d():
    init_module()
    test_list = ["rd_1d_0.1_100", "rd_1d_0.3_100", "rd_1d_0.5_100", "rd_1d_0.8_100", "rd_1d_1.0_100"]
    for test in test_list:
        module_iter_basic(test)


def test_random_2d():
    init_module()
    test_list = ["rd_2d_0.1_200", "rd_2d_0.3_200", "rd_2d_0.5_200", "rd_2d_0.8_200", "rd_2d_1.0_200"]
    for test in test_list:
        module_iter_basic(test)


def test_random_3d():
    init_module()
    test_list = ["rd_3d_0.1_200", "rd_3d_0.3_200", "rd_3d_0.5_200", "rd_3d_0.8_200", "rd_3d_1.0_200"]
    for test in test_list:
        module_iter_basic(test)


def test_seq():
    init_module()
    test_list =  ["rd_1d_0.1_100", "rd_1d_0.3_100", "rd_1d_0.5_100", "rd_1d_0.8_100", "rd_1d_1.0_100"] +\
                 ["rd_2d_0.1_200", "rd_2d_0.3_200", "rd_2d_0.5_200", "rd_2d_0.8_200", "rd_2d_1.0_200"] +\
                 ["rd_3d_0.1_200", "rd_3d_0.3_200", "rd_3d_0.5_200", "rd_3d_0.8_200", "rd_3d_1.0_200"]
    for i in range(10):
        rand = random.sample(test_list, 2)
        module_iter_basic(rand[0], rand[1])
