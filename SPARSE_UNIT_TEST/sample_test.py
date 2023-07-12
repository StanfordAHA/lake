from lake.modules.intersect import *
import magma as m
from magma import *
import tempfile
import kratos as k


from sam_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.joiner import Intersect2
from sam.sim.test.test import TIMEOUT


import subprocess
import os
import random
random.seed(15)
import string


def init_module():
    dut = Intersect(data_width=16,
                    use_merger=False,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)
                    # data_width=16,
                    # use_merger=False,
                    # fifo_depth=2,
                    # defer_fifos=True,
                    # add_flush=False,
                    # perf_debug=perf_debug
    magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)


def create_gold(in_crd1, in_crd2, in_ref1, in_ref2):
    assert (len(in_crd1) == len(in_ref1))
    assert (len(in_crd2) == len(in_ref2))
    
    i_c1_cpy = in_crd1[:]
    i_c2_cpy = in_crd2[:]
    i_r1_cpy = in_ref1[:]
    i_r2_cpy = in_ref2[:]

    done = False
    time = 0

    inter = Intersect2()
    out_crd = []
    out_ref1 = []
    out_ref2 = []

    while not done and time < TIMEOUT:
        if len(in_crd1) > 0:
            inter.set_in1(in_ref1.pop(0), in_crd1.pop(0))
        if len(in_crd2) > 0:
            inter.set_in2(in_ref2.pop(0), in_crd2.pop(0))

        inter.update()

        out_crd.append(inter.out_crd())
        out_ref1.append(inter.out_ref1())
        out_ref2.append(inter.out_ref2())

        # print("Timestep", time, "\t Crd:", inter.out_crd(), "\t Ref1:", inter.out_ref1(), "\t Ref2:", inter.out_ref2())

        done = inter.done
        time += 1

    print("sam cycle count: ", time)
    out_crd = remove_emptystr(out_crd)
    out_ref1 = remove_emptystr(out_ref1)
    out_ref2 = remove_emptystr(out_ref2)

    assert len(out_crd) == len(out_ref1) and len(out_crd) == len(out_ref2)
    st = [i_c1_cpy, i_c2_cpy, i_r1_cpy, i_r2_cpy, out_crd, out_ref1, out_ref2]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))

    return tr_st


def load_test_module(test_name):
    if test_name == "direct_2d":
        in_crd1 = [0, 'S0', 0, 1, 2, 'S1', 'D']
        in_ref1 = [0, 'S0', 1, 2, 3, 'S1', 'D']
        in_crd2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']
        in_ref2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']

        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)
    
    elif test_name == "direct_1d":
        in1 = 16
        in_crd1 = [x for x in range(in1)] + ['S0', 'D']
        in_ref1 = [x for x in range(in1)] + ['S0', 'D']
        in_crd2 = [0, 2, 4, 15, 17, 25, 31, 32, 50, 63, 'S0', 'D']
        in_ref2 = [x for x in range(10)] + ['S0', 'D']

        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)

    elif test_name == "empty_2d":
        in_crd1 = ['S0', 'S0', 'S0', 'S1', 'D']
        in_ref1 = ['S0', 'S0', 'S0', 'S1', 'D']
        in_crd2 = ['S0', 'S0', 'S0', 'S1', 'D']
        in_ref2 = ['S0', 'S0', 'S0', 'S1', 'D']
        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)

    else:
        in_crd1 = [0, 'S0', 'D']
        in_ref1 = [0, 'S0', 'D']
        in_crd2 = [0, 'S0', 'D']
        in_ref2 = [0, 'S0', 'D']

        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)


def module_iter_basic(test_name):
    [ic1, ic2, ir1, ir2, gc, gr1, gr2] = load_test_module(test_name)

    #convert each element of ic1 to hex and write to file coord_in_0.txt
    with open("coord_in_0.txt", "w") as f:
        for element in ic1:
            f.write(f'{element:x}' + '\n')
        f.close()

    #convert each element of ic2 to hex and write to file coord_in_1.txt
    with open("coord_in_1.txt", "w") as f:
        for element in ic2:
            f.write(f'{element:x}' + '\n')
        f.close()
    
    #convert each element of ir1 to hex and write to file ref_in_0.txt
    with open("pos_in_0.txt", "w") as f:
        for element in ir1:
            f.write(f'{element:x}' + '\n')
        f.close()
    
    #convert each element of ir2 to hex and write to file ref_in_1.txt
    with open("pos_in_1.txt", "w") as f:
        for element in ir2:
            f.write(f'{element:x}' + '\n')
        f.close()
    
    open("coord_out.txt", "w").close() 
    open("pos_out_0.txt", "w").close() 
    open("pos_out_1.txt", "w").close()    
    
    #run command "make sim" to run the simulation
    sim_result = subprocess.run(["make", "sim"], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    coord_out = []
    pos_out_0 = []
    pos_out_1 = []
    #read the output from coord_out.txt and convert each element from hex to int
    with open("coord_out.txt", "r") as f:
        for line in f:
            coord_out.append(int(line, 16))
            if int(line, 16) == 0x10100:
                break
        f.close()
    
    #read the output from pos_out_0.txt and convert each element from hex to int
    with open("pos_out_0.txt", "r") as f:
        for line in f:
            pos_out_0.append(int(line, 16))
            if int(line, 16) == 0x10100:
                break
        f.close()

    #read the output from pos_out_1.txt and convert each element from hex to int
    with open("pos_out_1.txt", "r") as f:
        for line in f:
            pos_out_1.append(int(line, 16))
            if int(line, 16) == 0x10100:
                break
        f.close()

    #compare each element in the output from coord_out.txt with the gold output
    assert len(coord_out) == len(gc), \
        f"Output length {len(coord_out)} didn't match gold length {len(gc)}"
    for i in range(len(coord_out)):
        assert coord_out[i] == gc[i], \
            f"Output {coord_out[i]} didn't match gold {gc[i]} at index {i}"
    
    #compare each element in the output from pos_out_0.txt with the gold output
    assert len(pos_out_0) == len(gr1), \
        f"Output length {len(pos_out_0)} didn't match gold length {len(gr1)}"
    for i in range(len(pos_out_0)):
        assert pos_out_0[i] == gr1[i], \
            f"Output {pos_out_0[i]} didn't match gold {gr1[i]} at index {i}"
    
    #compare each element in the output from pos_out_1.txt with the gold output
    assert len(pos_out_1) == len(gr2), \
        f"Output length {len(pos_out_1)} didn't match gold length {len(gr2)}"
    for i in range(len(pos_out_1)):
        assert pos_out_1[i] == gr2[i], \
            f"Output {pos_out_1[i]} didn't match gold {gr2[i]} at index {i}"
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["direct_1d", "direct_2d", "xxx", "empty_2d"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1d():


def test_random_2d():


def test_random_3d():
