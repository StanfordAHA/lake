from lake.modules.intersect import *
import magma as m
from magma import *
import tempfile
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.joiner import Union2
from sam.sim.test.test import TIMEOUT


import subprocess
import os
import random
import string
random.seed(15)


def init_module():
    dut = Intersect(data_width=16,
                    use_merger=False,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)
    # magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    verilog(dut, filename=f"./modules/Intersect.sv",
            optimize_if=False)
    sparse_helper.update_tcl("unioner_vmode_tb")

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
        val = [random.randint(0, 2**15-1) for i in range(s)]
        return val

def create_random(n, rate, size, d1=0):
    if n == 1:
        in_crd1 = create_random_fiber(rate, size, 0.2, "coord") + ['S0', 'D']
        in_ref1 = create_random_fiber(1, len(in_crd1) - 2, 0, "value") + ['S0', 'D']
        in_crd2 = create_random_fiber(rate, size, 0.2, "coord") + ['S0', 'D']
        in_ref2 = create_random_fiber(1, len(in_crd2) - 2, 0, "value") + ['S0', 'D']
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
                ret_t[1] = create_random_fiber(1, len(ret_t[0]), 0, "value")
            if random.random() < rate:
                ret_t[2] = create_random_fiber(rate, d2, 0.2, "coord")
                ret_t[3] = create_random_fiber(1, len(ret_t[2]), 0, "value")

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
                

def create_gold(in_crd1, in_crd2, in_val1, in_val2):
    assert (len(in_crd1) == len(in_val1))
    assert (len(in_crd2) == len(in_val2))
    assert (len([x for x in in_crd1 if type(x) is not int]) == len([x for x in in_crd2 if type(x) is not int]))
    assert (len([x for x in in_val1 if type(x) is not int]) == len([x for x in in_val2 if type(x) is not int]))
    
    out_crd = []
    out_val1 = []

    p1 = 0
    p2 = 0
    time = 0
    while p1 < len(in_crd1) or p2 < len(in_crd2):
        time += 1
        if sparse_helper.is_STOP_sam(in_crd1[p1]):
            if sparse_helper.is_STOP_sam(in_crd2[p2]):
                out_crd.append(in_crd1[p1])
                out_val1.append(in_crd1[p1])
                p1 += 1
                p2 += 1
            else:
                out_crd.append(in_crd2[p2])
                out_val1.append(in_val2[p2])
                p2 += 1
        elif sparse_helper.is_STOP_sam(in_crd2[p2]):
            out_crd.append(in_crd1[p1])
            out_val1.append(in_val1[p1])
            p1 += 1
        elif sparse_helper.is_DONE_sam(in_crd1[p1]) and sparse_helper.is_DONE_sam(in_crd2[p2]):
            out_crd.append(in_crd1[p1])
            out_val1.append(in_crd1[p1])
            p1 += 1
            p2 += 1
        elif in_crd1[p1] < in_crd2[p2]:
            out_crd.append(in_crd1[p1])
            out_val1.append(in_val1[p1])
            p1 += 1
        elif in_crd1[p1] > in_crd2[p2]:
            out_crd.append(in_crd2[p2])
            out_val1.append(in_val2[p2])
            p2 += 1
        elif in_crd1[p1] == in_crd2[p2]:
            out_crd.append(in_crd2[p2])
            out_val1.append(in_val1[p1] + in_val2[p2])
            p1 += 1
            p2 += 1
        else:
            print("got some issue!!!!")
            print(in_crd1[p1])
            print(in_crd2[p2])
    
    print("No equivalent SAM primitive, approximation time:", time)

    out_val2 = out_val1[:]
    assert len(out_crd) == len(out_val1)
    st = [in_crd1, in_crd2, in_val1, in_val2, out_crd, out_val1, out_val2]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))

    return tr_st


def load_test_module(test_name):
    if test_name == "direct_2d":
        in_crd1 = [0, 'S0', 0, 1, 2, 'S1', 'D']
        in_val1 = [10, 'S0', 1, 2, 3, 'S1', 'D']
        in_crd2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']
        in_val2 = [777, 10, 92, 'S0', 101, 1, 2, 'S1', 'D']

        return create_gold(in_crd1, in_crd2, in_val1, in_val2)
    
    elif test_name == "direct_1d":
        in1 = 16
        in_crd1 = [x for x in range(in1)] + ['S0', 'D']
        in_val1 = [random.randint(0, 1024) for x in range(in1)] + ['S0', 'D']
        in_crd2 = [0, 2, 4, 15, 17, 25, 31, 32, 50, 63, 'S0', 'D']
        in_val2 = [random.randint(0, 1024) for x in range(10)] + ['S0', 'D']

        return create_gold(in_crd1, in_crd2, in_val1, in_val2)

    elif test_name == "empty_start":
        in_crd1 = [0, 1, 2, 'S1', 'D']
        in_val1 = [1, 2, 3, 'S1', 'D']
        in_crd2 = ['S1', 'D']
        in_val2 = ['S1', 'D']
        return create_gold(in_crd1, in_crd2, in_val1, in_val2)

    elif test_name == "empty_full":
        in_crd1 = ['S0', 'S0', 'S0', 'S1', 'D']
        in_val1 = ['S0', 'S0', 'S0', 'S1', 'D']
        in_crd2 = ['S0', 'S0', 'S0', 'S1', 'D']
        in_val2 = ['S0', 'S0', 'S0', 'S1', 'D']
        return create_gold(in_crd1, in_crd2, in_val1, in_val2)

    elif test_name == "simple_3d":
        in_crd1 = [0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S2', 'D']
        in_val1 = [0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S2', 'D']
        in_crd2 = [1, 'S0', 'S1', 0, 1, 'S0', 0, 'S1', 'S0', 0, 1, 'S2', 'D']
        in_val2 = [9, 'S0', 'S1', 8, 7, 'S0', 6, 'S1', 'S0', 4, 3, 'S2', 'D']
        return create_gold(in_crd1, in_crd2, in_val1, in_val2)

    elif test_name == "array_1d":
        in_crd1 = [0, 1, 3, 5, 'S0', 'D']
        in_val1 = [0, 1, 2, 3, 'S0', 'D']
        in_crd2 = [0, 2, 3, 4, 'S0', 'D']
        in_val2 = [0, 1, 2, 3, 'S0', 'D']
        return create_gold(in_crd1, in_crd2, in_val1, in_val2)

    elif test_name == "array_2d":
        in_crd1 = [0, 1, 'S0', 2, 3, 'S0', 'S0', 4, 5, 'S1', 'D']
        in_val1 = [0, 1, 'S0', 2, 3, 'S0', 'S0', 4, 5, 'S1', 'D']
        in_crd2 = [1, 2, 3, 'S0', 'S0', 0, 1, 2, 'S0', 'S1', 'D']
        in_val2 = [0, 1, 2, 'S0', 'S0', 2, 3, 4, 'S0', 'S1', 'D']
        return create_gold(in_crd1, in_crd2, in_val1, in_val2)

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        n = int(t_arg[1][0])
        rate = float(t_arg[2])
        size = int(t_arg[3])
        [in_crd1, in_val1, in_crd2, in_val2] = create_random(n, rate, size)
        return create_gold(in_crd1, in_crd2, in_val1, in_val2)

    else:
        in_crd1 = [0, 'S0', 'D']
        in_val1 = [0, 'S0', 'D']
        in_crd2 = [0, 'S0', 'D']
        in_val2 = [0, 'S0', 'D']

        return create_gold(in_crd1, in_crd2, in_val1, in_val2)


def module_iter_basic(test_name, add_test=""):
    [ic1, ic2, iv1, iv2, gc, gv1, gv2] = load_test_module(test_name)

    if add_test != "":
        additional_t = load_test_module(add_test)
        ic1 = ic1 + additional_t[0]
        ic2 = ic2 + additional_t[1]
        iv1 = iv1 + additional_t[2]
        iv2 = iv2 + additional_t[3]
        gc = gc + additional_t[4]
        gv1 = gv1 + additional_t[5]
        gv2 = gv2 + additional_t[6]

    # print("ic1", ic1)
    # print("ic2", ic2)
    # print("iv1", iv1)
    # print("iv2", iv2)
    # print("gc", gc)
    # print("gv1", gv1)
    # print("gv2", gv2)

    sparse_helper.write_txt("coord_in_0.txt", ic1)
    sparse_helper.write_txt("coord_in_1.txt", ic2)
    sparse_helper.write_txt("pos_in_0.txt", iv1)
    sparse_helper.write_txt("pos_in_1.txt", iv2)

    sparse_helper.clear_txt("coord_out.txt")
    sparse_helper.clear_txt("pos_out_0.txt")
    sparse_helper.clear_txt("pos_out_1.txt") 
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=unioner_vmode_tb.sv", "TOP=unioner_vmode_tb",\
                             "TEST_UNIT=Intersect.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=unioner_vmode_tb.sv",\
                             "TOP=unioner_vmode_tb", "TX_NUM_GLB=2", "TEST_UNIT=Intersect.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    coord_out = sparse_helper.read_txt("coord_out.txt", addit=add_test != "")
    pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", addit=add_test != "")
    pos_out_1 = sparse_helper.read_txt("pos_out_1.txt", addit=add_test != "")
    
    # print(coord_out)
    # print(pos_out_0)
    # print(pos_out_1)

    #compare each element in the output from coord_out.txt with the gold output
    assert len(coord_out) == len(gc), \
        f"Output length {len(coord_out)} didn't match gold length {len(gc)}"
    for i in range(len(coord_out)):
        assert coord_out[i] == gc[i], \
            f"Output {coord_out[i]} didn't match gold {gc[i]} at index {i}"
    
    #compare each element in the output from pos_out_0.txt with the gold output
    assert len(pos_out_0) == len(gv1), \
        f"Output length {len(pos_out_0)} didn't match gold length {len(gv1)}"
    for i in range(len(pos_out_0)):
        assert pos_out_0[i] == gv1[i], \
            f"Output {pos_out_0[i]} didn't match gold {gv1[i]} at index {i}"
    
    #compare each element in the output from pos_out_1.txt with the gold output
    assert len(pos_out_1) == len(gv2), \
        f"Output length {len(pos_out_1)} didn't match gold length {len(gv2)}"
    for i in range(len(pos_out_1)):
        assert pos_out_1[i] == gv2[i], \
            f"Output {pos_out_1[i]} didn't match gold {gv2[i]} at index {i}"
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["direct_1d", "direct_2d", "xxx", "empty_full", "empty_start", "array_1d", "array_2d"]
    for test in test_list:
        module_iter_basic(test)


# def test_random_1d():
#     init_module()
#     test_list = ["rd_1d_0.1_400", "rd_1d_0.3_400", "rd_1d_0.5_400", "rd_1d_0.8_400", "rd_1d_1.0_400"]
#     for test in test_list:
#         module_iter_basic(test)


# def test_random_2d():
#     init_module()
#     test_list = ["rd_2d_0.1_400", "rd_2d_0.3_400", "rd_2d_0.5_400", "rd_2d_0.8_400", "rd_2d_1.0_400"]
#     for test in test_list:
#         module_iter_basic(test)


# def test_random_3d():
#     init_module()
#     test_list = ["rd_3d_0.1_400", "rd_3d_0.3_400", "rd_3d_0.5_400", "rd_3d_0.8_400", "rd_3d_1.0_400"]
#     for test in test_list:
#         module_iter_basic(test) 


# def test_seq():
#     init_module()
#     test_list =  ["rd_1d_0.1_400", "rd_1d_0.3_400", "rd_1d_0.5_400", "rd_1d_0.8_400", "rd_1d_1.0_400"] +\
#                  ["rd_2d_0.1_400", "rd_2d_0.3_400", "rd_2d_0.5_400", "rd_2d_0.8_400", "rd_1d_1.0_400"] +\
#                  ["rd_3d_0.1_400", "rd_3d_0.3_400", "rd_3d_0.5_400", "rd_3d_0.8_400", "rd_1d_1.0_400"] +\
#                  ["direct_1d", "direct_2d", "xxx", "empty_full", "empty_start", "array_1d", "array_2d"]
#     for i in range(10):
#         rand = random.sample(test_list, 2)
#         module_iter_basic(rand[0], rand[1])
