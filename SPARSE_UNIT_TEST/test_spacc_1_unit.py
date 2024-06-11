from lake.modules.intersect import *
import magma as m
from magma import *
import tempfile
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.accumulator import SpAcc1
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
    sparse_helper.update_tcl("unioner_tb")

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
                

def create_gold(ocrd_in, icrd_in, val_in):
    assert (len(icrd_in) == len(val_in))
    assert (len([x for x in ocrd_in if type(x) is int]) == len([x for x in icrd_in if sparse_helper.is_STOP_sam(x)]))
    assert (len([x for x in icrd_in if type(x) is not int]) == len([x for x in val_in if type(x) is not int]))
    
    ic_cpy = ocrd_in[:]
    oc_cpy = icrd_in[:]
    v_cpy = val_in[:]

    sa = SpAcc1(valtype=int)

    done = False
    time = 0
    out_icrd = []
    out_val = []

    while not done and time < TIMEOUT:
        if len(icrd_in) > 0:
            sa.set_in_crd0(icrd_in.pop(0))
        if len(ocrd_in) > 0:
            sa.set_in_crd1(ocrd_in.pop(0))
        if len(val_in) > 0:
            sa.set_val(val_in.pop(0))

        sa.update()

        out_icrd.append(sa.out_crd0())
        out_val.append(sa.out_val())

        # print("Timestep", time, "\t Done:", sa.out_done())

        done = sa.out_done()
        time += 1

    out_icrd = remove_emptystr(out_icrd)
    out_val = remove_emptystr(out_val)

    print("sam cycle count: ", time)

    assert len(out_icrd) == len(out_val)
    st = [ic_cpy, oc_cpy, v_cpy, out_icrd, out_val]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))

    return tr_st


def load_test_module(test_name):
    if test_name == "direct_3d":
        ocrd_in = [0, 2, 'S0', 2, 'S1', 'D']
        icrd_in = [0, 2, 3, 'S0', 0, 2, 3, 'S1', 0, 2, 3, 'S2', 'D']
        val_in = [50, 5, 10, 'S0', 40, 4, 8, 'S1', -40, 33, 36, 'S2', 'D']
        # icrd_gold = [0, 2, 3, 'S0', 0, 2, 3, 'S1', 'D']
        # val_gold = [90, 9, 18, 'S0', -40, 33, 36, 'S1', 'D']

        return create_gold(ocrd_in, icrd_in, val_in)

    if test_name == "direct_2d":
        ocrd_in = [0, 2, 4, 'S0', 'D']
        icrd_in = [1, 'S0', 0, 2, 'S0', 1, 3, 'S1', 'D']
        val_in = [1, 'S0', 2, 3, 'S0', 4, 5, 'S1', 'D']
        # icrd_gold = [0, 1, 2, 3, 'S0', 'D']
        # val_gold = [2, 5, 3, 5, 'S0', 'D']

        return create_gold(ocrd_in, icrd_in, val_in)

    if test_name == "direct_4d":
        ocrd_in = [0, 2, 'S0', 2, 'S2', 'D']
        icrd_in = [0, 2, 3, 'S0', 0, 2, 3, 'S1', 0, 2, 3, 'S3', 'D']
        val_in = [50, 5, 10, 'S0', 40, 4, 8, 'S1', -40, 33, 36, 'S3', 'D']
        # icrd_gold = [0, 2, 3, 'S0', 0, 2, 3, 'S1', 'D']
        # val_gold = [90, 9, 18, 'S0', -40, 33, 36, 'S1', 'D']

        return create_gold(ocrd_in, icrd_in, val_in)

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        n = int(t_arg[1][0])
        rate = float(t_arg[2])
        size = int(t_arg[3])
        [in_crd1, in_ref1, in_crd2, in_ref2] = create_random(n, rate, size)
        return create_gold(in_crd1, in_crd2, in_ref1)

    else:
        in_crd1 = [0, 'S0', 'D']
        in_ref1 = [0, 'S0', 'D']
        in_crd2 = [0, 'S0', 'D']
        in_ref2 = [0, 'S0', 'D']

        return create_gold(in_crd1, in_crd2, in_ref1)


def module_iter_basic(test_name, add_test=""):
    [ic, oc, v, out_c, out_v] = load_test_module(test_name)

    if add_test != "":
        additional_t = load_test_module(add_test)
        ic = ic + additional_t[0]
        oc = oc + additional_t[1]
        v = v + additional_t[2]
        out_c = out_c + additional_t[3]
        out_v = out_v + additional_t[4]

    print("ic", ic)
    print("oc", oc)
    print("v", v)
    print("out_c", out_c)
    print("out_v", out_v)

    return

    sparse_helper.write_txt("coord_in_0.txt", ic)
    sparse_helper.write_txt("coord_in_1.txt", oc)
    sparse_helper.write_txt("pos_in_0.txt", v)

    sparse_helper.clear_txt("coord_out.txt")
    sparse_helper.clear_txt("pos_out_0.txt")
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=unioner_tb.sv", "TOP=unioner_tb",\
                             "TEST_UNIT=Intersect.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=unioner_tb.sv",\
                             "TOP=unioner_tb", "TX_NUM_GLB=2", "TEST_UNIT=Intersect.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    tx_num = 1
    if add_test != "":
        tx_num = 2
    coord_out = sparse_helper.read_txt("coord_out.txt", count=tx_num)
    pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", count=tx_num)
    pos_out_1 = sparse_helper.read_txt("pos_out_1.txt", count=tx_num)

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
    test_list = ["direct_4d", "direct_3d", "direct_2d"]
    for test in test_list:
        module_iter_basic(test)
