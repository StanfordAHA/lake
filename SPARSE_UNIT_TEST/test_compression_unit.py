from lake.modules.crddrop import *
import magma as m
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.compression import ValDropper as ValDrop_sim
from sam.sim.test.test import TIMEOUT


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
    sparse_helper.update_tcl("compression_tb")

def create_random_fiber(crd_sparsity, size, val_zero_prob):
    # size = int(size*random.uniform(1.0, 1.0+d))
    s = int(crd_sparsity*size)
    crd = random.sample(range(size), s)
    crd.sort()
    val = []
    for c in crd: 
        if random.random() > val_zero_prob:
            val.append(random.randint(1, 1024))
        else:
            val.append(0)
    return crd, val



def create_random(dim, crd_sparsity, size, val_zero_prob):
    if dim == 1:
        in_crd, in_val = create_random_fiber(crd_sparsity, size, val_zero_prob)
        in_crd = in_crd + ['S0', 'D']
        in_val = in_val + ['S0', 'D']
        return in_crd, in_val
    
    elif dim == 2:
        d1 = int(random.uniform(2, int(size**(1/2))))
        d2 = max(size // d1, 1)
        # print("h: ", size, d1, d2, rate)
        ret_crd = []
        ret_val = []
        for i in range(d1):
            ret_t_crd = []
            ret_t_val = []
            in_crd, in_val = create_random_fiber(crd_sparsity, d2, val_zero_prob)
            ret_t_crd = ret_t_crd + in_crd
            ret_t_val = ret_t_val + in_val
            if i == d1 - 1:
                ret_crd = ret_crd + ret_t_crd + ['S1', 'D']
                ret_val = ret_val + ret_t_val + ['S1', 'D']
            else:
                ret_crd = ret_crd + ret_t_crd + ['S0']
                ret_val = ret_val + ret_t_val + ['S0']
        return ret_crd, ret_val
    elif dim == 3:
        d1 = int(random.uniform(2, int(size**(1/3))))
        total_d = d1
        d2 = size // d1
        ret_crd = []
        ret_val = []
        # print(d1, d2, rate, int(size**(1/3)), int(d1**(1/2)))
        for i in range(d1):
            ret_t_crd = []
            ret_t_val = []
            in_crd, in_val = create_random(2, crd_sparsity, d2, val_zero_prob)
            ret_t_crd = ret_t_crd + in_crd
            ret_t_val = ret_t_val + in_val
            ret_crd = ret_crd + ret_t_crd[:-1]
            ret_val = ret_val + ret_t_val[:-1]
        
        ret_crd = ret_crd[:-1] + ['S2', 'D']
        ret_val = ret_val[:-1] + ['S2', 'D']
        return ret_crd, ret_val


def remove_emptyfiber(out_in):
    r = []
    max_level = 0
    pushed_non_empty = False
    for i in range(len(out_in)):
        if sparse_helper.is_STOP_sam(out_in[i]):
            max_level = max(max_level, int(out_in[i][1]))
        if sparse_helper.is_STOP_sam(out_in[i]) and sparse_helper.is_STOP_sam(out_in[i + 1]): 
            continue
        if sparse_helper.is_STOP_sam(out_in[i]):
            if pushed_non_empty:
                r.append(f'S{max_level}')
            pushed_non_empty = False
        else:
            r.append(out_in[i])
            pushed_non_empty = True
        max_level = 0
        
    return r

def create_gold(in_crd, in_val): 
    assert(len(in_val) == len(in_crd))
    
    in_val_cpy = in_val[:]
    in_crd_cpy = in_crd[:]

    done = False
    time = 0

    vd = ValDrop_sim()
    out_crd = []
    out_val = []
    while not done and time < TIMEOUT:
        if len(in_val) > 0:
            input_val = in_val[0]
            vd.set_val(in_val.pop(0))
        if len(in_crd) > 0:
            input_crd = in_crd[0]
            vd.set_crd(in_crd.pop(0))

        vd.update()

        out_crd.append(vd.out_crd())
        out_val.append(vd.out_val())

        # print("Timestep", time, "\t Done:", vd.out_done(), "\t In: ",input_crd, input_val, "\t Out:", vd.out_crd(), vd.out_val())

        done = vd.out_done()
        time += 1

    out_crd = remove_emptystr(out_crd)
    out_val = remove_emptystr(out_val)

    print("sam cycle count: ", time)

    st = [in_crd_cpy, in_val_cpy, out_crd, out_val]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))

    return tr_st, time


def load_test_module(test_name):
    if test_name == "stream_1":
        in_crd = [0, 2, 'S0', 'D']
        in_val = [0, 1, 'S0', 'D']

        return create_gold(in_crd, in_val)

    elif test_name == "stream_2":
        in_crd = [0, 'S0', 0, 'S1', 'D']
        in_val =  [1, 'S0', 15, 'S1', 'D']

        return create_gold(in_crd, in_val)

    elif test_name == "stream_3":
        in_crd = [0, 1, 2, 'S0', 'S1', 'D']
        in_val = [1, 0, 0, 'S0', 'S1', 'D']

        return create_gold(in_crd, in_val)

    elif test_name == "stream_4":
        in_crd = [1, 2, 'S0', 5, 'S1', 'D']
        in_val = [1, 2, 'S0', 0, 'S1', 'D']

        return create_gold(in_crd, in_val)

    elif test_name == "stream_5":
        in_crd = [1, 2, 3, 'S0', 'D']
        in_val = [0, 0, 0, 'S0', 'D']

        return create_gold(in_crd, in_val)

    elif test_name == "stream_6":
        in_crd = [1, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_val = [1, 2, 3, 'S0', 0, 0, 1, 'S1', 'D']

        return create_gold(in_crd, in_val)

    elif test_name == "stream_7":
        in_crd = [1, 2, 3, 'S0', 4, 5, 'S1', 'S2', 'D']
        in_val = [1, 0, 0, 'S0', 0, 0, 'S1', 'S2', 'D']

        return create_gold(in_crd, in_val)

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        dim = int(t_arg[1][0])
        crd_sparsity = float(t_arg[2])
        size = int(t_arg[3])
        val_zero_prob = float(t_arg[4])
        in_crd, in_val = create_random(dim, crd_sparsity, size, val_zero_prob)

        return create_gold(in_crd, in_val)

    else:
        in_crd = [1, 'S0', 'D']
        in_val = [0, 'S0', 'D']

        return create_gold(in_crd, in_val)


def module_iter_basic(test_name, add_test=""):
    [ic, iv, gc, gv], sam_cycs = load_test_module(test_name)
    if add_test != "":
        additional_t = load_test_module(add_test)
        ic = ic + additional_t[0]
        iv = iv + additional_t[1]
        gc = gc + additional_t[2]
        gv = gv + additional_t[3]

    print("input crd", ic)
    print("input val", iv)
    print("gold crd", gc)
    print("gold val", gv)

    sparse_helper.write_txt("val_in.txt", iv)
    sparse_helper.write_txt("coord_in.txt", ic)

    sparse_helper.clear_txt("val_out.txt")
    sparse_helper.clear_txt("coord_out.txt") 
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=compression_tb.sv", "TOP=compression_tb",\
                             "TEST_UNIT=CrdDrop.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=compression_tb.sv",\
                             "TOP=compression_tb", "TX_NUM_GLB=2", "TEST_UNIT=CrdDrop.sv"\
                             ], capture_output=True, text=True)    
    output = sim_result.stdout
    assert output.find("Valid signal fails to end") == -1, "Valid signal fails to end"
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])
    hw_cycs = int(cycle_count_line.splitlines()[0].split(':')[1])
    # hw_cycs = 0
    
    val_out = sparse_helper.read_txt("val_out.txt", addit=add_test != "")
    crd_out = sparse_helper.read_txt("coord_out.txt", addit=add_test != "")

    # print(pos_out_0)
    # print(pos_out_1)
    
    #compare each element in the output from pos_out_0.txt with the gold output
    assert len(val_out) == len(gv), \
        f"Value stream output length {len(val_out)} didn't match gold length {len(gv)}"
    for i in range(len(val_out)):
        assert val_out[i] == gv[i], \
            f"Value stream output {val_out[i]} didn't match gold {gv[i]} at index {i}"
    
    #compare each element in the output from pos_out_1.txt with the gold output
    assert len(crd_out) == len(gc), \
        f"Coordinate stream output length {len(crd_out)} didn't match gold length {len(gc)}"
    for i in range(len(crd_out)):
        assert crd_out[i] == gc[i], \
            f"Output {crd_out[i]} didn't match gold {gc[i]} at index {i}"
    
    print(test_name, " passed\n")
    sparse_helper.write_csv('compression', test_name, hw_cycs, sam_cycs)


def test_iter_basic():
    init_module()
    test_list = ["stream_1", "stream_2", "stream_3", "stream_4", "stream_5", "stream_6", "stream_7", "xxx"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1d():
    init_module()
    test_list = []
    test_list.append("rd_1d_0.0_30_0.0")
    for i in range(1, 11):
        test_name = "rd_1d_" + str(float(i) / 10.0)
        for j in range(0, 11):
            test_list.append(test_name  + "_30_" + str(float(j) / 10.0))
    for test in test_list:
        module_iter_basic(test)

def test_random_2d():
    init_module()
    test_list = []
    test_list.append("rd_2d_0.0_30_0.0")
    for i in range(1, 11):
        test_name = "rd_2d_" + str(float(i) / 10.0)
        for j in range(0, 11):
            test_list.append(test_name + "_30_" + str(float(j) / 10.0))
    for test in test_list:
        module_iter_basic(test)


def test_random_3d():
    init_module()
    test_list = []
    test_list.append("rd_3d_0.0_30_0.0")
    for i in range(1, 11):
        test_name = "rd_3d_" + str(float(i) / 10.0)
        for j in range(0, 11):
            test_list.append(test_name + "_30_" + str(float(j) / 10.0))
    for test in test_list:
        module_iter_basic(test)

def test_seq():
    init_module()
    test_list =  ["stream_1", "stream_2", "stream_3", "stream_4", "stream_5", "stream_6", "stream_7", "xxx"]
    for i in range(1, 11):
        test_name = "rd_3d_" + str(float(i) / 10.0)
        for j in range(0, 11):
            test_list.append(test_name + "_30_" + str(float(j) / 10.0))
    for i in range(1, 11):
        test_name = "rd_2d_" + str(float(i) / 10.0)
        for j in range(0, 11):
            test_list.append(test_name + "_30_" + str(float(j) / 10.0))
    for i in range(1, 11):
        test_name = "rd_1d_" + str(float(i) / 10.0)
        for j in range(0, 11):
            test_list.append(test_name  + "_30_" + str(float(j) / 10.0))
    for i in range(20):
        rand = random.sample(test_list, 2)
        module_iter_basic(rand[0], rand[1])


# def test_eff():
#     init_module()
#     test_list = ["rd_2d_0.8_80_3d_0.1_200", "rd_2d_0.8_80_3d_0.3_200", "rd_2d_0.8_80_3d_0.5_200", "rd_2d_0.8_80_3d_0.8_200", "rd_2d_0.8_80_3d_1.0_200"]
#     for test in test_list:
#         module_iter_basic(test)
