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
            vd.set_val(in_val.pop(0))
        if len(in_crd) > 0:
            vd.set_crd(in_crd.pop(0))

        vd.update()

        out_crd.append(vd.out_crd())
        out_val.append(vd.out_val())

        # print("Timestep", time, "\t Done:", vd.out_done(), "\t Out:", vd.out_crd(), vd.out_val())

        done = vd.out_done()
        time += 1

    out_crd = remove_emptystr(out_crd)
    out_val = remove_emptystr(out_val)

    print("sam cycle count: ", time)

    st = [in_crd_cpy, in_val_cpy, out_crd, out_val]
    tr_st = []
    for s in st:
        tr_st.append(convert_stream_to_onyx_interp(s))

    return tr_st


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
        in_crd_o = remove_emptyfiber(in_crd_o)
        in_crd_i = create_random(dim3, rate2, size2, d1=fiber_num)

        return create_gold(in_crd_o, in_crd_i)

    else:
        in_crd = [1, 'S0', 'D']
        in_val = [0, 'S0', 'D']

        return create_gold(in_crd, in_val)


def module_iter_basic(test_name, add_test=""):
    [ic, iv, gc, gv] = load_test_module(test_name)
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


def test_iter_basic():
    init_module()
    test_list = ["stream_1", "stream_2", "stream_3", "stream_4", "stream_5", "stream_6", "stream_7", "xxx"]
    for test in test_list:
        module_iter_basic(test)


# def test_random_1d_2d():
#     init_module()
#     test_list = ["rd_1d_0.3_30_2d_0.1_80", "rd_1d_0.3_30_2d_0.3_80", "rd_1d_0.3_30_2d_0.5_80", "rd_1d_0.3_30_2d_0.8_80", "rd_1d_0.3_30_2d_1.0_80"]
#     for test in test_list:
#         module_iter_basic(test)


# def test_random_1d_3d():
#     init_module()
#     test_list = ["rd_1d_0.3_30_3d_0.1_80", "rd_1d_0.3_30_3d_0.3_80", "rd_1d_0.3_30_3d_0.5_80", "rd_1d_0.3_30_3d_0.8_80", "rd_1d_0.3_30_3d_1.0_80"]
#     for test in test_list:
#         module_iter_basic(test)


# def test_random_2d_2d():
#     init_module()
#     test_list = ["rd_2d_0.3_30_2d_0.1_80", "rd_2d_0.3_30_2d_0.3_80", "rd_2d_0.3_30_2d_0.5_80", "rd_2d_0.3_30_2d_0.8_80", "rd_2d_0.3_30_2d_1.0_80"]
#     for test in test_list:
#         module_iter_basic(test)


# def test_random_2d_3d():
#     init_module()
#     test_list = ["rd_2d_0.3_30_3d_0.1_80", "rd_2d_0.3_30_3d_0.3_80", "rd_2d_0.3_30_3d_0.5_80", "rd_2d_0.3_30_3d_0.8_80", "rd_2d_0.3_30_3d_1.0_80"]
#     for test in test_list:
#         module_iter_basic(test)


# def test_seq():
#     init_module()
#     test_list =  ["stream_1", "stream_2", "stream_3", "stream_4", "stream_5", "stream_6", "stream_7", "xxx"] +\
#                  ["rd_1d_0.3_30_2d_0.1_80", "rd_1d_0.3_30_2d_0.3_80", "rd_1d_0.3_30_2d_0.5_80", "rd_1d_0.3_30_2d_0.8_80", "rd_1d_0.3_30_2d_1.0_80"] +\
#                  ["rd_1d_0.3_30_3d_0.1_80", "rd_1d_0.3_30_3d_0.3_80", "rd_1d_0.3_30_3d_0.5_80", "rd_1d_0.3_30_3d_0.8_80", "rd_1d_0.3_30_3d_1.0_80"] +\
#                  ["rd_2d_0.3_30_2d_0.1_80", "rd_2d_0.3_30_2d_0.3_80", "rd_2d_0.3_30_2d_0.5_80", "rd_2d_0.3_30_2d_0.8_80", "rd_2d_0.3_30_2d_1.0_80"] +\
#                  ["rd_2d_0.3_30_3d_0.1_80", "rd_2d_0.3_30_3d_0.3_80", "rd_2d_0.3_30_3d_0.5_80", "rd_2d_0.3_30_3d_0.8_80", "rd_2d_0.3_30_3d_1.0_80"]
#     for i in range(20):
#         rand = random.sample(test_list, 2)
#         module_iter_basic(rand[0], rand[1])


# def test_eff():
#     init_module()
#     test_list = ["rd_2d_0.8_80_3d_0.1_200", "rd_2d_0.8_80_3d_0.3_200", "rd_2d_0.8_80_3d_0.5_200", "rd_2d_0.8_80_3d_0.8_200", "rd_2d_0.8_80_3d_1.0_200"]
#     for test in test_list:
#         module_iter_basic(test)
