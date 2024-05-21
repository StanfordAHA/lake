from lake.modules.intersect import *
import magma as m
from magma import *
import tempfile
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.src.accumulator import SparseAccumulator1
from sam.sim.test.test import TIMEOUT


import subprocess
import os
import random
import string
from lake.top.tech_maps import GF_Tech_Map
from lake.top.fiber_access import FiberAccess
random.seed(15)


def init_module():
    unioner = Intersect(data_width=16,
                    use_merger=False,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)
    # magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    verilog(unioner, filename=f"./modules/Intersect.sv",
            optimize_if=False)
    
    data_width = 16
    mem_depth = 512
    mem_width = 64
    macro_width = 32
    dual_port = False
    pipeline_scanner = True
    fiber_access = FiberAccess(data_width=16,
                        mem_width=64,
                        tb_harness=True,
                        local_memory=False,
                        tech_map=GF_Tech_Map(depth=mem_depth, width=macro_width, dual_port=dual_port),
                        defer_fifos=False,
                        add_flush=True,
                        use_pipelined_scanner=pipeline_scanner,
                        fifo_depth=2,
                        buffet_optimize_wide=True,
                        perf_debug=False)

    k.verilog(fiber_access, filename=f"./modules/Fiber_access.sv", optimize_if=False)
    sparse_helper.update_tcl("vector_reducer_tb")

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
                

def create_gold(in_crd0, in_crd1, in_val):
    assert (len(in_crd0) == len(in_val))
    assert (len([x for x in in_crd0 if type(x) is not int]) == len([x for x in in_val if type(x) is not int]))
    
    i_c0_cpy = in_crd0[:]
    i_c1_cpy = in_crd1[:]
    i_v_cpy = in_val[:]

    done = False
    time = 0

    spacc = SparseAccumulator1(valtype=int, val_stkn=True)
    out_crd = []
    out_val = []

    #print(in_crd0)
    #print(in_crd1)
    while not done and time < TIMEOUT:
        if len(in_crd0) > 0:
            spacc.set_in_crd0(in_crd0.pop(0))
        if len(in_crd1) > 0:
             spacc.set_in_crd1(in_crd1.pop(0))
        if len(in_val) > 0:
            spacc.set_val(in_val.pop(0))

        spacc.update()

        out_crd.append(spacc.out_crd0())
        out_val.append(spacc.out_val())

        # print("Timestep", time, "\t Crd:", union.out_crd(), "\t Ref1:", union.out_ref1(), "\t Ref2:", union.out_ref2())

        done = spacc.done
        time += 1

    print("sam cycle count: ", time)
    out_crd = remove_emptystr(out_crd)
    out_val = remove_emptystr(out_val)

    print(out_crd)
    print(out_val)

    assert len(out_crd) == len(out_val) 
    st = [i_c0_cpy, i_c1_cpy, i_v_cpy, out_crd, out_val]
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
    
    elif test_name == "direct_2d_0":
        num_rows = 3
        in_crd0 = [1, 'S0', 0, 2, 'S0', 1, 3, 'S1', 'D']
        in_crd1 = [0, 1, 3, 'S0', 'D']
        in_val = [1, 'S0', 2, 3, 'S0', 4, 5, 'S1', 'D']

        return create_gold(in_crd0, in_crd1, in_val), num_rows
    
    elif test_name == "direct_2d_1":
        num_rows = 5
        in_crd0 = [1, 4, 8, 'S0', 0, 4, 'S0', 3, 4, 7, 8, 15, 'S0', 7, 15, 'S0', 15, 'S1', 'D']
        in_crd1 = [0, 1, 2, 3, 4, 'S0', 'D']
        in_val = [94, 344, 8, 'S0', 3, 77, 'S0', 93, 441, 1007, 8, 956, 'S0', 7, 6, 'S0', 25, 'S1', 'D']

        return create_gold(in_crd0, in_crd1, in_val), num_rows
    
    elif test_name == "direct_2d_2":
        num_rows = 10
        in_crd0 = [4, 'S0', 4, 'S0', 4, 'S0', 4, 'S0', 4, 'S0', 4, 'S0', 4, 'S0', 4, 'S0', 4, 'S0', 4, 'S1', 'D']
        in_crd1 = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 'S0', 'D']
        in_val = [44, 'S0', 334, 'S0', 9982, 'S0', 742, 'S0', 36, 'S0', 1, 'S0', 1, 'S0', 23, 'S0', 99, 'S0', 567, 'S1', 'D']

        return create_gold(in_crd0, in_crd1, in_val), num_rows
    
    elif test_name == "direct_3d_1":
        num_rows = 3
        in_crd0 = [1, 'S0', 0, 2, 'S0', 1, 3, 'S1', 1, 'S0', 0, 2, 'S0', 1, 3, 'S1', 1, 'S0', 0, 2, 'S0', 1, 3, 'S2', 'D']
        in_crd1 = [0, 1, 3, 'S0', 0, 1, 3, 'S0', 0, 1, 3, 'S1', 'D']
        in_val = [1, 'S0', 2, 3, 'S0', 4, 5, 'S1', 1, 'S0', 2, 3, 'S0', 4, 5, 'S1', 1, 'S0', 2, 3, 'S0', 4, 5, 'S2', 'D']

        return create_gold(in_crd0, in_crd1, in_val), num_rows
    
    elif test_name == "direct_3d_2":
        num_rows = 3
        in_crd0 = [1, 'S0', 0, 2, 'S0', 1, 3, 'S1', 0, 2, 3, 'S0', 0, 1, 'S0', 1, 2, 3, 'S0', 2, 'S1', 1, 3, 'S0', 0, 'S2', 'D']
        in_crd1 = [0, 1, 3, 'S0', 0, 1, 2, 3, 'S0', 0, 3, 'S1', 'D']
        in_val = [11, 'S0', 20, 274, 'S0', 96, 3212, 'S1', 800, 1, 1, 'S0', 95, 463, 'S0', 2, 2, 50, 'S0', 5, 'S1', 1, 1, 'S0', 1, 'S2', 'D']

        return create_gold(in_crd0, in_crd1, in_val), num_rows
    
    elif test_name == "repeated_single_element_3d":
        num_rows = 3
        in_crd0 = [1, 'S1', 2, 'S1', 0, 'S1', 1, 'S2', 'D']
        in_crd1 = [0, 'S0', 3, 'S0', 1, 'S0', 0, 'S1', 'D']
        in_val = [21, 'S1', 721, 'S1', 3, 'S1', 99, 'S2', 'D']

        return create_gold(in_crd0, in_crd1, in_val), num_rows

    elif test_name == "blank_fiber":
        num_rows = 7
        in_crd0 = ['S1', 'S1', 'S1', 'S1', 'S1', 'S1', 'S2', 'D']
        in_crd1 = ['S0', 'S0', 'S0', 'S0', 'S0', 'S0', 'S1', 'D']
        in_val = ['S1', 'S1', 'S1', 'S1', 'S1', 'S1', 'S2', 'D']

        return create_gold(in_crd0, in_crd1, in_val), num_rows

    elif test_name == "empty_2d":
        in_crd1 = ['S0', 'S0', 'S0', 'S1', 'D']
        in_ref1 = ['S0', 'S0', 'S0', 'S1', 'D']
        in_crd2 = ['S0', 'S0', 'S0', 'S1', 'D']
        in_ref2 = ['S0', 'S0', 'S0', 'S1', 'D']
        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)

    elif test_name == "simple_3d":
        in_crd1 = [0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S2', 'D']
        in_ref1 = [0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S2', 'D']
        in_crd2 = [1, 'S0', 'S1', 0, 1, 'S0', 0, 'S1', 'S0', 0, 1, 'S2', 'D']
        in_ref2 = [9, 'S0', 'S1', 8, 7, 'S0', 6, 'S1', 'S0', 4, 3, 'S2', 'D']
        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)

    elif test_name == "array_1d":
        in_crd1 = [0, 1, 3, 5, 'S0', 'D']
        in_ref1 = [0, 1, 2, 3, 'S0', 'D']
        in_crd2 = [0, 2, 3, 4, 'S0', 'D']
        in_ref2 = [0, 1, 2, 3, 'S0', 'D']
        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)

    elif test_name == "array_2d":
        in_crd1 = [0, 1, 'S0', 2, 3, 'S0', 'S0', 4, 5, 'S1', 'D']
        in_ref1 = [0, 1, 'S0', 2, 3, 'S0', 'S0', 4, 5, 'S1', 'D']
        in_crd2 = [1, 2, 3, 'S0', 'S0', 0, 1, 2, 'S0', 'S1', 'D']
        in_ref2 = [0, 1, 2, 'S0', 'S0', 2, 3, 4, 'S0', 'S1', 'D']
        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        n = int(t_arg[1][0])
        rate = float(t_arg[2])
        size = int(t_arg[3])
        [in_crd1, in_ref1, in_crd2, in_ref2] = create_random(n, rate, size)
        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)

    else:
        in_crd1 = [0, 'S0', 'D']
        in_ref1 = [0, 'S0', 'D']
        in_crd2 = [0, 'S0', 'D']
        in_ref2 = [0, 'S0', 'D']

        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)


# TODO: Modify this 
def module_iter_basic(test_name, add_test=""):
    print("THIS IS MY TEST")
    print(test_name)
    [coords_and_vals, num_rows] = load_test_module(test_name)
    ic0 = coords_and_vals[0]
    ic1 = coords_and_vals[1]
    iv = coords_and_vals[2]
    gc = coords_and_vals[3]
    gv = coords_and_vals[4]

    if add_test != "":
        additional_t = load_test_module(add_test)
        ic0 = ic0 + additional_t[0]
        ic1 = ic1 + additional_t[1]
        iv = iv + additional_t[2]
        gc = gc + additional_t[3]
        gv = gv + additional_t[3]


    sparse_helper.write_txt("crddrp_crd_in.txt", ic0)
    sparse_helper.write_txt("mult_val_in.txt", iv)

    sparse_helper.clear_txt("column_coords_out.txt")
    sparse_helper.clear_txt("vals_out.txt")
    sparse_helper.clear_txt("coords_read_scanner_pos_out.txt")
    sparse_helper.clear_txt("vals_read_scanner_pos_out.txt")
    
    #print(num_rows)
    #run command "make sim" to run the simulation
    #if add_test == "":
    sim_result = subprocess.run(["make", "sim", "TEST_TAR=vector_reducer_tb.sv", "TOP=vector_reducer_tb",\
                             "TEST_UNIT=Fiber_access.sv", "TX_NUM_GLB=1"], capture_output=True, text=True)
    #else:
    #    sim_result = subprocess.run(["make", "sim", "TEST_TAR=unioner_tb.sv",\
    #                         "TOP=unioner_tb", "TX_NUM_GLB=2", "TEST_UNIT=Intersect.sv"\
    #                         ], capture_output=True, text=True)
    output = sim_result.stdout
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    tx_num = 1
    if add_test != "":
        tx_num = 2
    coord_out = sparse_helper.read_txt("column_coords_out.txt", count=tx_num)
    val_out = sparse_helper.read_txt("vals_out.txt", count=tx_num)

    #compare each element in the output from column_coords_out.txt with the gold output
    assert len(coord_out) == len(gc), \
        f"Output coord stream length {len(coord_out)} didn't match gold length {len(gc)}"
    for i in range(len(coord_out)):
        assert coord_out[i] == gc[i], \
            f"Output coord {coord_out[i]} didn't match gold {gc[i]} at index {i}"
    
    #compare each element in the output from vals_out.txt with the gold output
    assert len(val_out) == len(gv), \
        f"Output val stream length {len(val_out)} didn't match gold length {len(gv)}"
    for i in range(len(val_out)):
        assert val_out[i] == gv[i], \
            f"Output val {val_out[i]} didn't match gold {gv[i]} at index {i}"
    
    print(test_name, " passed\n")


# def test_iter_basic():
#     init_module()
#     #test_list = ["direct_1d", "direct_2d", "xxx", "empty_2d", "array_1d", "array_2d"]
#     test_list = ["direct_1d", "xxx", "array_1d"]
#     for test in test_list:
#         module_iter_basic(test)


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

def test_direct_1D():
    init_module()
    test_list =  ["direct_2d_0", "direct_2d_1", "direct_2d_2", "direct_3d_1", "direct_3d_2", "repeated_single_element_3d", "blank_fiber"]
    for test in test_list:
        module_iter_basic(test)
    #for i in range(10):
    #    rand = random.sample(test_list, 2)
    #    module_iter_basic(rand[0], rand[1])
