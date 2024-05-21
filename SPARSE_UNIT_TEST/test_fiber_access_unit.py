from lake.top.fiber_access import FiberAccess
from lake.top.tech_maps import GF_Tech_Map
import magma as m
from magma import *
import tempfile
# from kratos import *
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.test.test import TIMEOUT
from sam.sim.src.rd_scanner import CompressedCrdRdScan
from sam.sim.src.wr_scanner import ValsWrScan, CompressWrScan


import subprocess
import os
import random
random.seed(15)
import string


def init_module():
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

    sparse_helper.update_tcl("fiber_access_tb")

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

def create_random(n, rate, size, d1=0, maybe = 0.0): #d1 is the num of the outer level
    if n == 1:
        in_crd1 = create_random_fiber(rate, size, 0.2, "coord", maybe) + ['S0', 'D']
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
                ret_t = ret_t + create_random_fiber(rate, d2, 0.2, "coord", maybe)
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


def create_ref(coord, dim, rate, size, maybe = True):
    ran = []
    k = 0
    for i in coord:
        if sparse_helper.is_STOP_sam(i):
            ran.append(k)
            k += 1
    if maybe:
        ran.append('N')
    t = create_random(dim, rate, size)
    for j in range(len(t)):
        if type(t[j]) is int:
            t[j] = ran[int(random.uniform(0, len(ran)))]
    return t


def create_gold(in_crd, in_ref):
    i_c_cpy = in_crd[:]
    i_r_cpy = in_ref[:]

    # print(in_crd)
    # print(in_ref)
    # print("============")
    # process write
    buf_size = 1000
    fill = 0
    done = False
    time = 0

    wrscan = CompressWrScan(size=buf_size, seg_size=buf_size, fill=fill)

    while not done and time < TIMEOUT:
        if len(in_crd) > 0:
            wrscan.set_input(in_crd.pop(0))

        wrscan.update()

        # print("Timestep", time, "\t WrScan:", wrscan.out_done())

        done = wrscan.out_done()
        time += 1

    print("sam write cycle count: ", time)
    # print(wrscan.get_seg_arr())
    # print(wrscan.get_arr())
    crdscan = CompressedCrdRdScan(seg_arr=wrscan.get_seg_arr(), crd_arr=wrscan.get_arr())
    done = False
    time = 0
    out_crd = []
    out_ref = []

    while not done and time < TIMEOUT:
        if len(in_ref) > 0:
            crdscan.set_in_ref(in_ref.pop(0))

        crdscan.update()

        out_crd.append(crdscan.out_crd())
        out_ref.append(crdscan.out_ref())

        # print("Timestep", time, "\t Crd:", crdscan.out_crd(), "\t Ref:", crdscan.out_ref())

        done = crdscan.done
        time += 1

    print("sam read cycle count: ", time)
    
    out_crd = remove_emptystr(out_crd)
    out_ref = remove_emptystr(out_ref)

    # print(out_crd)
    # print(out_ref)

    out_c = []
    out_r = []
    for i in range(len(out_crd)):
        if out_crd[i] != 'N':
            out_c.append(out_crd[i])
            out_r.append(out_ref[i])
    st = [i_c_cpy, i_r_cpy, out_c, out_r]
    tr_st = [convert_stream_to_onyx_interp(i) for i in st]

    return tr_st


def load_test_module(test_name):
    if test_name == "direct_2d":
        in_crd = [0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_ref = [0, 1, 'S0', 'D']
        return create_gold(in_crd, in_ref)
    
    elif test_name == "direct_1d":
        in_crd = [0, 2, 3, 7, 8, 10] + ['S0', 'D']
        in_ref = [0, 'S1', 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "in_ref_2d_1":
        in_crd = [0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_ref = [0, 'S0', 1, 'S1', 'D']
        return create_gold(in_crd, in_ref)
    
    elif test_name == "in_ref_2d_2":
        in_crd = [0, 2, 3, 7, 8, 10] + ['S0', 'D']
        in_ref = [0, 0, 'S0', 0, 'S1', 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "in_ref_empty_fiber":
        in_crd = [0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_ref = ['S0', 'S0', 1, 0, 'S0', 'S0', 1, 'S1', 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "maybe_token":
        in_crd = [0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_ref = [0, 'N', 'S0', 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "empty_root_seq_1":
        in_crd = [1,2,3,4,5, 'S0', 'D']
        in_ref = [0, 0, 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "empty_root_seq_2":
        in_crd = [6,'S0', 'D']
        in_ref = [0, 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "empty_root_seq_3":
        in_crd = [0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 14, 'S0', 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 'S0', 0, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 'S0', 0, 1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14, 'S0', 0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 'S0', 0, 1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 'S1', 'D']
        in_ref = ['S0', 5, 5, 0, 'S0', 3, 1, 'S1', 'D']
        in_c_v = [i for i in in_crd if type(i) is int]
        print("seq3", len(in_c_v))
        return create_gold(in_crd, in_ref)

    elif test_name == "empty_root_seq_4":
        in_crd = [20, 21, 22, 23, 24, 25, 26, 27, 29, 210, 211, 212, 214, 'S0', 20, 22, 23, 24, 25, 26, 27, 28, 29, 210, 212, 213, 214, 'S0', 0, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 'S0', 0, 1, 2, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14, 'S0', 0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 'S0', 0, 1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 'S1', 'D']
        in_ref = [ 0, 'S1', 'D']
        in_c_v = [i for i in in_crd if type(i) is int]
        print("seq4", len(in_c_v))
        return create_gold(in_crd, in_ref)

    elif test_name == "arr_1":
        in_crd = [0, 1, 'S0', 2, 'S0', 3, 'S1', 'D']
        in_ref = [0, 1, 'N', 2, 'N', 'S0', 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "arr_2":
        in_crd = [0, 2, 3, 'S0', 0, 'S0', 2, 3, 'S1', 'D']
        in_ref = [0, 'N', 1, 'N', 2, 'S0', 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "arr_3":
        in_crd = [0, 1, 2, 3, 'S0', 4, 'S0', 5, 6, 'S0', 7, 8, 9, 'S0', 10, 'S1', 'D'] 
        in_ref = [0, 1, 'N', 'N', 'S0', 2, 3, 'S0', 'N', 'N', 'S0', 4, 'S0', 'N', 'N', 'S1', 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "seq_1_1":
        in_crd = [0, 1, 2, 3, 4, 5, 6, 'S1', 'D']
        in_ref = ['N', 'N', 'S0', 'D']
        return create_gold(in_crd, in_ref)
    
    elif test_name == "seq_1_2":
        in_crd = [3, 4, 5, 'S0', 'D']
        in_ref = [0, 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "cluster_small":
        in_crd = [i for i in range(10)]
        in_crd.extend(['S1', 'D'])
        in_ref = [0, 'S0', 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "cluster_large":
        in_crd = [i for i in range(200)]
        in_crd.extend(['S1', 'D'])
        in_ref = [0, 'S0', 'D']
        return create_gold(in_crd, in_ref)

    elif test_name == "diag_small":
        in_crd = []
        for i in range(20):
            in_crd.append(i+20)
            in_crd.append('S0')
        in_crd[-1] = 'S1'
        in_crd.append('D')

        in_ref = [i for i in range(20)]
        in_ref.extend(['S0', 'D'])
        return create_gold(in_crd, in_ref)

    elif test_name == "diag_large":
        in_crd = []
        for i in range(200):
            in_crd.append(i)
            in_crd.append('S0')
        in_crd[-1] = 'S1'
        in_crd.append('D')

        in_ref = [i for i in range(200)]
        in_ref.extend(['S0', 'D'])
        return create_gold(in_crd, in_ref)

    elif test_name == "diag_large_random":
        in_crd = []
        for i in range(100):
            in_crd.append(i)
            in_crd.append('S0')
        in_crd[-1] = 'S1'
        in_crd.append('D')

        in_ref = [int(random.uniform(0, 100)) for i in range(100)]
        in_ref.extend(['S0', 'D'])
        return create_gold(in_crd, in_ref)

    elif test_name[0:2] == "rd":
        t_arg = test_name.split("_")
        dim1 = int(t_arg[1][0])
        rate1 = float(t_arg[2])
        size1 = int(t_arg[3]) # unlike other tests, the size here is the final size
        t_size = int(size1 / rate1)

        use_root = False
        dim2 = 0
        if t_arg[4] == 'root':
            use_root = True
        
        if not use_root:
            dim2 = int(t_arg[4][0])
        rate2 = float(t_arg[5])
        size2 = int(t_arg[6])
        coord = sparse_helper.coord_drop(create_random(dim1, rate1, t_size))

        coord_t = coord[:] #only used for seq

        ref = create_ref(coord, 1, 1.0, 2)
        if use_root:
            ref = [i for i in ref if not sparse_helper.is_STOP_sam(i)]
        else:
            ref = create_ref(coord, dim2, rate2, size2)
        [ic, ir, gc, gr] = create_gold(coord, ref)
        
        if len(t_arg[0]) == 3:
            ref = create_ref(coord_t, 1, 1.0, 2)
            if use_root:
                ref = [i for i in ref if not sparse_helper.is_STOP_sam(i)]
            else:
                ref = create_ref(coord_t, dim2, rate2, size2)
            [ic1, ir1, gc1, gr1] = create_gold(coord_t, ref)
            ic += ic1
            ir += ir1
            gc += gc1
            gr += gr1
        return [ic, ir, gc, gr]

    else:
        in_crd = [0, 'S0', 'D']
        in_ref = [0, 0, 0, 'D']
        return create_gold(in_crd, in_ref)


def module_iter_basic(test_name, add_test=""):
    [ic, ir, gc, gr] = load_test_module(test_name)

    if add_test != "" and add_test != "void":
        additional_t = load_test_module(add_test)
        ic = ic + additional_t[0]
        ir = ir + additional_t[1]
        gc = gc + additional_t[2]
        gr = gr + additional_t[3]

    print(ic)
    print(ir)
    print(gc)
    print(gr)

    sparse_helper.write_txt("coord_in_0.txt", ic)
    sparse_helper.write_txt("pos_in_0.txt", ir)

    sparse_helper.clear_txt("coord_out.txt")
    sparse_helper.clear_txt("pos_out_0.txt")

    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_access_tb.sv", "TOP=fiber_access_tb",\
                             "TEST_UNIT=Fiber_access.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_access_tb.sv",\
                             "TOP=fiber_access_tb", "TX_NUM_GLB=2", "TEST_UNIT=Fiber_access.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    #print(lines[1])

    tx_num = 1
    if add_test != "":
        tx_num = 2

    coord_out = sparse_helper.read_txt("coord_out.txt", count=tx_num)
    pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", count=tx_num)
    # print(coord_out)
    # print(pos_out_0)

    #compare each element in the output from coord_out.txt with the gold output
    assert len(coord_out) == len(gc), \
        f"Output length {len(coord_out)} didn't match gold length {len(gc)}"
    for i in range(len(coord_out)):
        assert coord_out[i] == gc[i], \
            f"Output {coord_out[i]} didn't match gold {gc[i]} at index {i}"
    
    #compare each element in the output from pos_out_0.txt with the gold output
    assert len(pos_out_0) == len(gr), \
        f"Output length {len(pos_out_0)} didn't match gold length {len(gr)}"
    for i in range(len(pos_out_0)):
        assert pos_out_0[i] == gr[i], \
            f"Output {pos_out_0[i]} didn't match gold {gr[i]} at index {i}"
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["direct_1d", "direct_2d", "in_ref_2d_1", "in_ref_2d_2", "in_ref_empty_fiber", "maybe_token", "arr_1", "arr_2", "arr_3", "xxx"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1d_1d():
    init_module()
    test_list = ["rd_1d_0.1_200_1d_1.0_3", "rd_1d_0.3_200_1d_1.0_3", "rd_1d_0.5_200_1d_1.0_3", "rd_1d_0.8_200_1d_1.0_3", "rd_1d_1.0_200_1d_1.0_3"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1d_root():
    init_module()
    test_list = ["rd_1d_0.1_200_root_1.0_3", "rd_1d_0.3_200_root_1.0_3", "rd_1d_0.5_200_root_1.0_3", "rd_1d_0.8_200_root_1.0_3", "rd_1d_1.0_200_root_1.0_3"]
    for test in test_list:
        module_iter_basic(test)


def test_random_2d_1d():
    init_module()
    test_list = ["rd_2d_0.1_100_1d_1.0_3", "rd_2d_0.3_100_1d_1.0_3", "rd_2d_0.5_100_1d_1.0_3", "rd_2d_0.8_100_1d_1.0_3", "rd_2d_1.0_100_1d_1.0_3"]
    for test in test_list:
        module_iter_basic(test)


def test_random_2d_2d():
    init_module()
    test_list = ["rd_2d_0.1_100_2d_0.3_30", "rd_2d_0.3_100_2d_0.3_30", "rd_2d_0.5_100_2d_0.3_30", "rd_2d_0.8_100_2d_0.3_30", "rd_2d_1.0_100_2d_0.3_30"]
    for test in test_list:
        module_iter_basic(test)


def test_seq1():
    init_module()
    module_iter_basic("empty_root_seq_3", "empty_root_seq_4")
    module_iter_basic("seq_1_1", "seq_1_2")


def test_seq2():
    init_module()
    test_list = ["rdS_1d_0.1_50_1d_1.0_3", "rdS_1d_0.3_50_1d_1.0_3", "rdS_1d_0.5_50_1d_1.0_3", "rdS_1d_0.8_50_1d_1.0_3", "rdS_1d_1.0_50_1d_1.0_3"] +\
                ["rdS_1d_0.1_50_root_1.0_3", "rdS_1d_0.3_50_root_1.0_3", "rdS_1d_0.5_50_root_1.0_3", "rdS_1d_0.8_50_root_1.0_3", "rdS_1d_1.0_50_root_1.0_3"] +\
                ["rdS_2d_0.1_100_1d_1.0_3", "rdS_2d_0.3_100_1d_1.0_3", "rdS_2d_0.5_100_1d_1.0_3", "rdS_2d_0.8_100_1d_1.0_3", "rdS_2d_1.0_100_1d_1.0_3"] +\
                ["rdS_2d_0.1_100_2d_0.3_30", "rdS_2d_0.3_100_2d_0.3_30", "rdS_2d_0.5_100_2d_0.3_30", "rdS_2d_0.8_100_2d_0.3_30", "rdS_2d_1.0_100_2d_0.3_30"]
    for test in test_list:
        module_iter_basic(test, add_test="void")


def test_seq3():
    init_module()
    test_list =  ["rd_1d_0.1_200_1d_1.0_3", "rd_1d_0.3_200_1d_1.0_3", "rd_1d_0.5_200_1d_1.0_3", "rd_1d_0.8_200_1d_1.0_3", "rd_1d_1.0_200_1d_1.0_3"] +\
                 ["rd_1d_0.1_200_root_1.0_3", "rd_1d_0.3_200_root_1.0_3", "rd_1d_0.5_200_root_1.0_3", "rd_1d_0.8_200_root_1.0_3", "rd_1d_1.0_200_root_1.0_3"] +\
                 ["rd_2d_0.1_100_1d_1.0_3", "rd_2d_0.3_100_1d_1.0_3", "rd_2d_0.5_100_1d_1.0_3", "rd_2d_0.8_100_1d_1.0_3", "rd_2d_1.0_100_1d_1.0_3"] +\
                 ["rd_2d_0.1_100_2d_0.3_30", "rd_2d_0.3_100_2d_0.3_30", "rd_2d_0.5_100_2d_0.3_30", "rd_2d_0.8_100_2d_0.3_30", "rd_2d_1.0_100_2d_0.3_30"] +\
                 ["direct_1d", "direct_2d", "in_ref_2d_1", "in_ref_2d_2", "in_ref_empty_fiber", "maybe_token", "arr_1", "arr_2", "arr_3", "xxx"]
    for i in range(30):
        rand = random.sample(test_list, 2)
        module_iter_basic(rand[0], rand[1])
