from lake.top.fiber_access import FiberAccess
from lake.top.core_combiner import CoreCombiner
from lake.modules.strg_RAM import StrgRAM
from lake.modules.strg_ub_thin import StrgUBThin
from lake.modules.stencil_valid import StencilValid
from lake.top.tech_maps import GF_Tech_Map
import magma as m
from magma import *
import tempfile
from kratos import *


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp
from sam.sim.src.base import remove_emptystr
from sam.sim.test.test import TIMEOUT
from sam.sim.src.rd_scanner import CompressedCrdRdScan
from sam.sim.src.wr_scanner import ValsWrScan


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
                        local_memory=False,
                        tech_map=GF_Tech_Map(depth=mem_depth, width=macro_width, dual_port=dual_port),
                        defer_fifos=True,
                        add_flush=False,
                        use_pipelined_scanner=pipeline_scanner,
                        fifo_depth=2,
                        buffet_optimize_wide=True,
                        perf_debug=False)

    strg_ram = StrgRAM(data_width=16,
                        banks=1,
                        memory_width=mem_width,
                        memory_depth=mem_depth,
                        rw_same_cycle=False,
                        read_delay=1,
                        addr_width=16,
                        prioritize_write=True,
                        comply_with_17=True)
    stencil_valid = StencilValid()

    controllers = []

    # controllers.append(strg_ub)
    controllers.append(fiber_access)
    controllers.append(strg_ram)
    controllers.append(stencil_valid)

    core_comb = CoreCombiner(data_width=16,
                             mem_width=mem_width,
                             mem_depth=mem_depth,
                             banks=1,
                             add_clk_enable=True,
                             add_flush=True,
                             rw_same_cycle=False,
                             read_delay=1,
                             use_sim_sram=True,
                             controllers=controllers,
                             name=f"CoreCombiner_width_4_Smp",
                             do_config_lift=False,
                             io_prefix="MEM_",
                             fifo_depth=16)
    print(core_comb)
    core_comb_mapping = core_comb.dut.get_port_remap()
    print(core_comb_mapping)
    print(core_comb.get_modes_supported())

    # generate verilog
    verilog(core_comb.dut, filename=f"./modules/CoreCombiner.sv",
            optimize_if=False)

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
            # print("i---------: ", i)
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
                # print(j)
                # print(ret_t[j][:-1])
                # print(ret[j])
        return ret[0], ret[1], ret[2], ret[3]
                

def create_gold(in_crd1, in_crd2, in_ref1, in_ref2):
    assert (len(in_crd1) == len(in_ref1))
    assert (len(in_crd2) == len(in_ref2))
    assert (len([x for x in in_crd1 if type(x) is not int]) == len([x for x in in_crd2 if type(x) is not int]))
    assert (len([x for x in in_ref1 if type(x) is not int]) == len([x for x in in_ref2 if type(x) is not int]))
    
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

    elif test_name == "simple_3d":
        in_crd1 = [0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S2', 'D']
        in_ref1 = [0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S1', 0, 1, 'S0', 0, 1, 'S2', 'D']
        in_crd2 = [1, 'S0', 'S1', 0, 1, 'S0', 0, 'S1', 'S0', 0, 1, 'S2', 'D']
        in_ref2 = [9, 'S0', 'S1', 8, 7, 'S0', 6, 'S1', 'S0', 4, 3, 'S2', 'D']
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


def module_iter_basic(test_name, add_test=""):
    [ic1, ic2, ir1, ir2, gc, gr1, gr2] = load_test_module(test_name)
    if add_test != "":
        additional_t = load_test_module(add_test)
        ic1 = ic1 + additional_t[0]
        ic2 = ic2 + additional_t[1]
        ir1 = ir1 + additional_t[2]
        ir2 = ir2 + additional_t[3]
        gc = gc + additional_t[4]
        gr1 = gr1 + additional_t[5]
        gr2 = gr2 + additional_t[6]

    # print("ic1", ic1)
    # print("ic2", ic2)
    # print("ir1", ir1)
    # print("ir2", ir2)
    # print("gc", gc)
    # print("gr1", gr1)
    # print("gr2", gr2)

    sparse_helper.write_txt("coord_in_0.txt", ic1)
    sparse_helper.write_txt("coord_in_1.txt", ic2)
    sparse_helper.write_txt("pos_in_0.txt", ir1)
    sparse_helper.write_txt("pos_in_1.txt", ir2)

    sparse_helper.clear_txt("coord_out.txt")
    sparse_helper.clear_txt("pos_out_0.txt")
    sparse_helper.clear_txt("pos_out_1.txt") 
    
    #run command "make sim" to run the simulation
    if add_test == "":
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=intersect_tb.sv", "TOP=intersect_tb",\
                             "TEST_UNIT=intersect_unit-kratos.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=intersect_tb.sv",\
                             "TOP=intersect_tb", "TX_NUM_GLB=2", "TEST_UNIT=intersect_unit-kratos.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    coord_out = sparse_helper.read_txt("coord_out.txt", addit=add_test != "")
    pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", addit=add_test != "")
    pos_out_1 = sparse_helper.read_txt("pos_out_1.txt", addit=add_test != "")

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
#     test_list = ["direct_1d", "direct_2d", "xxx", "empty_2d"]
#     for test in test_list:
#         module_iter_basic(test)
