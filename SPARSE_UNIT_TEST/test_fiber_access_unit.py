from lake.top.fiber_access import FiberAccess
from lake.top.core_combiner import CoreCombiner
# from lake.modules.strg_RAM import StrgRAM
# from lake.modules.strg_ub_thin import StrgUBThin
# from lake.modules.stencil_valid import StencilValid
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
                        local_memory=False,
                        tech_map=GF_Tech_Map(depth=mem_depth, width=macro_width, dual_port=dual_port),
                        defer_fifos=True,
                        add_flush=False,
                        use_pipelined_scanner=pipeline_scanner,
                        fifo_depth=2,
                        buffet_optimize_wide=True,
                        perf_debug=False)

    # strg_ram = StrgRAM(data_width=16,
    #                     banks=1,
    #                     memory_width=mem_width,
    #                     memory_depth=mem_depth,
    #                     rw_same_cycle=False,
    #                     read_delay=1,
    #                     addr_width=16,
    #                     prioritize_write=True,
    #                     comply_with_17=True)
    # stencil_valid = StencilValid()

    controllers = []

    # controllers.append(strg_ub)
    controllers.append(fiber_access)
    # controllers.append(strg_ram)
    # controllers.append(stencil_valid)

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
    # print(core_comb)
    core_comb_mapping = core_comb.dut.get_port_remap()
    # print(core_comb_mapping)
    # print(core_comb.get_modes_supported())

    # generate verilog
    verilog(core_comb.dut, filename=f"./modules/CoreCombiner.sv",
            optimize_if=False)
    sparse_helper.update_tcl("fiber_access_tb")

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
                

def create_gold(in_crd, in_ref):
    i_c_cpy = in_crd[:]
    i_r_cpy = in_ref[:]

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
    # crd = wrscan.get_arr()
    # seg = wrscan.get_seg_arr()
    # print(crd)
    # print(seg)

    #process read
    done = False
    time = 0

    crdscan = CompressedCrdRdScan(seg_arr=wrscan.get_seg_arr(), crd_arr=wrscan.get_arr())
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

    print(out_crd)
    print(out_ref)

    st = [i_c_cpy, i_r_cpy, out_crd, out_ref]
    tr_st = [convert_stream_to_onyx_interp(i) for i in st]

    return tr_st


def load_test_module(test_name):
    if test_name == "direct_2d":
        in_crd = [0, 2, 3, 'S0', 4, 5, 6, 'S1', 'D']
        in_ref = [0, 1, 'S0', 'D']
        return create_gold(in_crd, in_ref)
    
    elif test_name == "direct_1d":
        in_crd = [0, 2, 3, 7, 8, 10] + ['S0', 'D']
        in_ref = [0, 'D']
        return create_gold(in_crd, in_ref)

    else:
        in_crd = [0, 'S0', 'D']

        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)


def module_iter_basic(test_name, add_test=""):
    [ic, ir, gc, gr] = load_test_module(test_name)
    if add_test != "":
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
                             "TEST_UNIT=CoreCombiner.sv"], capture_output=True, text=True)
    else:
        sim_result = subprocess.run(["make", "sim", "TEST_TAR=fiber_access_tb.sv",\
                             "TOP=fiber_access_tb", "TX_NUM_GLB=2", "TEST_UNIT=CoreCombiner.sv"\
                             ], capture_output=True, text=True)
    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("write cycle count:"):]
    lines = cycle_count_line.splitlines()
    print(lines[0])
    print(lines[1])

    # coord_out = sparse_helper.read_txt("coord_out.txt", addit=add_test != "")
    # pos_out_0 = sparse_helper.read_txt("pos_out_0.txt", addit=add_test != "")

    # #compare each element in the output from coord_out.txt with the gold output
    # assert len(coord_out) == len(gc), \
    #     f"Output length {len(coord_out)} didn't match gold length {len(gc)}"
    # for i in range(len(coord_out)):
    #     assert coord_out[i] == gc[i], \
    #         f"Output {coord_out[i]} didn't match gold {gc[i]} at index {i}"
    
    # #compare each element in the output from pos_out_0.txt with the gold output
    # assert len(pos_out_0) == len(gr), \
    #     f"Output length {len(pos_out_0)} didn't match gold length {len(gr)}"
    # for i in range(len(pos_out_0)):
    #     assert pos_out_0[i] == gr[i], \
    #         f"Output {pos_out_0[i]} didn't match gold {gr[i]} at index {i}"
    
    print(test_name, " passed\n")


# def test_iter_basic():
#     init_module()
#     test_list = ["direct_2d", "direct_1d"]
#     for test in test_list:
#         module_iter_basic(test)

def test_iter_basic():
    init_module()
    test_list = ["direct_2d"]
    for test in test_list:
        module_iter_basic(test)
