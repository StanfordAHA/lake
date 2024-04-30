from lake.modules.stream_arbiter import *
import magma as m
from magma import *
import tempfile
import kratos as k


import sparse_helper
from sparse_helper import convert_stream_to_onyx_interp


import subprocess
import os
import random
random.seed(15)
import string
import copy


def init_module():
    dut = StreamArbiter(data_width=16,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)

    # magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    verilog(dut, filename=f"./modules/StreamArbiter.sv",
            optimize_if=False)
    sparse_helper.update_tcl("stream_arbiter_tb")

# def create_random_fiber(rate, size, d, f_type = "coord"):
#     # size = int(size*random.uniform(1.0, 1.0+d))
#     s = int(rate*size)
#     if f_type == "coord":
#         crd = random.sample(range(size), s)
#         crd.sort()
#         return crd
#     elif f_type == "pos":
#         pos = random.sample(range(1, size + 1), s)
#         pos.sort()
#         return pos
#     else:
#         val = [random.uniform(0, 2**15-1) for i in range(s)]
#         return val

# def create_random(n, rate, size, d1=0):
#     if n == 1:
#         in_crd1 = create_random_fiber(rate, size, 0.2, "coord") + ['S0', 'D']
#         in_ref1 = create_random_fiber(1, len(in_crd1) - 2, 0, "pos") + ['S0', 'D']
#         in_crd2 = create_random_fiber(rate, size, 0.2, "coord") + ['S0', 'D']
#         in_ref2 = create_random_fiber(1, len(in_crd2) - 2, 0, "pos") + ['S0', 'D']
#         return in_crd1, in_crd2, in_ref1, in_ref2
    
#     elif n == 2:
#         if d1 == 0:
#             d1 = int(random.uniform(2, int(size**(1/2))))
#         d2 = size // d1
#         rate = rate**(1/2)
#         ret = [[] for i in range(4)]
#         for i in range(d1):
#             ret_t = [[] for j in range(4)]
#             if random.random() < rate:
#                 ret_t[0] = create_random_fiber(rate, d2, 0.2, "coord")
#                 ret_t[1] = create_random_fiber(1, len(ret_t[0]), 0, "pos")
#             if random.random() < rate:
#                 ret_t[2] = create_random_fiber(rate, d2, 0.2, "coord")
#                 ret_t[3] = create_random_fiber(1, len(ret_t[2]), 0, "pos")

#             for j in range(4):
#                 if i == d1 - 1:
#                     ret[j] = ret[j] + ret_t[j] + ['S1', 'D']
#                 else:
#                     ret[j] = ret[j] + ret_t[j] + ['S0']
#         return ret[0], ret[1], ret[2], ret[3]
#     elif n == 3:
#         if d1 == 0:
#             d1 = int(random.uniform(2, int(size**(1/3))))
#         d2 = size // d1
#         d1_ = int(random.uniform(2, int(d2**(1/2))))
#         rate = rate**(1/3)
#         ret = [[] for i in range(4)]
#         # print(d1, d2, rate)
#         for i in range(d1):
#             # print("i---------: ", i)
#             ret_t = [[] for j in range(4)]
#             if random.random() < rate:
#                 ret_t[0], ret_t[1], ret_t[2], ret_t[3] = create_random(2, rate*rate, d2, d1_)
#             else:
#                 for j in range(4):
#                     ret_t[j] = ret_t[j] + ['S1', 'D']
            
#             for j in range(4):
#                 if i == d1 - 1:
#                     ret[j] = ret[j] + ret_t[j][:-2] + ['S2', 'D']
#                 else:
#                     ret[j] = ret[j] + ret_t[j][:-1]
#                 # print(j)
#                 # print(ret_t[j][:-1])
#                 # print(ret[j])
#         return ret[0], ret[1], ret[2], ret[3]


def check_streams(inputs, outputs):
    assert len(inputs) == 4, "Input streams should have 4 elements"
    total_inputs = 0
    for i in inputs:
        total_inputs += len(i)
    assert len(outputs) == total_inputs, "Output stream length should match input stream length"

    valid_inpus = [i for i in inputs if len(i) > 0]
    ids = dict()
    for index, i in enumerate(valid_inpus):
        ids[i[0][0]] = index
    
    for out_stream in outputs:
        stream_id = out_stream[0]
        found = False
        assert stream_id in ids, f"Stream ID {stream_id} not found in input streams"
        try_in_stream = valid_inpus[ids[stream_id]][0]
        assert len(out_stream) == len(try_in_stream), f"Output stream length {len(out_stream)} should match input stream length {len(try_in_stream)}"
        for i in range(1, len(out_stream)):
            assert out_stream[i] == try_in_stream[i], f"Output stream {out_stream} should match input stream {try_in_stream}"
        valid_inpus[ids[stream_id]] = valid_inpus[ids[stream_id]][1:]
        


def load_test_module(test_name):
    sample_3tr_seg = []
    sample_3tr_val = []
    sample_3tr_seg.append([1, 5, 0, 2, 5, 6, 7, 7, 1, 2, 3, 4, 5, 6, 7])
    sample_3tr_seg.append([1, 2, 0, 10, 10, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17])
    sample_3tr_seg.append([1, 3, 0, 1, 2, 2, 100, 500])
    sample_3tr_val.append([1, 7, 1, 2, 3, 4, 5, 6, 7])
    sample_3tr_val.append([1, 10, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17])
    sample_3tr_val.append([1, 2, 100, 500])

    if test_name == "1_merge_3str_seg":
        in0 = copy.deepcopy(sample_3tr_seg)

        all_stream = [in0, [], [], []]
        seg_mode = 1

        return all_stream, seg_mode
    
    elif test_name == "1_merge_3str_val":
        in0 = copy.deepcopy(sample_3tr_val)
        all_stream = [in0, [], [], []]

        seg_mode = 0
        return all_stream, seg_mode

    elif test_name == "2_merge_3str_seg":
        in0 = copy.deepcopy(sample_3tr_seg)

        in1 = copy.deepcopy(in0[:])
        for i in in1:
            i[0] = 5

        all_stream = [in0, in1, [], []]
        seg_mode = 1

        return all_stream, seg_mode

    elif test_name == "2_merge_3str_val":
        in0 = copy.deepcopy(sample_3tr_val)
        in1 = copy.deepcopy(in0[:])
        for i in in1:
            i[0] = 5

        all_stream = [in0, in1, [], []]

        seg_mode = 0
        return all_stream, seg_mode

    elif test_name == "3_merge_3str_seg":
        in0 = copy.deepcopy(sample_3tr_seg)
        in1 = copy.deepcopy(in0[:])
        for i in in1:
            i[0] = 5
        in2 = copy.deepcopy(in0[:])
        for i in in2:
            i[0] = 10
        
        all_stream = [in0, in1, in2, []]
        seg_mode = 1
        return all_stream, seg_mode

    elif test_name == "3_merge_3str_val":
        in0 = copy.deepcopy(sample_3tr_val)
        in1 = copy.deepcopy(in0[:])
        for i in in1:
            i[0] = 5
        in2 = copy.deepcopy(in0[:])
        for i in in2:
            i[0] = 10
        
        all_stream = [in0, in1, in2, []]
        seg_mode = 0
        return all_stream, seg_mode

    elif test_name == "4_merge_3str_seg":
        in0 = copy.deepcopy(sample_3tr_seg)
        in1 = copy.deepcopy(in0[:])
        for i in in1:
            i[0] = 5
        in2 = copy.deepcopy(in0[:])
        for i in in2:
            i[0] = 10
        in3 = copy.deepcopy(in0[:])
        for i in in3:
            i[0] = 99
        
        all_stream = [in0, in1, in2, in3]
        seg_mode = 1
        return all_stream, seg_mode

    elif test_name == "4_merge_3str_val":
        in0 = copy.deepcopy(sample_3tr_val)
        in1 = copy.deepcopy(in0[:])
        for i in in1:
            i[0] = 5
        in2 = copy.deepcopy(in0[:])
        for i in in2:
            i[0] = 10
        in3 = copy.deepcopy(in0[:])
        for i in in3:
            i[0] = 99
        
        all_stream = [in0, in1, in2, in3]
        seg_mode = 0
        return all_stream, seg_mode

    elif test_name == "empty_seg":
        in0 = copy.deepcopy(sample_3tr_seg)
        in1 = copy.deepcopy(in0[:])
        for i in in1:
            i[0] = 5
        in2 = copy.deepcopy(in0[:])
        for i in in2:
            i[0] = 10
        
        in1[1] = [5, 2, 0, 0, 0]
        in2[0] = [10, 5, 0, 0, 0, 0, 0, 0]
        all_stream = [in0, in1, in2, []]
        seg_mode = 1
        return all_stream, seg_mode

    elif test_name == "empty_val":
        in0 = copy.deepcopy(sample_3tr_val)
        in1 = copy.deepcopy(in0[:])
        for i in in1:
            i[0] = 5
        in2 = copy.deepcopy(in0[:])
        for i in in2:
            i[0] = 10
        
        in1[1] = [5, 0]
        all_stream = [in0, in1, in2, []]
        seg_mode = 0
        return all_stream, seg_mode

    elif test_name[0:3] == "rd_":
        t_arg = test_name.split("_")
        n = int(t_arg[1][0])
        rate = float(t_arg[2])
        size = int(t_arg[3])
        [in_crd1, in_ref1, in_crd2, in_ref2] = create_random(n, rate, size)
        return create_gold(in_crd1, in_crd2, in_ref1, in_ref2)

    else:
        in0 = [[1, 2, 0, 0, 0]]

        all_stream = [in0, [], [], []]
        seg_mode = 1

        return all_stream, seg_mode


def module_iter_basic(test_name):
    all_stream, seg_mode = load_test_module(test_name)

    for index, stream in enumerate(all_stream):
        print("stream", index)
        print(stream)
        s = []
        for s_sub in stream:
            s += s_sub
        sparse_helper.write_txt(f"stream_in_{index}.txt", s)

    sparse_helper.clear_txt("stream_out.txt")
    
    tx_size = [len(all_stream[0]), len(all_stream[1]), len(all_stream[2]), len(all_stream[3])]
    total_num = 0
    for i in tx_size:
        total_num += i
    print(tx_size, total_num)
    assert total_num > 0, "Total number of streams should be greater than 0"

    sim_result = subprocess.run(["make", "sim", "TEST_TAR=stream_arbiter_tb.sv", "TOP=stream_arbiter_tb",\
                            f"TX_NUM_0={tx_size[0]}", f"TX_NUM_1={tx_size[1]}", f"TX_NUM_2={tx_size[2]}", f"TX_NUM_3={tx_size[3]}",\
                            f"SEG_MODE={seg_mode}", "TEST_UNIT=StreamArbiter.sv"], capture_output=True, text=True)

    output = sim_result.stdout
    # print(output)
    cycle_count_line = output[output.find("cycle count:"):]
    print(cycle_count_line.splitlines()[0])

    stream_out = sparse_helper.read_glb_stream("stream_out.txt", total_num=total_num, seg_mode=seg_mode)
    print(stream_out)

    check_streams(all_stream, stream_out)
    
    print(test_name, " passed\n")


# def test_iter_basic():
#     init_module()
#     test_list = ["1_merge_3str_seg", "1_merge_3str_val", "2_merge_3str_seg", "2_merge_3str_val", "3_merge_3str_seg", "3_merge_3str_val", "4_merge_3str_seg", "4_merge_3str_val"]
#     for test in test_list:
#         module_iter_basic(test)


def test_iter_advan():
    init_module()
    test_list = ["empty_seg", "empty_val"]
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
#                  ["rd_3d_0.1_400", "rd_3d_0.3_400", "rd_3d_0.5_400", "rd_3d_0.8_400", "rd_1d_1.0_400"]
#     for i in range(10):
#         rand = random.sample(test_list, 2)
#         module_iter_basic(rand[0], rand[1])
