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
import numpy as np


def init_module():
    dut = StreamArbiter(data_width=16,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)

    # magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    k.verilog(dut, filename=f"./modules/StreamArbiter.sv",
            optimize_if=False)
    sparse_helper.update_tcl("stream_arbiter_tb")


def create_random_stream(rate, size, dim, f_type="seg", allow_empty=False):
    assert rate >= 0 and rate <= 1, "Rate should be between 0 and 1"
    num_elem = size ** dim
    mat_1d = np.random.randint(-1024, 1024, size=num_elem)
    zero_index = random.sample(range(num_elem), int(num_elem * rate))
    for i in zero_index:
        mat_1d[i] = 0
    mat_nd = mat_1d.reshape((-1, size))
    mat_nd = mat_nd.tolist()
    # print(mat_nd)
    # print(mat_1d)

    if f_type == "seg":
        if allow_empty:
            seg = [0]
            cur_seg = 0
            crd = []
            for fiber in mat_nd:
                sub_crd = [e for e in fiber if e != 0]
                cur_seg += len(sub_crd)
                seg.append(cur_seg)
                crd += sub_crd
        else:
            seg = [0]
            for i in range(len(mat_nd)):
                seg.append((i+1)*size)
            crd = [i for i in range(len(mat_1d))]
        seg.insert(0, len(seg))
        crd.insert(0, len(crd))
        # print(seg)
        # print(crd)
        out_stream = seg + crd
    else:
        if allow_empty:
            val = mat_1d.tolist()
        else:
            val = [e for e in mat_1d if e != 0]
        val.insert(0, len(val))
        out_stream = val
    # print(out_stream)
    return out_stream


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
    sample_3tr_val.append([1, 10, 8, 9, 10, -11, 12, 13, 14, 15, 16, 17])
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
        stream_num = int(t_arg[1][0])
        assert stream_num > 0 and stream_num <= 4, "Stream number should be between 1 and 4"
        even = t_arg[2] == "even"
        allow_empty = t_arg[3] == "empty"
        f_type = t_arg[4]

        if f_type == "seg":
            seg_mode = 1
        else:
            seg_mode = 0

        all_stream = []
        ids = [1, 2, 4, 8]  # to be determined for future design

        # parameters
        trans_range = [8, 32]
        rate_range = [0.8, 1.0]
        dim = [1, 2, 3]
        size_range = [1, 5]

        stream_lengths = [0 for i in range(4)]

        st_length = random.randint(trans_range[0], trans_range[1])
        var_length = [random.randint(trans_range[0], trans_range[1]) for i in range(stream_num)]
        for i in range(stream_num):
            l = st_length
            if not even:
                l = var_length[i]
            in_temp = []
            for j in range(l):
                f = create_random_stream(random.uniform(rate_range[0], rate_range[1]), random.randint(size_range[0], size_range[1]), random.choice(dim), f_type=f_type, allow_empty=allow_empty)
                f.insert(0, ids[i])
                in_temp.append(f)
            all_stream.append(in_temp)
        
        if len(all_stream) < 4:
            for i in range(4 - len(all_stream)):
                all_stream.append([])

        return all_stream, seg_mode

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
    # print(stream_out)

    check_streams(all_stream, stream_out)
    
    print(test_name, " passed\n")


def test_iter_basic():
    init_module()
    test_list = ["1_merge_3str_seg", "1_merge_3str_val", "2_merge_3str_seg", "2_merge_3str_val", "3_merge_3str_seg", "3_merge_3str_val", "4_merge_3str_seg", "4_merge_3str_val"]
    for test in test_list:
        module_iter_basic(test)


def test_iter_advan():
    init_module()
    test_list = ["empty_seg", "empty_val"]
    for test in test_list:
        module_iter_basic(test)


def test_random_1stream():
    init_module()
    test_list = ["rd_1st_even_empty_val", "rd_1st_even_empty_seg", "rd_1st_even_noempty_val", "rd_1st_even_noempty_seg"\
                "rd_1st_uneven_empty_val", "rd_1st_uneven_empty_seg", "rd_1st_uneven_noempty_val", "rd_1st_uneven_noempty_seg"]
    for test in test_list:
        module_iter_basic(test)


def test_random_mulstream():
    init_module()
    test_list = ["rd_2st_even_empty_val", "rd_2st_even_empty_seg", "rd_2st_even_noempty_val", "rd_2st_even_noempty_seg"\
                "rd_3st_even_empty_val", "rd_3st_even_empty_seg", "rd_3st_even_noempty_val", "rd_3st_even_noempty_seg"\
                "rd_4st_even_empty_val", "rd_4st_even_empty_seg", "rd_4st_even_noempty_val", "rd_4st_even_noempty_seg"
                "rd_2st_uneven_empty_val", "rd_2st_uneven_empty_seg", "rd_2st_uneven_noempty_val", "rd_2st_uneven_noempty_seg"\
                "rd_3st_uneven_empty_val", "rd_3st_uneven_empty_seg", "rd_3st_uneven_noempty_val", "rd_3st_uneven_noempty_seg"\
                "rd_4st_uneven_empty_val", "rd_4st_uneven_empty_seg", "rd_4st_uneven_noempty_val", "rd_4st_uneven_noempty_seg"]
    for test in test_list:
        module_iter_basic(test)
