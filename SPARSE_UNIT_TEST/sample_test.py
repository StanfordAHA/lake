from lake.modules.intersect import *
import magma as m
from magma import *
import tempfile
import kratos as k


import enum
import subprocess
import os


class ControlCodeOnyx(enum.Enum):
    STOP = 0
    DONE = 1
    MAYBE = 2


def set_bit(old_val, bit_to_set, new_bit):
    new_val = old_val | (new_bit << bit_to_set)
    return new_val


def get_bit(val, n):
    return val >> n & 1


def convert_stream_to_onyx_interp(stream):

    ctrl_op_offset = 8
    num_ctrl_bits = 2
    top_bit = 17 - 1

    converted_stream = []
    for s_ in stream:
        if type(s_) is int:
            converted_stream.append(s_)
        elif type(s_) is str:
            control_code = 0
            if 'S' in s_:
                control_code = int(s_.lstrip('S'))
            elif 'D' in s_:
                set_ctrl = ControlCodeOnyx.DONE.value
                for offset_ in range(num_ctrl_bits):
                    bts = get_bit(set_ctrl, offset_)
                    control_code = set_bit(control_code, ctrl_op_offset + offset_, bts)
            elif 'N' in s_:
                set_ctrl = ControlCodeOnyx.MAYBE.value
                for offset_ in range(num_ctrl_bits):
                    bts = get_bit(set_ctrl, offset_)
                    control_code = set_bit(control_code, ctrl_op_offset + offset_, bts)
            else:
                raise NotImplementedError
            control_code = set_bit(control_code, top_bit, 1)
            converted_stream.append(control_code)
        else:
            raise NotImplementedError
    assert len(converted_stream) == len(stream), \
        f"Input length {len(stream)} didn't match output length {len(converted_stream)}"
    return converted_stream


def load_test_module(test_name):
    if test_name == "direct_2d":
        in_crd1 = [0, 'S0', 0, 1, 2, 'S1', 'D']
        in_ref1 = [0, 'S0', 1, 2, 3, 'S1', 'D']
        in_crd2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']
        in_ref2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']

        gold_crd = [0, 'S0', 0, 1, 2, 'S1', 'D']
        gold_ref1 = [0, 'S0', 1, 2, 3, 'S1', 'D']
        gold_ref2 = [0, 'S0', 0, 1, 2, 'S1', 'D']

        assert (len(gold_crd) == len(gold_ref1) and len(gold_crd) == len(gold_ref2))
        assert (len(in_crd1) == len(in_ref1))
        assert (len(in_crd2) == len(in_ref2))        

        return [in_crd1, in_crd2, in_ref1, in_ref2, gold_crd, gold_ref1, gold_ref2]
    
    elif test_name == "direct_1d":
        in1 = 16
        in_crd1 = [x for x in range(in1)] + ['S0', 'D']
        in_ref1 = [x for x in range(in1)] + ['S0', 'D']
        in_crd2 = [0, 2, 4, 15, 17, 25, 31, 32, 50, 63, 'S0', 'D']
        in_ref2 = [x for x in range(10)] + ['S0', 'D']
        assert (len(in_crd1) == len(in_ref1))
        assert (len(in_crd2) == len(in_ref2))

        gold_crd = [x for x in in_crd2[:-2] if x < in1] + ['S0', 'D']
        gold_ref1 = gold_crd
        gold_ref2 = [x for x in range(len(gold_crd[:-2]))] + ['S0', 'D']
        assert (len(gold_crd) == len(gold_ref1) and len(gold_crd) == len(gold_ref2))        
    
        return [in_crd1, in_crd2, in_ref1, in_ref2, gold_crd, gold_ref1, gold_ref2]

    else:
        return [[0, 'S0', 'D'], [0, 'S0', 'D'], [0, 'S0', 'D'], [0, 'S0', 'D'], [0, 'S0', 'D'],\
            [0, 'S0', 'D'], [0, 'S0', 'D']]


def module_iter_basic(test_name):
    dut = Intersect(data_width=16,
                    use_merger=False,
                    defer_fifos=False,
                    add_flush=True,
                    fifo_depth=2)
                    # data_width=16,
                    # use_merger=False,
                    # fifo_depth=2,
                    # defer_fifos=True,
                    # add_flush=False,
                    # perf_debug=perf_debug
    magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)

    [in_crd1, in_crd2, in_ref1, in_ref2, gold_crd, gold_ref1, gold_ref2] = load_test_module(test_name)

    ic1 = convert_stream_to_onyx_interp(in_crd1)
    ic2 = convert_stream_to_onyx_interp(in_crd2)
    ir1 = convert_stream_to_onyx_interp(in_ref1)
    ir2 = convert_stream_to_onyx_interp(in_ref2)

    gc = convert_stream_to_onyx_interp(gold_crd)
    gr1 = convert_stream_to_onyx_interp(gold_ref1)
    gr2 = convert_stream_to_onyx_interp(gold_ref2)

    #convert each element of ic1 to hex and write to file coord_in_0.txt
    with open("coord_in_0.txt", "w") as f:
        for element in ic1:
            # f.write("%s\n" % hex(element))
            f.write(f'{element:x}' + '\n')
        f.close()

    #convert each element of ic2 to hex and write to file coord_in_1.txt
    with open("coord_in_1.txt", "w") as f:
        for element in ic2:
            f.write(f'{element:x}' + '\n')
        f.close()
    
    #convert each element of ir1 to hex and write to file ref_in_0.txt
    with open("pos_in_0.txt", "w") as f:
        for element in ir1:
            f.write(f'{element:x}' + '\n')
        f.close()
    
    #convert each element of ir2 to hex and write to file ref_in_1.txt
    with open("pos_in_1.txt", "w") as f:
        for element in ir2:
            f.write(f'{element:x}' + '\n')
        f.close()
    
    #run command "make sim" to run the simulation
    subprocess.run(["make", "sim"])

    coord_out = []
    pos_out_0 = []
    pos_out_1 = []
    #read the output from coord_out.txt and convert each element from hex to int
    with open("coord_out.txt", "r") as f:
        for line in f:
            coord_out.append(int(line, 16))
            if int(line, 16) == 0x10100:
                break
        f.close()
    
    #read the output from pos_out_0.txt and convert each element from hex to int
    with open("pos_out_0.txt", "r") as f:
        for line in f:
            pos_out_0.append(int(line, 16))
            if int(line, 16) == 0x10100:
                break
        f.close()

    #read the output from pos_out_1.txt and convert each element from hex to int
    with open("pos_out_1.txt", "r") as f:
        for line in f:
            pos_out_1.append(int(line, 16))
            if int(line, 16) == 0x10100:
                break
        f.close()

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
    
    print(test_name, " passed")

# THIS APPROACH HAS UNKOWN BUG
# def test_iter_basic():
#     test_list = ["direct_1d", "direct_2d"]
#     for test in test_list:
#         module_iter_basic(test)


def test1():
    module_iter_basic("direct_1d")


def test2():
    module_iter_basic("direct_2d")


def test3():
    module_iter_basic("xxx")
