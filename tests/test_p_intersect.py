from lake.modules.intersect import *
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest
# from sam.sim.src.joiner import Intersect2
# from sam.sim.src.base import remove_emptystr


import enum


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


def test_iter_basic():
    dut = Intersect(data_width=16,
                    use_merger=False,
                    defer_fifos=False,
                    add_flush=True)
    magma_dut = k.util.to_magma(dut, flatten_array=False, check_flip_flop_always_ff=True)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    # setup the streams
    in_crd1 = [0, 'S0', 0, 1, 2, 'S1', 'D']
    in_ref1 = [0, 'S0', 1, 2, 3, 'S1', 'D']
    in_crd2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']
    in_ref2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']

    gold_crd = [0, 'S0', 0, 1, 2, 'S1', 'D']
    gold_ref1 = [0, 'S0', 1, 2, 3, 'S1', 'D']
    gold_ref2 = [0, 'S0', 0, 1, 2, 'S1', 'D']

    ic1 = convert_stream_to_onyx_interp(in_crd1)
    ic2 = convert_stream_to_onyx_interp(in_crd2)
    ir1 = convert_stream_to_onyx_interp(in_ref1)
    ir2 = convert_stream_to_onyx_interp(in_ref2)

    gc = convert_stream_to_onyx_interp(gold_crd)
    gr1 = convert_stream_to_onyx_interp(gold_ref1)
    gr2 = convert_stream_to_onyx_interp(gold_ref2)

    t = [ic1, ic2, ir1, ir2]

    for st in t:
        print("=================")
        for j in st:
            print(hex(j))

    assert (len(ic1) == len(ir1))
    assert (len(ic2) == len(ir2))
    assert (len(gc) == len(gr1))
    assert (len(gc) == len(gr2))

# def test_intersect_direct_2d():
#     gold_crd = [0, 'S0', 0, 1, 2, 'S1', 'D']
#     gold_ref1 = [0, 'S0', 1, 2, 3, 'S1', 'D']
#     gold_ref2 = [0, 'S0', 0, 1, 2, 'S1', 'D']
#     assert (len(gold_crd) == len(gold_ref1) and len(gold_crd) == len(gold_ref2))

#     in_crd1 = [0, 'S0', 0, 1, 2, 'S1', 'D']
#     in_ref1 = [0, 'S0', 1, 2, 3, 'S1', 'D']
#     in_crd2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']
#     in_ref2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']
#     assert (len(in_crd1) == len(in_ref1))
#     assert (len(in_crd2) == len(in_ref2))

#     inter = Intersect2()
#     TIMEOUT = 1000
#     done = False
#     time = 0
#     out_crd = []
#     out_ref1 = []
#     out_ref2 = []
#     while not done and time < TIMEOUT:
#         if len(in_crd1) > 0:
#             inter.set_in1(in_ref1.pop(0), in_crd1.pop(0))
#         if len(in_crd2) > 0:
#             inter.set_in2(in_ref2.pop(0), in_crd2.pop(0))

#         inter.update()

#         out_crd.append(inter.out_crd())
#         out_ref1.append(inter.out_ref1())
#         out_ref2.append(inter.out_ref2())

#         print("Timestep", time, "\t Crd:", inter.out_crd(), "\t Ref1:", inter.out_ref1(), "\t Ref2:", inter.out_ref2())

#         done = inter.done
#         time += 1

#     out_crd = remove_emptystr(out_crd)
#     out_ref1 = remove_emptystr(out_ref1)
#     out_ref2 = remove_emptystr(out_ref2)

#     assert (out_crd == gold_crd)
#     assert (out_ref1 == gold_ref1)
#     assert (out_ref2 == gold_ref2)
