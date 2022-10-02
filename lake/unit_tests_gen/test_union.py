import pytest
import copy

from sam.sim.src.base import remove_emptystr
from sam.sim.src.joiner import Union2
from sam.sim.src.rd_scanner import CompressedCrdRdScan
from sam.sim.test.test import TIMEOUT

from sam.onyx.util import convert_stream_to_onyx_interp
from sam.onyx.generate_matrices import MatrixGenerator 
import sys

def write_arr(str_list, name):
    dump_dir = "temp_rit"
    full_path = dump_dir + "/" + name
    with open(full_path, "w+") as wr_file:
        for item in str_list:
            wr_file.write(f"{item}\n")

arrs_dict1 = {'crd1_in': [0, 1, 3, 5, 'S0', 'D'],
              'crd2_in': [0, 2, 3, 4, 'S0', 'D'],
              'ref1_in': [0, 1, 2, 3, 'S0', 'D'],
              'ref2_in': [0, 1, 2, 3, 'S0', 'D'],
              'crd_gold': [0, 1, 2, 3, 4, 5, 'S0', 'D'],
              'ref1_gold': [0, 1, 'N', 2, 'N', 3, 'S0', 'D'],
              'ref2_gold': [0, 'N', 1, 2, 3, 'N', 'S0', 'D']}

arrs_dict2 = {'crd1_in': [0, 1, 'S0', 2, 3, 'S0', 'S0', 4, 5, 'S1', 'D'],
              'crd2_in': [1, 2, 3, 'S0', 'S0', 0, 1, 2, 'S0', 'S1', 'D'],
              'ref1_in': [0, 1, 'S0', 2, 3, 'S0', 'S0', 4, 5, 'S1', 'D'],
              'ref2_in': [0, 1, 2, 'S0', 'S0', 2, 3, 4, 'S0', 'S1', 'D'],
              'crd_gold': [0, 1, 2, 3, 'S0', 2, 3, 'S0', 0, 1, 2, 'S0', 4, 5, 'S1', 'D'],
              'ref1_gold': [0, 1, 'N', 'N', 'S0', 2, 3, 'S0', 'N', 'N', 'N', 'S0', 4, 5, 'S1', 'D'],
              'ref2_gold': ['N', 0, 1, 2, 'S0', 'N', 'N', 'S0', 2, 3, 4, 'S0', 'N', 'N', 'S1', 'D']}


def test_union_direct_nd(num, debug_sim=False):
    if num == 1:
        arrs = arrs_dict1
    if num == 2:
        arrs = arrs_dict2
    crd1 = copy.deepcopy(arrs['crd1_in'])
    ref1 = copy.deepcopy(arrs['ref1_in'])
    crd2 = copy.deepcopy(arrs['crd2_in'])
    ref2 = copy.deepcopy(arrs['ref2_in'])

    crd_gold = copy.deepcopy(arrs['crd_gold'])
    ref1_gold = copy.deepcopy(arrs['ref1_gold'])
    ref2_gold = copy.deepcopy(arrs['ref2_gold'])

    if debug_sim:
        print("Gold Crd:", crd_gold)
        print('Gold Ref1:', ref1_gold)
        print('Gold Ref2:', ref2_gold)

    union = Union2(debug=debug_sim)

    gold_crd_ = convert_stream_to_onyx_interp(crd_gold)
    gold_ref1_ = convert_stream_to_onyx_interp(ref1_gold)
    gold_ref2_ = convert_stream_to_onyx_interp(ref2_gold)
    in_crd1_ = convert_stream_to_onyx_interp(crd1)
    in_crd2_ = convert_stream_to_onyx_interp(crd2)
    in_ref1_ = convert_stream_to_onyx_interp(ref1)
    in_ref2_ = convert_stream_to_onyx_interp(ref2)
    
    write_arr(in_crd1_, name = "test_1_crd1")
    write_arr(in_crd2_, name = "test_1_crd2")
    write_arr(in_ref1_, name = "test_1_ref1")
    write_arr(in_ref2_, name = "test_1_ref2")
    
    write_arr(gold_crd_, name = "test_1_gold_crd")
    write_arr(gold_ref1_, name = "test_1_gold_ref1")
    write_arr(gold_ref2_, name = "test_1_gold_ref2")

    done = False
    time = 0
    out_crd = []
    out_ref1 = []
    out_ref2 = []
    
    while not done and time < TIMEOUT:
        if len(crd1) > 0:
            union.set_in1(ref1.pop(0), crd1.pop(0))
        if len(crd2) > 0:
            union.set_in2(ref2.pop(0), crd2.pop(0))
        union.update()

        print("Timestep", time, "\t Crd:", union.out_crd(), "\t Ref1:", union.out_ref1(), "\t Ref2:", union.out_ref2())
        out_crd.append(union.out_crd())
        out_ref1.append(union.out_ref1())
        out_ref2.append(union.out_ref2())

        done = union.done
        time += 1

    out_crd = remove_emptystr(out_crd)
    out_ref1 = remove_emptystr(out_ref1)
    out_ref2 = remove_emptystr(out_ref2)

    assert (out_crd == crd_gold)
    assert (out_ref1 == ref1_gold)
    assert (out_ref2 == ref2_gold)


if __name__== "__main__":
    test_union_direct_nd(int(sys.argv[1]), False)
