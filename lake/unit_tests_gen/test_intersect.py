import pytest
import copy
from sam.sim.src.base import remove_emptystr
from sam.sim.src.joiner import Intersect2

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


def test_intersect_direct_2d(debug_sim = True):

    matrix_gen = MatrixGenerator(name="B", shape=[10, 13], sparsity=0.6, format='CSF', dump_dir="/aha/lake/lake/unit_tests_gen/tmp_rit2", tensor=None)
    
    matrix_gen.dump_stream()
    matrix_gen.dump_outputs()


    gold_crd = [0, 'S0', 0, 1, 2, 'S1', 'D']
    gold_ref1 = [0, 'S0', 1, 2, 3, 'S1', 'D']
    gold_ref2 = [0, 'S0', 0, 1, 2, 'S1', 'D']
    assert (len(gold_crd) == len(gold_ref1) and len(gold_crd) == len(gold_ref2))

    in_crd1 = [0, 'S0', 0, 1, 2, 'S1', 'D']
    in_ref1 = [0, 'S0', 1, 2, 3, 'S1', 'D']
    in_crd2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']
    in_ref2 = [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'D']
    assert (len(in_crd1) == len(in_ref1))
    assert (len(in_crd2) == len(in_ref2))
    
    
    gold_crd_ = convert_stream_to_onyx_interp(gold_crd)
    gold_ref1_ = convert_stream_to_onyx_interp(gold_ref1)
    gold_ref2_ = convert_stream_to_onyx_interp(gold_ref2)
    in_crd1_ = convert_stream_to_onyx_interp(in_crd1)
    in_crd2_ = convert_stream_to_onyx_interp(in_crd2)
    in_ref1_ = convert_stream_to_onyx_interp(in_ref1)
    in_ref2_ = convert_stream_to_onyx_interp(in_ref2)


    
    write_arr(in_crd1_, name = "test_1_crd1")
    write_arr(in_crd2_, name = "test_1_crd2")
    write_arr(in_ref1_, name = "test_1_ref1")
    write_arr(in_ref2_, name = "test_1_ref2")
    
    write_arr(gold_crd_, name = "test_1_gold_crd")
    write_arr(gold_ref1_, name = "test_1_gold_ref1")
    write_arr(gold_ref2_, name = "test_1_gold_ref2")

    inter = Intersect2(debug=debug_sim)

    done = False
    time = 0
    out_crd = []
    out_ref1 = []
    out_ref2 = []
    while not done and time < TIMEOUT:
        if len(in_crd1) > 0:
            inter.set_in1(in_ref1.pop(0), in_crd1.pop(0))
        if len(in_crd2) > 0:
            inter.set_in2(in_ref2.pop(0), in_crd2.pop(0))
        inter.update()
        print("Timestep", time, "\t Crd:", inter.out_crd(), "\t Ref1:", inter.out_ref1(), "\t Ref2:", inter.out_ref2())
        out_crd.append(inter.out_crd())
        out_ref1.append(inter.out_ref1())
        out_ref2.append(inter.out_ref2())
        done = inter.done
        time += 1

    out_crd = remove_emptystr(out_crd)
    out_ref1 = remove_emptystr(out_ref1)
    out_ref2 = remove_emptystr(out_ref2)

    assert (out_crd == gold_crd)
    assert (out_ref1 == gold_ref1)
    assert (out_ref2 == gold_ref2)


def test_intersect_1d(in1, debug_sim):
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
    
    gold_crd_ = convert_stream_to_onyx_interp(gold_crd)
    gold_ref1_ = convert_stream_to_onyx_interp(gold_ref1)
    gold_ref2_ = convert_stream_to_onyx_interp(gold_ref2)
    in_crd1_ = convert_stream_to_onyx_interp(in_crd1)
    in_crd2_ = convert_stream_to_onyx_interp(in_crd2)
    in_ref1_ = convert_stream_to_onyx_interp(in_ref1)
    in_ref2_ = convert_stream_to_onyx_interp(in_ref2)

    write_arr(in_crd1_, name = "test_1_crd1")
    write_arr(in_crd2_, name = "test_1_crd2")
    write_arr(in_ref1_, name = "test_1_ref1")
    write_arr(in_ref2_, name = "test_1_ref2")
    write_arr(gold_crd_, name = "test_1_gold_crd")
    write_arr(gold_ref1_, name = "test_1_gold_ref1")
    write_arr(gold_ref2_, name = "test_1_gold_ref2")


arrs_dict1 = {'crd1_in': [0, 1, 2, 3, 'S0', 0, 1, 2, 3, 'S0', 0, 1, 2, 3, 'S1', 'D'],
              'ref1_in': [0, 1, 2, 3, 'S0', 4, 5, 6, 7, 'S0', 8, 9, 10, 11, 'S1', 'D'],
              'crd2_in': [4, 'S0', 0, 1, 'S0', 0, 'S1', 'D'],
              'ref2_in': [0, 'S0', 1, 2, 'S0', 3, 'S1', 'D'],
              'crd_gold': ['S0', 0, 1, 'S0', 0, 'S1', 'D'],
              'skip1_gold': [4, 'S0', 'S0', 'S1'],
              'skip2_gold': ['S0', 'S0', 'S1']}


@pytest.mark.parametrize("arrs", [arrs_dict1])
def test_intersect_direct_skip(arrs, debug_sim):
    in_crd1 = copy.deepcopy(arrs['crd1_in'])
    in_ref1 = copy.deepcopy(arrs['ref1_in'])
    in_crd2 = copy.deepcopy(arrs['crd2_in'])
    in_ref2 = copy.deepcopy(arrs['ref2_in'])

    crd_gold = copy.deepcopy(arrs['crd_gold'])
    # gold_ref1 = copy.deepcopy(arrs['ref1_gold'])
    # gold_ref2 = copy.deepcopy(arrs['ref2_gold'])
    gold_skip1 = copy.deepcopy(arrs['skip1_gold'])
    gold_skip2 = copy.deepcopy(arrs['skip2_gold'])

    inter = Intersect2(debug=debug_sim)

    done = False
    time = 0
    out_crd = []
    out_ref1 = []
    out_ref2 = []
    out_skip1 = []
    out_skip2 = []
    while not done and time < TIMEOUT:
        if len(in_crd1) > 0:
            inter.set_in1(in_ref1.pop(0), in_crd1.pop(0))
        if len(in_crd2) > 0:
            inter.set_in2(in_ref2.pop(0), in_crd2.pop(0))
        inter.update()
        print("Timestep", time, "\t Crd:", inter.out_crd(), "\t Ref1:", inter.out_ref1(), "\t Ref2:", inter.out_ref2())
        out_crd.append(inter.out_crd())
        out_ref1.append(inter.out_ref1())
        out_ref2.append(inter.out_ref2())
        out_skip1.append(inter.out_crd_skip1())
        out_skip2.append(inter.out_crd_skip2())
        done = inter.done
        time += 1

    out_crd = remove_emptystr(out_crd)
    # out_ref1 = remove_emptystr(out_ref1)
    # out_ref2 = remove_emptystr(out_ref2)
    out_skip1 = remove_emptystr(out_skip1)
    out_skip2 = remove_emptystr(out_skip2)

    if debug_sim:
        print("Skip1:", out_skip1)
        print("Skip2:", out_skip2)

    assert (out_crd == crd_gold)
    # assert (out_ref1 == gold_ref1)
    # assert (out_ref2 == gold_ref2)
    assert (out_skip1 == gold_skip1)
    assert (out_skip2 == gold_skip2)

if __name__== "__main__":
    if sys.argv[1] == "1D":
        test_intersect_1d(int(sys.argv[2]), False)
    if sys.argv[1] == "2D":
            test_intersect_direct_2d()
