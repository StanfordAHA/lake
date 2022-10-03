import pytest
import copy
from sam.sim.src.repeater import RepeatSigGen, Repeat
from sam.sim.src.base import remove_emptystr

from sam.sim.test.test import TIMEOUT, gen_stream, dedup_adj

from sam.onyx.util import convert_stream_to_onyx_interp
from sam.onyx.generate_matrices import MatrixGenerator 
import sys

def write_arr(str_list, name):
    dump_dir = "temp_rit"
    full_path = dump_dir + "/" + name
    with open(full_path, "w+") as wr_file:
        for item in str_list:
            wr_file.write(f"{item}\n")

####################
# Test Repeater
##################
arr_dict1 = {'in_ref': [0, 1, 2, 'S0', 'D'],
             'repeat': ['R', 'R', 'S', 'R', 'S', 'R', 'S', 'D'],
             'gold': [0, 0, 'S0', 1, 'S0', 2, 'S1', 'D']}
arr_dict2 = {'in_ref': [0, 1, 'S0', 2, 'S0', 3, 'S1', 'D'],
             'repeat': ['R', 'R', 'R', 'S', 'R', 'R', 'R', 'S', 'R', 'S', 'R', 'R', 'S', 'D'],
             'gold': [0, 0, 0, 'S0', 1, 1, 1, 'S1', 2, 'S1', 3, 3, 'S2', 'D']}
arr_dict3 = {'in_ref': [0, 'S0', 1, 2, 3, 'S1', 'D'],
             'repeat': ['R', 'R', 'R', 'S', 'R', 'R', 'R', 'S', 'R', 'S', 'R', 'R', 'S', 'D'],
             'gold': [0, 0, 0, 'S1', 1, 1, 1, 'S0', 2, 'S0', 3, 3, 'S2', 'D']}
arr_dict4 = {'in_ref': [0, 'D'],
             'repeat': ['R', 'R', 'S', 'D'],
             'gold': [0, 0, 'S0', 'D']}
arr_dict5 = {'in_ref': [0, 1, 'S0', 'D'],
             'repeat': ['R', 'R', 'R', 'S'] * 2 + ['D'],
             'gold': [0, 0, 0, 'S0', 1, 1, 1, 'S1', 'D']}


@pytest.mark.parametrize("arrs", [arr_dict1, arr_dict2, arr_dict3, arr_dict4, arr_dict5])
def test_repeat_direct(n, debug_sim):
    arrs_arr = [arr_dict1, arr_dict2, arr_dict3, arr_dict4, arr_dict5]
    arrs = arrs_arr[n]
    in_ref = copy.deepcopy(arrs['in_ref'])
    in_repeat = copy.deepcopy(arrs['repeat'])
    gold = copy.deepcopy(arrs['gold'])

    rep = Repeat(debug=debug_sim)

    done = False
    time = 0
    out = []

    in_ref_ = convert_stream_to_onyx_interp(in_ref)
    in_repeat_ = convert_stream_to_onyx_interp(in_repeat)
    gold_ = convert_stream_to_onyx_interp(gold)

    write_arr(in_ref_, name = "test_in_ref")
    write_arr(in_repeat_, name = "test_in_repeat")
    write_arr(gold_, name = "test_gold")

    while not done and time < TIMEOUT:
        if len(in_ref) > 0:
            rep.set_in_ref(in_ref.pop(0))
        if len(in_repeat) > 0:
            rep.set_in_repeat(in_repeat.pop(0))
        rep.update()
        print("Timestep", time, "\t Done:", rep.out_done(), "\t Repeat Sig:", rep.out_ref())
        out.append(rep.out_ref())
        done = rep.out_done()
        time += 1

    out = remove_emptystr(out)
    assert (out == gold)

if __name__ == "__main__":
    test_repeat_direct(int(sys.argv[1]), False)
