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
# Test Repeat Signal Generator
##################
arr_dict1 = {'istream': [0, 1, 'S0', 2, 'S0', 3, 'S1', 'D'],
             'gold': ['R', 'R', 'S', 'R', 'S', 'R', 'S', 'D']}
arr_dict2 = {'istream': [0, 1, 2, 'S0', 0, 1, 2, 'S1', 3, 'S1', 4, 5, 'S2', 'D'],
             'gold': ['R', 'R', 'R', 'S', 'R', 'R', 'R', 'S', 'R', 'S', 'R', 'R', 'S', 'D']}
arr_dict3 = {'istream': [0, 1, 2, 'S0', 0, 1, 2, 'S1', 'S1', 4, 5, 'S2', 'D'],
             'gold': ['R', 'R', 'R', 'S', 'R', 'R', 'R', 'S', 'S', 'R', 'R', 'S', 'D']}


@pytest.mark.parametrize("arrs", [arr_dict1, arr_dict2, arr_dict3])
def test_repeat_gen_direct(n, debug_sim):
    arrs_ = [arr_dict1, arr_dict2, arr_dict3]
    arrs = arrs_[n]
    in_stream = copy.deepcopy(arrs['istream'])
    gold = copy.deepcopy(arrs['gold'])

    repsig = RepeatSigGen(debug=debug_sim)

    done = False
    time = 0
    out = []
    
    in_stream_ = convert_stream_to_onyx_interp(in_stream)
    gold_ = convert_stream_to_onyx_interp(gold)
    
    write_arr(in_stream_, name = "test_in_stream")
    write_arr(gold_, name = "test_gold")


    while not done and time < TIMEOUT:
        if len(in_stream) > 0:
            repsig.set_istream(in_stream.pop(0))
        repsig.update()
        print("Timestep", time, "\t Done:", repsig.out_done(), "\t Repeat Sig:", repsig.out_repeat())
        out.append(repsig.out_repeat())
        done = repsig.out_done()
        time += 1

    out = remove_emptystr(out)
    assert (out == gold)


@pytest.mark.parametrize("max_val", [4, 16, 32, 64])
@pytest.mark.parametrize("nd", [1, 2, 3, 4, 5])
def test_repeat_gen_random_nd(max_val_, nd_, debug_sim):
    max_val__ = [4, 16, 32, 64]

    max_val = max_val__[max_val_]
    nd = nd_

    in_stream = gen_stream(n=nd, max_val=max_val, max_nnz=max_val)

    if debug_sim:
        print("Input Stream:", in_stream)

    gold = dedup_adj(in_stream)
    gold = ['R' if isinstance(x, int) else x if x == 'D' else 'S' for x in gold]

    repsig = RepeatSigGen(debug=debug_sim)

    done = False
    time = 0
    out = []

 
    in_stream_ = convert_stream_to_onyx_interp(in_stream)
    gold_ = convert_stream_to_onyx_interp(gold)
    
    write_arr(in_stream_, name = "test_in_stream")
    write_arr(gold_, name = "test_gold")

    while not done and time < TIMEOUT:
        if len(in_stream) > 0:
            repsig.set_istream(in_stream.pop(0))
        repsig.update()
        print("Timestep", time, "\t Done:", repsig.out_done(), "\t Repeat Sig:", repsig.out_repeat())
        out.append(repsig.out_repeat())
        done = repsig.out_done()
        time += 1

    out = remove_emptystr(out)
    assert (out == gold)

if __name__ == "__main__":
    n = int(sys.argv[1])
    if n < 3:
        test_repeat_gen_direct(n, False)
    else:
        n = n-3
        test_repeat_gen_random_nd(n, int(sys.argv[2]))
