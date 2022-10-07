import copy
import pytest

from sam.sim.src.crd_manager import CrdHold
from sam.sim.src.base import remove_emptystr
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



arrs_dict1 = {'ocrd_in': [0, 1, 2, 'S0', 'D'],
              'icrd_in': [0, 2, 'S0', 2, 'S0', 2, 'S1', 'D'],
              'gold': [0, 0, 'S0', 1, 'S0', 2, 'S1', 'D']}

arrs_dict2 = {'ocrd_in': [0, 1, 2, 5, 'S0', 'D'],
              'icrd_in': [1, 2, 5, 'S0', 2, 'S0', 2, 'S0', 2, 3, 4, 5, 'S1', 'D'],
              'gold': [0, 0, 0, 'S0', 1, 'S0', 2, 'S0', 5, 5, 5, 5, 'S1', 'D']}

arrs_dict3 = {'ocrd_in': [0, 2, 'S0', 3, 'S0', 4, 'S1', 'D'],
              'icrd_in': [0, 2, 3, 'S0', 0, 2, 3, 'S1', 0, 'S1', 2, 3, 'S2', 'D'],
              'gold': [0, 0, 0, 'S0', 2, 2, 2, 'S1', 3, 'S1', 4, 4, 'S2', 'D']}


@pytest.mark.parametrize("arrs", [arrs_dict1, arrs_dict2, arrs_dict3])
def test_crd_hold_nd(num, debug_sim, max_val=1000):
    arrs_l = [arrs_dict1, arrs_dict2, arrs_dict3]
    arrs = arrs_l[num]

    icrd = copy.deepcopy(arrs['icrd_in'])
    ocrd = copy.deepcopy(arrs['ocrd_in'])

    gold = copy.deepcopy(arrs['gold'])

    ch = CrdHold(debug=debug_sim)

    
    o_crd_ = convert_stream_to_onyx_interp(ocrd)
    i_crd_ = convert_stream_to_onyx_interp(icrd)
    gold_ = convert_stream_to_onyx_interp(gold)
    print(o_crd_, " ", ocrd)
    write_arr(o_crd_, name = "test_ocrd")
    write_arr(i_crd_, name = "test_icrd")
    write_arr(gold_, name = "test_gold")
    print(num) 
    done = False
    time = 0
    out = []
    while not done and time < TIMEOUT:
        if len(icrd) > 0:
            ch.set_inner_crd(icrd.pop(0))
        if len(ocrd) > 0:
            ch.set_outer_crd(ocrd.pop(0))
        ch.update()
        print("Timestep", time, "\t Done:", ch.out_done(), "\t Out:", ch.out_crd_outer())
        out.append(ch.out_crd_outer())
        done = ch.out_done()
        time += 1

    out = remove_emptystr(out)
    assert (out == gold)

if __name__ == "__main__":
    test_crd_hold_nd(int(sys.argv[1]), False)
