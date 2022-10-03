import copy
import pytest

from sam.sim.src.crd_manager import CrdDrop
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




arrs_dict1 = {'ocrd_in': [0, 1, 'S0', 'D'],
              'icrd_in': [1, 'S0', 'S1', 'D'],
              'gold': [0, 'S0', 'D']}

arrs_dict2 = {'ocrd_in': [0, 1, 'S0', 'D'],
              'icrd_in': [1, 'S0', 1, 'S1', 'D'],
              'gold': [0, 1, 'S0', 'D']}

arrs_dict3 = {'ocrd_in': [0, 1, 2, 3, 'S0', 'D'],
              'icrd_in': [1, 'S0', 1, 'S0', 'S0', 1, 'S1', 'D'],
              'gold': [0, 1, 3, 'S0', 'D']}

arrs_dict4 = {'ocrd_in': ['S0', 'D'],
              'icrd_in': ['S1', 'D'],
              'gold': ['S0', 'D']}

arrs_dict5 = {'ocrd_in': [1, 'S0', 'D'],
              'icrd_in': [1, 2, '', '', 'S1', 'D'],
              'gold_ocrd': [1, 'S0', 'D']}


@pytest.mark.parametrize("arrs", [arrs_dict1, arrs_dict2, arrs_dict3, arrs_dict4])
def test_crd_drop_1d(n, debug_sim):
    arrs_ = [arrs_dict1, arrs_dict2, arrs_dict3, arrs_dict4]
    arrs = arrs_[n]
    icrd = copy.deepcopy(arrs['icrd_in'])
    icrd_gold = copy.deepcopy(arrs['icrd_in'])
    ocrd = copy.deepcopy(arrs['ocrd_in'])

    gold = copy.deepcopy(arrs['gold'])

    cd = CrdDrop(debug=debug_sim)

    o_crd_ = convert_stream_to_onyx_interp(ocrd)
    i_crd_ = convert_stream_to_onyx_interp(icrd)
    gold_ = convert_stream_to_onyx_interp(gold)

    write_arr(o_crd_, name = "test_ocrd")
    write_arr(i_crd_, name = "test_icrd")
    write_arr(gold_, name = "test_gold")

    print(icrd_gold)
    print(icrd)

    done = False
    time = 0
    out_outer = []
    out_inner = []
    while not done and time < TIMEOUT:
        if len(icrd) > 0:
            cd.set_inner_crd(icrd.pop(0))
        if len(ocrd) > 0:
            cd.set_outer_crd(ocrd.pop(0))
        cd.update()
        print("Timestep", time, "\t Done:", cd.out_done(), "\t Out:", cd.out_crd_outer())
        out_outer.append(cd.out_crd_outer())
        out_inner.append(cd.out_crd_inner())
        done = cd.out_done()
        time += 1

    out_outer = remove_emptystr(out_outer)
    out_inner = remove_emptystr(out_inner)
    assert (out_outer == gold)
    assert (out_inner == remove_emptystr(icrd_gold))


arrs_dict0 = {"ocrd_in": ['', 0, 1, '', '', 'S0', 'D', '', '', '', ''],
              "icrd_in": ['', '', '', 1, '', '', 'S0', '', '', 'S1', 'D'],
              "gold": [0, 'S0', 'D']}


@pytest.mark.parametrize("arrs", [arrs_dict0])
def test_crd_drop_emptystr_1d(arrs, debug_sim):
    icrd = copy.deepcopy(arrs['icrd_in'])
    icrd_gold = copy.deepcopy(arrs['icrd_in'])
    ocrd = copy.deepcopy(arrs['ocrd_in'])

    gold = copy.deepcopy(arrs['gold'])

    cd = CrdDrop(debug=debug_sim)

    done = False
    time = 0
    out_outer = []
    out_inner = []
    while not done and time < TIMEOUT:
        if len(icrd) > 0:
            cd.set_inner_crd(icrd.pop(0))
        if len(ocrd) > 0:
            cd.set_outer_crd(ocrd.pop(0))
        cd.update()
        print("Timestep", time, "\t Done:", cd.out_done(), "\t Out:", cd.out_crd_outer())
        out_outer.append(cd.out_crd_outer())
        out_inner.append(cd.out_crd_inner())
        done = cd.out_done()
        time += 1

    out_outer = remove_emptystr(out_outer)
    out_inner = remove_emptystr(out_inner)
    assert (out_outer == gold)
    assert (out_inner == remove_emptystr(icrd_gold))

if __name__ == "__main__":
    test_crd_drop_1d(int(sys.argv[1]), False)
