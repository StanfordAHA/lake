"""Spec-level test: ready-valid accumulation pond (build_pond_rv as garnet
builds it, dims=4) running the vector-accumulation program.

Function: for each of P output pixels, the init port (the pond's dangling
flush port) clears the storage, the update read r0 / update write w0 perform R
read-modify-write steps on one address (the PE adds ``lb_add`` to every value
it reads), and the final read r1 returns the pixel's sum. Ordering is carried
only by the RV constraints:
  (r0,0 | w0,0, -1)  r0 reads k once w0 wrote k-1
  (r1,0 | w0,1,  0)  r1 reads pixel p once w0 moved past pixel p
  (r0,1 | w1,0,  0)  r0 starts pixel p once w1 cleared for pixel p
  (w1,0 | r1,0,  1)  w1 clears for pixel p+1 once r1 read pixel p
Also checks that clockwork's emitted pond program (its JSON format, converted
by Spec.convert_app_json_to_config) is bit-identical to get_vec_accum_pond.
"""
import copy

import pytest

from lake.spec.hack_rv_mem_pond_bitstream import get_vec_accum_pond
from lake.spec.spec_memory_controller import build_pond_rv
from rtl_harness import requires_xrun
from spec_rtl import SpecSim


def clockwork_pond_program(R, P):
    """The RV fields clockwork's add_rv_pond_info_to_json emits (matmul)."""
    upd_w, upd_r = "mul_op_upd_write", "mul_op_upd_read"
    init_w, fin_r = "mul_op_init_write", "out_op_final_read"
    dom = {upd_w: [R, P], upd_r: [R, P], init_w: [P], fin_r: [P]}
    return {
        "port_mappings": {upd_w: "pond.data_in_pond_0", init_w: "pond.data_in_pond_1",
                          upd_r: "pond.data_out_pond_0", fin_r: "pond.data_out_pond_1"},
        "domain": {p: {"dimensionality": [len(e)], "extents": e} for p, e in dom.items()},
        "access_map": {p: {"dimensionality": [len(e)], "address_offset": [0], "address_stride": [0] * len(e)}
                       for p, e in dom.items()},
        "dep_values": {f"{upd_r}___DEPTO___{upd_w}": [0, 0, -1],
                       f"{fin_r}___DEPTO___{upd_w}": [0, 1, 0],
                       f"{upd_r}___DEPTO___{init_w}": [1, 0, 0],
                       f"{init_w}___DEPTO___{fin_r}": [0, 0, 1]},
    }


def garnet_pond():
    return build_pond_rv(storage_capacity=64, data_width=16, dims=4, physical=False, reg_file=True, opt_rv=True)


@pytest.fixture(scope="module")
def pond_sim():
    return SpecSim(garnet_pond())


@pytest.mark.parametrize("R,P", [(16, 256), (8, 64), (3, 5)])
def test_clockwork_pond_program_matches_hand_written(R, P):
    spec = garnet_pond()
    spec.generate_hardware()
    bs_cw = spec.gen_bitstream(copy.deepcopy(clockwork_pond_program(R, P)))
    bs_hand = spec.gen_bitstream(get_vec_accum_pond(num_partial_reduction=R, num_output_pixels=P), over=True)
    assert bs_cw == bs_hand


def expected(R, P, add):
    r0 = [(r * add) & 0xffff for p in range(P) for r in range(R)]
    r1 = [(R * add) & 0xffff] * P
    return r0, r1


@requires_xrun
@pytest.mark.parametrize("R,P,stress,seed", [(16, 32, False, 1), (16, 32, True, 7), (8, 64, True, 3),
                                             (2, 40, True, 11), (40, 6, True, 5)])
def test_pond_accumulation_rtl(pond_sim, R, P, stress, seed):
    add = 3
    bs = pond_sim.spec.gen_bitstream(get_vec_accum_pond(num_partial_reduction=R, num_output_pixels=P), over=True)
    outs, done, cycles = pond_sim.run(bs, {"port_w0": R * P, "port_r0": R * P, "port_r1": P},
                                      loopback={"port_w0": "port_r0"}, stress=stress, seed=seed,
                                      name=f"acc_{R}_{P}_{int(stress)}_{seed}", lb_add=add)
    r0, r1 = expected(R, P, add)
    assert done, f"timeout after {cycles} cycles: r0 {len(outs['port_r0'])}/{R * P}, r1 {len(outs['port_r1'])}/{P}"
    assert outs["port_r0"] == r0
    assert outs["port_r1"] == r1
