"""build_spec_rv(filter_fifo_depth=N): the filter path as an N-deep FIFO.

Each FIFO side keeps its iteration domain (and the input side its item
filter); the address is a wrapping pointer (LinearAddressGenerator) and the
schedule is fixed full/empty (FIFOScheduleGenerator). The FIFO ports are not in
the RV comparison network, so the network numbers its writers/readers among the
data ports only and Spec renumbers constraints for it.

  * structure: network size, pointer width, config bits, depth checks
  * a constraint on a FIFO side is rejected
  * pass-through: N tokens in, the item filter drops the first D, N - D out in
    order (FIFO depth 2 and 8, free-flow and under stress)
  * conv_3_3 "eat rows up front" (session cfc4174c): one stream broadcast to
    the line-buffer writer and the FIFO (the FIFO's filter drops rows 0-1),
    rows k and k+1 from the SRAM, row k+2 from the FIFO, drained together like
    a PE: exercises the renumbered SRAM constraints next to the FIFO
"""
import contextlib
import io

import pytest

from lake.spec.address_generator import AddressGenerator, LinearAddressGenerator
from lake.spec.schedule_generator import ScheduleGenerator, FIFOScheduleGenerator
from lake.spec.spec_memory_controller import build_spec_rv
from rtl_harness import requires_xrun
from spec_rtl import SpecSim

FW4 = dict(storage_capacity=8192, data_width=16, vec_width=4, vec_capacity=2, in_ports=2, out_ports=2)
FW1 = dict(storage_capacity=2048, data_width=16, vec_width=1, in_ports=1, out_ports=1)


def built(depth, **kw):
    with contextlib.redirect_stdout(io.StringIO()):
        spec = build_spec_rv(physical=False, filter_fifo_depth=depth, **kw)
        spec.generate_hardware()
    return spec


def two_d(off, W):
    return {"address_offset": [off], "address_stride": [1, W], "dimensionality": [2]}


def dom(w, h):
    return {"dimensionality": [2], "extents": [w, h]}


def eat_rows_program(W, H, n_in=2, n_out=2, fifo_constraints=False):
    """conv_3_3's line buffer with the newest tap through the filter path, as a
    clockwork-style RV program (app port names -> data_in_/data_out_)."""
    rows = H - 2
    prog = {
        "port_mappings": {"W": "data_in_0", "R2": "data_out_0", "R1": "data_out_1",
                          "FI": f"data_in_{n_in}", "FO": f"data_out_{n_out}"},
        "dep_values": {},
        "domain": {"W": dom(W, H), "R2": dom(W, rows), "R1": dom(W, rows), "FI": dom(W, rows), "FO": dom(W, rows)},
        "access_map": {"W": two_d(0, W), "R2": two_d(0, W), "R1": two_d(W, W), "FI": two_d(0, W), "FO": two_d(0, W)},
        # the FIFO's input keeps tokens 2W + c + W*k: drops rows 0 and 1
        "filter": {"FI": {"offset": [2 * W], "stride": [1, W]}},
        "lake_constraints": [
            ["port_r0", 1, "port_w0", 1, "LT", 0],    # R2 reads row k: row k written
            ["port_r1", 1, "port_w0", 1, "LT", 1],    # R1 reads row k + 1: row k + 1 written
            ["port_w0", 1, "port_r0", 1, "GT", 8],
            ["port_w0", 1, "port_r1", 1, "GT", 8],
        ],
    }
    if fifo_constraints:
        prog["lake_constraints"].append([f"port_r{n_out}", 0, f"port_w{n_in}", 0, "LT", 6])
    return prog


def passthrough_program(N, D, n_in=1, n_out=1):
    """N tokens into the FIFO; its input filter drops the first D."""
    line = {"address_offset": [0], "address_stride": [1], "dimensionality": [1]}
    one_d = {"dimensionality": [1], "extents": [N - D]}
    return {
        "port_mappings": {"FI": f"data_in_{n_in}", "FO": f"data_out_{n_out}"},
        "dep_values": {},
        "domain": {"FI": one_d, "FO": one_d},
        "access_map": {"FI": line, "FO": line},
        "filter": {"FI": {"offset": [D], "stride": [1]}},
    }


def test_filter_fifo_structure():
    general = built(None, **FW4)
    fifo = built(8, **FW4)
    net = fifo.rv_comparison_network
    assert (len(net.writes), len(net.reads)) == (2, 2)
    assert len(net.lfcs) == 2 * 2 * 2
    assert (len(general.rv_comparison_network.writes), len(general.rv_comparison_network.lfcs)) == (3, 2 * 2 * 2 + 2)
    fi, fo = fifo.get_in_ports()[-1], fifo.get_out_ports()[-1]
    for p in (fi, fo):
        ag = fifo.get_associated_controller(AddressGenerator, p)
        sg = fifo.get_associated_controller(ScheduleGenerator, p)
        assert isinstance(ag, LinearAddressGenerator) and ag.get_address_width() == 3
        assert ag.get_pointer().width == 4 and ag.get_config_size() == 0
        assert isinstance(sg, FIFOScheduleGenerator) and sg.get_config_size() == 1
    assert fifo._fifo_pairs() == [(fi, fo)]
    assert fifo.get_total_config_size() < general.get_total_config_size()
    with pytest.raises(ValueError):
        build_spec_rv(physical=False, filter_fifo_depth=6, **FW4)


def test_filter_fifo_constraint_rejected():
    fifo = built(8, **FW4)
    with contextlib.redirect_stdout(io.StringIO()):
        fifo.gen_bitstream(eat_rows_program(16, 8))
        with pytest.raises(ValueError, match="FIFO side"):
            fifo.gen_bitstream(eat_rows_program(16, 8, fifo_constraints=True))


def test_filter_fifo_network_renumbering():
    """The SRAM constraints land on the same comparators with or without the
    filter ports in the network: same network bits, other offsets."""
    general, fifo = built(None, **FW4), built(8, **FW4)
    with contextlib.redirect_stdout(io.StringIO()):
        cfg_g = general.convert_app_json_to_config(general.rewrite_app_json(eat_rows_program(16, 8)))
        cfg_f = fifo.convert_app_json_to_config(fifo.rewrite_app_json(eat_rows_program(16, 8)))
    assert cfg_g["constraints"] == cfg_f["constraints"]
    # port numbers: w0, w1, w2 | r0 = 3, r1 = 4; network: r0 = 2 / 3 without / with the filter writer
    assert [c[0] for c in fifo._network_constraints(cfg_f["constraints"])] == [2, 3, 0, 0]
    assert [c[0] for c in general._network_constraints(cfg_g["constraints"])] == [3, 4, 0, 0]


_SIMS = {}


def sim_for(key, depth, kw, **simkw):
    if key not in _SIMS:
        _SIMS[key] = SpecSim(build_spec_rv(physical=False, filter_fifo_depth=depth, **kw), **simkw)
    return _SIMS[key]


@requires_xrun
@pytest.mark.parametrize("stress", [False, True])
@pytest.mark.parametrize("depth", [2, 8])
def test_filter_fifo_passthrough(depth, stress):
    N, D = 700, 37
    sim = sim_for(f"pt{depth}", depth, FW1)
    spec = sim.spec
    with contextlib.redirect_stdout(io.StringIO()):
        bs = spec.gen_bitstream(passthrough_program(N, D))
    outs, done, cyc = sim.run(bs, {"port_w1": N, "port_r1": N - D}, stress=stress, seed=3,
                              name=f"pt{depth}_{int(stress)}")
    assert done, f"timeout after {cyc} cycles"
    assert outs["port_r1"] == list(range(D, N))


@requires_xrun
@pytest.mark.parametrize("stress", [False, True])
def test_filter_fifo_eat_rows(stress):
    W, H = 32, 24
    sim = sim_for("eat8", 8, FW4, bcast=[0, 2], lockstep=[0, 1, 2])
    with contextlib.redirect_stdout(io.StringIO()):
        bs = sim.spec.gen_bitstream(eat_rows_program(W, H))
    n = W * (H - 2)
    outs, done, cyc = sim.run(bs, {"port_w0": W * H, "port_w2": W * H, "port_r0": n, "port_r1": n, "port_r2": n},
                              stress=stress, seed=2, name=f"eat_{int(stress)}")
    assert done, f"timeout after {cyc} cycles"
    assert outs["port_r0"] == list(range(n))
    assert outs["port_r1"] == list(range(W, W + n))
    assert outs["port_r2"] == list(range(2 * W, 2 * W + n))
