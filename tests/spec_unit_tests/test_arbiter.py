"""Functional tests for lake.modules.arbiter.Arbiter (memory-port arbitration
in lake specs: one arbiter per MemoryPort, one request line per Port that uses
the MemoryPort; a Port's ID/AG step and an IN Port's ready are its grant).

Function:
  * a grant is only given to a requester, and only while resource_ready;
  * at most one grant per cycle;
  * work conserving: resource_ready and any request -> exactly one grant;
  * RR: the one-hot priority line starts at requester 0 and rotates every
    cycle; its requester wins, else the lowest-numbered requester;
  * PRIO (2 inputs): requester 1 wins over requester 0.
"""
import random

import pytest

from lake.modules.arbiter import Arbiter
from rtl_harness import requires_xrun, run_vectors


def arbiter_model(ins, algo, cycles):
    """Expected grant_out per cycle for [(request_bits, resource_ready), ...]."""
    grant_line = 1
    out = []
    for req, ready in cycles:
        if ins == 1:
            out.append(req & ready)
            continue
        if algo == "PRIO":
            grant_line = 2 if (req >> 1) & 1 else 1
        prio = grant_line & req if ready else 0
        if prio:
            grant = prio
        elif ready and req:
            grant = req & -req      # lowest set request bit
        else:
            grant = 0
        out.append(grant)
        if algo == "RR":
            grant_line = ((grant_line << 1) | (grant_line >> (ins - 1))) & ((1 << ins) - 1)
    return out


def check_invariants(ins, cycles, grants):
    for (req, ready), g in zip(cycles, grants):
        assert g & ~req == 0, "grant to a non-requester"
        assert ready or g == 0, "grant while the resource is not ready"
        assert g & (g - 1) == 0, "more than one grant"
        if ready and req:
            assert g != 0, "not work conserving"


def stimulus(ins, n, seed):
    rng = random.Random(seed)
    cycles = []
    for i in range(n):
        if i < 3 * ins:     # walk single requesters first
            req = 1 << (i % ins)
        else:
            req = rng.getrandbits(ins)
        ready = 0 if rng.random() < 0.25 else 1
        cycles.append((req, ready))
    # idle cycles: requests without ready, ready without requests
    cycles += [((1 << ins) - 1, 0), (0, 1), (0, 0)]
    return cycles


@pytest.mark.parametrize("ins,algo", [(1, "RR"), (2, "RR"), (3, "RR"), (4, "RR"), (2, "PRIO")])
def test_arbiter_model_invariants(ins, algo):
    cycles = stimulus(ins, 400, seed=ins)
    check_invariants(ins, cycles, arbiter_model(ins, algo, cycles))


@requires_xrun
@pytest.mark.parametrize("ins,algo", [(1, "RR"), (2, "RR"), (3, "RR"), (4, "RR"), (2, "PRIO")])
def test_arbiter_rtl(ins, algo):
    dut = Arbiter(ins=ins, algo=algo)
    cycles = stimulus(ins, 400, seed=ins)
    grants = arbiter_model(ins, algo, cycles)
    res = run_vectors(dut,
                      [{"request_in": r, "resource_ready": rd} for r, rd in cycles],
                      [{"grant_out": g} for g in grants])
    assert res.passed, res


@requires_xrun
def test_arbiter_single_input_needs_request():
    """Regression: the 1-input arbiter granted on resource_ready alone, so a
    spec Port alone on its MemoryPort (every build_pond_rv port, garnet
    default's filter ports) stepped its iteration domain every cycle."""
    dut = Arbiter(ins=1, algo="RR")
    cycles = [(0, 1)] * 8 + [(1, 1), (0, 1), (1, 0), (1, 1)]
    res = run_vectors(dut,
                      [{"request_in": r, "resource_ready": rd} for r, rd in cycles],
                      [{"grant_out": r & rd} for r, rd in cycles])
    assert res.passed, res
