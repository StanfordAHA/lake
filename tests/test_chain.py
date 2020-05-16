from lake.models.chain_model import ChainModel
from lake.modules.chain import Chain
import magma as m
from magma import *
import fault
import tempfile
import kratos as k
import random as rand
import pytest


@pytest.mark.skip
def test_chain_module(data_width=16,
                      interconnect_output_ports=3,
                      chain_idx_bits=2,
                      enable_chain_output=0,
                      chain_idx_output=0):

    model_chain = ChainModel(data_width=data_width,
                             interconnect_output_ports=interconnect_output_ports,
                             chain_idx_bits=chain_idx_bits,
                             enable_chain_output=0,
                             chain_idx_output=0)

    dut = Chain(data_width=data_width,
                interconnect_output_ports=interconnect_output_ports,
                chain_idx_bits=chain_idx_bits)

    magma_dut = k.util.to_magma(dut, flatten_array=True, check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.clk = 0
    tester.circuit.clk_en = 1
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1

    # configuration registers
    new_config = {}
    new_config["enable_chain_output"] = 0
    new_config["chain_idx_output"] = 0

    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    rand.seed(0)

    num_iters = 300
    for i in range(num_iters):
        curr_tile_valid_out = []
        curr_tile_data_out = []
        chain_valid_in = []
        chain_data_in = []
        for j in range(interconnect_output_ports):
            curr_tile_valid_out.append(rand.randint(0, 1))
            curr_tile_data_out.append(rand.randint(0, 2**data_width - 1))
            chain_valid_in.append(rand.randint(0, 1))
            chain_data_in.append(rand.randint(0, 2**data_width - 1))

        for j in range(interconnect_output_ports):
            setattr(tester.circuit, f"curr_tile_valid_out_{j}", curr_tile_valid_out[j])
            setattr(tester.circuit, f"curr_tile_data_out_{j}", curr_tile_data_out[j])
            setattr(tester.circuit, f"chain_valid_in_{j}", chain_valid_in[j])
            setattr(tester.circuit, f"chain_data_in_{j}", chain_data_in[j])

        model_cdo, model_cvo, model_dot, model_vot = \
            model_chain.interact(curr_tile_valid_out, curr_tile_data_out,
                                 chain_valid_in, chain_data_in)

        tester.eval()

#        for j in range(interconnect_output_ports):
#            getattr(tester.circuit, f"chain_data_out_{j}").expect(model_cdo[j])
#            getattr(tester.circuit, f"chain_valid_out_{j}").expect(model_cvo[j])
#            getattr(tester.circuit, f"data_out_tile_{j}").expect(model_dot[j])
#            getattr(tester.circuit, f"valid_out_tile_{j}").expect(model_vot[j])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_chain_module(data_width=16,
                      interconnect_output_ports=3,
                      chain_idx_bits=2)
