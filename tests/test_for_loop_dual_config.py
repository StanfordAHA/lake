from lake.models.for_loop_dual_config_model import ForLoopDualConfigModel
from lake.modules.for_loop import ForLoop
import magma as m
from magma import *
import fault
import tempfile
from kratos import *
import kratos as k
import pytest
import random as rand


@pytest.mark.parametrize("config_width", [16])
@pytest.mark.parametrize("iterator_support", [4])
@pytest.mark.parametrize("iterator_support2", [2])
@pytest.mark.parametrize("mux_sel_msb_init", [0, 1])
@pytest.mark.parametrize("test_cases", [1000])
@pytest.mark.parametrize("test_seed", [0, 6])
def test_for_loop_dual_config(config_width,
                              iterator_support,
                              iterator_support2,
                              mux_sel_msb_init,
                              test_cases,
                              test_seed):

    model_id = ForLoopDualConfigModel(iterator_support=iterator_support,
                                      iterator_support2=iterator_support2,
                                      config_width=config_width)

    rand.seed(test_seed)

    config_dict = {}
    config_dict["mux_sel_msb_init"] = mux_sel_msb_init
    config_dict["dimensionality"] = iterator_support
    for i in range(iterator_support):
        # avoid large ranges that only test 1 of the config
        config_dict[f"ranges_{i}"] = rand.randint(0, 2 ** 5)
        if i > 0:
            config_dict[f"ranges_{i}"] = 0

    config_dict["dimensionality2"] = iterator_support2
    for i in range(iterator_support2):
        # avoid large ranges that only test 1 of the config
        config_dict[f"ranges2_{i}"] = rand.randint(0, 2 ** 5)
        if i > 0:
            config_dict[f"ranges2_{i}"] = 0

    model_id.set_config(config_dict)

    dut = ForLoop(iterator_support=iterator_support,
                  config_width=config_width,
                  dual_config=True,
                  iterator_support2=iterator_support2)

    dut.add_attribute("sync-reset=flush")
    k.passes.auto_insert_sync_reset(dut.internal_generator)

    magma_dut = k.util.to_magma(dut, flatten_array=True,
                                check_flip_flop_always_ff=False)
    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.mux_sel_msb_init = config_dict["mux_sel_msb_init"]
    tester.circuit.dimensionality = config_dict["dimensionality"]
    tester.circuit.ranges_0 = config_dict["ranges_0"]
    if iterator_support > 1:
        tester.circuit.ranges_1 = config_dict["ranges_1"]
    if iterator_support > 2:
        tester.circuit.ranges_2 = config_dict["ranges_2"]
    if iterator_support > 3:
        tester.circuit.ranges_3 = config_dict["ranges_3"]

    tester.circuit.dimensionality2 = config_dict["dimensionality2"]
    tester.circuit.ranges2_0 = config_dict["ranges2_0"]
    if iterator_support2 > 1:
        tester.circuit.ranges2_1 = config_dict["ranges2_1"]
    if iterator_support2 > 2:
        tester.circuit.ranges2_2 = config_dict["ranges2_2"]
    if iterator_support2 > 3:
        tester.circuit.ranges2_3 = config_dict["ranges2_3"]

    tester.circuit.clk = 0
    # tester.circuit.clk_en = 1
    tester.circuit.rst_n = 0
    tester.circuit.step = 0
    tester.eval()
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.eval()
    tester.step(2)

    tester.circuit.flush = 1
    tester.eval()
    tester.step(2)
    tester.circuit.flush = 0
    tester.eval()
    tester.step(2)

    for i in range(test_cases):
        tester.circuit.step = 1
        tester.eval()
        tester.circuit.mux_sel_out.expect(model_id.get_mux_sel())
        tester.circuit.restart.expect(model_id.get_restart())
        print(model_id.get_restart())
        print(model_id.get_mux_sel())
        print(model_id.get_dim_cnt())
        model_id.step()
        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        # tester.compile_and_run(target="system-verilog",
        #                        simulator="xcelium",
        #                        directory="tempdir",
        #                        magma_output="verilog",
        #                        dump_waveforms=True,
        #                        flags=["-sv"])
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_for_loop_dual_config()
