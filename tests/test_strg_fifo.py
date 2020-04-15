from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.models.lake_top_model import LakeTopModel
from lake.modules.strg_fifo import StrgFIFO
from lake.models.reg_fifo_model import RegFIFOModel


@pytest.mark.parametrize("mem_width", [16, 64])
@pytest.mark.parametrize("banks", [1, 2])
@pytest.mark.parametrize("depth", [16, 100])
def test_storage_fifo(mem_width,  # CGRA Params
                      banks,
                      depth,
                      data_width=16,
                      mem_depth=512,
                      input_iterator_support=6,  # Addr Controllers
                      output_iterator_support=6,
                      interconnect_input_ports=1,  # Connection to int
                      interconnect_output_ports=1,
                      mem_input_ports=1,
                      mem_output_ports=1,
                      use_sram_stub=1,
                      read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                      rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                      agg_height=4,
                      max_agg_schedule=16,
                      input_max_port_sched=16,
                      output_max_port_sched=16,
                      align_input=1,
                      max_line_length=128,
                      max_tb_height=1,
                      tb_range_max=32,
                      tb_sched_max=32,
                      max_tb_stride=15,
                      num_tb=1,
                      tb_iterator_support=2,
                      multiwrite=1,
                      max_prefetch=64,
                      config_data_width=16,
                      config_addr_width=8,
                      remove_tb=False,
                      fifo_mode=True):

    fw_int = int(mem_width / data_width)

    if banks == 1 and fw_int == 1:
        return

    new_config = {}
    new_config["fifo_ctrl_fifo_depth"] = depth
    new_config["mode"] = 1

    model_rf = RegFIFOModel(data_width=data_width,
                            width_mult=fw_int,
                            depth=depth)

    ### DUT
    lt_dut = LakeTop(data_width=data_width,
                     mem_width=mem_width,
                     mem_depth=mem_depth,
                     banks=banks,
                     input_iterator_support=input_iterator_support,
                     output_iterator_support=output_iterator_support,
                     interconnect_input_ports=interconnect_input_ports,
                     interconnect_output_ports=interconnect_output_ports,
                     mem_input_ports=mem_input_ports,
                     mem_output_ports=mem_output_ports,
                     use_sram_stub=use_sram_stub,
                     read_delay=read_delay,
                     rw_same_cycle=rw_same_cycle,
                     agg_height=agg_height,
                     max_agg_schedule=max_agg_schedule,
                     input_max_port_sched=input_max_port_sched,
                     output_max_port_sched=output_max_port_sched,
                     align_input=align_input,
                     max_line_length=max_line_length,
                     max_tb_height=max_tb_height,
                     tb_range_max=tb_range_max,
                     tb_sched_max=tb_sched_max,
                     max_tb_stride=max_tb_stride,
                     num_tb=num_tb,
                     tb_iterator_support=tb_iterator_support,
                     multiwrite=multiwrite,
                     max_prefetch=max_prefetch,
                     config_data_width=config_data_width,
                     config_addr_width=config_addr_width,
                     remove_tb=remove_tb,
                     fifo_mode=fifo_mode)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    tester.zero_inputs()
    ###
    for key, value in new_config.items():
        setattr(tester.circuit, key, value)

    rand.seed(0)
    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)

    data_in = 0
    push = 1
    pop = 0

    push_cnt = 0
    pop_cnt = 0

    tester.circuit.clk_en = 1

    for i in range(2000):
        data_in = rand.randint(0, 2 ** data_width - 1)
        push = rand.randint(0, 1)
        pop = rand.randint(0, 1)

        tester.circuit.data_in = data_in
        tester.circuit.wen = push
        tester.circuit.ren = pop
        (model_out, model_val, model_empty, model_full) = model_rf.interact(push, pop, [data_in])

        push_cnt = push_cnt + push
        pop_cnt = pop_cnt + pop

        tester.eval()

        tester.circuit.empty.expect(model_empty)
        tester.circuit.full.expect(model_full)
        # Now check the outputs
        tester.circuit.valid_out_tile.expect(model_val)
        if model_val:
            tester.circuit.data_out_tile.expect(model_out[0])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_storage_fifo(mem_width=32,
                      banks=1,
                      depth=100)
