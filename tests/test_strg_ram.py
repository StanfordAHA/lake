from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import random as rand
import pytest
import tempfile
from lake.models.lake_top_model import LakeTopModel
from lake.modules.strg_fifo import StrgFIFO
from lake.models.reg_fifo_model import RegFIFOModel
from lake.models.sram_model import SRAMModel


@pytest.mark.parametrize("mem_width", [16, 64])
@pytest.mark.parametrize("banks", [1, 2])
def test_storage_ram(mem_width,  # CGRA Params
                     banks,
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

    new_config = {}
    new_config["mode"] = 2

    sram_model = SRAMModel(data_width=data_width,
                           width_mult=fw_int,
                           depth=mem_depth)

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
    addr_in = 0

    write = 0
    read = 0

    prev_wr = 0
    prev_rd = 0

    stall = fw_int > 1

    for i in range(5000):
        data_in = rand.randint(0, 2 ** data_width - 1)
        write = rand.randint(0, 1)
        read = rand.randint(0, 1)
        addr_in = rand.randint(0, 64)

        if prev_wr == 1 and stall:
            write = 0
            read = 0
            prev_wr = 0

        if write:
            prev_wr = 1
            read = 0

        tester.circuit.data_in = data_in
        tester.circuit.wen = write
        tester.circuit.ren = read
        tester.circuit.addr_in = addr_in
        model_out = sram_model.interact(wen=write, cen=(write | read), addr=addr_in, data=[data_in])

        tester.eval()

        # tester.circuit.empty.expect(model_empty)
        # tester.circuit.full.expect(model_full)
        # # Now check the outputs
        tester.circuit.valid_out.expect(prev_rd)
        if prev_rd:
            tester.circuit.data_out.expect(model_out[0])

        tester.step(2)
        prev_rd = read

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_storage_ram(mem_width=16,
                     banks=2)
