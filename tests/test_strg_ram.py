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
@pytest.mark.parametrize("in_out_ports", [1, 2])
def test_storage_ram(mem_width,  # CGRA Params
                     in_out_ports,
                     banks=1,
                     data_width=16,
                     mem_depth=512,
                     input_iterator_support=6,  # Addr Controllers
                     output_iterator_support=6,
                     mem_input_ports=1,
                     mem_output_ports=1,
                     use_sim_sram=1,
                     read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                     rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                     agg_height=4,
                     num_tiles=1,
                     config_data_width=32,
                     config_addr_width=8,
                     fifo_mode=True):

    # TODO: This currently doesn't generate...
    if mem_width == 16 and in_out_ports == 2:
        return

    fw_int = int(mem_width / data_width)

    new_config = {'mode': 'ROM',
                  'tile_en': 1}

    sram_model = SRAMModel(data_width=data_width,
                           width_mult=fw_int,
                           depth=mem_depth,
                           num_tiles=num_tiles)

    # DUT
    lt_dut = LakeTop(data_width=data_width,
                     mem_width=mem_width,
                     mem_depth=mem_depth,
                     banks=banks,
                     input_iterator_support=input_iterator_support,
                     output_iterator_support=output_iterator_support,
                     interconnect_input_ports=in_out_ports,
                     interconnect_output_ports=in_out_ports,
                     mem_input_ports=mem_input_ports,
                     mem_output_ports=mem_output_ports,
                     use_sim_sram=use_sim_sram,
                     num_tiles=num_tiles,
                     read_delay=read_delay,
                     rw_same_cycle=rw_same_cycle,
                     agg_height=agg_height,
                     config_data_width=config_data_width,
                     config_addr_width=config_addr_width,
                     fifo_mode=fifo_mode)

    lt_dut = lt_dut.dut

    new_config = lt_dut.get_bitstream(new_config)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)
    tester.zero_inputs()
    ###
    for key, value in new_config:
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

    tester.circuit.clk_en = 1

    for i in range(2000):
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

        if in_out_ports > 1:
            tester.circuit.input_width_16_num_0 = data_in
            tester.circuit.input_width_16_num_1 = addr_in
            tester.circuit.input_width_16_num_2 = addr_in
        else:
            tester.circuit.input_width_16_num_0 = data_in
            tester.circuit.input_width_16_num_1 = addr_in
            tester.circuit.input_width_16_num_2 = addr_in

        tester.circuit.input_width_1_num_0[0] = read
        tester.circuit.input_width_1_num_1[0] = write
        model_out = sram_model.interact(wen=write, cen=(write | read), addr=addr_in, data=[data_in])

        tester.eval()

        # # Now check the outputs
        valid_line = tester.circuit.output_width_1_num_0
        if fw_int > 1:
            valid_line = tester.circuit.output_width_1_num_1

        # tester.circuit.output_width_1_num_0.expect(prev_rd)
        # tester.circuit[f"output_width_1_num_{valid_line}"].expect(prev_rd)
        valid_line.expect(prev_rd)
        if prev_rd:
            if in_out_ports > 1:
                tester.circuit.output_width_16_num_0.expect(model_out[0])
            else:
                tester.circuit.output_width_16_num_0.expect(model_out[0])

        tester.step(2)
        prev_rd = read

    with tempfile.TemporaryDirectory() as tempdir:
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])


if __name__ == "__main__":
    test_storage_ram(mem_width=64,
                     in_out_ports=1)
