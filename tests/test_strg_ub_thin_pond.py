import pytest
import kratos as kts
import fault
import tempfile
from lake.top.lake_top import LakeTop


# CGRA Params
data_width = 16
mem_depth = 32
config_data_width = 32
config_addr_width = 8
add_clk_enable = True
add_flush = True


@pytest.mark.parametrize("num_ports", [2])
@pytest.mark.parametrize("pond_area_opt", [True])
@pytest.mark.parametrize("pond_area_opt_share", [False])  # shared not tested
@pytest.mark.parametrize("pond_area_opt_dual_config", [True, False])
def test_pond_strg_ub_thin(num_ports,
                           pond_area_opt,
                           pond_area_opt_share,
                           pond_area_opt_dual_config):

    interconnect_input_ports, interconnect_output_ports = num_ports, num_ports

    pond_dut = LakeTop(data_width=data_width,
                       mem_width=data_width,
                       mem_depth=mem_depth,
                       input_iterator_support=4,
                       output_iterator_support=4,
                       interconnect_input_ports=num_ports,
                       interconnect_output_ports=num_ports,
                       use_sim_sram=True,
                       read_delay=0,
                       rw_same_cycle=True,
                       config_width=data_width,
                       config_data_width=config_data_width,
                       config_addr_width=config_addr_width,
                       area_opt=pond_area_opt,
                       pond_area_opt_share=pond_area_opt_share,
                       pond_area_opt_dual_config=pond_area_opt_dual_config,
                       iterator_support2=2,
                       fifo_mode=False,
                       add_clk_enable=add_clk_enable,
                       add_flush=add_flush,
                       enable_ram_mode=False,
                       comply_with_17=False,
                       stencil_valid=False,
                       name="PondTop")

    pond_dut_port_remap = pond_dut.get_port_remap()['pond']

    config_data = {"ID": "_U133",
                   "config": {"in2regfile_1": {"cycle_starting_addr": [1],
                                               "cycle_stride": [1, 2],
                                               "dimensionality": 2,
                                               "extent": [2, 2],
                                               "write_data_starting_addr": [0],
                                               "write_data_stride": [1, 2]},
                              "in2regfile_0": {"cycle_starting_addr": [20],
                                               "cycle_stride": [1, 2, 4],
                                               "dimensionality": 3,
                                               "extent": [2, 2, 2],
                                               "write_data_starting_addr": [10],
                                               "write_data_stride": [1, 2, 4]},
                              "regfile2out_1": {"cycle_starting_addr": [100],
                                                "cycle_stride": [1, 2],
                                                "dimensionality": 2,
                                                "extent": [2, 2],
                                                "read_data_starting_addr": [0],
                                                "read_data_stride": [1, 2]},
                              "regfile2out_0": {"cycle_starting_addr": [120],
                                                "cycle_stride": [1, 2, 4],
                                                "dimensionality": 3,
                                                "extent": [2, 2, 2],
                                                "read_data_starting_addr": [10],
                                                "read_data_stride": [1, 2, 4]}},
                   "mode": "pond",
                   "num_inputs": 1,
                   "num_outputs": 1,
                   "width": 16}
    pond_config = pond_dut.dut.get_bitstream(config_json=config_data)

    magma_dut = kts.util.to_magma(pond_dut.dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.zero_inputs()
    tester.circuit.clk = 0
    tester.circuit.clk_en = 1
    tester.circuit.tile_en = 1
    tester.circuit.rst_n = 0
    tester.eval()
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.eval()
    tester.step(2)

    for key, value in pond_config:
        setattr(tester.circuit, key, value)
    tester.eval()
    tester.step(2)

    tester.circuit.flush = 1
    tester.eval()
    tester.step(2)
    tester.circuit.flush = 0

    data_in_pond_0 = [i for i in range(4)]
    data_in_pond_1 = [i for i in range(8)]
    valid_in = [0] * interconnect_input_ports
    for i in range(150):

        # first write
        in_data_idx = i - config_data["config"]["in2regfile_1"]["cycle_starting_addr"][0]
        if in_data_idx >= 0 and i < 5:
            setattr(tester.circuit, pond_dut_port_remap["data_in_1"], data_in_pond_0[in_data_idx])

        # second write
        in_data_idx = i - config_data["config"]["in2regfile_0"]["cycle_starting_addr"][0]
        if in_data_idx >= 0 and i < 28:
            setattr(tester.circuit, pond_dut_port_remap["data_in_0"], data_in_pond_1[in_data_idx])

        # first read
        in_data_idx = i - config_data["config"]["regfile2out_1"]["cycle_starting_addr"][0]
        if in_data_idx >= 0 and i < 104:
            getattr(tester.circuit, pond_dut_port_remap["data_out_1"]).expect(data_in_pond_0[in_data_idx])

        # second read
        in_data_idx = i - config_data["config"]["regfile2out_0"]["cycle_starting_addr"][0]
        if in_data_idx >= 0 and i < 128:
            getattr(tester.circuit, pond_dut_port_remap["data_out_0"]).expect(data_in_pond_1[in_data_idx])

        tester.eval()
        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        # tempdir = "temp_pondtop"
        # tester.compile_and_run(target="system-verilog",
        #                        simulator="xcelium",
        #                        directory=tempdir,
        #                        magma_output="verilog",
        #                        dump_waveforms=True,
        #                        flags=["-sv"])
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               magma_output="verilog",
                               flags=["-Wno-fatal"])
