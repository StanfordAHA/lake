from lake.top.lake_top import LakeTop
import kratos as kts
import fault
import pytest
import tempfile
import collections

from lake.passes.passes import lift_config_reg, change_sram_port_names
from lake.utils.sram_macro import SRAMMacroInfo
from lake.utils.parse_clkwork_csv import generate_data_lists

ControllerInfo = collections.namedtuple('ControllerInfo',
                                        'dim extent cyc_stride in_data_stride cyc_strt in_data_strt out_data_stride out_data_strt mux_data_stride mux_data_strt')

def transform_strides_and_ranges(ranges, strides, dimensionality):
    assert len(ranges) == len(strides), "Strides and ranges should be same length..."
    tform_ranges = [range_item - 2 for range_item in ranges[0:dimensionality]]
    range_sub_1 = [range_item - 1 for range_item in ranges]
    tform_strides = [strides[0]]
    offset = 0
    for i in range(dimensionality - 1):
        offset -= (range_sub_1[i] * strides[i])
        tform_strides.append(strides[i + 1] + offset)
    for j in range(len(ranges) - dimensionality):
        tform_strides.append(0)
        tform_ranges.append(0)
    return (tform_ranges, tform_strides)

def search_for_config(cfg_file, key):
    lines = cfg_file
    matches = [l for l in lines if key in l]
    # to account for multiple aggs, tbs, take lowest starting addr
    # for blocks of memories
    if len(matches) > 1:
        addrs = []
        for i in range(len(matches)):
            addrs.append(matches[i].split(',')[1])
        matches[0] = matches[addrs.index(min(addrs))]
    # return nothing if no matches (in / out do not always exist)
    if len(matches) > 0:
        return int(matches[0].split(',')[1])

def extract_controller(file_path):
    file_lines = None
    with open(file_path) as ctrl_f:
        file_lines = ctrl_f.readlines()
    
    dim = search_for_config(file_lines, 'dimensionality')
    cyc_strt = search_for_config(file_lines, 'cycle_starting_addr')
    in_data_strt = search_for_config(file_lines, 'write_data_starting_addr')
    out_data_strt = search_for_config(file_lines, 'read_data_starting_addr')
    mux_data_strt = search_for_config(file_lines, 'mux_write_data_starting_addr')

    ranges = []
    cyc_strides = []
    in_data_strides = []
    out_data_strides = []
    mux_data_strides = []

    for i in range(dim):
        ranges.append(search_for_config(file_lines, f"extent_{i}"))
        cyc_strides.append(search_for_config(file_lines, f"cycle_stride_{i}"))
        in_data_strides.append(search_for_config(file_lines, f"write_data_stride_{i}"))
        out_data_strides.append(search_for_config(file_lines, f"read_data_stride_{i}"))
        mux_data_strides.append(search_for_config(file_lines, f"mux_write_data_stride_{i}"))

    ctrl_info = ControllerInfo(dim=dim,
                               cyc_strt=cyc_strt,
                               in_data_strt=in_data_strt,
                               extent=ranges,
                               cyc_stride=cyc_strides,
                               in_data_stride=in_data_strides,
                               out_data_strt=out_data_strt,
                               out_data_stride=out_data_strides,
                               mux_data_stride=mux_data_strides,
                               mux_data_strt=mux_data_strt)
    return ctrl_info

def map_controller(controller, name):
    ctrl_dim = controller.dim
    ctrl_ranges = controller.extent
    ctrl_cyc_strides = controller.cyc_stride
    ctrl_in_data_strides = controller.in_data_stride
    ctrl_cyc_strt = controller.cyc_strt
    ctrl_in_data_strt = controller.in_data_strt
    ctrl_out_data_strides = controller.out_data_stride
    ctrl_out_data_strt = controller.out_data_strt
    ctrl_mux_data_strides = controller.mux_data_stride
    ctrl_mux_data_strt = controller.mux_data_strt

    print(f"extracted controller for: {name}")
    print(f"dim: {ctrl_dim}")
    print(f"range: {ctrl_ranges}")
    print(f"sched stride: {ctrl_cyc_strides}")
    print(f"in data stride: {ctrl_in_data_strides}")
    print(f"sched start: {ctrl_cyc_strt}")
    print(f"in data start: {ctrl_in_data_strt}")
    print(f"out data stride: {ctrl_out_data_strides}")
    print(f"out data start: {ctrl_out_data_strt}")
    print(f"mux data start: {ctrl_mux_data_strt}")
    print(f"mux data stride: {ctrl_mux_data_strides}")
    print()

    # Now transforms ranges and strides
    (tform_extent, tform_cyc_strides) = transform_strides_and_ranges(ctrl_ranges, ctrl_cyc_strides, ctrl_dim)
    tform_in_data_strides = None
    if ctrl_in_data_strt is not None:
        (tform_extent, tform_in_data_strides) = transform_strides_and_ranges(ctrl_ranges, ctrl_in_data_strides, ctrl_dim)
    
    tform_out_data_strides = None
    if ctrl_out_data_strt is not None:
        (tform_extent, tform_out_data_strides) = transform_strides_and_ranges(ctrl_ranges, ctrl_out_data_strides, ctrl_dim)

    tform_mux_data_strides = None
    if ctrl_mux_data_strt is not None:
        (tform_extent, tform_mux_data_strides) = transform_strides_and_ranges(ctrl_ranges, ctrl_mux_data_strides, ctrl_dim)

    # Basically give a starting margin for everything...
    garnet_delay = 0

    mapped_ctrl = ControllerInfo(dim=ctrl_dim,
                                 cyc_strt=ctrl_cyc_strt + garnet_delay,
                                 in_data_strt=ctrl_in_data_strt,
                                 extent=tform_extent,
                                 cyc_stride=tform_cyc_strides,
                                 in_data_stride=tform_in_data_strides,
                                 out_data_strt=ctrl_out_data_strt,
                                 out_data_stride=tform_out_data_strides,
                                 mux_data_strt=ctrl_mux_data_strt,
                                 mux_data_stride=tform_mux_data_strides)

    return mapped_ctrl


def get_static_bitstream(config_path):

    in2agg = map_controller(extract_controller(config_path + '/input_in2agg_0.csv'), "in2agg")
    agg2sram = map_controller(extract_controller(config_path + '/input_agg2sram.csv'), "agg2sram")
    sram2tb = map_controller(extract_controller(config_path + '/output_2_sram2tb.csv'), "sram2tb")
    tb2out0 = map_controller(extract_controller(config_path + '/output_2_tb2out_0.csv'), "tb2out0")
    tb2out1 = map_controller(extract_controller(config_path + '/output_2_tb2out_1.csv'), "tb2out1")

    # Set configuration...
    config = [
        ("strg_ub_agg_read_addr_gen_0_starting_addr", agg2sram.out_data_strt),
        ("strg_ub_input_addr_gen_starting_addr", agg2sram.in_data_strt),
        ("strg_ub_input_sched_gen_sched_addr_gen_starting_addr", agg2sram.cyc_strt),
        ("strg_ub_loops_in2buf_autovec_read_0_dimensionality", agg2sram.dim),
        ("strg_ub_loops_in2buf_autovec_write_dimensionality", agg2sram.dim),

        ("strg_ub_output_addr_gen_starting_addr", sram2tb.out_data_strt),
        ("strg_ub_tb_write_addr_gen_0_starting_addr", sram2tb.in_data_strt),
        ("strg_ub_tb_write_addr_gen_1_starting_addr", sram2tb.in_data_strt),
        ("strg_ub_out_port_sel_addr_starting_addr", sram2tb.mux_data_strt),
        ("strg_ub_output_sched_gen_sched_addr_gen_starting_addr", sram2tb.cyc_strt),
        ("loops_buf2out_autovec_read_dimensionality", sram2tb.dim),

        ("strg_ub_loops_buf2out_out_sel_dimensionality", sram2tb.dim),

        ("strg_ub_agg_write_addr_gen_0_starting_addr", in2agg.in_data_strt),
        ("strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr", in2agg.cyc_strt),
        ("strg_ub_loops_in2buf_0_dimensionality", in2agg.dim),

        ("strg_ub_tb_read_addr_gen_0_starting_addr", tb2out0.out_data_strt),
        ("strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr", tb2out0.cyc_strt),
        ("strg_ub_loops_buf2out_read_0_dimensionality", tb2out0.dim),
        ("strg_ub_loops_buf2out_autovec_write_0_dimensionality", tb2out0.dim),

        ("strg_ub_tb_read_addr_gen_1_starting_addr", tb2out1.out_data_strt),
        ("strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr", tb2out1.cyc_strt),
        ("strg_ub_loops_buf2out_read_1_dimensionality", tb2out1.dim),
        ("strg_ub_loops_buf2out_autovec_write_1_dimensionality", tb2out1.dim),

        # Chaining
        ("chain_idx_input", 0),  # 1
        ("chain_idx_output", 0),  # 1
        ("enable_chain_input", 0),  # 1
        ("enable_chain_output", 0),  # 1
        ("chain_valid_in_reg_sel", 1),  # 1
        ("chain_valid_in_reg_value", 0),  # 1

        # Control Signals...
        ("flush_reg_sel", 1),  # 1
        ("flush_reg_value", 0),  # 1
        ("ren_in_reg_sel", 1),  # 1
        ("ren_in_reg_value", 0),  # 1
        ("wen_in_reg_sel", 1),  # 1
        ("wen_in_reg_value", 0),  # 1

        # Set the mode and activate the tile...
        ("mode", 0),  # 2
        ("tile_en", 1),  # 1
    ]

    for i in range(in2agg.dim):
        config.append((f"strg_ub_loops_in2buf_0_ranges_{i}", in2agg.extent[i]))
        config.append((f"strg_ub_agg_write_addr_gen_0_strides_{i}", in2agg.in_data_stride[i]))
        config.append((f"strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_{i}", in2agg.cyc_stride[i]))

    for i in range(agg2sram.dim):
        config.append((f"strg_ub_loops_in2buf_autovec_read_0_ranges_{i}", agg2sram.extent[i]))
        config.append((f"strg_ub_agg_read_addr_gen_0_strides_{i}", agg2sram.out_data_stride[i]))
        config.append((f"strg_ub_loops_in2buf_autovec_write_ranges_{i}", agg2sram.extent[i]))
        config.append((f"strg_ub_input_addr_gen_strides_{i}", agg2sram.in_data_stride[i]))
        config.append((f"strg_ub_input_sched_gen_sched_addr_gen_strides_{i}", agg2sram.cyc_stride[i]))

    tbs = [tb2out0, tb2out1]

    for i in range(sram2tb.dim):
        config.append((f"strg_ub_loops_buf2out_autovec_read_ranges_{i}", sram2tb.extent[i]))
        config.append((f"strg_ub_output_addr_gen_strides_{i}", sram2tb.out_data_stride[i]))
        config.append((f"strg_ub_output_sched_gen_sched_addr_gen_strides_{i}", sram2tb.cyc_stride[i]))
        config.append((f"strg_ub_loops_buf2out_out_sel_ranges_{i}", sram2tb.extent[i]))
        config.append((f"strg_ub_out_port_sel_addr_strides_{i}", sram2tb.mux_data_stride[i]))
        for tb in range(len(tbs)):
            config.append((f"strg_ub_tb_write_addr_gen_{tb}_strides_{i}", sram2tb.in_data_stride[i]))

    tbs = [tb2out0, tb2out1]
    for tb in range(len(tbs)):
        elem = tbs[tb]
        for i in range(elem.dim):
            config.append((f"strg_ub_loops_buf2out_autovec_write_{tb}_ranges_{i}", elem.extent[i]))
            config.append((f"strg_ub_loops_buf2out_read_{tb}_ranges_{i}", elem.extent[i]))
            config.append((f"strg_ub_tb_read_addr_gen_{tb}_strides_{i}", elem.out_data_stride[i]))
            config.append((f"strg_ub_tb_read_sched_gen_{tb}_sched_addr_gen_strides_{i}", elem.cyc_stride[i]))

    return config


def test_lake(config_path, 
              stream_path,
              in_ports=2,
              out_ports=2):

    lt_dut = LakeTop(interconnect_input_ports=in_ports,
                     interconnect_output_ports=out_ports)

    magma_dut = kts.util.to_magma(lt_dut,
                                  flatten_array=True,
                                  check_multiple_driver=False,
                                  optimize_if=False,
                                  check_flip_flop_always_ff=False)

    tester = fault.Tester(magma_dut, magma_dut.clk)

    tester.circuit.clk = 0
    tester.circuit.rst_n = 0
    tester.step(2)
    tester.circuit.rst_n = 1
    tester.step(2)
    tester.circuit.clk_en = 1
    tester.eval()

    # args are input ports, output ports
    in_data, out_data = generate_data_lists(stream_path, in_ports, out_ports)

    configs = get_static_bitstream(config_path)

    for (f1, f2) in configs:
        setattr(tester.circuit, f1, f2)

    for i in range(len(in_data[0])):
        for j in range(len(in_data)):
            if i < len(in_data[j]):
                setattr(tester.circuit, f"data_in_{j}", in_data[j][i])

        tester.eval()

        # for j in range(len(out_data)):
        #     if i < len(out_data[j]):
        #         getattr(tester.circuit, f"data_out_{j}").expect(out_data[j][i])

        tester.step(2)

    with tempfile.TemporaryDirectory() as tempdir:
        tempdir="dump"
        tester.compile_and_run(target="verilator",
                               directory=tempdir,
                               flags=["-Wno-fatal", "--trace"])

if __name__ == "__main__":
    config_path = "/nobackupkiwi/skavya/clockwork/lake_controllers/conv_3_3_new"
    stream_path = "/nobackupkiwi/skavya/lake/lake/utils/test.csv"

    test_lake(config_path, stream_path)
