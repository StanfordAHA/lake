import collections
from lake.utils.util import transform_strides_and_ranges


ControllerInfo = collections.namedtuple('ControllerInfo',
                                        'dim extent cyc_stride in_data_stride cyc_strt \
                                            in_data_strt out_data_stride out_data_strt mux_data_stride mux_data_strt')


verbose_controller_info = False


def search_for_config(cfg_file, key):
    lines = cfg_file
    matches = [l for l in lines if key in l]
    # to account for multiple aggs, tbs, take lowest starting addr
    # for blocks of memories
    if key[0:5] == "write":
        mux_matches = [l for l in lines if "mux_" + key in l]

        for mux in mux_matches:
            matches.remove(mux)
    if len(matches) > 1:
        addrs = []
        for i in range(len(matches)):
            addrs.append(matches[i].split(',')[1])
        matches[0] = matches[addrs.index(min(addrs))]
    # return nothing if no matches (in / out do not always exist)
    if len(matches) > 0:
        return int(matches[0].split(',')[1])


def get_property(node, prop):
    if prop in node:
        return node[prop]
    else:
        return None


def extract_controller_json(control_node):
    dim = get_property(control_node, 'dimensionality')
    cyc_strt = get_property(control_node, "cycle_starting_addr")[0]
    mux_data_strt = get_property(control_node, 'mux_write_data_starting_addr')
    if mux_data_strt:
        mux_data_strt = mux_data_strt[0]
    in_data_strt = get_property(control_node, 'write_data_starting_addr')
    if in_data_strt:
        in_data_strt = in_data_strt[0]
    out_data_strt = get_property(control_node, 'read_data_starting_addr')
    if out_data_strt:
        out_data_strt = out_data_strt[0]
    ranges = get_property(control_node, 'extent')
    cyc_strides = get_property(control_node, 'cycle_stride')
    in_data_strides = get_property(control_node, 'write_data_stride')
    out_data_strides = get_property(control_node, 'read_data_stride')
    mux_data_strides = get_property(control_node, 'mux_write_data_stride')

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


def extract_controller(file_path):
    file_lines = None
    with open(file_path) as ctrl_f:
        file_lines = ctrl_f.readlines()

    dim = search_for_config(file_lines, 'dimensionality')
    cyc_strt = search_for_config(file_lines, 'cycle_starting_addr')
    mux_data_strt = search_for_config(file_lines, 'mux_write_data_starting_addr')
    in_data_strt = search_for_config(file_lines, 'write_data_starting_addr')
    out_data_strt = search_for_config(file_lines, 'read_data_starting_addr')

    ranges = []
    cyc_strides = []
    in_data_strides = []
    out_data_strides = []
    mux_data_strides = []

    for i in range(dim):
        ranges.append(search_for_config(file_lines, f"extent_{i}"))
        cyc_strides.append(search_for_config(file_lines, f"cycle_stride_{i}"))
        mux_data_strides.append(search_for_config(file_lines, f"mux_write_data_stride_{i}"))
        in_data_strides.append(search_for_config(file_lines, f"write_data_stride_{i}"))
        out_data_strides.append(search_for_config(file_lines, f"read_data_stride_{i}"))

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

    if verbose_controller_info:
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
        if "agg2sram" in name:
            ctrl_in_data_strt *= 4
            for i in range(len(tform_in_data_strides)):
                tform_in_data_strides[i] *= 4

    tform_out_data_strides = None
    if ctrl_out_data_strt is not None:
        (tform_extent, tform_out_data_strides) = transform_strides_and_ranges(ctrl_ranges, ctrl_out_data_strides, ctrl_dim)
        if "sram2tb" in name:
            ctrl_out_data_strt *= 4
            for i in range(len(tform_out_data_strides)):
                tform_out_data_strides[i] *= 4

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


def factor_sram2tb(sram2tb, tb2out, max_outer_loops):
    # outer loop factorization
    # find which loop levels of sram2tb and tb2out have the same extent and cycle strides (before map_controller)
    sram2tb_idx = len(sram2tb.extent) - 1
    tb2out_idx = len(tb2out.extent) - 1
    shared_loop_lvls = 0
    while(sram2tb_idx >= 0 or tb2out_idx >= 0):
        if (shared_loop_lvls < max_outer_loops) and \
                (sram2tb.extent[sram2tb_idx] == tb2out.extent[tb2out_idx]) and \
                (sram2tb.cyc_stride[sram2tb_idx] == tb2out.cyc_stride[tb2out_idx]):

            # print(f"same extent {sram2tb.extent[sram2tb_idx]}, sram2tb_idx {sram2tb_idx}, tb2out_idx {tb2out_idx}")
            # print(f"untouched cyc_stride {sram2tb.cyc_stride[sram2tb_idx]}, cyc_stride {tb2out.cyc_stride[tb2out_idx]}")
            # print(f"cyc_stride {sram2tb_0.cyc_stride[sram2tb_idx]}, cyc_stride {tb2out_0.cyc_stride[tb2out_idx]}")
            sram2tb_idx -= 1
            tb2out_idx -= 1
            shared_loop_lvls += 1
        else:
            break
    assert(sram2tb.dim - shared_loop_lvls >= 0)
    assert(sram2tb.dim - shared_loop_lvls <= max_outer_loops)
    assert(tb2out.dim - shared_loop_lvls >= 0)
    assert(tb2out.dim - shared_loop_lvls <= max_outer_loops)

    if shared_loop_lvls == 0:
        return 0

    # calculate the delay from sram2tb's outer loop to tb2out's outer loop
    sram2tb_inner_extent_sum = 0
    for i in range(sram2tb.dim - shared_loop_lvls):
        sram2tb_inner_extent_sum += (sram2tb.extent[i] - 1) * sram2tb.cyc_stride[i]
    tb2out_inner_extent_sum = 0
    for i in range(tb2out.dim - shared_loop_lvls):
        tb2out_inner_extent_sum += (tb2out.extent[i] - 1) * tb2out.cyc_stride[i]
    sram2tb_delay = tb2out.cyc_strt - sram2tb.cyc_strt + tb2out_inner_extent_sum - sram2tb_inner_extent_sum
    print("delay", sram2tb_delay)

    # now check how many sram2tb has happened within the delay
    num_sram2tb = 0
    for i in range(sram2tb.dim - 1, -1, -1):
        if sram2tb_delay >= sram2tb.cyc_stride[i]:
            num_sram2tb_lvl = int(sram2tb_delay / sram2tb.cyc_stride[i])
            # find the total number of all inner loop levels
            for j in range(i):
                num_sram2tb_lvl *= sram2tb.extent[j]
            num_sram2tb += num_sram2tb_lvl
            sram2tb_delay = sram2tb_delay % sram2tb.cyc_stride[i]
    if sram2tb_delay > 0:
        num_sram2tb += 1

    print("num_sram2tb", num_sram2tb)
    assert num_sram2tb <= 12, f"sram2tb outer loop delay fifo (12) cannot hold {num_sram2tb} items"

    return shared_loop_lvls
