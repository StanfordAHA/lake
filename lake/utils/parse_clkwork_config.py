import collections

ControllerInfo = collections.namedtuple('ControllerInfo',
                                        'dim extent cyc_stride in_data_stride cyc_strt \
                                            in_data_strt out_data_stride out_data_strt mux_data_stride mux_data_strt')


verbose_controller_info = False


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
