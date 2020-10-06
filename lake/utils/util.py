import kratos as kts
from kratos import *
import math
import os as os
from enum import Enum
from lake.attributes.formal_attr import FormalAttr


def check_env():
    lake_controller_path = os.getenv("LAKE_CONTROLLERS")
    lake_stream_path = os.getenv("LAKE_STREAM")

    assert lake_controller_path is not None and lake_stream_path is not None,\
        f"Please check env vars:\nLAKE_CONTROLLERS: {lake_controller_path}\nLAKE_STREAM: {lake_stream_path}"

    return (lake_controller_path + "/", lake_stream_path + "/")


def increment(var, value):
    return var + kts.const(value, var.width)


def decrement(var, value):
    return var - kts.const(value, var.width)


def transpose(generator: kts.Generator, port, name):
    pass


def int_to_list(data, width, num_items):
    to_ret = []
    comp = 2 ** width - 1
    for i in range(num_items):
        item = (data >> (width * i)) & comp
        to_ret.append(item)
    return to_ret


def list_to_int(list_d, width):
    to_ret = 0
    for i in range(len(list_d)):
        to_ret = to_ret | (list_d[i] << (width * i))
    return to_ret


def get_size_str(port):
    dim_1 = ""
    dim_2 = ""
    if port.size[0] > 1 or port.explicit_array:
        dim_2 = f"[{port.size[0] - 1}:0] "
    if port.width > 1:
        dim_1 = f"[{port.width - 1}:0] "
    return dim_2 + dim_1


def extract_formal_annotation(generator, filepath):
    # Get the port list and emit the annotation for each...
    int_gen = generator.internal_generator

    with open(filepath, "w+") as fi:
        # Now get the config registers from the top definition
        for port_name in int_gen.get_port_names():
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, FormalAttr))
            if len(attrs) != 1:
                continue
            form_attr = attrs[0]
            pdir = "input"
            size_str = get_size_str(curr_port)
            if str(curr_port.port_direction) == "PortDirection.Out":
                pdir = "output"

            fi.write(f"{pdir} logic {size_str}" + form_attr.get_annotation() + "\n")


def get_configs_dict(configs):
    configs_dict = {}
    for (f1, f2) in configs:
        configs_dict[f1] = f2
    return configs_dict


def set_configs_sv(generator, filepath, configs_dict):
    int_gen = generator.internal_generator
    ports = int_gen.get_port_names()

    remain = []
    for port_name in ports:
        curr_port = int_gen.get_port(port_name)
        attrs = curr_port.find_attribute(lambda a: isinstance(a, FormalAttr))
        if len(attrs) != 1:
            continue
        port = attrs[0].get_port_name()
        if ("dimensionality" in port) or ("starting_addr" in port):
            remain.append(port)
        else:
            for i in range(6):
                remain.append(port + f"_{i}")

    with open(filepath, "w+") as fi:
        for name in configs_dict.keys():
            # binstr = str(hex(configs_dict[name]))
            # binsplit = binstr.split("x")
            # value = binsplit[-1].upper()
            value = str(configs_dict[name])
            port_name = name
            if ("strides" in name) or ("ranges" in name):
                splitstr = name.split("_")
                index = -1 * len(splitstr[-1]) - 1
                port_name = name[:index]
                # name = port_name + f"[{splitstr[-1]}]"
            port = int_gen.get_port(port_name)
            if port is not None:
                port_width = port.width
                if name in remain:
                    remain.remove(name)
                # fi.write("assign " + name + " = " + str(port_width) + "'h" + value + ";\n")
                # fi.write("wire [" + str(port_width - 1) + ":0] " + name + " = " + str(port_width) + "'h" + value + ";\n")
                fi.write("wire [" + str(port_width - 1) + ":0] " + name + " = " + value + ";\n")

        # set all unused config regs to 0 since we remove them
        # from tile interface and they need to be set
        for remaining in remain:
            port_split = remaining.split("_")
            if not (("dimensionality" in remaining) or ("starting_addr" in remaining)):
                port_name = "_".join(port_split[:-1])
            port = int_gen.get_port(port_name)
            if port is None:
                print(port_name)
            if port is not None:
                fi.write("wire [" + str(port_width - 1) + ":0] " + remaining + " = 0;\n")
                # fi.write("wire [" + str(port_width - 1) + ":0] " + remaining + " = " + str(port_width) + "'h0;\n")
                # fi.write("assign " + remaining + " = " + str(port.width) + "'h0;\n")


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


def safe_wire(gen, w_to, w_from):
    '''
    Wire together two signals of (potentially) mismatched width to
    avoid the exception that Kratos throws.
    '''
    # Only works in one dimension...
    if w_to.width != w_from.width:
        print(f"SAFEWIRE: WIDTH MISMATCH: {w_to.name} width {w_to.width} <-> {w_from.name} width {w_from.width}")
        # w1 contains smaller width...
        if w_to.width < w_from.width:
            gen.wire(w_to, w_from[w_to.width - 1, 0])
        else:
            gen.wire(w_to[w_from.width - 1, 0], w_from)
            zero_overlap = w_to.width - w_from.width
            gen.wire(w_to[w_to.width - 1, w_from.width], kts.const(0, zero_overlap))
    else:
        gen.wire(w_to, w_from)


def zext(gen, wire, size):
    if wire.width >= size:
        return wire
    else:
        zext_signal = gen.var(f"{wire.name}_zext", size)
        gen.wire(zext_signal, kts.concat(kts.const(0, size - wire.width), wire))
        return zext_signal


def trim_config(flat_gen, cfg_reg_name, value):
    cfg_port = flat_gen.get_port(cfg_reg_name)
    if cfg_port is None:
        print(f"No config reg: {cfg_reg_name}...is that expected?")
        return (cfg_reg_name, 0)
    bmask = int(math.pow(2, cfg_port.width)) - 1
    print(f"Port name: {cfg_reg_name}, Port width: {cfg_port.width}, corresponding mask_val: {bmask}")
    return (cfg_reg_name, value & bmask)


# Add a simple counter to a design and return the signal
def add_counter(generator, name, bitwidth):
    ctr = generator.var(name, bitwidth)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def ctr_inc_code():
        if ~generator._rst_n:
            ctr = 0
        else:
            ctr = ctr + 1

    generator.add_code(ctr_inc_code)
    return ctr


# Function for generating Pond API
def generate_pond_api(ctrl_rd, ctrl_wr):
    (tform_ranges_rd, tform_strides_rd) = transform_strides_and_ranges(ctrl_rd[0], ctrl_rd[1], ctrl_rd[2])
    (tform_ranges_wr, tform_strides_wr) = transform_strides_and_ranges(ctrl_wr[0], ctrl_wr[1], ctrl_wr[2])

    new_config = {}

    new_config["rf_read_iter_0_dimensionality"] = ctrl_rd[2]
    new_config["rf_read_addr_0_starting_addr"] = ctrl_rd[3]
    new_config["rf_read_addr_0_strides_0"] = tform_strides_rd[0]
    new_config["rf_read_addr_0_strides_1"] = tform_strides_rd[1]
    new_config["rf_read_iter_0_ranges_0"] = tform_ranges_rd[0]
    new_config["rf_read_iter_0_ranges_1"] = tform_ranges_rd[1]

    new_config["rf_read_sched_0_sched_addr_gen_starting_addr"] = ctrl_rd[4]
    new_config["rf_read_sched_0_sched_addr_gen_strides_0"] = tform_strides_rd[0]
    new_config["rf_read_sched_0_sched_addr_gen_strides_1"] = tform_strides_rd[1]

    new_config["rf_write_iter_0_dimensionality"] = ctrl_wr[2]
    new_config["rf_write_addr_0_starting_addr"] = ctrl_wr[3]
    new_config["rf_write_addr_0_strides_0"] = tform_strides_wr[0]
    new_config["rf_write_addr_0_strides_1"] = tform_strides_wr[1]
    new_config["rf_write_iter_0_ranges_0"] = tform_ranges_wr[0]
    new_config["rf_write_iter_0_ranges_1"] = tform_ranges_wr[1]

    new_config["rf_write_sched_0_sched_addr_gen_starting_addr"] = ctrl_wr[4]
    new_config["rf_write_sched_0_sched_addr_gen_strides_0"] = tform_strides_wr[0]
    new_config["rf_write_sched_0_sched_addr_gen_strides_1"] = tform_strides_wr[1]

    return new_config
