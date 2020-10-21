from lake.attributes.config_reg_attr import ConfigRegAttr
import kratos as kts
from kratos import *
import math
import os as os
from enum import Enum
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


lake_util_verbose_trim = False


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
            pdir = "input"
            if str(curr_port.port_direction) == "PortDirection.Out":
                pdir = "output"
            # If there are 0 or more than one attributes, let's just use the default X attribute
            if len(attrs) != 1:
                if pdir is "input":
                    form_attr = FormalAttr(port_name, FormalSignalConstraint.SET0)
                else:
                    form_attr = FormalAttr(port_name, FormalSignalConstraint.X)

            else:
                form_attr = attrs[0]
            size_str = get_size_str(curr_port)

            fi.write(f"{pdir} logic {size_str}" + form_attr.get_annotation() + "\n")


def get_configs_dict(configs):
    configs_dict = {}
    for (f1, f2) in configs:
        configs_dict[f1] = f2
    return configs_dict


def set_configs_sv(generator, filepath, configs_dict):
    int_gen = generator.internal_generator
    ports = int_gen.get_port_names()
    configs_list = []

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
                configs_list.append(name)

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
                configs_list.append(remaining)
                # fi.write("wire [" + str(port_width - 1) + ":0] " + remaining + " = " + str(port_width) + "'h0;\n")
                # fi.write("assign " + remaining + " = " + str(port.width) + "'h0;\n")
    return configs_list


def generate_lake_config_wrapper(configs_list,
                                 configs_file,
                                 lake_file,
                                 module_name):

    # get top level interface, minus config regs in not_configs
    with open(lake_file, 'r') as lake:
        start = False
        not_configs = []
        for line in lake:
            if "module LakeTop_W (" in line:
                start = True
            elif start and ");" in line:
                start = False
                break
            elif start:
                add = 1
                for config in configs_list:
                    if config in line:
                        add = 0
                        break
                if add == 1:
                    not_configs.append(line)

    with open(f"LakeWrapper_{module_name}.v", "w+") as wrapper:

        # write top level interface (without config regs)
        wrapper.write(f"module {module_name} (\n")
        for not_config in not_configs:
            wrapper.write(not_config)
        wrapper.write(");\n")

        # set config regs as constants
        with open(configs_file, "r") as configs:
            for config in configs:
                wrapper.write(config)

        # instantiate LakeTop_W module with
        # full interface, first with nonconfigs
        wrapper.write("LakeTop_W LakeTop_W (\n")
        for i in range(len(not_configs)):
            not_config = not_configs[i]
            try_name = not_config.split("]")
            if len(try_name) == 1:
                try_name = not_config.split("logic")
            name = try_name[1].split(",")[0]
            # if there are no config regs, this is the last
            # signal in the interface
            if (i == len(not_configs) - 1) and (len(configs_list) == 0):
                wrapper.write(f".{name}({name})\n);\n")
            else:
                wrapper.write(f".{name}({name}),\n")

        # hook up config regs for LakeTop_W as well
        for i in range(len(configs_list)):
            config = configs_list[i]
            if i == len(configs_list) - 1:
                wrapper.write(f".{config}({config})\n);\n")
            else:
                wrapper.write(f".{config}({config}),\n")

        wrapper.write("endmodule\n\n")

    # prepend wrapper module to original verilog file
    with open("LakeWrapper.v", "a") as wrapper:
        with open(lake_file, "r") as original:
            for line in original:
                wrapper.write(line)


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


# Trim an individual config
def trim_config(flat_gen, cfg_reg_name, value):
    cfg_port = flat_gen.get_port(cfg_reg_name)
    if cfg_port is None:
        print(f"No config reg: {cfg_reg_name}...is that expected?")
        return (cfg_reg_name, 0)
    bmask = int(math.pow(2, cfg_port.width)) - 1
    if lake_util_verbose_trim:
        print(f"Port name: {cfg_reg_name}, Port width: {cfg_port.width}, corresponding mask_val: {bmask}")
    return (cfg_reg_name, value & bmask)


def trim_config_list(flat_gen, config_list):
    # Store trimmed values here...
    config = []
    for name, value in config_list:
        config.append(trim_config(flat_gen, name, value))
    return config


# Add a simple counter to a design and return the signal
def add_counter(generator, name, bitwidth, increment=kts.const(1, 1)):
    ctr = generator.var(name, bitwidth)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def ctr_inc_code():
        if ~generator._rst_n:
            ctr = 0
        elif increment:
            ctr = ctr + 1

    generator.add_code(ctr_inc_code)
    return ctr


def add_config_reg(generator, name, description, bitwidth, **kwargs):
    cfg_reg = generator.input(name, bitwidth, **kwargs)
    cfg_reg.add_attribute(ConfigRegAttr(description))
    return cfg_reg


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


def process_line(item):
    item = item.strip()
    item_nobrack = item.rstrip("]").lstrip("[")
    individ = item_nobrack.split(" ")
    print(f"individ: {individ}")
    inced = []
    for i in range(len(individ)):
        inced.append(int(individ[i]) + 1)

    ret_str = "[" + str(inced[0])
    for i in range(len(inced) - 1):
        ret_str = ret_str + " " + str(inced[i + 1])
    ret_str = ret_str + "]"
    return ret_str


def increment_line(line):
    splitline = line.split(",")
    di = splitline[0]
    vi = splitline[1]
    do = splitline[2]
    vo = splitline[3]

    di_p = process_line(di)
    vi_p = vi
    do_p = process_line(do)
    vo_p = vo

    return di_p + ", " + vi_p + ", " + do_p + ", " + vo_p


def increment_csv(file_in, file_out, fields):
    with open(file_in) as infile:
        infile_lines = infile.readlines()
        with open(file_out, "w+") as outfile:
            outfile.write(infile_lines[0])
            for i in range(len(infile_lines) - 1):
                if len(infile_lines[i + 1]) > 5:
                    print(f"line {i}: {infile_lines[i + 1]}")
                    outfile.write(increment_line(infile_lines[i + 1]))


if __name__ == "__main__":
    increment_csv("sequence.csv", "inced_csv.csv", [])
