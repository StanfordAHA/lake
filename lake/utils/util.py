import kratos as kts
from enum import Enum
from lake.attributes.formal_attr import FormalAttr


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


def extract_formal_annotation(generator, filepath):
    # Get the port list and emit the annotation for each...
    # int_gen = generator.internal_generator
    int_gen = generator

    with open(filepath, "w+") as fi:
        # Now get the config registers from the top definition
        for port_name in int_gen.get_port_names():
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, FormalAttr))
            if len(attrs) != 1:
                continue
            form_attr = attrs[0]
            fi.write(str(curr_port.width) + " " + form_attr.get_port_name() + "\n")
            # fi.write(form_attr.get_annotation() + "\n")


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
                remain.append(port+f"_{i}")

    with open(filepath, "w+") as fi:
        for name in configs_dict.keys():
            binstr = str(hex(configs_dict[name]))
            binsplit = binstr.split("x")
            value = binsplit[-1].upper()
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
                #fi.write("assign " + name + " = " + str(port_width) + "'h" + value + ";\n")
                fi.write("wire [" + str(port_width - 1) + ":0] " + name + " = " + str(port_width) + "'h" + value + ";\n")

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
                fi.write("wire [" + str(port_width - 1) + ":0] " + remaining + " = " + str(port_width) + "'h0;\n")
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
