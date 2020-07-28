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
    int_gen = generator.internal_generator

    with open(filepath, "w+") as fi:
        # Now get the config registers from the top definition
        for port_name in int_gen.get_port_names():
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, FormalAttr))
            if len(attrs) != 1:
                continue
            form_attr = attrs[0]
            fi.write(form_attr.get_annotation() + "\n")
