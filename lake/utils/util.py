from lake.attributes.config_reg_attr import ConfigRegAttr
import kratos as kts
from kratos import *
import math
import os as os
from enum import Enum
from lake.attributes.formal_attr import *
import csv

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
    # format for formal team: no space between the size dims,
    # but space between the size dims and width dim
    width_dim = ""
    size_dims = ""
    for dim in range(len(port.size)):
        if port.size[dim] > 1 or port.explicit_array:
            size_dims += f"[{port.size[dim] - 1}:0]"
    if port.width > 1:
        width_dim = f"[{port.width - 1}:0] "
    return size_dims + " " + width_dim


def modular_formal_annotation(gen, mem_names):
    filepath = "mod_strg_ub_signals.txt"

    int_gen = gen.internal_generator
    mems = {}
    for i in mem_names:
        mems[i] = []

    for var_name, var in gen.vars:

        in_mem = False
        for i in mem_names:
            if i in var_name:
                in_mem = True
                curr_port = int_gen.get_port(var_name)
                if curr_port is not None:
                    attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
                    if len(attrs) == 0:
                        mems[i].append(var_name)
                else:
                    mems[i].append(var_name)

        if not in_mem:
            for i in mem_names:
                mems[i].append(var_name)

    for i in mem_names:
        with open(i + filepath, "w+") as fi:
            for a in mems[i]:
                fi.write(a + "\n")


def extract_formal_annotation(generator, filepath, module_attr="agg"):
    # Get the port list and emit the annotation for each...
    int_gen = generator.internal_generator
    port_names = int_gen.get_port_names()

    # mapping for which config regs the dimensionality config regs constrain
    pairings = {}

    # get list of agg/sram shared and sram/tb shared config regs for
    # formal config reg propagation for the sram formal subproblem
    agg_sram_shared, sram_tb_shared = [], []

    with open(filepath, "w+") as fi:
        # Now get the config registers from the top definition
        for port_name in port_names:

            # get list of agg/sram shared and sram/tb shared config regs for
            # formal config reg propagation for the sram formal subproblem
            if module_attr == "sram":
                if "agg_sram_shared" in port_name:
                    agg_sram_shared.append(port_name)
                if "sram_tb_shared" in port_name:
                    sram_tb_shared.append(port_name)

            # print(port_name)
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, FormalAttr))

            pdir = "output" if str(curr_port.port_direction) == "PortDirection.Out" else "input"

            def get_unlabeled_formal_attr(pdir, port_name):
                if pdir == "input":
                    return FormalAttr(port_name, FormalSignalConstraint.SET0)
                else:
                    return FormalAttr(port_name, FormalSignalConstraint.X)

            # If there are 0 or more than one attributes, let's just use the default X attribute
            # If there is 1 attribute, but it is not applied for this module or all modules,
            # use the default X attribute

            if len(attrs) != 1:
                form_attr = get_unlabeled_formal_attr(pdir, port_name)
            # len(attrs) == 1 for rest of the cases
            elif attrs[0].get_module() not in ("all", module_attr):
                # for full lake_top annotation, agg inputs and tb outputs
                # should be in full lake_top annotation
                port_module_attr = attrs[0].get_module()
                if module_attr == "full" and \
                    ((pdir == "input" and port_module_attr == "agg") or
                     (pdir == "output" and port_module_attr == "tb")):
                    form_attr = attrs[0]
                else:
                    form_attr = get_unlabeled_formal_attr(pdir, port_name)
            else:
                form_attr = attrs[0]

            size_str = get_size_str(curr_port)

            fi.write(f"{pdir} logic {size_str}" + form_attr.get_annotation() + "\n")

            # dimensionality pairing constraints
            keywords = ["agg_only", "agg_sram_shared", "sram_only", "sram_tb_shared", "tb_only"]
            # make this dependent on agg_height and tb_height
            height = 2
            indices = [f"_{i}_" for i in range(height)]

            if "dimensionality" in port_name:
                dim_keyword = None
                for keyword in keywords:
                    if keyword in port_name:
                        dim_keyword = keyword
                        break
                index = None
                for i in indices:
                    if i in port_name:
                        index = i
                        break

                if dim_keyword is None or index is None:
                    print(f"Error! Does not belong to any module...skipping {port_name}")
                else:
                    # use keyword and index for this mapping to find
                    # corresponding constrained config regs
                    pairings[port_name[:-len("dimensionality")]] = {"keyword": dim_keyword, "index": index, "maps": []}

    # find config regs to be constrained by keys in pairings
    for port_name in port_names:
        if "dimensionality" in port_name or "ranges" in port_name:
            continue

        for key in pairings.keys():
            pairing = pairings[key]
            index = pairing["index"]
            if pairing["keyword"] in port_name and pairing["index"] in port_name:
                pairing["maps"].append(port_name)
                break
            if module_attr == "sram" and "sram_only" in port_name:
                if f"input_addr_gen{index}" in port_name and f"autovec_write{index}" in key:
                    pairing["maps"].append(port_name)
                elif f"output_addr_gen{index}" in port_name and f"autovec_read{index}" in key:
                    pairing["maps"].append(port_name)

    # print just the mappings
    with open(f"mapping_{filepath}", "w+") as fi:
        move_regs = {}

        for key in pairings.keys():
            maps = pairings[key]["maps"]
            if "agg_only" in key:
                for reg in maps:
                    if "read" in reg:
                        move_regs[reg] = {"orig_key": key, "keyword": "agg_sram_shared", "index": pairings[key]["index"]}
            elif "tb_only" in key:
                for reg in maps:
                    if "write" in reg:
                        move_regs[reg] = {"orig_key": key, "keyword": "sram_tb_shared", "index": pairings[key]["index"]}

        for reg_key in move_regs.keys():
            reg = move_regs[reg_key]
            for pairing_key in pairings.keys():
                pairing = pairings[pairing_key]
                if reg["keyword"] == pairing["keyword"] and reg["index"] == pairing["index"]:
                    pairings[reg["orig_key"]]["maps"].remove(reg_key)
                    pairing["maps"].append(reg_key)

        for key in pairings.keys():
            pairings[key] = pairings[key]["maps"]
            for p in pairings[key]:
                if "enable" in p:
                    pairings[key].remove(p)

        print(pairings, file=fi)

    # print list of agg/sram shared and sram/tb shared config regs for
    # formal config reg propagation for the sram formal subproblem
    if module_attr == "sram":
        print(agg_sram_shared, file=open("agg_sram_shared.txt", "w+"))
        print(sram_tb_shared, file=open("sram_tb_shared.txt", "w+"))


def extract_formal_annotation_collat(generator, filepath, mem_names, edges):
    # Get the port list and emit the annotation for each...
    int_gen = generator.internal_generator

    mems = {}
    for i in mem_names:
        mems[i] = []

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
                if pdir == "input":
                    form_attr = FormalAttr(port_name, FormalSignalConstraint.SET0)
                else:
                    form_attr = FormalAttr(port_name, FormalSignalConstraint.X)

            else:
                form_attr = attrs[0]
            size_str = get_size_str(curr_port)

            out_string = f"{pdir} logic {size_str}" + form_attr.get_annotation() + "\n"
            fi.write(out_string)

            in_mem = False
            for i in mem_names:
                if i in form_attr.get_annotation():
                    in_mem = True
                    mems[i].append(out_string)

            if not in_mem:
                for i in mem_names:
                    mems[i].append(out_string)

    for i in mem_names:
        with open(i + filepath, "w+") as fi:
            for a in mems[i]:
                fi.write(a)


def get_configs_dict(configs):
    configs_dict = {}
    for (f1, f2) in configs:
        configs_dict[f1] = f2
    return configs_dict


def set_configs_sv(generator, filepath, configs_dict, iterator_support=6):
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
        # find config regs
        if len(curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))) == 1:
            if ("strides" in port) or ("ranges" in port):
                for i in range(iterator_support):
                    remain.append(port + f"_{i}")
            else:
                remain.append(port)

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
            if port is None and lake_util_verbose_trim:
                print("No port: ", port_name)
            if port is not None:
                fi.write("wire [" + str(port_width - 1) + ":0] " + remaining + " = 0;\n")
                configs_list.append(remaining)
                # fi.write("wire [" + str(port_width - 1) + ":0] " + remaining + " = " + str(port_width) + "'h0;\n")
                # fi.write("assign " + remaining + " = " + str(port.width) + "'h0;\n")
    return configs_list


def generate_lake_config_wrapper(configs_list,
                                 configs_file,
                                 lake_file,
                                 module_name,
                                 flattened_name="LakeTop"):

    # get top level interface, minus config regs in not_configs
    with open(lake_file, 'r') as lake:
        start = False
        not_configs = []
        for line in lake:
            if f"module {flattened_name}_W (" in line:
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
        wrapper.write(f"{flattened_name}_W {flattened_name}_W (\n")
        for i in range(len(not_configs)):
            not_config = not_configs[i]
            try_name = not_config.split("]")
            if len(try_name) == 1:
                try_name = not_config.split("logic")
            if "," in not_config:
                try_name = try_name[-1].split(",")[-2]
            else:
                try_name = try_name[-1][:-1]
            name = try_name
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
        if lake_util_verbose_trim:
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


def register(generator, signal, enable=kts.const(1, 1), clear=kts.const(0, 1), name=None):
    ''' Pass a generator and a signal to create a registered
        version of any signal easily.
    '''
    use_name = signal.name + "_d1"
    if name is not None:
        use_name = name
    reg = generator.var(use_name, signal.width)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def reg_code():
        if ~generator._rst_n:
            reg = 0
        elif clear:
            reg = 0
        elif enable:
            reg = signal

    generator.add_code(reg_code)
    return reg


def add_config_reg(generator, name, description, bitwidth, **kwargs):
    cfg_reg = generator.input(name, bitwidth, **kwargs)
    cfg_reg.add_attribute(ConfigRegAttr(description))
    return cfg_reg


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


# Takes in a bus of valid tags with an associated data stream
# Send the proper data stream thru
def decode(generator, sel, signals):

    # This base case means we don't need to actually do anything
    if sel.width == 1:
        return signals[0]

    # Create scan signal
    tmp_done = generator.var(f"decode_sel_done_{sel.name}_{signals.name}", 1)
    if signals.size[0] > 1:
        if len(signals.size) > 1:
            ret = generator.var(f"decode_ret_{sel.name}_{signals.name}",
                                signals.width,
                                size=signals.size[1:],
                                explicit_array=True,
                                packed=True)
        else:
            ret = generator.var(f"decode_ret_{sel.name}_{signals.name}",
                                signals.width)
    else:
        ret = generator.var(f"decode_ret_{sel.name}_{signals.name}", 1)

    @always_comb
    def scan_lowest():
        tmp_done = 0
        ret = 0
        for i in range(sel.width):
            if ~tmp_done & sel[i]:
                ret = signals[i]
                tmp_done = 1

    generator.add_code(scan_lowest)
    return ret


def get_priority_encode(generator, signal):
    assert generator is not None
    assert signal is not None
    sig_width = signal.width
    new_sig = None
    if sig_width == 1:
        new_sig = generator.var(f"{signal.name}_pri_enc", 1)
        generator.wire(new_sig, kts.const(0, 1))
    else:
        # In the case of a multibit signal, create the encoded signal and then create the assignment
        new_sig = generator.var(f"{signal.name}_pri_enc", kts.clog2(sig_width))
        encode_comb = generator.combinational()
        # create the ifs
        prev_if = None
        for i in range(sig_width):
            first = i == 0
            last = i == sig_width - 1
            new_if = None
            if first:
                # Create the if chain first
                new_if = encode_comb.if_(signal[i])
                new_if.then_(new_sig.assign(i))
                prev_if = new_if
            elif last:
                # At the end, apply the final else as well
                new_if = IfStmt(signal[i])
                new_if.then_(new_sig.assign(i))
                prev_if.else_(new_if)
                new_if.else_(new_sig.assign(0))
            else:
                # In the middle, create new if and chain with the previous
                new_if = IfStmt(signal[i])
                new_if.then_(new_sig.assign(i))
                prev_if.else_(new_if)
                prev_if = new_if

    return new_sig


if __name__ == "__main__":
    increment_csv("sequence.csv", "inced_csv.csv", [])
