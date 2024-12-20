from lake.attributes.config_reg_attr import ConfigRegAttr
import kratos as kts
from kratos import *
import math
import os as os
from lake.utils.spec_enum import MemoryPortType, Direction
from lake.attributes.formal_attr import *
import shutil as shutil
# from lake.spec.component import Component


lake_util_verbose_trim = False


def lift_port(child_gen, parent_gen, child_port):

    pname = child_port.name

    # Need to get the attributes and copy them up...
    port_attrs = child_port.attributes
    tmp_port = parent_gen.port_from_def(child_port, name=f"{child_port.name}")
    for attr in port_attrs:
        tmp_port.add_attribute(attr)
    parent_gen.wire(tmp_port, child_port)
    return tmp_port


def check_file_exists_and_has_content(file_path):
    # Check if the file exists
    if os.path.exists(file_path):
        # Check if the file is not empty
        if os.path.getsize(file_path) > 0:
            return True
        else:
            return False
    else:
        return False


def get_file_contents(file_path):
    good_file = check_file_exists_and_has_content(file_path=file_path)
    if good_file is True:
        contents = None
        with open(file_path, 'r') as open_file:
            contents = open_file.readlines()
        return contents
    else:
        return None


class TestPrepper():

    def __init__(self, base_dir: str = None) -> None:
        self.base_dir = base_dir
        self.pargs_file = None

    def prepare_hw_test(self):
        # Put it at the lake directory
        if self.base_dir is None:
            self.base_dir = os.path.dirname(os.path.abspath(__file__))
            final_dir = os.path.join(self.base_dir, "../../", "TEST")

        else:
            # Simpler to use absolute path here
            final_dir = os.path.abspath(self.base_dir)

        print(f" Prepare hw test at ...{final_dir}")

        os.makedirs(final_dir, exist_ok=True)
        os.makedirs(os.path.join(final_dir, "inputs"), exist_ok=True)
        os.makedirs(os.path.join(final_dir, "outputs"), exist_ok=True)
        os.makedirs(os.path.join(final_dir, "gold"), exist_ok=True)

        # Now copy over the tests/test_hw_spec
        tb_base_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "../../tests/test_spec_hw/")
        for filename in os.listdir(tb_base_path):
            src_file = os.path.join(tb_base_path, filename)
            dst_file = os.path.join(final_dir, filename)
            if os.path.isfile(src_file):  # Check if it's a file (not a directory)
                shutil.copy2(src_file, dst_file)

        # Create the PARGS file even if empty...
        file_path = os.path.join(final_dir, "inputs", "PARGS.txt")
        with open(file_path, 'w') as file:
            pass
        self.pargs_file = file_path

        return final_dir

    def add_pargs(self, pargs):
        if type(pargs) is list:
            self.__add_pargs_list(pargs)
        elif type(pargs) is tuple:
            self.__add_pargs_tuple(pargs)

    def __add_pargs_list(self, pargs_list):
        with open(self.pargs_file, 'a') as pargs_file_open:
            for parg_tuple in pargs_list:
                parg_name, parg_value = parg_tuple
                parg_string = f"+{parg_name}={str(parg_value)}\n"
                pargs_file_open.write(parg_string)

    def __add_pargs_tuple(self, parg_tuple):
        with open(self.pargs_file, 'a') as pargs_file_open:
            parg_name, parg_value = parg_tuple
            parg_string = f"+{parg_name}={str(parg_value)}\n"
            pargs_file_open.write(parg_string)


def prepare_hw_test(base_dir: str = None):

    # Put it at the lake directory
    if base_dir is None:
        base_dir = os.path.dirname(os.path.abspath(__file__))
        final_dir = os.path.join(base_dir, "../../", "TEST")

    else:
        # Simpler to use absolute path here
        final_dir = os.path.abspath(base_dir)

    print(f" Prepare hw test at ...{final_dir}")

    os.makedirs(final_dir, exist_ok=True)
    os.makedirs(os.path.join(final_dir, "inputs"), exist_ok=True)
    os.makedirs(os.path.join(final_dir, "outputs"), exist_ok=True)

    # Now copy over the tests/test_hw_spec
    tb_base_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "../../tests/test_spec_hw/")
    for filename in os.listdir(tb_base_path):
        src_file = os.path.join(tb_base_path, filename)
        dst_file = os.path.join(final_dir, filename)
        if os.path.isfile(src_file):  # Check if it's a file (not a directory)
            shutil.copy2(src_file, dst_file)

    return final_dir


def check_env():
    lake_controller_path = os.getenv("LAKE_CONTROLLERS")
    lake_stream_path = os.getenv("LAKE_STREAM")

    assert lake_controller_path is not None and lake_stream_path is not None, \
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


def intercept_cfg(generator, port):
    int_gen = generator.internal_generator
    actual_port = int_gen.get_port(port)
    # Remove the attribute
    attrs = actual_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
    if len(attrs) == 0:
        print("Trying to intercept normal port, doing nothing...")
    elif len(attrs) != 1:
        print("No clue what you're trying to intercept...doing nothing...")
    else:
        cr_attr = attrs[0]
        cr_attr.set_intercepted(True)
    return generator.ports[port]


def observe_cfg(generator, port, other_gen, cfg_reg_port):
    int_gen = other_gen.internal_generator
    actual_port = int_gen.get_port(cfg_reg_port)
    # Remove the attribute
    attrs = actual_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
    if len(attrs) == 0:
        print("Trying to intercept normal port, doing nothing...")
    elif len(attrs) != 1:
        print("No clue what you're trying to intercept...doing nothing...")
    else:
        cr_attr = attrs[0]
        cr_attr.add_observer(generator, port)


def shift_reg(generator, signal, chain_depth, name=None):
    '''Creates a shift register of depth `chain_depth` and returns the output of it
    '''
    name_use = signal.name
    if name is not None:
        name_use = name
    #  Check for packed
    packed_sig = signal.is_packed
    to_use = signal
    for i in range(chain_depth):
        to_use = register(generator, to_use, name=f"{name_use}_d{i + 1}", packed=packed_sig)
    return to_use


def register(generator, signal, enable=kts.const(1, 1), clear=kts.const(0, 1),
             name=None, packed=False, reset_value=0):
    ''' Pass a generator and a signal to create a registered
        version of any signal easily.
    '''
    use_name = signal.name + "_d1"
    if name is not None:
        use_name = name
    reg = generator.var(use_name, signal.width, size=signal.size, packed=packed)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def reg_code():
        if ~generator._rst_n:
            reg = reset_value
        elif clear:
            reg = 0
        elif enable:
            reg = signal

    generator.add_code(reg_code)
    return reg


def sticky_flag(generator, signal, clear=kts.const(0, 1), name=None, seq_only=False, verbose=False):
    ''' Create a signal that indicates whether a signal is high
        or has been high in the past
    '''
    use_name = signal.name
    if name is not None:
        use_name = name
    reg = generator.var(f"{use_name}_was_high", signal.width)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def reg_code():
        if ~generator._rst_n:
            reg = 0
        elif clear:
            reg = 0
        elif signal:
            reg = 1
    generator.add_code(reg_code)

    sticky = generator.var(f"{use_name}_sticky", 1)
    if seq_only:
        if verbose:
            print(f"Using seq only...")
        generator.wire(sticky, reg)
    else:
        generator.wire(sticky, signal | reg)
    return sticky


# Add a simple counter to a design and return the signal
def add_counter(generator, name, bitwidth, increment=kts.const(1, 1), clear=None, pos_reset=False):

    ctr = generator.var(name, bitwidth, packed=True)
    if pos_reset is True:
        if clear is not None:
            @always_ff((posedge, "clk"), (posedge, "rst_n"))
            def ctr_inc_clr_code():
                if generator._rst_n:
                    ctr = 0
                elif clear:
                    ctr = 0
                elif increment:
                    ctr = ctr + 1
            generator.add_code(ctr_inc_clr_code)
        else:
            @always_ff((posedge, "clk"), (posedge, "rst_n"))
            def ctr_inc_code():
                if generator._rst_n:
                    ctr = 0
                elif increment:
                    ctr = ctr + 1
            generator.add_code(ctr_inc_code)
    else:
        if clear is not None:
            @always_ff((posedge, "clk"), (negedge, "rst_n"))
            def ctr_inc_clr_code():
                if ~generator._rst_n:
                    ctr = 0
                elif clear:
                    ctr = 0
                elif increment:
                    ctr = ctr + 1
            generator.add_code(ctr_inc_clr_code)
        else:
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


def sum_bits(generator, signal, name):

    bits_size = kts.clog2(signal.width)
    bits_sum = generator.var(f"{name}_sum", bits_size + 1)
    # bits_sum_done = generator.var(f"{name}_sum_done", 1)

    @always_comb
    def sum_bits_comb():
        bits_sum = 0
        # bits_sum_done = 0
        for i in range(signal.width):
            bits_sum = bits_sum + signal[i]
    generator.add_code(sum_bits_comb)

    return bits_sum


def process_line(item):
    item = item.strip()
    item_nobrack = item.rstrip("]").lstrip("[")
    individ = item_nobrack.split(" ")
    # print(f"individ: {individ}")
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


def round_up_to_power_of_2(x):
    if x < 1:
        return 1
    return 1 << (x - 1).bit_length()


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


def connect_memoryport_storage(generator: kts.Generator, mptype: MemoryPortType = None,
                               memport_intf=None, strg_intf=None):
    if mptype == MemoryPortType.R:
        signals = ['addr',
                   'read_data',
                   'read_en']
    elif mptype == MemoryPortType.W:
        signals = ['addr',
                   'write_data',
                   'write_en']
    elif mptype == MemoryPortType.RW:
        signals = ['addr',
                   'read_data',
                   'write_data',
                   'read_en',
                   'write_en']
    else:
        raise NotImplementedError

    for signal in signals:
        # generator.wire(memport_intf[signal], strg_intf[signal])
        # self._final_gen.wire(memport_intf[signal], strg_intf[signal])
        generator.wire(memport_intf[signal], strg_intf[signal])


def inline_multiplexer(generator, name, sel, one, many, one_hot_sel=True):

    many_list = False
    if type(many) is list:
        assert len(many) > 0
        many_list = True
        if len(many) == 1:
            generator.wire(one, many[0])
            return

    # print(sel)
    # print(many)
    # assert type(many) is list
    # assert len(many) > 0

    mux_gen = kts.Generator(name=name)
    mux_width = one.width

    mux_gen_sel_in = None
    if one_hot_sel:
        mux_gen_sel_in = mux_gen.input("sel", len(many))
    else:
        mux_gen_sel_in = mux_gen.input("sel", sel.width)
    mux_gen_one_out = mux_gen.output('mux_gen_one_out', mux_width)
    # if many_list:
    mux_gen_many_in = [mux_gen.input(f"many_in_{i}", mux_width) for i in range(len(many))]
    # else:
    # mux_gen_many_in = mux_gen.input(f"many_in", )

    # Do a scan through and pick the lowest one (priority)
    if one_hot_sel:
        print("Building one-hot mux...")
        tmp_done = mux_gen.var("tmp_done", 1)
        len_sel = mux_gen_sel_in.width

        @always_comb
        def set_outs():
            tmp_done = 0
            mux_gen_one_out = 0
            # Iterate through the bits of the signal, find the first one that's high
            for i in range(len_sel):
                if ~tmp_done:
                    if mux_gen_sel_in[i]:
                        mux_gen_one_out = mux_gen_many_in[i]
                        tmp_done = 1

        mux_gen.add_code(set_outs)
    else:

        print("Building non-one-hot mux...")
        # Non-one-hot (normal sel lines)
        tmp_done = mux_gen.var("tmp_done", 1)
        # len_sel = len(sel)

        # For this, assume the bitwidth of sel is large enough
        # to support many, could assert this
        len_many = len(many)
        assert sel.width >= kts.clog2(len_many)

        @always_comb
        def set_outs():
            tmp_done = 0
            mux_gen_one_out = 0
            # Iterate through the bits of the signal, find the first one that's high
            for i in range(len_many):
                if ~tmp_done:
                    if mux_gen_sel_in == i:
                        mux_gen_one_out = mux_gen_many_in[i]
                        tmp_done = 1

        mux_gen.add_code(set_outs)

    # Now instantiate the generator, add the child, and hook it up
    generator.add_child(f"{name}_inst", mux_gen)
    generator.wire(one, mux_gen_one_out)
    if one_hot_sel:
        for i_ in range(len(many)):
            generator.wire(many[i_], mux_gen_many_in[i_])
            generator.wire(sel[i_], mux_gen_sel_in[i_])
    else:
        for i_ in range(len(many)):
            generator.wire(many[i_], mux_gen_many_in[i_])
        generator.wire(sel, mux_gen_sel_in)


def generate_affine_sequence(dimensionality, extents, strides, offset):

    seq_size = 1
    for i in range(dimensionality):
        seq_size = seq_size * extents[i]
    final_seq = [0 for i in range(seq_size)]
    it_domain = [0 for i in range(dimensionality)]
    # done = False
    for i in range(seq_size):
        # Add the new value
        new_val = offset
        for j in range(dimensionality):
            new_val += it_domain[j] * strides[j]
        final_seq[i] = new_val
        # Update the iterators
        for j in range(dimensionality):
            it_domain[j] += 1
            # if it is equal to the extent, set it back to 0 and
            # keep going up the chain
            if it_domain[j] == extents[j]:
                it_domain[j] = 0
            # Otherwise we are done
            else:
                break

    return final_seq


def calculate_read_out_vec(schedule, vec=4, mem_depth=2048):
    '''Handle vectorized - three phases
    '''
    # Phase one - calculate all inputs to SIPO
    # Get all input ports
    # in_ports = {port_num: port_sched for port_num, port_sched in schedule.items() if port_sched['type'] == Direction.IN}

    in_ports = {}
    for port_num, port_sched in schedule.items():
        # Need to ignore vec constraints as well
        if not isinstance(port_num, int):
            continue
        if port_sched['type'] == Direction.IN:
            in_ports[port_num] = port_sched
    # quit()
    # Go through the ports and calculate their SIPO output/schedules
    sipo_outs = {}
    sipo_outs_data = {}

    # Simply generate a new app and use other function to get the outputs...
    for pnum, port_sched in in_ports.items():
        sipo_outs[pnum] = {'time': [],
                           'data': []}

        vec_in_config = port_sched['vec_in_config']
        vec_out_config = port_sched['vec_out_config']

        sub_test = {
            0: {
                'type': Direction.IN,
                'name': f"port_{pnum}_sub_in",
                'config': vec_in_config
            },
            1: {
                'type': Direction.OUT,
                'name': f"port_{pnum}_sub_out",
                'config': vec_out_config
            }

        }
        sub_in_test_out = calculate_read_out(sub_test, vec=(1, vec), sanitize=False)
        sipo_outs[pnum] = sub_in_test_out[1]
        sipo_outs_data[pnum] = sub_in_test_out[1]['data']

    # Now that we have these, we just need to feed the normal schedule, except we need
    # to send this data instead
    mid_result = calculate_read_out(schedule=schedule, vec=(vec, vec), data_in=sipo_outs_data, sanitize=False, mem_depth=mem_depth)
    mid_result_data = {}
    for pnum, info in mid_result.items():
        mid_result_data[pnum] = info['data']

    # Time for the last part
    # out_ports = {port_num: port_sched for port_num, port_sched in schedule.items() if port_sched['type'] == Direction.OUT}
    out_ports = {}
    for port_num, port_sched in schedule.items():
        # Need to ignore vec constraints as well
        if not isinstance(port_num, int):
            continue
        if port_sched['type'] == Direction.OUT:
            out_ports[port_num] = port_sched
    piso_outs = {}
    piso_outs_data = {}

    for pnum, port_sched in out_ports.items():
        piso_outs[pnum] = {'time': [],
                           'data': []}

        vec_in_config = port_sched['vec_in_config']
        vec_out_config = port_sched['vec_out_config']
        sub_test = {
            0: {
                'type': Direction.IN,
                'name': f"port_{pnum}_sub_in",
                'config': vec_in_config
            },
            1: {
                'type': Direction.OUT,
                'name': f"port_{pnum}_sub_out",
                'config': vec_out_config
            }
        }

        send_data = {0: mid_result_data[pnum]}

        sub_in_test_out = calculate_read_out(sub_test, vec=(vec, 1),
                                             data_in=send_data)
        piso_outs[pnum] = sub_in_test_out[1]
        piso_outs_data[pnum] = sub_in_test_out[1]['data']

    return piso_outs


def calculate_read_out(schedule, vec=(1, 1), data_in=None, sanitize=True, mem_depth=2048):
    '''Use this function to create a list of
        (data, timestamp)
    '''
    # Start with two port
    memory = [0 for i in range(mem_depth)]

    data_ins = {}
    data_outs = {}
    all_sequences = {}

    vec_in, vec_out = vec

    for port_num, port_sched in schedule.items():

        # Ignore the dynamic portion for these purposes...
        if not isinstance(port_sched, dict):
            continue

        config = port_sched['config']

        port_addr_seq = generate_affine_sequence(dimensionality=config['dimensionality'],
                                                 extents=config['extents'],
                                                 strides=config['address']['strides'],
                                                 offset=config['address']['offset'])

        port_sched_seq = generate_affine_sequence(dimensionality=config['dimensionality'],
                                                  extents=config['extents'],
                                                  strides=config['schedule']['strides'],
                                                  offset=config['schedule']['offset'])

        if port_sched['type'] == Direction.IN:
            # check if data in is None first...
            if data_in is None:
                data_size_ = get_data_sizes_alone(port_sched)
                # data_ins[port_num] = iter([list(range(z_ * vec_in, z_ * vec_in + vec_in)) for z_ in range(2048 // vec_in)])
                data_ins[port_num] = iter([list(range(z_ * vec_in, z_ * vec_in + vec_in)) for z_ in range(data_size_ // vec_in)])
            else:
                data_ins[port_num] = iter(data_in[port_num])
        else:
            data_outs[port_num] = {'time': [],
                                   'data': []}

        # Use iters so we can just call next very easily
        all_sequences[port_num] = {'addr': iter(port_addr_seq),
                                   'sched': iter(port_sched_seq)}

    # Now we have the sequences of all ports
    # Just need to find the smallest current schedule sequence value, advance
    # the time step to then, and then perform all actions at the timestep
    timestep = -1
    curr_seqs = {}
    seqs_done = {}
    for pnum, seqs in all_sequences.items():
        curr_seqs[pnum] = (next(seqs['sched']), next(seqs['addr']))
        seqs_done[pnum] = False
    more_events = True
    while more_events:
        new_timestep = None
        # Get smallest timestep
        for pnum, seqs in curr_seqs.items():
            if seqs_done[pnum] is True:
                continue
            sched, addr = seqs
            if new_timestep is None or sched < new_timestep:
                new_timestep = sched
        assert new_timestep > timestep
        timestep = new_timestep
        # Now perform all actions at the timestep
        new_curr_seqs = {}
        for pnum, seqs in curr_seqs.items():
            if seqs_done[pnum] is True:
                continue
            sched, addr = seqs
            # If this should happen, perform either read or write
            this_seqs = all_sequences[pnum]
            if sched == timestep:
                # print(f"Triggered timestep match on {pnum} with sched {sched}")
                if schedule[pnum]['type'] == Direction.IN:
                    data_to_write = next(data_ins[pnum])
                    for z_ in range(vec_in):
                        memory[addr * vec_in + z_] = data_to_write[z_]
                else:
                    # data_outs[pnum].append((sched, memory[addr]))
                    data_outs[pnum]['time'].append(sched)
                    # data_outs[pnum]['data'].append(memory[addr])
                    new_data = []
                    for z_ in range(vec_out):
                        new_data.append(memory[addr * vec_out + z_])
                    # print(f"TIMESTEP: {timestep}\tADDR:{addr}\tDATA:{new_data}")
                    data_outs[pnum]['data'].append(new_data)
                    # quit()
                try:
                    new_curr_seqs[pnum] = (next(this_seqs['sched']), next(this_seqs['addr']))
                except StopIteration:
                    seqs_done[pnum] = True
            else:
                new_curr_seqs[pnum] = seqs
        curr_seqs = new_curr_seqs
        # Now calcualte if we are done based on all dones
        more_events = False
        for p_, sd_ in seqs_done.items():
            if sd_ is False:
                more_events = True

    if sanitize is True:
        assert vec_out == 1, f"vec_out is not 1, cannot sanitize"
        for pnum, info in data_outs.items():
            data_outs[pnum]['data'] = [sl[0] for sl in data_outs[pnum]['data']]

    return data_outs


def get_data_sizes_alone(schedule: dict = None):
    # A schedule will have a bunch of ports - can always analyze the extens of the config to
    # get the total sizes
    assert schedule is not None

    use_port_schedule = schedule['config']

    # Get direction
    if schedule['type'] == Direction.IN:
        if 'vec_in_config' in schedule:
            use_port_schedule = schedule['vec_in_config']
    else:
        if 'vec_out_config' in schedule:
            use_port_schedule = schedule['vec_out_config']

    dim_ = use_port_schedule['dimensionality']
    extents = use_port_schedule['extents']
    num_data = 1
    for i_ in range(dim_):
        # Now have full extent data, add it to map
        num_data = num_data * extents[i_]
    return num_data


def get_data_sizes(schedule: dict = None, num_ports=2):
    # A schedule will have a bunch of ports - can always analyze the extens of the config to
    # get the total sizes
    assert schedule is not None
    div = num_ports // 2

    # sizes_map = {}
    sizes_map = []
    for port_num, port_schedule in schedule.items():

        if type(port_num) is not int:
            continue

        use_port_schedule = port_schedule['config']

        if port_num < div:
            new_port_num = port_num
            port_plus_arg = f"w{new_port_num}_num_data"
            if 'vec_in_config' in port_schedule:
                use_port_schedule = port_schedule['vec_in_config']
        else:
            new_port_num = port_num - div
            port_plus_arg = f"r{new_port_num}_num_data"
            if 'vec_out_config' in port_schedule:
                use_port_schedule = port_schedule['vec_out_config']

        dim_ = use_port_schedule['dimensionality']
        extents = use_port_schedule['extents']
        num_data = 1
        for i_ in range(dim_):
            num_data = num_data * extents[i_]
        # Now have full extent data, add it to map
        sizes_map.append((port_plus_arg, num_data))
    return sizes_map


def read_dump_sw(sw):
    sw_list = None
    with open(sw, 'r') as sw_file:
        sw_list = sw_file.readlines()
    sw_list_final = []
    for line in sw_list:
        sw_list_final.append(int(line.strip()))
    return sw_list_final


def read_dump_hw(hw, hex=True):
    hw_list_pre = None
    hw_list_final = []
    with open(hw, 'r') as hw_file:
        hw_list_pre = hw_file.readlines()
    # Now we need to trim stuff with x or X
    # and write the hex into integers
    for line in hw_list_pre:
        line_strip = line.strip()
        if 'x' in line_strip or 'X' in line_strip:
            break
        hw_list_final.append(int(line_strip, base=16))
    return hw_list_final


def verify_gold(dir, mflowgen=False):
    # Read in both...
    outdir = os.path.join(dir, "outputs")
    if mflowgen is True:
        golddir = os.path.join(dir, "inputs", "gold")
    else:
        golddir = os.path.join(dir, "gold")
    indir = os.path.join(dir, "inputs")

    static = False
    # Check if this is a static or dynamic test
    # by checking the PARGS
    pargs_file = os.path.join(indir, "PARGS.txt")
    pargs_lines = None
    with open(pargs_file, 'r') as pargs_file_handle:
        pargs_lines = pargs_file_handle.readlines()
    for line_ in pargs_lines:
        if 'static=1' in line_:
            static = True

    print(f"Test is static: {static}")

    # Iterate through the files in the gold dir
    for filename in os.listdir(golddir):
        # Get both version and compare them...
        if 'time' in filename and static is False:
            continue
        sw_path = os.path.join(golddir, filename)
        hw_path = os.path.join(outdir, filename)
        sw_version = read_dump_sw(sw_path)
        hw_version = read_dump_hw(hw_path)
        if len(sw_version) != len(hw_version):
            return False
        for i in range(len(sw_version)):
            if sw_version[i] != hw_version[i]:
                return False
    return True


if __name__ == "__main__":
    increment_csv("sequence.csv", "inced_csv.csv", [])
