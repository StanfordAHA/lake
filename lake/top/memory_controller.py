from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.top.memory_interface import MemoryPort, MemoryPortExclusionAttr
from lake.attributes.config_reg_attr import ConfigRegAttr
import kratos as kts
from kratos.generator import PortDirection, InitialCodeBlock
import _kratos
import math


class MemoryController(kts.Generator):
    '''
    Provides the utilities to interface a memory controller with a memory interface
    '''
    MAXIMUM_CONFIG_REG = 32
    OPCODE_WIDTH = 2
    OPCODE_BASE = 8
    OPCODE_BT = (OPCODE_BASE + OPCODE_WIDTH - 1, OPCODE_BASE)
    STOP_CODE = 0
    STOP_BT = (OPCODE_BASE - 1, 0)
    DONE_CODE = 1
    MAYBE_CODE = 2
    EOS_BIT = 16
    DONE_PROXY = kts.concat(kts.const(1, 1),
                            kts.const(0, EOS_BIT - (OPCODE_WIDTH + OPCODE_BASE)),
                            kts.const(DONE_CODE, OPCODE_WIDTH),
                            kts.const(0, OPCODE_BASE))
    STOP0_PROXY = kts.concat(kts.const(1, 1),
                             kts.const(0, EOS_BIT - (OPCODE_WIDTH + OPCODE_BASE)),
                             kts.const(STOP_CODE, OPCODE_WIDTH),
                             kts.const(0, OPCODE_BASE))

    def __init__(self, name: str,
                 debug: bool = False,
                 is_clone: bool = False,
                 internal_generator=None,
                 exclusive: bool = False,
                 add_flush=False):
        super().__init__(name, debug, is_clone, internal_generator)
        self.exclusive = exclusive
        self.num_perf_ctrs = 0
        self.add_flush = add_flush

    def get_exclusive(self):
        return self.exclusive

    def add_flush_pass(self):
        self.add_attribute("sync-reset=flush")
        kts.passes.auto_insert_sync_reset(self.internal_generator)
        flush_port = self.internal_generator.get_port("flush")
        flush_port.add_attribute(ControlSignalAttr(True))

    def set_bit(self, old_val, bit_to_set, new_bit):
        new_val = old_val | (new_bit << bit_to_set)
        return new_val

    def get_bit(self, val, n):
        return (val >> n & 1)

    def chop_config(self, config_base):
        config = []
        # Go through all of the original configs...
        for config_tuple in config_base:
            config_name, config_value = config_tuple
            cfg_port_width = self.get_port(config_name).width
            # Chop it if it's too wide...
            if cfg_port_width > self.MAXIMUM_CONFIG_REG:
                # The usual suspects - just grab each bit and set it sequentially
                num_regs = math.ceil(cfg_port_width / self.MAXIMUM_CONFIG_REG)
                remaining_bits = cfg_port_width
                for idx_ in range(num_regs):
                    if remaining_bits > self.MAXIMUM_CONFIG_REG:
                        use_bits = self.MAXIMUM_CONFIG_REG
                    else:
                        use_bits = remaining_bits
                    chopped_val = 0
                    for i_ in range(use_bits):
                        cfg_val_bit = self.get_bit(config_value, idx_ * self.MAXIMUM_CONFIG_REG + i_)
                        chopped_val = self.set_bit(chopped_val, i_, cfg_val_bit)
                    config += [(f"{config_name}_{idx_}", chopped_val)]
                    remaining_bits -= use_bits
            else:
                config += [(f"{config_name}", chopped_val)]

        return config

    def get_child_generator(self, port):
        if port.generator == self.internal_generator:
            return self
        else:
            children = self.child_generator()
            for child_name, child_gen in children.items():
                local_result = self.__get_child_generator_helper(child_gen, port)
                if local_result is not None:
                    return local_result
            return None

    def __get_child_generator_helper(self, generator, port):
        if port.generator == generator.internal_generator:
            return generator
        else:
            children = generator.child_generator()
            for child_name, child_gen in children.items():
                local_result = self.__get_child_generator_helper(child_gen, port)
                if local_result is not None:
                    return local_result
            return None

    def get_child_generator_gen(self, generator):
        if generator == self.internal_generator:
            return self
        else:
            children = self.child_generator()
            for child_name, child_gen in children.items():
                local_result = self.__get_child_generator_gen_helper(child_gen, generator)
                if local_result is not None:
                    return local_result
            return None

    def __get_child_generator_gen_helper(self, search_gen, generator):
        if generator == search_gen.internal_generator:
            return search_gen
        else:
            children = search_gen.child_generator()
            for child_name, child_gen in children.items():
                local_result = self.__get_child_generator_gen_helper(child_gen, generator)
                if local_result is not None:
                    return local_result
            return None

    def add_performance_indicator(self, signal, edge='posedge', label='start', cycle_count=None):

        assert cycle_count is not None

        val_ = f'val_{self.num_perf_ctrs}'

        self.add_stmt(kts.RawStringStmt(f'logic {val_};').stmt())

        # Create inital block...
        ib = self.initial()

        if edge == 'posedge':
            raw_text_posedge = f'// benign\n{val_} = 0;\n@(posedge flush)\nwhile({val_} == 0) begin\n@(posedge clk);\n{val_} = {signal.name};\nend\n$display(\"%m_{label}_%d\", {cycle_count.name});\n'
        else:
            raw_text_posedge = ''

        raw_stmt = kts.RawStringStmt(raw_text_posedge)
        ib.add_stmt(raw_stmt)

        self.num_perf_ctrs += 1

    def get_port(self, name):
        int_gen = self.internal_generator
        ret_port = int_gen.get_port(name)
        return ret_port

    def get_port_attributes(self, port_name, attr_type):
        int_gen = self.internal_generator
        ret_port = int_gen.get_port(port_name)
        # attrs = ret_port.find_attribute(lambda: isinstance(a, ConfigRegAttr))
        attrs = ret_port.find_attribute(lambda a: isinstance(a, attr_type))
        return attrs

    def get_inputs(self):
        '''
        Use this method to return pure inputs to the tile interface
        '''
        ins = []
        int_gen = self.internal_generator
        # Now get the config registers from the top definition
        for port_name in int_gen.get_port_names():
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
            memport_excl_attr = curr_port.find_attribute(lambda a: isinstance(a, MemoryPortExclusionAttr))
            port_dir = PortDirection(curr_port.port_direction)
            if len(attrs) > 0 or len(memport_excl_attr) > 0 or "clk" in curr_port.name or "rst_n" in curr_port.name:
                continue
            if port_dir == PortDirection.Out:
                continue
            ins.append((curr_port, curr_port.width))
        return ins

    def get_outputs(self):
        '''
        Use this method to return pure outputs to the tile interface
        '''
        outs = []
        int_gen = self.internal_generator
        # Now get the config registers from the top definition
        for port_name in int_gen.get_port_names():
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
            memport_excl_attr = curr_port.find_attribute(lambda a: isinstance(a, MemoryPortExclusionAttr))
            port_dir = PortDirection(curr_port.port_direction)
            if len(attrs) > 0 or len(memport_excl_attr) > 0 or "clk" in curr_port.name or "rst_n" in curr_port.name:
                continue
            if port_dir == PortDirection.In:
                continue
            outs.append((curr_port, curr_port.width))
        return outs

    def get_liftable_ports(self):
        '''
        Use this method to return all other ports that can be safely lifted
        '''
        liftable = []
        int_gen = self.internal_generator
        # Now get the config registers from the top definition
        for port_name in int_gen.get_port_names():
            curr_port = int_gen.get_port(port_name)
            attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
            memport_excl_attr = curr_port.find_attribute(lambda a: isinstance(a, MemoryPortExclusionAttr))
            port_dir = PortDirection(curr_port.port_direction)
            if len(memport_excl_attr) > 0:
                continue
            if "clk" in curr_port.name or "rst_n" in curr_port.name:
                liftable.append(curr_port)
        return liftable

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        raise NotImplementedError

    def get_bitstream(self, config_json, prefix=""):
        '''
        Pass in a config-related json to return a list of
        (config_reg, value) tuples
        '''
        raise NotImplementedError

    def get_config_mode_str(self):
        '''
        Use this function to define a unique name for the controller type
        - useful when referring through the CoreIR JSON
        '''
        raise NotImplementedError

    def get_dedicated_clock(self):
        return False

    def __get_other_fifos_helper(self, gen):

        children = gen.child_generator()
        for child_name, child_gen in children.items():
            if "RegFIFO" in str(type(child_gen)):
                if child_gen.get_hardware_genned() is False:
                    self.__alt_fifos.append(child_gen)
            else:
                self.__get_other_fifos_helper(child_gen)

    def get_other_fifos(self):

        self.__alt_fifos = []
        children = self.child_generator()
        for child_name, child_gen in children.items():
            if "RegFIFO" in str(type(child_gen)):
                if child_gen.get_hardware_genned() is False:
                    self.__alt_fifos.append(child_gen)
            else:
                self.__get_other_fifos_helper(child_gen)

        return self.__alt_fifos

    def get_fifos(self):
        '''
        Get the shareable fifos from a generator, even nested submodules
        - descend the hierarachy to find them
        '''

        # TODO: Handle multibit push signals

        self.__fifo_list = []
        # First get ports
        self_ports = self.internal_generator.get_port_names()
        # just track the valids
        valids_in = [self.get_port(port) for port in self_ports if "_valid" in port and "In" in str(self.get_port(port).port_direction)]
        valids_out = [self.get_port(port) for port in self_ports if "_valid" in port and "Out" in str(self.get_port(port).port_direction)]

        max_tries = 1000

        for valid_in in valids_in:
            sinks = valid_in.sinks
            # Only use direct connections...
            if len(sinks) > 1:
                continue
            hit_fifo = False
            use_gen = self

            atg = self
            tries = 0

            for actual_sink in sinks:
                pass
            while hit_fifo is False:

                if tries == max_tries:
                    break

                assigned_to = actual_sink.left
                assigned_to_gen = assigned_to.generator
                if assigned_to_gen.instance_name != use_gen.instance_name:
                    atg = use_gen[assigned_to_gen.instance_name]
                # Make sure we are connecting to a signal in a new generator
                if atg == use_gen:
                    for actual_sink in assigned_to.sinks:
                        pass
                    use_gen = atg
                elif "RegFIFO" in str(type(atg)):
                    rfifo_attr = assigned_to_gen.get_attributes()
                    for f_attr in rfifo_attr:
                        str_f_attr = str(type(f_attr))
                        if 'SharedFifoAttr' in str_f_attr:
                            self.__fifo_list.append(atg)
                            hit_fifo = True
                    else:
                        break
                else:
                    for actual_sink in assigned_to.sinks:
                        pass
                    use_gen = atg

                tries += 1

                # Now we have the generator,

        atg = None

        for valid_out in valids_out:
            sources = valid_out.sources
            # Only use direct connections...
            if len(sources) > 1:
                continue
            hit_fifo = False
            use_gen = self

            atg = self
            tries = 0

            for actual_src in sources:
                pass
            while hit_fifo is False:

                # Give a failure if we get through 1000 wires...
                if tries == max_tries:
                    break

                assigned_to = actual_src.right
                assigned_to_gen = assigned_to.generator
                if assigned_to_gen.instance_name != use_gen.instance_name:
                    atg = use_gen[assigned_to_gen.instance_name]
                # Make sure we are connecting to a signal in a new generator
                if atg == use_gen:
                    for actual_src in assigned_to.sources:
                        pass
                    use_gen = atg
                elif "RegFIFO" in str(type(atg)):
                    rfifo_attr = assigned_to_gen.get_attributes()
                    for f_attr in rfifo_attr:
                        str_f_attr = str(type(f_attr))
                        if 'SharedFifoAttr' in str_f_attr:
                            self.__fifo_list.append(atg)
                            hit_fifo = True
                    else:
                        break
                else:
                    for actual_src in assigned_to.sources:
                        pass
                    use_gen = atg

                tries += 1

        return self.__fifo_list

    def __str__(self):
        return self.name


class MemoryControllerFlatWrapper(MemoryController):
    '''
    This class exists to take in a memory controller and flatten any
    inputs and outputs, while preserving config registers and memory ports
    '''
    def __init__(self, mem_ctrl: MemoryController, legal_list=[1, 16], add_rv=True):
        super().__init__(f"{str(mem_ctrl)}_flat", debug=True)
        self.mem_ctrl = mem_ctrl
        self.add_child(f"{mem_ctrl}_inst", mem_ctrl)
        self.legal_widths = legal_list
        self.add_rv = add_rv
        # Sort the legal widths in descending order...
        self.legal_widths.sort(reverse=True)
        self.lift_ports()
        self.flatten_inputs()
        self.flatten_outputs()
        self.lift_memory_ports()

    def lift_ports(self):
        liftable_ports = self.mem_ctrl.get_liftable_ports()
        for port in liftable_ports:
            pname = port.name
            if pname == "clk":
                tmp_clk = self.clock("clk")
                self.wire(tmp_clk, port)
            elif pname == "rst_n":
                tmp_rst = self.reset("rst_n")
                self.wire(tmp_rst, port)
            elif pname == "clk_en":
                tmp_clk_en = self.clock_en("clk_en")
                self.wire(tmp_clk_en, port)
            else:
                # Need to get the attributes and copy them up...
                port_attrs = port.attributes
                tmp_port = self.port_from_def(port, name=f"{port.name}_f")
                for attr in port_attrs:
                    tmp_port.add_attribute(attr)
                self.wire(tmp_port, port)

    def flatten_port(self, port, in_outn=True, name="", suffix=""):
        '''
        Flatten a port and bring it up based on the width/size of the port
        '''
        port_width = port.width
        if port.name == "flush":
            return
        # Port width either has to be in the legal_list or
        # 1 must be in the list - if width is 1, then the size[0] should be 1 as well
        # if port_width in self.legal_widths or (1 in self.legal_widths and port.size[0] == 1):
        if port_width in self.legal_widths or 1 in self.legal_widths:
            # Now we can handle it
            port_dim = len(port.size)
            # This is a single port - check for widest compatibility
            if port.size[0] == 1 and port_dim == 1:
                # Already sorted in descending order, so we will find the largest compatible...
                for legal_width in self.legal_widths:
                    # Too big - keep looking
                    if legal_width > port_width:
                        continue
                    # Correct size - copy it
                    elif legal_width == port_width:
                        if in_outn:
                            if port_width != 1:
                                # If it is not packed, wire it into a 1D packed deal
                                if not port.is_packed:
                                    intercept = self.var(f"{name}_f_{suffix}_intercept", port_width, explicit_array=True, packed=True)
                                    self.wire(intercept[0], port)
                                    port = intercept
                                tmp_port = self.input(f"{name}_f_{suffix}", port_width, explicit_array=True, packed=True)
                            else:
                                tmp_port = self.input(f"{name}_f_{suffix}", port_width)
                        else:
                            if port_width != 1:
                                if not port.is_packed:
                                    intercept = self.var(f"{name}_f_{suffix}_intercept", port_width, explicit_array=True, packed=True)
                                    self.wire(intercept[0], port)
                                    port = intercept
                                tmp_port = self.output(f"{name}_f_{suffix}", port_width, explicit_array=True, packed=True)
                            else:
                                tmp_port = self.output(f"{name}_f_{suffix}", port_width)
                        self.wire(tmp_port, port)
                        # Copy attributes
                        self.copy_attributes(copy_to=tmp_port, copy_from=port)
                        break
                    # Still a mismatch
                    elif legal_width < port_width and legal_width != 1:
                        continue
                    # Legal width of 1 is guaranteed to be here if there wasn't a direct port width match
                    elif legal_width < port_width and legal_width == 1:
                        for bit in range(port_width):
                            alt_suffix = suffix
                            if len(alt_suffix) > 0:
                                alt_suffix += "_"
                            if in_outn:
                                tmp_bit = self.input(f"{name}_f_{alt_suffix}b_{bit}", 1)
                            else:
                                tmp_bit = self.output(f"{name}_f_{alt_suffix}b_{bit}", 1)
                            self.wire(tmp_bit, port[bit])
                            # Copy attributes
                            self.copy_attributes(copy_to=tmp_bit, copy_from=port)
                        break
            # This is a 2D signal
            elif port.size[0] > 1 and port_dim == 1:
                # Flatten to a bunch of different ports recursively
                for i in range(port.size[0]):
                    self.flatten_port(port[i], in_outn=in_outn, name=port.name, suffix=f"{i}")
            # For now, make it illegal to flatten 3D ports...since we don't really use them
            else:
                print("CANNOT FLATTEN MORE THAN 2D SIGNALS")
                raise NotImplementedError
        else:
            print(f"Cannot flatten this port...{port.name}, {port.width}, {port.size}")

    def copy_attributes(self, copy_to, copy_from):
        for attr in copy_from.attributes:
            copy_to.add_attribute(attr)

    def flatten_inputs(self):
        child_ins = self.mem_ctrl.get_inputs()
        for (inp, width) in child_ins:
            self.flatten_port(inp, in_outn=True, name=inp.name)

    def flatten_outputs(self):
        child_outs = self.mem_ctrl.get_outputs()
        for (outp, width) in child_outs:
            self.flatten_port(outp, in_outn=False, name=outp.name)

    def lift_memory_port(self, mem_prt: MemoryPort, verbose=False):
        new_mem_prt = MemoryPort(mem_prt.get_port_type(), mem_prt.get_port_delay(), mem_prt.get_active_read())
        new_mem_prt_intf = new_mem_prt.get_port_interface()
        # These interfaces should match directly...
        for (name, sig) in mem_prt.get_port_interface().items():
            # These should all comply anyway, so just need to recreate them
            try:
                if sig is None:
                    continue
                # Check if slice and lift the parent
                if isinstance(sig, _kratos.VarSlice):
                    sig = sig.parent_var
                # Lift the port up directly...
                if sig.generator.parent_generator() != self.internal_generator:
                    # Make a list of lifting
                    lifting_gens_int = [sig.generator.parent_generator()]
                    tmp_gen = sig.generator.parent_generator().parent_generator()
                    while tmp_gen != self.internal_generator:
                        lifting_gens_int.append(tmp_gen)
                        tmp_gen = tmp_gen.parent_generator()
                    # Lift through the hierarchy first...
                    for tmp_gen_ in lifting_gens_int:
                        real_tmp_gen = self.get_child_generator_gen(tmp_gen_)
                        # tmp_lifted = tmp_gen_.port(sig, f"{sig}_lifted", False)
                        tmp_lifted = real_tmp_gen.port_from_def(sig, name=f"{sig.generator.instance_name}_{sig}_lifted")
                        real_tmp_gen.wire(sig, tmp_lifted)
                        for attr in sig.attributes:
                            tmp_lifted.add_attribute(attr)
                        sig = tmp_lifted

                lifted_port = self.port_from_def(sig, name=f"{sig.generator.instance_name}_{sig}_lifted")
                new_mem_prt_intf[name] = lifted_port
                self.wire(sig, lifted_port)
                for attr in sig.attributes:
                    lifted_port.add_attribute(attr)
            except kts.VarException:
                if verbose:
                    print("Port already exists...copying into MemoryPort")
                new_mem_prt_intf['read_addr'] = new_mem_prt_intf['write_addr']
        return new_mem_prt

    def handle_duplicate_address(self):
        pass

    def lift_memory_ports(self):
        child_mem_ports = self.mem_ctrl.get_memory_ports()
        # Get a deep copy so we have the correct dimensions
        self.mem_ports = child_mem_ports.copy()
        for bank in range(len(child_mem_ports)):
            for port in range(len(child_mem_ports[0])):
                if child_mem_ports[bank][port] is not None:
                    self.mem_ports[bank][port] = self.lift_memory_port(child_mem_ports[bank][port])

    def get_memory_ports(self):
        return self.mem_ports

    def get_dedicated_clock(self):
        return self.mem_ctrl.get_dedicated_clock()
