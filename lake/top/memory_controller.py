from lake.top.memory_interface import MemoryPortExclusionAttr
from lake.attributes.config_reg_attr import ConfigRegAttr
import kratos as kts
from kratos.generator import PortDirection


class MemoryController(kts.Generator):
    '''
    Provides the utilities to interface a memory controller with a memory interface
    '''
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
            if len(attrs) > 0 or len(memport_excl_attr) > 0 or "clk" in curr_port.name or "rst_n" in curr_port.name:
                liftable.append(curr_port)
        return liftable

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        raise NotImplementedError

    def __str__(self):
        return self.name


class MemoryControllerFlatWrapper(MemoryController):
    '''
    This class exists to take in a memory controller and flatten any
    inputs and outputs, while preserving config registers and memory ports
    '''
    def __init__(self, mem_ctrl: MemoryController, legal_list=[16, 1]):
        super().__init__(f"{str(mem_ctrl)}_flat", debug=True)
        self.mem_ctrl = mem_ctrl
        self.add_child(f"{mem_ctrl}_inst", mem_ctrl)
        self.legal_widths = legal_list
        self.lift_ports()
        self.flatten_inputs()
        self.flatten_outputs()

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
            else:
                # Need to get the attributes and copy them up...
                port_attrs = port.attributes
                tmp_port = self.port_from_def(port, name=f"{port.name}_f")
                for attr in port_attrs:
                    tmp_port.add_attribute(attr)
                self.wire(tmp_port, port)

    def flatten_port(self):
        pass

    def flatten_inputs(self):
        child_ins = self.mem_ctrl.get_inputs()
        for (inp, width) in child_ins:
            print(f"{inp}, {width}, {inp.size}")
            self.flatten_port(inp)

    def flatten_outputs(self):
        child_outs = self.mem_ctrl.get_outputs()
        for (outp, width) in child_outs:
            print(f"{outp}, {width}, {outp.size}")
            self.flatten_port(outp)

    def get_memory_ports(self):
        return self.mem_ctrl.get_memory_ports()
