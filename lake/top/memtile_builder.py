from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_interface import *
from lake.top.memory_controller import *
from lake.attributes.formal_attr import *
import kratos as kts


class MemoryTileBuilder(kts.Generator):
    '''
    This class provides utilities to add memories and memory controllers and automagically generate the hardware
    '''
    def __init__(self, memory_interface: MemoryInterface = None, memory_banks=1, controllers=[]):

        self.memory_interface = memory_interface
        self.memory_banks = memory_banks
        self.controllers = controllers
        self.num_modes = 0

        self.memories = []

        self.controllers_finalized = False

        self.inputs_dict = {}
        self.outputs_dict = {}

        # CLK and RST
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(self._clk.name, FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(self._rst_n.name, FormalSignalConstraint.RSTN))

    def set_banks(self, banks):
        self.memory_banks = banks

    def set_memory_interface(self, name_prefix, mem_params, ports, sim_macro_n, tech_map):
        self.memories = [MemoryInterface(name=f"{name_prefix}_{i}",
                                         mem_params=mem_params,
                                         ports=ports,
                                         sim_macro_n=sim_macro_n,
                                         tech_map=tech_map) for i in range(self.memory_banks)]

    def get_memory_interface(self, mem: MemoryInterface):
        return self.memory_interface

    def finalize_controllers(self):
        self.controllers_finalized = True
        self.num_modes = len(self.controllers)
        # Create the mode config reg if we have multiple controllers,
        # otherwise we can just wire it to 0 and let synth handle it?
        if self.num_modes > 1:
            self._mode = self.input("mode", kts.clog2(self.num_modes))
            self._mode.add_attribute(ConfigRegAttr("MODE!"))

        self.resolve_memports()
        self.resolve_inputs()
        self.resolve_outputs()

    def resolve_memports(self):
        '''
        This function finds the connections between each controller
        and the requested ports
        '''
        pass

    def resolve_inputs(self):
        '''
        This function finds the connections between each controller
        and the requested inputs
        '''
        # Go through each controller and get all the inputs - figure out maximum of each size
        # and indicate in which mode which input goes to which port in the controllers
        # Go through controllers
        for mem_ctrl in self.controllers:
            ctrl_name = str(mem_ctrl)
            ctrl_ins = mem_ctrl.get_inputs()
            # At each input, we can create a dictionary of lists of dictionaries for merging inputs
            for ctrl_in in ctrl_ins:
                in_size = ctrl_in.size
                if in_size not in self.inputs_dict:
                    self.inputs_dict[in_size] = []

    def resolve_outputs(self):
        '''
        This function finds the connections between each controller
        and the requested ports
        '''
        pass

    def add_memory_controller(self, mem_ctrl: MemoryController):
        if not self.controllers_finalized:
            self.controllers += mem_ctrl
        else:
            print(f"Controllers finalized - {mem_ctrl} isn't being added...")

    def add_config_hooks(self, config_data_width=32, config_addr_width=8):
        pass
