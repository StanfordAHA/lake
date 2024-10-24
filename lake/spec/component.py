import kratos
import kratos as kts
import random as rand
from lake.attributes.config_reg_attr import ConfigRegAttr
from kratos import PortDirection, always_ff, posedge, negedge
from lake.modules.ready_valid_interface import RVInterface
from lake.spec.config_memory import ConfigMemory
from lake.utils.spec_enum import Direction


class Component(kratos.Generator):

    component_num = 0

    """
        General component for a Lake specification...
    """
    def __init__(self, name=None):
        """
            Create component
        """
        if name is None:
            use_name = f"{self.__class__.__name__}_{Component.component_num}"
        else:
            use_name = name
        super().__init__(name=use_name, debug=True)
        Component.component_num += 1
        # super().__init__(name=f"{rand.randint(0, 10000)}", debug=True)
        self.config_space = []
        self.config_size = 0
        self.cfg_reg_to_range = {}
        # A live configuration consists of a list of ranges and value
        self.configuration = []
        self.remap_flatten_config = {}
        self.child_cfg_bases = None
        self.config_space_fixed = False
        self._config_memory = None
        self._config_memory_harden = None
        self._config_memory_harden_en = None
        self._name_to_var_cfg = {}
        self._clk = self.clock('clk')
        self._rst_n = self.clock('rst_n')

    def get_clock(self):
        return self._clk

    def get_reset(self):
        return self._rst_n

    def get_config_space_fixed(self):
        return self.config_space_fixed

    def rvinput(self, **kwargs):
        '''This utility allows a designer to add rv signals to an interface but still using the same
            interface as kratos input/output
        '''
        new_rv = RVInterface(self, direction=Direction.IN, **kwargs)
        return new_rv

    def rvoutput(self, **kwargs):
        '''This utility allows a designer to add rv signals to an interface but still using the same
            interface as kratos input/output
        '''
        new_rv = RVInterface(self, direction=Direction.OUT, **kwargs)
        return new_rv

    def populate_child_cfg_bases(self):
        pass

    def _assemble_cfg_memory_input(self, harden_storage=False, clkgate=False):
        # So this should be called at the end of a gen_hardware.
        # this will set the config_space_fixed as well
        # Calling this means the hardware consutrction for this Component and any children is
        # complete
        # We can consider assembling it with the physical, hardened registers (in the spec or wherever)
        print(f"Making config space for {self.name}")
        if self.config_size == 0:
            # Early out in case there is no configuration
            return
        self._config_memory = self.input("config_memory", self.config_size, packed=True)
        use_wire = self._config_memory

        if harden_storage:

            harden_config_mem = ConfigMemory(width=self.config_size)
            self._config_memory_harden = self.var("config_memory_harden", self.config_size, packed=True)
            self._config_memory_harden_en = self.input("config_memory_wen", 1)

            if clkgate:
                # Clock gate here to the memory...
                print("Gating config memory clock...")
                config_mem_clk = self.var("config_mem_clk", 1)
                self._config_mem_clk = kts.util.clock(config_mem_clk)
                self.wire(config_mem_clk, kts.util.clock(self._clk & self._config_memory_harden_en))

            else:
                print("Not gating config memory clock...")
                self._config_mem_clk = self._clk

            # Do normal add child...
            super().add_child(instance_name="config_memory_instance", generator=harden_config_mem,
                              clk=self._config_mem_clk,
                              rst_n=self._rst_n,
                              flush=kratos.const(0, 1),
                              config_memory_wen=self._config_memory_harden_en,
                              config_memory=self._config_memory,
                              config_memory_out=self._config_memory_harden)

            # Wrap it in a module
            use_wire = self._config_memory_harden

        # Now go through the config space
        for cfg_reg_name, range_tuple in self.cfg_reg_to_range.items():
            range_upper, range_lower = range_tuple
            cfg_reg_signal = self._name_to_var_cfg[cfg_reg_name]
            self.wire(cfg_reg_signal, use_wire[range_upper, range_lower])

    def add_child(self, instance_name, generator, comment="", python_only=False, **kwargs):
        super().add_child(instance_name=instance_name, generator=generator,
                          comment=comment, python_only=python_only, **kwargs)
        if self.child_cfg_bases is None:
            self.child_cfg_bases = {}
        # For now, let's assume if we make it to the add_child phase, the config space is fixed
        # Also, I am going to make it so you can only add components as children to components,
        # to guarantee this additional functionality.
        # assert isinstance(generator, Component)
        if isinstance(generator, Component):
            print("lifting...")
            assert generator.get_config_space_fixed()
            child_size = generator.get_config_size()

            # Now we know the child's config base is fixed here...
            # So we can automatically raise the configuration registers
            self.child_cfg_bases[generator] = self.config_size

            # Escape if there is no config memory in the child...
            if child_size == 0:
                return

            # To lift the space, we don't need to do much, just ask the child for its
            # config size and wire up...
            child_name = generator.name
            temporary_cfg_reg = self.config_reg(name=f"{child_name}_{instance_name}_config_memory", width=child_size)

            self.wire(temporary_cfg_reg, generator.ports.config_memory)
            # lift_config_space(self, generator)

        # self.child_cfg_bases[kwargs['generator']] =

    def _add_base_to_cfg_space(self, cfg_space, base):
        for i_, (range_tuple, value) in enumerate(cfg_space):
            range_upper, range_lower = range_tuple
            cfg_space[i_] = ((range_upper + base, range_lower + base), value)
        return cfg_space

    def gen_hardware(self, pos_reset=False):
        """Generate the kratos hardware for this component

        Parameters
        ----------
        pos_reset : bool, optional
            To use a high rst vs low

        Raises
        ------
        NotImplementedError
            If this function is not overriden in inheriting classes it will throw this error
        """
        raise NotImplementedError

    def get_configuration(self):
        return self.configuration

    def clear_configuration(self):
        self.configuration = []

    def _add_configuration_manual(self, config):
        if type(config) is not list:
            self.configuration.append(config)
        else:
            self.configuration.extend(config)

    def configure(self, cfg_reg, value):
        '''Do the actual configuration of the config reg
        '''
        # Can actually pass in the var or the name - if it's not a string I will make it a string
        if type(cfg_reg) is not str:
            cfg_reg = cfg_reg.name

        # Now use the string to locate the map and add the value to the current configuration
        # If it's not in the map, check if it was flattened and renamed...
        if cfg_reg not in self.cfg_reg_to_range:
            # If not in here, check if it was flattened and handed a list, else bad
            if cfg_reg in self.remap_flatten_config and type(value) is list:
                # for i_, remapped_name in enumerate(self.remap_flatten_config[cfg_reg]):
                # for i_, remapped_name in enumerate(self.remap_flatten_config[cfg_reg]):
                for i_ in range(len(value)):
                    range_ = self.cfg_reg_to_range[self.remap_flatten_config[cfg_reg][i_]]
                    upper, lower = range_
                    diff = upper - lower + 1
                    # trim the value
                    # use_value = int(bin(value[i_])[-1 * diff:], 2)
                    use_value = value[i_]
                    self.configuration.append((range_, use_value))
            else:
                raise NotImplementedError
        else:
            range_ = self.cfg_reg_to_range[cfg_reg]
            upper, lower = range_
            diff = upper - lower + 1
            # trim the value
            # use_value = int(bin(value)[-1 * diff:], 2)
            use_value = value
            self.configuration.append((range_, use_value))

    def config_reg(self, **kwargs):
        assert 'name' in kwargs
        assert 'width' in kwargs
        name_ = kwargs['name']
        width_ = kwargs['width']
        if 'size' not in kwargs:
            size_ = 1
        else:
            size_ = kwargs['size']

        # total_config_size_ = width_ * size_
        total_config_size_ = width_

        # Actually flatten these here if size > 1
        if size_ > 1:

            save_unflat_name = kwargs['name']
            self.remap_flatten_config[kwargs['name']] = []

            # Let this be the actual variable
            lcl_var = self.var(**kwargs)
            ret_ = []
            del kwargs['size']
            del kwargs['name']
            for i_ in range(size_):
                kwargs['name'] = f"{name_}_{i_}"
                self.remap_flatten_config[save_unflat_name].append(kwargs['name'])
                # Don't make it input, just make it a lcl var, wire it up, then we will come through at the
                # end and
                # tmp = self.input(**kwargs)
                # tmp = self.var(**kwargs)
                # ret_.append(tmp)
                self.config_space.append((total_config_size_, lcl_var[i_]))
                # self.config_space.append((total_config_size_, tmp))
                # self.wire(tmp, lcl_var[i_])
                # Map the name to this range
                self.cfg_reg_to_range[kwargs['name']] = (self.config_size + total_config_size_ - 1, self.config_size)
                self._name_to_var_cfg[kwargs['name']] = lcl_var[i_]
                # self._name_to_var_cfg[kwargs['name']] = tmp
                self.config_size += total_config_size_
            ret_ = lcl_var

        else:
            ret_ = self.var(**kwargs)
            # ret_ = self.input(**kwargs)
            self.config_space.append((total_config_size_, ret_))
            self.cfg_reg_to_range[kwargs['name']] = (self.config_size + total_config_size_ - 1, self.config_size)
            self._name_to_var_cfg[kwargs['name']] = ret_
            self.config_size += total_config_size_

        # ret_.add_attribute(ConfigRegAttr("Default Description"))

        # return lcl_var
        return ret_

    def get_cfg_map(self):
        return self.cfg_reg_to_range

    def gen_bitstream(self):
        return self.get_configuration()

    def get_name(self):
        return self.name

    def get_config_space(self):
        return self.config_space

    def get_config_size(self):
        return self.config_size

    # def get_liftable_ports(self):
    #     '''
    #     Use this method to return all other ports that can be safely lifted
    #     '''
    #     liftable = []
    #     int_gen = self.internal_generator
    #     # Now get the config registers from the top definition
    #     for port_name in int_gen.get_port_names():
    #         curr_port = int_gen.get_port(port_name)
    #         attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
    #         memport_excl_attr = curr_port.find_attribute(lambda a: isinstance(a, MemoryPortExclusionAttr))
    #         port_dir = PortDirection(curr_port.port_direction)
    #         if len(memport_excl_attr) > 0:
    #             continue
    #         if "clk" in curr_port.name or "rst_n" in curr_port.name:
    #             liftable.append(curr_port)
    #     return liftable


def lift_config_space(parent_component: Component, child_component: Component):
    # assert child_component in parent_component.internal_generator.children
    # Get the child name
    child_name = child_component.name
    # Get its config space which is (size, name) tuples, already should be
    # flattened at this stage

    # Actually now everything has config_memory at its top, so make a local one
    # and wire it up.

    child_space = child_component.get_config_space()
    for cfg_reg_size, child_cfg_reg_input in child_space:
        cfg_reg_kwargs = {'name': child_cfg_reg_input.name,
                          'width': cfg_reg_size}
        new_cfg_reg = parent_component.config_reg(name=f"{child_name}_{child_cfg_reg_input.name}", width=cfg_reg_size, packed=child_cfg_reg_input.is_packed)
        parent_component.wire(new_cfg_reg, child_cfg_reg_input)
        # tmp = parent_component.confi
        # self.config_space.append((total_config_size_, ret_))
        # self.cfg_reg_to_range[kwargs['name']] = (self.config_size + total_config_size_, self.config_size)
        # self.config_size += total_config_size_
