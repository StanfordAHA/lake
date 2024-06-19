import kratos
import random as rand
from lake.attributes.config_reg_attr import ConfigRegAttr
from kratos import PortDirection


class Component(kratos.Generator):

    component_num = 0

    """
        General component for a Lake specification...
    """
    def __init__(self, name=None):
        """
            Create component
        """
        use_name = f"{self.__class__.__name__}_{Component.component_num}"
        super().__init__(name=use_name, debug=True)
        Component.component_num += 1
        # super().__init__(name=f"{rand.randint(0, 10000)}", debug=True)
        self.config_space = []
        self.config_size = 0
        self.cfg_reg_to_range = {}
        # A live configuration consists of a list of ranges and value
        self.configuration = []
        self.remap_flatten_config = {}

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

    def configure(self, cfg_reg, value):
        '''Do the actual configuration of the config reg
        '''
        # Can actually pass in the var or the name - if it's not a string I will make it a string
        if type(cfg_reg) is not str:
            cfg_reg = cfg_reg.name

        # Now use the string to locate the map and add the value to the current configuration
        print(self.cfg_reg_to_range)
        # If it's not in the map, check if it was flattened and renamed...
        if cfg_reg not in self.cfg_reg_to_range:
            # If not in here, check if it was flattened and handed a list, else bad
            if cfg_reg in self.remap_flatten_config and type(value) is list:
                # for i_, remapped_name in enumerate(self.remap_flatten_config[cfg_reg]):
                # for i_, remapped_name in enumerate(self.remap_flatten_config[cfg_reg]):
                for i_ in range(len(value)):
                    range_ = self.cfg_reg_to_range[self.remap_flatten_config[cfg_reg][i_]]
                    self.configuration.append((range_, value[i_]))
            else:
                raise NotImplementedError
        else:
            range_ = self.cfg_reg_to_range[cfg_reg]
            self.configuration.append((range_, value))

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

            lcl_var = self.var(**kwargs)
            ret_ = []
            del kwargs['size']
            del kwargs['name']
            for i_ in range(size_):
                kwargs['name'] = f"{name_}_{i_}"
                self.remap_flatten_config[save_unflat_name].append(kwargs['name'])
                tmp = self.input(**kwargs)
                ret_.append(tmp)
                self.config_space.append((total_config_size_, tmp))
                self.wire(tmp, lcl_var[i_])
                # Map the name to this range
                self.cfg_reg_to_range[kwargs['name']] = (self.config_size + total_config_size_, self.config_size)
                self.config_size += total_config_size_
            ret_ = lcl_var

        else:
            ret_ = self.input(**kwargs)
            self.config_space.append((total_config_size_, ret_))
            self.cfg_reg_to_range[kwargs['name']] = (self.config_size + total_config_size_, self.config_size)
            self.config_size += total_config_size_

        # ret_.add_attribute(ConfigRegAttr("Default Description"))

        # return lcl_var
        return ret_

    def get_cfg_map(self):
        return self.cfg_reg_to_range

    def gen_bitstream(self):
        raise NotImplementedError

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
