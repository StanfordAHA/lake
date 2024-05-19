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
            lcl_var = self.var(**kwargs)
            ret_ = []
            del kwargs['size']
            del kwargs['name']
            for i_ in range(size_):
                kwargs['name'] = f"{name_}_{i_}"
                tmp = self.input(**kwargs)
                ret_.append(tmp)
                self.config_size += total_config_size_
                self.config_space.append((total_config_size_, tmp))
                self.wire(tmp, lcl_var[i_])
            ret_ = lcl_var

        else:
            ret_ = self.input(**kwargs)
            self.config_size += total_config_size_
            self.config_space.append((total_config_size_, ret_))

        # ret_.add_attribute(ConfigRegAttr("Default Description"))

        # return lcl_var
        return ret_

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
