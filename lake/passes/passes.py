import kratos
from kratos import Attribute, verilog
from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
import _kratos


# this is a pass
def lift_config_reg(generator):
    class ConfigRegLiftVisitor(IRVisitor):
        def __init__(self):
            IRVisitor.__init__(self)
            #self.tag = tag

        def visit(self, node):
            if isinstance(node, _kratos.Generator):
                ports_ = node.get_port_names()
                for port_name in ports_:
                    curr_port = node.get_port(port_name)
                    if(len(curr_port.get_attributes()) <= 0):
                        continue
                    else:
                        cr_attr = None
                        for i in range(len(curr_port.get_attributes())):
                            if(isinstance(curr_port.get_attributes()[i].get(), ConfigRegAttr)):
                                cr_attr = curr_port.get_attributes()[i].get()
                        if cr_attr:
                            # need to wire it to the instances parent and up
                            gen = node
                            parent_gen = gen.parent_generator()
                            child_port = curr_port
                            child_gen = gen
                            while parent_gen is not None:
                                # create a port based on the target's definition
                                new_name = child_gen.instance_name + "_" + child_port.name
                                p = parent_gen.port(child_port, new_name)
                                parent_gen.wire(child_port, p)
                                # move up the hierarchy
                                child_port = p
                                child_gen = parent_gen
                                parent_gen = parent_gen.parent_generator()

                            child_port.add_attribute(ConfigRegAttr())

    v = ConfigRegLiftVisitor()
    v.visit_root(generator)
