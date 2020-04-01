import kratos
from kratos import Attribute, verilog
from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.sram_port_attr import SRAMPortAttr
import _kratos


# this is a pass
def lift_config_reg(generator):
    class ConfigRegLiftVisitor(IRVisitor):
        def __init__(self):
            IRVisitor.__init__(self)

        def visit(self, node):
            if isinstance(node, _kratos.Generator):
                ports_ = node.get_port_names()
                for port_name in ports_:
                    curr_port = node.get_port(port_name)
                    if(len(curr_port.get_attributes()) <= 0):
                        continue
                    else:
                        cr_attr = None
                        doc = None
                        for i in range(len(curr_port.get_attributes())):
                            if(isinstance(curr_port.get_attributes()[i].get(), ConfigRegAttr)):
                                cr_attr = curr_port.get_attributes()[i].get()
                        if cr_attr:
                            doc = cr_attr.get_documentation()
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

                            child_port_cra = ConfigRegAttr()
                            child_port_cra.set_documentation(doc)
                            child_port.add_attribute(child_port_cra)

    v = ConfigRegLiftVisitor()
    v.visit_root(generator)


# Inputs:
#   - use_sram_stub: if stub is not being used, we are using external provided
#       sram macro and should replace port names accordingly
#   - sram_macro_info: information about sram macro, including port names to
#       be replaced
def change_sram_port_names(use_sram_stub, sram_macro_info):

    def change_sram_port_names_wrapper(generator):

        class SRAMPortNames(IRVisitor):
            def __init__(self, use_sram_stub, ports):
                IRVisitor.__init__(self)
                self.use_sram_stub = use_sram_stub
                self.sram_macro_info = sram_macro_info

            def visit(self, node):
                if isinstance(node, Port):
                    if not(len(node.get_attributes()) <= 0 or self.use_sram_stub):
                        for i in range(len(node.get_attributes())):
                            if (isinstance(node.get_attributes()[i].get(), SRAMPortAttr)):
                                if node.name == "sram_addr":
                                    node.name = self.sram_macro_info.addr_port_name
                                elif node.name == "sram_cen":
                                    node.name = self.sram_macro_info.ce_port_name
                                elif node.name == "sram_clk":
                                    node.name = self.sram_macro_info.clk_port_name
                                elif node.name == "sram_data_in":
                                    node.name = self.sram_macro_info.data_in_port_name
                                elif node.name == "sram_data_out":
                                    node.name = self.sram_macro_info.data_out_port_name
                                elif node.name == "sram_wen":
                                    node.name = self.sram_macro_info.wen_port_name
                                elif node.name == "sram_wtsel":
                                    node.name = self.sram_macro_info.wtsel_port_name
                                elif node.name == "sram_rtsel":
                                    node.name = self.sram_macro_info.rtsel_port_name

        v = SRAMPortNames(use_sram_stub, sram_macro_info)
        v.visit_root(generator)

    return change_sram_port_names_wrapper
