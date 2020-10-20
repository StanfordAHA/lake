import kratos
from kratos import Attribute, verilog
from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.sram_port_attr import SRAMPortAttr
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
import _kratos


# this is a pass
def lift_config_reg(generator):
    # if not hasattr(generator, "lifted"):
    #     setattr(generator, "lifted", False)
    # if generator.lifted:
    #     print("WARNING: LAKE PASSES: LIFT_CONFIG_REGS: Configuration registers have already been lifted...")
    #     print("WARNING: LAKE PASSES: LIFT_CONFIG_REGS: Exiting pass early...")
    #     return
    class ConfigRegLiftVisitor(IRVisitor):
        def __init__(self):
            IRVisitor.__init__(self)

        def visit(self, node):
            if isinstance(node, _kratos.Generator):
                ports_ = node.get_port_names()
                for port_name in ports_:
                    curr_port = node.get_port(port_name)
                    attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
                    annotation_attr = curr_port.find_attribute(lambda a: isinstance(a, FormalAttr))
                    if port_name is "mode":
                        print("Found mode...")
                        print(attrs)
                    if len(attrs) != 1:
                        continue

                    cr_attr = attrs[0]
                    doc = cr_attr.get_documentation()
                    # need to wire it to the instances parent and up
                    gen = node
                    parent_gen = gen.parent_generator()
                    child_port = curr_port
                    child_gen = gen
                    top_lvl_cfg = parent_gen is None
                    while parent_gen is not None:
                        # create a port based on the target's definition
                        new_name = child_gen.instance_name + "_" + child_port.name
                        p = parent_gen.port(child_port, new_name, False)
                        parent_gen.wire(child_port, p)
                        # move up the hierarchy
                        child_port = p
                        child_gen = parent_gen
                        parent_gen = parent_gen.parent_generator()
                    # Only add the attribute if this is a newly created port, not a top-level cfg reg
                    if top_lvl_cfg is False:
                        child_port_cra = ConfigRegAttr()
                        child_port_cra.set_documentation(doc)
                        child_port.add_attribute(child_port_cra)
                        if annotation_attr:
                            ann_att = annotation_attr[0]
                            annot_type = ann_att.get_formal_ann()
                            child_port.add_attribute(FormalAttr(f"{child_port}", annot_type))

    v = ConfigRegLiftVisitor()
    v.visit_root(generator)
    # Make sure we don't lift twice
    # generator.lifted = True


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
                if self.use_sram_stub:
                    return
                if isinstance(node, Port):
                    attrs = node.find_attribute(lambda a: isinstance(a, SRAMPortAttr))
                    if len(attrs) != 1:
                        return
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
