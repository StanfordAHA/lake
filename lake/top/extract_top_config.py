import kratos as kts
import _kratos
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.top.lake_top import LakeTop

if __name__ == "__main__":
    lake_dut = LakeTop()
    # Perform pass to move config_reg

    int_gen = lake_dut.internal_generator
    lift_config_reg(int_gen)

    # Now get the config registers from the top definition
    for port_name in int_gen.get_port_names():
        curr_port = int_gen.get_port(port_name)
        if(len(curr_port.get_attributes()) <= 0):
            continue
        else:
            cr_attr = None
            for i in range(len(curr_port.get_attributes())):
                if(isinstance(curr_port.get_attributes()[i].get(), ConfigRegAttr)):
                    cr_attr = curr_port.get_attributes()[i].get()
            if cr_attr:
                for i in range(len(curr_port.size)):
                    curr_port.size[i] -= 1
                print(f"{curr_port.size}[{curr_port.width - 1}:0] {port_name}: " +
                      f"{cr_attr.get_documentation()}")
