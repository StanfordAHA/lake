import kratos as kts
import _kratos
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.passes.passes import lift_config_reg, change_sram_port_names
from lake.top.lake_top import LakeTop
from lake.utils.sram_macro import SRAMMacroInfo


def extract_top_config(circuit_gen: kts.Generator):
    int_gen = circuit_gen.internal_generator
    config_list = []
    # Now get the config registers from the top definition
    for port_name in int_gen.get_port_names():
        curr_port = int_gen.get_port(port_name)
        attrs = curr_port.find_attribute(lambda a: isinstance(a, ConfigRegAttr))
        if len(attrs) != 1:
            continue
        cr_attr = attrs[0]
        config_list.append((port_name, curr_port.size, curr_port.width, curr_port.explicit_array))
        for i in range(len(curr_port.size)):
            curr_port.size[i] -= 1
    return config_list


def get_interface(circuit_gen: kts.Generator):
    intf_sigs = []
    int_gen = circuit_gen.internal_generator
    # Now get the config registers from the top definition
    for port_name in int_gen.get_port_names():
        curr_port = int_gen.get_port(port_name)
        attrs = curr_port.find_attribute(lambda a: isinstance(a, ControlSignalAttr))
        if len(attrs) != 1:
            continue
        cr_attr = attrs[0]
        # Now we have this
        intf_sigs.append((port_name, curr_port.size, curr_port.width,
                          cr_attr.get_control(), str(curr_port.port_direction),
                          curr_port.explicit_array))
    return intf_sigs


if __name__ == "__main__":
    tsmc_info = SRAMMacroInfo("tsmc_name")
    use_sram_stub = False
    fifo_mode = True
    mem_width = 64
    lake_dut = LakeTop(mem_width=mem_width,
                       sram_macro_info=tsmc_info,
                       use_sram_stub=use_sram_stub,
                       fifo_mode=fifo_mode,
                       add_clk_enable=True,
                       add_flush=True)
    sram_port_pass = change_sram_port_names(use_sram_stub=use_sram_stub, sram_macro_info=tsmc_info)
    # Perform pass to move config_reg
    extract_top_config(lake_dut)
    # get_interface(lake_dut)
