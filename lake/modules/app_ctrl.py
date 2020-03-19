from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
import kratos as kts


class AppCtrl(Generator):
    '''
    Application Controller.
        This module holds the logic/expression of read/write sequencing at the application level.
        For example, a pure double buffering application needs a deterministic lockstep between sets
        of reads and writes. The constraint imposed on this application prevents the divided memory space
        from being self-conflicting.
        For a line buffer, we only need
    '''
    def __init__(self,
                 interconnect_input_ports,
                 interconnect_output_ports):
        super().__init__("app_ctrl", debug=True)

        self.int_in_ports = interconnect_input_ports
        self.int_out_ports = interconnect_output_ports

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs + Outputs
        # Take in the valid and data and attach an address + direct to a port
        self._wen_in = self.input("wen_in", self.int_in_ports)
        self._ren_in = self.input("ren_in", self.int_out_ports)

        self._tb_valid = self.intput("tb_valid", self.int_out_ports)

        self._valid_out_data = self.output("valid_out_data", self.int_out_ports)
        self._valid_out_stencil = self.output("valid_out_stencil", self.int_out_ports)

        self._wen_en = self.output("wen_en", self.int_in_ports)
        self._ren_en = self.output("ren_en", self.int_out_ports)

        self._write_depth = self.var("write_depth", 32,
                                     size=self.int_in_ports,
                                     explicit_array=True,
                                     packed=True)
        self._write_depth.add_attribute(ConfigRegAttr("Depth of writes"))

        self._read_iters = self.var("read_iters", 32,
                                    size=self.int_out_ports,
                                    explicit_array=True,
                                    packed=True)
        self._read_iters.add_attribute(ConfigRegAttr("Depth of reads"))


if __name__ == "__main__":
    db_dut = AppCtrl(interconnect_input_ports=2,
                     data_width=16,
                     mem_depth=512,
                     banks=4,
                     iterator_support=6,
                     max_port_schedule=64,
                     address_width=16,
                     multiwrite=2)
    verilog(db_dut, filename="input_addr_ctrl.sv",
            additional_passes={"lift config regs": lift_config_reg})
