from kratos import *
from math import log
from lake.modules.sram_stub import SRAMStub
from lake.modules.sram_stub_generator import SRAMStubGenerator
from utils.flattenND import FlattenND
from utils.reverse_flatten import ReverseFlatten


# instantiate either sram stub or stub for external provided sram macro
class SRAMWrapper(Generator):

    def __init__(self,
                 use_sram_stub,
                 sram_name,
                 data_width,
                 fw_int,
                 mem_depth,
                 mem_input_ports,
                 mem_output_ports,
                 address_width,
                 bank_num):
        super().__init__(f"{sram_name}_generator")

        self.use_sram_stub = use_sram_stub
        self.sram_name = sram_name
        self.data_width = data_width
        self.fw_int = fw_int
        self.mem_depth = mem_depth
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.address_width = address_width
        self.bank_num = bank_num

        self._gclk = self.clock("clk")

        if self.fw_int > 1:
            self._mem_data_in_bank = self.input("mem_data_in_bank",
                                                self.data_width,
                                                size=self.fw_int,
                                                packed=True,
                                                explicit_array=True)

            self._mem_data_out_bank = self.output("mem_data_out_bank",
                                                  self.data_width,
                                                  size=(self.mem_output_ports,
                                                        self.fw_int),
                                                  packed=True,
                                                  explicit_array=True)

            if not self.use_sram_stub:
                # flattened version of input data
                self._sram_mem_data_in_bank = self.var("sram_mem_data_in_bank",
                                                       self.data_width *
                                                       self.fw_int *
                                                       self.mem_input_ports)

                # flattened version of output data
                self._sram_mem_data_out_bank = self.var("sram_mem_data_out_bank",
                                                        self.data_width *
                                                        self.fw_int *
                                                        self.mem_output_ports)
        else:
            self._mem_data_in_bank = self.input("mem_data_in_bank", self.data_width, packed=True)

            self._mem_data_out_bank = self.output("mem_data_out_bank", self.data_width, packed=True)

        self._mem_addr_in_bank = self.input("mem_addr_in_bank",
                                            self.address_width)

        self._mem_cen_in_bank = self.input("mem_cen_in_bank", self.mem_output_ports)

        self._mem_wen_in_bank = self.input("mem_wen_in_bank", self.mem_input_ports)

        if self.use_sram_stub:
            mbank = SRAMStub(data_width=self.data_width,
                             width_mult=self.fw_int,
                             depth=self.mem_depth)

            self.add_child(f"mem_{self.bank_num}", mbank,
                           clk=self._gclk,
                           data_in=self._mem_data_in_bank,
                           addr=self._mem_addr_in_bank,
                           cen=self._mem_cen_in_bank,
                           wen=self._mem_wen_in_bank,
                           data_out=self._mem_data_out_bank)

        # instantiante external provided sram macro and flatten input/output data
        # if fetch width is greater than 1
        else:

            mbank = SRAMStubGenerator(sram_name=self.sram_name,
                                      data_width=self.data_width,
                                      width_mult=self.fw_int,
                                      depth=self.mem_depth)

            if self.fw_int > 1:
                flatten_data_in = FlattenND(self.data_width,
                                            self.fw_int,
                                            self.mem_input_ports)

                self.add_child(f"flatten_data_in_{self.bank_num}",
                               flatten_data_in,
                               input_array=self._mem_data_in_bank,
                               output_array=self._sram_mem_data_in_bank)

                self.add_child(f"mem_{self.bank_num}",
                               mbank,
                               sram_addr=self._mem_addr_in_bank,
                               sram_cen=~self._mem_cen_in_bank,
                               sram_clk=self._gclk,
                               sram_data_in=self._sram_mem_data_in_bank,
                               sram_data_out=self._sram_mem_data_out_bank,
                               sram_wen=self._mem_wen_in_bank)

                flatten_data_out = ReverseFlatten(self.data_width,
                                                  self.fw_int,
                                                  self.mem_output_ports)

                self.add_child(f"flatten_data_out_{self.bank_num}",
                               flatten_data_out,
                               input_array=self._sram_mem_data_out_bank,
                               output_array=self._mem_data_out_bank)

            else:
                self.add_child(f"mem_{self.bank_num}",
                               mbank,
                               sram_addr=self._mem_addr_in_bank,
                               sram_cen=~self._mem_cen_in_bank,
                               sram_clk=self._gclk,
                               sram_data_in=self._mem_data_in_bank,
                               sram_data_out=self._mem_data_out_bank,
                               sram_wen=self._mem_wen_in_bank)


if __name__ == "__main__":
    dut = SRAMWrapper(0, "TSMC", 16, 4, 128, 1, 1, 7, 4)
    verilog(dut, filename="wrapper.sv")
