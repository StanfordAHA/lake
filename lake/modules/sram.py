from kratos import *
from math import log
from lake.modules.sram_stub import SRAMStub
from lake.modules.sram_stub_generator import SRAMStubGenerator
from lake.utils.flattenND import FlattenND
from lake.utils.reverse_flatten import ReverseFlatten


class SRAM(Generator):

    def __init__(self,
                 use_sram_stub,
                 sram_name,
                 data_width,
                 fw_int,
                 mem_depth,
                 mem_input_ports,
                 mem_output_ports,
                 address_width,
                 bank_num,
                 num_tiles):
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
        self.num_tiles = num_tiles

        self.chain_idx_bits = max(1, clog2(num_tiles))

        self._gclk = self.clock("clk")

        self._clk_en = self.input("clk_en", 1)

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
        if num_tiles == 1:
            self._mem_addr_to_sram = self.var("mem_addr_to_sram",
                                              self.address_width)
        else:
            self._mem_addr_to_sram = self.var("mem_addr_to_sram",
                                              self.address_width - self.chain_idx_bits)

        self._mem_cen_in_bank = self.input("mem_cen_in_bank", self.mem_output_ports)

        self._mem_wen_in_bank = self.input("mem_wen_in_bank", self.mem_input_ports)

        self._wtsel = self.input("wtsel", 2)
        self._rtsel = self.input("rtsel", 2)

        if self.use_sram_stub:
            mbank = SRAMStub(data_width=self.data_width,
                             width_mult=self.fw_int,
                             depth=self.mem_depth)

            self.add_child(f"mem_{self.bank_num}", mbank,
                           clk=self._gclk,
                           data_in=self._mem_data_in_bank,
                           addr=self._mem_addr_to_sram,
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

            compose_wide = True

            if self.fw_int > 1:
                flatten_data_in = FlattenND(self.data_width,
                                            self.fw_int,
                                            self.mem_input_ports)

                self.add_child(f"flatten_data_in_{self.bank_num}",
                               flatten_data_in,
                               input_array=self._mem_data_in_bank,
                               output_array=self._sram_mem_data_in_bank)

                if compose_wide:
                    for j in range(2):
                        mbank = SRAMStubGenerator(sram_name=self.sram_name,
                                                  data_width=self.data_width,
                                                  width_mult=self.fw_int // 2,
                                                  depth=self.mem_depth)

                        self.add_child(f"mem_inst_{self.bank_num}_pt_{j}",
                                       mbank,
                                       sram_addr=self._mem_addr_to_sram,
                                       sram_cen=~self._mem_cen_in_bank,
                                       sram_clk=self._gclk,
                                       sram_data_in=self._sram_mem_data_in_bank[j * 32 + 32 - 1, j * 32],
                                       sram_data_out=self._sram_mem_data_out_bank[j * 32 + 32 - 1, j * 32],
                                       sram_wen=~self._mem_wen_in_bank,
                                       sram_wtsel=self._wtsel,
                                       sram_rtsel=self._rtsel)

                else:
                    self.add_child(f"mem_inst_{self.bank_num}",
                                   mbank,
                                   sram_addr=self._mem_addr_to_sram,
                                   sram_cen=~self._mem_cen_in_bank,
                                   sram_clk=self._gclk,
                                   sram_data_in=self._sram_mem_data_in_bank,
                                   sram_data_out=self._sram_mem_data_out_bank,
                                   sram_wen=~self._mem_wen_in_bank,
                                   sram_wtsel=self._wtsel,
                                   sram_rtsel=self._rtsel)

                flatten_data_out = ReverseFlatten(self.data_width,
                                                  self.fw_int,
                                                  self.mem_output_ports)

                self.add_child(f"flatten_data_out_{self.bank_num}",
                               flatten_data_out,
                               input_array=self._sram_mem_data_out_bank,
                               output_array=self._mem_data_out_bank)

            else:
                self.add_child(f"mem_inst_{self.bank_num}",
                               mbank,
                               sram_addr=self._mem_addr_to_sram,
                               sram_cen=~self._mem_cen_in_bank,
                               sram_clk=self._gclk,
                               sram_data_in=self._mem_data_in_bank,
                               sram_data_out=self._mem_data_out_bank,
                               sram_wen=~self._mem_wen_in_bank,
                               sram_wtsel=self._wtsel,
                               sram_rtsel=self._rtsel)

        self.add_code(self.set_mem_addr)

    @always_comb
    def set_mem_addr(self):
        # these ranges are inclusive
        if self.num_tiles == 1:
            self._mem_addr_to_sram = self._mem_addr_in_bank
        else:
            self._mem_addr_to_sram = self._mem_addr_in_bank[self.address_width - self.chain_idx_bits - 1, 0]


if __name__ == "__main__":
    dut = SRAM(use_sram_stub=False,
               sram_name="TSMC",
               data_width=16,
               fw_int=4,
               mem_depth=128,
               mem_input_ports=1,
               mem_output_ports=1,
               address_width=7,
               bank_num=4,
               num_tiles=1)
    verilog(dut, filename="wrapper.sv")
