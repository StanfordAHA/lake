from kratos import *
from math import log
from lake.modules.sram_stub import SRAMStub
from lake.modules.sram_stub_generator import SRAMStubGenerator
from lake.utils.flattenND import FlattenND
from lake.utils.reverse_flatten import ReverseFlatten


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

        # Chaining related signals
        self._enable_chain_input = self.input("enable_chain_input", 1)
        self._enable_chain_output = self.input("enable_chain_output", 1)

        self._chain_idx_input = self.input("chain_idx_input", self.chain_idx_bits)
        self._chain_idx_output = self.input("chain_idx_output", self.chain_idx_bits)
        self._chain_idx_tile = self.var("chain_idx_tile", self.chain_idx_bits)
        self._valid_data = self.output("valid_data", self.mem_output_ports)

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
        self._mem_cen_in_bank_chain = self.var("mem_cen_in_bank_chain", self.mem_output_ports)

        self._mem_wen_in_bank = self.input("mem_wen_in_bank", self.mem_input_ports)
        self._mem_wen_in_bank_chain = self.var("mem_wen_in_bank_chain", self.mem_input_ports)

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
                           cen=self._mem_cen_in_bank_chain,
                           wen=self._mem_wen_in_bank_chain,
                           data_out=self._mem_data_out_bank)

        # instantiante external provided sram macro and flatten input/output data
        # if fetch width is greater than 1
        else:
            mbank = SRAMStubGenerator(sram_name=self.sram_name,
                                      data_width=self.data_width,
                                      width_mult=self.fw_int,
                                      depth=self.mem_depth)

            compose_wide = False

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
                                       sram_cen=~self._mem_cen_in_bank_chain,
                                       sram_clk=self._gclk,
                                       sram_data_in=self._sram_mem_data_in_bank[j * 32 + 32 - 1, j * 32],
                                       sram_data_out=self._sram_mem_data_out_bank[j * 32 + 32 - 1, j * 32],
                                       sram_wen=~self._mem_wen_in_bank_chain,
                                       sram_wtsel=self._wtsel,
                                       sram_rtsel=self._rtsel)

                else:
                    self.add_child(f"mem_inst_{self.bank_num}",
                                   mbank,
                                   sram_addr=self._mem_addr_to_sram,
                                   sram_cen=~self._mem_cen_in_bank_chain,
                                   sram_clk=self._gclk,
                                   sram_data_in=self._sram_mem_data_in_bank,
                                   sram_data_out=self._sram_mem_data_out_bank,
                                   sram_wen=~self._mem_wen_in_bank_chain,
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
                               sram_cen=~self._mem_cen_in_bank_chain,
                               sram_clk=self._gclk,
                               sram_data_in=self._mem_data_in_bank,
                               sram_data_out=self._mem_data_out_bank,
                               sram_wen=~self._mem_wen_in_bank_chain,
                               sram_wtsel=self._wtsel,
                               sram_rtsel=self._rtsel)

        self.add_code(self.set_chain_idx_tile)
        self.add_code(self.set_mem_addr)
        self.add_code(self.set_chain_wen)
        self.add_code(self.set_chain_cen)
        self.add_code(self.set_valid_data)

    @always_comb
    def set_chain_idx_tile(self):
        # these ranges are inclusive
        if self.num_tiles == 1:
            self._chain_idx_tile = 0
        else:
            self._chain_idx_tile = self._mem_addr_in_bank[self.address_width - 1,
                                                          self.address_width - self.chain_idx_bits]

    @always_comb
    def set_mem_addr(self):
        # these ranges are inclusive
        if num_tiles == 1:
            self._mem_addr_to_sram = self._mem_addr_in_bank
        else:
            self._mem_addr_to_sram = self._mem_addr_in_bank[self.address_width - self.chain_idx_bits - 1, 0]

    @always_comb
    def set_chain_wen(self):
        if (self.num_tiles == 1):
            self._mem_wen_in_bank_chain = self._mem_wen_in_bank
        elif ~self._enable_chain_input:
            self._mem_wen_in_bank_chain = self._mem_wen_in_bank
        # enable chain input
        else:
            # write
            if self._mem_wen_in_bank:
                if self._chain_idx_input == self._chain_idx_tile:
                    self._mem_wen_in_bank_chain = self._mem_wen_in_bank
                else:
                    self._mem_wen_in_bank_chain = 0
            # read
            else:
                self._mem_wen_in_bank_chain = 0

    @always_comb
    def set_chain_cen(self):
        if self.num_tiles == 1:
            self._mem_cen_in_bank_chain = self._mem_cen_in_bank
        # write
        elif self._mem_wen_in_bank:
            if self._enable_chain_input:
                if self._chain_idx_input == self._chain_idx_tile:
                    self._mem_cen_in_bank_chain = self._mem_cen_in_bank
                else:
                    self._mem_cen_in_bank_chain = 0
            else:
                self._mem_cen_in_bank_chain = self._mem_cen_in_bank
        # read
        else:
            if self._enable_chain_output:
                if self._chain_idx_output == self._chain_idx_tile:
                    self._mem_cen_in_bank_chain = self._mem_cen_in_bank
                else:
                    self._mem_cen_in_bank_chain = 0
            else:
                self._mem_cen_in_bank_chain = self._mem_cen_in_bank

    @always_ff((posedge, "clk"))
    def set_valid_data(self):
        for i in range(self.mem_output_ports):
            # read
            if ~self._mem_wen_in_bank:
                if self._enable_chain_output:
                    if self._chain_idx_output == self._chain_idx_tile:
                        self._valid_data[i] = self._mem_cen_in_bank
                    else:
                        self._valid_data[i] = 0
                else:
                    self._valid_data[i] = self._mem_cen_in_bank
            # write
            else:
                self._valid_data[i] = 0


if __name__ == "__main__":
    dut = SRAMWrapper(use_sram_stub=0,
                      sram_name="TSMC",
                      data_width=16,
                      fw_int=4,
                      mem_depth=512,
                      mem_input_ports=1,
                      mem_output_ports=1,
                      address_width=9,
                      bank_num=0,
                      num_tiles=1)
    verilog(dut, filename="wrapper512.sv")
