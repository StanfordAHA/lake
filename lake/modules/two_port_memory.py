from kratos import *
from lake.modules.sram_stub import SRAMStub
from lake.modules.two_port_sram_stub import TwoPortSRAMStub
from lake.modules.pipe_reg import PipeReg
from lake.modules.virtual_remap_table import VirtualRemapTable
from math import *


class TwoPortMemory(Generator):
    def __init__(self,
                 macro_depth,
                 macro_width,
                 desired_depth,
                 desired_width,
                 two_port):

        super().__init__("two_port_memory")

        # PORT DEFS: begin
        self._clk = self.clock("i_clk")
        self._rst_n = self.reset("i_rst_n")

        self.full_addr = clog2(desired_depth)
        self.addr_width_macro = clog2(macro_depth)

        self._rd_addr = self.input("i_rd_addr", self.full_addr)
        self._rd_data = self.output("o_rd_data", desired_width)
        self._ren = self.input("i_ren", 1)

        self._wr_addr = self.input("i_wr_addr", self.full_addr)
        self._wr_data = self.input("i_wr_data", desired_width)
        self._wen = self.input("i_wen", 1)
        # PORT DEFS: end

        banks_wide = int(desired_width / macro_width)
        banks_tall = int(desired_depth / macro_depth)
        print(f"Generating memory with {banks_wide} banks
              for width and {banks_tall} banks for capacity")

        # LOCAL SIGNALS: begin
        # This is the output of all banks (tallwise)
        self._memory_space = self.var("memory_space",
                                      width=desired_width,
                                      size=banks_tall,
                                      packed=True,
                                      explicit_array=True)
        # Need to reg the rd addr to know how to select the data on the next cycle
        self._bank_sel = self.var("bank_sel", max(self.full_addr - self.addr_width_macro, 1))
        self._bank_sel_reg = self.var("bank_sel_reg",
                                      max(self.full_addr - self.addr_width_macro, 1))
        # LOCAL_SIGNALS: end

        # GENERATION LOGIC: begin

        # If already a two-port, there is only one cycle of latency
        latency = 0

        # Either the difference in address widths or 1 bit if there's no difference

        if(banks_tall > 1):
            self.wire(self._bank_sel, self._rd_addr[self.full_addr - 1, self.addr_width_macro])
        else:
            self.wire(self._bank_sel, 0)
        # Update the bank sel reg
        self.add_code(self.always_bank_sel_reg)

        if two_port:
            self.instantiate_true_two_port(banks_tall, banks_wide, macro_width, macro_depth)
        else:
            # typically you want to break it up into more banks
            # Based on the capacity in words vs macro capacity
            latency = self.build_two_port(banks_tall, banks_wide, macro_width, macro_depth)

        pipe_reg_rd_data = PipeReg(desired_width, latency)

        # GENERATION LOGIC: end
        # still want passthru_block for read+write to same location ?

    def instantiate_true_two_port(self, banks_tall, banks_wide, macro_width, macro_depth):
        '''
        Hook up the set of two port memories
        '''

        self._sub_rd_addr = self.var("rd_addr_macro", self.addr_width_macro)
        self._sub_wr_addr = self.var("wr_addr_macro", self.addr_width_macro)
        self.wire(self._sub_rd_addr, self._rd_addr[self.addr_width_macro - 1, 0])
        self.wire(self._sub_wr_addr, self._wr_addr[self.addr_width_macro - 1, 0])

        # Hook up the grid of SRAM banks
        for i in range(banks_tall):
            for j in range(banks_wide):
                self.add_child(f"sram_d{i}_w{j}", TwoPortSRAMStub(macro_width, macro_depth, 0))

                self.wire(self[f"sram_d{i}_w{j}"].ports.i_clk, self._clk)
                self.wire(self[f"sram_d{i}_w{j}"].ports.i_rst_n, self._rst_n)

                self.wire(self[f"sram_d{i}_w{j}"].ports.i_rd_addr, self._sub_rd_addr)
                self.wire(self[f"sram_d{i}_w{j}"].ports.i_wr_addr, self._sub_wr_addr)
                if(banks_tall > 1):
                    self.wire(self[f"sram_d{i}_w{j}"].ports.i_wen,
                              self._wen & (self._wr_addr[self.full_addr - 1,
                                                         self.addr_width_macro] == i))
                    self.wire(self[f"sram_d{i}_w{j}"].ports.i_ren,
                              self._ren & (self._rd_addr[self.full_addr - 1,
                                                         self.addr_width_macro] == i))
                else:
                    self.wire(self[f"sram_d{i}_w{j}"].ports.i_wen, self._wen)
                    self.wire(self[f"sram_d{i}_w{j}"].ports.i_ren, self._ren)

                # Send the data across horizontally based on j and the macro width
                self.wire(self[f"sram_d{i}_w{j}"].ports.i_data,
                          self._wr_data[((j + 1) * macro_width) - 1, j * macro_width])

        for i in range(banks_tall):
            if(banks_wide > 1):
                self.wire(self._memory_space[i],
                          concat(*[self[f"sram_d{i}_w{j}"].ports.o_data
                                 for j in range(banks_wide - 1, -1, -1)]))
            else:
                self.wire(self._memory_space, self[f"sram_d{i}_w{j}"].ports.o_data)

        if(banks_tall > 1):
            self.wire(self._rd_data, self._memory_space[self._bank_sel_reg])
        else:
            self.wire(self._rd_data, self._memory_space)

        return 0

    def build_two_port(self, banks_tall, banks_wide, macro_width, macro_depth):
        '''
        Takes a single port memory and constructs a
        virtualized two port memory from a single port memory
        '''
        # We need one extra bank to create 1R1W
        logical_banks = banks_tall + 1

        # Create all logical banks as a bank array
        for i in range(logical_banks):
            for j in range(banks_wide):
                self.add_child(f"sram_d{i}_w{j}", SRAMStub(macro_width, macro_depth))

        return 5

    @always((posedge, "i_clk"), (negedge, "i_rst_n"))
    def always_bank_sel_reg(self):
        if ~self._rst_n:
            self._bank_sel_reg = 0
        elif self._ren:
            self._bank_sel_reg = self._bank_sel


if __name__ == "__main__":
    tpm_dut = TwoPortMemory(macro_depth=128,
                            macro_width=16,
                            desired_depth=256,
                            desired_width=32,
                            two_port=1)
    verilog(tpm_dut, filename="two_port_memory.sv", check_active_high=False)
