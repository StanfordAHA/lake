from kratos import *
from lake.modules.sram_stub import SRAMStub


# Should we do this with registers or SRAM?
# What options do we have?
class VirtualRemapTable(Generator):
    def __init__(self, macro_width, logical_banks, macro_depth):
        super().__init__("virtual_remap_table")
        self._virt_addr = self.input("i_virt_addr", width)
        self._ren = self.input("i_ren", 1)
        self._out = self.output("o_data_out", width)
        self._in = self.input("i_data_in", width)
        self._clk = self.clock("i_clk")
        self._rst_n = self.reset("i_rst_n")

        # First wrap sram_stub
        sram_stub = SRAMStub(width, 1024)
        self.add_child_generator(f"u_sram_stub_0", sram_stub)
        self.wire(sram_stub.i_data, self._in)
        self.wire(self._out, sram_stub.o_data)

        self.wire(sram_stub.i_addr, 0)
        self.wire(sram_stub.i_cen, 0)
        self.wire(sram_stub.i_wen, 0)
        self.wire(sram_stub.i_clk, self._clk)
        self.wire(sram_stub.i_rst_n, self._rst_n)


if __name__ == "__main__":
    vrt_dut = VirtualRemapTable()
    verilog(vrt_dut, filename="virtual_remap_table.sv", check_active_high=False)
