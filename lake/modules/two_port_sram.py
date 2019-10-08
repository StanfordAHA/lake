from kratos import *
from lake.modules.sram_stub import SRAMStub
from lake.modules.pipe_reg import PipeReg

class TwoPortMemory(Generator):
    def __init__(self, 
                macro_capacity, 
                macro_width,
                desired_capacity, 
                desired_width,
                two_port):
                 #banks):

        super().__init__("two_port_mem")

        self._clk = self.clock("i_clk")
        self._rst_n = self.reset("i_rst_n")

        self._rd_addr = self.input("i_rd_addr", clog2(desired_capacity))
        self._rd_data = self.output("o_rd_data", desired_width)
        self._ren     = self.intput("i_ren", 1)

        self._wr_addr = self.input("i_wr_addr", clog2(desired_capacity))
        self._wr_data = self.input("i_wr_data", desired_width)
        self._wen     = self.input("i_wen", 1)

        self._rd_data_int = self.var("rd_data_in", desired_width)

        # If already a two-port, there is only one cycle of latency
        latency = 1

        if two_port:
            self.instantiate_true_two_port(macro_width, macro_capacity, )
        else:
            # typically you want to break it up into more banks
            # Based on the capacity in words vs macro capacity
            latency = self.build_two_port(macro_capacity, macro_width, desired_capacity, desired_width)


        pipe_reg_rd_data = PipeReg(desired_width, latency)
        # still want passthru_block for read+write to same location


        # First wrap sram_stub
        sram_stub = SRAMStub(width, 1024)
        self.add_child(f"u_sram_stub_0", sram_stub)
        self.wire(sram_stub.i_data, self._in)
        self.wire(self._out, sram_stub.o_data)

        self.wire(sram_stub.i_addr, 0)
        self.wire(sram_stub.i_cen, 0)
        self.wire(sram_stub.i_wen, 0)
        self.wire(sram_stub.i_clk, self._clk)
        self.wire(sram_stub.i_rst_n, self._rst_n)

    def instantiate_true_two_port(self, data_width):
        return 0

    def build_two_port(self, macro_capacity, macro_width, desired_capacity, desired_width):
        return 1


if __name__ == "__main__":
    tpm_dut = TwoPortMemory()
    verilog(tpm_dut, filename="two_port_mem.sv", check_active_high=False)