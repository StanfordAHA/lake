from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.modules.arbiter import Arbiter
from lake.utils.util import safe_wire, add_counter
import kratos as kts


class RVArbiter(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512
                 ):

        super().__init__("rv_arbiter")

        ##################################################################################
        # Capture constructor parameter...
        ##################################################################################
        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.mem_addr_width = clog2(self.mem_depth)
        self.fetch_width = mem_width // data_width

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # write request 0
        self._wen_0_i = self.input("wen_0_i", 1)
        self._waddr_0_i = self.input("waddr_0_i", self.mem_addr_width)
        self._wdata_0_i = self.input("wdata_0_i",
                                     self.data_width,
                                     size=self.fetch_width,
                                     packed=True)

        # write request 1
        self._wen_1_i = self.input("wen_1_i", 1)
        self._waddr_1_i = self.input("waddr_1_i", self.mem_addr_width)
        self._wdata_1_i = self.input("wdata_1_i",
                                     self.data_width,
                                     size=self.fetch_width,
                                     packed=True)

        # read request 0
        self._ren_0_i = self.input("ren_0_i", 1)
        self._raddr_0_i = self.input("raddr_0_i", self.mem_addr_width)

        # read request 1
        self._ren_1_i = self.input("ren_1_i", 1)
        self._raddr_1_i = self.input("raddr_1_i", self.mem_addr_width)

        # arbitration output, one-hot
        self._is_granted_0_o = self.output("is_granted_0_o", 1)
        self._is_granted_1_o = self.output("is_granted_1_o", 1)
        self._is_granted_2_o = self.output("is_granted_2_o", 1)
        self._is_granted_3_o = self.output("is_granted_3_o", 1)

        # to memory
        self._mem_wen_o = self.output("mem_wen_o", 1)
        self._mem_ren_o = self.output("mem_ren_o", 1)
        self._mem_addr_o = self.output("mem_addr_o", self.mem_addr_width)
        self._mem_wdata_o = self.output("mem_wdata_o",
                                        self.data_width,
                                        size=self.fetch_width,
                                        packed=True)

        ##################################################################################
        # Config Registers
        ##################################################################################

        ##################################################################################
        # Internal Registers and Wires
        ##################################################################################
        self._requests = self.var("requests", 4)
        self._grants = self.var("grants", 4)

        # assign module outputs
        self.wire(self._is_granted_0_o, self._grants[0])
        self.wire(self._is_granted_1_o, self._grants[1])
        self.wire(self._is_granted_2_o, self._grants[2])
        self.wire(self._is_granted_3_o, self._grants[3])

        @always_comb
        def mem_request_ctrl():
            self._mem_wen_o = 0
            self._mem_ren_o = 0
            self._mem_addr_o = 0
            self._mem_wdata_o = 0

            if self._grants[0]:
                self._mem_wen_o = 1
                self._mem_addr_o = self._waddr_0_i
                self._mem_wdata_o = self._wdata_0_i
            elif self._grants[1]:
                self._mem_wen_o = 1
                self._mem_addr_o = self._waddr_1_i
                self._mem_wdata_o = self._wdata_1_i
            elif self._grants[2]:
                self._mem_ren_o = 1
                self._mem_addr_o = self._raddr_0_i
            elif self._grants[3]:
                self._mem_ren_o = 1
                self._mem_addr_o = self._raddr_1_i

        self.add_code(mem_request_ctrl)

        # internal
        self.wire(self._requests, concat(self._ren_1_i,
                                         self._ren_0_i,
                                         self._wen_1_i,
                                         self._wen_0_i))

        ##################################################################################
        # Controllers
        ##################################################################################
        arbiter = Arbiter(add_flush=False, ins=4, algo="Rotating", add_clk_enable=False)
        self.add_child("memory_arbiter", arbiter,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       request_in=self._requests,
                       resource_ready=const(1, 1),
                       grant_out=self._grants)


if __name__ == "__main__":
    dut = RVArbiter()
    verilog(dut, filename="rv_arbiter.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
