from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import safe_wire, add_counter
import kratos as kts


class RVReadCtrl(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 iterator_support=6,
                 id_config_width=11,
                 config_width=16
                 ):

        super().__init__("rv_read_ctrl")

        ##################################################################################
        # Capture constructor parameter...
        ##################################################################################
        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.mem_addr_width = clog2(self.mem_depth)
        self.fetch_width = mem_width // data_width
        self.iterator_support = iterator_support
        self.id_config_width = id_config_width
        self.config_width = config_width

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # tile input
        self._ready_i = self.input("ready_i", 1)
        # from rd ctrl
        self._token_dec_i = self.input("token_dec_i", 1)
        self._token_avail_i = self.input("token_avail_i", 1)
        # from ariber
        self._is_granted_i = self.input("is_granted_i", 1)
        self._rdata_i = self.input("rdata_i",
                                    self.data_width,
                                    size=self.fetch_width,
                                    packed=True)

        # tile output
        self._valid_o = self.output("valid_o", 1)
        self._dout_o = self.output("dout_o", self.data_width)
        # to rd ctrl
        self._token_dec_o = self.output("token_dec_o", 1)
        self._token_avail_o = self.output("token_avail_o", 1)
        # to arbiter
        self._ren_o = self.output("ren_o", 1)
        self._raddr_o = self.output("raddr_o", self.mem_addr_width)

        ##################################################################################
        # Config Registers
        ##################################################################################
        self._threshold = self.input("threshold", self.config_width)
        self._threshold.add_attribute(ConfigRegAttr("Valid memory read threshold to generate a memory write token"))
        self._threshold.add_attribute(FormalAttr(f"{self._threshold.name}", FormalSignalConstraint.SOLVE))

        ##################################################################################
        # Internal Registers and Wires
        ##################################################################################
        self._tb = self.var(f"tb",
                            width=self.data_width,
                            size=self.fetch_width,
                            packed=True,
                            explicit_array=True)
        self._tb_write = self.var("tb_write", 1)
        self._tb_empty = self.var("tb_empty", 1)
        self._tb_cnt = self.var("tb_cnt", clog2(self.fetch_width))

        self._zero_threshold = self.var("zero_threshold", 1)
        self._ren = self.var("ren", 1)
        self._tb_valid = self.var("tb_valid", 1)
        self._id_restart = self.var("id_restart", 1)
        self._raddr = self.var("raddr", self.mem_addr_width)
        self._token_count = self.var("token_count", self.config_width)
        self._valid_count = add_counter(self, "valid_count",
                                        self.config_width,
                                        increment=self._tb_write,
                                        clear=(self._token_dec_i | self._zero_threshold))

        # assign module outputs
        self.wire(self._valid_o, self._tb_valid)
        self.wire(self._dout_o, self._tb[self._tb_cnt])
        self.wire(self._token_dec_o, self._id_restart)
        self.wire(self._token_avail_o, ternary(self._zero_threshold, 1, self._token_count > 0))
        self.wire(self._ren_o, self._ren)
        self.wire(self._raddr_o, self._raddr)

        # internal
        self.wire(self._zero_threshold, self._threshold == 0)

        ##################################################################################
        # Controllers
        ##################################################################################
        # mem iteration domain
        id = ForLoop(iterator_support=self.iterator_support,
                     config_width=self.id_config_width)

        self.add_child(f"id", id,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._is_granted_i)

        self.wire(self._id_restart, id.ports.restart)

        # mem address generation
        ag = AddrGen(iterator_support=self.iterator_support,
                     config_width=self.mem_addr_width)

        self.add_child(f"ag", ag,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._is_granted_i,
                       mux_sel=id.ports.mux_sel_out,
                       restart=0)

        self.wire(self._raddr, ag.ports.addr_out)

        # handling tokens
        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def token_avail_ctrl():
            if ~self._rst_n:
                self._token_count = 0
            elif ~self._zero_threshold:
                if ((self._valid_count == self._threshold) & (~self._token_dec_i)):
                    self._token_count = self._token_count + 1
                elif ((self._valid_count != self._threshold) & self._token_dec_i):
                    self._token_count = self._token_count - 1

        self.add_code(token_avail_ctrl)

        # tb stores wide words
        @always_ff((posedge, "clk"))
        def tb_ctrl():
            if self._tb_write:
                self._tb = self._rdata_i

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def tb_empty_ctrl():
            if ~self._rst_n:
                self._tb_write = 0
                self._tb_valid = 0
                self._tb_cnt = 0
                self._ren = 0
            # assumes tb_height is 1
            else:
                self._tb_write = self._is_granted_i

                if self._tb_valid & self._ready_i:
                    if self._tb_cnt == (self.fetch_width - 1):
                        self._tb_cnt = 0
                    else:
                        self._tb_cnt = self._tb_cnt + 1

                if self._tb_write:
                    self._tb_valid = 1
                elif self._ready_i & (self._tb_cnt == (self.fetch_width - 1)):
                    self._tb_valid = 0

                if self._is_granted_i | self._tb_write:
                    self._ren = 0
                elif self._token_avail_i & (~self._tb_valid):
                    self._ren = 1
                else:
                    self._ren = 0

        self.add_code(tb_ctrl)
        self.add_code(tb_empty_ctrl)


if __name__ == "__main__":
    dut = RVReadCtrl()
    dut.add_attribute("sync-reset=flush")
    kts.passes.auto_insert_sync_reset(dut.internal_generator)
    verilog(dut, filename="rv_read_ctrl.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
