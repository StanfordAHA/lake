from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.utils.util import safe_wire, add_counter
import kratos as kts


class RVWriteCtrl(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 iterator_support=6,
                 id_config_width=11,
                 config_width=16
                 ):

        super().__init__("rv_write_ctrl")

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
        self._valid_i = self.input("valid_i", 1)
        self._din_i = self.input("din_i", self.data_width)
        # from rd ctrl
        self._token_dec_i = self.input("token_dec_i", 1)
        self._token_dec2_i = self.input("token_dec2_i", 1)
        self._token_avail_i = self.input("token_avail_i", 1)
        # from ariber
        self._is_granted_i = self.input("is_granted_i", 1)

        # tile output
        self._ready_o = self.output("ready_o", 1)
        # to rd ctrl
        self._token_avail_o = self.output("token_avail_o", 1)
        self._token_avail2_o = self.output("token_avail2_o", 1)
        self._token_dec_o = self.output("token_dec_o", 1)
        # to arbiter
        self._wen_o = self.output("wen_o", 1)
        self._waddr_o = self.output("waddr_o", self.mem_addr_width)
        self._wdata_o = self.output("wdata_o",
                                    self.data_width,
                                    size=self.fetch_width,
                                    packed=True)

        ##################################################################################
        # Config Registers
        ##################################################################################
        self._threshold = self.input("threshold", self.config_width)
        self._threshold.add_attribute(ConfigRegAttr("Valid memory write threshold to generate a memory read token"))
        self._threshold.add_attribute(FormalAttr(f"{self._threshold.name}", FormalSignalConstraint.SOLVE))

        self._threshold2 = self.input("threshold2", self.config_width)
        self._threshold2.add_attribute(ConfigRegAttr("Valid memory write threshold to generate a memory read token"))
        self._threshold2.add_attribute(FormalAttr(f"{self._threshold2.name}", FormalSignalConstraint.SOLVE))
        ##################################################################################
        # Internal Registers and Wires
        ##################################################################################
        self._agg = self.var(f"agg",
                             width=self.data_width,
                             size=self.fetch_width,
                             packed=True,
                             explicit_array=True)
        self._agg_write = self.var("agg_write", 1)
        self._agg_full = self.var("agg_full", 1)
        self._agg_cnt = self.var("agg_cnt", clog2(self.fetch_width))

        self._ready = self.var("ready", 1)
        self._id_restart = self.var("id_restart", 1)
        self._waddr = self.var("waddr", self.mem_addr_width)
        self._token_count = self.var("token_count", self.config_width)
        self._met_threshold = self.var("met_threshold", 1)
        self._valid_count = add_counter(self, "valid_count",
                                        self.config_width,
                                        increment=self._valid_i,
                                        clear=self._met_threshold)
        self._token_count2 = self.var("token_count2", self.config_width)
        self._met_threshold2 = self.var("met_threshold2", 1)
        self._valid_count2 = add_counter(self, "valid_count2",
                                        self.config_width,
                                        increment=self._valid_i,
                                        clear=self._met_threshold2)

        # assign module outputs
        self.wire(self._ready_o, self._ready)
        self.wire(self._token_dec_o, self._id_restart)
        self.wire(self._token_avail_o, self._token_count > 0)
        self.wire(self._token_avail2_o, self._token_count2 > 0)
        self.wire(self._wen_o, self._agg_full)
        self.wire(self._waddr_o, self._waddr)
        self.wire(self._wdata_o, self._agg)

        # internal
        self.wire(self._met_threshold, self._valid_count == self._threshold)
        self.wire(self._met_threshold2, self._valid_count2 == self._threshold2)
        self.wire(self._ready, self._is_granted_i | (~self._agg_full))
        self.wire(self._agg_write, self._ready & self._valid_i)

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
                       restart=id.ports.restart)

        self.wire(self._waddr, ag.ports.addr_out)

        # handling tokens
        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def token_count_ctrl():
            if ~self._rst_n:
                self._token_count = 0
            elif (self._met_threshold & (~self._token_dec_i)):
                self._token_count = self._token_count + 1
            elif ((~self._met_threshold) & self._token_dec_i):
                self._token_count = self._token_count - 1

        self.add_code(token_count_ctrl)

        # handling tokens
        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def token_count2_ctrl():
            if ~self._rst_n:
                self._token_count2 = 0
            elif (self._met_threshold2 & (~self._token_dec2_i)):
                self._token_count2 = self._token_count2 + 1
            elif ((~self._met_threshold2) & self._token_dec2_i):
                self._token_count2 = self._token_count2 - 1

        self.add_code(token_count2_ctrl)

        # AGG to pack into wide words
        @always_ff((posedge, "clk"))
        def agg_ctrl():
            if self._agg_write:
                self._agg[self._agg_cnt] = self._din_i

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def agg_full_ctrl():
            if ~self._rst_n:
                self._agg_full = 0
                self._agg_cnt = 0
            # assumes agg_height is 1
            else:
                if self._agg_write:
                    if self._agg_cnt == (self.fetch_width - 1):
                        self._agg_cnt = 0
                    else:
                        self._agg_cnt = self._agg_cnt + 1

                if self._agg_write & (self._agg_cnt == (self.fetch_width - 1)):
                    self._agg_full = 1
                elif self._is_granted_i:
                    self._agg_full = 0

        self.add_code(agg_ctrl)
        self.add_code(agg_full_ctrl)


if __name__ == "__main__":
    dut = RVWriteCtrl()
    dut.add_attribute("sync-reset=flush")
    kts.passes.auto_insert_sync_reset(dut.internal_generator)
    verilog(dut, filename="rv_write_ctrl.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
