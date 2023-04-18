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
                 config_width=16,
                 dependent=False,
                 dual_token=False,
                 ):

        module_name = "rv_write_ctrl"
        if dual_token and dependent:
            module_name += "_dependent_dual_token"
        super().__init__(module_name)

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
        self.dependent = dependent
        self.dual_token = dual_token

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
        if self.dual_token:
            self._token_dec2_i = self.input("token_dec2_i", 1)
        if self.dependent:
            self._token_avail_i = self.input("token_avail_i", 1)
            self._reset_window_addr_i = self.input("reset_window_addr_i", 1)
            self._window_addr_i = self.input("window_addr_i", self.mem_addr_width)
        # from ariber
        self._is_granted_i = self.input("is_granted_i", 1)

        # tile output
        self._ready_o = self.output("ready_o", 1)
        # to rd ctrl
        self._token_avail_o = self.output("token_avail_o", 1)
        self._reset_window_addr_o = self.output("reset_window_addr_o", 1)
        if self.dual_token:
            self._token_avail2_o = self.output("token_avail2_o", 1)
            self._reset_window_addr2_o = self.output("reset_window_addr2_o", 1)
        if self.dependent:
            self._token_dec_o = self.output("token_dec_o", 1)
        self._window_addr_o = self.output("window_addr_o", self.mem_addr_width)
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
        if self.dependent:
            self._dep = self.input("dep", 1)
            self._dep.add_attribute(ConfigRegAttr("Makes wctrl depend on rctrl's token"))
            self._dep.add_attribute(FormalAttr(f"{self._dep.name}", FormalSignalConstraint.SOLVE))

        self._threshold = self.input("threshold", self.config_width)
        self._threshold.add_attribute(ConfigRegAttr("Valid memory write threshold to generate a memory read token"))
        self._threshold.add_attribute(FormalAttr(f"{self._threshold.name}", FormalSignalConstraint.SOLVE))

        if self.dual_token:
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
        if self.dependent:
            self._one_time_restart = self.var("one_time_restart", 1)
        self._window_done = self.var("window_done", 1)
        self._waddr = self.var("waddr", self.mem_addr_width)

        self._token_count = self.var("token_count", 2)
        self._met_threshold = self.var("met_threshold", 1)
        self._rctrl_busy_state = self.var("rctrl_busy_state", 1)
        # self._credit_gen_en = self.var("credit_gen_en", 1)

        if self.dual_token:
            self._rctrl_busy_state2 = self.var("rctrl_busy_state2", 1)
            self._credit_gen_en2 = self.var("credit_gen_en2", 1)
            self._token_count2 = self.var("token_count2", 2)
            self._met_threshold2 = self.var("met_threshold2", 1)

            self._valid_count_state = self.var("valid_count_state", 1)
            self._valid_count = add_counter(self, "valid_count",
                                            self.config_width,
                                            increment=self._is_granted_i,
                                            clear=ternary(self._valid_count_state,
                                                          self._met_threshold,
                                                          self._met_threshold2))
        else:
            self._valid_count = add_counter(self, "valid_count",
                                            self.config_width,
                                            increment=self._is_granted_i,
                                            clear=self._met_threshold)

        # assign module outputs
        self.wire(self._ready_o, self._ready)
        if self.dependent:
            self.wire(self._token_dec_o, self._id_restart)
        self.wire(self._token_avail_o, self._rctrl_busy_state)
        if self.dual_token:
            self.wire(self._token_avail2_o, self._rctrl_busy_state2)
            self.wire(self._wen_o,
                      self._agg_full & ternary(self._valid_count_state,
                                               ternary(self._rctrl_busy_state2,
                                                       self._token_count2 == 1,
                                                       self._token_count2 < 1),
                                               ternary(self._rctrl_busy_state,
                                                       self._token_count == 1,
                                                       self._token_count < 1)))
        else:
            self.wire(self._wen_o,
                      self._agg_full & ternary(self._rctrl_busy_state,
                                               self._token_count == 1,
                                               self._token_count < 1))
        self.wire(self._waddr_o, self._waddr)
        self.wire(self._wdata_o, self._agg)

        # internal
        if self.dual_token:
            self.wire(self._met_threshold, self._is_granted_i & (self._valid_count == self._threshold))
            self.wire(self._met_threshold2, self._is_granted_i & (self._valid_count == self._threshold2))
            self.wire(self._credit_gen_en2, self._threshold2 != 0)
            self.wire(self._ready, self._is_granted_i | ~self._agg_full)
        else:
            self.wire(self._met_threshold, self._is_granted_i & (self._valid_count == self._threshold))
            self.wire(self._ready, self._is_granted_i | ~self._agg_full)
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
                       step=self._is_granted_i,
                       restart=self._id_restart)

        # mem address generation
        ag = AddrGen(iterator_support=self.iterator_support,
                     config_width=self.mem_addr_width,
                     get_window_base=True,
                     get_window_base_dep=self.dependent,
                     window_restart=self.dependent)

        self.add_child(f"ag", ag,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       window_done=self._window_done,
                       mux_sel=id.ports.mux_sel_out,
                       addr_out=self._waddr,
                       window_addr_o=self._window_addr_o)

        if self.dependent:
            self.wire(ag.ports.dep_i, self._dep)
            self.wire(ag.ports.window_addr_i, self._window_addr_i)
            self.wire(ag.ports.step,
                      self._is_granted_i | (self._dep & self._reset_window_addr_i))
            self.wire(ag.ports.restart, self._dep & self._reset_window_addr_i)
        else:
            self.wire(ag.ports.step, self._is_granted_i)
            self.wire(ag.ports.restart, 0)

        # handling tokens
        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def token_count_ctrl(token_count, met_threshold, token_dec_i):
            if ~self._rst_n:
                token_count = 0
            elif (met_threshold & ~token_dec_i):
                token_count = token_count + 1
            elif (~met_threshold & token_dec_i):
                token_count = token_count - 1

        # busy state to generate the reset_window_addr pulse
        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def rctrl_busy_state_ctrl(token_count, token_dec_i, rctrl_busy_state):
            if ~self._rst_n:
                rctrl_busy_state = 0
            else:
                if rctrl_busy_state == 0:
                    if token_count > 0:
                        rctrl_busy_state = 1
                elif rctrl_busy_state == 1:
                    if token_dec_i & ~(token_count > 1):
                        rctrl_busy_state = 0

        @always_comb
        def reset_window_addr_ctrl(token_count, token_dec_i, rctrl_busy_state, reset_window_addr_o):
            reset_window_addr_o = 0
            if (token_count > 0) & (rctrl_busy_state == 0):
                reset_window_addr_o = 1
            if (rctrl_busy_state == 1) & token_dec_i & (token_count > 1):
                reset_window_addr_o = 1

        self.add_code(token_count_ctrl,
                      token_count=self._token_count,
                      met_threshold=self._met_threshold,
                      token_dec_i=self._token_dec_i)

        self.add_code(rctrl_busy_state_ctrl,
                      token_count=self._token_count,
                      token_dec_i=self._token_dec_i,
                      rctrl_busy_state=self._rctrl_busy_state)

        self.add_code(reset_window_addr_ctrl,
                      token_count=self._token_count,
                      token_dec_i=self._token_dec_i,
                      rctrl_busy_state=self._rctrl_busy_state,
                      reset_window_addr_o=self._reset_window_addr_o)

        # choose between two token counts
        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def valid_count_state_ctrl():
            if ~self._rst_n:
                self._valid_count_state = 0
            elif ~self._credit_gen_en2:
                self._valid_count_state = 1
            elif self._valid_count_state == 0:
                if self._met_threshold2:
                    self._valid_count_state = 1
            elif self._valid_count_state == 1:
                if self._met_threshold:
                    self._valid_count_state = 0

        if self.dual_token:
            self.add_code(valid_count_state_ctrl)
            self.add_code(token_count_ctrl,
                          token_count=self._token_count2,
                          met_threshold=self._met_threshold2,
                          token_dec_i=self._token_dec2_i)
            self.add_code(rctrl_busy_state_ctrl,
                          token_count=self._token_count2,
                          token_dec_i=self._token_dec2_i,
                          rctrl_busy_state=self._rctrl_busy_state2)
            self.add_code(reset_window_addr_ctrl,
                          token_count=self._token_count2,
                          token_dec_i=self._token_dec2_i,
                          rctrl_busy_state=self._rctrl_busy_state2,
                          reset_window_addr_o=self._reset_window_addr2_o)

        # setting the base_addr of the next window
        @always_ff((posedge, "clk"))
        def window_done_ctrl():
            if ~self._rst_n:
                self._window_done = 0
            elif self._id_restart:
                self._window_done = 1
            elif self._window_done & self._is_granted_i:
                self._window_done = 0
            elif self.dependent:
                if ~self._one_time_restart:
                    self._window_done = 1

        self.add_code(window_done_ctrl)

        # restart ag immediately after flush
        @always_ff((posedge, "clk"))
        def one_time_restart_ctrl():
            if ~self._rst_n:
                self._one_time_restart = 0
            elif self._dep & (~self._one_time_restart):
                self._one_time_restart = 1

        if self.dependent:
            self.add_code(one_time_restart_ctrl)

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
