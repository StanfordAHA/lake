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
                 config_width=16,
                 gen_token=False
                 ):

        module_name = "rv_read_ctrl"
        if gen_token:
            module_name += "_gen_token"
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
        self.gen_token = gen_token

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # tile input
        self._ready_i = self.input("ready_i", 1)
        # from wr ctrl
        self._ag_starting_addr = self.input("ag_starting_addr", self.mem_addr_width)
        self._token_avail_i = self.input("token_avail_i", 1)
        self._reset_window_addr_i = self.input("reset_window_addr_i", 1)
        self._window_addr_i = self.input("window_addr_i", self.mem_addr_width)
        if self.gen_token:
            self._token_gen_en_i = self.input("token_gen_en_i", 1)
            self._token_dec_i = self.input("token_dec_i", 1)
        # from ariber
        self._is_granted_i = self.input("is_granted_i", 1)
        self._rdata_i = self.input("rdata_i",
                                    self.data_width,
                                    size=self.fetch_width,
                                    packed=True)

        # tile output
        self._valid_o = self.output("valid_o", 1)
        self._dout_o = self.output("dout_o", self.data_width)
        # to wr ctrl
        self._token_dec_o = self.output("token_dec_o", 1)
        if self.gen_token:
            self._token_avail_o = self.output("token_avail_o", 1)
            self._reset_window_addr_o = self.output("reset_window_addr_o", 1)
            self._window_addr_o = self.output("window_addr_o", self.mem_addr_width)
        # to arbiter
        self._ren_o = self.output("ren_o", 1)
        self._raddr_o = self.output("raddr_o", self.mem_addr_width)

        ##################################################################################
        # Config Registers
        ##################################################################################
        if self.gen_token:
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

        self._ren = self.var("ren", 1)
        self._raddr = self.var("raddr", self.mem_addr_width)
        self._tb_valid = self.var("tb_valid", 1)
        self._token_dec = self.var("token_dec", 1)
        if self.gen_token:
            self._met_threshold = self.var("met_threshold", 1)
            self._wctrl_busy_state = self.var("wctrl_busy_state", 1)
            self._window_done = self.var("window_done", 1)
            self._token_count = self.var("token_count", 2)
            self._valid_count = add_counter(self, "valid_count",
                                            self.config_width,
                                            increment=self._tb_write,
                                            clear=(self._met_threshold | ~self._token_gen_en_i))

        # assign module outputs
        self.wire(self._valid_o, self._tb_valid)
        self.wire(self._dout_o, self._tb[self._tb_cnt])
        self.wire(self._token_dec_o, self._token_dec)
        if self.gen_token:
            self.wire(self._token_avail_o, ternary(self._token_gen_en_i, 1, self._wctrl_busy_state))
        self.wire(self._ren_o, self._ren)
        self.wire(self._raddr_o, self._raddr)

        # internal
        if self.gen_token:
            self.wire(self._met_threshold,
                      self._token_gen_en_i & self._tb_write & (self._threshold == self._valid_count))

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
                       restart=self._token_dec)

        # mem address generation
        ag = AddrGen(iterator_support=self.iterator_support,
                     config_width=self.mem_addr_width,
                     get_window_base=self.gen_token,
                     window_restart=True,
                     share_starting_addr=True)

        self.add_child(f"ag", ag,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       starting_addr=self._ag_starting_addr,
                       window_addr_i=self._window_addr_i,
                       step=(self._is_granted_i | self._reset_window_addr_i),
                       mux_sel=id.ports.mux_sel_out,
                       restart=self._reset_window_addr_i,
                       addr_out=self._raddr)

        if self.gen_token:
            self.wire(ag.ports.window_done, self._window_done)
            self.wire(ag.ports.window_addr_o, self._window_addr_o)

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
        def wctrl_busy_state_ctrl(token_count, token_dec_i, wctrl_busy_state):
            if ~self._rst_n:
                wctrl_busy_state = 0
            else:
                if wctrl_busy_state == 0:
                    if token_count > 0:
                        wctrl_busy_state = 1
                elif wctrl_busy_state == 1:
                    if token_dec_i & ~(token_count > 1):
                        wctrl_busy_state = 0

        @always_comb
        def reset_window_addr_ctrl(token_count, token_dec_i, wctrl_busy_state, reset_window_addr_o):
            reset_window_addr_o = 0
            if (token_count > 0) & (wctrl_busy_state == 0):
                reset_window_addr_o = 1
            if (wctrl_busy_state == 1) & token_dec_i & (token_count > 1):
                reset_window_addr_o = 1

        # setting the base_addr of the next window
        @always_ff((posedge, "clk"))
        def window_done_ctrl():
            if ~self._rst_n:
                self._window_done = 0
            elif id.ports.restart:
                self._window_done = 1
            elif self._window_done & self._is_granted_i:
                self._window_done = 0

        if self.gen_token:
            self.add_code(token_count_ctrl,
                          token_count=self._token_count,
                          met_threshold=self._met_threshold,
                          token_dec_i=self._token_dec_i)

            self.add_code(wctrl_busy_state_ctrl,
                          token_count=self._token_count,
                          token_dec_i=self._token_dec_i,
                          wctrl_busy_state=self._wctrl_busy_state)

            self.add_code(reset_window_addr_ctrl,
                          token_count=self._token_count,
                          token_dec_i=self._token_dec_i,
                          wctrl_busy_state=self._wctrl_busy_state,
                          reset_window_addr_o=self._reset_window_addr_o)

            self.add_code(window_done_ctrl)

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
                elif self._token_avail_i & ~self._tb_valid:
                    self._ren = 1
                elif self._tb_valid & self._ready_i & self._tb_cnt == (self.fetch_width - 1):
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
