from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg
import kratos as kts


class AppCtrl(Generator):
    '''
    Application Controller.
        This module holds the logic/expression of read/write sequencing at the application level.
        For example, a pure double buffering application needs a deterministic lockstep between sets
        of reads and writes. The constraint imposed on this application prevents the divided memory space
        from being self-conflicting.
        For a line buffer, we only need
    '''
    def __init__(self,
                 interconnect_input_ports,
                 interconnect_output_ports,
                 depth_width=16,
                 sprt_stcl_valid=False,
                 stcl_cnt_width=16,
                 stcl_iter_support=4):
        super().__init__("app_ctrl", debug=True)

        self.int_in_ports = interconnect_input_ports
        self.int_out_ports = interconnect_output_ports
        self.depth_width = depth_width
        self.sprt_stcl_valid = sprt_stcl_valid
        self.stcl_cnt_width = stcl_cnt_width
        self.stcl_iter_support = stcl_iter_support

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # IO
        self._wen_in = self.input("wen_in", self.int_in_ports)
        self._ren_in = self.input("ren_in", self.int_out_ports)

        self._ren_update = self.input("ren_update", self.int_out_ports)

        self._tb_valid = self.input("tb_valid", self.int_out_ports)

        self._valid_out_data = self.output("valid_out_data", self.int_out_ports)

        self._valid_out_stencil = self.output("valid_out_stencil", self.int_out_ports)

        # Send tb valid to valid out for now...

        if self.sprt_stcl_valid:
            # Add the config registers to watch
            self._ranges = self.input("ranges", self.stcl_cnt_width,
                                      size=self.stcl_iter_support,
                                      packed=True,
                                      explicit_array=True)
            self._ranges.add_attribute(ConfigRegAttr("Ranges of stencil valid generator"))

            self._threshold = self.input("threshold", self.stcl_cnt_width,
                                         size=self.stcl_iter_support,
                                         packed=True,
                                         explicit_array=True)
            self._threshold.add_attribute(ConfigRegAttr("Threshold of stencil valid generator"))

            self._dim_counter = self.var("dim_counter", self.stcl_cnt_width,
                                         size=self.stcl_iter_support,
                                         packed=True,
                                         explicit_array=True)

            self._update = self.var("update", self.stcl_iter_support)

            self.wire(self._update[0], const(1, 1))
            for i in range(self.stcl_iter_support - 1):
                self.wire(self._update[i + 1],
                          (self._dim_counter[i] == (self._ranges[i] - 1)) & self._update[i])

            for i in range(self.stcl_iter_support):
                self.add_code(self.dim_counter_update, idx=i)

            # Now we need to just compute stencil valid
            threshold_comps = [self._dim_counter[_i] >= self._threshold[_i] for _i in range(self.stcl_iter_support)]
            self.wire(self._valid_out_stencil[0], kts.concat(*threshold_comps).r_and())
            for i in range(self.int_out_ports - 1):
                # self.wire(self._valid_out_stencil[i + 1], 0)
                # for multiple ports
                self.wire(self._valid_out_stencil[i + 1], kts.concat(*threshold_comps).r_and())

        else:
            self.wire(self._valid_out_stencil, self._tb_valid)

        # Now gate the valid with stencil valid
        self.wire(self._valid_out_data, self._tb_valid & self._valid_out_stencil)
        self._wr_delay_state_n = self.var("wr_delay_state_n", self.int_out_ports)
        self._wen_out = self.output("wen_out", self.int_in_ports)
        self._ren_out = self.output("ren_out", self.int_out_ports)

        self._write_depth_wo = self.input("write_depth_wo", self.depth_width,
                                          size=self.int_in_ports,
                                          explicit_array=True,
                                          packed=True)
        self._write_depth_wo.add_attribute(ConfigRegAttr("Depth of writes"))

        self._write_depth_ss = self.input("write_depth_ss", self.depth_width,
                                          size=self.int_in_ports,
                                          explicit_array=True,
                                          packed=True)
        self._write_depth_ss.add_attribute(ConfigRegAttr("Depth of writes"))

        self._write_depth = self.var("write_depth", self.depth_width,
                                     size=self.int_in_ports,
                                     explicit_array=True,
                                     packed=True)

        for i in range(self.int_in_ports):
            self.wire(self._write_depth[i],
                      kts.ternary(self._wr_delay_state_n[i],
                                  self._write_depth_ss[i],
                                  self._write_depth_wo[i]))

        self._read_depth = self.input("read_depth", self.depth_width,
                                      size=self.int_out_ports,
                                      explicit_array=True,
                                      packed=True)
        self._read_depth.add_attribute(ConfigRegAttr("Depth of reads"))

        self._write_count = self.var("write_count", self.depth_width,
                                     size=self.int_in_ports,
                                     explicit_array=True,
                                     packed=True)

        self._read_count = self.var("read_count", self.depth_width,
                                    size=self.int_out_ports,
                                    explicit_array=True,
                                    packed=True)
        self._write_done = self.var("write_done", self.int_in_ports)
        self._write_done_ff = self.var("write_done_ff", self.int_in_ports)
        self._read_done = self.var("read_done", self.int_out_ports)
        self._read_done_ff = self.var("read_done_ff", self.int_out_ports)

        self.in_port_bits = max(1, kts.clog2(self.int_in_ports))
        self._input_port = self.input("input_port", self.in_port_bits,
                                      size=self.int_out_ports,
                                      explicit_array=True,
                                      packed=True)
        self._input_port.add_attribute(ConfigRegAttr("Relative input port for an output port"))

        self.out_port_bits = max(1, kts.clog2(self.int_out_ports))
        self._output_port = self.input("output_port", self.out_port_bits,
                                       size=self.int_in_ports,
                                       explicit_array=True,
                                       packed=True)
        self._output_port.add_attribute(ConfigRegAttr("Relative output port for an input port"))

        self._prefill = self.input("prefill", self.int_out_ports)
        self._prefill.add_attribute(ConfigRegAttr("Is the input stream prewritten?"))

        for i in range(self.int_out_ports):
            self.add_code(self.set_read_done, idx=i)
            if self.int_in_ports == 1:
                self.add_code(self.set_read_done_ff_one_wr, idx=i)
            else:
                self.add_code(self.set_read_done_ff, idx=i)

        # self._write_done_comb = self.var("write_done_comb", self.int_in_ports)
        for i in range(self.int_in_ports):
            self.add_code(self.set_write_done, idx=i)
            self.add_code(self.set_write_done_ff, idx=i)

        for i in range(self.int_in_ports):
            self.add_code(self.set_write_cnt, idx=i)
        for i in range(self.int_out_ports):
            if self.int_in_ports == 1:
                self.add_code(self.set_read_cnt_one_wr, idx=i)
            else:
                self.add_code(self.set_read_cnt, idx=i)

        for i in range(self.int_out_ports):
            if self.int_in_ports == 1:
                self.add_code(self.set_wr_delay_state_one_wr, idx=i)
            else:
                self.add_code(self.set_wr_delay_state, idx=i)

        self._read_on = self.var("read_on", self.int_out_ports)
        for i in range(self.int_out_ports):
            self.wire(self._read_on[i], self._read_depth[i].r_or())

        # If we have prefill enabled, we are skipping the initial delay step...
        self.wire(self._ren_out,
                  (self._wr_delay_state_n | self._prefill) & ~self._read_done_ff & self._ren_in &
                  self._read_on)
        self.wire(self._wen_out, ~self._write_done_ff & self._wen_in)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_write_done_ff(self, idx):
        if ~self._rst_n:
            self._write_done_ff[idx] = 0
        elif self._write_done[idx] & self._read_done[self._output_port[idx]]:
            self._write_done_ff[idx] = 0
        elif self._write_done[idx]:
            self._write_done_ff[idx] = 1

    @always_comb
    def set_write_done(self, idx):
        self._write_done[idx] = (self._wen_in[idx] &
                                 (self._write_count[idx] == (self._write_depth[idx] - 1))) | \
            self._write_done_ff[idx]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_write_cnt(self, idx):
        if ~self._rst_n:
            self._write_count[idx] = 0
        elif self._write_done[idx] & self._read_done[self._output_port[idx]]:
            self._write_count[idx] = 0
        elif self._wen_in[idx] & ~self._write_done_ff[idx]:
            self._write_count[idx] = self._write_count[idx] + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_read_done_ff(self, idx):
        if ~self._rst_n:
            self._read_done_ff[idx] = 0
        elif self._write_done[self._input_port[idx]] & self._read_done[idx]:
            self._read_done_ff[idx] = 0
        elif self._read_done[idx]:
            self._read_done_ff[idx] = 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_read_done_ff_one_wr(self, idx):
        if ~self._rst_n:
            self._read_done_ff[idx] = 0
        elif self._write_done & self._read_done[idx]:
            self._read_done_ff[idx] = 0
        elif self._read_done[idx]:
            self._read_done_ff[idx] = 1

    @always_comb
    def set_read_done(self, idx):
        self._read_done[idx] = (self._ren_update[idx] & self._ren_in[idx] &
                                (self._read_count[idx] == (self._read_depth[idx] - 1))) | \
            self._read_done_ff[idx] | (~self._prefill[idx] & ~self._wr_delay_state_n[idx])

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_read_cnt(self, idx):
        if ~self._rst_n:
            self._read_count[idx] = 0
        elif self._write_done[self._input_port[idx]] & self._read_done[idx]:
            self._read_count[idx] = 0
        elif self._ren_in[idx] & self._ren_update[idx]:
            self._read_count[idx] = self._read_count[idx] + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_read_cnt_one_wr(self, idx):
        if ~self._rst_n:
            self._read_count[idx] = 0
        elif self._write_done & self._read_done[idx]:
            self._read_count[idx] = 0
        elif self._ren_in[idx] & self._ren_update[idx]:
            self._read_count[idx] = self._read_count[idx] + 1

    # When we start up, there is no way to read data from storage.
    # We use this flag to gate read logic TODO : change to a separate counter
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_wr_delay_state(self, idx):
        if ~self._rst_n:
            self._wr_delay_state_n[idx] = 0
        elif self._write_done[self._input_port[idx]]:
            self._wr_delay_state_n[idx] = 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_wr_delay_state_one_wr(self, idx):
        if ~self._rst_n:
            self._wr_delay_state_n[idx] = 0
        elif self._write_done:
            self._wr_delay_state_n[idx] = 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def dim_counter_update(self, idx):
        if ~self._rst_n:
            self._dim_counter[idx] = 0
        elif self._ren_in[0] & self._ren_update[0]:
            # if self._update[idx] & (idx < self._dimensionality):
            if self._update[idx]:
                if self._dim_counter[idx] == (self._ranges[idx] - 1):
                    self._dim_counter[idx] = 0
                else:
                    self._dim_counter[idx] = self._dim_counter[idx] + 1


if __name__ == "__main__":
    ac_dut = AppCtrl(interconnect_input_ports=1,
                     interconnect_output_ports=3)
    verilog(ac_dut, filename="app_ctrl.sv",
            additional_passes={"lift config regs": lift_config_reg})
