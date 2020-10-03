from kratos import *
from lake.utils.util import increment, decrement
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.modules.reg_fifo import RegFIFO


class Prefetcher(Generator):
    '''
    This module will handle prefetching from SRAM to the tb.
    Since there is a variable latency for a ready cycle to
    provide the next valid set of data to the tbs, we need to
    buffer some amount so that we can satisfy these requests with immediacy
    '''
    def __init__(self,
                 fetch_width,
                 data_width,
                 max_prefetch):
        super().__init__("prefetcher")
        # Capture to the object
        self.fetch_width = fetch_width
        self.data_width = data_width
        self.fw_int = int(self.fetch_width / self.data_width)
        self.max_prefetch = max_prefetch

        # Clock and Reset
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Inputs
        self._data_in = self.input("data_in",
                                   self.data_width,
                                   size=self.fw_int,
                                   explicit_array=True,
                                   packed=True)

        self._valid_read = self.input("valid_read", 1)
        self._tba_rdy_in = self.input("tba_rdy_in", 1)

        self._input_latency = self.input("input_latency", clog2(self.max_prefetch) + 1)
        doc = "This register is set to denote the input latency loop for reads. " + \
              "This is sent to an internal fifo and an almost full signal is " + \
              "used to pull more reads that the transpose buffers need."
        self._input_latency.add_attribute(ConfigRegAttr(doc))

        self._max_lat = const(self.max_prefetch - 1,
                              clog2(self.max_prefetch) + 1)

        # Outputs
        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=self.fw_int,
                                     explicit_array=True,
                                     packed=True)

        self._valid_out = self.output("valid_out", 1)
        self._prefetch_step = self.output("prefetch_step", 1)

        # Local Signals
        self._cnt = self.var("cnt", clog2(self.max_prefetch) + 1)
        self._fifo_empty = self.var("fifo_empty", 1)
        self._fifo_full = self.var("fifo_full", 1)

        reg_fifo = RegFIFO(data_width=self.data_width,
                           width_mult=self.fw_int,
                           depth=self.max_prefetch)

        self.add_child("fifo", reg_fifo,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       clk_en=1,
                       data_in=self._data_in,
                       data_out=self._data_out,
                       push=self._valid_read,
                       pop=self._tba_rdy_in,
                       empty=self._fifo_empty,
                       full=self._fifo_full,
                       valid=self._valid_out)

        # Generate
        self.add_code(self.update_cnt)
        self.add_code(self.set_prefetch_step)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def update_cnt(self):
        if ~self._rst_n:
            self._cnt = 0
        elif(self._valid_read & ~self._tba_rdy_in & ~self._fifo_full):
            self._cnt = self._cnt + 1
        elif(~self._valid_read & self._tba_rdy_in & ~self._fifo_empty):
            self._cnt = self._cnt - 1

    @always_comb
    def set_prefetch_step(self):
        self._prefetch_step = (self._cnt + self._input_latency) < (self.max_prefetch)


if __name__ == "__main__":
    align_dut = Prefetcher(fetch_width=32,
                           data_width=16,
                           max_prefetch=64)
    verilog(align_dut, filename="prefetcher.sv")
