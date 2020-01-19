from kratos import *


class LineBufferControl(Generator):
    def __init__(self):
        super().__init__("linebuffer_control", True)

        # PORT DEFS: begin
        self._clk = self.clock("clk", 1)
        self._clk_en = self.input("clk_en", 1)
        self._reset = self.reset("reset")
        self._flush = self.input("flush", 1)

        self._wen = self.input("wen", 1)
        self._depth = self.input("depth", 16)
        self._stencil_width = self.input("stencil_width", 16)
        self._num_words_mem = self.input("num_words_mem", 16)

        self._valid = self.output("valid", 1)
        self._ren_to_fifo = self.output("ren_to_fifo", 1)
        # PORT DEFS: end

        # LOCAL SIGNALS: begin
        self._vg_ctr = self.var("vg_ctr", 16)
        self._valid_gate = self.var("valid_gate", 1)
        self._valid_int = self.var("valid_int", 1)
        self._threshold = self.var("threshold", 1)
        # LOCAL SIGNALS: end

        # GENERATION LOGIC: begin
        self.wire(self._valid_gate,
                  ternary((self._stencil_width == const(0, 16)),
                          const(1, 1),
                          (self._vg_ctr > (self._stencil_width - const(1, 16)))))
        self.wire(self._valid_int,
                  (self._num_words_mem >= (self._depth - 1)) &
                  self._wen & self._threshold & (self._depth > 0))
        self.wire(self._valid, self._valid_gate & self._valid_int)
        self.wire(self._ren_to_fifo, (self._num_words_mem >= (self._depth - 1)) &
                  self._wen & (self._depth > 0))

        self.add_code(self.threshold_update)
        self.add_code(self.vg_ctr_update)
        # GENERATION LOGIC: end

    @always((posedge, "clk"), (posedge, "reset"))
    def threshold_update(self):
        if(self._reset):
            self._threshold = 0
        elif self._clk_en:
            if self._flush:
                self._threshold = 0
            elif (self._num_words_mem == (self._depth - 1)) & self._wen:
                self._threshold = 1

    @always((posedge, "clk"), (posedge, "reset"))
    def vg_ctr_update(self):
        if(self._reset):
            self._vg_ctr = 0
        elif self._clk_en:
            if self._flush:
                self._vg_ctr = 0
            elif self._valid_int:
                if self._vg_ctr == (self._depth - 1):
                    self._vg_ctr = 0
                else:
                    self._vg_ctr = self._vg_ctr + 1


if __name__ == "__main__":
    dut = LineBufferControl()
    verilog(dut, filename="linebuffer_control.sv", check_active_high=False)
