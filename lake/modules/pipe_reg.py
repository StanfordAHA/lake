from kratos import *


class PipeReg(Generator):
    def __init__(self,
                 data_width,
                 stages):
        super().__init__("pipe_reg")
        self._data_in = self.input("data_in", data_width)
        self._data_out = self.output("data_out", data_width)
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._clk_en = self.input("clk_en", 1)

        self.stages = stages

        if stages > 1:
            self._reg_array = self.var("reg_array",
                                       data_width,
                                       size=stages,
                                       packed=True,
                                       explicit_array=True)
            self.add_code(self.stage_elab)
            self.add_code(self.set_out)
        elif stages == 1:
            self._reg_ = self.var("reg_", data_width)
            self.add_code(self.set_out_one_stage)
            self.wire(self._data_out, self._reg_)
        else:
            self.wire(self._data_out, self._data_in)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def stage_elab(self):
        if ~self._rst_n:
            self._reg_array = 0
        elif self._clk_en:
            self._reg_array[0] = self._data_in
            for i in range(self.stages - 1):
                self._reg_array[i + 1] = self._reg_array[i]

    @always_comb
    def set_out(self):
        # Combinationally set this to the last register in the stage array.
        self._data_out = self._reg_array[self.stages - 1]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_out_one_stage(self):
        if ~self._rst_n:
            self._reg_ = 0
        elif self._clk_en:
            self._reg_ = self._data_in


if __name__ == "__main__":
    stages = 3
    piperegdut = PipeReg(16, stages)
    verilog(piperegdut, filename="pipe_reg.sv")
