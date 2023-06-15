from kratos import *
import kratos as kts


class ALU(Generator):
    def __init__(self, data_width=16):
        super().__init__(name=f"alu_{data_width}")

        self.data_width = data_width

        self._data_in = self.input("data_in", self.data_width, size=2, explicit_array=True, packed=True)
        self._data_out = self.output("data_out", self.data_width)

        self._op = self.input("op", 1)
        self._execute_op = self.input("execute_op", 1)

        self.add_code(self.ALU_comb)

    @always_comb
    def ALU_comb(self):
        if self._execute_op:
            if self._op == 0:
                self._data_out = self._data_in[0] + self._data_in[1]
            elif self._op == 1:
                self._data_out = self._data_in[0] * self._data_in[1]
            else:
                self._data_out = 0
        else:
            self._data_out = 0


if __name__ == "__main__":

    alu_dut = ALU(data_width=16)

    verilog(alu_dut, filename="alu.sv",
            optimize_if=False)
