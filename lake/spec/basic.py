from kratos import *
from functools import reduce
import operator

class Counter(Generator):
    '''
    Create a counter for <var type> hw codegen
    '''

    def __init__(self):

        super().__init__(f"counter")

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._bound = self.input("bound", 32)
        self._current_val = self.output("current_val", 32)

        self.add_code(self.counter_update)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def counter_update(self):
       if ~self._rst_n:
           self._current_val = 0
       else:
           if self._current_val == self._bound:
               self._current_val = 0
           else:
               self._current_val += 1

class PWA(Generator):
    '''
    create a hardware for <expr type>
    '''

    def __init__(self,
                 piece_num,
                 input_dim):

        super().__init__(f"counter")

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self.piece_num = piece_num
        self.input_dim = input_dim

        self._lb_arr = self.input("lb", 32, size = self.piece_num,
                                  packed = True, explicit_array = True)
        self._ub_arr = self.input("ub", 32, size = self.piece_num,
                                  packed = True, explicit_array = True)
        self._weight = self.input("weight", 32,
                                  size = [self.piece_num, self.input_dim + 1],
                                  packed = True, explicit_array = True)
        self._iter = self.input("iterator", 32, size = self.input_dim,
                                  packed = True, explicit_array = True)
        self._choice = self.var("choice", 32)
        self._choice_flatten = self.var("ch_flat", 1, size=self.piece_num,
                                        packed = True, explicit_array = True)
        self._tmp_mul= self.var("tmp_mul", 32, size = self.input_dim,
                            packed = True, explicit_array = True)

        self._value = self.output("value", 32)
        self.add_code(self.piece_sel)
        self.add_code(self.multiply)
        self.add_code(self.calc_pwaff_expr)

    @always_comb
    def piece_sel(self):
        for i in range(self.piece_num):
            self._choice_flatten[i] = reduce(lambda a, b: a and b,
                                            [(self._iter[j] >= self._lb_arr[j] and \
                                              self._iter[j] <= self._ub_arr[j]) \
                                             for j in range(input_dim)])

        self._choice = reduce(operator.add, [self._choice_flatten[i]<<i for i in range(self.piece_num)])

    @always_comb
    def multiply(self):
        for i in range(self.input_dim):
            self._tmp_mul[i] = self._iter[i] * self._weight[self._choice][i]


    @always_comb
    def calc_pwaff_expr(self):
        self._value - reduce(operator.add,
                             [self._tmp_mul[i] for i in range(self.input_dim)]
                             + self._weight[self.choice][-1])


if __name__ == "__main__":
    counter_dut = Counter()
    verilog(counter_dut, filename = "counter.sv")
    pwa_dut = PWA(piece_num = 1, input_dim = 2)
    verilog(pwa_dut, filename = "pwa.sv")


