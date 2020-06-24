from kratos import *

class Counter(Generator):
    '''
    Create a counter for <var type> hw codegen
    '''

    def __init__(self):

        super().__init__(f"counter")

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._bound = self.input("bound", 32)
        self._current_val = self.var("current_val", 32)

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

if __name__ == "__main__":
    counter_dut = Counter()
    verilog(counter_dut, filename = "counter.sv")


