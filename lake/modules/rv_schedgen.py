from kratos import *
import operator
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.modules.addr_gen import AddrGen
from lake.passes.passes import lift_config_reg


class SchedGenRV(Generator):
    '''
    Generate schedule
    '''

    def __init__(self,
                 capacity=4,
                 iterator_support=6,
                 config_width=16,
                 data_in_width=1,
                 data_out_width=4,
                 read_width=4,
                 write_width=1):

        super().__init__(f"sched_gen_{iterator_support}_{config_width}")

        self.capacity = capacity
        self.iterator_support = iterator_support
        self.config_width = config_width
        self.data_in_width = data_in_width
        self.data_out_width = data_out_width
        self.read_width = read_width
        self.write_width = write_width

        # clock
        self.clk = self.clock("clk")
        # asynchronous active low reset
        self.rst_n = self.reset("rst_n")

        # ready and valids
        self.valid_in = self.input("valid_in", 1)
        self.did_read = self.input("did_read", 1)
        self.valid_out = self.output("valid_out", 1)
        self.ready = self.output("ready", 1)
        self.write = self.var("write", 1)
        self.write_out = self.output("write_out", 1)
        self.wire(self.write, self.valid_in & self.ready)
        self.wire(self.write_out, self.write)

        self.valid_in_count = self.var("valid_count", 16)
        self.write_count = self.var("write_count", clog2(self.capacity)+1)
        self.read_count = self.var("read_count", clog2(self.capacity)+1)

        self.add_code(self.set_valid_in_count)
        self.add_code(self.set_read_count)
        self.add_code(self.set_ready)
        self.add_code(self.set_valid_out)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_valid_in_count(self):
        if ~self.rst_n:
            self.valid_in_count = 0
        else:
            if self.write & self.did_read:
                self.valid_in_count = self.valid_in_count + self.write_width - self.read_width
            elif self.write:
                self.valid_in_count = self.valid_in_count + self.write_width
            elif self.did_read:
                self.valid_in_count = self.valid_in_count - (self.read_width)
    
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_read_count(self):
        if ~self.rst_n:
            self.read_count = 0
        elif self.read_count == self.read_width:
            if self.write:
                self.read_count = 1
            else:
                self.read_count = 0
        elif self.write:
            self.read_count = self.read_count + 1

    @always_comb
    def set_ready(self):
        if self.valid_in_count < self.capacity:
            self.ready = 1
        else:
            self.ready = 0

    @always_comb
    def set_valid_out(self):
        if (self.data_out_width == self.read_width):
            if self.valid_in_count >= self.read_width:
                self.valid_out = 1
            else:
                self.valid_out = 0
        elif self.read_count == self.read_width:
            self.valid_out = 1
        else:
            self.valid_out = 0


if __name__ == "__main__":
    db_dut = SchedGenRV(iterator_support=6)
    verilog(db_dut,
            filename="sched_gen.sv",
            additional_passes={"lift config regs": lift_config_reg})
