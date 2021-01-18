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
                 ext_cycle_count=False,
                 read_width=4,
                 write_width=4):

        super().__init__(f"sched_gen_{iterator_support}_{config_width}")

        self.iterator_support = iterator_support
        self.config_width = config_width
        self.read_width = read_width
        self.write_width = write_width

        # clock
        self.clk = self.clock("clk")
        # asynchronous active low reset
        self.rst_n = self.reset("rst_n")

        # ready and valids
        self.valid_in = self.input("valid_in", 1)
        self.valid_out = self.output("valid_out", 1)
        self.ready = self.output("ready", 1)

        # config reg
        self.starting_addr = self.input("starting_addr",
                                        width=self.config_width)
        self.starting_addr.add_attribute(ConfigRegAttr("starting addr"))
        self.range_product = self.input("range_product",
                                        width=self.iterator_support * self.config_width)
        self.range_product.add_attribute(ConfigRegAttr("range product"))

        if ext_cycle_count:
            self.cycle_counter = self.input("cycle_counter", 16)
        else:
            self.cycle_counter = self.var("cycle_counter", 16)
        
        self.write_valid_count = self.var("write_valid_count", max(1, clog2(self.write_width)))
        self.valid_count = self.var("valid_count",
                                    self.iterator_support * self.config_width)


        self.add_code(self.set_write_valid_count)
        self.add_code(self.set_valid_count)
        self.add_code(self.set_valid_out)
        # self.add_code(self.set_cycle_counter)
        self.add_code(self.set_ready)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_write_valid_count(self):
        if ~self.rst_n:
            self.write_valid_count = 0
        elif self.valid_in:
            if self.write_valid_count == self.write_width - 1:
                self.write_valid_count = 0
            else:
                self.write_valid_count = self.write_valid_count + 1
    
    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_valid_count(self):
        if ~self.rst_n:
            self.valid_count = 0
        elif self.valid_count == self.range_product - 1:
            self.valid_count = 0
        elif self.write_valid_count == self.write_width - 1:
            self.valid_count = self.valid_count + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_valid_out(self):
        if ~self.rst_n:
            self.valid_out = 0
        elif self.valid_count > self.read_width:
            self.valid_out = 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_ready(self):
        if ~self.rst_n:
            self.ready = 0
        elif self.cycle_counter < self.starting_addr:
            self.ready = 0
        elif self.valid_count < self.range_product:
            self.ready = 1
        else:
            self.ready = 0

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_cycle_counter(self):
        if ~self.rst_n:
            self.cycle_counter = 0
        elif self.cycle_counter < self.starting_addr:
            self.cycle_counter = self.cycle_counter + 1
        elif self.valid_count < self.range_product:
            self.cycle_counter = self.cycle_counter
        else:
            self.cycle_counter = self.cycle_counter

    @always_comb
    def set_at_chunk_size(self):
        self.at_chunk_size = (self.valid_in_count == self.update_chunk_size - 1)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_valid_in_count(self):
        if ~self.rst_n:
            self.valid_in_count = 0
        elif self.at_chunk_size:
            self.valid_in_count = 0
        elif self.valid_in == 1:
            self.valid_in_count = self.valid_in_count + 1


if __name__ == "__main__":
    db_dut = SchedGenRV(iterator_support=6)
    verilog(db_dut,
            filename="sched_gen.sv",
            additional_passes={"lift config regs": lift_config_reg})
