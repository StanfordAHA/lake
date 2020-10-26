from kratos import *
import operator
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.modules.addr_gen import AddrGen
from lake.passes.passes import lift_config_reg


class SchedGen(Generator):
    '''
    Generate schedule
    '''
    def __init__(self,
                 iterator_support=6,
                 config_width=16):

        super().__init__(f"sched_gen_{iterator_support}_{config_width}")

        self.iterator_support = iterator_support
        self.config_width = config_width

        # clock
        self.clk = self.clock("clk")
        # asynchronous active low reset
        self.rst_n = self.reset("rst_n")

        # ready and valids
        self.valid_in = self.input("valid_in", 1)
        self.valid_out = self.output("valid_out", 1)
        self.ready = self.output("ready", 1)

        self.step = self.input("step", 1)

        # config reg
        self.steady_state = self.input("steady_state",
                width = self.config_width)

        # use this config reg as update chunk size?
        # write addressor extent
        # self.write_ad_extent0 = self.input("write_ad_extent0",
        #         width=self.config_width)
        self.update_chunk_size = self.input("update_chunk_size",
                width = self.config_width)
        self.valid_in_count = self.var("valid_in_count",
                width=self.config_width)
        self.at_chunk_size = self.var("at_chunk_size", 1)
        
        self.wire(self.valid_out, self.at_chunk_size)

        self.add_code(self.set_valid_in_count)
        self.add_code(self.set_at_chunk_size)

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
    db_dut = SchedGen(iterator_support=6)
    verilog(db_dut,
            filename="sched_gen.sv",
            additional_passes={"lift config regs": lift_config_reg})
