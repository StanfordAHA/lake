from kratos import *
import operator
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.modules.addr_gen import AddrGen
from lake.passes.passes import lift_config_reg
from lake.modules.rv_schedgen import SchedGen


class SchedGenTop(Generator):
    '''
    Generate schedule
    '''
    def __init__(self):

        super().__init__(f"sched_gen_top")

        # clock
        self.clk = self.clock("clk")
        # asynchronous active low reset
        self.rst_n = self.reset("rst_n")

        self.valid_in = self.input("valid_in", 1)
        self.valid_out = self.output("valid_out", 1)

        agg_sched_gen = SchedGen(4, 6, 16)
        sram_sched_gen = SchedGen(512, 6, 16)
        tb_sched_gen = SchedGen(8, 6, 16)

        self.agg_ready = self.var("agg_ready", 1)
        self.agg_valid_out = self.var("agg_valid_out", 1)

        self.add_child("agg_sched_gen", agg_sched_gen,
           clk=self.clk,
           rst_n=self.rst_n,
           valid_in=self.valid_in & self.agg_ready,
           ready=self.agg_ready,
           valid_out=self.agg_valid_out)

        self.sram_ready = self.var("sram_ready", 1)
        self.sram_valid_out = self.var("sram_valid_out", 1)
        self.add_child("sram_sched_gen", sram_sched_gen,
           clk=self.clk,
           rst_n=self.rst_n,
           valid_in=self.agg_valid_out & self.sram_ready,
           ready=self.sram_ready,
           valid_out=self.sram_valid_out)

        self.tb_ready = self.var("tb_ready", 1)
        self.add_child("tb_sched_gen", tb_sched_gen,
           clk=self.clk,
           rst_n=self.rst_n,
           valid_in=self.sram_valid_out & self.tb_ready,
           ready=self.tb_ready,
           valid_out=self.valid_out)
        
        lift_config_reg(self.internal_generator)

if __name__ == "__main__":
    db_dut = SchedGenTop()
    verilog(db_dut,
            filename="top_sched_gen.sv")
            # additional_passes={"lift config regs": lift_config_reg})
