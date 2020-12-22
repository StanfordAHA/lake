from kratos import *
from lake.passes.passes import lift_config_reg
from lake.modules.sram_wrapper import SRAMWrapper
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.utils.util import extract_formal_annotation
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint


class SRAMFormal(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 input_addr_iterator_support=6,
                 output_addr_iterator_support=6,
                 input_sched_iterator_support=6,
                 output_sched_iterator_support=6,
                 config_width=16,
                 #  output_config_width=16,
                 interconnect_input_ports=1,  # Connection to int
                 interconnect_output_ports=1,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4):
        super().__init__("sram_formal", debug=True)

        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.mem_depth = mem_depth
        self.banks = banks
        self.data_width = data_width
        self.config_width = config_width
        self.input_addr_iterator_support = input_addr_iterator_support
        self.output_addr_iterator_support = output_addr_iterator_support
        self.input_sched_iterator_support = input_sched_iterator_support
        self.output_sched_iterator_support = output_sched_iterator_support

        self.default_iterator_support = 6
        self.default_config_width = 16
        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))

        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.increment_cycle_count)

        self._data_in = self.input("data_in", self.data_width,
                                   size=self.fetch_width,
                                   packed=True)
        self._data_in.add_attribute(FormalAttr(f"{self._data_in.name}", FormalSignalConstraint.SEQUENCE))

        self._data_out = self.output("data_out", self.data_width,
                                     size=self.fetch_width,
                                     packed=True,
                                     explicit_array=True)
        self._data_out.add_attribute(FormalAttr(f"{self._data_out.name}", FormalSignalConstraint.SEQUENCE))

        self._addr = self.var("addr", clog2(self.mem_depth))

        # Connect up the write to valid in for sequence
        self._write = self.var("write", 1)
        self._valid_in = self.output("valid_in", 1)
        self._valid_in.add_attribute(FormalAttr(f"{self._valid_in.name}", FormalSignalConstraint.SEQUENCE))
        self.wire(self._write, self._valid_in)

        self._read = self.var("read", 1)
        self._wen_to_sram = self.var("wen_to_strg", 1, packed=True)
        self._cen_to_sram = self.var("cen_to_strg", 1, packed=True)
        self._valid_out = self.output("valid_out", 1)
        self._valid_out.add_attribute(FormalAttr(f"{self._valid_out.name}", FormalSignalConstraint.SEQUENCE))

        # Valid out should just be if a read was on the previous cycle...
        self.add_code(self.set_valid_out)

        self.wire(self._wen_to_sram, self._write)
        self.wire(self._cen_to_sram, self._write | self._read)

        self._write_addr = self.var("write_addr", self.config_width)
        self._read_addr = self.var("read_addr", self.config_width)

        fl_ctr_sram_wr = ForLoop(iterator_support=self.default_iterator_support,
                                 config_width=self.default_config_width)
        loop_itr = fl_ctr_sram_wr.get_iter()
        loop_wth = fl_ctr_sram_wr.get_cfg_width()

        self.add_child(f"sram_write_loops",
                       fl_ctr_sram_wr,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write)
        # Whatever comes through here should hopefully just pipe through seamlessly
        # addressor modules
        self.add_child(f"sram_write_addr_gen",
                       AddrGen(iterator_support=self.default_iterator_support,
                               config_width=self.default_config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._write,
                       mux_sel=fl_ctr_sram_wr.ports.mux_sel_out,
                       addr_out=self._write_addr,
                       restart=fl_ctr_sram_wr.ports.restart)

        # scheduler modules
        self.add_child(f"sram_write_sched_gen",
                       SchedGen(iterator_support=self.default_iterator_support,
                                config_width=self.default_config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       mux_sel=fl_ctr_sram_wr.ports.mux_sel_out,
                       finished=fl_ctr_sram_wr.ports.restart,
                       valid_output=self._write)

        # -------------------------------- Delineate new group -------------------------------
        fl_ctr_sram_rd = ForLoop(iterator_support=self.default_iterator_support,
                                 config_width=self.default_config_width)
        loop_itr = fl_ctr_sram_rd.get_iter()
        loop_wth = fl_ctr_sram_rd.get_cfg_width()

        self.add_child(f"sram_read_loops",
                       fl_ctr_sram_rd,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._read)

        for i in range(self.banks):
            mbank = SRAMWrapper(use_sram_stub=True,
                                sram_name="NA",
                                data_width=self.data_width,
                                fw_int=self.fetch_width,
                                mem_depth=self.mem_depth,
                                mem_input_ports=1,
                                mem_output_ports=1,
                                # address_width=self.config_width,
                                address_width=9,
                                bank_num=i,
                                num_tiles=1)

            self.add_child(f"mem_{i}", mbank,
                           clk=self._clk,
                           enable_chain_input=0,
                           enable_chain_output=0,
                           chain_idx_input=0,
                           chain_idx_output=0,
                           clk_en=1,
                           mem_data_in_bank=self._data_in,
                           mem_data_out_bank=self._data_out,
                           mem_addr_in_bank=self._addr,
                           mem_cen_in_bank=self._write | self._read,
                           mem_wen_in_bank=self._write,
                           wtsel=0,
                           # valid_data=,
                           rtsel=0)

        self.add_child(f"sram_read_addr_gen",
                       AddrGen(iterator_support=self.default_iterator_support,
                               config_width=self.default_config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       step=self._read,
                       mux_sel=fl_ctr_sram_rd.ports.mux_sel_out,
                       addr_out=self._read_addr,
                       restart=fl_ctr_sram_rd.ports.restart)

        self.add_child(f"sram_read_sched_gen",
                       SchedGen(iterator_support=self.default_iterator_support,
                                config_width=self.default_config_width),
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       mux_sel=fl_ctr_sram_rd.ports.mux_sel_out,
                       finished=fl_ctr_sram_rd.ports.restart,
                       valid_output=self._read)

        self.add_code(self.set_sram_addr)

    @always_comb
    def set_sram_addr(self):
        if self._write:
            self._addr = self._write_addr[clog2(self.mem_depth) - 1, 0]
        else:
            self._addr = self._read_addr[clog2(self.mem_depth) - 1, 0]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def increment_cycle_count(self):
        if ~self._rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def set_valid_out(self):
        if ~self._rst_n:
            self._valid_out = 0
        else:
            self._valid_out = self._cen_to_sram & ~self._wen_to_sram


if __name__ == "__main__":
    lake_dut = SRAMFormal()

    # Lift config regs and generate annotation
    lift_config_reg(lake_dut.internal_generator)
    extract_formal_annotation(lake_dut, "sram_formal_annotation.txt")

    verilog(lake_dut, filename="sram_formal.sv",
            optimize_if=False)
    # additional_passes={"lift config regs": lift_config_reg})
