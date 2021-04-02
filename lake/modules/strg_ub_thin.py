from lake.utils.util import get_priority_encode
from kratos import *
from lake.modules.passthru import *
from lake.modules.register_file import RegisterFile
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.range_group import RangeGroupAttr
from lake.passes.passes import lift_config_reg
from lake.modules.sram_stub import SRAMStub
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
import kratos as kts


class StrgUBThin(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=16,
                 mem_depth=512,
                 input_addr_iterator_support=6,
                 input_sched_iterator_support=6,
                 output_addr_iterator_support=6,
                 output_sched_iterator_support=6,
                 interconnect_input_ports=1,  # Connection to int
                 interconnect_output_ports=1,
                 config_width=16,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=True,
                 gen_addr=True):

        super().__init__("strg_ub_thin", debug=True)

        assert mem_width == data_width, f"This module should only be used when the fetch width is 1!"

        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_depth = mem_depth
        self.config_width = config_width
        self.data_width = data_width
        self.input_addr_iterator_support = input_addr_iterator_support
        self.input_sched_iterator_support = input_sched_iterator_support
        self.output_addr_iterator_support = output_addr_iterator_support
        self.output_sched_iterator_support = output_sched_iterator_support
        self.rw_same_cycle = rw_same_cycle
        self.read_delay = read_delay
        self.gen_addr = gen_addr
        self.default_iterator_support = 6
        self.default_config_width = 16
        # generation parameters
        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._data_in = self.input("data_in", self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        # outputs
        self._data_out = self.output("data_out", self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        self._data_to_sram = self.output("data_to_strg", self.data_width,
                                         size=self.fetch_width,
                                         packed=True)
        self._data_from_sram = self.input("data_from_strg", self.data_width,
                                          size=self.fetch_width,
                                          packed=True)
        # Early out in case...
        if self.gen_addr is False:
            # Pass through write enable, addr data and
            # read enable, addr data
            self._read = self.input("ren_in", 1)
            self._write = self.input("wen_in", 1)
            self._write_addr = self.input("write_addr", self.config_width)
            self._read_addr = self.input("read_addr", self.config_width)
            self._cen_to_sram = self.output("cen_to_strg", 1, packed=True)
            self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
            self._ren_to_sram = self.output("ren_to_strg", 1, packed=True)
            self._wr_addr_to_sram = self.output("wr_addr_out", clog2(self.mem_depth), packed=True)
            self._rd_addr_to_sram = self.output("rd_addr_out", clog2(self.mem_depth), packed=True)
            self._accessor_output = self.output("accessor_output", self.interconnect_output_ports)
            self.wire(self._accessor_output, self._read)
            self.wire(self._cen_to_sram, self._write | self._read)
            self.wire(self._wen_to_sram, self._write)
            self.wire(self._ren_to_sram, self._read)
            self.wire(self._data_out, self._data_from_sram)
            self.wire(self._data_in, self._data_to_sram)
            self.wire(self._wr_addr_to_sram, self._write_addr[clog2(self.mem_depth) - 1, 0])
            self.wire(self._rd_addr_to_sram, self._read_addr[clog2(self.mem_depth) - 1, 0])

            return

        # Create cycle counter to share...
        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.increment_cycle_count)

        # local variables
        self._write = self.var("write", self.interconnect_input_ports)
        self._read = self.var("read", self.interconnect_output_ports)
        self._accessor_output = self.output("accessor_output", self.interconnect_output_ports)
        self.wire(self._accessor_output, self._read)

        self._valid_out = self.output("valid_out", self.interconnect_output_ports)
        if self.read_delay == 1:
            self._read_d1 = self.var("read_d1", self.interconnect_output_ports)
            self.add_code(self.delay_read)
            self.wire(self._valid_out, self._read_d1)
        else:
            self.wire(self._valid_out, self._read)

        self._write_addr = self.var("write_addr", self.config_width, size=self.interconnect_input_ports, explicit_array=True)
        self._read_addr = self.var("read_addr", self.config_width, size=self.interconnect_output_ports, explicit_array=True)
        self._addr = self.var("addr", clog2(self.mem_depth))

        # Set up addr/cycle gens for input side
        for i in range(self.interconnect_input_ports):

            FOR_LOOP_WRITE = ForLoop(iterator_support=self.default_iterator_support,
                                     config_width=self.default_config_width)

            ADDR_WRITE = AddrGen(iterator_support=self.default_iterator_support,
                                 config_width=self.default_config_width)
            SCHED_WRITE = SchedGen(iterator_support=self.default_iterator_support,
                                   config_width=self.default_config_width)

            self.add_child(f"in2sram_for_loop_{i}",
                           FOR_LOOP_WRITE,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._write[i])

            # Whatever comes through here should hopefully just pipe through seamlessly
            # addressor modules
            self.add_child(f"in2sram_addr_gen_{i}",
                           ADDR_WRITE,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._write[i],
                           mux_sel=FOR_LOOP_WRITE.ports.mux_sel_out,
                           restart=FOR_LOOP_WRITE.ports.restart,
                           addr_out=self._write_addr[i])

            # scheduler modules
            self.add_child(f"in2sram_sched_gen_{i}",
                           SCHED_WRITE,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=FOR_LOOP_WRITE.ports.mux_sel_out,
                           finished=FOR_LOOP_WRITE.ports.restart,
                           valid_output=self._write[i])

        # Set up addr/cycle gens for output side
        for i in range(self.interconnect_output_ports):

            FOR_LOOP_READ = ForLoop(iterator_support=self.default_iterator_support,
                                    config_width=self.default_config_width)

            ADDR_READ = AddrGen(iterator_support=self.default_iterator_support,
                                config_width=self.default_config_width)
            SCHED_READ = SchedGen(iterator_support=self.default_iterator_support,
                                  config_width=self.default_config_width)

            self.add_child(f"sram2out_for_loop_{i}",
                           FOR_LOOP_READ,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._read[i])

            self.add_child(f"sram2out_addr_gen_{i}",
                           ADDR_READ,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._read[i],
                           mux_sel=FOR_LOOP_READ.ports.mux_sel_out,
                           restart=FOR_LOOP_READ.ports.restart,
                           addr_out=self._read_addr[i])

            self.add_child(f"sram2out_sched_gen_{i}",
                           SCHED_READ,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=FOR_LOOP_READ.ports.mux_sel_out,
                           finished=FOR_LOOP_READ.ports.restart,
                           valid_output=self._read[i])
        # -------------------------------- Delineate new group -------------------------------

        # Now deal with dual_port/single_port madness...
        self._cen_to_sram = self.output("cen_to_strg", 1, packed=True)
        self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
        self._ren_to_sram = self.output("ren_to_strg", 1, packed=True)
        self.wire(self._cen_to_sram, self._write.r_or() | self._read.r_or())
        self.wire(self._wen_to_sram, self._write.r_or())
        self.wire(self._ren_to_sram, self._read.r_or())

        for i in range(self.interconnect_output_ports):
            self.wire(self._data_out[i], self._data_from_sram)

        if self.rw_same_cycle:
            # If we can read and write the same cycle we
            # can pretty safeuly assume we have separate read/write ports...

            # Handle write side....
            # Send the address/data based on the lowest writer on the port
            self._wr_addr_to_sram = self.output("wr_addr_out", clog2(self.mem_depth), packed=True)
            pri_enc_wr = get_priority_encode(self, self._write)
            self.wire(self._wr_addr_to_sram, self._write_addr[pri_enc_wr][clog2(self.mem_depth) - 1, 0])
            self.wire(self._data_to_sram, self._data_in[pri_enc_wr])

            # Read side...
            pri_enc_rd = get_priority_encode(self, self._read)
            self._rd_addr_to_sram = self.output("rd_addr_out", clog2(self.mem_depth), packed=True)
            self.wire(self._rd_addr_to_sram, self._read_addr[pri_enc_rd][clog2(self.mem_depth) - 1, 0])
        else:
            self._addr_to_sram = self.output("addr_out", clog2(self.mem_depth), packed=True)
            self.wire(self._data_to_sram, self._data_in[0])
            self.wire(self._addr_to_sram, self._addr)
            self.add_code(self.set_sram_addr)

    @always_comb
    def set_sram_addr(self):
        if self._write:
            self._addr = self._write_addr[clog2(self.mem_depth) - 1, 0]
        else:
            self._addr = self._read_addr[clog2(self.mem_depth) - 1, 0]

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def delay_read(self):
        if ~self._rst_n:
            self._read_d1 = 0
        else:
            self._read_d1 = self._read

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def increment_cycle_count(self):
        if ~self._rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1


if __name__ == "__main__":
    lake_dut = StrgUBThin()
    verilog(lake_dut, filename="strg_ub_thin.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
