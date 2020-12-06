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
from lake.utils.util import safe_wire, add_counter, decode
import kratos as kts


class StrgUBVec(Generator):
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
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4,
                 tb_height=2):

        super().__init__("strg_ub_vec")

        ##################################################################################
        # Capture constructor parameter...
        ##################################################################################
        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.tb_height = tb_height
        self.mem_depth = mem_depth
        self.config_width = config_width
        self.data_width = data_width
        self.input_addr_iterator_support = input_addr_iterator_support
        self.input_sched_iterator_support = input_sched_iterator_support

        self.default_iterator_support = 6
        self.default_config_width = 16
        self.sram_iterator_support = 6
        self.agg_rd_addr_gen_width = 8

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._data_in = self.input("data_in", self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        # Create cycle counter to share...
        self._cycle_count = add_counter(self, "cycle_count", 16, clk=self._clk)

        self._data_from_sram = self.input("data_from_strg", self.data_width,
                                          size=self.fetch_width,
                                          packed=True)

        self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
        self._cen_to_sram = self.output("cen_to_strg", 1, packed=True)
        self._addr_to_sram = self.output("addr_out", clog2(self.mem_depth), packed=True)
        self._data_to_sram = self.output("data_to_strg", self.data_width,
                                         size=self.fetch_width,
                                         packed=True)

        self._valid_out = self.output("accessor_output", self.interconnect_output_ports)
        self._data_out = self.output("data_out", self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        ##################################################################################
        # AGG RELEVANT SIGNALS
        ##################################################################################
        # Create an input to agg write scheduler + addressor for each input
        # Also need an addressor for the mux in addition to the read addr
        self._agg = self.var(f"agg",
                             width=self.data_width,
                             size=(self.interconnect_input_ports,
                                   self.agg_height,
                                   self.fetch_width),
                             packed=True,
                             explicit_array=True)
        # The SRAM write is just the OR reduction of the aggregator reads
        self._agg_read = self.var("agg_read", self.interconnect_input_ports)

        # self.add_code(self.delay_read)

        self._agg_data_out = self.var(f"agg_data_out", self.data_width,
                                      size=(self.interconnect_input_ports,
                                            self.fetch_width),
                                      packed=True,
                                      explicit_array=True)
        self._agg_write = self.var("agg_write", self.interconnect_input_ports)
        # Make this based on the size
        self._agg_write_addr = self.var("agg_write_addr", 2 + clog2(self.agg_height),
                                        size=self.interconnect_input_ports,
                                        packed=True,
                                        explicit_array=True)
        self._agg_read_addr = self.var("agg_read_addr", max(1, clog2(self.agg_height)),
                                       size=self.interconnect_input_ports,
                                       packed=True,
                                       explicit_array=True)
        self._agg_read_addr_gen_out = self.var("agg_read_addr_gen_out", self.agg_rd_addr_gen_width,
                                               size=self.interconnect_input_ports,
                                               packed=True,
                                               explicit_array=True)
        self._s_write_addr = self.var("s_write_addr", self.config_width,
                                      size=self.interconnect_input_ports,
                                      packed=True,
                                      explicit_array=True)

        ##################################################################################
        # TB RELEVANT SIGNALS
        ##################################################################################
        self._tb = self.var("tb",
                            width=self.data_width,
                            size=(self.interconnect_output_ports,
                                  self.tb_height,
                                  self.fetch_width),
                            packed=True,
                            explicit_array=True)
        self._tb_write_addr = self.var("tb_write_addr", 2 + max(1, clog2(self.tb_height)),
                                       size=self.interconnect_output_ports,
                                       packed=True,
                                       explicit_array=True)
        self._tb_read_addr = self.var("tb_read_addr", 2 + max(1, clog2(self.tb_height)),
                                      size=self.interconnect_output_ports,
                                      packed=True,
                                      explicit_array=True)
        self._s_read_addr = self.var("s_read_addr",
                                     self.config_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        self._mux_sel_d1 = self.var("mux_sel_d1", kts.clog2(self.default_iterator_support), size=self.interconnect_output_ports,
                                    packed=True,
                                    explicit_array=True)
        self._t_read = self.var("t_read", self.interconnect_output_ports)
        self._t_read_d1 = self.var("t_read_d1", self.interconnect_output_ports)
        self._restart_d1 = self.var("restart_d1", self.interconnect_output_ports)
        self._tb_read = self.var("tb_read", self.interconnect_output_ports)

        ##################################################################################
        # SRAM RELEVANT SIGNALS
        ##################################################################################
        self._write = self.var("write", 1)
        self._read = self.var("read", 1)
        self._addr = self.var("addr", clog2(self.mem_depth))

        self._sram_write_data = self.var("sram_write_data", data_width,
                                         size=self.fetch_width,
                                         packed=True)
        self._sram_read_data = self.var("sram_read_data", self.data_width,
                                        size=self.fetch_width,
                                        packed=True,
                                        explicit_array=True)

        ##################################################################################
        # BEGIN GENERATION LOGIC
        ##################################################################################

        # Break out valids...
        self.wire(self._valid_out, self._tb_read)

        ##################################################################################
        # AGG PATHS
        ##################################################################################
        for i in range(self.interconnect_input_ports):

            self.agg_iter_support = 6
            self.agg_addr_width = 4
            self.agg_range_width = 16

            forloop_ctr = ForLoop(iterator_support=self.agg_iter_support,
                                  # config_width=self.default_config_width)
                                  config_width=self.agg_range_width)
            loop_itr = forloop_ctr.get_iter()
            loop_wth = forloop_ctr.get_cfg_width()

            self.add_child(f"loops_in2buf_{i}",
                           forloop_ctr,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_write[i])

            newAG = AddrGen(iterator_support=self.agg_iter_support,
                            config_width=self.agg_addr_width)
            self.add_child(f"agg_write_addr_gen_{i}",
                           newAG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_write[i],
                           mux_sel=forloop_ctr.ports.mux_sel_out,
                           restart=forloop_ctr.ports.restart)
            safe_wire(gen=self, w_to=self._agg_write_addr[i], w_from=newAG.ports.addr_out)

            newSG = SchedGen(iterator_support=self.agg_iter_support,
                             # config_width=self.agg_addr_width)
                             config_width=16)

            self.add_child(f"agg_write_sched_gen_{i}",
                           newSG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           mux_sel=forloop_ctr.ports.mux_sel_out,
                           finished=forloop_ctr.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self._agg_write[i])

            @always_ff((posedge, "clk"))
            def agg_ctrl():
                if self._agg_write[i]:
                    if self.agg_height == 1:
                        self._agg[i][0][self._agg_write_addr[i][clog2(self.fetch_width) - 1, 0]] = self._data_in[i]
                    else:
                        self._agg[i][self._agg_write_addr[i]
                                     [self._agg_write_addr[0].width - 1, clog2(self.fetch_width)]]\
                                    [self._agg_write_addr[i][clog2(self.fetch_width) - 1, 0]] = self._data_in[i]

            self.add_code(agg_ctrl)

            # Create for loop counters that can be shared across the input port selection and SRAM write
            fl_ctr_sram_wr = ForLoop(iterator_support=self.default_iterator_support,
                                     config_width=self.default_config_width)

            self.add_child(f"loops_in2buf_autovec_write_{i}",
                           fl_ctr_sram_wr,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_read[i])

            newAG = AddrGen(iterator_support=self.default_iterator_support,
                            config_width=self.agg_addr_width)

            self.add_child(f"agg_read_addr_gen_{i}",
                           newAG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_read[i],
                           #  (self._input_port_sel_addr == const(i, self._input_port_sel_addr.width))),
                           mux_sel=fl_ctr_sram_wr.ports.mux_sel_out,
                           restart=fl_ctr_sram_wr.ports.restart)
            safe_wire(gen=self, w_to=self._agg_read_addr_gen_out[i], w_from=newAG.ports.addr_out)
            self.wire(self._agg_read_addr[i], self._agg_read_addr_gen_out[i][self._agg_read_addr.width - 1, 0])

            # Now pick out the data from the agg...
            @always_comb
            def get_agg_data():
                self._agg_data_out[i] = self._agg[i][self._agg_read_addr[i]]
            self.add_code(get_agg_data)

            # scheduler modules
            self.add_child(f"agg_read_sched_gen_{i}",
                           SchedGen(iterator_support=self.default_iterator_support,
                                    # config_width=self.mem_addr_width),
                                    config_width=16),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=fl_ctr_sram_wr.ports.mux_sel_out,
                           finished=fl_ctr_sram_wr.ports.restart,
                           valid_output=self._agg_read[i])

            self.mem_addr_width = clog2(self.mem_depth)

            _AG = AddrGen(iterator_support=self.default_iterator_support,
                          config_width=self.mem_addr_width)
            self.add_child(f"input_addr_gen_{i}",
                           _AG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._agg_read[i],
                           mux_sel=fl_ctr_sram_wr.ports.mux_sel_out,
                           restart=fl_ctr_sram_wr.ports.restart)
            safe_wire(gen=self, w_to=self._s_write_addr[i], w_from=_AG.ports.addr_out)

        ##################################################################################
        # TB PATHS
        ##################################################################################
        for i in range(self.interconnect_output_ports):

            loops_sram2tb = ForLoop(iterator_support=self.default_iterator_support,
                                    config_width=self.default_config_width)

            self.add_child(f"loops_buf2out_autovec_read_{i}",
                           loops_sram2tb,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._t_read[i])

            _AG = AddrGen(iterator_support=self.default_iterator_support,
                          config_width=self.mem_addr_width)
            self.add_child(f"output_addr_gen_{i}",
                           _AG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._t_read[i],
                           mux_sel=loops_sram2tb.ports.mux_sel_out,
                           restart=loops_sram2tb.ports.restart)
            safe_wire(gen=self, w_to=self._s_read_addr[i], w_from=_AG.ports.addr_out)

            self.add_child(f"output_sched_gen_{i}",
                           SchedGen(iterator_support=self.default_iterator_support,
                                    # config_width=self.default_config_width),
                                    config_width=16),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=loops_sram2tb.ports.mux_sel_out,
                           finished=loops_sram2tb.ports.restart,
                           valid_output=self._t_read[i])

            self.tb_iter_support = 6
            self.tb_addr_width = 4
            self.tb_range_width = 16

            _AG = AddrGen(iterator_support=self.default_iterator_support,
                          config_width=self.tb_addr_width)

            @always_ff((posedge, "clk"), (negedge, "rst_n"))
            def delay_read():
                if ~self._rst_n:
                    self._t_read_d1[i] = 0
                    self._mux_sel_d1[i] = 0
                    self._restart_d1[i] = 0
                else:
                    self._t_read_d1[i] = self._t_read[i]
                    self._mux_sel_d1[i] = loops_sram2tb.ports.mux_sel_out
                    self._restart_d1[i] = loops_sram2tb.ports.restart
            self.add_code(delay_read)

            self.add_child(f"tb_write_addr_gen_{i}",
                           _AG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._t_read_d1[i],
                           mux_sel=self._mux_sel_d1[i],
                           restart=self._restart_d1[i])
            safe_wire(gen=self, w_to=self._tb_write_addr[i], w_from=_AG.ports.addr_out)

            @always_ff((posedge, "clk"))
            def tb_ctrl():
                if self._t_read_d1[i]:
                    self._tb[i][self._tb_write_addr[i][0]] = \
                        self._sram_read_data
            self.add_code(tb_ctrl)

            ##### READ FROM TB

            fl_ctr_tb_rd = ForLoop(iterator_support=self.tb_iter_support,
                                   config_width=self.tb_range_width)

            self.add_child(f"loops_buf2out_read_{i}",
                           fl_ctr_tb_rd,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._tb_read[i])

            _AG = AddrGen(iterator_support=self.tb_iter_support,
                          config_width=self.tb_addr_width)
            self.add_child(f"tb_read_addr_gen_{i}",
                           _AG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._tb_read[i],
                           # addr_out=self._tb_read_addr[i])
                           mux_sel=fl_ctr_tb_rd.ports.mux_sel_out,
                           restart=fl_ctr_tb_rd.ports.restart)
            safe_wire(gen=self, w_to=self._tb_read_addr[i], w_from=_AG.ports.addr_out)

            self.add_child(f"tb_read_sched_gen_{i}",
                           SchedGen(iterator_support=self.tb_iter_support,
                                    # config_width=self.tb_addr_width),
                                    config_width=16),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=fl_ctr_tb_rd.ports.mux_sel_out,
                           finished=fl_ctr_tb_rd.ports.restart,
                           valid_output=self._tb_read[i])

            @always_comb
            def tb_to_out():
                self._data_out[i] = self._tb[i][self._tb_read_addr[i][clog2(self.tb_height) +
                                                                      clog2(self.fetch_width) - 1,
                                                                      clog2(self.fetch_width)]][self._tb_read_addr[i][clog2(self.fetch_width) - 1, 0]]
            self.add_code(tb_to_out)

        ##################################################################################
        # WIRE TO SRAM INTERFACE
        ##################################################################################
        # Now select the write address as a decode of the underlying enables
        self.wire(self._addr_to_sram, self._addr)
        self.wire(self._data_to_sram, self._sram_write_data)
        self.wire(self._data_from_sram, self._sram_read_data)
        self.wire(self._wen_to_sram, self._write)
        self.wire(self._cen_to_sram, self._write | self._read)
        self.wire(self._write, self._agg_read.r_or())
        self.wire(self._read, self._t_read.r_or())
        self.wire(self._sram_write_data, decode(self, self._agg_read, self._agg_data_out))
        self._write_addr = decode(self, self._agg_read, self._s_write_addr)
        self._read_addr = decode(self, self._t_read, self._s_read_addr)
        self.add_code(self.set_sram_addr)

    @always_comb
    def set_sram_addr(self):
        if self._write:
            self._addr = self._write_addr[clog2(self.mem_depth) - 1, 0]
        else:
            self._addr = self._read_addr[clog2(self.mem_depth) - 1, 0]


if __name__ == "__main__":
    lake_dut = StrgUBVec()
    verilog(lake_dut, filename="strg_ub_vec.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
