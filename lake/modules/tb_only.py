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
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.utils.util import safe_wire, add_counter, decode
import kratos as kts


class StrgUBTBOnly(Generator):
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
                 area_opt=True,
                 addr_fifo_depth=8,
                 reduced_id_config_width=10,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=2,
                 tb_height=2):

        super().__init__("strg_ub_tb_only")

        ##################################################################################
        # Capture constructor parameter...
        ##################################################################################
        self.fetch_width = mem_width // data_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.agg_height = agg_height
        self.tb_height = tb_height
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.config_width = config_width
        self.data_width = data_width
        self.input_addr_iterator_support = input_addr_iterator_support
        self.input_sched_iterator_support = input_sched_iterator_support
        self.area_opt = area_opt
        self.addr_fifo_depth = addr_fifo_depth
        self.reduced_id_config_width = reduced_id_config_width

        self.default_iterator_support = 6
        self.default_config_width = 16
        self.sram_iterator_support = 6
        self.agg_rd_addr_gen_width = 8

        ##################################################################################
        # IO
        ##################################################################################
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._cycle_count = self.input("cycle_count", 16)

        # data from SRAM
        self._sram_read_data = self.input("sram_read_data", self.data_width,
                                          size=self.fetch_width,
                                          packed=True,
                                          explicit_array=True)
        # read enable from SRAM
        self._t_read = self.input("t_read", self.interconnect_output_ports)

        # sram to tb for loop
        self._loops_sram2tb_mux_sel = self.input("loops_sram2tb_mux_sel",
                                                 width=max(clog2(self.default_iterator_support), 1),
                                                 size=self.interconnect_output_ports,
                                                 explicit_array=True,
                                                 packed=True)

        self._loops_sram2tb_restart = self.input("loops_sram2tb_restart",
                                                 width=1,
                                                 size=self.interconnect_output_ports,
                                                 explicit_array=True,
                                                 packed=True)

        self._valid_out = self.output("accessor_output", self.interconnect_output_ports)
        self._data_out = self.output("data_out", self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        if self.area_opt:
            self._tb_read_d_out = self.output("tb_read_d_out", self.interconnect_output_ports)
            self._tb_read_addr_d_out = self.output("tb_read_addr_d_out", 2 + clog2(self.agg_height),
                                                   size=self.interconnect_output_ports,
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

        self._tb_read_addr = self.var("tb_read_addr", 3 + max(1, clog2(self.tb_height)),
                                      size=self.interconnect_output_ports,
                                      packed=True,
                                      explicit_array=True)

        # write enable to tb, delayed 1 cycle from SRAM reads
        self._t_read_d1 = self.var("t_read_d1", self.interconnect_output_ports)
        # read enable for reads from tb
        self._tb_read = self.var("tb_read", self.interconnect_output_ports)

        # Break out valids...
        self.wire(self._valid_out, self._tb_read)

        # delayed input mux_sel and restart signals from sram read/tb write
        # for loop and scheduling
        self._mux_sel_d1 = self.var("mux_sel_d1",
                                    kts.clog2(self.default_iterator_support),
                                    size=self.interconnect_output_ports,
                                    packed=True,
                                    explicit_array=True)

        self._restart_d1 = self.var("restart_d1",
                                    width=1,
                                    size=self.interconnect_output_ports,
                                    explicit_array=True,
                                    packed=True)

        for i in range(self.interconnect_output_ports):
            # signals delayed by 1 cycle from SRAM
            @always_ff((posedge, "clk"), (negedge, "rst_n"))
            def delay_read():
                if ~self._rst_n:
                    self._t_read_d1[i] = 0
                    self._mux_sel_d1[i] = 0
                    self._restart_d1[i] = 0
                else:
                    self._t_read_d1[i] = self._t_read[i]
                    self._mux_sel_d1[i] = self._loops_sram2tb_mux_sel[i]
                    self._restart_d1[i] = self._loops_sram2tb_restart[i]
            self.add_code(delay_read)

        if self.area_opt:
            # configuration register for port 0 to make TB shared (depth 4 virtually)
            self._shared_tb_0 = self.input(f"shared_tb_0", 1)
            self._shared_tb_0.add_attribute(ConfigRegAttr("Treat the TB's as shared"))
            self._shared_tb_0.add_attribute(FormalAttr(self._shared_tb_0.name, FormalSignalConstraint.SOLVE))

            self._tb_write_sel_0 = self.var("tb_write_sel_0", max(1, clog2(self.interconnect_output_ports)))
            self._tb_read_sel_0 = self.var("tb_read_sel_0", max(1, clog2(self.interconnect_output_ports)))
            if self.interconnect_output_ports == 2:
                self.wire(self._tb_write_sel_0,
                          ternary(self._shared_tb_0,
                                  self._tb_write_addr[0][clog2(tb_height) + clog2(self.interconnect_output_ports) - 1,
                                                         clog2(tb_height)],
                                  const(0, self._tb_write_sel_0.width)))
                self.wire(self._tb_read_sel_0,
                          ternary(self._shared_tb_0,
                                  self._tb_read_addr[0][clog2(tb_height) + clog2(self.fetch_width) + clog2(self.interconnect_output_ports) - 1,
                                                        clog2(tb_height) + clog2(self.fetch_width)],
                                  const(0, self._tb_read_sel_0.width)))
            else:
                self.wire(self._tb_write_sel_0, const(0, self._tb_write_sel_0.width))
                self.wire(self._tb_read_sel_0, const(0, self._tb_read_sel_0.width))

        ##################################################################################
        # TB PATHS
        ##################################################################################
        for i in range(self.interconnect_output_ports):

            self.tb_iter_support = 6
            self.tb_addr_width = 4
            self.tb_range_width = 16

            _AG = AddrGen(iterator_support=self.default_iterator_support,
                          config_width=self.tb_addr_width)

            self.add_child(f"tb_write_addr_gen_{i}",
                           _AG,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._t_read_d1[i],
                           mux_sel=self._mux_sel_d1[i],
                           restart=self._restart_d1[i])
            safe_wire(gen=self, w_to=self._tb_write_addr[i], w_from=_AG.ports.addr_out)

            # READ FROM TB

            if self.area_opt:
                fl_ctr_tb_rd = ForLoop(iterator_support=self.tb_iter_support,
                                       config_width=self.reduced_id_config_width)
            else:
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

            tb2out_sg = SchedGen(iterator_support=self.tb_iter_support,
                                 # config_width=self.tb_addr_width),
                                 delay_addr=self.area_opt,
                                 addr_fifo_depth=self.addr_fifo_depth,
                                 config_width=16)
            self.add_child(f"tb_read_sched_gen_{i}",
                           tb2out_sg,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=fl_ctr_tb_rd.ports.mux_sel_out,
                           finished=fl_ctr_tb_rd.ports.restart,
                           valid_output=self._tb_read[i])
            if self.area_opt:
                self._delay_en = self.var(f"delay_en_{i}", 1)
                self._tb_read_d = self.var(f"tb_read_d_{i}", 1)
                self._tb_addr_fifo = self.var(f"tb_addr_fifo_{i}", 2 + clog2(self.agg_height),
                                              size=self.addr_fifo_depth,
                                              packed=True,
                                              explicit_array=True)
                self._addr_fifo_in = self.var(f"addr_fifo_in_{i}", 2 + clog2(self.agg_height))
                self._wr_ptr = self.var(f"wr_ptr_{i}", clog2(self.addr_fifo_depth))
                self._rd_ptr = self.var(f"rd_ptr_{i}", clog2(self.addr_fifo_depth))

                self.wire(self._delay_en, tb2out_sg.ports.delay_en_out)
                self.wire(self._tb_read_d, tb2out_sg.ports.valid_output_d)
                safe_wire(gen=self, w_to=self._addr_fifo_in, w_from=_AG.ports.addr_out)

                @always_ff((posedge, "clk"), (negedge, "rst_n"))
                def update_delayed_tb_addr_in():
                    if ~self._rst_n:
                        self._wr_ptr = 0
                        self._rd_ptr = 0
                        self._tb_addr_fifo = 0
                    elif self._delay_en:
                        if self._tb_read[i]:
                            self._tb_addr_fifo[self._wr_ptr] = self._addr_fifo_in
                            self._wr_ptr = self._wr_ptr + 1

                        if self._tb_read_d:
                            self._rd_ptr = self._rd_ptr + 1
                self.add_code(update_delayed_tb_addr_in)

                self.wire(self._tb_read_d_out[i], ternary(self._delay_en,
                                                          self._tb_read_d,
                                                          self._tb_read[i]))
                self.wire(self._tb_read_addr_d_out[i], ternary(self._delay_en,
                                                               self._tb_addr_fifo[self._rd_ptr],
                                                               self._addr_fifo_in))

            @always_comb
            def tb_to_out():
                if self.area_opt:
                    if i == 0:
                        self._data_out[i] = \
                            self._tb[self._tb_read_sel_0] \
                            [self._tb_read_addr[i][clog2(self.tb_height) + clog2(self.fetch_width) - 1, clog2(self.fetch_width)]] \
                            [self._tb_read_addr[i][clog2(self.fetch_width) - 1, 0]]
                    # its really a duplicated assignment due to a kratos bug
                    # see kratos issue #177
                    else:
                        self._data_out[i] = \
                            self._tb[i] \
                            [self._tb_read_addr[i][clog2(self.tb_height) + clog2(self.fetch_width) - 1, clog2(self.fetch_width)]] \
                            [self._tb_read_addr[i][clog2(self.fetch_width) - 1, 0]]
                else:
                    self._data_out[i] = \
                        self._tb[i] \
                        [self._tb_read_addr[i][clog2(self.tb_height) + clog2(self.fetch_width) - 1, clog2(self.fetch_width)]] \
                        [self._tb_read_addr[i][clog2(self.fetch_width) - 1, 0]]
            self.add_code(tb_to_out)

        @always_ff((posedge, "clk"))
        def tb_ctrl():
            for i in range(self.interconnect_output_ports):
                if self._t_read_d1[i]:
                    if self.area_opt:
                        if i == 0:
                            # shared TB case for port 0 only
                            self._tb[self._tb_write_sel_0][self._tb_write_addr[i][clog2(tb_height) - 1, 0]] = self._sram_read_data
                        # its really a duplicated assignment due to a kratos bug
                        # see kratos issue #177
                        else:
                            self._tb[i][self._tb_write_addr[i][clog2(tb_height) - 1, 0]] = self._sram_read_data
                    else:
                        # regular TB access
                        self._tb[i][self._tb_write_addr[i][clog2(tb_height) - 1, 0]] = self._sram_read_data
        self.add_code(tb_ctrl)


if __name__ == "__main__":
    lake_dut = StrgUBTBOnly()
    verilog(lake_dut, filename="strg_ub_tb_only.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
