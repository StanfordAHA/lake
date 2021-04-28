from lake.utils.parse_clkwork_config import extract_controller, map_controller
from kratos import *
from lake.modules.passthru import *
from lake.attributes.formal_attr import *
from lake.passes.passes import lift_config_reg
from lake.modules.agg_only import StrgUBAggOnly
from lake.modules.agg_sram_shared import StrgUBAggSRAMShared
from lake.modules.sram_only import StrgUBSRAMOnly
from lake.modules.sram_tb_shared import StrgUBSRAMTBShared
from lake.modules.tb_only import StrgUBTBOnly
from lake.utils.util import add_counter, decode
import os


class StrgUBVec(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 input_addr_iterator_support=6,
                 input_sched_iterator_support=6,
                 config_width=16,
                 #  output_config_width=16,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4,
                 tb_height=2,
                 agg_data_top=False):

        super().__init__("strg_ub_vec")

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
        self.banks = banks
        self.config_width = config_width
        self.data_width = data_width
        self.read_delay = read_delay
        self.rw_same_cycle = rw_same_cycle
        self.input_config_width = config_width
        self.input_addr_iterator_support = input_addr_iterator_support
        self.input_sched_iterator_support = input_sched_iterator_support

        self.input_iterator_support = 6
        self.output_iterator_support = 6
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

        self._data_from_sram = self.input("data_from_strg", self.data_width,
                                          size=self.fetch_width,
                                          packed=True)
        # The interface is slightly different when dealing with a dual port SRAM
        # This could be abstracted away, but for now it's slightly easier to handle this
        if self.rw_same_cycle:
            self._ren_to_sram = self.output("ren_to_strg", 1, packed=True)
            self._wr_addr_to_sram = self.output("wr_addr_out", clog2(self.mem_depth), packed=True)
            self._rd_addr_to_sram = self.output("rd_addr_out", clog2(self.mem_depth), packed=True)
        else:
            self._cen_to_sram = self.output("cen_to_strg", 1, packed=True)
            self._addr_to_sram = self.output("addr_out", clog2(self.mem_depth), packed=True)

        self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
        self._data_to_sram = self.output("data_to_strg", self.data_width,
                                         size=self.fetch_width,
                                         packed=True)

        self._valid_out = self.output("accessor_output", self.interconnect_output_ports)
        self._data_out = self.output("data_out", self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        ##################################################################################
        # CYCLE COUNTER
        ##################################################################################

        # Create cycle counter to share...
        self._cycle_count = add_counter(self, "cycle_count", 16)

        agg_only = StrgUBAggOnly(data_width=self.data_width,
                                 mem_width=self.mem_width,
                                 mem_depth=self.mem_depth,
                                 input_addr_iterator_support=self.input_iterator_support,
                                 input_sched_iterator_support=self.input_iterator_support,
                                 interconnect_input_ports=self.interconnect_input_ports,
                                 interconnect_output_ports=self.interconnect_output_ports,
                                 agg_height=self.agg_height,
                                 config_width=self.input_config_width)

        agg_sram_shared = StrgUBAggSRAMShared(data_width=self.data_width,
                                              mem_width=self.mem_width,
                                              mem_depth=self.mem_depth,
                                              banks=self.banks,
                                              input_addr_iterator_support=self.input_iterator_support,
                                              output_addr_iterator_support=self.output_iterator_support,
                                              input_sched_iterator_support=self.input_iterator_support,
                                              output_sched_iterator_support=self.output_iterator_support,
                                              interconnect_input_ports=self.interconnect_input_ports,
                                              interconnect_output_ports=self.interconnect_output_ports,
                                              read_delay=self.read_delay,
                                              rw_same_cycle=self.rw_same_cycle,
                                              agg_height=self.agg_height,
                                              config_width=self.input_config_width)

        sram_only = StrgUBSRAMOnly(data_width=self.data_width,
                                   mem_width=self.mem_width,
                                   mem_depth=self.mem_depth,
                                #    banks=self.banks,
                                   input_addr_iterator_support=self.input_iterator_support,
                                #    output_addr_iterator_support=self.output_iterator_support,
                                   input_sched_iterator_support=self.input_iterator_support,
                                #    output_sched_iterator_support=self.output_iterator_support,
                                   interconnect_input_ports=self.interconnect_input_ports,
                                   interconnect_output_ports=self.interconnect_output_ports,
                                   read_delay=self.read_delay,
                                   rw_same_cycle=self.rw_same_cycle,
                                   agg_height=self.agg_height,
                                   config_width=self.input_config_width)

        sram_tb_shared = StrgUBSRAMTBShared(data_width=self.data_width,
                                            mem_width=self.mem_width,
                                            mem_depth=self.mem_depth,
                                            banks=self.banks,
                                            input_addr_iterator_support=self.input_iterator_support,
                                            output_addr_iterator_support=self.output_iterator_support,
                                            input_sched_iterator_support=self.input_iterator_support,
                                            output_sched_iterator_support=self.output_iterator_support,
                                            interconnect_input_ports=self.interconnect_input_ports,
                                            interconnect_output_ports=self.interconnect_output_ports,
                                            read_delay=self.read_delay,
                                            rw_same_cycle=self.rw_same_cycle,
                                            agg_height=self.agg_height,
                                            config_width=self.input_config_width)

        tb_only = StrgUBTBOnly(data_width=self.data_width,
                               mem_width=self.mem_width,
                               mem_depth=self.mem_depth,
                               banks=self.banks,
                               input_addr_iterator_support=self.input_iterator_support,
                               output_addr_iterator_support=self.output_iterator_support,
                               input_sched_iterator_support=self.input_iterator_support,
                               output_sched_iterator_support=self.output_iterator_support,
                               interconnect_input_ports=self.interconnect_input_ports,
                               interconnect_output_ports=self.interconnect_output_ports,
                               read_delay=self.read_delay,
                               rw_same_cycle=self.rw_same_cycle,
                               agg_height=self.agg_height,
                               config_width=self.input_config_width)

        self.add_child("agg_only",
                       agg_only,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       data_in=self._data_in)

        self.add_child("agg_sram_shared",
                       agg_sram_shared,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count)

        self.add_child("sram_only",
                       sram_only,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       wen_to_sram=self._wen_to_sram,
                    #    addr_to_sram=self._addr_to_sram,
                       data_to_sram=self._data_to_sram)

        # Dual port/single port guard.
        if self.rw_same_cycle:
            self.wire(sram_only.ports.ren_to_sram, self._ren_to_sram)
            self.wire(sram_only.ports.wr_addr_to_sram, self._wr_addr_to_sram)
            self.wire(sram_only.ports.rd_addr_to_sram, self._rd_addr_to_sram)
        else:
            self.wire(sram_only.ports.cen_to_sram, self._cen_to_sram)
            self.wire(sram_only.ports.addr_to_sram, self._addr_to_sram)

        self.add_child("sram_tb_shared",
                       sram_tb_shared,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count)

        self.add_child("tb_only",
                       tb_only,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       sram_read_data=self._data_from_sram,
                       accessor_output=self._valid_out,
                       data_out=self._data_out)

        self.wire(agg_only.ports.agg_read, agg_sram_shared.ports.agg_read_out)
        self.wire(agg_only.ports.floop_mux_sel, agg_sram_shared.ports.floop_mux_sel)
        self.wire(agg_only.ports.floop_restart, agg_sram_shared.ports.floop_restart)

        self.wire(sram_only.ports.floop_mux_sel, agg_sram_shared.ports.floop_mux_sel)
        self.wire(sram_only.ports.floop_restart, agg_sram_shared.ports.floop_restart)
        self.wire(sram_only.ports.loops_sram2tb_mux_sel, sram_tb_shared.ports.loops_sram2tb_mux_sel)
        self.wire(sram_only.ports.loops_sram2tb_restart, sram_tb_shared.ports.loops_sram2tb_restart)
        self.wire(sram_only.ports.agg_read, agg_sram_shared.ports.agg_read_out)
        self.wire(sram_only.ports.t_read, sram_tb_shared.ports.t_read_out)
        self.wire(sram_only.ports.agg_data_out, agg_only.ports.agg_data_out)

        self.wire(tb_only.ports.t_read, sram_tb_shared.ports.t_read_out)
        self.wire(tb_only.ports.loops_sram2tb_mux_sel, sram_tb_shared.ports.loops_sram2tb_mux_sel)
        self.wire(tb_only.ports.loops_sram2tb_restart, sram_tb_shared.ports.loops_sram2tb_restart)

        if agg_data_top:
            self._agg_data_out = self.output(f"strg_ub_agg_data_out", self.data_width,
                                             size=(self.interconnect_input_ports,
                                                   self.fetch_width),
                                             packed=True,
                                             explicit_array=True)
            self.wire(self._agg_data_out, agg_only.ports.agg_data_out)

    def get_static_bitstream(self, config_path, in_file_name, out_file_name):

        config = []
        controllers = ["in2agg_0",
                       "in2agg_1",
                       "agg2sram_0",
                       "agg2sram_1",
                       "sram2tb_0",
                       "sram2tb_1",
                       "tb2out_0",
                       "tb2out_1"]

        controller_objs = [None] * len(controllers)
        for i in range(len(controllers)):
            c = controllers[i]
            in_path = config_path + '/' + in_file_name + c + '.csv'
            out_path = config_path + '/' + out_file_name + c + '.csv'

            if os.path.isfile(in_path):
                controller_objs[i] = map_controller(extract_controller(in_path), c)
            elif os.path.isfile(out_path):
                controller_objs[i] = map_controller(extract_controller(out_path), c)
            else:
                print(f"No {c} file provided. Is this expected?")

        in2agg_0, in2agg_1, agg2sram_0, agg2sram_1, \
            sram2tb_0, sram2tb_1, tb2out_0, tb2out_1 = \
            controller_objs

        if in2agg_0 is not None:
            config.append(("strg_ub_agg_only_agg_write_addr_gen_0_starting_addr", in2agg_0.in_data_strt))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_0_enable", 1))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_starting_addr", in2agg_0.cyc_strt))
            config.append(("strg_ub_agg_only_loops_in2buf_0_dimensionality", in2agg_0.dim))
            for i in range(in2agg_0.dim):
                config.append((f"strg_ub_agg_only_loops_in2buf_0_ranges_{i}", in2agg_0.extent[i]))
                config.append((f"strg_ub_agg_only_agg_write_addr_gen_0_strides_{i}", in2agg_0.in_data_stride[i]))
                config.append((f"strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_{i}", in2agg_0.cyc_stride[i]))

        if in2agg_1 is not None:
            config.append(("strg_ub_agg_only_agg_write_addr_gen_1_starting_addr", in2agg_1.in_data_strt))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_1_enable", 1))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_starting_addr", in2agg_1.cyc_strt))
            config.append(("strg_ub_agg_only_loops_in2buf_1_dimensionality", in2agg_1.dim))
            for i in range(in2agg_1.dim):
                config.append((f"strg_ub_agg_only_loops_in2buf_1_ranges_{i}", in2agg_1.extent[i]))
                config.append((f"strg_ub_agg_only_agg_write_addr_gen_1_strides_{i}", in2agg_1.in_data_stride[i]))
                config.append((f"strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_{i}", in2agg_1.cyc_stride[i]))

        if agg2sram_0 is not None:
            config.append(("strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_dimensionality", agg2sram_0.dim))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_0_enable", 1))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_starting_addr", agg2sram_0.cyc_strt))
            config.append(("strg_ub_agg_only_agg_read_addr_gen_0_starting_addr", agg2sram_0.out_data_strt))
            config.append(("strg_ub_sram_only_input_addr_gen_0_starting_addr", agg2sram_0.in_data_strt))
            for i in range(agg2sram_0.dim):
                config.append((f"strg_ub_agg_only_agg_read_addr_gen_0_strides_{i}", agg2sram_0.out_data_stride[i]))
                config.append((f"strg_ub_agg_sram_shared_loops_in2buf_autovec_write_0_ranges_{i}", agg2sram_0.extent[i]))
                config.append((f"strg_ub_sram_only_input_addr_gen_0_strides_{i}", agg2sram_0.in_data_stride[i]))
                config.append((f"strg_ub_agg_sram_shared_agg_read_sched_gen_0_sched_addr_gen_strides_{i}", agg2sram_0.cyc_stride[i]))

        if agg2sram_1 is not None:
            config.append(("strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_dimensionality", agg2sram_1.dim))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_1_enable", 1))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_starting_addr", agg2sram_1.cyc_strt))
            config.append(("strg_ub_agg_only_agg_read_addr_gen_1_starting_addr", agg2sram_1.out_data_strt))
            config.append(("strg_ub_sram_only_input_addr_gen_1_starting_addr", agg2sram_1.in_data_strt))
            for i in range(agg2sram_1.dim):
                config.append((f"strg_ub_agg_only_agg_read_addr_gen_1_strides_{i}", agg2sram_1.out_data_stride[i]))
                config.append((f"strg_ub_agg_sram_shared_loops_in2buf_autovec_write_1_ranges_{i}", agg2sram_1.extent[i]))
                config.append((f"strg_ub_sram_only_input_addr_gen_1_strides_{i}", agg2sram_1.in_data_stride[i]))
                config.append((f"strg_ub_agg_sram_shared_agg_read_sched_gen_1_sched_addr_gen_strides_{i}", agg2sram_1.cyc_stride[i]))

        if sram2tb_0 is not None:
            config.append(("strg_ub_sram_only_output_addr_gen_0_starting_addr", sram2tb_0.out_data_strt))
            config.append(("strg_ub_tb_only_tb_write_addr_gen_0_starting_addr", sram2tb_0.in_data_strt))
            config.append(("strg_ub_sram_tb_shared_output_sched_gen_0_enable", 1))
            config.append(("strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_starting_addr", sram2tb_0.cyc_strt))
            config.append(("strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_dimensionality", sram2tb_0.dim))
            for i in range(sram2tb_0.dim):
                config.append((f"strg_ub_sram_tb_shared_loops_buf2out_autovec_read_0_ranges_{i}", sram2tb_0.extent[i]))
                config.append((f"strg_ub_sram_only_output_addr_gen_0_strides_{i}", sram2tb_0.out_data_stride[i]))
                config.append((f"strg_ub_sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_{i}", sram2tb_0.cyc_stride[i]))
                config.append((f"strg_ub_tb_only_tb_write_addr_gen_0_strides_{i}", sram2tb_0.in_data_stride[i]))

        if sram2tb_1 is not None:
            config.append(("strg_ub_sram_only_output_addr_gen_1_starting_addr", sram2tb_1.out_data_strt))
            config.append(("strg_ub_tb_only_tb_write_addr_gen_1_starting_addr", sram2tb_1.in_data_strt))
            config.append(("strg_ub_sram_tb_shared_output_sched_gen_1_enable", 1))
            config.append(("strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_starting_addr", sram2tb_1.cyc_strt))
            config.append(("strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_dimensionality", sram2tb_1.dim))
            for i in range(sram2tb_1.dim):
                config.append((f"strg_ub_sram_tb_shared_loops_buf2out_autovec_read_1_ranges_{i}", sram2tb_1.extent[i]))
                config.append((f"strg_ub_sram_only_output_addr_gen_1_strides_{i}", sram2tb_1.out_data_stride[i]))
                config.append((f"strg_ub_sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_{i}", sram2tb_1.cyc_stride[i]))
                config.append((f"strg_ub_tb_only_tb_write_addr_gen_1_strides_{i}", sram2tb_1.in_data_stride[i]))

        if tb2out_0 is not None:
            config.append((f"strg_ub_tb_only_tb_read_addr_gen_0_starting_addr", tb2out_0.out_data_strt))
            config.append((f"strg_ub_tb_only_tb_read_sched_gen_0_enable", 1))
            config.append((f"strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_starting_addr", tb2out_0.cyc_strt))
            config.append((f"strg_ub_tb_only_loops_buf2out_read_0_dimensionality", tb2out_0.dim))
            for i in range(tb2out_0.dim):
                config.append((f"strg_ub_tb_only_loops_buf2out_read_0_ranges_{i}", tb2out_0.extent[i]))
                config.append((f"strg_ub_tb_only_tb_read_addr_gen_0_strides_{i}", tb2out_0.out_data_stride[i]))
                config.append((f"strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_{i}", tb2out_0.cyc_stride[i]))

        if tb2out_1 is not None:
            config.append((f"strg_ub_tb_only_tb_read_addr_gen_1_starting_addr", tb2out_1.out_data_strt))
            config.append((f"strg_ub_tb_only_tb_read_sched_gen_1_enable", 1))
            config.append((f"strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_starting_addr", tb2out_1.cyc_strt))
            config.append((f"strg_ub_tb_only_loops_buf2out_read_1_dimensionality", tb2out_1.dim))
            for i in range(tb2out_1.dim):
                config.append((f"strg_ub_tb_only_loops_buf2out_read_1_ranges_{i}", tb2out_1.extent[i]))
                config.append((f"strg_ub_tb_only_tb_read_addr_gen_1_strides_{i}", tb2out_1.out_data_stride[i]))
                config.append((f"strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_{i}", tb2out_1.cyc_stride[i]))

        return config


if __name__ == "__main__":
    lake_dut = StrgUBVec()
    verilog(lake_dut, filename="strg_ub_vec.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
