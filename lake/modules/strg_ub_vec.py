from lake.modules.chain_accessor import ChainAccessor
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_interface import MemoryPort, MemoryPortType
from lake.top.memory_controller import MemoryController
from lake.utils.parse_clkwork_config import extract_controller, extract_controller_json, map_controller
from kratos import *
from lake.modules.passthru import *
from lake.attributes.formal_attr import *
from lake.passes.passes import lift_config_reg
from lake.modules.agg_only import StrgUBAggOnly
from lake.modules.agg_sram_shared import StrgUBAggSRAMShared
from lake.modules.sram_only import StrgUBSRAMOnly
from lake.modules.sram_tb_shared import StrgUBSRAMTBShared
from lake.modules.tb_only import StrgUBTBOnly
from lake.utils.util import add_counter, decode, trim_config_list
from _kratos import create_wrapper_flatten
from lake.attributes.control_signal_attr import ControlSignalAttr
import os


class StrgUBVec(MemoryController):
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
        self.agg_iter_support_small = 2

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
            self._ren_to_sram = self.output("ren_to_strg", 1, packed=True)
            self._addr_to_sram = self.output("addr_out", clog2(self.mem_depth), packed=True)

        self._wen_to_sram = self.output("wen_to_strg", 1, packed=True)
        self._data_to_sram = self.output("data_to_strg", self.data_width,
                                         size=self.fetch_width,
                                         packed=True)

        self._valid_out = self.output("accessor_output", self.interconnect_output_ports)
        self._valid_out_int = self.var("accessor_output_int", self.interconnect_output_ports)
        self._data_out = self.output("data_out", self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        self._data_out_int = self.var("data_out_int", self.data_width,
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
                                 agg_iter_support_small=self.agg_iter_support_small,
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
                       rst_n=self._rst_n)

        self.add_child("sram_only",
                       sram_only,
                       clk=self._clk,
                       rst_n=self._rst_n,
                       cycle_count=self._cycle_count,
                       wen_to_sram=self._wen_to_sram,
                       #    addr_to_sram=self._addr_to_sram,
                       data_to_sram=self._data_to_sram)

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
                       accessor_output=self._valid_out_int,
                       data_out=self._data_out_int)

        self.wire(agg_only.ports.agg_read, agg_sram_shared.ports.agg_read_out)

        self.wire(agg_only.ports.update_mode_in, agg_sram_shared.ports.update_mode_out)
        self.wire(agg_only.ports.tb_read_in, tb_only.ports.tb_read_out)
        self.wire(agg_only.ports.tb_read_addr_in, tb_only.ports.tb_read_addr_out)
        self.wire(agg_only.ports.sram_read_addr_in, agg_sram_shared.ports.agg_sram_shared_addr_out)
        self.wire(sram_only.ports.sram_read_addr_in, agg_sram_shared.ports.agg_sram_shared_addr_out)
        self.wire(agg_only.ports.agg_write_restart_out, agg_sram_shared.ports.agg_write_restart_in)
        self.wire(agg_only.ports.agg_write_out, agg_sram_shared.ports.agg_write_in)
        self.wire(agg_only.ports.agg_write_addr_l2b_out, agg_sram_shared.ports.agg_write_addr_l2b_in)
        self.wire(agg_only.ports.agg_write_mux_sel_out, agg_sram_shared.ports.agg_write_mux_sel_in)
        self.wire(sram_tb_shared.ports.t_read_out, agg_sram_shared.ports.sram_read_in)
        self.wire(sram_only.ports.sram_read_addr_out, agg_sram_shared.ports.sram_read_addr_in)

        self.wire(sram_only.ports.loops_sram2tb_mux_sel, sram_tb_shared.ports.loops_sram2tb_mux_sel)
        self.wire(sram_only.ports.loops_sram2tb_restart, sram_tb_shared.ports.loops_sram2tb_restart)
        self.wire(sram_only.ports.agg_read, agg_sram_shared.ports.agg_read_out)
        self.wire(sram_only.ports.t_read, sram_tb_shared.ports.t_read_out)
        self.wire(sram_only.ports.agg_data_out, agg_only.ports.agg_data_out)

        self.wire(tb_only.ports.t_read, sram_tb_shared.ports.t_read_out)
        self.wire(tb_only.ports.loops_sram2tb_mux_sel, sram_tb_shared.ports.loops_sram2tb_mux_sel)
        self.wire(tb_only.ports.loops_sram2tb_restart, sram_tb_shared.ports.loops_sram2tb_restart)

        self.base_ports = [[None]]

        # Dual port/single port guard.
        if self.rw_same_cycle:
            self.wire(sram_only.ports.ren_to_sram, self._ren_to_sram)
            self.wire(sram_only.ports.wr_addr_to_sram, self._wr_addr_to_sram)
            self.wire(sram_only.ports.rd_addr_to_sram, self._rd_addr_to_sram)
            self.base_ports = [[None, None]]
            tmp0_rdaddr = self.output("tmp0_rdaddr", width=self._rd_addr_to_sram.width)
            tmp0_rden = self.output("tmp0_rden", width=self._ren_to_sram.width)
            self.wire(tmp0_rdaddr, kts.const(0, width=self._rd_addr_to_sram.width))
            self.wire(tmp0_rden, kts.const(0, width=self._ren_to_sram.width))
            # Use first port as just W
            rw_port = MemoryPort(MemoryPortType.READWRITE)
            rw_port_intf = rw_port.get_port_interface()
            rw_port_intf['data_in'] = self._data_to_sram
            rw_port_intf['data_out'] = None
            rw_port_intf['write_addr'] = self._wr_addr_to_sram
            rw_port_intf['write_enable'] = self._wen_to_sram
            rw_port_intf['read_addr'] = tmp0_rdaddr
            rw_port_intf['read_enable'] = tmp0_rden
            rw_port.annotate_port_signals()
            self.base_ports[0][0] = rw_port
            # Populate second port as just R
            r_port = MemoryPort(MemoryPortType.READ)
            r_port_intf = r_port.get_port_interface()
            r_port_intf['data_out'] = self._data_from_sram
            r_port_intf['read_addr'] = self._rd_addr_to_sram
            r_port_intf['read_enable'] = self._ren_to_sram
            r_port.annotate_port_signals()
            self.base_ports[0][1] = r_port
        else:
            self.base_ports = [[None]]
            self.wire(self._ren_to_sram, sram_tb_shared.ports.t_read_out.r_or())
            # self.wire(sram_only.ports.cen_to_sram, self._cen_to_sram)
            self.wire(sram_only.ports.addr_to_sram, self._addr_to_sram)
            # Assume a single RW Port
            # TODO - have the memory interface automatically find ports
            # in the interface.
            rw_port = MemoryPort(MemoryPortType.READWRITE)
            rw_port_intf = rw_port.get_port_interface()
            rw_port_intf['data_in'] = self._data_to_sram
            rw_port_intf['data_out'] = self._data_from_sram
            rw_port_intf['write_addr'] = self._addr_to_sram
            # rw_port_intf['write_addr'] = self._wr_addr_to_sram
            rw_port_intf['write_enable'] = self._wen_to_sram
            rw_port_intf['read_addr'] = self._addr_to_sram
            # rw_port_intf['read_addr'] = self._rd_addr_to_sram
            rw_port_intf['read_enable'] = self._ren_to_sram
            rw_port.annotate_port_signals()
            self.base_ports[0][0] = rw_port

        if agg_data_top:
            self._agg_data_out = self.output(f"strg_ub_agg_data_out", self.data_width,
                                             size=(self.interconnect_input_ports,
                                                   self.fetch_width),
                                             packed=True,
                                             explicit_array=True)
            self.wire(self._agg_data_out, agg_only.ports.agg_data_out)

        # Add chaining in here... since we only use in the UB case...
        self._chain_data_in = self.input("chain_data_in",
                                         self.data_width,
                                         size=self.interconnect_output_ports,
                                         packed=True,
                                         explicit_array=True)

        chaining = ChainAccessor(data_width=self.data_width,
                                 interconnect_output_ports=self.interconnect_output_ports)

        self.add_child(f"chain", chaining,
                       curr_tile_data_out=self._data_out_int,
                       chain_data_in=self._chain_data_in,
                       accessor_output=self._valid_out_int,
                       data_out_tile=self._data_out)

        self.wire(self._valid_out, self._valid_out_int)

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
            for i in range(min(in2agg_0.dim, self.agg_iter_support_small)):
                config.append((f"strg_ub_agg_only_loops_in2buf_0_ranges_{i}", in2agg_0.extent[i]))
                config.append((f"strg_ub_agg_only_agg_write_addr_gen_0_strides_{i}", in2agg_0.in_data_stride[i]))
                config.append((f"strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_{i}", in2agg_0.cyc_stride[i]))

        if in2agg_1 is not None:
            config.append(("strg_ub_agg_only_agg_write_addr_gen_1_starting_addr", in2agg_1.in_data_strt))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_1_enable", 1))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_starting_addr", in2agg_1.cyc_strt))
            config.append(("strg_ub_agg_only_loops_in2buf_1_dimensionality", in2agg_1.dim))
            for i in range(min(in2agg_1.dim, self.agg_iter_support_small)):
                config.append((f"strg_ub_agg_only_loops_in2buf_1_ranges_{i}", in2agg_1.extent[i]))
                config.append((f"strg_ub_agg_only_agg_write_addr_gen_1_strides_{i}", in2agg_1.in_data_stride[i]))
                config.append((f"strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_{i}", in2agg_1.cyc_stride[i]))

        if agg2sram_0 is not None:
            config.append(("strg_ub_agg_sram_shared_delay_0", agg2sram_0.delay[0]))
            config.append(("strg_ub_agg_sram_shared_mode_0", agg2sram_0.mode[0]))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_0_agg_read_padding", agg2sram_0.agg_read_padding[0]))

        if agg2sram_1 is not None:
            config.append(("strg_ub_agg_sram_shared_delay_1", agg2sram_1.delay[0]))
            config.append(("strg_ub_agg_sram_shared_mode_1", agg2sram_1.mode[0]))
            config.append(("strg_ub_agg_sram_shared_agg_read_sched_gen_1_agg_read_padding", agg2sram_1.agg_read_padding[0]))

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

        # better to be generated in clockwork
        if agg2sram_0 is not None:
            if agg2sram_0.mode[0] == 0:
                assert in2agg_0.dim <= self.agg_iter_support_small, f"Non-update operations require more than {self.agg_iter_support_small} levels of iterators, {in2agg_0.dim}"

            if agg2sram_0.mode[0] == 2:
                config.append(("strg_ub_agg_only_delay_0", tb2out_0.cyc_strt - in2agg_0.cyc_strt))
            elif agg2sram_0.mode[0] == 3:
                config.append(("strg_ub_agg_only_delay_0", tb2out_1.cyc_strt - in2agg_0.cyc_strt))

        if agg2sram_1 is not None:
            if agg2sram_1.mode[0] == 0:
                assert in2agg_1.dim <= self.agg_iter_support_small, f"Non-update operations require more than {self.agg_iter_support_small} levels of iterators, {in2agg_1.dim}"

            if agg2sram_1.mode[0] == 2:
                config.append(("strg_ub_agg_only_delay_1", tb2out_0.cyc_strt - in2agg_1.cyc_strt))
            elif agg2sram_1.mode[0] == 3:
                config.append(("strg_ub_agg_only_delay_1", tb2out_1.cyc_strt - in2agg_1.cyc_strt))

        return config

    def get_memory_ports(self):
        return self.base_ports

    def get_inputs(self):
        return super().get_inputs()

    def get_outputs(self):
        return super().get_outputs()

    def __str__(self):
        return self.name

    def get_bitstream(self, config_json):
        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        input_ports = 1
        output_ports = 1

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        controller_objs = [None] * 8
        in2agg_0, in2agg_1, agg2sram_0, agg2sram_1, \
            sram2tb_0, sram2tb_1, tb2out_0, tb2out_1 = \
            controller_objs

        # Store all configurations here
        config = []

        # Compiler tells us to turn on the chain enable...
        if "chain_en" in config_json:
            config.append(("chain_chain_en", 1))

        if "in2agg_0" in config_json:
            in2agg_0 = map_controller(extract_controller_json(config_json["in2agg_0"]), "in2agg_0", True)
            config.append(("agg_only_agg_write_addr_gen_0_starting_addr", in2agg_0.in_data_strt))
            config.append(("agg_only_agg_write_sched_gen_0_enable", 1))
            config.append(("agg_only_agg_write_sched_gen_0_sched_addr_gen_starting_addr", in2agg_0.cyc_strt))
            config.append(("agg_only_loops_in2buf_0_dimensionality", in2agg_0.dim))
            for i in range(min(in2agg_0.dim, self.agg_iter_support_small)):
                config.append((f"agg_only_loops_in2buf_0_ranges_{i}", in2agg_0.extent[i]))
                config.append((f"agg_only_agg_write_addr_gen_0_strides_{i}", in2agg_0.in_data_stride[i]))
                config.append((f"agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_{i}", in2agg_0.cyc_stride[i]))

        if "in2agg_1" in config_json:
            in2agg_1 = map_controller(extract_controller_json(config_json["in2agg_1"]), "in2agg_1", True)
            config.append(("agg_only_agg_write_addr_gen_1_starting_addr", in2agg_1.in_data_strt))
            config.append(("agg_only_agg_write_sched_gen_1_enable", 1))
            config.append(("agg_only_agg_write_sched_gen_1_sched_addr_gen_starting_addr", in2agg_1.cyc_strt))
            config.append(("agg_only_loops_in2buf_1_dimensionality", in2agg_1.dim))
            for i in range(min(in2agg_1.dim, self.agg_iter_support_small)):
                config.append((f"agg_only_loops_in2buf_1_ranges_{i}", in2agg_1.extent[i]))
                config.append((f"agg_only_agg_write_addr_gen_1_strides_{i}", in2agg_1.in_data_stride[i]))
                config.append((f"agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_{i}", in2agg_1.cyc_stride[i]))

        if "agg2sram_0" in config_json:
            agg2sram_0 = map_controller(extract_controller_json(config_json["agg2sram_0"]), "agg2sram_0")
            config.append(("agg_sram_shared_delay_0", agg2sram_0.delay[0]))
            config.append(("agg_sram_shared_mode_0", agg2sram_0.mode[0]))
            config.append(("agg_sram_shared_agg_read_sched_gen_0_agg_read_padding", agg2sram_0.agg_read_padding[0]))

        if "agg2sram_1" in config_json:
            agg2sram_1 = map_controller(extract_controller_json(config_json["agg2sram_1"]), "agg2sram_1")
            config.append(("agg_sram_shared_delay_1", agg2sram_1.delay[0]))
            config.append(("agg_sram_shared_mode_1", agg2sram_1.mode[0]))
            config.append(("agg_sram_shared_agg_read_sched_gen_1_agg_read_padding", agg2sram_1.agg_read_padding[0]))

        # Count tbs
        num_tbs = 0

        if "tb2out_0" in config_json:
            num_tbs += 1
            tb2out_0 = map_controller(extract_controller_json(config_json["tb2out_0"]), "tb2out_0")
            config.append(("tb_only_tb_read_sched_gen_0_enable", 1))
            config.append(("tb_only_tb_read_sched_gen_0_sched_addr_gen_starting_addr", tb2out_0.cyc_strt))
            config.append(("tb_only_tb_read_addr_gen_0_starting_addr", tb2out_0.out_data_strt))
            config.append(("tb_only_loops_buf2out_read_0_dimensionality", tb2out_0.dim))
            for i in range(tb2out_0.dim):
                config.append((f"tb_only_loops_buf2out_read_0_ranges_{i}", tb2out_0.extent[i]))
                config.append((f"tb_only_tb_read_addr_gen_0_strides_{i}", tb2out_0.out_data_stride[i]))
                config.append((f"tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_{i}", tb2out_0.cyc_stride[i]))

        if "tb2out_1" in config_json:
            num_tbs += 1
            tb2out_1 = map_controller(extract_controller_json(config_json["tb2out_1"]), "tb2out_1")
            config.append(("tb_only_tb_read_sched_gen_1_enable", 1))
            config.append(("tb_only_tb_read_addr_gen_1_starting_addr", tb2out_1.out_data_strt))
            config.append(("tb_only_tb_read_sched_gen_1_sched_addr_gen_starting_addr", tb2out_1.cyc_strt))
            config.append(("tb_only_loops_buf2out_read_1_dimensionality", tb2out_1.dim))
            for i in range(tb2out_1.dim):
                config.append((f"tb_only_loops_buf2out_read_1_ranges_{i}", tb2out_1.extent[i]))
                config.append((f"tb_only_tb_read_addr_gen_1_strides_{i}", tb2out_1.out_data_stride[i]))
                config.append((f"tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_{i}", tb2out_1.cyc_stride[i]))

        if "sram2tb_0" in config_json:
            sram2tb_0 = map_controller(extract_controller_json(config_json["sram2tb_0"]), "sram2tb_0")
            config.append(("sram_only_output_addr_gen_0_starting_addr", sram2tb_0.out_data_strt))
            config.append(("tb_only_tb_write_addr_gen_0_starting_addr", sram2tb_0.in_data_strt))
            config.append(("sram_tb_shared_output_sched_gen_0_enable", 1))
            config.append(("sram_tb_shared_output_sched_gen_0_sched_addr_gen_starting_addr", sram2tb_0.cyc_strt))
            config.append(("sram_tb_shared_loops_buf2out_autovec_read_0_dimensionality", sram2tb_0.dim))
            for i in range(sram2tb_0.dim):
                config.append((f"sram_tb_shared_loops_buf2out_autovec_read_0_ranges_{i}", sram2tb_0.extent[i]))
                config.append((f"sram_only_output_addr_gen_0_strides_{i}", sram2tb_0.out_data_stride[i]))
                config.append((f"sram_tb_shared_output_sched_gen_0_sched_addr_gen_strides_{i}", sram2tb_0.cyc_stride[i]))
                config.append((f"tb_only_tb_write_addr_gen_0_strides_{i}", sram2tb_0.in_data_stride[i]))

        if "sram2tb_1" in config_json:
            sram2tb_1 = map_controller(extract_controller_json(config_json["sram2tb_1"]), "sram2tb_1")
            config.append(("sram_only_output_addr_gen_1_starting_addr", sram2tb_1.out_data_strt))
            config.append(("tb_only_tb_write_addr_gen_1_starting_addr", sram2tb_1.in_data_strt))
            config.append(("sram_tb_shared_output_sched_gen_1_enable", 1))
            config.append(("sram_tb_shared_output_sched_gen_1_sched_addr_gen_starting_addr", sram2tb_1.cyc_strt))
            config.append(("sram_tb_shared_loops_buf2out_autovec_read_1_dimensionality", sram2tb_1.dim))
            for i in range(sram2tb_1.dim):
                config.append((f"sram_tb_shared_loops_buf2out_autovec_read_1_ranges_{i}", sram2tb_1.extent[i]))
                config.append((f"sram_only_output_addr_gen_1_strides_{i}", sram2tb_1.out_data_stride[i]))
                config.append((f"sram_tb_shared_output_sched_gen_1_sched_addr_gen_strides_{i}", sram2tb_1.cyc_stride[i]))
                config.append((f"tb_only_tb_write_addr_gen_1_strides_{i}", sram2tb_1.in_data_stride[i]))

        # better to be generated in clockwork
        if agg2sram_0 is not None:
            if agg2sram_0.mode[0] == 0:
                assert in2agg_0.dim <= self.agg_iter_support_small, f"Non-update operations require more than {self.agg_iter_support_small} levels of iterators, {in2agg_0.dim}"

            if agg2sram_0.mode[0] == 2:
                config.append(("agg_only_delay_0", tb2out_0.cyc_strt - in2agg_0.cyc_strt))
            elif agg2sram_0.mode[0] == 3:
                config.append(("agg_only_delay_0", tb2out_1.cyc_strt - in2agg_0.cyc_strt))

        if agg2sram_1 is not None:
            if agg2sram_1.mode[0] == 0:
                assert in2agg_1.dim <= self.agg_iter_support_small, f"Non-update operations require more than {self.agg_iter_support_small} levels of iterators, {in2agg_1.dim}"

            if agg2sram_1.mode[0] == 2:
                config.append(("agg_only_delay_1", tb2out_0.cyc_strt - in2agg_1.cyc_strt))
            elif agg2sram_1.mode[0] == 3:
                config.append(("agg_only_delay_1", tb2out_1.cyc_strt - in2agg_1.cyc_strt))

        return trim_config_list(flattened, config)

    def get_config_mode_str(self):
        return "UB"


if __name__ == "__main__":
    lake_dut = StrgUBVec()
    verilog(lake_dut, filename="strg_ub_vec.sv",
            optimize_if=False,
            additional_passes={"lift config regs": lift_config_reg})
