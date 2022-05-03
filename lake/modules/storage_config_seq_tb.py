from lake.attributes.formal_attr import *
import os
from kratos import *
from lake.modules.passthru import *
from lake.modules.sram import SRAM
from lake.modules.strg_ub_vec import StrgUBVec
from lake.modules.strg_ub_thin import StrgUBThin
from lake.modules.storage_config_seq import StorageConfigSeq
from lake.modules.register_file import RegisterFile
from lake.modules.strg_fifo import StrgFIFO
from lake.modules.strg_RAM import StrgRAM
from lake.modules.chain_accessor import ChainAccessor
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.passes.passes import lift_config_reg, change_sram_port_names
from lake.passes.cut_generator import cut_generator
from lake.utils.sram_macro import SRAMMacroInfo
from lake.utils.util import trim_config_list, extract_formal_annotation
from lake.utils.parse_clkwork_config import map_controller, extract_controller
from lake.utils.parse_clkwork_config import extract_controller_json
from lake.modules.for_loop import ForLoop
from lake.modules.spec.sched_gen import SchedGen
import kratos as kts
from _kratos import create_wrapper_flatten
import argparse


class StorageConfigSeqTb(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 input_iterator_support=6,  # Addr Controllers
                 output_iterator_support=6,
                 input_config_width=16,
                 output_config_width=16,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sram_stub=True,
                 sram_macro_info=SRAMMacroInfo("tsmc_name"),
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4,
                 config_data_width=32,
                 config_addr_width=8,
                 num_tiles=1,
                 fifo_mode=True,
                 add_clk_enable=True,
                 add_flush=True,
                 name="StorageConfigSeqTb",
                 gen_addr=True,
                 stencil_valid=True,
                 formal_module=None,
                 do_config_lift=True):
        super().__init__(name, debug=True)

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.input_iterator_support = input_iterator_support
        self.output_iterator_support = output_iterator_support
        self.input_config_width = input_config_width
        self.output_config_width = output_config_width
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.use_sram_stub = use_sram_stub
        self.sram_macro_info = sram_macro_info
        self.agg_height = agg_height
        self.input_port_sched_width = clog2(self.interconnect_input_ports)
        assert self.mem_width >= self.data_width, "Data width needs to be smaller than mem"
        self.fw_int = int(self.mem_width / self.data_width)
        self.config_data_width = config_data_width
        self.config_addr_width = config_addr_width
        self.num_tiles = num_tiles
        self.read_delay = read_delay
        self.rw_same_cycle = rw_same_cycle
        self.fifo_mode = fifo_mode
        self.gen_addr = gen_addr
        self.stencil_valid = stencil_valid
        self.formal_module = formal_module

        self.data_words_per_set = 2 ** self.config_addr_width
        self.sets = int((self.fw_int * self.mem_depth) / self.data_words_per_set)

        self.sets_per_macro = max(1, int(self.mem_depth / self.data_words_per_set))
        self.total_sets = max(1, self.banks * self.sets_per_macro)

        # phases = [] TODO

        # CLK and RST
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(self._clk.name, FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(self._rst_n.name, FormalSignalConstraint.RSTN))

        self._config_data_in = self.input("config_data_in",
                                          self.config_data_width)
        self._config_data_in.add_attribute(ControlSignalAttr(False))
        # self._config_data_in.add_attribute(FormalAttr(self._ren.name, FormalSignalConstraint.SET0))

        self._config_data_in_shrt = self.var("config_data_in_shrt",
                                             self.data_width)

        self.wire(self._config_data_in_shrt, self._config_data_in[self.data_width - 1, 0])

        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.cycle_count_inc)

        if self.stencil_valid:

            self.stencil_valid_width = 16

            self._stencil_valid = self.output("stencil_valid", 1)
            self._stencil_valid.add_attribute(ControlSignalAttr(False))
            self._loops_stencil_valid = ForLoop(iterator_support=6,
                                                config_width=16)
            self._stencil_valid_int = self.var("stencil_valid_internal", 1)

            # Loop Iterators for stencil valid...
            self.add_child(f"loops_stencil_valid",
                           self._loops_stencil_valid,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           step=self._stencil_valid_int)
            # Schedule Generator for stencil valid...
            self.add_child(f"stencil_valid_sched_gen",
                           SchedGen(iterator_support=6,
                                    config_width=16),
                           clk=self._clk,
                           rst_n=self._rst_n,
                           cycle_count=self._cycle_count,
                           mux_sel=self._loops_stencil_valid.ports.mux_sel_out,
                           finished=self._loops_stencil_valid.ports.restart,
                           valid_output=self._stencil_valid_int)
            # Wire out internal wire
            # self.wire(self._stencil_valid, self._stencil_valid_int & self._stencil_valid_gate & self._use_stencil_valid)
            self.wire(self._stencil_valid, self._stencil_valid_int)

        self._config_addr_in = self.input("config_addr_in",
                                          self.config_addr_width)
        self._config_addr_in.add_attribute(ControlSignalAttr(False))

        self._config_data_out_shrt = self.var("config_data_out_shrt", self.data_width,
                                              size=self.total_sets,
                                              explicit_array=True,
                                              packed=True)

        self._config_data_out = self.output("config_data_out", self.config_data_width,
                                            size=self.total_sets,
                                            explicit_array=True,
                                            packed=True)
        self._config_data_out.add_attribute(ControlSignalAttr(False))

        self._clk_en = self.clock_en("clk_en", 1)

        for i in range(self.total_sets):
            self.wire(self._config_data_out[i],
                      self._config_data_out_shrt[i].extend(self.config_data_width))

        self._config_read = self.input("config_read", 1)
        self._config_read.add_attribute(ControlSignalAttr(False))

        self._config_write = self.input("config_write", 1)
        self._config_write.add_attribute(ControlSignalAttr(False))

        self._config_en = self.input("config_en", self.total_sets)
        self._config_en.add_attribute(ControlSignalAttr(False))

        self.address_width = clog2(self.num_tiles * self.mem_depth)

        # Add tile enable!
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))
        self._tile_en.add_attribute(FormalAttr(self._tile_en.name, FormalSignalConstraint.SET1))

        # Currenlt mode = 0 is UB, mode = 1 is FIFO
        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        self._mem_data_out = self.var("mem_data_out",
                                      self.data_width,
                                      size=(self.banks,
                                            self.mem_output_ports,
                                            self.fw_int),
                                      packed=True,
                                      explicit_array=True)

        if self.formal_module == "sram":
            self._formal_mem_data = self.output("formal_mem_data",
                                                self.data_width,
                                                size=(self.banks,
                                                      self.mem_output_ports,
                                                      self.fw_int),
                                                packed=True,
                                                explicit_array=True)
            self._formal_mem_data.add_attribute(
                FormalAttr(self._formal_mem_data.name, FormalSignalConstraint.SEQUENCE, self.formal_module))

            self.wire(self._formal_mem_data, self._mem_data_out)
        elif formal_module == "tb":
            self._formal_mem_data = self.input("formal_mem_data",
                                               self.data_width,
                                               size=(self.banks,
                                                     self.mem_output_ports,
                                                     self.fw_int),
                                               packed=True,
                                               explicit_array=True)
            self._formal_mem_data.add_attribute(
                FormalAttr(self._formal_mem_data.name, FormalSignalConstraint.SEQUENCE, self.formal_module))

            self.wire(self._mem_data_out, self._formal_mem_data)

        self._mem_data_low_pt = self.var("mem_data_low_pt",
                                         self.data_width,
                                         size=(self.banks,
                                               self.fw_int),
                                         packed=True,
                                         explicit_array=True)

        for i in range(self.banks):
            self.wire(self._mem_data_low_pt[i], self._mem_data_out[i][0])

        self._mem_data_in = self.var("mem_data_in",
                                     self.data_width,
                                     size=(self.banks,
                                           self.mem_input_ports,
                                           self.fw_int),
                                     packed=True,
                                     explicit_array=True)

        self._mem_data_dp = self.var("mem_data_dp",
                                     self.data_width,
                                     size=(self.banks,
                                           self.mem_input_ports,
                                           self.fw_int),
                                     packed=True,
                                     explicit_array=True)

        self._mem_data_cfg = self.var("mem_data_cfg",
                                      self.data_width,
                                      size=self.fw_int,
                                      packed=True,
                                      explicit_array=True)

        if self.rw_same_cycle:
            self._wr_mem_addr_dp = self.var("wr_mem_addr_dp",
                                            self.address_width,
                                            size=(self.banks,
                                                  self.mem_input_ports),
                                            explicit_array=True,
                                            packed=True)
            self._wr_mem_addr_in = self.var("wr_mem_addr_in",
                                            self.address_width,
                                            size=(self.banks,
                                                  self.mem_input_ports),
                                            explicit_array=True,
                                            packed=True)
            self._rd_mem_addr_dp = self.var("rd_mem_addr_dp",
                                            self.address_width,
                                            size=(self.banks,
                                                  self.mem_output_ports),
                                            explicit_array=True,
                                            packed=True)
            self._rd_mem_addr_in = self.var("rd_mem_addr_in",
                                            self.address_width,
                                            size=(self.banks,
                                                  self.mem_output_ports),
                                            explicit_array=True,
                                            packed=True)

        else:
            self._mem_addr_dp = self.var("mem_addr_dp",
                                         self.address_width,
                                         size=(self.banks,
                                               self.mem_input_ports),
                                         packed=True,
                                         explicit_array=True)
            self._mem_addr_in = self.var("mem_addr_in",
                                         self.address_width,
                                         size=(self.banks,
                                               self.mem_input_ports),
                                         packed=True,
                                         explicit_array=True)

        self._mem_addr_cfg = self.var("mem_addr_cfg", self.address_width)

        self._mem_ren_cfg = self.var("mem_ren_cfg", self.banks)
        self._mem_wen_cfg = self.var("mem_wen_cfg", self.banks)

        self._mem_cen_dp = self.var("mem_cen_dp", self.mem_output_ports,
                                    size=self.banks,
                                    explicit_array=True,
                                    packed=True)

        self._mem_wen_dp = self.var("mem_wen_dp", self.mem_input_ports,
                                    size=self.banks,
                                    explicit_array=True,
                                    packed=True)
        self._mem_cen_in = self.var("mem_cen_in", self.mem_output_ports,
                                    size=self.banks,
                                    explicit_array=True,
                                    packed=True)

        self._mem_wen_in = self.var("mem_wen_in", self.mem_input_ports,
                                    size=self.banks,
                                    explicit_array=True,
                                    packed=True)

        ####################################
        ##### DEMUX WRITE/SRAM WRAPPER #####
        ####################################

        stg_cfg_seq = StorageConfigSeq(data_width=self.data_width,
                                       config_addr_width=self.config_addr_width,
                                       addr_width=self.address_width,
                                       fetch_width=self.mem_width,
                                       total_sets=self.total_sets,
                                       sets_per_macro=self.sets_per_macro)

        # The clock to config sequencer needs to be the normal clock or
        # if the tile is off, we bring the clock back in based on config_en
        cfg_seq_clk = self.var("cfg_seq_clk", 1)
        self._cfg_seq_clk = kts.util.clock(cfg_seq_clk)
        # self.wire(cfg_seq_clk, kts.util.clock(self._gclk | (self._clk & self._config_en.r_or())))
        self.wire(cfg_seq_clk, kts.util.clock(self._gclk))

        self.add_child(f"config_seq", stg_cfg_seq,
                       clk=self._cfg_seq_clk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en | self._config_en.r_or(),
                       config_data_in=self._config_data_in_shrt,
                       config_addr_in=self._config_addr_in,
                       config_wr=self._config_write,
                       config_rd=self._config_read,
                       config_en=self._config_en,
                       rd_data_stg=self._mem_data_low_pt,
                       wr_data=self._mem_data_cfg,
                       rd_data_out=self._config_data_out_shrt,
                       addr_out=self._mem_addr_cfg,
                       wen_out=self._mem_wen_cfg,
                       ren_out=self._mem_ren_cfg)

        for i in range(self.banks):
            # If we are not sharing the ports...
            if self.rw_same_cycle:
                self.wire(self._mem_wen_in[i][0], ternary(self._config_en.r_or(),
                                                          self._mem_wen_cfg[i],
                                                          self._mem_wen_dp[i][0]))
                # Treat cen as ren for all intents and purpose
                self.wire(self._mem_cen_in[i][0], ternary(self._config_en.r_or(),
                                                          self._mem_ren_cfg[i],
                                                          self._mem_cen_dp[i][0]))
                self.wire(self._wr_mem_addr_in[i][0], ternary(self._config_en.r_or(),
                                                              self._mem_addr_cfg,
                                                              self._wr_mem_addr_dp[i][0]))
                self.wire(self._rd_mem_addr_in[i][0], ternary(self._config_en.r_or(),
                                                              self._mem_addr_cfg,
                                                              self._rd_mem_addr_dp[i][0]))
                self.wire(self._mem_data_in[i][0], ternary(self._config_en.r_or(),
                                                           self._mem_data_cfg,
                                                           self._mem_data_dp[i][0]))
                # Don't route the config to any but the first port
                for j in range(self.mem_input_ports - 1):
                    self.wire(self._mem_wen_in[i][j + 1], self._mem_wen_dp[i][j + 1])
                    self.wire(self._wr_mem_addr_in[i][j + 1], self._wr_mem_addr_dp[i][j + 1])
                    self.wire(self._mem_data_in[i][j + 1], self._mem_data_dp[i][j + 1])
                for j in range(self.mem_output_ports - 1):
                    self.wire(self._mem_cen_in[i][j + 1], self._mem_cen_dp[i][j + 1])
                    self.wire(self._rd_mem_addr_in[i][j + 1], self._rd_mem_addr_dp[i][j + 1])
            else:
                self.wire(self._mem_wen_in[i][0], ternary(self._config_en.r_or(),
                                                          self._mem_wen_cfg[i],
                                                          self._mem_wen_dp[i][0]))
                self.wire(self._mem_cen_in[i][0], ternary(self._config_en.r_or(),
                                                          self._mem_wen_cfg[i] | self._mem_ren_cfg[i],
                                                          self._mem_cen_dp[i][0]))
                self.wire(self._mem_addr_in[i][0], ternary(self._config_en.r_or(),
                                                           self._mem_addr_cfg,
                                                           self._mem_addr_dp[i][0]))
                self.wire(self._mem_data_in[i][0], ternary(self._config_en.r_or(),
                                                           self._mem_data_cfg,
                                                           self._mem_data_dp[i][0]))
                # Don't route the config to any but the first port
                for j in range(self.mem_input_ports - 1):
                    self.wire(self._mem_wen_in[i][j + 1], self._mem_wen_dp[i][j + 1])
                    self.wire(self._mem_cen_in[i][j + 1], self._mem_cen_dp[i][j + 1])
                    self.wire(self._mem_addr_in[i][j + 1], self._mem_addr_dp[i][j + 1])
                    self.wire(self._mem_data_in[i][j + 1], self._mem_data_dp[i][j + 1])

        # ground the unused nets
        self.wire(self._mem_wen_dp, 0)
        self.wire(self._mem_cen_dp, 0)
        self.wire(self._mem_data_dp, 0)
        self.wire(self._mem_addr_dp, 0)

        # Wrap sram_stub
        if self.read_delay == 1 and self.rw_same_cycle is False:

            # have SRAM when not doing formal module generation or when generating
            # SRAM modular problem for formal team
            if self.formal_module is None or self.formal_module == "sram":
                for i in range(self.banks):
                    mbank = SRAM(use_sram_stub=self.use_sram_stub,
                                 sram_name=self.sram_macro_info.name,
                                 data_width=self.data_width,
                                 fw_int=self.fw_int,
                                 mem_depth=self.mem_depth,
                                 mem_input_ports=self.mem_input_ports,
                                 mem_output_ports=self.mem_output_ports,
                                 address_width=self.address_width,
                                 bank_num=i,
                                 num_tiles=self.num_tiles)

                    self.add_child(f"mem_{i}", mbank,
                                   clk=self._gclk,
                                   clk_en=self._clk_en | self._config_en.r_or(),
                                   mem_data_in_bank=self._mem_data_in[i],
                                   mem_data_out_bank=self._mem_data_out[i],
                                   mem_addr_in_bank=self._mem_addr_in[i],
                                   mem_cen_in_bank=self._mem_cen_in[i],
                                   mem_wen_in_bank=self._mem_wen_in[i],
                                   wtsel=self.sram_macro_info.wtsel_value,
                                   rtsel=self.sram_macro_info.rtsel_value)
        else:
            for i in range(self.banks):
                rfile = RegisterFile(data_width=self.data_width,
                                     write_ports=self.mem_input_ports,
                                     read_ports=self.mem_output_ports,
                                     width_mult=self.fw_int,
                                     depth=self.mem_depth,
                                     read_delay=self.read_delay)
                if self.rw_same_cycle:
                    self.add_child(f"rf_{i}", rfile,
                                   clk=self._gclk,
                                   rst_n=self._rst_n,
                                   wen=self._mem_wen_in[i],
                                   wr_addr=self._wr_mem_addr_in[i],
                                   rd_addr=self._rd_mem_addr_in[i],
                                   data_in=self._mem_data_in[i],
                                   data_out=self._mem_data_out[i])
                else:
                    self.add_child(f"rf_{i}", rfile,
                                   clk=self._gclk,
                                   rst_n=self._rst_n,
                                   wen=self._mem_wen_in[i],
                                   wr_addr=self._mem_addr_in[i],
                                   rd_addr=self._mem_addr_in[i],
                                   data_in=self._mem_data_in[i],
                                   data_out=self._mem_data_out[i])
                if self.read_delay == 1:
                    self.wire(rfile.ports.ren, self._mem_cen_dp[0])

        ########################
        ##### CLOCK ENABLE #####
        ########################
        if add_clk_enable:
            # self.clock_en("clk_en")
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            # Add input attr and formal attr...
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))
            clk_en_port.add_attribute(FormalAttr(clk_en_port.name, FormalSignalConstraint.SET1))
        if add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

        if do_config_lift:
            lift_config_reg(self.internal_generator)

    @always_ff((posedge, "clk"), (negedge, "rst_n"))
    def cycle_count_inc(self):
        if ~self._rst_n:
            self._cycle_count = 0
        else:
            self._cycle_count = self._cycle_count + 1

    def supports(self, prop):
        attr = getattr(self, prop)
        if attr:
            return attr
        else:
            return False

    def get_static_bitstream_json(self,
                                  root_node):
        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        input_ports = 1
        output_ports = 1

        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Store all configurations here
        config = []

        # Compiler tells us to turn on the chain enable...
        if "chain_en" in root_node:
            config.append(("chain_chain_en", 1))

        if "in2agg_0" in root_node:
            in2agg_0 = map_controller(extract_controller_json(root_node["in2agg_0"]), "in2agg_0")
            config.append(("strg_ub_agg_only_agg_write_addr_gen_0_starting_addr", in2agg_0.in_data_strt))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_0_enable", 1))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_starting_addr", in2agg_0.cyc_strt))
            config.append(("strg_ub_agg_only_loops_in2buf_0_dimensionality", in2agg_0.dim))
            for i in range(in2agg_0.dim):
                config.append((f"strg_ub_agg_only_loops_in2buf_0_ranges_{i}", in2agg_0.extent[i]))
                config.append((f"strg_ub_agg_only_agg_write_addr_gen_0_strides_{i}", in2agg_0.in_data_stride[i]))
                config.append((f"strg_ub_agg_only_agg_write_sched_gen_0_sched_addr_gen_strides_{i}", in2agg_0.cyc_stride[i]))

        if "in2agg_1" in root_node:
            in2agg_1 = map_controller(extract_controller_json(root_node["in2agg_1"]), "in2agg_1")
            config.append(("strg_ub_agg_only_agg_write_addr_gen_1_starting_addr", in2agg_1.in_data_strt))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_1_enable", 1))
            config.append(("strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_starting_addr", in2agg_1.cyc_strt))
            config.append(("strg_ub_agg_only_loops_in2buf_1_dimensionality", in2agg_1.dim))
            for i in range(in2agg_1.dim):
                config.append((f"strg_ub_agg_only_loops_in2buf_1_ranges_{i}", in2agg_1.extent[i]))
                config.append((f"strg_ub_agg_only_agg_write_addr_gen_1_strides_{i}", in2agg_1.in_data_stride[i]))
                config.append((f"strg_ub_agg_only_agg_write_sched_gen_1_sched_addr_gen_strides_{i}", in2agg_1.cyc_stride[i]))

        if "agg2sram_0" in root_node:
            agg2sram_0 = map_controller(extract_controller_json(root_node["agg2sram_0"]), "agg2sram_0")
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

        if "agg2sram_1" in root_node:
            agg2sram_1 = map_controller(extract_controller_json(root_node["agg2sram_1"]), "agg2sram_1")
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

        # Count tbs
        num_tbs = 0

        if "tb2out_0" in root_node:
            num_tbs += 1
            tb2out_0 = map_controller(extract_controller_json(root_node["tb2out_0"]), "tb2out_0")
            config.append(("strg_ub_tb_only_tb_read_sched_gen_0_enable", 1))
            config.append(("strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_starting_addr", tb2out_0.cyc_strt))
            config.append(("strg_ub_tb_only_tb_read_addr_gen_0_starting_addr", tb2out_0.out_data_strt))
            config.append(("strg_ub_tb_only_loops_buf2out_read_0_dimensionality", tb2out_0.dim))
            for i in range(tb2out_0.dim):
                config.append((f"strg_ub_tb_only_loops_buf2out_read_0_ranges_{i}", tb2out_0.extent[i]))
                config.append((f"strg_ub_tb_only_tb_read_addr_gen_0_strides_{i}", tb2out_0.out_data_stride[i]))
                config.append((f"strg_ub_tb_only_tb_read_sched_gen_0_sched_addr_gen_strides_{i}", tb2out_0.cyc_stride[i]))

        if "tb2out_1" in root_node:
            num_tbs += 1
            tb2out_1 = map_controller(extract_controller_json(root_node["tb2out_1"]), "tb2out_1")
            config.append(("strg_ub_tb_only_tb_read_sched_gen_1_enable", 1))
            config.append(("strg_ub_tb_only_tb_read_addr_gen_1_starting_addr", tb2out_1.out_data_strt))
            config.append(("strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_starting_addr", tb2out_1.cyc_strt))
            config.append(("strg_ub_tb_only_loops_buf2out_read_1_dimensionality", tb2out_1.dim))
            for i in range(tb2out_1.dim):
                config.append((f"strg_ub_tb_only_loops_buf2out_read_1_ranges_{i}", tb2out_1.extent[i]))
                config.append((f"strg_ub_tb_only_tb_read_addr_gen_1_strides_{i}", tb2out_1.out_data_stride[i]))
                config.append((f"strg_ub_tb_only_tb_read_sched_gen_1_sched_addr_gen_strides_{i}", tb2out_1.cyc_stride[i]))

        if "sram2tb_0" in root_node:
            sram2tb_0 = map_controller(extract_controller_json(root_node["sram2tb_0"]), "sram2tb_0")
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

        if "sram2tb_1" in root_node:
            sram2tb_1 = map_controller(extract_controller_json(root_node["sram2tb_1"]), "sram2tb_1")
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

        if "stencil_valid" in root_node:
            stencil_valid = map_controller(extract_controller_json(root_node["stencil_valid"]), "stencil_valid")
            # Check actual stencil valid property of hardware before programming
            if self.stencil_valid:
                config.append((f"stencil_valid_sched_gen_enable", 1))
                config.append((f"stencil_valid_sched_gen_sched_addr_gen_starting_addr", stencil_valid.cyc_strt))
                config.append((f"loops_stencil_valid_dimensionality", stencil_valid.dim))
                for i in range(stencil_valid.dim):
                    config.append((f"loops_stencil_valid_ranges_{i}", stencil_valid.extent[i]))
                    config.append((f"stencil_valid_sched_gen_sched_addr_gen_strides_{i}", stencil_valid.cyc_stride[i]))

        # Control Signals...
        # Set the mode and activate the tile...
        config.append(("flush_reg_sel", 0))  # 1
        config.append(("flush_reg_value", 0))  # 1
        config.append(("mode", 0))  # 2
        config.append(("tile_en", 1))  # 1

        # TODO: Maybe need to check if size 1?
        for i in range(input_ports):
            config.append((f"ren_in_{i}_reg_sel", 1))
            config.append((f"ren_in_{i}_reg_value", 0))

        for i in range(output_ports):
            config.append((f"wen_in_{i}_reg_sel", 1))
            config.append((f"wen_in_{i}_reg_value", 0))

        return trim_config_list(flattened, config)

    def get_static_bitstream(self,
                             config_path,
                             in_file_name="",
                             out_file_name=""):

        input_ports = 1
        output_ports = 1

        config = []

        controllers = ["in2agg_0", "in2agg_1", "agg2sram_0", "agg2sram_1", "sram2tb_0", "sram2tb_1", "tb2out_0", "tb2out_1"]

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

        # Getting bitstreams is a little unwieldy due to fault (or its underlying implementation) not
        # handling arrays in the interface.
        # To alleviate this, we create the flattened wrapper so we can query widths of config
        # registers and trim values to their bitwidths...
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        config = []

        # control signals
        # config.append(("flush_reg_sel", 0))
        # config.append(("flush_reg_value", 0))

        # set the mode and activate the tile
        config.append(("mode", 0))
        config.append(("tile_en", 1))

        # TODO: Maybe need to check if size 1?
        # for i in range(input_ports):
        #     config.append((f"ren_in_{i}_reg_sel", 1))
        #     config.append((f"ren_in_{i}_reg_value", 0))

        # for i in range(output_ports):
        #     config.append((f"wen_in_{i}_reg_sel", 1))
        #     config.append((f"wen_in_{i}_reg_value", 0))

        # Check the hardware if it supports stencil valid
        if self.stencil_valid:
            cfg_path = config_path + '/' + 'stencil_valid.csv'
            # Check if the stencil valid file exists...if it doesn't we just won't program it
            if os.path.exists(cfg_path):
                stcl_valid = map_controller(extract_controller(cfg_path), "stencil_valid")
                config.append((f"loops_stencil_valid_dimensionality", stcl_valid.dim))
                config.append((f"stencil_valid_sched_gen_enable", 1))
                config.append((f"stencil_valid_sched_gen_sched_addr_gen_starting_addr", stcl_valid.cyc_strt))
                for i in range(stcl_valid.dim):
                    config.append((f"loops_stencil_valid_ranges_{i}", stcl_valid.extent[i]))
                    config.append((f"stencil_valid_sched_gen_sched_addr_gen_strides_{i}", stcl_valid.cyc_stride[i]))
            else:
                print("No configuration file provided for stencil valid...are you expecting one to exist?")
                print(f"Bogus stencil valid path: {cfg_path}")

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

        return trim_config_list(flattened, config)


def get_db_dut(formal_module=None,
               in_ports=2,
               out_ports=2,
               # no stencil valid needed for formal problems
               stencil_valid=False,
               tsmc_info=SRAMMacroInfo("tsmc_name"),
               use_sram_stub=True,
               fifo_mode=True,
               data_width=16,
               mem_width=64,
               mem_depth=512,
               do_config_lift=True):

    db_dut = StorageConfigSeqTb(data_width=data_width,
                                mem_width=mem_width,
                                mem_depth=mem_depth,
                                interconnect_input_ports=in_ports,
                                interconnect_output_ports=out_ports,
                                sram_macro_info=tsmc_info,
                                use_sram_stub=use_sram_stub,
                                fifo_mode=fifo_mode,
                                add_clk_enable=True,
                                add_flush=True,
                                stencil_valid=stencil_valid,
                                formal_module=formal_module,
                                do_config_lift=do_config_lift)

    # if do_config_lift, then do not need_config_lift later
    return db_dut, not do_config_lift, use_sram_stub, tsmc_info


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='StorageConfigSeqTb')
    parser.add_argument("-f",
                        help="optional: will generate verilog, annotation file, and dim to strides/range mapping collateral to solve a formal problem. must provide module to solve for")

    args = parser.parse_args()

    need_config_lift = True

    # normal generation
    if args.f is None:
        prefix = ""
        db_dut, need_config_lift, use_sram_stub, tsmc_info = get_db_dut()
        # extract_formal_annotation(db_dut, f"lake_top_annotation.txt", "full")
    # optional: to add generator cuts for formal module verilog + annotations
    # else:
    #     module = args.f
    #     db_dut, need_config_lift, use_sram_stub, tsmc_info = get_formal_module(module)
    #     prefix = f"{module}_"

    # config lift happens in all possible cases by this point
    assert not need_config_lift

    sram_port_pass = change_sram_port_names(use_sram_stub=use_sram_stub, sram_macro_info=tsmc_info)
    # generate verilog
    verilog(db_dut, filename=f"storage_config_seq_tb.sv",
            optimize_if=False,
            additional_passes={"change sram port names": sram_port_pass})
