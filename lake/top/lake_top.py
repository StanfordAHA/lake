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
from lake.utils.sram_macro import SRAMMacroInfo
from lake.utils.util import extract_formal_annotation, trim_config
from lake.utils.parse_clkwork_config import map_controller, extract_controller
from lake.utils.parse_clkwork_config import extract_controller_json
from lake.modules.for_loop import ForLoop
from lake.modules.spec.sched_gen import SchedGen
import kratos as kts
from _kratos import create_wrapper_flatten


class LakeTop(Generator):
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
                 name="LakeTop",
                 gen_addr=True,
                 stencil_valid=True):
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

        self.data_words_per_set = 2 ** self.config_addr_width
        self.sets = int((self.fw_int * self.mem_depth) / self.data_words_per_set)

        self.sets_per_macro = max(1, int(self.mem_depth / self.data_words_per_set))
        self.total_sets = max(1, self.banks * self.sets_per_macro)

        # phases = [] TODO

        # CLK and RST
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Chaining config regs

        # self._enable_chain_input = self.input("enable_chain_input", 1)
        # self._enable_chain_input.add_attribute(ConfigRegAttr("Enable chain on input"))

        # self._enable_chain_output = self.input("enable_chain_output", 1)
        # self._enable_chain_output.add_attribute(ConfigRegAttr("Enable chain on output"))

        # self._chain_idx_input = self.input("chain_idx_input",
        #                                    self.chain_idx_bits)
        # self._chain_idx_input.add_attribute(ConfigRegAttr("Tile input index when having multiple tiles"))
        # self._chain_idx_output = self.input("chain_idx_output",
        #                                     self.chain_idx_bits)
        # self._chain_idx_output.add_attribute(ConfigRegAttr("Tile output index when having multiple tiles"))

        # Chaining signals
        # self._chain_valid_in = self.input("chain_valid_in",
        #                                   self.interconnect_output_ports)
        # self._chain_valid_in.add_attribute(ControlSignalAttr(True))

        self._chain_data_in = self.input("chain_data_in",
                                         self.data_width,
                                         size=self.interconnect_output_ports,
                                         packed=True,
                                         explicit_array=True)
        self._chain_data_in.add_attribute(ControlSignalAttr(False))

        # self._chain_data_out = self.output("chain_data_out",
        #                                    self.data_width,
        #                                    size=self.interconnect_output_ports,
        #                                    packed=True,
        #                                    explicit_array=True)
        # self._chain_data_out.add_attribute(ControlSignalAttr(False))

        # self._chain_valid_out = self.output("chain_valid_out",
        #                                     self.interconnect_output_ports)
        # self._chain_valid_out.add_attribute(ControlSignalAttr(False))

        # Want to accept DATA_IN, CONFIG_DATA, ADDR_IN, CONFIG_ADDR, and take in the OUT
        # MAIN Inputs
        # Get the input portI  from the interconnect
        self._data_in = self.input("data_in",
                                   self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)
        self._data_in.add_attribute(ControlSignalAttr(False))

        if self.rw_same_cycle:
            self._wr_addr_in = self.input("waddr",
                                          self.data_width,
                                          size=self.interconnect_input_ports,
                                          packed=True,
                                          explicit_array=True)
            self._wr_addr_in.add_attribute(ControlSignalAttr(False))
            self._rd_addr_in = self.input("raddr",
                                          self.data_width,
                                          size=self.interconnect_input_ports,
                                          packed=True,
                                          explicit_array=True)
            self._rd_addr_in.add_attribute(ControlSignalAttr(False))
        else:
            self._addr_in = self.input("addr_in",
                                       self.data_width,
                                       size=self.interconnect_input_ports,
                                       packed=True,
                                       explicit_array=True)
            self._addr_in.add_attribute(ControlSignalAttr(False))

        self._wen = self.input("wen_in", self.interconnect_input_ports)
        self._wen.add_attribute(ControlSignalAttr(True))

        self._ren = self.input("ren_in", self.interconnect_output_ports)
        self._ren.add_attribute(ControlSignalAttr(True))

        self._config_data_in = self.input("config_data_in",
                                          self.config_data_width)
        self._config_data_in.add_attribute(ControlSignalAttr(False))

        self._config_data_in_shrt = self.var("config_data_in_shrt",
                                             self.data_width)

        self.wire(self._config_data_in_shrt, self._config_data_in[self.data_width - 1, 0])

        self._cycle_count = self.var("cycle_count", 16)
        self.add_code(self.cycle_count_inc)

        if self.stencil_valid:
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
                           valid_output=self._stencil_valid_int)
            # Wire out internal wire
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

        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)
        self._data_out.add_attribute(ControlSignalAttr(False))

        # self._valid_out = self.output("valid_out",
        #                               self.interconnect_output_ports)
        # self._valid_out.add_attribute(ControlSignalAttr(False))

        self._data_out_tile = self.var("data_out_tile",
                                       self.data_width,
                                       size=self.interconnect_output_ports,
                                       packed=True,
                                       explicit_array=True)

        self._valid_out_tile = self.output("valid_out",
                                           self.interconnect_output_ports)
        self._valid_out_tile.add_attribute(ControlSignalAttr(False))

        self.address_width = clog2(self.num_tiles * self.mem_depth)

        # Add tile enable!
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))
        # either normal or fifo mode rn...
        self.num_modes = 3
        self._mode = self.input("mode", max(1, clog2(self.num_modes)))
        self._mode.add_attribute(ConfigRegAttr("MODE!"))

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

        self._mem_data_ub = self.var("mem_data_ub",
                                     self.data_width,
                                     size=(self.banks,
                                           self.mem_input_ports,
                                           self.fw_int),
                                     packed=True,
                                     explicit_array=True)

        if self.fw_int > 1:
            strg_ub = StrgUBVec(data_width=self.data_width,
                                mem_width=self.mem_width,
                                mem_depth=self.mem_depth,
                                banks=self.banks,
                                input_addr_iterator_support=self.input_iterator_support,
                                output_addr_iterator_support=self.output_iterator_support,
                                input_sched_iterator_support=self.input_iterator_support,
                                output_sched_iterator_support=self.output_iterator_support,
                                interconnect_input_ports=self.interconnect_input_ports,
                                interconnect_output_ports=self.interconnect_output_ports,
                                mem_input_ports=self.mem_input_ports,
                                mem_output_ports=self.mem_output_ports,
                                read_delay=self.read_delay,
                                rw_same_cycle=self.rw_same_cycle,
                                agg_height=self.agg_height,
                                config_width=self.input_config_width)

        else:

            strg_ub = StrgUBThin(data_width=self.data_width,
                                 mem_width=self.mem_width,
                                 mem_depth=self.mem_depth,
                                 banks=self.banks,
                                 input_addr_iterator_support=self.input_iterator_support,
                                 input_sched_iterator_support=self.input_iterator_support,
                                 output_addr_iterator_support=self.output_iterator_support,
                                 output_sched_iterator_support=self.output_iterator_support,
                                 interconnect_input_ports=self.interconnect_input_ports,
                                 interconnect_output_ports=self.interconnect_output_ports,
                                 config_width=self.input_config_width,
                                 mem_input_ports=self.mem_input_ports,
                                 mem_output_ports=self.mem_output_ports,
                                 read_delay=self.read_delay,
                                 rw_same_cycle=self.rw_same_cycle,
                                 gen_addr=self.gen_addr)

        self._ub_data_to_mem = self.var("ub_data_to_mem",
                                        self.data_width,
                                        size=(self.banks,
                                              self.mem_input_ports,
                                              self.fw_int),
                                        packed=True,
                                        explicit_array=True)

        self._ub_data_out = self.var("ub_data_out",
                                     self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        self._ub_valid_out = self.var("ub_valid_out",
                                      self.interconnect_output_ports)

        self._ub_wen_to_mem = self.var("ub_wen_to_mem", self.mem_output_ports,
                                       size=self.banks,
                                       explicit_array=True,
                                       packed=True)
        self._ub_cen_to_mem = self.var("ub_cen_to_mem", self.mem_input_ports,
                                       size=self.banks,
                                       explicit_array=True,
                                       packed=True)

        if self.rw_same_cycle:
            self._ub_wr_addr_to_mem = self.var("ub_wr_addr_to_mem",
                                               self.address_width,
                                               size=(self.banks,
                                                     self.mem_input_ports),
                                               explicit_array=True,
                                               packed=True)
            self._ub_rd_addr_to_mem = self.var("ub_rd_addr_to_mem",
                                               self.address_width,
                                               size=(self.banks,
                                                     self.mem_output_ports),
                                               explicit_array=True,
                                               packed=True)
        else:
            self._ub_addr_to_mem = self.var("ub_addr_to_mem",
                                            self.address_width,
                                            size=(self.banks,
                                                  self.mem_input_ports),
                                            packed=True,
                                            explicit_array=True)

        self._accessor_output = self.var("accessor_output", self.interconnect_output_ports)

        self.add_child("strg_ub", strg_ub,
                       # clk + rst
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       # inputs
                       data_in=self._data_in,
                       data_from_strg=self._mem_data_out,
                       # outputs
                       data_out=self._ub_data_out,
                       data_to_strg=self._ub_data_to_mem,
                       wen_to_strg=self._ub_wen_to_mem,
                       accessor_output=self._accessor_output)

        # Handle different names - sorry
        if self.rw_same_cycle:
            self.wire(strg_ub.ports.ren_to_strg, self._ub_cen_to_mem)
        else:
            self.wire(strg_ub.ports.cen_to_strg, self._ub_cen_to_mem)

        # Wire addrs
        if self.rw_same_cycle:
            self.wire(self._ub_wr_addr_to_mem, strg_ub.ports.wr_addr_out)
            self.wire(self._ub_rd_addr_to_mem, strg_ub.ports.rd_addr_out)
        # wire single addr
        else:
            self.wire(self._ub_addr_to_mem, strg_ub.ports.addr_out)

        if self.gen_addr is False:
            self.wire(strg_ub.ports.read_addr, self._rd_addr_in[0])
            self.wire(strg_ub.ports.write_addr, self._wr_addr_in[0])
            self.wire(strg_ub.ports.wen_in, self._wen)
            self.wire(strg_ub.ports.ren_in, self._ren)

        # Wrap sram_stub
        if self.read_delay == 1 and self.rw_same_cycle is False:

            self._all_data_to_mem = self.var("all_data_to_mem", self.data_width,
                                             size=(self.num_modes,
                                                   self.banks,
                                                   self.fw_int),
                                             explicit_array=True,
                                             packed=True)
            self._all_wen_to_mem = self.var("all_wen_to_mem", self.mem_input_ports,
                                            size=(self.num_modes,
                                                  self.banks),
                                            explicit_array=True,
                                            packed=True)
            self._all_ren_to_mem = self.var("all_ren_to_mem", self.mem_output_ports,
                                            size=(self.num_modes,
                                                  self.banks),
                                            explicit_array=True,
                                            packed=True)

            self._all_addr_to_mem = self.var("all_addr_to_mem", self.address_width,
                                             size=(self.num_modes,
                                                   self.banks),
                                             explicit_array=True,
                                             packed=True)

            self._fifo_data_out = self.var("fifo_data_out", self.data_width)
            self._fifo_valid_out = self.var("fifo_valid_out", 1)
            self._fifo_empty = self.var("fifo_empty", 1)
            self._fifo_full = self.var("fifo_full", 1)
            self._fifo_data_to_mem = self.var("fifo_data_to_mem", self.data_width,
                                              size=(self.banks,
                                                    self.fw_int),
                                              explicit_array=True,
                                              packed=True)
            self._fifo_wen_to_mem = self.var("fifo_wen_to_mem", self.banks)
            self._fifo_ren_to_mem = self.var("fifo_ren_to_mem", self.banks)

            self._fifo_addr_to_mem = self.var("fifo_addr_to_mem", self.address_width,
                                              size=self.banks,
                                              explicit_array=True,
                                              packed=True)

            self._sram_data_out = self.var("sram_data_out", self.data_width)
            self._sram_valid_out = self.var("sram_valid_out", 1)
            self._sram_empty = self.var("sram_empty", 1)
            self._sram_full = self.var("sram_full", 1)
            self._sram_data_to_mem = self.var("sram_data_to_mem", self.data_width,
                                              size=(self.banks,
                                                    self.fw_int),
                                              explicit_array=True,
                                              packed=True)
            self._sram_wen_to_mem = self.var("sram_wen_to_mem", self.banks)
            self._sram_ren_to_mem = self.var("sram_ren_to_mem", self.banks)

            self._sram_addr_to_mem = self.var("sram_addr_to_mem", self.address_width,
                                              size=self.banks,
                                              explicit_array=True,
                                              packed=True)

            if self.fw_int > 1:
                self._sram_ready_out = self.output("sram_ready_out", 1)
                self._sram_ready_out.add_attribute(ControlSignalAttr(False))

            strg_ram = StrgRAM(data_width=self.data_width,
                               banks=self.banks,
                               memory_width=self.mem_width,
                               memory_depth=self.mem_depth,
                               num_tiles=self.num_tiles,
                               rw_same_cycle=self.rw_same_cycle,
                               read_delay=self.read_delay,
                               addr_width=16,
                               prioritize_write=True)

            self.add_child("sram_ctrl", strg_ram,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           wen=self._wen[0],
                           ren=self._ren[0],
                           data_in=self._data_in[0],
                           wr_addr_in=self._addr_in[0],
                           rd_addr_in=self._addr_in[0],
                           data_from_strg=self._mem_data_out,
                           data_out=self._sram_data_out,
                           valid_out=self._sram_valid_out,
                           data_to_strg=self._sram_data_to_mem,
                           wen_to_strg=self._sram_wen_to_mem,
                           ren_to_strg=self._sram_ren_to_mem,
                           addr_out=self._sram_addr_to_mem)

            if self.fw_int > 1:
                self.wire(self._sram_ready_out, strg_ram.ports.ready)

            # If we have the fifo mode enabled -
            # Instantiate a FIFO first off...
            stfo = StrgFIFO(data_width=self.data_width,
                            banks=self.banks,
                            memory_width=self.mem_width,
                            rw_same_cycle=False,
                            read_delay=self.read_delay,
                            addr_width=self.address_width)
            self.add_child("fifo_ctrl", stfo,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           data_in=self._data_in[0],
                           push=self._wen[0],
                           pop=self._ren[0],
                           data_from_strg=self._mem_data_out,
                           data_out=self._fifo_data_out,
                           valid_out=self._fifo_valid_out,
                           empty=self._fifo_empty,
                           full=self._fifo_full,
                           data_to_strg=self._fifo_data_to_mem,
                           wen_to_strg=self._fifo_wen_to_mem,
                           ren_to_strg=self._fifo_ren_to_mem,
                           addr_out=self._fifo_addr_to_mem)

            self._empty = self.output("empty", 1)
            self._empty.add_attribute(ControlSignalAttr(False))

            self._full = self.output("full", 1)
            self._full.add_attribute(ControlSignalAttr(False))

            self.wire(self._empty, self._fifo_empty)
            self.wire(self._full, self._fifo_full)

            self.wire(self._all_data_to_mem[0], self._ub_data_to_mem)
            self.wire(self._all_wen_to_mem[0], self._ub_wen_to_mem)
            self.wire(self._all_ren_to_mem[0], self._ub_cen_to_mem)
            self.wire(self._all_addr_to_mem[0], self._ub_addr_to_mem)

            self.wire(self._all_data_to_mem[1], self._fifo_data_to_mem)
            for i in range(self.banks):
                self.wire(self._all_wen_to_mem[1][i], self._fifo_wen_to_mem[i])
                self.wire(self._all_ren_to_mem[1][i], self._fifo_ren_to_mem[i])
                self.wire(self._all_addr_to_mem[1][i], self._fifo_addr_to_mem[i])

            self.wire(self._all_data_to_mem[2], self._sram_data_to_mem)
            for i in range(self.banks):
                self.wire(self._all_wen_to_mem[2][i], self._sram_wen_to_mem[i])
                self.wire(self._all_ren_to_mem[2][i], self._sram_ren_to_mem[i])
                self.wire(self._all_addr_to_mem[2][i], self._sram_addr_to_mem[i])

            # Mux all of these signals when in FIFO mode
            self.wire(self._mem_data_dp, self._all_data_to_mem[self._mode])
            self.wire(self._mem_cen_dp, self._all_ren_to_mem[self._mode] | self._all_wen_to_mem[self._mode])
            self.wire(self._mem_wen_dp, self._all_wen_to_mem[self._mode])
            self.wire(self._mem_addr_dp, self._all_addr_to_mem[self._mode])

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

            self.wire(self._mem_data_dp, self._ub_data_to_mem)
            self.wire(self._mem_wen_dp, self._ub_wen_to_mem)
            self.wire(self._mem_cen_dp, self._ub_cen_to_mem)
            if self.rw_same_cycle:
                self.wire(self._wr_mem_addr_dp, self._ub_wr_addr_to_mem)
                self.wire(self._rd_mem_addr_dp, self._ub_rd_addr_to_mem)
            else:
                self.wire(self._mem_addr_dp, self._ub_addr_to_mem)

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

        if self.fifo_mode:
            self._all_data_out = self.var("all_data_out", self.data_width,
                                          size=self.num_modes,
                                          explicit_array=True,
                                          packed=True)
            self._all_valid_out = self.var("all_valid_out", self.num_modes)

            self.wire(self._all_data_out[0], self._ub_data_out[0])
            self.wire(self._all_valid_out[0], self._ub_valid_out[0])

            self.wire(self._all_data_out[1], self._fifo_data_out)
            self.wire(self._all_valid_out[1], self._fifo_valid_out)

            self.wire(self._all_data_out[2], self._sram_data_out)
            self.wire(self._all_valid_out[2], self._sram_valid_out)

            self.wire(self._data_out_tile[0], self._all_data_out[self._mode])
            self.wire(self._valid_out_tile[0], self._all_valid_out[self._mode])
        else:
            self.wire(self._data_out_tile[0], self._ub_data_out[0])
            self.wire(self._valid_out_tile[0], self._ub_valid_out[0])

        for i in range(self.interconnect_output_ports - 1):
            self.wire(self._data_out_tile[i + 1], self._ub_data_out[i + 1])
            self.wire(self._valid_out_tile[i + 1], self._ub_valid_out[i + 1])

        chaining = ChainAccessor(data_width=self.data_width,
                                 interconnect_output_ports=self.interconnect_output_ports)

        self._mode_mask = self.var("mode_mask", self._accessor_output.width)
        self.wire(self._mode_mask[0], self._mode.r_or())
        for i in range(self._accessor_output.width - 1):
            self.wire(self._mode_mask[i + 1], kts.const(0, 1))

        self.add_child(f"chain", chaining,
                       curr_tile_data_out=self._data_out_tile,
                       chain_data_in=self._chain_data_in,
                       accessor_output=(self._accessor_output | self._mode_mask),
                       data_out_tile=self._data_out)

        ########################
        ##### CLOCK ENABLE #####
        ########################
        if add_clk_enable:
            # self.clock_en("clk_en")
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            # Add input attr-
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))

        if add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

        # config regs
        lift_config_reg(self.internal_generator)

        # extract_formal_annotation(self.internal_generator, "test.txt")

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

        if "in2agg_0" in root_node:
            in2agg = map_controller(extract_controller_json(root_node["in2agg_0"]), "in2agg_0")
            config.append(trim_config(flattened, "strg_ub_agg_write_addr_gen_0_starting_addr", in2agg.in_data_strt))
            config.append(trim_config(flattened, "strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr", in2agg.cyc_strt))
            config.append(trim_config(flattened, "strg_ub_loops_in2buf_0_dimensionality", in2agg.dim))
            for i in range(in2agg.dim):
                config.append(trim_config(flattened, f"strg_ub_loops_in2buf_0_ranges_{i}", in2agg.extent[i]))
                config.append(trim_config(flattened, f"strg_ub_agg_write_addr_gen_0_strides_{i}", in2agg.in_data_stride[i]))
                config.append(trim_config(flattened, f"strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_{i}", in2agg.cyc_stride[i]))

        if "agg2sram" in root_node:
            agg2sram = map_controller(extract_controller_json(root_node["agg2sram"]), "agg2sram")
            config.append(trim_config(flattened, "strg_ub_agg_read_addr_gen_0_starting_addr", agg2sram.out_data_strt))
            config.append(trim_config(flattened, "strg_ub_input_addr_gen_starting_addr", agg2sram.in_data_strt))
            config.append(trim_config(flattened, "strg_ub_input_sched_gen_sched_addr_gen_starting_addr", agg2sram.cyc_strt))
            config.append(trim_config(flattened, "strg_ub_loops_in2buf_autovec_write_dimensionality", agg2sram.dim))
            for i in range(agg2sram.dim):
                config.append(trim_config(flattened, f"strg_ub_agg_read_addr_gen_0_strides_{i}", agg2sram.out_data_stride[i]))
                config.append(trim_config(flattened, f"strg_ub_loops_in2buf_autovec_write_ranges_{i}", agg2sram.extent[i]))
                config.append(trim_config(flattened, f"strg_ub_input_addr_gen_strides_{i}", agg2sram.in_data_stride[i]))
                config.append(trim_config(flattened, f"strg_ub_input_sched_gen_sched_addr_gen_strides_{i}", agg2sram.cyc_stride[i]))
        # Count tbs
        num_tbs = 0

        if "tb2out_0" in root_node:
            num_tbs += 1
            tb2out_0 = map_controller(extract_controller_json(root_node["tb2out_0"]), "tb2out_0")
            config.append(trim_config(flattened, "strg_ub_tb_read_addr_gen_0_starting_addr", tb2out_0.out_data_strt))
            config.append(trim_config(flattened, "strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr", tb2out_0.cyc_strt))
            config.append(trim_config(flattened, "strg_ub_loops_buf2out_read_0_dimensionality", tb2out_0.dim))
            for i in range(tb2out_0.dim):
                config.append(trim_config(flattened, f"strg_ub_loops_buf2out_read_0_ranges_{i}", tb2out_0.extent[i]))
                config.append(trim_config(flattened, f"strg_ub_tb_read_addr_gen_0_strides_{i}", tb2out_0.out_data_stride[i]))
                config.append(trim_config(flattened, f"strg_ub_tb_read_sched_gen_0_sched_addr_gen_strides_{i}", tb2out_0.cyc_stride[i]))

        if "tb2out_1" in root_node:
            num_tbs += 1
            tb2out_1 = map_controller(extract_controller_json(root_node["tb2out_1"]), "tb2out_1")
            config.append(trim_config(flattened, "strg_ub_tb_read_addr_gen_1_starting_addr", tb2out_1.out_data_strt))
            config.append(trim_config(flattened, "strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr", tb2out_1.cyc_strt))
            config.append(trim_config(flattened, "strg_ub_loops_buf2out_read_1_dimensionality", tb2out_1.dim))
            for i in range(tb2out_1.dim):
                config.append(trim_config(flattened, f"strg_ub_loops_buf2out_read_1_ranges_{i}", tb2out_1.extent[i]))
                config.append(trim_config(flattened, f"strg_ub_tb_read_addr_gen_1_strides_{i}", tb2out_1.out_data_stride[i]))
                config.append(trim_config(flattened, f"strg_ub_tb_read_sched_gen_1_sched_addr_gen_strides_{i}", tb2out_1.cyc_stride[i]))

        if "sram2tb" in root_node:
            sram2tb = map_controller(extract_controller_json(root_node["sram2tb"]), "sram2tb")
            config.append(trim_config(flattened, "strg_ub_output_addr_gen_starting_addr", sram2tb.out_data_strt))
            config.append(trim_config(flattened, "strg_ub_tb_write_addr_gen_0_starting_addr", sram2tb.in_data_strt))
            config.append(trim_config(flattened, "strg_ub_tb_write_addr_gen_1_starting_addr", sram2tb.in_data_strt))
            config.append(trim_config(flattened, "strg_ub_out_port_sel_addr_starting_addr", sram2tb.mux_data_strt))
            config.append(trim_config(flattened, "strg_ub_output_sched_gen_sched_addr_gen_starting_addr", sram2tb.cyc_strt))
            config.append(trim_config(flattened, "strg_ub_loops_buf2out_autovec_read_dimensionality", sram2tb.dim))
            for i in range(sram2tb.dim):
                config.append(trim_config(flattened, f"strg_ub_loops_buf2out_autovec_read_ranges_{i}", sram2tb.extent[i]))
                config.append(trim_config(flattened, f"strg_ub_output_addr_gen_strides_{i}", sram2tb.out_data_stride[i]))
                config.append(trim_config(flattened, f"strg_ub_output_sched_gen_sched_addr_gen_strides_{i}", sram2tb.cyc_stride[i]))
                config.append(trim_config(flattened, f"strg_ub_out_port_sel_addr_strides_{i}", sram2tb.mux_data_stride[i]))
                for tb in range(num_tbs):
                    config.append(trim_config(flattened, f"strg_ub_tb_write_addr_gen_{tb}_strides_{i}", sram2tb.in_data_stride[i]))

        if "stencil_valid" in root_node:
            stencil_valid = map_controller(extract_controller_json(root_node["stencil_valid"]), "stencil_valid")
            # Check actual stencil valid property of hardware before programming
            if self.stencil_valid:
                config.append(trim_config(flattened, f"loops_stencil_valid_dimensionality", stencil_valid.dim))
                config.append(trim_config(flattened, f"stencil_valid_sched_gen_sched_addr_gen_starting_addr", stencil_valid.cyc_strt))
                for i in range(stencil_valid.dim):
                    config.append(trim_config(flattened, f"loops_stencil_valid_ranges_{i}", stencil_valid.extent[i]))
                    config.append(trim_config(flattened, f"stencil_valid_sched_gen_sched_addr_gen_strides_{i}", stencil_valid.cyc_stride[i]))

        # Control Signals...
        config.append(trim_config(flattened, "flush_reg_sel", 0))  # 1
        config.append(trim_config(flattened, "flush_reg_value", 0))  # 1
        # ("chain_valid_in_reg_sel", 1),  # 1
        # ("ren_in_reg_sel", 1),  # 1
        # ("ren_in_reg_value", 0),  # 1
        # ("wen_in_reg_sel", 1),  # 1
        # ("wen_in_reg_value", 0),  # 1
        # Set the mode and activate the tile...
        config.append(trim_config(flattened, "mode", 0))  # 2
        config.append(trim_config(flattened, "tile_en", 1))  # 1

        # TODO: Maybe need to check if size 1?
        for i in range(input_ports):
            config.append(trim_config(flattened, f"ren_in_{i}_reg_sel", 1))
            config.append(trim_config(flattened, f"ren_in_{i}_reg_value", 0))

        for i in range(output_ports):
            config.append(trim_config(flattened, f"wen_in_{i}_reg_sel", 1))
            config.append(trim_config(flattened, f"wen_in_{i}_reg_value", 0))

        return config

    def get_static_bitstream(self,
                             config_path,
                             in_file_name,
                             out_file_name):

        input_ports = 1
        output_ports = 1

        in2agg = map_controller(extract_controller(config_path + '/' + in_file_name + '_in2agg_0.csv'), "in2agg")
        agg2sram = map_controller(extract_controller(config_path + '/' + in_file_name + '_agg2sram.csv'), "agg2sram")
        sram2tb = map_controller(extract_controller(config_path + '/' + out_file_name + '_2_sram2tb.csv'), "sram2tb")
        tb2out0 = map_controller(extract_controller(config_path + '/' + out_file_name + '_2_tb2out_0.csv'), "tb2out0")
        tb2out1 = map_controller(extract_controller(config_path + '/' + out_file_name + '_2_tb2out_1.csv'), "tb2out1")

        # Getting bitstreams is a little unweildy due to fault (or its underlying implementation) not
        # handling arrays in the interface.
        # To alleviate this, we create the flattened wrapper so we can query widths of config
        # registers and trim values to their bitwidths...
        print(f"Current_name: {self.name}")
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Set configuration...
        config = [
            trim_config(flattened, "strg_ub_agg_read_addr_gen_0_starting_addr", agg2sram.out_data_strt),
            trim_config(flattened, "strg_ub_input_addr_gen_starting_addr", agg2sram.in_data_strt),
            trim_config(flattened, "strg_ub_input_sched_gen_sched_addr_gen_starting_addr", agg2sram.cyc_strt),
            trim_config(flattened, "strg_ub_loops_in2buf_autovec_write_dimensionality", agg2sram.dim),

            trim_config(flattened, "strg_ub_output_addr_gen_starting_addr", sram2tb.out_data_strt),
            trim_config(flattened, "strg_ub_tb_write_addr_gen_0_starting_addr", sram2tb.in_data_strt),
            trim_config(flattened, "strg_ub_tb_write_addr_gen_1_starting_addr", sram2tb.in_data_strt),
            trim_config(flattened, "strg_ub_out_port_sel_addr_starting_addr", sram2tb.mux_data_strt),
            trim_config(flattened, "strg_ub_output_sched_gen_sched_addr_gen_starting_addr", sram2tb.cyc_strt),
            trim_config(flattened, "strg_ub_loops_buf2out_autovec_read_dimensionality", sram2tb.dim),

            trim_config(flattened, "strg_ub_agg_write_addr_gen_0_starting_addr", in2agg.in_data_strt),
            trim_config(flattened, "strg_ub_agg_write_sched_gen_0_sched_addr_gen_starting_addr", in2agg.cyc_strt),
            trim_config(flattened, "strg_ub_loops_in2buf_0_dimensionality", in2agg.dim),

            trim_config(flattened, "strg_ub_tb_read_addr_gen_0_starting_addr", tb2out0.out_data_strt),
            trim_config(flattened, "strg_ub_tb_read_sched_gen_0_sched_addr_gen_starting_addr", tb2out0.cyc_strt),
            trim_config(flattened, "strg_ub_loops_buf2out_read_0_dimensionality", tb2out0.dim),

            trim_config(flattened, "strg_ub_tb_read_addr_gen_1_starting_addr", tb2out1.out_data_strt),
            trim_config(flattened, "strg_ub_tb_read_sched_gen_1_sched_addr_gen_starting_addr", tb2out1.cyc_strt),
            trim_config(flattened, "strg_ub_loops_buf2out_read_1_dimensionality", tb2out1.dim),

            # ("chain_valid_in_reg_sel", 1),  # 1

            # Control Signals...
            trim_config(flattened, "flush_reg_sel", 0),  # 1
            trim_config(flattened, "flush_reg_value", 0),  # 1
            # ("ren_in_reg_sel", 1),  # 1
            # ("ren_in_reg_value", 0),  # 1
            # ("wen_in_reg_sel", 1),  # 1
            # ("wen_in_reg_value", 0),  # 1

            # Set the mode and activate the tile...
            trim_config(flattened, "mode", 0),  # 2
            trim_config(flattened, "tile_en", 1),  # 1
        ]

        # TODO: Check actual stencil_valid property for hardware
        if self.stencil_valid:
            cfg_path = config_path + '/' + 'stencil_valid.csv'
            # Check if the stencil valid file exists...if it doesn't we just won't program it
            if os.path.exists(cfg_path):
                stcl_valid = map_controller(extract_controller(cfg_path), "stencil_valid")
                config.append(trim_config(flattened, f"loops_stencil_valid_dimensionality", stcl_valid.dim))
                config.append(trim_config(flattened, f"stencil_valid_sched_gen_sched_addr_gen_starting_addr", stcl_valid.cyc_strt))
                for i in range(stcl_valid.dim):
                    config.append(trim_config(flattened, f"loops_stencil_valid_ranges_{i}", stcl_valid.extent[i]))
                    config.append(trim_config(flattened, f"stencil_valid_sched_gen_sched_addr_gen_strides_{i}", stcl_valid.cyc_stride[i]))
            else:
                print("No configuration file provided for stencil valid...are you expecting one to exist?")
                print(f"Bogus stencil valid path: {cfg_path}")

        # TODO: Maybe need to check if size 1?
        for i in range(input_ports):
            config.append(trim_config(flattened, f"ren_in_{i}_reg_sel", 1))
            config.append(trim_config(flattened, f"ren_in_{i}_reg_value", 0))

        for i in range(output_ports):
            config.append(trim_config(flattened, f"wen_in_{i}_reg_sel", 1))
            config.append(trim_config(flattened, f"wen_in_{i}_reg_value", 0))

        for i in range(in2agg.dim):
            config.append(trim_config(flattened, f"strg_ub_loops_in2buf_0_ranges_{i}", in2agg.extent[i]))
            config.append(trim_config(flattened, f"strg_ub_agg_write_addr_gen_0_strides_{i}", in2agg.in_data_stride[i]))
            config.append(trim_config(flattened, f"strg_ub_agg_write_sched_gen_0_sched_addr_gen_strides_{i}", in2agg.cyc_stride[i]))

        for i in range(agg2sram.dim):
            # config.append((f"strg_ub_loops_in2buf_autovec_read_0_ranges_{i}", agg2sram.extent[i]))
            config.append(trim_config(flattened, f"strg_ub_agg_read_addr_gen_0_strides_{i}", agg2sram.out_data_stride[i]))
            config.append(trim_config(flattened, f"strg_ub_loops_in2buf_autovec_write_ranges_{i}", agg2sram.extent[i]))
            config.append(trim_config(flattened, f"strg_ub_input_addr_gen_strides_{i}", agg2sram.in_data_stride[i]))
            config.append(trim_config(flattened, f"strg_ub_input_sched_gen_sched_addr_gen_strides_{i}", agg2sram.cyc_stride[i]))

        tbs = [tb2out0, tb2out1]

        for i in range(sram2tb.dim):
            config.append(trim_config(flattened, f"strg_ub_loops_buf2out_autovec_read_ranges_{i}", sram2tb.extent[i]))
            config.append(trim_config(flattened, f"strg_ub_output_addr_gen_strides_{i}", sram2tb.out_data_stride[i]))
            config.append(trim_config(flattened, f"strg_ub_output_sched_gen_sched_addr_gen_strides_{i}", sram2tb.cyc_stride[i]))
            config.append(trim_config(flattened, f"strg_ub_out_port_sel_addr_strides_{i}", sram2tb.mux_data_stride[i]))
            for tb in range(len(tbs)):
                config.append(trim_config(flattened, f"strg_ub_tb_write_addr_gen_{tb}_strides_{i}", sram2tb.in_data_stride[i]))
        tbs = [tb2out0, tb2out1]
        for tb in range(len(tbs)):
            elem = tbs[tb]
            for i in range(elem.dim):
                config.append(trim_config(flattened, f"strg_ub_loops_buf2out_read_{tb}_ranges_{i}", elem.extent[i]))
                config.append(trim_config(flattened, f"strg_ub_tb_read_addr_gen_{tb}_strides_{i}", elem.out_data_stride[i]))
                config.append(trim_config(flattened, f"strg_ub_tb_read_sched_gen_{tb}_sched_addr_gen_strides_{i}", elem.cyc_stride[i]))

        return config


if __name__ == "__main__":
    tsmc_info = SRAMMacroInfo("tsmc_name")
    use_sram_stub = True
    fifo_mode = True
    mem_width = 64
    lake_dut = LakeTop(mem_width=mem_width,
                       sram_macro_info=tsmc_info,
                       use_sram_stub=use_sram_stub,
                       fifo_mode=fifo_mode,
                       add_clk_enable=True,
                       add_flush=True)
    print(f"Supports Stencil Valid: {lake_dut.supports('stencil_valid')}")
    sram_port_pass = change_sram_port_names(use_sram_stub=use_sram_stub, sram_macro_info=tsmc_info)
    verilog(lake_dut, filename="lake_top.sv",
            optimize_if=False,
            additional_passes={"change sram port names": sram_port_pass})
