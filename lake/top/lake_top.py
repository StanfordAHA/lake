from kratos import *
from lake.modules.passthru import *
from lake.modules.sram_wrapper import SRAMWrapper
from lake.modules.aggregation_buffer import AggregationBuffer
from lake.modules.input_addr_ctrl import InputAddrCtrl
from lake.modules.output_addr_ctrl import OutputAddrCtrl
from lake.modules.agg_aligner import AggAligner
from lake.modules.rw_arbiter import RWArbiter
from lake.modules.transpose_buffer import TransposeBuffer
from lake.modules.transpose_buffer_aggregation import TransposeBufferAggregation
from lake.modules.demux_reads import DemuxReads
from lake.modules.sync_groups import SyncGroups
from lake.modules.prefetcher import Prefetcher
from lake.modules.storage_config_seq import StorageConfigSeq
from lake.modules.register_file import RegisterFile
from lake.modules.strg_fifo import StrgFIFO
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg, change_sram_port_names
import kratos as kts


class LakeTop(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=2,
                 input_iterator_support=6,  # Addr Controllers
                 output_iterator_support=6,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sram_stub=0,
                 sram_name="tsmc_sram_stub",
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4,
                 max_agg_schedule=32,
                 input_max_port_sched=32,
                 output_max_port_sched=32,
                 align_input=1,
                 max_line_length=128,
                 max_tb_height=1,
                 tb_range_max=128,
                 tb_sched_max=64,
                 max_tb_stride=15,
                 num_tb=1,
                 tb_iterator_support=2,
                 multiwrite=1,
                 max_prefetch=64,
                 config_data_width=16,
                 config_addr_width=8,
                 remove_tb=False,
                 fifo_mode=False):
        super().__init__("LakeTop", debug=True)

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.input_iterator_support = input_iterator_support
        self.output_iterator_support = output_iterator_support
        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.use_sram_stub = use_sram_stub
        self.sram_name = sram_name
        self.agg_height = agg_height
        self.max_agg_schedule = max_agg_schedule
        self.input_max_port_sched = input_max_port_sched
        self.output_max_port_sched = output_max_port_sched
        self.input_port_sched_width = clog2(self.interconnect_input_ports)
        self.align_input = align_input
        self.max_line_length = max_line_length
        assert self.mem_width >= self.data_width, "Data width needs to be smaller than mem"
        self.fw_int = int(self.mem_width / self.data_width)
        self.num_tb = num_tb
        self.max_tb_height = max_tb_height
        self.tb_range_max = tb_range_max
        self.max_tb_stride = max_tb_stride
        self.tb_sched_max = tb_sched_max
        self.tb_iterator_support = tb_iterator_support
        self.multiwrite = multiwrite
        self.max_prefetch = max_prefetch
        self.config_data_width = config_data_width
        self.config_addr_width = config_addr_width
        self.remove_tb = remove_tb
        self.read_delay = read_delay
        self.rw_same_cycle = rw_same_cycle
        self.fifo_mode = fifo_mode

        self.data_words_per_set = 2 ** self.config_addr_width
        self.sets = int((self.fw_int * self.mem_depth) / self.data_words_per_set)

        self.sets_per_macro = max(1, int(self.mem_depth / self.data_words_per_set))
        self.total_sets = max(1, self.banks * self.sets_per_macro)
        # phases = [] TODO

        # CLK and RST
        self._gclk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        # Want to accept DATA_IN, CONFIG_DATA, ADDR_IN, CONFIG_ADDR, and take in the OUT
        # MAIN Inputs
        # Get the input portI  from the interconnect
        self._data_in = self.input("data_in",
                                   self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)
        self._addr_in = self.input("addr_in",
                                   self.data_width,
                                   size=self.interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        self._wen = self.input("wen", self.interconnect_input_ports)
        self._ren = self.input("ren", self.interconnect_output_ports)

        self._config_data_in = self.input("config_data_in",
                                          self.config_data_width)

        self._config_addr_in = self.input("config_addr_in",
                                          self.config_addr_width)

        self._config_data_out = self.output("config_data_out", self.config_data_width,
                                            size=self.total_sets,
                                            explicit_array=True,
                                            packed=True)

        self._config_read = self.input("config_read", 1)
        self._config_write = self.input("config_write", 1)
        self._config_en = self.input("config_en", self.total_sets)

        self._output_en = self.input("output_en", 1)

        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        self._valid_out = self.output("valid_out",
                                      self.interconnect_output_ports)

        if self.banks == 1:
            self.address_width = clog2(mem_depth)
        else:
            self.address_width = clog2(mem_depth)  # + clog2(banks)

        # Add tile enable!
        # self._tile_en = self.input("tile_en", 1)
        # self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))
        # either normal or fifo mode rn...
        self._mode = self.input("mode", 1)
        self._mode.add_attribute(ConfigRegAttr("MODE!"))

        # Currenlt mode = 0 is UB, mode = 1 is FIFO

        # self._gclk = self.var("gclk", 1)
        # self.wire(self._gclk, kts.util.clock(self._clk & self._tile_en))

        ###########################
        ##### INPUT AGG SCHED #####
        ###########################

        ###########################################
        ##### AGGREGATION ALIGNERS (OPTIONAL) #####
        ###########################################
        # These variables are holders and can be swapped out if needed
        self._data_consume = self._data_in
        self._valid_consume = self._wen
        # Zero out if not aligning
        if(self.agg_height > 0):
            self._align_to_agg = self.var("align_input",
                                          self.interconnect_input_ports)
        # Add the aggregation buffer aligners
        if(self.align_input):
            self._data_consume = self.var("data_consume",
                                          self.data_width,
                                          size=self.interconnect_input_ports,
                                          packed=True,
                                          explicit_array=True)
            self._valid_consume = self.var("valid_consume",
                                           self.interconnect_input_ports)

            # Make new aggregation aligners for each port
            for i in range(self.interconnect_input_ports):
                new_child = AggAligner(self.data_width, self.max_line_length)
                self.add_child(f"agg_align_{i}", new_child,
                               clk=self._gclk,
                               rst_n=self._rst_n,
                               in_dat=self._data_in[i],
                               in_valid=self._wen[i],
                               align=self._align_to_agg[i],
                               out_valid=self._valid_consume[i],
                               out_dat=self._data_consume[i])
        else:
            if self.agg_height > 0:
                self.wire(self._align_to_agg, const(0, self._align_to_agg.width))
        ################################################
        ##### END: AGGREGATION ALIGNERS (OPTIONAL) #####
        ################################################

        self._to_iac_dat = self._data_consume
        self._to_iac_valid = self._valid_consume

        ##################################
        ##### AGG BUFFERS (OPTIONAL) #####
        ##################################
        # Only instantiate agg_buffer if needed
        if(self.agg_height > 0):
            self._to_iac_dat = self.var("ab_to_mem_dat",
                                        self.mem_width,
                                        size=self.interconnect_input_ports,
                                        packed=True,
                                        explicit_array=True)

            self._to_iac_valid = self.var("ab_to_mem_valid",
                                          self.interconnect_input_ports)

            self._agg_buffers = []
            # Add input aggregations buffers
            for i in range(self.interconnect_input_ports):
                # add children aggregator buffers...
                agg_buffer_new = AggregationBuffer(self.agg_height,
                                                   self.data_width,
                                                   self.mem_width,
                                                   self.max_agg_schedule)
                self._agg_buffers.append(agg_buffer_new)
                self.add_child(f"agg_in_{i}", agg_buffer_new,
                               clk=self._gclk,
                               rst_n=self._rst_n,
                               data_in=self._data_consume[i],
                               valid_in=self._valid_consume[i],
                               align=self._align_to_agg[i],
                               write_act=const(1, 1),
                               data_out=self._to_iac_dat[i],
                               valid_out=self._to_iac_valid[i])

        #######################################
        ##### END: AGG BUFFERS (OPTIONAL) #####
        #######################################
        self._arb_wen_in = self.input("wen_en", self.interconnect_input_ports)
        self._arb_ren_in = self.input("ren_en", self.interconnect_output_ports)
        # self._arb_ren_in = self.input("arb_ren_in", self.interconnect_output_ports)
        self._ready_tba = self.var("ready_tba", self.interconnect_output_ports)
        ####################################
        ##### INPUT ADDRESS CONTROLLER #####
        ####################################
        self._wen_to_arb = self.var("wen_to_arb", self.mem_input_ports,
                                    size=self.banks,
                                    explicit_array=True,
                                    packed=True)
        self._addr_to_arb = self.var("addr_to_arb",
                                     self.address_width,
                                     size=(self.banks,
                                           self.mem_input_ports),
                                     explicit_array=True,
                                     packed=True)
        self._data_to_arb = self.var("data_to_arb",
                                     self.data_width,
                                     size=(self.banks,
                                           self.mem_input_ports,
                                           self.fw_int),
                                     explicit_array=True,
                                     packed=True)

        # Connect these inputs ports to an address generator
        iac = InputAddrCtrl(interconnect_input_ports=self.interconnect_input_ports,
                            mem_depth=self.mem_depth,
                            banks=self.banks,
                            iterator_support=self.input_iterator_support,
                            address_width=self.address_width,
                            data_width=self.data_width,
                            fetch_width=self.mem_width,
                            multiwrite=self.multiwrite,
                            strg_wr_ports=self.mem_input_ports)
        self.add_child(f"input_addr_ctrl", iac,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       valid_in=self._to_iac_valid,
                       wen_en=self._arb_wen_in,
                       data_in=self._to_iac_dat,
                       wen_to_sram=self._wen_to_arb,
                       addr_out=self._addr_to_arb,
                       data_out=self._data_to_arb)

        #########################################
        ##### END: INPUT ADDRESS CONTROLLER #####
        #########################################
        self._arb_acks = self.var("arb_acks",
                                  self.interconnect_output_ports,
                                  size=self.banks,
                                  explicit_array=True,
                                  packed=True)

        self._ack_transpose = self.var("ack_transpose",
                                       self.banks,
                                       size=self.interconnect_output_ports,
                                       explicit_array=True,
                                       packed=True)

        self._ack_reduced = self.var("ack_reduced",
                                     self.interconnect_output_ports)
        self._prefetch_step = self.var("prefetch_step", self.interconnect_output_ports)
        self._oac_step = self.var("oac_step", self.interconnect_output_ports)
        self._oac_valid = self.var("oac_valid", self.interconnect_output_ports)
        self._ren_out = self.var("ren_out",
                                 self.interconnect_output_ports,
                                 size=self.banks,
                                 explicit_array=True,
                                 packed=True)
        self._ren_out_tpose = self.var("ren_out_tpose",
                                       self.banks,
                                       size=self.interconnect_output_ports,
                                       explicit_array=True,
                                       packed=True)
        self._ren_out_reduced = self.var("ren_out_reduced",
                                         self.interconnect_output_ports)
        self._addr_out = self.var("addr_out",
                                  clog2(self.mem_depth),
                                  size=self.interconnect_output_ports,
                                  explicit_array=True,
                                  packed=True)
        #####################################
        ##### OUTPUT ADDRESS CONTROLLER #####
        #####################################
        oac = OutputAddrCtrl(interconnect_output_ports=self.interconnect_output_ports,
                             mem_depth=self.mem_depth,
                             banks=self.banks,
                             iterator_support=self.output_iterator_support,
                             address_width=self.address_width)

        if self.remove_tb:
            self.wire(self._oac_valid, self._ren)
            self.wire(self._oac_step, self._ren)
        else:
            self.wire(self._oac_valid, self._prefetch_step)
            self.wire(self._oac_step, self._ack_reduced)

        self.add_child(f"output_addr_ctrl", oac,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       valid_in=self._oac_valid,
                       ren=self._ren_out,
                       addr_out=self._addr_out,
                       step_in=self._oac_step)

        for i in range(self.interconnect_output_ports):
            for j in range(self.banks):
                self.wire(self._ren_out_tpose[i][j], self._ren_out[j][i])
        ##############################
        ##### READ/WRITE ARBITER #####
        ##############################
        # Hook up the read write arbiters for each bank
        self._arb_dat_out = self.var("arb_dat_out",
                                     self.data_width,
                                     size=(self.banks,
                                           self.mem_output_ports,
                                           self.fw_int),
                                     explicit_array=True,
                                     packed=True)

        self._arb_port_out = self.var("arb_port_out",
                                      self.interconnect_output_ports,
                                      size=(self.banks,
                                            self.mem_output_ports),
                                      explicit_array=True,
                                      packed=True)
        self._arb_valid_out = self.var("arb_valid_out", self.mem_output_ports,
                                       size=self.banks,
                                       explicit_array=True,
                                       packed=True)

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

        self._rd_sync_gate = self.var("rd_sync_gate",
                                      self.interconnect_output_ports)

        self._mem_ren_cfg = self.var("mem_ren_cfg", self.banks)
        self._mem_wen_cfg = self.var("mem_wen_cfg", self.banks)

        self._mem_cen_datapath = self.var("mem_cen_datapath", self.mem_output_ports,
                                          size=self.banks,
                                          explicit_array=True,
                                          packed=True)
        self._mem_wen_datapath = self.var("mem_wen_datapath", self.mem_input_ports,
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

        self.arbiters = []
        for i in range(self.banks):
            rw_arb = RWArbiter(fetch_width=self.mem_width,
                               data_width=self.data_width,
                               memory_depth=self.mem_depth,
                               int_in_ports=self.interconnect_input_ports,
                               int_out_ports=self.interconnect_output_ports,
                               strg_wr_ports=self.mem_input_ports,
                               strg_rd_ports=self.mem_output_ports,
                               read_delay=self.read_delay,
                               rw_same_cycle=self.rw_same_cycle,
                               separate_addresses=self.rw_same_cycle)
            self.arbiters.append(rw_arb)
            self.add_child(f"rw_arb_{i}", rw_arb,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           wen_in=self._wen_to_arb[i],
                           w_data=self._data_to_arb[i],
                           w_addr=self._addr_to_arb[i],
                           data_from_mem=self._mem_data_out[i],
                           ren_en=self._arb_ren_in,
                           rd_addr=self._addr_out,
                           out_data=self._arb_dat_out[i],
                           out_port=self._arb_port_out[i],
                           out_valid=self._arb_valid_out[i],
                           cen_mem=self._mem_cen_datapath[i],
                           wen_mem=self._mem_wen_datapath[i],
                           data_to_mem=self._mem_data_dp[i],
                           out_ack=self._arb_acks[i])

            # Bind the separate addrs
            if self.rw_same_cycle:
                self.wire(rw_arb.ports.wr_addr_to_mem, self._wr_mem_addr_dp[i])
                self.wire(rw_arb.ports.rd_addr_to_mem, self._rd_mem_addr_dp[i])
            else:
                self.wire(rw_arb.ports.addr_to_mem, self._mem_addr_dp[i])

            if self.remove_tb:
                self.wire(rw_arb.ports.ren_in, self._ren_out[i])
            else:
                self.wire(rw_arb.ports.ren_in, self._ren_out[i] & self._rd_sync_gate)

        ####################################
        ##### DEMUX WRITE/SRAM WRAPPER #####
        ####################################

        stg_cfg_seq = StorageConfigSeq(data_width=16,
                                       config_addr_width=self.config_addr_width,
                                       addr_width=self.address_width,
                                       fetch_width=self.mem_width,
                                       total_sets=self.total_sets,
                                       sets_per_macro=self.sets_per_macro)

        self.add_child(f"config_seq", stg_cfg_seq,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       config_data_in=self._config_data_in,
                       config_addr_in=self._config_addr_in,
                       config_wr=self._config_write,
                       config_rd=self._config_read,
                       config_en=self._config_en,
                       rd_data_stg=self._mem_data_low_pt,
                       wr_data=self._mem_data_cfg,
                       rd_data_out=self._config_data_out,
                       addr_out=self._mem_addr_cfg,
                       wen_out=self._mem_wen_cfg,
                       ren_out=self._mem_ren_cfg)

        for i in range(self.banks):
            # If we are not sharing the ports...
            if self.rw_same_cycle:
                self.wire(self._mem_wen_in[i][0], ternary(self._config_en.r_or(),
                                                          self._mem_wen_cfg[i],
                                                          self._mem_wen_datapath[i][0]))
                # Treat cen as ren for all intents and purpose
                self.wire(self._mem_cen_in[i][0], ternary(self._config_en.r_or(),
                                                          self._mem_ren_cfg[i],
                                                          self._mem_cen_datapath[i][0]))
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
                    self.wire(self._mem_wen_in[i][j + 1], self._mem_wen_datapath[i][j + 1])
                    self.wire(self._wr_mem_addr_in[i][j + 1], self._wr_mem_addr_dp[i][j + 1])
                    self.wire(self._mem_data_in[i][j + 1], self._mem_data_dp[i][j + 1])
                for j in range(self.mem_output_ports - 1):
                    self.wire(self._mem_cen_in[i][j + 1], self._mem_cen_datapath[i][j + 1])
                    self.wire(self._rd_mem_addr_in[i][j + 1], self._rd_mem_addr_dp[i][j + 1])
            else:
                self.wire(self._mem_wen_in[i][0], ternary(self._config_en.r_or(),
                                                          self._mem_wen_cfg[i],
                                                          self._mem_wen_datapath[i][0]))
                self.wire(self._mem_cen_in[i][0], ternary(self._config_en.r_or(),
                                                          self._mem_wen_cfg[i] | self._mem_ren_cfg[i],
                                                          self._mem_cen_datapath[i][0]))
                self.wire(self._mem_addr_in[i][0], ternary(self._config_en.r_or(),
                                                           self._mem_addr_cfg,
                                                           self._mem_addr_dp[i][0]))
                self.wire(self._mem_data_in[i][0], ternary(self._config_en.r_or(),
                                                           self._mem_data_cfg,
                                                           self._mem_data_dp[i][0]))
                # Don't route the config to any but the first port
                for j in range(self.mem_input_ports - 1):
                    self.wire(self._mem_wen_in[i][j + 1], self._mem_wen_datapath[i][j + 1])
                    self.wire(self._mem_cen_in[i][j + 1], self._mem_cen_datapath[i][j + 1])
                    self.wire(self._mem_addr_in[i][j + 1], self._mem_addr_dp[i][j + 1])
                    self.wire(self._mem_data_in[i][j + 1], self._mem_data_dp[i][j + 1])

        # Wrap sram_stub
        if self.read_delay == 1:
            if self.fifo_mode:
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

                self._mem_data_in_f = self.var("mem_data_out_f",
                                               self.data_width,
                                               size=(self.banks,
                                                     self.mem_output_ports,
                                                     self.fw_int),
                                               packed=True,
                                               explicit_array=True)

                self._mem_cen_in_f = self.var("mem_cen_in_f", self.mem_output_ports,
                                              size=self.banks,
                                              explicit_array=True,
                                              packed=True)

                self._mem_wen_in_f = self.var("mem_wen_in_f", self.mem_input_ports,
                                              size=self.banks,
                                              explicit_array=True,
                                              packed=True)

                self._mem_addr_in_f = self.var("mem_addr_in_f",
                                               self.address_width,
                                               size=(self.banks,
                                                     self.mem_input_ports),
                                               packed=True,
                                               explicit_array=True)

                # Mux all of these signals when in FIFO mode
                self.wire(self._mem_data_in_f,
                          ternary(self._mode,
                                  self._fifo_data_to_mem,
                                  self._mem_data_in))

                # self.wire(self._mem_cen_in_f,
                #           ternary(self._mode,
                #                   self._fifo_wen_to_mem | self._fifo_ren_to_mem,
                #                   self._mem_cen_in))
                for i in range(self.banks):
                    self.wire(self._mem_cen_in_f[i][0],
                              ternary(self._mode,
                                      self._fifo_wen_to_mem[i] | self._fifo_ren_to_mem[i],
                                      self._mem_cen_in[i][0]))
                    self.wire(self._mem_wen_in_f[i][0],
                              ternary(self._mode,
                                      self._fifo_wen_to_mem[i],
                                      self._mem_wen_in[i][0]))

                # self.wire(self._mem_wen_in_f,
                #           ternary(self._mode,
                #                   self._fifo_wen_to_mem,
                #                   self._mem_wen_in))

                self.wire(self._mem_addr_in_f,
                          ternary(self._mode,
                                  self._fifo_addr_to_mem,
                                  self._mem_addr_in))

            for i in range(self.banks):
                mbank = SRAMWrapper(use_sram_stub=self.use_sram_stub,
                                    sram_name=self.sram_name,
                                    data_width=self.data_width,
                                    fw_int=self.fw_int,
                                    mem_depth=self.mem_depth,
                                    mem_input_ports=self.mem_input_ports,
                                    mem_output_ports=self.mem_output_ports,
                                    address_width=self.address_width,
                                    bank_num=i)

                self.add_child(f"sram_wrapper_{i}", mbank,
                               clk=self._gclk,
                               mem_data_in_bank=self._mem_data_in[i],
                               mem_data_out_bank=self._mem_data_out[i],
                               mem_addr_in_bank=self._mem_addr_in[i],
                               mem_cen_in_bank=self._mem_cen_in[i],
                               mem_wen_in_bank=self._mem_wen_in[i])

        else:
            for i in range(self.banks):
                rfile = RegisterFile(data_width=self.data_width,
                                     write_ports=self.mem_input_ports,
                                     read_ports=self.mem_output_ports,
                                     width_mult=self.fw_int,
                                     depth=self.mem_depth)
                if self.rw_same_cycle:
                    self.add_child(f"rf_{i}", rfile,
                                   clk=self._gclk,
                                   wen=self._mem_wen_in[i],
                                   wr_addr=self._wr_mem_addr_in[i],
                                   rd_addr=self._rd_mem_addr_in[i],
                                   data_in=self._mem_data_in[i],
                                   data_out=self._mem_data_out[i])
                else:
                    self.add_child(f"rf_{i}", rfile,
                                   clk=self._gclk,
                                   wen=self._mem_wen_in[i],
                                   wr_addr=self._mem_addr_in[i],
                                   rd_addr=self._mem_addr_in[i],
                                   data_in=self._mem_data_in[i],
                                   data_out=self._mem_data_out[i])

        #########################################
        ##### END: DEMUX WRITE/SRAM WRAPPER #####
        #########################################
        self.num_tb_bits = max(1, clog2(self.num_tb))
        self.max_range_bits = max(1, clog2(self.tb_range_max))

        self._data_to_sync = self.var("data_to_sync",
                                      self.data_width,
                                      size=(self.interconnect_output_ports,
                                            self.fw_int),
                                      explicit_array=True,
                                      packed=True)
        self._valid_to_sync = self.var("valid_to_sync", self.interconnect_output_ports)

        self._data_to_tba = self.var("data_to_tba",
                                     self.data_width,
                                     size=(self.interconnect_output_ports,
                                           self.fw_int),
                                     explicit_array=True,
                                     packed=True)

        self._valid_to_tba = self.var("valid_to_tba", self.interconnect_output_ports)

        self._data_to_pref = self.var("data_to_pref",
                                      self.data_width,
                                      size=(self.interconnect_output_ports,
                                            self.fw_int),
                                      explicit_array=True,
                                      packed=True)

        self._valid_to_pref = self.var("valid_to_pref", self.interconnect_output_ports)
        #######################
        ##### DEMUX READS #####
        #######################
        dmux_rd = DemuxReads(fetch_width=self.mem_width,
                             data_width=self.data_width,
                             banks=self.banks,
                             int_out_ports=self.interconnect_output_ports,
                             strg_rd_ports=self.mem_output_ports)

        self._arb_dat_out_f = self.var("arb_dat_out_f",
                                       self.data_width,
                                       size=(self.banks * self.mem_output_ports,
                                             self.fw_int),
                                       explicit_array=True,
                                       packed=True)

        self._arb_port_out_f = self.var("arb_port_out_f",
                                        self.interconnect_output_ports,
                                        size=(self.banks * self.mem_output_ports),
                                        explicit_array=True,
                                        packed=True)
        self._arb_valid_out_f = self.var("arb_valid_out_f", self.mem_output_ports * self.banks)

        tmp_cnt = 0
        for i in range(self.banks):
            for j in range(self.mem_output_ports):
                self.wire(self._arb_dat_out_f[tmp_cnt], self._arb_dat_out[i][j])
                self.wire(self._arb_port_out_f[tmp_cnt], self._arb_port_out[i][j])
                self.wire(self._arb_valid_out_f[tmp_cnt], self._arb_valid_out[i][j])
                tmp_cnt = tmp_cnt + 1

        # If this is end of the road...
        if self.remove_tb:
            assert self.fw_int == 1, "Make it easier on me now..."
            self.add_child("demux_rds", dmux_rd,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           data_in=self._arb_dat_out_f,
                           valid_in=self._arb_valid_out_f,
                           port_in=self._arb_port_out_f,
                           valid_out=self._valid_out)
            for i in range(self.interconnect_output_ports):
                self.wire(self._data_out[i], dmux_rd.ports.data_out[i])

        else:
            self.add_child("demux_rds", dmux_rd,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           data_in=self._arb_dat_out_f,
                           valid_in=self._arb_valid_out_f,
                           port_in=self._arb_port_out_f,
                           data_out=self._data_to_sync,
                           valid_out=self._valid_to_sync)

            #######################
            ##### SYNC GROUPS #####
            #######################
            sync_group = SyncGroups(fetch_width=self.mem_width,
                                    data_width=self.data_width,
                                    int_out_ports=self.interconnect_output_ports)

            for i in range(self.interconnect_output_ports):
                self.wire(self._ren_out_reduced[i], self._ren_out_tpose[i].r_or())

            self.add_child("sync_grp", sync_group,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           data_in=self._data_to_sync,
                           valid_in=self._valid_to_sync,
                           data_out=self._data_to_pref,
                           valid_out=self._valid_to_pref,
                           ren_in=self._ren_out_reduced,
                           rd_sync_gate=self._rd_sync_gate,
                           ack_in=self._ack_reduced)

            # This is the end of the line if we aren't using tb
            ######################
            ##### PREFETCHER #####
            ######################
            prefetchers = []
            for i in range(self.interconnect_output_ports):

                pref = Prefetcher(fetch_width=self.mem_width,
                                  data_width=self.data_width,
                                  max_prefetch=self.max_prefetch)

                prefetchers.append(pref)

                if self.num_tb == 0:
                    assert self.fw_int == 1, \
                        "If no transpose buffer, data width needs match memory width"
                    self.add_child(f"pre_fetch_{i}", pref,
                                   clk=self._gclk,
                                   rst_n=self._rst_n,
                                   data_in=self._data_to_pref[i],
                                   valid_read=self._valid_to_pref[i],
                                   tba_rdy_in=self._ren[i],
                                   #    data_out=self._data_out[i],
                                   valid_out=self._valid_out[i],
                                   prefetch_step=self._prefetch_step[i])
                    self.wire(self._data_out[i], pref.ports.data_out[0])
                else:
                    self.add_child(f"pre_fetch_{i}", pref,
                                   clk=self._gclk,
                                   rst_n=self._rst_n,
                                   data_in=self._data_to_pref[i],
                                   valid_read=self._valid_to_pref[i],
                                   tba_rdy_in=self._ready_tba[i],
                                   data_out=self._data_to_tba[i],
                                   valid_out=self._valid_to_tba[i],
                                   prefetch_step=self._prefetch_step[i])

                    #############################
                    ##### TRANSPOSE BUFFERS #####
                    #############################
            if self.num_tb > 0:

                self._tb_data_out = self.var("tb_data_out", self.data_width,
                                             size=self.interconnect_output_ports,
                                             explicit_array=True,
                                             packed=True)
                self._tb_valid_out = self.var("tb_valid_out", self.interconnect_output_ports)

                for i in range(self.interconnect_output_ports):

                    tba = TransposeBufferAggregation(word_width=self.data_width,
                                                     fetch_width=self.fw_int,
                                                     num_tb=self.num_tb,
                                                     max_tb_height=self.max_tb_height,
                                                     max_range=self.tb_range_max,
                                                     max_stride=self.max_tb_stride,
                                                     tb_iterator_support=self.tb_iterator_support)

                    self.add_child(f"tba_{i}", tba,
                                   clk=self._gclk,
                                   rst_n=self._rst_n,
                                   SRAM_to_tb_data=self._data_to_tba[i],
                                   valid_data=self._valid_to_tba[i],
                                   tb_index_for_data=0,
                                   ack_in=self._valid_to_tba[i],
                                   tb_to_interconnect_data=self._tb_data_out[i],
                                   tb_to_interconnect_valid=self._tb_valid_out[i],
                                   tb_arbiter_rdy=self._ready_tba[i],
                                   tba_ren=self._output_en)

                if self.fifo_mode:
                    self.wire(self._data_out[0],
                              ternary(self._mode,
                                      self._fifo_data_out,
                                      self._tb_data_out[0]))
                    self.wire(self._valid_out[0],
                              ternary(self._mode,
                                      self._fifo_valid_out,
                                      self._tb_valid_out[0]))
                else:
                    self.wire(self._data_out[0], self._tb_data_out[0])
                    self.wire(self._valid_out[0], self._tb_valid_out[0])

                for i in range(self.interconnect_output_ports - 1):
                    self.wire(self._data_out[i + 1], self._tb_data_out[i + 1])
                    self.wire(self._valid_out[i + 1], self._tb_valid_out[i + 1])

        if self.fifo_mode:
            self._empty = self.output("empty", 1)
            self._full = self.output("full", 1)
            self.wire(self._empty, self._fifo_empty)
            self.wire(self._full, self._fifo_full)
        ####################
        ##### ADD CODE #####
        ####################
        self.add_code(self.transpose_acks)
        self.add_code(self.reduce_acks)

    @always_comb
    def transpose_acks(self):
        for i in range(self.interconnect_output_ports):
            for j in range(self.banks):
                self._ack_transpose[i][j] = self._arb_acks[j][i]

    @always_comb
    def reduce_acks(self):
        for i in range(self.interconnect_output_ports):
            self._ack_reduced[i] = self._ack_transpose[i].r_or()


if __name__ == "__main__":
    lake_dut = LakeTop()
    verilog(lake_dut, filename="lake_top.sv",
            check_multiple_driver=False,
            optimize_if=False,
            check_flip_flop_always_ff=False,
            additional_passes={"lift config regs": lift_config_reg,
                               "change sram port names": change_sram_port_names(
                                   False,
                                   ["ADDR",
                                    "CEB",
                                    "CLK",
                                    "D",
                                    "Q",
                                    "WE"],
                                   0,
                                   0)})
