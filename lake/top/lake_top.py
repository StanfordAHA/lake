from kratos import *
from lake.modules.passthru import *
from lake.modules.sram_wrapper import SRAMWrapper


from lake.modules.strg_ub import StrgUB
from lake.modules.storage_config_seq import StorageConfigSeq
from lake.modules.register_file import RegisterFile
from lake.modules.strg_fifo import StrgFIFO
from lake.modules.strg_RAM import StrgRAM
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.passes.passes import lift_config_reg, change_sram_port_names
from utils.sram_macro import SRAMMacroInfo
import kratos as kts


class LakeTop(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=1,
                 input_iterator_support=6,  # Addr Controllers
                 output_iterator_support=6,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sram_stub=1,
                 sram_macro_info=SRAMMacroInfo(),
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
                 fifo_mode=True,
                 add_clk_enable=True,
                 add_flush=True):
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
        self.sram_macro_info = sram_macro_info
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

        self._data_out = self.output("data_out",
                                     self.data_width,
                                     size=self.interconnect_output_ports,
                                     packed=True,
                                     explicit_array=True)

        self._valid_out = self.output("valid_out",
                                      self.interconnect_output_ports)

        self.address_width = clog2(mem_depth)

        # Add tile enable!
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))
        # either normal or fifo mode rn...
        self.num_modes = 3
        self._mode = self.input("mode", max(1, clog2(self.num_modes)))
        self._mode.add_attribute(ConfigRegAttr("MODE!"))

        # Currenlt mode = 0 is UB, mode = 1 is FIFO

        # self._gclk = self.var("gclk", 1)
        # self.wire(self._gclk, kts.util.clock(self._clk & self._tile_en))

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

        strg_ub = StrgUB(data_width=self.data_width,
                         mem_width=self.mem_width,
                         mem_depth=self.mem_depth,
                         banks=self.banks,
                         input_iterator_support=self.input_iterator_support,
                         output_iterator_support=self.output_iterator_support,
                         interconnect_input_ports=self.interconnect_input_ports,
                         interconnect_output_ports=self.interconnect_output_ports,
                         mem_input_ports=self.mem_input_ports,
                         mem_output_ports=self.mem_output_ports,
                         read_delay=self.read_delay,
                         rw_same_cycle=self.rw_same_cycle,
                         agg_height=self.agg_height,
                         max_agg_schedule=self.max_agg_schedule,
                         input_max_port_sched=self.input_max_port_sched,
                         output_max_port_sched=self.output_max_port_sched,
                         align_input=self.align_input,
                         max_line_length=self.max_line_length,
                         max_tb_height=self.max_tb_height,
                         tb_range_max=self.tb_range_max,
                         tb_sched_max=self.tb_sched_max,
                         max_tb_stride=self.max_tb_stride,
                         num_tb=self.num_tb,
                         tb_iterator_support=self.tb_iterator_support,
                         multiwrite=self.multiwrite,
                         max_prefetch=self.max_prefetch,
                         remove_tb=self.remove_tb)

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

        self._ub_output_en = self.input("output_en", 1)
        self._ub_wen_en = self.input("wen_en", self.interconnect_input_ports)
        self._ub_ren_en = self.input("ren_en", self.interconnect_output_ports)

        self.add_child("strg_ub", strg_ub,
                       # clk + rst
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       # inputs
                       data_in=self._data_in,
                       wen=self._wen,
                       ren=self._ren,
                       wen_en=self._ub_wen_en,
                       ren_en=self._ub_ren_en,
                       output_en=self._ub_output_en,
                       data_from_strg=self._mem_data_out,
                       # outputs
                       data_out=self._ub_data_out,
                       valid_out=self._ub_valid_out,
                       data_to_strg=self._ub_data_to_mem,
                       cen_to_strg=self._ub_cen_to_mem,
                       wen_to_strg=self._ub_wen_to_mem)

        # Wire addrs
        if self.rw_same_cycle:
            self.wire(self._ub_wr_addr_to_mem, strg_ub.ports.wr_addr_out)
            self.wire(self._ub_rd_addr_to_mem, strg_ub.ports.rd_addr_out)
        # wire single addr
        else:
            self.wire(self._ub_addr_to_mem, strg_ub.ports.addr_out)

        # Wrap sram_stub
        if self.read_delay == 1:

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

            strg_ram = StrgRAM(data_width=self.data_width,
                               banks=self.banks,
                               memory_width=self.mem_width,
                               memory_depth=self.mem_depth,
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
            self._full = self.output("full", 1)
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
                mbank = SRAMWrapper(use_sram_stub=self.use_sram_stub,
                                    sram_name=self.sram_macro_info.name,
                                    data_width=self.data_width,
                                    fw_int=self.fw_int,
                                    mem_depth=self.mem_depth,
                                    mem_input_ports=self.mem_input_ports,
                                    mem_output_ports=self.mem_output_ports,
                                    address_width=self.address_width,
                                    bank_num=i)

                self.add_child(f"mem_{i}", mbank,
                               clk=self._gclk,
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

            self.wire(self._data_out[0], self._all_data_out[self._mode])
            self.wire(self._valid_out[0], self._all_valid_out[self._mode])
        else:
            self.wire(self._data_out[0], self._ub_data_out[0])
            self.wire(self._valid_out[0], self._ub_valid_out[0])

        for i in range(self.interconnect_output_ports - 1):
            self.wire(self._data_out[i + 1], self._ub_data_out[i + 1])
            self.wire(self._valid_out[i + 1], self._ub_valid_out[i + 1])

        ########################
        ##### CLOCK ENABLE #####
        ########################
        if add_clk_enable:
            self.clock_en("clk_en")
            kts.passes.auto_insert_clock_enable(self.internal_generator)

        if add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)

        # config regs
        lift_config_reg(self.internal_generator)


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
    sram_port_pass = change_sram_port_names(use_sram_stub=True, sram_macro_info=tsmc_info)
    verilog(lake_dut, filename="lake_top.sv",
            optimize_if=False,
            additional_passes={"change sram port names": sram_port_pass})
