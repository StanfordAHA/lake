import kratos as kts
import os

from kratos import *
from numpy import full
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.modules.spec.sched_gen import SchedGen
from lake.utils.util import transform_strides_and_ranges, extract_formal_annotation, decode, safe_wire, add_counter, trim_config_list
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.modules.register_file import RegisterFile
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.modules.storage_config_seq import StorageConfigSeq
from lake.utils.parse_clkwork_config import extract_controller_json, extract_controller
from lake.utils.parse_clkwork_config import map_controller
from lake.utils.parse_clkwork_config import ControllerInfo
from lake.utils.sram_macro import SRAMMacroInfo
from _kratos import create_wrapper_flatten
import csv


class Pond(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_depth=32,
                 default_iterator_support=3,
                 interconnect_input_ports=2,  # Connection to int
                 interconnect_output_ports=2,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 config_data_width=32,
                 config_addr_width=8,
                 cycle_count_width=16,
                 add_clk_enable=True,
                 add_flush=True,
                 name="pond",
                 comply_17=False):
        super().__init__(name=name, debug=True)

        self.interconnect_input_ports = interconnect_input_ports
        self.interconnect_output_ports = interconnect_output_ports
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.mem_depth = mem_depth
        self.data_width = data_width
        self.config_data_width = config_data_width
        self.config_addr_width = config_addr_width
        self.add_clk_enable = add_clk_enable
        self.add_flush = add_flush
        self.cycle_count_width = cycle_count_width
        self.default_iterator_support = default_iterator_support
        self.default_config_width = kts.clog2(self.mem_depth)
        self.comply_17 = comply_17

        # inputs
        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        self._cycle_count = add_counter(self, "cycle_count", self.cycle_count_width)

        # Create write enable + addr, same for read.
        # self._write = self.input("write", self.interconnect_input_ports)
        self._write = self.var("write", self.mem_input_ports)
        # self._write.add_attribute(ControlSignalAttr(is_control=True))

        self._write_addr = self.var("write_addr",
                                    kts.clog2(self.mem_depth),
                                    size=self.interconnect_input_ports,
                                    explicit_array=True,
                                    packed=True)

        # Add "_pond" suffix to avoid error during garnet RTL generation
        # self._data_in = self.input("data_in_pond", self.data_width,
        self._data_in = self.input("data_in_pond", self.data_width + 1,
                                   size=self.interconnect_input_ports,
                                   explicit_array=True,
                                   packed=True)
        self._data_in.add_attribute(FormalAttr(f"{self._data_in.name}", FormalSignalConstraint.SEQUENCE))
        self._data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._read = self.var("read", self.mem_output_ports)
        self._t_write = self.var("t_write", self.interconnect_input_ports)
        self._t_read = self.var("t_read", self.interconnect_output_ports)
        # self._read.add_attribute(ControlSignalAttr(is_control=True))

        self._read_addr = self.var("read_addr",
                                   kts.clog2(self.mem_depth),
                                   size=self.interconnect_output_ports,
                                   explicit_array=True,
                                   packed=True)

        self._s_read_addr = self.var("s_read_addr",
                                     kts.clog2(self.mem_depth),
                                     size=self.interconnect_output_ports,
                                     explicit_array=True,
                                     packed=True)

        # self._data_out = self.output("data_out_pond", self.data_width,
        self._data_out = self.output("data_out_pond", self.data_width + 1,
                                     size=self.interconnect_output_ports,
                                     explicit_array=True,
                                     packed=True)
        self._data_out.add_attribute(FormalAttr(f"{self._data_out.name}", FormalSignalConstraint.SEQUENCE))
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_out = self.output("valid_out_pond", self.interconnect_output_ports)
        self._valid_out.add_attribute(FormalAttr(f"{self._valid_out.name}", FormalSignalConstraint.SEQUENCE))
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._mem_data_out = self.var("mem_data_out", self.data_width,
                                      size=self.mem_output_ports,
                                      explicit_array=True,
                                      packed=True)

        self._s_mem_data_in = self.var("s_mem_data_in", self.data_width,
                                       size=self.interconnect_input_ports,
                                       explicit_array=True,
                                       packed=True)

        self._mem_data_in = self.var("mem_data_in", self.data_width,
                                     size=self.mem_input_ports,
                                     explicit_array=True,
                                     packed=True)

        self._s_mem_write_addr = self.var("s_mem_write_addr",
                                          kts.clog2(self.mem_depth),
                                          size=self.interconnect_input_ports,
                                          explicit_array=True,
                                          packed=True)

        self._s_mem_read_addr = self.var("s_mem_read_addr",
                                         kts.clog2(self.mem_depth),
                                         size=self.interconnect_output_ports,
                                         explicit_array=True,
                                         packed=True)

        self._mem_write_addr = self.var("mem_write_addr",
                                        kts.clog2(self.mem_depth),
                                        size=self.mem_input_ports,
                                        explicit_array=True,
                                        packed=True)

        self._mem_read_addr = self.var("mem_read_addr",
                                       kts.clog2(self.mem_depth),
                                       size=self.mem_output_ports,
                                       explicit_array=True,
                                       packed=True)

        if self.interconnect_output_ports == 1:
            self.wire(self._data_out[0][15, 0], self._mem_data_out[0])
            self.wire(self._data_out[0][16], kts.const(0, 1))
        else:
            for i in range(self.interconnect_output_ports):
                self.wire(self._data_out[i][15, 0], self._mem_data_out[0])
                self.wire(self._data_out[i][16], kts.const(0, 1))

        # Valid out is simply passing the read signal through...
        self.wire(self._valid_out, self._t_read)

        # Create write addressors
        for wr_port in range(self.interconnect_input_ports):

            RF_WRITE_ITER = ForLoop(iterator_support=self.default_iterator_support,
                                    config_width=self.cycle_count_width)
            RF_WRITE_ADDR = AddrGen(iterator_support=self.default_iterator_support,
                                    config_width=self.default_config_width)
            RF_WRITE_SCHED = SchedGen(iterator_support=self.default_iterator_support,
                                      config_width=self.cycle_count_width,
                                      use_enable=True)

            self.add_child(f"rf_write_iter_{wr_port}",
                           RF_WRITE_ITER,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           step=self._t_write[wr_port])
            # Whatever comes through here should hopefully just pipe through seamlessly
            # addressor modules
            self.add_child(f"rf_write_addr_{wr_port}",
                           RF_WRITE_ADDR,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           step=self._t_write[wr_port],
                           mux_sel=RF_WRITE_ITER.ports.mux_sel_out,
                           restart=RF_WRITE_ITER.ports.restart)
            safe_wire(self, self._write_addr[wr_port], RF_WRITE_ADDR.ports.addr_out)

            self.add_child(f"rf_write_sched_{wr_port}",
                           RF_WRITE_SCHED,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           mux_sel=RF_WRITE_ITER.ports.mux_sel_out,
                           finished=RF_WRITE_ITER.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self._t_write[wr_port])

        # Create read addressors
        for rd_port in range(self.interconnect_output_ports):

            RF_READ_ITER = ForLoop(iterator_support=self.default_iterator_support,
                                   config_width=self.cycle_count_width)
            RF_READ_ADDR = AddrGen(iterator_support=self.default_iterator_support,
                                   config_width=self.default_config_width)
            RF_READ_SCHED = SchedGen(iterator_support=self.default_iterator_support,
                                     config_width=self.cycle_count_width,
                                     use_enable=True)

            self.add_child(f"rf_read_iter_{rd_port}",
                           RF_READ_ITER,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           step=self._t_read[rd_port])

            self.add_child(f"rf_read_addr_{rd_port}",
                           RF_READ_ADDR,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           step=self._t_read[rd_port],
                           mux_sel=RF_READ_ITER.ports.mux_sel_out,
                           restart=RF_READ_ITER.ports.restart)
            if self.interconnect_output_ports > 1:
                safe_wire(self, self._read_addr[rd_port], RF_READ_ADDR.ports.addr_out)
            else:
                safe_wire(self, self._read_addr[rd_port], RF_READ_ADDR.ports.addr_out)

            self.add_child(f"rf_read_sched_{rd_port}",
                           RF_READ_SCHED,
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           mux_sel=RF_READ_ITER.ports.mux_sel_out,
                           finished=RF_READ_ITER.ports.restart,
                           cycle_count=self._cycle_count,
                           valid_output=self._t_read[rd_port])

        self.wire(self._write, self._t_write.r_or())
        self.wire(self._mem_write_addr[0], decode(self, self._t_write, self._s_mem_write_addr))

        self.wire(self._mem_data_in[0], decode(self, self._t_write, self._s_mem_data_in))

        self.wire(self._read, self._t_read.r_or())
        self.wire(self._mem_read_addr[0], decode(self, self._t_read, self._s_mem_read_addr))
        # ===================================
        # Instantiate config hooks...
        # ===================================
        self.fw_int = 1
        self.data_words_per_set = 2 ** self.config_addr_width
        self.sets = int((self.fw_int * self.mem_depth) / self.data_words_per_set)

        self.sets_per_macro = max(1, int(self.mem_depth / self.data_words_per_set))
        self.total_sets = max(1, 1 * self.sets_per_macro)

        self._config_data_in = self.input("config_data_in",
                                          self.config_data_width)
        self._config_data_in.add_attribute(ControlSignalAttr(is_control=False))

        self._config_data_in_shrt = self.var("config_data_in_shrt",
                                             self.data_width)

        self.wire(self._config_data_in_shrt, self._config_data_in[self.data_width - 1, 0])

        self._config_addr_in = self.input("config_addr_in",
                                          self.config_addr_width)
        self._config_addr_in.add_attribute(ControlSignalAttr(is_control=False))

        self._config_data_out_shrt = self.var("config_data_out_shrt", self.data_width,
                                              size=self.total_sets,
                                              explicit_array=True,
                                              packed=True)

        self._config_data_out = self.output("config_data_out", self.config_data_width,
                                            size=self.total_sets,
                                            explicit_array=True,
                                            packed=True)
        self._config_data_out.add_attribute(ControlSignalAttr(is_control=False))

        for i in range(self.total_sets):
            self.wire(self._config_data_out[i],
                      self._config_data_out_shrt[i].extend(self.config_data_width))

        self._config_read = self.input("config_read", 1)
        self._config_read.add_attribute(ControlSignalAttr(is_control=False))

        self._config_write = self.input("config_write", 1)
        self._config_write.add_attribute(ControlSignalAttr(is_control=False))

        self._config_en = self.input("config_en", self.total_sets)
        self._config_en.add_attribute(ControlSignalAttr(is_control=False))

        self._mem_data_cfg = self.var("mem_data_cfg", self.data_width,
                                      explicit_array=True,
                                      packed=True)

        self._mem_addr_cfg = self.var("mem_addr_cfg", kts.clog2(self.mem_depth))

        # Add config...
        stg_cfg_seq = StorageConfigSeq(data_width=self.data_width,
                                       config_addr_width=self.config_addr_width,
                                       addr_width=kts.clog2(self.mem_depth),
                                       fetch_width=self.data_width,
                                       total_sets=self.total_sets,
                                       sets_per_macro=self.sets_per_macro)

        # The clock to config sequencer needs to be the normal clock or
        # if the tile is off, we bring the clock back in based on config_en
        cfg_seq_clk = self.var("cfg_seq_clk", 1)
        self._cfg_seq_clk = kts.util.clock(cfg_seq_clk)
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
                       wr_data=self._mem_data_cfg,
                       rd_data_out=self._config_data_out_shrt,
                       addr_out=self._mem_addr_cfg)

        if self.interconnect_output_ports == 1:
            self.wire(stg_cfg_seq.ports.rd_data_stg, self._mem_data_out)
        else:
            self.wire(stg_cfg_seq.ports.rd_data_stg[0], self._mem_data_out[0])

        self.RF_GEN = RegisterFile(data_width=self.data_width,
                                   write_ports=self.mem_input_ports,
                                   read_ports=self.mem_output_ports,
                                   width_mult=1,
                                   depth=self.mem_depth,
                                   read_delay=0)

        # Now we can instantiate and wire up the register file
        self.add_child(f"rf",
                       self.RF_GEN,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       data_out=self._mem_data_out)

        # Opt in for config_write
        self._write_rf = self.var("write_rf", self.mem_input_ports)
        self.wire(self._write_rf[0], kts.ternary(self._config_en.r_or(), self._config_write, self._write[0]))
        for i in range(self.mem_input_ports - 1):
            self.wire(self._write_rf[i + 1], kts.ternary(self._config_en.r_or(), kts.const(0, 1), self._write[i + 1]))
        self.wire(self.RF_GEN.ports.wen, self._write_rf)

        # Opt in for config_data_in
        for i in range(self.interconnect_input_ports):
            # self.wire(self._s_mem_data_in[i], kts.ternary(self._config_en.r_or(), self._mem_data_cfg, self._data_in[i]))
            self.wire(self._s_mem_data_in[i], kts.ternary(self._config_en.r_or(), self._mem_data_cfg, self._data_in[i][15, 0]))
        self.wire(self.RF_GEN.ports.data_in, self._mem_data_in)

        # Opt in for config_addr
        for i in range(self.interconnect_input_ports):
            self.wire(self._s_mem_write_addr[i], kts.ternary(self._config_en.r_or(),
                                                             self._mem_addr_cfg,
                                                             self._write_addr[i]))

        self.wire(self.RF_GEN.ports.wr_addr, self._mem_write_addr[0])

        for i in range(self.interconnect_output_ports):
            self.wire(self._s_mem_read_addr[i], kts.ternary(self._config_en.r_or(),
                                                            self._mem_addr_cfg,
                                                            self._read_addr[i]))

        self.wire(self.RF_GEN.ports.rd_addr, self._mem_read_addr[0])

        if self.add_clk_enable:
            # self.clock_en("clk_en")
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))

        if self.add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

        # Finally, lift the config regs...
        lift_config_reg(self.internal_generator)

    def get_pond_configs(config_file):

        # Ranges, Strides, Dimensionality, Starting Addr
        # Starting Addr (schedule), Ranges (schedule)
        # ctrl_rd = [[16, 1], [1, 1], 2, 0, 16, [1, 1]]
        # ctrl_wr = [[16, 1], [1, 1], 2, 0, 0, [1, 1]]

        configs = {}
        with open(config_file) as csvfile:
            config_data = csv.reader(csvfile, delimiter=',')
            for row in config_data:
                configs[row[0]] = int(row[1])
        ctrl_rd = [[configs["rf_read_iter_0_ranges_0"], configs["rf_read_iter_0_ranges_1"], configs["rf_read_iter_0_ranges_2"]],
                   [configs["rf_read_addr_0_strides_0"], configs["rf_read_addr_0_strides_1"], configs["rf_read_addr_0_strides_2"]],
                   configs["rf_read_iter_0_dimensionality"],
                   configs["rf_read_addr_0_starting_addr"],
                   configs["rf_read_sched_0_sched_addr_gen_starting_addr"],
                   [configs["rf_read_sched_0_sched_addr_gen_strides_0"], configs["rf_read_sched_0_sched_addr_gen_strides_1"], configs["rf_read_sched_0_sched_addr_gen_strides_2"]]]

        ctrl_wr = [[configs["rf_write_iter_0_ranges_0"], configs["rf_write_iter_0_ranges_1"], configs["rf_write_iter_0_ranges_2"]],
                   [configs["rf_write_addr_0_strides_0"], configs["rf_write_addr_0_strides_1"], configs["rf_write_addr_0_strides_2"]],
                   configs["rf_write_iter_0_dimensionality"],
                   configs["rf_write_addr_0_starting_addr"],
                   configs["rf_write_sched_0_sched_addr_gen_starting_addr"],
                   [configs["rf_write_sched_0_sched_addr_gen_strides_0"], configs["rf_write_sched_0_sched_addr_gen_strides_1"], configs["rf_write_sched_0_sched_addr_gen_strides_2"]]]

        return ctrl_rd, ctrl_wr

    # Function for generating Pond API
    def generate_pond_api(self, ctrl_rd, ctrl_wr, num_acc=1):
        tform_ranges_rd = [0] * num_acc
        tform_strides_rd = [0] * num_acc
        tform_ranges_rd_sched = [0] * num_acc
        tform_strides_rd_sched = [0] * num_acc
        tform_ranges_wr = [0] * num_acc
        tform_strides_wr = [0] * num_acc
        tform_ranges_wr_sched = [0] * num_acc
        tform_strides_wr_sched = [0] * num_acc
        dim_rd = [0] * num_acc
        dim_wr = [0] * num_acc

        for i in range(num_acc):
            (tform_ranges_rd[i], tform_strides_rd[i]) = transform_strides_and_ranges(ctrl_rd[i][0], ctrl_rd[i][1], ctrl_rd[i][2])
            (tform_ranges_wr[i], tform_strides_wr[i]) = transform_strides_and_ranges(ctrl_wr[i][0], ctrl_wr[i][1], ctrl_wr[i][2])

            (tform_ranges_rd_sched[i], tform_strides_rd_sched[i]) = transform_strides_and_ranges(ctrl_rd[i][0], ctrl_rd[i][5], ctrl_rd[i][2])
            (tform_ranges_wr_sched[i], tform_strides_wr_sched[i]) = transform_strides_and_ranges(ctrl_wr[i][0], ctrl_wr[i][5], ctrl_wr[i][2])

            dim_rd[i] = ctrl_rd[i][2]
            dim_wr[i] = ctrl_wr[i][2]

        new_config = {}

        for i in range(num_acc):
            new_config[f"rf_read_iter_{i}_dimensionality"] = ctrl_rd[i][2]
            new_config[f"rf_read_addr_{i}_starting_addr"] = ctrl_rd[i][3]
            new_config[f"rf_read_sched_{i}_sched_addr_gen_starting_addr"] = ctrl_rd[i][4]
            new_config[f"rf_read_sched_{i}_enable"] = 1

            for j in range(dim_rd[i]):
                new_config[f"rf_read_addr_{i}_strides_{j}"] = tform_strides_rd[i][j]
                new_config[f"rf_read_iter_{i}_ranges_{j}"] = tform_ranges_rd[i][j]
                new_config[f"rf_read_sched_{i}_sched_addr_gen_strides_{j}"] = tform_strides_rd_sched[i][j]

            new_config[f"rf_write_iter_{i}_dimensionality"] = ctrl_wr[i][2]
            new_config[f"rf_write_addr_{i}_starting_addr"] = ctrl_wr[i][3]
            new_config[f"rf_write_sched_{i}_sched_addr_gen_starting_addr"] = ctrl_wr[i][4]
            new_config[f"rf_write_sched_{i}_enable"] = 1

            for j in range(dim_wr[i]):
                new_config[f"rf_write_addr_{i}_strides_{j}"] = tform_strides_wr[i][j]
                new_config[f"rf_write_iter_{i}_ranges_{j}"] = tform_ranges_wr[i][j]
                new_config[f"rf_write_sched_{i}_sched_addr_gen_strides_{j}"] = tform_strides_wr_sched[i][j]
        return new_config

    def get_static_bitstream_json(self,
                                  root_node):

        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Store all configurations here
        config = []

        # Get controllers from json node...
        assert "in2regfile_0" in root_node
        assert "regfile2out_0" in root_node
        in2rf_ctrl = map_controller(extract_controller_json(root_node["in2regfile_0"]), "in2regfile")
        rf2out_ctrl = map_controller(extract_controller_json(root_node["regfile2out_0"]), "regfile2out")

        # Configure registers based on controller data...
        config.append(("rf_write_iter_0_dimensionality", in2rf_ctrl.dim))
        config.append(("rf_write_addr_0_starting_addr", in2rf_ctrl.in_data_strt))
        config.append(("rf_write_sched_0_sched_addr_gen_starting_addr", in2rf_ctrl.cyc_strt))
        config.append(("rf_write_sched_0_enable", 1))
        for i in range(in2rf_ctrl.dim):
            config.append((f"rf_write_addr_0_strides_{i}", in2rf_ctrl.in_data_stride[i]))
            config.append((f"rf_write_iter_0_ranges_{i}", in2rf_ctrl.extent[i]))
            config.append((f"rf_write_sched_0_sched_addr_gen_strides_{i}", in2rf_ctrl.cyc_stride[i]))

        config.append(("rf_read_iter_0_dimensionality", rf2out_ctrl.dim))
        config.append(("rf_read_addr_0_starting_addr", rf2out_ctrl.out_data_strt))
        config.append(("rf_read_sched_0_sched_addr_gen_starting_addr", rf2out_ctrl.cyc_strt))
        config.append(("rf_read_sched_0_enable", 1))

        for i in range(rf2out_ctrl.dim):
            config.append((f"rf_read_addr_0_strides_{i}", rf2out_ctrl.out_data_stride[i]))
            config.append((f"rf_read_iter_0_ranges_{i}", rf2out_ctrl.extent[i]))
            config.append((f"rf_read_sched_0_sched_addr_gen_strides_{i}", rf2out_ctrl.cyc_stride[i]))

        # Handle control registers... (should really be done in garnet TODO)
        config.append(("flush_reg_sel", 0))  # 1
        config.append(("flush_reg_value", 0))  # 1
        # Activate the tile...
        config.append(("tile_en", 1))  # 1

        # Trim the list
        return trim_config_list(flattened, config)

    def get_static_bitstream(self,
                             config_path):

        controllers = ["in2regfile_0", "in2regfile_1", "regfile2out_0", "regfile2out_1"]
        controller_objs = [None] * len(controllers)

        for i in range(len(controllers)):
            c = controllers[i]
            path = config_path + '/' + c + '.csv'

            if os.path.isfile(path):
                print(path)
                controller_objs[i] = map_controller(extract_controller(path), c)
            else:
                # print(f"No {c} file provided. Is this expected?")
                print(f"No {c} file provided. ?")
                controller_objs[i] = None

        in2rf_ctrl_1, in2rf_ctrl_2, rf2out_ctrl_1, rf2out_ctrl_2 = controller_objs

        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Store all configurations here
        config = []
        # Configure registers based on controller data...
        if in2rf_ctrl_1:
            config.append(("rf_write_iter_0_dimensionality", in2rf_ctrl_1.dim))
            config.append(("rf_write_addr_0_starting_addr", in2rf_ctrl_1.in_data_strt))
            config.append(("rf_write_sched_0_sched_addr_gen_starting_addr", in2rf_ctrl_1.cyc_strt))
            config.append(("rf_write_sched_0_enable", 1))
            for i in range(in2rf_ctrl_1.dim):
                config.append((f"rf_write_addr_0_strides_{i}", in2rf_ctrl_1.in_data_stride[i]))
                config.append((f"rf_write_iter_0_ranges_{i}", in2rf_ctrl_1.extent[i]))
                config.append((f"rf_write_sched_0_sched_addr_gen_strides_{i}", in2rf_ctrl_1.cyc_stride[i]))

        if rf2out_ctrl_1:
            config.append(("rf_read_iter_0_dimensionality", rf2out_ctrl_1.dim))
            config.append(("rf_read_addr_0_starting_addr", rf2out_ctrl_1.out_data_strt))
            config.append(("rf_read_sched_0_sched_addr_gen_starting_addr", rf2out_ctrl_1.cyc_strt))
            config.append(("rf_read_sched_0_enable", 1))

            for i in range(rf2out_ctrl_1.dim):
                config.append((f"rf_read_addr_0_strides_{i}", rf2out_ctrl_1.out_data_stride[i]))
                config.append((f"rf_read_iter_0_ranges_{i}", rf2out_ctrl_1.extent[i]))
                config.append((f"rf_read_sched_0_sched_addr_gen_strides_{i}", rf2out_ctrl_1.cyc_stride[i]))

        if in2rf_ctrl_2:
            config.append(("rf_write_iter_1_dimensionality", in2rf_ctrl_2.dim))
            config.append(("rf_write_addr_1_starting_addr", in2rf_ctrl_2.in_data_strt))
            config.append(("rf_write_sched_1_sched_addr_gen_starting_addr", in2rf_ctrl_2.cyc_strt))
            config.append(("rf_write_sched_1_enable", 1))
            for i in range(in2rf_ctrl_2.dim):
                config.append((f"rf_write_addr_1_strides_{i}", in2rf_ctrl_2.in_data_stride[i]))
                config.append((f"rf_write_iter_1_ranges_{i}", in2rf_ctrl_2.extent[i]))
                config.append((f"rf_write_sched_1_sched_addr_gen_strides_{i}", in2rf_ctrl_2.cyc_stride[i]))

        if rf2out_ctrl_2:
            config.append(("rf_read_iter_1_dimensionality", rf2out_ctrl_2.dim))
            config.append(("rf_read_addr_1_starting_addr", rf2out_ctrl_2.out_data_strt))
            config.append(("rf_read_sched_1_sched_addr_gen_starting_addr", rf2out_ctrl_2.cyc_strt))
            config.append(("rf_read_sched_1_enable", 1))

            for i in range(rf2out_ctrl_2.dim):
                config.append((f"rf_read_addr_1_strides_{i}", rf2out_ctrl_2.out_data_stride[i]))
                config.append((f"rf_read_iter_1_ranges_{i}", rf2out_ctrl_2.extent[i]))
                config.append((f"rf_read_sched_1_sched_addr_gen_strides_{i}", rf2out_ctrl_2.cyc_stride[i]))

        # Handle control registers... (should really be done in garnet TODO)
        config.append(("flush_reg_sel", 0))  # 1
        config.append(("flush_reg_value", 0))  # 1
        # Activate the tile...
        config.append(("tile_en", 1))  # 1

        # Trim the list
        return trim_config_list(flattened, config)


def get_pond_dut(depth=32,
                 iterator_support=3,
                 in_ports=2,
                 out_ports=2,
                 mem_in_ports=1,
                 mem_out_ports=1,
                 tsmc_info=SRAMMacroInfo("tsmc_name"),
                 use_sram_stub=True,
                 do_config_lift=True,
                 **pond_kwargs):

    pond_dut = Pond(data_width=16,  # CGRA Params
                    mem_depth=depth,
                    default_iterator_support=iterator_support,
                    interconnect_input_ports=in_ports,  # Connection to int
                    interconnect_output_ports=out_ports,
                    mem_input_ports=mem_in_ports,
                    mem_output_ports=mem_out_ports,
                    config_data_width=32,
                    config_addr_width=8,
                    cycle_count_width=16,
                    add_clk_enable=True,
                    add_flush=True,
                    **pond_kwargs)

    # print(f"Supports Stencil Valid: {lake_dut.supports('stencil_valid')}")

    # if do_config_lift, then do not need_config_lift later
    return pond_dut, not do_config_lift, use_sram_stub, tsmc_info


if __name__ == "__main__":
    pond_dut = Pond(data_width=16,  # CGRA Params
                    mem_depth=32,
                    default_iterator_support=2,
                    interconnect_input_ports=2,  # Connection to int
                    interconnect_output_ports=2,
                    mem_input_ports=1,
                    mem_output_ports=1,
                    cycle_count_width=16,
                    add_clk_enable=True,
                    add_flush=True)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    extract_formal_annotation(pond_dut, "pond.txt")

    verilog(pond_dut, filename="pond.sv",
            optimize_if=False)
