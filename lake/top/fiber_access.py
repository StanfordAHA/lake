import kratos as kts
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.top.memory_controller import MemoryController
from lake.modules.buffet_like import BuffetLike
from lake.modules.scanner import Scanner
from lake.modules.scanner_pipe import ScannerPipe
from lake.modules.write_scanner import WriteScanner
from lake.passes.passes import lift_config_reg
from lake.top.tech_maps import GF_Tech_Map


class FiberAccess(MemoryController):

    def __init__(self, data_width=16,
                 local_memory=True,
                 tech_map=GF_Tech_Map(depth=512, width=32),
                 defer_fifos=True,
                 use_pipelined_scanner=False,
                 add_flush=False,
                 fifo_depth=2,
                 buffet_optimize_wide=False,
                 perf_debug=False,
                 split_memory_ports=True):
        super().__init__(f'fiber_access_{data_width}', debug=True)

        self.wr_scan_pre = "write_scanner"
        self.rd_scan_pre = "read_scanner"
        self.buffet_pre = "buffet"

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = add_flush
        self.local_memory = local_memory
        self.tech_map = tech_map
        self.defer_fifos = defer_fifos
        self.use_pipelined_scanner = use_pipelined_scanner
        self.fifo_depth = fifo_depth
        self.buffet_optimize_wide = buffet_optimize_wide
        self.split_memory_ports = split_memory_ports

        if self.split_memory_ports:
            num_ports = 2
        else:
            num_ports = 1

        # inputs
        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        self._wr_scan_data_in = self.input(f"{self.wr_scan_pre}_data_in", self.data_width + 1, packed=True)
        self._wr_scan_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._wr_scan_data_in_ready = self.output(f"{self.wr_scan_pre}_data_in_ready", 1)
        self._wr_scan_data_in_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        self._wr_scan_data_in_valid = self.input(f"{self.wr_scan_pre}_data_in_valid", 1)
        self._wr_scan_data_in_valid.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self._wr_scan_addr_in = self.input(f"{self.wr_scan_pre}_addr_in", self.data_width + 1, packed=True)
        self._wr_scan_addr_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._wr_scan_addr_in_ready = self.output(f"{self.wr_scan_pre}_addr_in_ready", 1)
        self._wr_scan_addr_in_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        self._wr_scan_addr_in_valid = self.input(f"{self.wr_scan_pre}_addr_in_valid", 1)
        self._wr_scan_addr_in_valid.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self._wr_scan_block_wr_in = self.input(f"{self.wr_scan_pre}_block_wr_in", self.data_width + 1, packed=True)
        self._wr_scan_block_wr_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._wr_scan_block_wr_in_ready = self.output(f"{self.wr_scan_pre}_block_wr_in_ready", 1)
        self._wr_scan_block_wr_in_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        self._wr_scan_block_wr_in_valid = self.input(f"{self.wr_scan_pre}_block_wr_in_valid", 1)
        self._wr_scan_block_wr_in_valid.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self._rd_scan_coord_out = self.output(f"{self.rd_scan_pre}_coord_out", self.data_width + 1, packed=True)
        self._rd_scan_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._rd_scan_coord_out_ready = self.input(f"{self.rd_scan_pre}_coord_out_ready", 1)
        self._rd_scan_coord_out_ready.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
        self._rd_scan_coord_out_valid = self.output(f"{self.rd_scan_pre}_coord_out_valid", 1)
        self._rd_scan_coord_out_valid.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._rd_scan_pos_out = self.output(f"{self.rd_scan_pre}_pos_out", self.data_width + 1, packed=True)
        self._rd_scan_pos_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._rd_scan_pos_out_ready = self.input(f"{self.rd_scan_pre}_pos_out_ready", 1)
        self._rd_scan_pos_out_ready.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
        self._rd_scan_pos_out_valid = self.output(f"{self.rd_scan_pre}_pos_out_valid", 1)
        self._rd_scan_pos_out_valid.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._rd_scan_us_pos_in = self.input(f"{self.rd_scan_pre}_us_pos_in", self.data_width + 1, packed=True)
        self._rd_scan_us_pos_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._rd_scan_us_pos_in_ready = self.output(f"{self.rd_scan_pre}_us_pos_in_ready", 1)
        self._rd_scan_us_pos_in_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        self._rd_scan_us_pos_in_valid = self.input(f"{self.rd_scan_pre}_us_pos_in_valid", 1)
        self._rd_scan_us_pos_in_valid.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self.buffet = BuffetLike(data_width=self.data_width, num_ID=2, mem_depth=512,
                                 local_memory=self.local_memory,
                                 tech_map=self.tech_map,
                                 defer_fifos=self.defer_fifos,
                                 fifo_depth=self.fifo_depth,
                                 optimize_wide=self.buffet_optimize_wide,
                                 split_mem_requests=self.split_memory_ports)

        self.wr_scan = WriteScanner(data_width=self.data_width,
                                    defer_fifos=self.defer_fifos,
                                    fifo_depth=self.fifo_depth,
                                    perf_debug=perf_debug)

        if self.use_pipelined_scanner:
            self.rd_scan = ScannerPipe(data_width=self.data_width,
                                       defer_fifos=self.defer_fifos,
                                       fifo_depth=self.fifo_depth,
                                       perf_debug=perf_debug,
                                       split_mem_requests=self.split_memory_ports)
        else:
            self.rd_scan = Scanner(data_width=self.data_width,
                                   defer_fifos=self.defer_fifos,
                                   fifo_depth=self.fifo_depth)

        self.add_child(self.buffet_pre,
                       self.buffet,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en)

        self.add_child(self.wr_scan_pre,
                       self.wr_scan,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en)

        self.add_child(self.rd_scan_pre,
                       self.rd_scan,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en)

        if self.use_pipelined_scanner:
            self._rd_scan_block_rd_out = self.output(f"{self.rd_scan_pre}_block_rd_out", self.data_width + 1, packed=True)
            self._rd_scan_block_rd_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            self._rd_scan_block_rd_out_ready = self.input(f"{self.rd_scan_pre}_block_rd_out_ready", 1)
            self._rd_scan_block_rd_out_ready.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
            self._rd_scan_block_rd_out_valid = self.output(f"{self.rd_scan_pre}_block_rd_out_valid", 1)
            self._rd_scan_block_rd_out_valid.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

            self.wire(self._rd_scan_block_rd_out, self.rd_scan.ports.block_rd_out)
            self.wire(self._rd_scan_block_rd_out_ready, self.rd_scan.ports.block_rd_out_ready)
            self.wire(self._rd_scan_block_rd_out_valid, self.rd_scan.ports.block_rd_out_valid)

        # Now wire everything
        # buffet to wr_scan
        self.wire(self.buffet.ports.wr_ID, self.wr_scan.ports.ID_out)
        self.wire(self.buffet.ports.wr_ID_ready, self.wr_scan.ports.ID_out_ready)
        self.wire(self.buffet.ports.wr_ID_valid, self.wr_scan.ports.ID_out_valid)

        self.wire(self.buffet.ports.wr_addr, self.wr_scan.ports.addr_out)
        self.wire(self.buffet.ports.wr_addr_ready, self.wr_scan.ports.addr_out_ready)
        self.wire(self.buffet.ports.wr_addr_valid, self.wr_scan.ports.addr_out_valid)

        self.wire(self.buffet.ports.wr_data, self.wr_scan.ports.data_out)
        self.wire(self.buffet.ports.wr_data_ready, self.wr_scan.ports.data_out_ready)
        self.wire(self.buffet.ports.wr_data_valid, self.wr_scan.ports.data_out_valid)

        [self.wire(self.buffet.ports[f"rd_op_{i}"], self.rd_scan.ports[f"op_out_{i}"]) for i in range(num_ports)]
        [self.wire(self.buffet.ports[f"rd_op_{i}_ready"], self.rd_scan.ports[f"op_out_{i}_ready"]) for i in range(num_ports)]
        [self.wire(self.buffet.ports[f"rd_op_{i}_valid"], self.rd_scan.ports[f"op_out_{i}_valid"]) for i in range(num_ports)]

        [self.wire(self.buffet.ports[f"rd_addr_{i}"], self.rd_scan.ports[f"addr_out_{i}"]) for i in range(num_ports)]
        [self.wire(self.buffet.ports[f"rd_addr_{i}_ready"], self.rd_scan.ports[f"addr_out_{i}_ready"]) for i in range(num_ports)]
        [self.wire(self.buffet.ports[f"rd_addr_{i}_valid"], self.rd_scan.ports[f"addr_out_{i}_valid"]) for i in range(num_ports)]
        # [self.wire(self.buffet.ports[f"rd_op_{i}"], self.rd_scan.ports.op_out) for i in range(num_ports)]
        # [self.wire(self.buffet.ports.rd_op_ready, self.rd_scan.ports.op_out_ready) for i in range(num_ports)]
        # [self.wire(self.buffet.ports.rd_op_valid, self.rd_scan.ports.op_out_valid) for i in range(num_ports)]

        # [self.wire(self.buffet.ports.rd_addr, self.rd_scan.ports.addr_out) for i in range(num_ports)]
        # [self.wire(self.buffet.ports.rd_addr_ready, self.rd_scan.ports.addr_out_ready) for i in range(num_ports)]
        # [self.wire(self.buffet.ports.rd_addr_valid, self.rd_scan.ports.addr_out_valid) for i in range(num_ports)]

        if num_ports == 1:
            self.wire(self.buffet.ports.rd_ID_0, self.rd_scan.ports.ID_out_0)
            self.wire(self.buffet.ports.rd_ID_0_ready, self.rd_scan.ports.ID_out_0_ready)
            self.wire(self.buffet.ports.rd_ID_0_valid, self.rd_scan.ports.ID_out_0_valid)

        [self.wire(self.buffet.ports[f"rd_rsp_data_{i}"], self.rd_scan.ports[f"rd_rsp_data_in_{i}"]) for i in range(num_ports)]
        [self.wire(self.buffet.ports[f"rd_rsp_data_{i}_ready"], self.rd_scan.ports[f"rd_rsp_data_in_{i}_ready"]) for i in range(num_ports)]
        [self.wire(self.buffet.ports[f"rd_rsp_data_{i}_valid"], self.rd_scan.ports[f"rd_rsp_data_in_{i}_valid"]) for i in range(num_ports)]

        self.wire(self._wr_scan_data_in, self.wr_scan.ports.data_in)
        self.wire(self._wr_scan_data_in_ready, self.wr_scan.ports.data_in_ready)
        self.wire(self._wr_scan_data_in_valid, self.wr_scan.ports.data_in_valid)

        self.wire(self._wr_scan_addr_in, self.wr_scan.ports.addr_in)
        self.wire(self._wr_scan_addr_in_ready, self.wr_scan.ports.addr_in_ready)
        self.wire(self._wr_scan_addr_in_valid, self.wr_scan.ports.addr_in_valid)

        self.wire(self._wr_scan_block_wr_in, self.wr_scan.ports.block_wr_in)
        self.wire(self._wr_scan_block_wr_in_ready, self.wr_scan.ports.block_wr_in_ready)
        self.wire(self._wr_scan_block_wr_in_valid, self.wr_scan.ports.block_wr_in_valid)

        self.wire(self._rd_scan_coord_out, self.rd_scan.ports.coord_out)
        self.wire(self._rd_scan_coord_out_ready, self.rd_scan.ports.coord_out_ready)
        self.wire(self._rd_scan_coord_out_valid, self.rd_scan.ports.coord_out_valid)

        self.wire(self._rd_scan_pos_out, self.rd_scan.ports.pos_out)
        self.wire(self._rd_scan_pos_out_ready, self.rd_scan.ports.pos_out_ready)
        self.wire(self._rd_scan_pos_out_valid, self.rd_scan.ports.pos_out_valid)

        self.wire(self._rd_scan_us_pos_in, self.rd_scan.ports.us_pos_in)
        self.wire(self._rd_scan_us_pos_in_ready, self.rd_scan.ports.us_pos_in_ready)
        self.wire(self._rd_scan_us_pos_in_valid, self.rd_scan.ports.us_pos_in_valid)

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

        # Now lift the rest of the inputs?
        # lift_io(self.internal_generator)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return self.buffet.get_memory_ports()

    def get_config_mode_str(self):
        return "fiber_access"

    def get_bitstream(self, config_kwargs):

        print("IN FLAVOR TOWN!!!")

        assert 'flavor' in config_kwargs
        flavor = config_kwargs['flavor']

        if flavor == "read_scanner":
            config = self.rd_scan.get_bitstream(config_kwargs=config_kwargs)
        elif flavor == "write_scanner":
            config = self.wr_scan.get_bitstream(config_kwargs=config_kwargs)
        elif flavor == "buffet":
            config = self.buffet.get_bitstream(config_kwargs=config_kwargs)

        print(config)
        for i, (name, val) in enumerate(config):
            config[i] = (f"{flavor}_{name}", val)
        print(config)

        config += [("tile_en", 1)]

        # stop_lvl = config_kwargs['stop_lvl']
        # root = config_kwargs['root']
        # if 'spacc_mode' in config_kwargs:
        #         spacc_mode = config_kwargs['spacc_mode']

        # # Store all configurations here
        # config = [("tile_en", 1),
        #           ("stop_lvl", stop_lvl),
        #           ("root", root),
        #           ("spacc_mode", spacc_mode)
        #           ]

        return config


if __name__ == "__main__":
    fiber_access_dut = FiberAccess(data_width=16,
                                   defer_fifos=False,
                                   use_pipelined_scanner=True,
                                   add_flush=True)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    kts.verilog(fiber_access_dut, filename="fiber_access.sv",
                optimize_if=False)
