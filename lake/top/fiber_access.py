import kratos as kts
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.top.memory_controller import MemoryController
from lake.modules.buffet_like import BuffetLike
from lake.modules.scanner import Scanner
from lake.modules.write_scanner import WriteScanner
from lake.passes.passes import lift_config_reg


class FiberAccess(MemoryController):

    def __init__(self, data_width=16):
        super().__init__(f'fiber_access_{data_width}', debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = True

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

        self._wr_scan_data_in = self.input("wr_scan_data_in", self.data_width + 1, packed=True)
        self._wr_scan_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._wr_scan_data_in_ready = self.output("wr_scan_data_in_ready", 1)
        self._wr_scan_data_in_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        self._wr_scan_data_in_valid = self.input("wr_scan_data_in_valid", 1)
        self._wr_scan_data_in_valid.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self._wr_scan_addr_in = self.input("wr_scan_addr_in", self.data_width + 1, packed=True)
        self._wr_scan_addr_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._wr_scan_addr_in_ready = self.output("wr_scan_addr_in_ready", 1)
        self._wr_scan_addr_in_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        self._wr_scan_addr_in_valid = self.input("wr_scan_addr_in_valid", 1)
        self._wr_scan_addr_in_valid.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self._rd_scan_coord_out = self.output("rd_scan_coord_out", self.data_width + 1, packed=True)
        self._rd_scan_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._rd_scan_coord_out_ready = self.input("rd_scan_coord_out_ready", 1)
        self._rd_scan_coord_out_ready.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
        self._rd_scan_coord_out_valid = self.output("rd_scan_coord_out_valid", 1)
        self._rd_scan_coord_out_valid.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._rd_scan_pos_out = self.output("rd_scan_pos_out", self.data_width + 1, packed=True)
        self._rd_scan_pos_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._rd_scan_pos_out_ready = self.input("rd_scan_pos_out_ready", 1)
        self._rd_scan_pos_out_ready.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
        self._rd_scan_pos_out_valid = self.output("rd_scan_pos_out_valid", 1)
        self._rd_scan_pos_out_valid.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._rd_scan_us_pos_in = self.input("rd_scan_us_pos_in", self.data_width + 1, packed=True)
        self._rd_scan_us_pos_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._rd_scan_us_pos_in_ready = self.output("rd_scan_us_pos_in_ready", 1)
        self._rd_scan_us_pos_in_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        self._rd_scan_us_pos_in_valid = self.input("rd_scan_us_pos_in_valid", 1)
        self._rd_scan_us_pos_in_valid.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        buffet = BuffetLike(data_width=self.data_width, num_ID=2, mem_depth=512, local_memory=True)

        wr_scan = WriteScanner(data_width=self.data_width)

        rd_scan = Scanner(data_width=self.data_width)

        self.add_child('buffet_like',
                       buffet,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       tile_en=self._tile_en)

        self.add_child('wr_scan',
                       wr_scan,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       tile_en=self._tile_en)

        self.add_child('rd_scan',
                       rd_scan,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       tile_en=self._tile_en)

        # Now wire everything
        # buffet to wr_scan
        self.wire(buffet.ports.wr_ID, wr_scan.ports.ID_out)
        self.wire(buffet.ports.wr_ID_ready, wr_scan.ports.ID_out_ready)
        self.wire(buffet.ports.wr_ID_valid, wr_scan.ports.ID_out_valid)

        self.wire(buffet.ports.wr_addr, wr_scan.ports.addr_out)
        self.wire(buffet.ports.wr_addr_ready, wr_scan.ports.addr_out_ready)
        self.wire(buffet.ports.wr_addr_valid, wr_scan.ports.addr_out_valid)

        self.wire(buffet.ports.wr_data, wr_scan.ports.data_out)
        self.wire(buffet.ports.wr_data_ready, wr_scan.ports.data_out_ready)
        self.wire(buffet.ports.wr_data_valid, wr_scan.ports.data_out_valid)

        self.wire(buffet.ports.rd_op, rd_scan.ports.op_out)
        self.wire(buffet.ports.rd_op_ready, rd_scan.ports.op_out_ready)
        self.wire(buffet.ports.rd_op_valid, rd_scan.ports.op_out_valid)

        self.wire(buffet.ports.rd_addr, rd_scan.ports.addr_out)
        self.wire(buffet.ports.rd_addr_ready, rd_scan.ports.addr_out_ready)
        self.wire(buffet.ports.rd_addr_valid, rd_scan.ports.addr_out_valid)

        self.wire(buffet.ports.rd_ID, rd_scan.ports.ID_out)
        self.wire(buffet.ports.rd_ID_ready, rd_scan.ports.ID_out_ready)
        self.wire(buffet.ports.rd_ID_valid, rd_scan.ports.ID_out_valid)

        self.wire(buffet.ports.rd_rsp_data, rd_scan.ports.rd_rsp_data_in)
        self.wire(buffet.ports.rd_rsp_data_ready, rd_scan.ports.rd_rsp_data_in_ready)
        self.wire(buffet.ports.rd_rsp_data_valid, rd_scan.ports.rd_rsp_data_in_valid)

        self.wire(self._wr_scan_data_in, wr_scan.ports.data_in)
        self.wire(self._wr_scan_data_in_ready, wr_scan.ports.data_in_ready)
        self.wire(self._wr_scan_data_in_valid, wr_scan.ports.data_in_valid)

        self.wire(self._wr_scan_addr_in, wr_scan.ports.addr_in)
        self.wire(self._wr_scan_addr_in_ready, wr_scan.ports.addr_in_ready)
        self.wire(self._wr_scan_addr_in_valid, wr_scan.ports.addr_in_valid)

        self.wire(self._rd_scan_coord_out, rd_scan.ports.coord_out)
        self.wire(self._rd_scan_coord_out_ready, rd_scan.ports.coord_out_ready)
        self.wire(self._rd_scan_coord_out_valid, rd_scan.ports.coord_out_valid)

        self.wire(self._rd_scan_pos_out, rd_scan.ports.pos_out)
        self.wire(self._rd_scan_pos_out_ready, rd_scan.ports.pos_out_ready)
        self.wire(self._rd_scan_pos_out_valid, rd_scan.ports.pos_out_valid)

        self.wire(self._rd_scan_us_pos_in, rd_scan.ports.us_pos_in)
        self.wire(self._rd_scan_us_pos_in_ready, rd_scan.ports.us_pos_in_ready)
        self.wire(self._rd_scan_us_pos_in_valid, rd_scan.ports.us_pos_in_valid)

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
        return [[None]]

    def get_config_mode_str(self):
        return "fiber_access"


if __name__ == "__main__":
    fiber_access_dut = FiberAccess(data_width=16)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    kts.verilog(fiber_access_dut, filename="fiber_access.sv",
                optimize_if=False)
