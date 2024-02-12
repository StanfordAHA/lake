import kratos as kts
from kratos import *
from lake.attributes.hybrid_port_attr import HybridPortAddr
from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.modules.alu import ALU
from lake.modules.onyx_pe_intf import OnyxPEInterface
from lake.passes.passes import lift_config_reg
from lake.modules.for_loop import ForLoop
from lake.modules.addr_gen import AddrGen
from lake.top.memory_controller import MemoryController
from lake.utils.util import add_counter, add_counter, sticky_flag
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class OnyxDensePE(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 defer_fifos=True,
                 ext_pe_prefix="PEGEN_",
                 pe_ro=True,
                 do_config_lift=False,
                 add_flush=False,
                 perf_debug=True):

        super().__init__("PE_onyx", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = add_flush
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos
        self.ext_pe_prefix = ext_pe_prefix
        self.pe_ro = pe_ro
        self.do_config_lift = do_config_lift
        self.perf_debug = perf_debug

        # For consistency with Core wrapper in garnet...
        self.total_sets = 0

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

        # Scanner interface will need
        # input data, input valid
        # output address, output valid
        self._data_in = []

        for i in range(3):
            tmp_data_in = self.input(f"data{i}", self.data_width, packed=True)
            tmp_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            # Mark as hybrid port to allow bypassing at the core combiner level
            tmp_data_in.add_attribute(HybridPortAddr())
            self._data_in.append(tmp_data_in)

        self._data_in.append(tmp_data_in)

        self._bit_in = []

        for i in range(3):

            tmp_data_in = self.input(f"bit{i}", 1)
            tmp_data_in.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
            self._bit_in.append(tmp_data_in)

        self._data_out = self.output("res", self.data_width, packed=True)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._data_out.add_attribute(HybridPortAddr())

        self._data_out_p = self.output("res_p", 1)
        self._data_out_p.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._pe_output = self.var("pe_output", self.data_width)
        self.wire(self._data_out, self._pe_output)

# =============================
# Instantiate actual PE
# =============================

        self.my_alu = OnyxPEInterface(data_width=self.data_width,
                                      name_prefix=self.ext_pe_prefix,
                                      include_RO_cfg=self.pe_ro)

        # Need active high reset for PE
        self._active_high_reset = kts.util.async_reset(~self._rst_n)

        self.add_child(f"onyxpeintf",
                       self.my_alu,
                       CLK=self._gclk,
                       clk_en=self._clk_en,
                       ASYNCRESET=self._active_high_reset,
                       data0=self._data_in[0][self.data_width - 1, 0],
                       data1=self._data_in[1][self.data_width - 1, 0],
                       data2=self._data_in[2][self.data_width - 1, 0],
                       bit0=self._bit_in[0],
                       bit1=self._bit_in[1],
                       bit2=self._bit_in[2],
                       O0=self._pe_output,
                       O1=self._data_out_p)

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
        if self.do_config_lift:
            lift_config_reg(self.internal_generator)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "alu"

    # def get_bitstream(self, op):
    def get_bitstream(self, config_kwargs):

        # Store all configurations here
        config = [("tile_en", 1)]

        op = config_kwargs['op']
        sub_config = self.my_alu.get_bitstream(op=op)
        for config_tuple in sub_config:
            config_name, config_value = config_tuple
            config += [(f"{self.my_alu.instance_name}_{config_name}", config_value)]

        return config


if __name__ == "__main__":

    pe_dut = OnyxDensePE(data_width=16, defer_fifos=False, do_config_lift=False)

    # Lift config regs and generate annotation
    verilog(pe_dut, filename="onyxdensepe.sv",
            optimize_if=False)
