from math import e
from struct import pack
import kratos as kts
from kratos import *
from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.passes.passes import lift_config_reg
from lake.utils.util import sticky_flag, trim_config_list, add_counter, register
from lake.attributes.formal_attr import FormalAttr, FormalSignalConstraint
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.top.memory_controller import MemoryController
from lake.attributes.control_signal_attr import ControlSignalAttr
from _kratos import create_wrapper_flatten
from lake.modules.reg_fifo import RegFIFO


class Locator(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 add_clk_enable=True,
                 add_flush=False,
                 lift_config=False,
                 defer_fifos=True,
                 perf_debug=False):

        name_str = f"locator"
        super().__init__(name=name_str, debug=True)

        self.data_width = data_width
        self.add_clk_enable = add_clk_enable
        self.add_flush = add_flush
        self.lift_config = lift_config
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos
        self.perf_debug = perf_debug

        # For compatibility with tile integration...
        self.total_sets = 0

        self._clk = self.clock("clk")
        self._clk.add_attribute(FormalAttr(f"{self._clk.name}", FormalSignalConstraint.CLK))
        self._rst_n = self.reset("rst_n")
        self._rst_n.add_attribute(FormalAttr(f"{self._rst_n.name}", FormalSignalConstraint.RSTN))
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        # gated clk
        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # config regs
        # this sepcifies what fiber level is the locator performaning on
        self._locate_lvl = self.input("locate_lvl", 16)
        self._locate_lvl.add_attribute(ConfigRegAttr("Locator level"))
        # the stride of the locator 
        self._locate_dim_size = self.input("locate_dim_size", 5)
        self._locate_dim_size.add_attribute(ConfigRegAttr("Locator dimension size"))

        # IO ports 
        self._coord_in_0 = self.input("coord_in_0", self.data_width + 1, packed=True)
        self._coord_in_0.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._coord_in_0_valid = self.input("coord_in_0_valid", 1)
        self._coord_in_0_valid.add_attribute(ControlSignalAttr(is_control=True))
        self._coord_in_0_ready = self.output("coord_in_0_ready", 1)
        self._coord_in_0_ready.add_attribute(ControlSignalAttr(is_control=False))
    
        self._coord_in_1 = self.input("coord_in_1", self.data_width + 1, packed=True)
        self._coord_in_1.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._coord_in_1_valid = self.input("coord_in_1_valid", 1)
        self._coord_in_1_valid.add_attribute(ControlSignalAttr(is_control=True))
        self._coord_in_1_ready = self.output("coord_in_1_ready", 1)
        self._coord_in_1_ready.add_attribute(ControlSignalAttr(is_control=False))

        self._addr_out = self.output("addr_out", self.data_width + 1, packed=True)
        self._addr_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._addr_out_valid = self.output("addr_out_valid", 1)
        self._addr_out_valid.add_attribute(ControlSignalAttr(is_control=True))
        self._addr_out_ready = self.input("addr_out_ready", 1)
        self._addr_out_ready.add_attribute(ControlSignalAttr(is_control=False))

        # FIFOs
        fifo_kwargs = {
            "data_width": self.data_width + 1,
            "width_mult": 1,
            "depth": self.fifo_depth,
            'defer_hrdwr_gen': self.defer_fifos
        }
        inner_infifo = RegFIFO(**fifo_kwargs)
        inner_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        outer_infifo = RegFIFO(**fifo_kwargs)
        outer_infifo.add_attribute(SharedFifoAttr(direction="IN"))
        addr_outfifo = RegFIFO(**fifo_kwargs)
        addr_outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

        ##############
        # Inner Stream Input FIFO
        ##############
        self._inner_infifo_data_packed = self.var(f"inner_infifo_data_packed", self.data_width + 1, packed=True)
        # pop singal that would need to hooked up later
        self._inner_infifo_pop = self.var(f"inner_infifo_pop", 1)

        self.add_child(f"inner_infifo",
                       inner_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._coord_in_0_valid,
                       pop=self._inner_infifo_pop,
                       data_in=self._coord_in_0,
                       data_out=self._inner_infifo_data_packed)

        # Unpacked data and indicator signals from the fifo for ease of use
        self._inner_infifo_data = self.var("inner_infifo_data", 16)
        self._inner_infifo_is_eos = self.var("inner_infifo_is_eos", 1)
        self._inner_infifo_data_valid = self.var("inner_infifo_data_valid", 1)

        # Unpack the data
        self.wire(self._inner_infifo_is_eos, self._inner_infifo_data_packed[self.data_width])
        self.wire(self._inner_infifo_data, self._inner_infifo_data_packed[self.data_width - 1, 0])

        self.wire(self._inner_infifo_data_valid, ~inner_infifo.ports.empty)

        # Hook up the ready singal to upstream primitive
        self.wire(self._coord_in_0_ready, ~inner_infifo.ports.full)

        ##############
        # Outer Stream Input FIFO
        ##############
        self._outer_infifo_data_packed = self.var(f"outer_infifo_data_packed", self.data_width + 1, packed=True)
        # pop singal that would need to hooked up later
        self._outer_infifo_pop = self.var(f"outer_infifo_pop", 1)

        self.add_child(f"outer_infifo",
                       outer_infifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._coord_in_1_valid,
                       pop=self._outer_infifo_pop,
                       data_in=self._coord_in_1,
                       data_out=self._outer_infifo_data_packed)

        # Unpacked data and indicator signals from the fifo for ease of use
        self._outer_infifo_data = self.var("outer_infifo_data", 16)
        self._outer_infifo_is_eos = self.var("outer_infifo_is_eos", 1)
        self._outer_infifo_data_valid = self.var("outer_infifo_data_valid", 1)

        # Unpack the data
        self.wire(self._outer_infifo_is_eos, self._outer_infifo_data_packed[self.data_width])
        self.wire(self._outer_infifo_data, self._outer_infifo_data_packed[self.data_width - 1, 0])

        self.wire(self._outer_infifo_data_valid, ~outer_infifo.ports.empty)

        # Hook up the ready singal to upstream primitive
        self.wire(self._coord_in_1_ready, ~outer_infifo.ports.full)
        
        ##############
        # Address Output FIFO
        ##############
        # push singal that would need to hooked up later
        self._addr_outfifo_push = self.var(f"addr_outfifo_push", 1)
        # input to the inner stream output fifo that would need to be hooked up later
        self._addr_outfifo_data_packed = self.var(f"addr_outfifo_data_packed", self.data_width + 1, packed=True)

        self.add_child(f"addr_outfifo",
                       addr_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._addr_outfifo_push,
                       pop=self._addr_out_ready,
                       data_in=self._addr_outfifo_data_packed,
                       data_out=self._addr_out)

        # indicator signals from the fifo for ease of use
        self._addr_outfifo_full = self.var("addr_outfifo_full", 1)

        self.wire(self._addr_outfifo_full, addr_outfifo.ports.full)

        # Hook up the valid singal to downstream primitive
        self.wire(self._addr_out_valid, ~addr_outfifo.ports.empty)

        # helper signals
        self._inner_is_data = self.var("inner_is_data", 1)
        self.wire(self._inner_is_data, ~self._inner_infifo_is_eos & self._inner_infifo_data_valid)
        self._inner_is_eos = self.var("inner_is_eos", 1)
        self.wire(self._inner_is_eos, self._inner_infifo_is_eos & self._inner_infifo_data_valid)
        self._inner_is_done = self.var("inner_is_done", 1)
        self.wire(self._inner_is_done, self._inner_infifo_is_eos & (self._inner_infifo_data[9, 8] == kts.const(1, 2)) * self._inner_infifo_data_valid)
        self._outer_is_data = self.var("outer_is_data", 1)
        self.wire(self._outer_is_data, ~self._outer_infifo_is_eos & self._outer_infifo_data_valid)
        self._outer_is_eos = self.var("outer_is_eos", 1)
        self.wire(self._outer_is_eos, self._outer_infifo_is_eos & self._outer_infifo_data_valid)
        self._outer_is_done = self.var("outer_is_done", 1)
        self.wire(self._outer_is_done, self._outer_infifo_is_eos & (self._outer_infifo_data[9, 8] == kts.const(1, 2)) & self._outer_infifo_data_valid)
        self._addr_outfifo_is_eos = self.var("addr_outfifo_is_eos", 1)
        self._addr_outfifo_data = self.var("addr_outfifo_data", self.data_width)
        self.wire(self._addr_outfifo_data_packed[self.data_width - 1, 0], self._addr_outfifo_data)
        self.wire(self._addr_outfifo_data_packed[self.data_width], self._addr_outfifo_is_eos)

        @always_comb
        def locate_logic(self):
            # default values, do nothing
            self._inner_infifo_pop = 0
            self._outer_infifo_pop = 0
            self._addr_outfifo_push = 0
            self._addr_outfifo_is_eos = 0
            self._addr_outfifo_data = 0

            if (self._inner_is_data & self._outer_is_data):
                # both inner and out fifo are acutal data, peform locating address offset
                if (~self._addr_outfifo_full):
                    self._inner_infifo_pop = 1
                    self._addr_outfifo_push = 1
                    self._addr_outfifo_is_eos = 0
                    self._addr_outfifo_data = self._inner_infifo_data + self._outer_infifo_data * self._locate_dim_size

            # the inner and output are eos tokens or done tokens, pop and push both if there's space
            elif (self._inner_is_eos & ~self._inner_is_done & self._outer_is_data):
                if (~self._addr_outfifo_full):
                    self._addr_outfifo_push = 1
                    self._addr_outfifo_is_eos = 1
                    self._addr_outfifo_data = self._inner_infifo_data
                    if (self._inner_infifo_data >= self._locate_lvl):
                        # if inner matches the expected level of eos token, pop both inner and outer
                        self._inner_infifo_pop = 1
                        self._outer_infifo_pop = 1
                    else:
                        self._inner_infifo_pop = 1
            # done with the inner fifo, drain the outer fifo(should be a single eos)
            elif (self._inner_is_done & ~self._outer_is_done):
                self._outer_infifo_pop = 1
            elif (self._inner_is_done & self._outer_is_done):
                if (~self._addr_outfifo_full):
                    self._addr_outfifo_push = 1
                    self._addr_outfifo_is_eos = 1
                    self._addr_outfifo_data = self._inner_infifo_data
                    self._inner_infifo_pop = 1
                    self._outer_infifo_pop = 1

        self.add_code(locate_logic)

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

        if self.lift_config:
            # Finally, lift the config regs...
            lift_config_reg(self.internal_generator)

    def get_bitstream(self, config_kwargs):
        locator_lvl = config_kwargs['locator_lvl']
        locator_dim_size = config_kwargs['locator_dim_size']

        # Store all configurations here
        config = [("tile_en", 1)]
        config += [("locator_lvl", locator_lvl)]
        config += [("locator_dim_size", locator_dim_size)]

        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Trim the list
        return trim_config_list(flattened, config)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "locator"


if __name__ == "__main__":
    crddrop_dut = Locator(data_width=16, defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    kts.verilog(crddrop_dut, filename="locator.sv",
            optimize_if=False)
