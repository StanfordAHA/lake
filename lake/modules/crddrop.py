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


class CrdDrop(MemoryController):
    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 add_clk_enable=True,
                 add_flush=False,
                 lift_config=False,
                 defer_fifos=True,
                 perf_debug=False):

        name_str = f"crddrop"
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

        self.num_streams = 2

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
        
        # mode selection configuration register
        # mode 0: dropping zeroes from the inner stream and their corresponding coordinates from the outer stream
        # mode 1: dropping empty fibers from the outer stream and their corresponding coordinates from the outer stream
        self._cmrg_mode = self.input("cmrg_mode", 1)
        self._cmrg_mode.add_attribute(ConfigRegAttr("Merge mode"))

        self.add_stream_io()

        # performance debuggin hw
        if self.perf_debug:

            cyc_count = add_counter(self, "clock_cycle_count", 64, increment=self._clk & self._clk_en)

            # Start when any of the coord inputs is valid
            self._start_signal = sticky_flag(self, kts.concat((*[self._coord_in_valid_in[i] for i in range(2)])).r_or(),
                                             name='start_indicator')
            self.add_performance_indicator(self._start_signal, edge='posedge', label='start', cycle_count=cyc_count)

            # End when we see DONE on the output coord
            self._done_signal = sticky_flag(self, (self._cmrg_coord_out[0] == MemoryController.DONE_PROXY) &
                                                    self._cmrg_coord_out[0][MemoryController.EOS_BIT] & self._cmrg_coord_out_valid_out[0],
                                                    name='done_indicator')
            self.add_performance_indicator(self._done_signal, edge='posedge', label='done', cycle_count=cyc_count)

        self.add_io_fifo()

        ####################
        # Signals interfacing the fsm, the empty fiber drop logic and the output
        ####################
        # inner stream output fifo push signal generated by the crddrop compression fsm
        # this signal is used to push to the inner stream output fifo in the compression mode
        # in the crddrop mode, this signal is used to push the inner stream token to the empty fiber drop logic
        self._inner_outfifo_push_crddrop_compress_fsm = self.var("inner_outfifo_push_crddrop_compress_fsm", 1)
        self._empty_fiber_drop_buffer_avail = self.var("empty_fiber_drop_buffer_avail", 1)
        # inner stream output fifo push signal generated from the empty fiber drop logic 
        self._inner_outfifo_push_empty_fiber_drop = self.var("inner_outfifo_push_empty_fiber_drop", 1)

        self.add_crddrop_compression_fsm()
        self.add_empty_fiber_drop_logic()

        # wire up the push signal for the inner stream output fifo
        # in crddrop mode, the we use the empty fiber drop to push the inner stream token
        # in the compression mode, we use the crddrop compression fsm to push the inner stream token
        self.wire(self._inner_outfifo_push, kts.ternary(self._cmrg_mode, self._inner_outfifo_push_empty_fiber_drop, self._inner_outfifo_push_crddrop_compress_fsm))
        # wire up the input data for the inner stream output fifo
        self.wire(self._inner_outfifo_data_packed[self.data_width], kts.ternary(self._cmrg_mode, self._empty_fiber_drop_eos_buffer, self._inner_infifo_is_eos))
        self.wire(self._inner_outfifo_data_packed[self.data_width - 1, 0], kts.ternary(self._cmrg_mode, self._empty_fiber_drop_data_buffer, self._inner_infifo_data))

        # Force FSM realization first so that flush gets added...
        kts.passes.realize_fsm(self.internal_generator)

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

    def add_stream_io(self):
        # inner and outer stream input/output
        self._coord_out = []
        self._coord_out_ready_in = []
        self._coord_out_valid_out = []

        self._coord_in = []
        self._coord_in_ready_out = []
        self._coord_in_valid_in = []

        for i in range(2):

            tmp_coord_out = self.output(f"coord_out_{i}", self.data_width + 1, packed=True)
            tmp_coord_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_coord_out_valid_out = self.output(f"coord_out_{i}_valid", 1)
            tmp_coord_out_valid_out.add_attribute(ControlSignalAttr(is_control=False))

            tmp_coord_out_ready_in = self.input(f"coord_out_{i}_ready", 1)
            tmp_coord_out_ready_in.add_attribute(ControlSignalAttr(is_control=True))

            # Add to lists
            self._coord_out.append(tmp_coord_out)
            self._coord_out_ready_in.append(tmp_coord_out_ready_in)
            self._coord_out_valid_out.append(tmp_coord_out_valid_out)

            tmp_coord_in = self.input(f"coord_in_{i}", self.data_width + 1, packed=True)
            tmp_coord_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_coord_in_valid_in = self.input(f"coord_in_{i}_valid", 1)
            tmp_coord_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))

            tmp_coord_in_ready_out = self.output(f"coord_in_{i}_ready", 1)
            tmp_coord_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

            self._coord_in.append(tmp_coord_in)
            self._coord_in_ready_out.append(tmp_coord_in_ready_out)
            self._coord_in_valid_in.append(tmp_coord_in_valid_in)

    def add_io_fifo(self):
        # Create fifos to buffer the input and output streams
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
        inner_outfifo = RegFIFO(**fifo_kwargs)
        inner_outfifo.add_attribute(SharedFifoAttr(direction="OUT"))
        outer_outfifo = RegFIFO(**fifo_kwargs)
        outer_outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

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
                       push=self._coord_in_valid_in[0],
                       pop=self._inner_infifo_pop,
                       data_in=self._coord_in[0],
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
        self.wire(self._coord_in_ready_out[0], ~inner_infifo.ports.full)

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
                       push=self._coord_in_valid_in[1],
                       pop=self._outer_infifo_pop,
                       data_in=self._coord_in[1],
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
        self.wire(self._coord_in_ready_out[1], ~outer_infifo.ports.full)

        ##############
        # Inner Stream Output FIFO
        ##############
        # push singal that would need to hooked up later
        self._inner_outfifo_push = self.var(f"inner_outfifo_push", 1)
        # input to the inner stream output fifo that would need to be hooked up later
        self._inner_outfifo_data_packed = self.var(f"inner_outfifo_data_packed", self.data_width + 1, packed=True)

        self.add_child(f"inner_outfifo",
                       inner_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._inner_outfifo_push,
                       pop=self._coord_out_ready_in[0],
                       data_in=self._inner_outfifo_data_packed,
                       data_out=self._coord_out[0])
        
        # indicator signals from the fifo for ease of use
        self._inner_outfifo_full = self.var("inner_outfifo_full", 1)

        self.wire(self._inner_outfifo_full, inner_outfifo.ports.full)

        # Hook up the valid singal to downstream primitive
        self.wire(self._coord_out_valid_out[0], ~inner_outfifo.ports.empty)

        ##############
        # Outer Stream Output FIFO
        ##############
        # push singal that would need to hooked up later
        self._outer_outfifo_push = self.var(f"outer_outfifo_push", 1)

        self.add_child(f"outer_outfifo",
                       outer_outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._outer_outfifo_push,
                       pop=self._coord_out_ready_in[1],
                       data_in=self._outer_infifo_data_packed,
                       data_out=self._coord_out[1])
        
        # indicator signals from the fifo for ease of use
        self._outer_outfifo_full = self.var("outer_outfifo_full", 1)

        self.wire(self._outer_outfifo_full, outer_outfifo.ports.full)

        # Hook up the valid singal to downstream primitive
        self.wire(self._coord_out_valid_out[1], ~outer_outfifo.ports.empty)

    def get_bitstream(self, config_kwargs):
        cmrg_mode = config_kwargs['cmrg_mode']

        # Store all configurations here
        config = [("tile_en", 1)]
        config += [("cmrg_mode", cmrg_mode)]

        # Dummy variables to fill in later when compiler
        # generates different collateral for different designs
        flattened = create_wrapper_flatten(self.internal_generator.clone(),
                                           self.name + "_W")

        # Trim the list
        return trim_config_list(flattened, config)        

    def add_crddrop_compression_fsm(self):
        ####################
        # Helper Logic/Register for the FSM
        ####################

        # indicators that is set to one if the current coordinate is actual data, not eos or done
        self._inner_is_data = self.var("inner_is_data", 1)
        self.wire(self._inner_is_data, self._inner_infifo_data_valid & ~self._inner_infifo_is_eos)
        self._outer_is_data = self.var("outer_is_data", 1)
        self.wire(self._outer_is_data, self._outer_infifo_data_valid & ~self._outer_infifo_is_eos)

        # indicators that is set to one if the current coordinate is eos
        self._inner_is_eos = self.var("inner_is_eos", 1)
        self.wire(self._inner_is_eos, self._inner_infifo_data_valid & self._inner_infifo_is_eos & (self._inner_infifo_data[9, 8] == kts.const(0, 2)))
        self._outer_is_eos = self.var("outer_is_eos", 1)
        self.wire(self._outer_is_eos, self._outer_infifo_data_valid & self._outer_infifo_is_eos & (self._outer_infifo_data[9, 8] == kts.const(0, 2)))

        # indicators that is set to one if the current coordinate is done
        self._inner_is_done = self.var("inner_is_done", 1)
        self.wire(self._inner_is_done, self._inner_infifo_data_valid & self._inner_infifo_is_eos & (self._inner_infifo_data[9, 8] == kts.const(1, 2)))
        self._outer_is_done = self.var("outer_is_done", 1)
        self.wire(self._outer_is_done, self._outer_infifo_data_valid & self._outer_infifo_is_eos & (self._outer_infifo_data[9, 8] == kts.const(1, 2)))

        # sticky flag that indicate that the current inner fiber is not empty
        self._clr_inner_fiber_not_empty = self.var("clr_inner_fiber_not_empty", 1)
        self._inner_fiber_not_empty = sticky_flag(self, self._inner_is_data, 
                                                  name="inner_fiber_not_empty", 
                                                  clear=self._clr_inner_fiber_not_empty,
                                                  seq_only=True)
            
        ######################################################################
        # State Machine For Outer Stream Coordinate Dropping And Compression #
        ######################################################################

        # Create FSM
        self.crddrop_compress_fsm = self.add_fsm("crddrop_compress_fsm", reset_high=False)
        START = self.crddrop_compress_fsm.add_state("START")
        CRDDROP = self.crddrop_compress_fsm.add_state("CRDDROP")
        COMPRESS = self.crddrop_compress_fsm.add_state("COMPRESS")

        ####################
        # Next State Logic #
        ####################

        ####################
        # START #
        ####################
        START.next(CRDDROP, self._tile_en & self._cmrg_mode)
        START.next(COMPRESS, self._tile_en & ~self._cmrg_mode)
        START.next(START, None)

        ###########
        # CRDDROP #
        ###########
        # In CRDDROP, the inner input is the lower fiber level coordinate stream, 
        # and the outer is the higher level coordinate stream.
        # The goal is to drop all empty fibers from the innner stream (the stop tokens) and 
        # their corresponding coordinates from the outer stream
        # TODO: when all the tokens are processed, should we go back to START state?
        # TODO: when all the tokens are processed, should we go back to START state?
        CRDDROP.next(CRDDROP, None)

        ####################
        # DROPZERO #
        ####################
        # In DROPZERO, the inner input stream is the value stream, 
        # and the outer is its corresponding coordinate stream. 
        # The goal is to drop all coordinates where its corresponding value entries are zero
        # TODO: when all the tokens are processed, should we go back to START state?
        COMPRESS.next(COMPRESS, None)

        ####################
        # FSM Output Logic
        ####################

        self.crddrop_compress_fsm.output(self._inner_infifo_pop)
        self.crddrop_compress_fsm.output(self._outer_infifo_pop)
        self.crddrop_compress_fsm.output(self._inner_outfifo_push_crddrop_compress_fsm)
        self.crddrop_compress_fsm.output(self._outer_outfifo_push)
        self.crddrop_compress_fsm.output(self._clr_inner_fiber_not_empty)

        ################
        # START
        ################
        START.output(self._inner_infifo_pop, 0)
        START.output(self._outer_infifo_pop, 0)
        START.output(self._inner_outfifo_push_crddrop_compress_fsm, 0)
        START.output(self._outer_outfifo_push, 0)
        START.output(self._clr_inner_fiber_not_empty, 0)

        ################
        # CRDDROP
        ################
        self._inner_infifo_pop_crddrop = self.var("inner_infifo_pop_crddrop", 1)
        self._outer_infifo_pop_crddrop = self.var("outer_infifo_pop_crddrop", 1)
        self._inner_outfifo_push_crddrop = self.var("inner_outfifo_push_crddrop", 1)
        self._outer_outfifo_push_crddrop = self.var("outer_outfifo_push_crddrop", 1)
        self._clr_inner_fiber_not_empty_crddrop = self.var("clr_inner_fiber_not_empty_crddrop", 1)

        CRDDROP.output(self._inner_infifo_pop, self._inner_infifo_pop_crddrop)
        CRDDROP.output(self._outer_infifo_pop, self._outer_infifo_pop_crddrop)
        CRDDROP.output(self._inner_outfifo_push_crddrop_compress_fsm, self._inner_outfifo_push_crddrop)
        CRDDROP.output(self._outer_outfifo_push, self._outer_outfifo_push_crddrop)
        CRDDROP.output(self._clr_inner_fiber_not_empty, self._clr_inner_fiber_not_empty_crddrop)

        @always_comb
        def crddrop_logic():
            # default values, do nothing
            self._inner_infifo_pop_crddrop = 0
            self._outer_infifo_pop_crddrop = 0
            self._inner_outfifo_push_crddrop = 0
            self._outer_outfifo_push_crddrop = 0
            self._clr_inner_fiber_not_empty_crddrop = 0

            # outer coordinate stream is eos, there must be a corresponding
            # eos token on the inner stream. Same thing for done tokens.
            # in this situation, we need to pop both the inner and outer stream
            # and push them to the outer fifo and the inner straem empty fiber drop logic
            if (self._outer_is_eos | self._outer_is_done):
                # check the availability of the outer output fifo and the emprt fiber drop buffer
                if (~self._outer_outfifo_full & self._empty_fiber_drop_buffer_avail):
                    self._inner_infifo_pop_crddrop = 1
                    self._outer_infifo_pop_crddrop = 1
                    self._inner_outfifo_push_crddrop = 1
                    self._outer_outfifo_push_crddrop = 1
                    self._clr_inner_fiber_not_empty_crddrop = 1

            # the current outer coordinate is a actual coordinate (data)
            # scan through the inner fiber corresponding to this outer coordinate
            elif (self._outer_is_data):
                # the innner coordinate is a peice of data, check for space in the
                # empty fiber drop buffer and push the inner coordinate to the buffer
                if (self._inner_is_data):
                    if (self._empty_fiber_drop_buffer_avail):
                        self._inner_infifo_pop_crddrop = 1
                        self._inner_outfifo_push_crddrop = 1
               
                elif (self._inner_is_eos):
                    # the token is a S0 stop token, there's no corresponding stop token in the outer stream
                    if (self._inner_infifo_data == kts.const(0, self.data_width)):
                        # the inner fiber is not empty, we are pushing both innner eos and outer coordinate 
                        # check for fifo capacity and empty fiber drop buffer availability, pop both the 
                        # inner and outer stream and push both of them. 
                        if (self._inner_fiber_not_empty):
                            if (~self._inner_outfifo_full & ~self._outer_outfifo_full):
                                self._inner_infifo_pop_crddrop = 1
                                self._outer_infifo_pop_crddrop = 1
                                self._inner_outfifo_push_crddrop = 1
                                self._outer_outfifo_push_crddrop = 1
                                # end of inner fiber, clear empty flag
                                self._clr_inner_fiber_not_empty_crddrop = 1

                        # the inner fiber is empty, discard the outer coordinate by not 
                        # pushing them into the output fifos, however, we still need to push the 
                        # inner eos to the empty fiber drop logic
                        else:
                            if (self._empty_fiber_drop_buffer_avail):
                                self._inner_infifo_pop_crddrop = 1
                                self._outer_infifo_pop_crddrop = 1
                                self._inner_outfifo_push_crddrop = 1
                                # end of inner fiber, clear empty flag
                                self._clr_inner_fiber_not_empty_crddrop = 1

                    # the inner stream is a stop token of higher dimension than S0, there must be a 
                    # corresponding stop token comming up next in the outer fifo. We need to pop the outer 
                    # coordinate and retain the inner eos to make the inner and outer eos aligned
                    else:
                        # the inner fiber is not empty, check for space in the outer output fifo and push the outer coordinate
                        if (self._inner_fiber_not_empty):
                            if (~self._outer_outfifo_full):
                                self._outer_infifo_pop_crddrop = 1
                                self._outer_outfifo_push_crddrop = 1
                            
                        # inner fiber is empty, discard the outer coordinate by not pushing it 
                        else:
                            self._outer_infifo_pop_crddrop = 1

        self.add_code(crddrop_logic)

        ################
        # COMPRESS
        ################
        self._inner_infifo_pop_compress = self.var("inner_infifo_pop_compress", 1)
        self._outer_infifo_pop_compress = self.var("outer_infifo_pop_compress", 1)
        self._inner_outfifo_push_compress = self.var("inner_outfifo_push_compress", 1)
        self._outer_outfifo_push_compress = self.var("outer_outfifo_push_compress", 1)
        self._clr_inner_fiber_not_empty_compress = self.var("clr_inner_fiber_not_empty_compress", 1)

        COMPRESS.output(self._inner_infifo_pop, self._inner_infifo_pop_compress)
        COMPRESS.output(self._outer_infifo_pop, self._outer_infifo_pop_compress)
        COMPRESS.output(self._inner_outfifo_push_crddrop_compress_fsm, self._inner_outfifo_push_compress)
        COMPRESS.output(self._outer_outfifo_push, self._outer_outfifo_push_compress)
        COMPRESS.output(self._clr_inner_fiber_not_empty, self._clr_inner_fiber_not_empty_compress)

        @always_comb
        def compress_logic(self):
            # default values, do nothing
            self._inner_infifo_pop_compress = 0
            self._outer_infifo_pop_compress = 0
            self._inner_outfifo_push_compress = 0
            self._outer_outfifo_push_compress = 0
            self._clr_inner_fiber_not_empty_compress = 0

            if (self._inner_is_data & self._outer_is_data):
                # the value is zero, drop the value and its corresponding coordintate by not pushing them into the output fifos
                if (self._inner_infifo_data == kts.const(0, self.data_width)):
                    self._inner_infifo_pop_compress = 1
                    self._outer_infifo_pop_compress = 1

                else:
                    # if the data value on the value stream is not zero, we need to check for space in the output fifos to push them 
                    if (~self._inner_outfifo_full & ~self._outer_outfifo_full):
                        # check for available space in both output fifos
                        self._inner_infifo_pop_compress = 1
                        self._inner_outfifo_push_compress = 1
                        self._outer_infifo_pop_compress = 1
                        self._outer_outfifo_push_compress = 1
    
            # the inner and output are eos tokens or done tokens, pop and push both if there's space
            elif ((self._inner_is_eos & self._outer_is_eos) | (self._inner_is_done & self._outer_is_done)):
                if (~self._inner_outfifo_full & ~self._outer_outfifo_full):
                    self._inner_infifo_pop_compress = 1
                    self._outer_infifo_pop_compress = 1
                    self._inner_outfifo_push_compress = 1
                    self._outer_outfifo_push_compress = 1
        self.add_code(compress_logic)
        self.crddrop_compress_fsm.set_start_state(START)

    def add_empty_fiber_drop_logic(self):
        #############################
        # Helper Logic and Register #
        #############################
        # registers for buffering the inner stream data and eos indicator 
        self._empty_fiber_drop_buffer_en = self.var("empty_fiber_drop_data_buffer_en", 1)
        self._empty_fiber_drop_data_buffer = register(self, self._inner_infifo_data, 
                                                      enable=self._empty_fiber_drop_buffer_en,
                                                      name="empty_fiber_drop_data_buffer")
        self._empty_fiber_drop_eos_buffer = register(self, self._inner_infifo_is_eos, 
                                                     enable=self._empty_fiber_drop_buffer_en,
                                                     name="empty_fiber_drop_eos_buffer")
        # indicator flag for indicating if the empty fiber drop buffer is occupied or not
        # data is sucessfully pushed to the buffer (by setting the empty_fiber_drop_buffer_en singal), 
        # it will be valid. the only situation where it is not going to be valid is when we are pushing 
        # the content in the buffer to the outpuf fifo and no data is coming up to replace it
        self._empty_fiber_drop_buffer_valid = sticky_flag(self, self._empty_fiber_drop_buffer_en,
                                                          clear=self._inner_outfifo_push_empty_fiber_drop & ~self._empty_fiber_drop_buffer_en, 
                                                          name="empty_fiber_drop_buffer_valid", seq_only=True)
        self._empty_fiber_drop_buffer_avail = self.var("empty_fiber_drop_buffer_avail", 1)
        # indicator flag for indicating if the empty fiber drop is not seeing leading stop tokens
        self._clr_not_leading_eos = self.var("clr_not_leading_eos", 1)
        # it's no longer possible to have a leading stop token as soon as we see a valid data or done token
        self._not_leading_eos = sticky_flag(self, self._inner_is_data | self._inner_is_done, 
                                            clear=self._clr_not_leading_eos,
                                            name="leading_not_eos", seq_only=True)
        
        # indicators showing whether the buffered data is a coordinate, stop token or done token
        self._empty_fiber_drop_buffer_is_data = self.var("empty_fiber_drop_buffer_is_data", 1)
        self.wire(self._empty_fiber_drop_buffer_is_data, self._empty_fiber_drop_buffer_valid & ~self._empty_fiber_drop_eos_buffer)
        self._empty_fiber_drop_buffer_is_eos = self.var("empty_fiber_drop_buffer_is_eos", 1)
        self.wire(self._empty_fiber_drop_buffer_is_eos, self._empty_fiber_drop_buffer_valid & self._empty_fiber_drop_eos_buffer & (self._empty_fiber_drop_data_buffer[9, 8] == kts.const(0, 2)))
        self._empty_fiber_drop_buffer_is_done = self.var("empty_fiber_drop_buffer_is_done", 1)
        self.wire(self._empty_fiber_drop_buffer_is_done, self._empty_fiber_drop_buffer_valid & self._empty_fiber_drop_eos_buffer & (self._empty_fiber_drop_data_buffer[9, 8] == kts.const(1, 2)))
        
        @always_comb
        def empty_fiber_drop_buffer_logic(self):
            # default value, do nothing
            self._empty_fiber_drop_buffer_en = 0
            self._clr_not_leading_eos = 0
            # only operates when the crddrop is in crddrop mode
            if (self._tile_en & self._cmrg_mode):
                if (self._inner_outfifo_push_crddrop_compress_fsm):
                    # buffer the inner stream token it is a peice of data or done token
                    if (self._inner_is_data | self._inner_is_done):
                        self._empty_fiber_drop_buffer_en = 1
                        # end of fiber stree, reset the not leading eos flag
                        if (self._inner_is_done):
                            self._clr_not_leading_eos = 1
                    # only consider buffering eos if it is not leading the fiber tree
                    elif (self._inner_is_eos & self._not_leading_eos):
                        # the buffer have a eos in it, compare the level between the existing eos and the new eos
                        # replace the buffered eos if the new eos is of higher level
                        if (self._empty_fiber_drop_buffer_is_eos):
                            if (self._inner_infifo_data > self._empty_fiber_drop_data_buffer):
                                self._empty_fiber_drop_buffer_en = 1
                        # the buffer is empty, buffer the eos
                        else:
                            self._empty_fiber_drop_buffer_en = 1
        self.add_code(empty_fiber_drop_buffer_logic)

        @always_comb
        def empty_fiber_drop_buffer_push_logic(self):
            # default values, do nothing
            self._inner_outfifo_push_empty_fiber_drop = 0
            # only do work when the crddrop is in crddrop mode
            if (self._tile_en & self._cmrg_mode):
                # if the buffered data is done token or coordinate, and there's space in the output fifo, push the data
                if ((self._empty_fiber_drop_buffer_is_data | self._empty_fiber_drop_buffer_is_done) & ~self._inner_outfifo_full):
                    self._inner_outfifo_push_empty_fiber_drop = 1
                # if the current buffered data is eos, and the data coming up is coordinate or done token 
                # check for space in the output fifo and push the eos
                elif (self._empty_fiber_drop_buffer_is_eos & (self._inner_is_done | self._inner_is_data) & ~self._inner_outfifo_full):
                    self._inner_outfifo_push_empty_fiber_drop = 1

        self.add_code(empty_fiber_drop_buffer_push_logic)

        @always_comb
        def empty_fiber_drop_buffer_avail_logic(self):
            # default valules, the buffer is not available
            self._empty_fiber_drop_buffer_avail = 0
            # the buffer is available to be pushed if it is empty
            if (~self._empty_fiber_drop_buffer_valid):
                self._empty_fiber_drop_buffer_avail = 1
            else:
                # the buffer is not empty, but it is available if the current buffered data is getting 
                # pushed to the output fifo and can be replaced
                if (self._inner_outfifo_push_empty_fiber_drop):
                    self._empty_fiber_drop_buffer_avail = 1
                # the currently buffered data is a stop token, and the next token is also a stop token,
                # the buffer will be marked as available to allow the crddrop fsm to push to it,
                # so we can compare the level of the two stop tokens
                elif (self._empty_fiber_drop_buffer_is_eos & self._inner_is_eos):
                    self._empty_fiber_drop_buffer_avail = 1
        self.add_code(empty_fiber_drop_buffer_avail_logic)

    def get_memory_ports(self):
        '''
        Use this method to indicate what memory ports this controller has
        '''
        return [[None]]

    def get_config_mode_str(self):
        return "crddrop"


if __name__ == "__main__":
    crddrop_dut = CrdDrop(data_width=16, defer_fifos=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    kts.verilog(crddrop_dut, filename="crd_drop.sv",
            optimize_if=False)
