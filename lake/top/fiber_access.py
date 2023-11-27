import kratos as kts
from kratos import *
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.top.memory_controller import MemoryController
from lake.modules.buffet_like import BuffetLike
from lake.modules.scanner import Scanner
from lake.modules.scanner_pipe import ScannerPipe
from lake.modules.write_scanner import WriteScanner
from lake.passes.passes import lift_config_reg
from lake.top.tech_maps import GF_Tech_Map
from lake.utils.util import sticky_flag


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
                 split_memory_ports=True,
                 mem_width=32,
                 mem_depth=512,
                 tb_harness=False):
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
        self.mem_width = mem_width
        self.tb_harness = tb_harness
        self.mem_depth = mem_depth
        self.mem_addr_width = kts.clog2(self.mem_depth)

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

        # Vector Reduce Mode
        self._vector_reduce_mode = self.input("vector_reduce_mode", 1)
        self._vector_reduce_mode.add_attribute(ConfigRegAttr("Operating in vector reduce mode?"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # MO: Moved the below stuff up here b/c it is needed by VR FSM
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

        self.add_child(self.rd_scan_pre,
                self.rd_scan,
                clk=self._gclk,
                rst_n=self._rst_n,
                clk_en=self._clk_en)

        self._wr_scan_data_in = self.input(f"{self.wr_scan_pre}_data_in", self.data_width + 1, packed=True)
        self._wr_scan_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._wr_scan_data_in_ready = self.output(f"{self.wr_scan_pre}_data_in_ready", 1)
        self._wr_scan_data_in_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        self._wr_scan_data_in_valid = self.input(f"{self.wr_scan_pre}_data_in_valid", 1)
        self._wr_scan_data_in_valid.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

        self._rd_scan_coord_out_ready = self.input(f"{self.rd_scan_pre}_coord_out_ready", 1)
        self._rd_scan_coord_out_ready.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
        self.wire(self._rd_scan_coord_out_ready, self.rd_scan.ports.coord_out_ready)

        self._rd_scan_us_pos_in_ready = self.output(f"{self.rd_scan_pre}_us_pos_in_ready", 1)
        self._rd_scan_us_pos_in_ready.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        self.wire(self._rd_scan_us_pos_in_ready, self.rd_scan.ports.us_pos_in_ready)

        self._rd_scan_pos_out = self.output(f"{self.rd_scan_pre}_pos_out", self.data_width + 1, packed=True)
        self._rd_scan_pos_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._rd_scan_pos_out_ready = self.input(f"{self.rd_scan_pre}_pos_out_ready", 1)
        self._rd_scan_pos_out_ready.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
        self._rd_scan_pos_out_valid = self.output(f"{self.rd_scan_pre}_pos_out_valid", 1)
        self._rd_scan_pos_out_valid.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))
        # MO: Moved the above stuff up here b/c it is needed by VR FSM

        ###################
        # MO: Begin VR FSM
        ###################

        # Define useful constants
        self._S_level_0 = self.var("S_level_0", self.data_width + 1)
        self._S_level_1 = self.var("S_level_1", self.data_width + 1)
        self._S_level_2 = self.var("S_level_2", self.data_width + 1)
        self._done_token = self.var("done_token", self.data_width + 1)
        self._semi_done_token = self.var("semi_done_token", self.data_width + 1)
        self.wire(self._S_level_0, kts.concat(kts.const(1, 1), kts.const(0, 16)))
        self.wire(self._S_level_1, kts.concat(kts.const(1, 1), kts.const(0, 15), kts.const(1, 1)))
        self.wire(self._S_level_2, kts.concat(kts.const(1, 1), kts.const(0, 14), kts.const(1, 1), kts.const(0, 1)))
        self.wire(self._done_token, kts.concat(kts.const(1, 1), kts.const(0, 7), kts.const(1, 1), kts.const(0, 8)))
        self.wire(self._semi_done_token, kts.concat(kts.const(1, 1), kts.const(0, 11), kts.const(1, 1), kts.const(0, 4)))

        # Sticky flags for FSM logic
        self._input_row_fully_processed = self.var("input_row_fully_processed", 1)
        self._output_row_fully_accumulated = self.var("output_row_fully_accumulated", 1)
        self._output_matrix_fully_accumulated = self.var("output_matrix_fully_accumulated", 1)
        self._rs_has_prepped_ds_row = self.var("rs_has_prepped_ds_row", 1)
        self.wire(self._rs_has_prepped_ds_row, self.rd_scan.ports.rs_has_prepped_ds_row)

        # State definition
        self.vr_fsm = self.add_fsm("vr_seq", reset_high=False)
        START = self.vr_fsm.add_state("START")
        INIT_BLANK_SEND_S0 = self.vr_fsm.add_state("INIT_BLANK_SEND_S0")
        INIT_BLANK_SEND_DONE = self.vr_fsm.add_state("INIT_BLANK_SEND_DONE")
        ISSUE_READ_SEND_REF_CNT = self.vr_fsm.add_state("ISSUE_READ_SEND_REF_CNT")
        ISSUE_READ_SEND_S0 = self.vr_fsm.add_state("ISSUE_READ_SEND_S0")
        ISSUE_READ_SEND_DONE = self.vr_fsm.add_state("ISSUE_READ_SEND_DONE")
        PROCESS_ROW = self.vr_fsm.add_state("PROCESS_ROW")
        DS_READ_ROW = self.vr_fsm.add_state("DS_READ_ROW")

        self.vr_fsm.set_start_state(START)

        # Create FSM output wires
        self._vr_fsm_pos_to_read_scanner = self.var("vr_fsm_pos_to_read_scanner", self.data_width + 1, packed=True)
        self._vr_fsm_pos_to_read_scanner.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._vr_fsm_pos_valid_to_read_scanner = self.var("vr_fsm_pos_valid_to_read_scanner", 1)
        self._vr_fsm_pos_valid_to_read_scanner.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
        self._vr_fsm_init_blank_S0 = self.var("vr_fsm_init_blank_S0", 1)
        self._vr_fsm_init_blank_DONE = self.var("vr_fsm_init_blank_DONE", 1)

        # Bind FSM Outputs
        self.vr_fsm.output(self._vr_fsm_pos_to_read_scanner)
        self.vr_fsm.output(self._vr_fsm_pos_valid_to_read_scanner)
        self.vr_fsm.output(self._vr_fsm_init_blank_S0)
        self.vr_fsm.output(self._vr_fsm_init_blank_DONE)

        # To know when the DS has read all the results
        self._done_sent_to_ds = self.var("done_sent_to_ds", 1)
        self.wire(self._done_sent_to_ds, (self.rd_scan.ports.pos_out == self._done_token) & self.rd_scan.ports.pos_out_valid & self._rd_scan_pos_out_ready)
        self._done_sent_to_ds_d1 = self.var("done_sent_to_ds_d1", 1)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def delay_one_cycle_block(self):
            if ~self._rst_n:
                self._done_sent_to_ds_d1 = 0
            else:
                self._done_sent_to_ds_d1 = self._done_sent_to_ds

        self.add_always(delay_one_cycle_block)

        # Keep track of bottom-most 3 bits of highest stop token seen (useful for FSM logic)
        # self._highest_seen_stoken = self.var("highest_seen_stoken", self.data_width + 1)
        self._highest_seen_stoken = self.var("highest_seen_stoken", 3)

        self._is_stop_token = self.var("is_stop_token", 1)
        self.wire(self._is_stop_token, (self._wr_scan_data_in[self.data_width] == 1) & ~(self._wr_scan_data_in == self._done_token) & ~(self._wr_scan_data_in == self._semi_done_token))

        self._new_highest_stoken_seen = self.var("new_highest_stoken_seen", 1)
        self.wire(self._new_highest_stoken_seen, self._is_stop_token & (self._wr_scan_data_in[2, 0] > self._highest_seen_stoken) & self._wr_scan_data_in_valid & self._wr_scan_data_in_ready)

        @always_ff((posedge, "clk"), (negedge, "rst_n"))
        def highest_seen_stoken_block(self):
            if ~self._rst_n:
                self._highest_seen_stoken = self._S_level_0[2, 0]
            elif self._done_sent_to_ds:
                self._highest_seen_stoken = self._S_level_0[2, 0]
            elif self._new_highest_stoken_seen:
                self._highest_seen_stoken = self._wr_scan_data_in[2, 0]

        self.add_always(highest_seen_stoken_block)

        # Next State Logic
        START.next(INIT_BLANK_SEND_S0, self._vector_reduce_mode)
        START.next(START, None)

        INIT_BLANK_SEND_S0.next(INIT_BLANK_SEND_DONE, self._rd_scan_coord_out_ready)
        INIT_BLANK_SEND_S0.next(INIT_BLANK_SEND_S0, None)

        INIT_BLANK_SEND_DONE.next(PROCESS_ROW, self._rd_scan_coord_out_ready)
        INIT_BLANK_SEND_DONE.next(INIT_BLANK_SEND_DONE, None)

        PROCESS_ROW.next(ISSUE_READ_SEND_REF_CNT, self._input_row_fully_processed)
        PROCESS_ROW.next(PROCESS_ROW, None)

        ISSUE_READ_SEND_REF_CNT.next(ISSUE_READ_SEND_DONE, self._rd_scan_us_pos_in_ready & (~self._output_matrix_fully_accumulated | (self._output_matrix_fully_accumulated & ~(self._highest_seen_stoken > self._S_level_1[2, 0]))))
        ISSUE_READ_SEND_REF_CNT.next(ISSUE_READ_SEND_S0, self._rd_scan_us_pos_in_ready & self._output_matrix_fully_accumulated & (self._highest_seen_stoken > self._S_level_1[2, 0]))
        ISSUE_READ_SEND_REF_CNT.next(ISSUE_READ_SEND_REF_CNT, None)

        ISSUE_READ_SEND_S0.next(ISSUE_READ_SEND_DONE, self._rd_scan_us_pos_in_ready)
        ISSUE_READ_SEND_S0.next(ISSUE_READ_SEND_S0, None)

        ISSUE_READ_SEND_DONE.next(PROCESS_ROW, self._rd_scan_us_pos_in_ready & ~self._output_row_fully_accumulated)
        ISSUE_READ_SEND_DONE.next(DS_READ_ROW, self._rd_scan_us_pos_in_ready & self._output_row_fully_accumulated)
        ISSUE_READ_SEND_DONE.next(ISSUE_READ_SEND_DONE, None)

        DS_READ_ROW.next(INIT_BLANK_SEND_S0, self._rs_has_prepped_ds_row)
        DS_READ_ROW.next(DS_READ_ROW, None)

        # FSM Output logic
        START.output(self._vr_fsm_pos_to_read_scanner, kts.const(0, self.data_width + 1))
        START.output(self._vr_fsm_pos_valid_to_read_scanner, 0)
        START.output(self._vr_fsm_init_blank_S0, 0)
        START.output(self._vr_fsm_init_blank_DONE, 0)

        INIT_BLANK_SEND_S0.output(self._vr_fsm_pos_to_read_scanner, kts.const(0, self.data_width + 1))
        INIT_BLANK_SEND_S0.output(self._vr_fsm_pos_valid_to_read_scanner, 0)
        INIT_BLANK_SEND_S0.output(self._vr_fsm_init_blank_S0, 1)
        INIT_BLANK_SEND_S0.output(self._vr_fsm_init_blank_DONE, 0)

        INIT_BLANK_SEND_DONE.output(self._vr_fsm_pos_to_read_scanner, kts.const(0, self.data_width + 1))
        INIT_BLANK_SEND_DONE.output(self._vr_fsm_pos_valid_to_read_scanner, 0)
        INIT_BLANK_SEND_DONE.output(self._vr_fsm_init_blank_S0, 0)
        INIT_BLANK_SEND_DONE.output(self._vr_fsm_init_blank_DONE, 1)

        ISSUE_READ_SEND_REF_CNT.output(self._vr_fsm_pos_to_read_scanner, kts.const(0, self.data_width + 1))
        ISSUE_READ_SEND_REF_CNT.output(self._vr_fsm_pos_valid_to_read_scanner, 1)
        ISSUE_READ_SEND_REF_CNT.output(self._vr_fsm_init_blank_S0, 0)
        ISSUE_READ_SEND_REF_CNT.output(self._vr_fsm_init_blank_DONE, 0)

        ISSUE_READ_SEND_S0.output(self._vr_fsm_pos_to_read_scanner, self._S_level_0)
        ISSUE_READ_SEND_S0.output(self._vr_fsm_pos_valid_to_read_scanner, 1)
        ISSUE_READ_SEND_S0.output(self._vr_fsm_init_blank_S0, 0)
        ISSUE_READ_SEND_S0.output(self._vr_fsm_init_blank_DONE, 0)

        ISSUE_READ_SEND_DONE.output(self._vr_fsm_pos_to_read_scanner, self._done_token)
        ISSUE_READ_SEND_DONE.output(self._vr_fsm_pos_valid_to_read_scanner, 1)
        ISSUE_READ_SEND_DONE.output(self._vr_fsm_init_blank_S0, 0)
        ISSUE_READ_SEND_DONE.output(self._vr_fsm_init_blank_DONE, 0)

        PROCESS_ROW.output(self._vr_fsm_pos_to_read_scanner, kts.const(0, self.data_width + 1))
        PROCESS_ROW.output(self._vr_fsm_pos_valid_to_read_scanner, 0)
        PROCESS_ROW.output(self._vr_fsm_init_blank_S0, 0)
        PROCESS_ROW.output(self._vr_fsm_init_blank_DONE, 0)

        DS_READ_ROW.output(self._vr_fsm_pos_to_read_scanner, kts.const(0, self.data_width + 1))
        DS_READ_ROW.output(self._vr_fsm_pos_valid_to_read_scanner, 0)
        DS_READ_ROW.output(self._vr_fsm_init_blank_S0, 0)
        DS_READ_ROW.output(self._vr_fsm_init_blank_DONE, 0)

        # Realize FSM
        self.vr_fsm.realize()

        vr_fsm_current_state = self.vr_fsm.current_state
        vr_fsm_state_enum = vr_fsm_current_state.enum_type()

        # Sticky flags used for FSM logic
        input_row_fully_processed_sticky = sticky_flag(self, ((self._wr_scan_data_in == self._S_level_0) | (self._wr_scan_data_in == self._S_level_1) | (self._wr_scan_data_in == self._S_level_2)) & self._wr_scan_data_in_valid & self._wr_scan_data_in_ready,
                                    clear=(vr_fsm_current_state == vr_fsm_state_enum.ISSUE_READ_SEND_DONE), name="input_row_fully_processed_sticky")
        self.wire(self._input_row_fully_processed, input_row_fully_processed_sticky)

        output_row_fully_accumulated_sticky = sticky_flag(self, ((self._wr_scan_data_in == self._S_level_1) | (self._wr_scan_data_in == self._S_level_2)) & self._wr_scan_data_in_valid & self._wr_scan_data_in_ready,
                                    clear=(vr_fsm_current_state == vr_fsm_state_enum.INIT_BLANK_SEND_S0), name="output_row_fully_accumulated_sticky")
        self.wire(self._output_row_fully_accumulated, output_row_fully_accumulated_sticky)

        output_matrix_fully_accumulated_sticky = sticky_flag(self, (self._wr_scan_data_in == self._done_token) & self._wr_scan_data_in_valid & self._wr_scan_data_in_ready,
                                    clear=self._done_sent_to_ds_d1, name="output_matrix_fully_accumulated_sticky")
        self.wire(self._output_matrix_fully_accumulated, output_matrix_fully_accumulated_sticky)

        # Realize FSM once more
        self.vr_fsm.realize()

        ##################
        # MO: End VR FSM
        ##################

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
        self._rd_scan_coord_out_valid = self.output(f"{self.rd_scan_pre}_coord_out_valid", 1)
        self._rd_scan_coord_out_valid.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._rd_scan_us_pos_in = self.input(f"{self.rd_scan_pre}_us_pos_in", self.data_width + 1, packed=True)
        self._rd_scan_us_pos_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
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

        if not self.local_memory and self.tb_harness:
            # Create data from mem port and wire it in if the memory is remote
            self._data_from_mem = self.input(f"data_from_mem", self.mem_width, packed=True)
            self._data_from_mem.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            self.wire(self._data_from_mem, self.buffet.ports.data_from_mem)
            self._data_to_mem = self.output(f"data_to_mem", self.mem_width, packed=True)
            self._data_to_mem.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            self.wire(self._data_to_mem, self.buffet.ports.data_to_mem)
            self._addr_to_mem = self.output(f"addr_to_mem", self.mem_addr_width, packed=True)
            self._addr_to_mem.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            self.wire(self._addr_to_mem, self.buffet.ports.addr_to_mem)
            self._wen_to_mem = self.output(f"wen_to_mem", 1, packed=True)
            self._wen_to_mem.add_attribute(ControlSignalAttr(is_control=False))
            self.wire(self._wen_to_mem, self.buffet.ports.wen_to_mem)
            self._ren_to_mem = self.output(f"ren_to_mem", 1, packed=True)
            self._ren_to_mem.add_attribute(ControlSignalAttr(is_control=False))
            self.wire(self._ren_to_mem, self.buffet.ports.ren_to_mem)

        # Now wire everything; buffet to wr_scan
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

        if num_ports == 1:
            self.wire(self.buffet.ports.rd_ID_0, self.rd_scan.ports.ID_out_0)
            self.wire(self.buffet.ports.rd_ID_0_ready, self.rd_scan.ports.ID_out_0_ready)
            self.wire(self.buffet.ports.rd_ID_0_valid, self.rd_scan.ports.ID_out_0_valid)

        [self.wire(self.buffet.ports[f"rd_rsp_data_{i}"], self.rd_scan.ports[f"rd_rsp_data_in_{i}"]) for i in range(num_ports)]
        [self.wire(self.buffet.ports[f"rd_rsp_data_{i}_ready"], self.rd_scan.ports[f"rd_rsp_data_in_{i}_ready"]) for i in range(num_ports)]
        [self.wire(self.buffet.ports[f"rd_rsp_data_{i}_valid"], self.rd_scan.ports[f"rd_rsp_data_in_{i}_valid"]) for i in range(num_ports)]

        """
        # MO: Logic to drop highest order stop-token (remember, we are doing a reduction operation)
        self.wire(self._wr_scan_data_in, self.wr_scan.ports.data_in)
        self._vr_fsm_wr_scan_data_in_tmp = self.var("vr_fsm_wr_scan_data_in_tmp", self.data_width + 1)

        self._wr_scan_data_in_mux1_sel = kts.concat(self._wr_scan_data_in == self._S_level_2, self._wr_scan_data_in == self._S_level_1)
        wr_scan_data_in_comb_block = self.combinational()
        wr_scan_data_mux1 = wr_scan_data_in_comb_block.switch_(self._wr_scan_data_in_mux1_sel)
        wr_scan_data_mux1.case_(kts.const(0, 2), self._vr_fsm_wr_scan_data_in_tmp(self._wr_scan_data_in))
        wr_scan_data_mux1.case_(kts.const(1, 2), self._vr_fsm_wr_scan_data_in_tmp(self._S_level_0))
        wr_scan_data_mux1.case_(kts.const(2, 2), self._vr_fsm_wr_scan_data_in_tmp(self._S_level_1))
        wr_scan_data_mux1.case_(None, self._vr_fsm_wr_scan_data_in_tmp(self._wr_scan_data_in))
        """

        # self._vr_fsm_wr_scan_data_in = self.var("vr_fsm_wr_scan_data_in", self.data_width + 1)
        # self.wire(self._vr_fsm_wr_scan_data_in, kts.ternary((self._wr_scan_data_in == self._semi_done_token), self._done_token, self._wr_scan_data_in))

        # self.wire(self.wr_scan.ports.data_in, kts.ternary(self._vector_reduce_mode, self._vr_fsm_wr_scan_data_in, self._wr_scan_data_in))
        self.wire(self._vector_reduce_mode, self.wr_scan.ports.vector_reduce_mode)
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
        # self.wire(self._rd_scan_coord_out, kts.ternary(self._vr_fsm_init_blank_S0, self._S_level_0, self.rd_scan.ports.coord_out))
        self.wire(self._rd_scan_coord_out_valid, self.rd_scan.ports.coord_out_valid)
        # self.wire(self._rd_scan_coord_out_valid, kts.ternary(self._vr_fsm_init_blank_S0, kts.const(1, 1), self.rd_scan.ports.coord_out_valid))

        self.wire(self._rd_scan_pos_out, self.rd_scan.ports.pos_out)
        self.wire(self._rd_scan_pos_out_ready, self.rd_scan.ports.pos_out_ready)
        # self.wire(self._rd_scan_pos_out_valid, kts.ternary((self._vector_reduce_mode & (self.rd_scan.ports.pos_out == self._done_token)), (self._output_matrix_fully_accumulated & self.rd_scan.ports.pos_out_valid), self.rd_scan.ports.pos_out_valid))
        self.wire(self._rd_scan_pos_out_valid, self.rd_scan.ports.pos_out_valid)
        self.wire(self.rd_scan.ports.us_pos_in, kts.ternary(self._vector_reduce_mode, self._vr_fsm_pos_to_read_scanner, self._rd_scan_us_pos_in))
        self.wire(self.rd_scan.ports.us_pos_in_valid, kts.ternary(self._vector_reduce_mode, self._vr_fsm_pos_valid_to_read_scanner, self._rd_scan_us_pos_in_valid))

        self.wire(self._vr_fsm_pos_to_read_scanner, self.rd_scan.ports.pos_to_read_scanner_from_vr_fsm)
        # self.wire(self._vr_fsm_pos_valid_to_read_scanner, self.rd_scan.ports.pos_valid_to_read_scanner_from_vr_fsm)
        self.wire(self._vr_fsm_init_blank_S0, self.rd_scan.ports.vr_fsm_state_init_blank_S0)
        self.wire(self._vr_fsm_init_blank_DONE, self.rd_scan.ports.vr_fsm_state_init_blank_DONE)

        self.wire(self._vector_reduce_mode, self.rd_scan.ports.vector_reduce_mode)
        self.wire(self._output_row_fully_accumulated, self.rd_scan.ports.output_row_fully_accumulated)
        self.wire(self._output_matrix_fully_accumulated, self.rd_scan.ports.output_matrix_fully_accumulated)

        if self.add_clk_enable:
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))

        flush_port = None
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
        vr_mode = config_kwargs['vr_mode']

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
        config += [("vector_reduce_mode", vr_mode)]

        return config


if __name__ == "__main__":

    # Test local memory
    fiber_access_dut = FiberAccess(data_width=16,
                                   local_memory=True,
                                   tech_map=GF_Tech_Map(depth=512, width=64, dual_port=False),
                                   defer_fifos=False,
                                   add_flush=True,
                                   use_pipelined_scanner=True,
                                   fifo_depth=2,
                                   buffet_optimize_wide=True,
                                   perf_debug=False,
                                   mem_width=64,
                                   mem_depth=512,
                                   tb_harness=True)
    kts.verilog(fiber_access_dut, filename="fiber_access_standalone_local_memory.sv",
                optimize_if=False)

    fiber_access_dut = FiberAccess(data_width=16,
                                   local_memory=False,
                                   tech_map=GF_Tech_Map(depth=512, width=64, dual_port=False),
                                   defer_fifos=False,
                                   add_flush=True,
                                   use_pipelined_scanner=True,
                                   fifo_depth=2,
                                   buffet_optimize_wide=True,
                                   perf_debug=False,
                                   mem_width=64,
                                   mem_depth=512,
                                   tb_harness=True)
    kts.verilog(fiber_access_dut, filename="fiber_access_standalone_remote_memory.sv",
                optimize_if=False)
