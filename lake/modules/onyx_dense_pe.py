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
        # Dense mode for bypassing FIFOs
        #self._dense_mode = self.input("dense_mode", 1)
        #self._dense_mode.add_attribute(ConfigRegAttr("Dense mode to skip the registers"))
        # Sparse num inputs for rv/eos logic
        #self._sparse_num_inputs = self.input("sparse_num_inputs", 3)
        #self._sparse_num_inputs.add_attribute(ConfigRegAttr("Sparse num inputs for rv/eos logic"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        # Scanner interface will need
        # input data, input valid
        # output address, output valid
        self._data_in = []
        #self._data_in_valid_in = []
        #self._data_in_ready_out = []
        #self._data_in_eos_in = []

        for i in range(3):

            #tmp_data_in = self.input(f"data{i}", self.data_width + 1, packed=True)
            tmp_data_in = self.input(f"data{i}", self.data_width, packed=True)
            tmp_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
            # Mark as hybrid port to allow bypassing at the core combiner level
            tmp_data_in.add_attribute(HybridPortAddr())
            # self._data_in = self.input("data_in", self.data_width, size=2, explicit_array=True, packed=True)

            #tmp_data_in_valid_in = self.input(f"data{i}_valid", 1)
            #tmp_data_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))
            # self._valid_in = self.input("valid_in", 2)

            #tmp_data_in_ready_out = self.output(f"data{i}_ready", 1)
            #tmp_data_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))
            # self._ready_out = self.output("ready_out", 2)

            # tmp_data_in_eos_in = self.var(f"data{i}_eos", 1)
            # tmp_data_in_eos_in.add_attribute(ControlSignalAttr(is_control=True))
            # self.wire(tmp_data_in_eos_in, tmp_data_in[self.data_width])
            # self._eos_in = self.input("eos_in", 2)

            self._data_in.append(tmp_data_in)
            #self._data_in_valid_in.append(tmp_data_in_valid_in)
            #self._data_in_ready_out.append(tmp_data_in_ready_out)
            # self._data_in_eos_in.append(tmp_data_in_eos_in)

        # for i in range(3):

        # tmp_data_in = self.input(f"data2", self.data_width + 1, packed=True)
        # tmp_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        # self._data_in = self.input("data_in", self.data_width, size=2, explicit_array=True, packed=True)

        # tmp_data_in_valid_in = self.input(f"data2_valid", 1)
        # tmp_data_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))
        # self._valid_in = self.input("valid_in", 2)

        # tmp_data_in_ready_out = self.output(f"data2_ready", 1)
        # tmp_data_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))
        # self._ready_out = self.output("ready_out", 2)

        # tmp_data_in_eos_in = self.var(f"data2_eos", 1)
        # tmp_data_in_eos_in.add_attribute(ControlSignalAttr(is_control=True))
        # self.wire(tmp_data_in_eos_in, tmp_data_in[self.data_width])
        # self._eos_in = self.input("eos_in", 2)

        self._data_in.append(tmp_data_in)
        # self._data_in_eos_in.append(tmp_data_in_eos_in)

        self._bit_in = []
        # self._data_in_valid_in = []
        # self._data_in_ready_out = []
        # self._data_in_eos_in = []

        for i in range(3):

            tmp_data_in = self.input(f"bit{i}", 1)
            tmp_data_in.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))
            # self._data_in = self.input("data_in", self.data_width, size=2, explicit_array=True, packed=True)

            # tmp_data_in_valid_in = self.input(f"bit{i}_valid", 1)
            # tmp_data_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))

            # tmp_data_in_ready_out = self.output(f"bit{i}_ready", 1)
            # tmp_data_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

            self._bit_in.append(tmp_data_in)

        #self._data_out = self.output("res", self.data_width + 1, packed=True)
        self._data_out = self.output("res", self.data_width, packed=True)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))
        self._data_out.add_attribute(HybridPortAddr())

        #self._valid_out = self.output("res_valid", 1)
        #self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        #self._ready_in = self.input("res_ready", 1)
        #self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._data_out_p = self.output("res_p", 1)
        self._data_out_p.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        # if self.perf_debug:

        #     cyc_count = add_counter(self, "clock_cycle_count", 64, increment=self._clk & self._clk_en)

        #     # Start when any of the coord inputs is valid
        #     self._start_signal = sticky_flag(self, kts.concat((*[self._data_in_valid_in[i] for i in range(2)])).r_or(),
        #                                      name='start_indicator')
        #     self.add_performance_indicator(self._start_signal, edge='posedge', label='start', cycle_count=cyc_count)

        #     # End when we see DONE on the output coord
        #     self._done_signal = sticky_flag(self, (self._data_out == MemoryController.DONE_PROXY) &
        #                                             self._valid_out,
        #                                             name='done_indicator')
        #     self.add_performance_indicator(self._done_signal, edge='posedge', label='done', cycle_count=cyc_count)

# ==============================
# INPUT FIFO
# ==============================
        # self._infifo = []
        # self._infifo.append(RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos))
        # self._infifo.append(RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos))
        # self._infifo.append(RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos))
        # self._infifo[0].add_attribute(SharedFifoAttr(direction="IN"))
        # self._infifo[1].add_attribute(SharedFifoAttr(direction="IN"))
        # self._infifo[2].add_attribute(SharedFifoAttr(direction="IN"))

        # Ready is just a function of having room in the FIFO
        # self.wire(self._data_in_ready_out[0], kts.ternary(self._dense_mode, kts.const(1, 1), ~self._infifo[0].ports.full))
        # self.wire(self._data_in_ready_out[1], kts.ternary(self._dense_mode, kts.const(1, 1), ~self._infifo[1].ports.full))
        # self.wire(self._data_in_ready_out[2], kts.ternary(self._dense_mode, kts.const(1, 1), ~self._infifo[2].ports.full))

        # Convert to packed
        # self._infifo_in_packed = self.var("infifo_in_packed", self.data_width + 1, size=3, explicit_array=True, packed=True)
        # self._infifo_out_packed = self.var("infifo_out_packed", self.data_width + 1, size=3, explicit_array=True, packed=True)

        # self._infifo_out_eos = self.var("infifo_out_eos", 3)
        # self._infifo_out_valid = self.var("infifo_out_valid", 3)
        # self._infifo_out_data = self.var("infifo_out_data", self.data_width, size=3, explicit_array=True, packed=True)

        # indicate valid data as well
        # self.wire(self._infifo_in_packed[0][self.data_width], self._data_in_eos_in[0])
        # self.wire(self._infifo_in_packed[0], self._data_in[0])
        # self.wire(self._infifo_out_eos[0], self._infifo_out_packed[0][self.data_width])
        # self.wire(self._infifo_out_data[0], self._infifo_out_packed[0][self.data_width - 1, 0])

        # # self.wire(self._infifo_in_packed[1][self.data_width], self._data_in_eos_in[1])
        # self.wire(self._infifo_in_packed[1], self._data_in[1])
        # self.wire(self._infifo_out_eos[1], self._infifo_out_packed[1][self.data_width])
        # self.wire(self._infifo_out_data[1], self._infifo_out_packed[1][self.data_width - 1, 0])

        # self.wire(self._infifo_in_packed[2], self._data_in[2])
        # self.wire(self._infifo_out_eos[2], self._infifo_out_packed[2][self.data_width])
        # self.wire(self._infifo_out_data[2], self._infifo_out_packed[2][self.data_width - 1, 0])

        # # Push when there's incoming transaction and room to accept it
        # # Do this separate naming scheme so that the get_fifos function in the MemoryController
        # # base class will pick them up properly
        # self._infifo_push = [self.var(f"infifo_push_{i}", 1) for i in range(3)]
        # self.wire(self._infifo_push[0], self._data_in_valid_in[0])
        # self.wire(self._infifo_push[1], self._data_in_valid_in[1])
        # self.wire(self._infifo_push[2], self._data_in_valid_in[2])

        # # Pop when ready to accum more streams
        # self._infifo_pop = self.var("infifo_pop", 3)

        # for i in range(3):
        #     self.add_child(f"input_fifo_{i}",
        #                    self._infifo[i],
        #                    clk=self._gclk,
        #                    rst_n=self._rst_n,
        #                    clk_en=self._clk_en,
        #                    push=self._infifo_push[i],
        #                    pop=self._infifo_pop[i],
        #                    data_in=self._infifo_in_packed[i],
        #                    data_out=self._infifo_out_packed[i])

        # self.wire(self._infifo_out_valid[0], ~self._infifo[0].ports.empty)
        # self.wire(self._infifo_out_valid[1], ~self._infifo[1].ports.empty)
        # self.wire(self._infifo_out_valid[2], ~self._infifo[2].ports.empty)

# ==============================
# OUTPUT FIFO
# ==============================

        #self._data_to_fifo = self.var("data_to_fifo", self.data_width)
        self._pe_output = self.var("pe_output", self.data_width)
        #self._outfifo_in_eos = self.var("outfifo_in_eos", 1)

        #self._outfifo = RegFIFO(data_width=self.data_width + 1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        #self._outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

        # Convert to packed
        #self._outfifo_in_packed = self.var("outfifo_in_packed", self.data_width + 1, packed=True)
        #self._outfifo_out_packed = self.var("outfifo_out_packed", self.data_width + 1, packed=True)

        #self.wire(self._outfifo_in_packed[self.data_width], self._outfifo_in_eos)
        #self.wire(self._outfifo_in_packed[self.data_width - 1, 0], self._data_to_fifo)

        # self.wire(self._eos_out, self._outfifo_out_packed[self.data_width])
        #self.wire(self._data_out, kts.ternary(self._dense_mode, self._pe_output, self._outfifo_out_packed))
        self.wire(self._data_out, self._pe_output)

        # Push when there's incoming transaction and room to accept it
        #self._outfifo_push = self.var("outfifo_push", 1)

        # Pop when ready to accum more streams
        #self._outfifo_pop = self.var("outfifo_pop", 1)
        #self._outfifo_full = self.var("outfifo_full", 1)
        # self._outfifo_empty = self.var("outfifo_empty", 1)

        # self.add_child(f"output_fifo",
        #                self._outfifo,
        #                clk=self._gclk,
        #                rst_n=self._rst_n,
        #                clk_en=self._clk_en,
        #                push=self._outfifo_push,
        #                pop=self._outfifo_pop,
        #                data_in=self._outfifo_in_packed,
        #                data_out=self._outfifo_out_packed)

        #self.wire(self._valid_out, kts.ternary(self._dense_mode, kts.const(1, 1), ~self._outfifo.ports.empty))

        #self.wire(self._outfifo_pop, self._ready_in)
        #self.wire(self._outfifo_full, self._outfifo.ports.full)
        # self.wire(self._outfifo_empty, self._outfifo.ports.empty)

# =============================
# Instantiate actual PE
# =============================

        # self._execute_op = self.var("execute_op", 1)
        # self.wire(self._execute_op, (self._infifo_out_valid.r_and() & ~self._infifo_out_eos.r_or() & ~self._outfifo_full))

        # self._op = self.input("op", 1)
        # self._op.add_attribute(ConfigRegAttr("Operation"))

        self.my_alu = OnyxPEInterface(data_width=self.data_width,
                                      name_prefix=self.ext_pe_prefix,
                                      include_RO_cfg=self.pe_ro)

        # self._infifo_out_maybe = [self.var(f"infifo_out_maybe_{idx}", 1) for idx in range(3)]
        # [self.wire(self._infifo_out_maybe[idx], self._infifo_out_eos[idx] & self._infifo_out_valid[idx] &
        #     (self._infifo_out_data[idx][self.OPCODE_BT] == self.MAYBE_CODE)) for idx in range(3)]

        # Need active high reset for PE
        self._active_high_reset = kts.util.async_reset(~self._rst_n)

        self.add_child(f"onyxpeintf",
                       self.my_alu,
                       CLK=self._gclk,
                       clk_en=self._clk_en,
                       ASYNCRESET=self._active_high_reset,
                      #data0=kts.ternary(self._dense_mode,
                      #                   self._data_in[0][self.data_width - 1, 0],
                      #                   kts.ternary(self._infifo_out_maybe[0],
                      #                               0,
                      #                               self._infifo_out_data[0])),
                       data0 = self._data_in[0][self.data_width - 1, 0],
                      # data1=kts.ternary(self._dense_mode,
                      #                   self._data_in[1][self.data_width - 1, 0],
                      #                   kts.ternary(self._infifo_out_maybe[1],
                      #                               0,
                      #                               self._infifo_out_data[1])),
                       data1 = self._data_in[1][self.data_width - 1, 0],
                      # data2=kts.ternary(self._dense_mode,
                      #                   self._data_in[2][self.data_width - 1, 0],
                      #                   kts.ternary(self._infifo_out_maybe[2],
                      #                               0,
                      #                               self._infifo_out_data[2])),
                       data2 = self._data_in[2][self.data_width - 1, 0],
                       bit0=self._bit_in[0],
                       bit1=self._bit_in[1],
                       bit2=self._bit_in[2],
                       O0=self._pe_output,
                       O1=self._data_out_p)

        # @always_comb
        # def fifo_push():
        #     self._outfifo_push = 0
        #     self._outfifo_in_eos = 0
        #     self._data_to_fifo = 0
        #     self._infifo_pop[0] = 0
        #     self._infifo_pop[1] = 0
        #     self._infifo_pop[2] = 0
        #     # TODO Fix comment If both inputs are valid, then we either can perform the op, otherwise we push through EOS
        #     if ((self._infifo_out_valid & self._sparse_num_inputs) == self._sparse_num_inputs) & ~self._outfifo_full & ~self._dense_mode:
        #         # if eos's are low, we push through pe output, otherwise we push through the input data (streams are aligned)
        #         if ~((self._infifo_out_eos & self._sparse_num_inputs) == self._sparse_num_inputs):
        #             self._outfifo_push = 1
        #             self._outfifo_in_eos = 0
        #             self._data_to_fifo = self._pe_output
        #             self._infifo_pop[0] = self._infifo_out_valid[0] & self._sparse_num_inputs[0]
        #             self._infifo_pop[1] = self._infifo_out_valid[1] & self._sparse_num_inputs[1]
        #             self._infifo_pop[2] = self._infifo_out_valid[2] & self._sparse_num_inputs[2]
        #         else:
        #             self._outfifo_push = 1
        #             self._outfifo_in_eos = 1
        #             # TODO what if stream not on first input
        #             self._data_to_fifo = kts.ternary(self._sparse_num_inputs[0], self._infifo_out_data[0], kts.ternary(self._sparse_num_inputs[1], self._infifo_out_data[1], self._infifo_out_data[2]))
        #             self._infifo_pop[0] = self._infifo_out_valid[0] & self._sparse_num_inputs[0]
        #             self._infifo_pop[1] = self._infifo_out_valid[1] & self._sparse_num_inputs[1]
        #             self._infifo_pop[2] = self._infifo_out_valid[2] & self._sparse_num_inputs[2]

        # self.add_code(fifo_push)

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
        override_dense = False
        # op_sparse_num_inputs_mapping = {
        #     0: 0b011,
        #     1: 0b011,
        #     2: 0b011,
        #     3: 0b010,
        #     4: 0b010,
        #     5: 0b011,
        #     6: 0b011,
        #     7: 0b001,
        #     8: 0b001,
        #     9: 0b011,
        #     10: 0b011,
        #     11: 0b011,
        # }
        # sparse_num_inputs = 0b000
        # if 'use_dense' in config_kwargs and config_kwargs['use_dense'] is True:
        #     override_dense = True
        #     config += [("dense_mode", 1)]
        # elif 'pe_connected_to_reduce' in config_kwargs and config_kwargs['pe_connected_to_reduce'] is True:
        #     # If this flag is set, we want to connect the pe to reduce
        #     # We want to bypass the fifos
        #     config += [("dense_mode", 1)]
        #     # But we are still not explicitly supplying the opcode to pe
        #     # Instead, we are still relying on the assembler to decode the opcode
        #     override_dense = False
        # else:
        #     # we are deploying the pe in sparse mode
        #     sparse_num_inputs = op_sparse_num_inputs_mapping[op]
        #     if config_kwargs["rb_const"] is not None:
        #         # the b operand is a constant
        #         # support constant operand for and and fp_mul for now
        #         assert op == 5 or op == 6
        #         # only accepting one input from port a
        #         sparse_num_inputs = 0b001
        # config += [('sparse_num_inputs', sparse_num_inputs)]

        only_dense_hw = True
        sub_config = self.my_alu.get_bitstream(op=op, override_dense=override_dense, config_kwargs=config_kwargs, only_dense_hw=only_dense_hw)
        for config_tuple in sub_config:
            config_name, config_value = config_tuple
            config += [(f"{self.my_alu.instance_name}_{config_name}", config_value)]

        return config


if __name__ == "__main__":

    pe_dut = OnyxDensePE(data_width=16, defer_fifos=False, do_config_lift=False)

    # Lift config regs and generate annotation
    # lift_config_reg(pond_dut.internal_generator)
    # extract_formal_annotation(pond_dut, "pond.txt")

    verilog(pe_dut, filename="onyxdensepe.sv",
            optimize_if=False)