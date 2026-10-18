import kratos as kts
from kratos import *
from lake.attributes.shared_fifo_attr import SharedFifoAttr
from lake.modules.onyx_pe_intf import OnyxPEInterface
from lake.passes.passes import lift_config_reg
from lake.top.memory_controller import MemoryController
from lake.attributes.config_reg_attr import ConfigRegAttr
from lake.attributes.control_signal_attr import ControlSignalAttr
from lake.modules.reg_fifo import RegFIFO


class OnyxDenseRVPE(MemoryController):
    '''
    The PE controller of a dense ready-valid CGRA: OnyxPE without what only sparse
    or static operation needs.

    - Data ports are data_width bits: no end-of-stream bit (OnyxPE's data_width + 1).
    - No static mode (OnyxPE bypass_rv_mode) and no sparse maybe/EOS handling.
    - Ports are not hybrid: in the tile every data and bit port is a ready-valid port
      with its tile fifo and no bypass.

    Kept from OnyxPE: the RV synchronization (active input/output encodings, constant
    PE), the primitive output fifo with its bypass_prim_outfifo config bit (a latency
    balancing knob, not a static bypass), and the 1-bit output fifo.
    Config mode "alu", as OnyxPE, so the CGRA's PE configuration is unchanged.
    '''

    # The tile connects the 1-bit ports (bit0-2, res_p) to the ready-valid
    # interconnect too (MemoryTileBuilder)
    rv_1bit_ports = True

    def __init__(self,
                 data_width=16,
                 fifo_depth=8,
                 defer_fifos=True,
                 ext_pe_prefix="PG_",
                 pe_ro=True,
                 do_config_lift=False,
                 add_flush=False):

        super().__init__("PE_onyx", debug=True)

        self.data_width = data_width
        self.add_clk_enable = True
        self.add_flush = add_flush
        self.fifo_depth = fifo_depth
        self.defer_fifos = defer_fifos
        self.ext_pe_prefix = ext_pe_prefix
        self.pe_ro = pe_ro
        self.do_config_lift = do_config_lift

        # For consistency with Core wrapper in garnet...
        self.total_sets = 0

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")
        self._clk_en = self.clock_en("clk_en", 1)

        # Enable/Disable tile
        self._tile_en = self.input("tile_en", 1)
        self._tile_en.add_attribute(ConfigRegAttr("Tile logic enable manifested as clock gate"))

        # Active inputs/outputs encoding for RV synchronization
        self._active_inputs_encoding = self.input("active_inputs_encoding", 3)
        self._active_inputs_encoding.add_attribute(ConfigRegAttr("Active inputs encoding for RV synchronization"))

        self._active_bit_inputs_encoding = self.input("active_bit_inputs_encoding", 3)
        self._active_bit_inputs_encoding.add_attribute(ConfigRegAttr("Active bit inputs encoding for RV synchronization"))

        self._active_16b_output = self.input("active_16b_output", 1)
        self._active_16b_output.add_attribute(ConfigRegAttr("Is the data output active? For RV synchronization"))

        self._active_1b_output = self.input("active_1b_output", 1)
        self._active_1b_output.add_attribute(ConfigRegAttr("Is the 1b output active? For RV synchronization"))

        # Is this a constant PE?
        self._is_constant_pe = self.input("is_constant_pe", 1)
        self._is_constant_pe.add_attribute(ConfigRegAttr("Is this PE outputting a constant value? (i.e. no inputs)"))

        # Bypass ONLY the primitive output fifo (one pipeline stage less at this output)
        self._bypass_prim_outfifo = self.input("bypass_prim_outfifo", 1)
        self._bypass_prim_outfifo.add_attribute(ConfigRegAttr("mode to skip the output primitive FIFO"))

        gclk = self.var("gclk", 1)
        self._gclk = kts.util.clock(gclk)
        self.wire(gclk, kts.util.clock(self._clk & self._tile_en))

        self._data_in = []
        self._data_in_valid_in = []
        self._data_in_ready_out = []
        for i in range(3):
            tmp_data_in = self.input(f"data{i}", self.data_width, packed=True)
            tmp_data_in.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

            tmp_data_in_valid_in = self.input(f"data{i}_valid", 1)
            tmp_data_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))

            tmp_data_in_ready_out = self.output(f"data{i}_ready", 1)
            tmp_data_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

            self._data_in.append(tmp_data_in)
            self._data_in_valid_in.append(tmp_data_in_valid_in)
            self._data_in_ready_out.append(tmp_data_in_ready_out)

        self._bit_in = []
        self._bit_in_valid_in = []
        self._bit_in_ready_out = []
        for i in range(3):
            tmp_bit_in = self.input(f"bit{i}", 1)
            tmp_bit_in.add_attribute(ControlSignalAttr(is_control=True, full_bus=False))

            tmp_bit_in_valid_in = self.input(f"bit{i}_valid", 1)
            tmp_bit_in_valid_in.add_attribute(ControlSignalAttr(is_control=True))

            tmp_bit_in_ready_out = self.output(f"bit{i}_ready", 1)
            tmp_bit_in_ready_out.add_attribute(ControlSignalAttr(is_control=False))

            self._bit_in.append(tmp_bit_in)
            self._bit_in_valid_in.append(tmp_bit_in_valid_in)
            self._bit_in_ready_out.append(tmp_bit_in_ready_out)

        self._data_out = self.output("res", self.data_width, packed=True)
        self._data_out.add_attribute(ControlSignalAttr(is_control=False, full_bus=True))

        self._valid_out = self.output("res_valid", 1)
        self._valid_out.add_attribute(ControlSignalAttr(is_control=False))

        self._ready_in = self.input("res_ready", 1)
        self._ready_in.add_attribute(ControlSignalAttr(is_control=True))

        self._data_out_p = self.output("res_p", 1)
        self._data_out_p.add_attribute(ControlSignalAttr(is_control=False, full_bus=False))

        self._valid_bit_out = self.output("res_p_valid", 1)
        self._valid_bit_out.add_attribute(ControlSignalAttr(is_control=False))

        self._ready_bit_in = self.input("res_p_ready", 1)
        self._ready_bit_in.add_attribute(ControlSignalAttr(is_control=True))

        self.all_active_inputs_valid = self.var("all_active_inputs_valid", 1)
        self.all_active_bit_inputs_valid = self.var("all_active_bit_inputs_valid", 1)
        self.output_16b_ready = self.var("output_16b_ready", 1)
        self.output_rv_transaction_ack = self.var("output_rv_transaction_ack", 1)

# ==============================
# INPUT FIFO (shared: the tile fifo; depth 0 here once the tile is built)
# ==============================
        self._infifo = []
        self._infifo_bit = []
        for i in range(3):
            self._infifo.append(RegFIFO(data_width=self.data_width, width_mult=1, depth=self.fifo_depth,
                                        defer_hrdwr_gen=self.defer_fifos))
            self._infifo[i].add_attribute(SharedFifoAttr(direction="IN"))
            self._infifo_bit.append(RegFIFO(data_width=1, width_mult=1, depth=self.fifo_depth,
                                            defer_hrdwr_gen=self.defer_fifos))
            self._infifo_bit[i].add_attribute(SharedFifoAttr(direction="IN"))

        # Ready is just a function of having room in the FIFO
        for i in range(3):
            self.wire(self._data_in_ready_out[i], ~self._infifo[i].ports.full)
            self.wire(self._bit_in_ready_out[i], ~self._infifo_bit[i].ports.full)

        self._infifo_out_data = self.var("infifo_out_data", self.data_width, size=3, explicit_array=True, packed=True)
        self._infifo_out_valid = self.var("infifo_out_valid", 3)
        # The 1-bit fifos take packed data
        self._infifo_bit_in_packed = self.var("infifo_bit_in_packed", 1, size=3, explicit_array=True, packed=True)
        self._infifo_bit_out_data = self.var("infifo_bit_out_data", 1, size=3, explicit_array=True, packed=True)
        self._infifo_bit_out_valid = self.var("infifo_bit_out_valid", 3)
        for i in range(3):
            self.wire(self._infifo_bit_in_packed[i], self._bit_in[i])

        # Separate push names so MemoryController.get_fifos picks the fifos up
        self._infifo_push = [self.var(f"infifo_push_{i}", 1) for i in range(3)]
        self._infifo_bit_push = [self.var(f"infifo_bit_push_{i}", 1) for i in range(3)]
        for i in range(3):
            self.wire(self._infifo_push[i], self._data_in_valid_in[i])
            self.wire(self._infifo_bit_push[i], self._bit_in_valid_in[i])

        self._infifo_pop = self.var("infifo_pop", 3)
        self._infifo_bit_pop = self.var("infifo_bit_pop", 3)

        for i in range(3):
            self.add_child(f"input_fifo_{i}",
                           self._infifo[i],
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           clk_en=self._clk_en,
                           push=self._infifo_push[i],
                           pop=self._infifo_pop[i],
                           data_in=self._data_in[i],
                           data_out=self._infifo_out_data[i])
            self.wire(self._infifo_out_valid[i], ~self._infifo[i].ports.empty)

            self.add_child(f"input_bit_fifo_{i}",
                           self._infifo_bit[i],
                           clk=self._gclk,
                           rst_n=self._rst_n,
                           clk_en=self._clk_en,
                           push=self._infifo_bit_push[i],
                           pop=self._infifo_bit_pop[i],
                           data_in=self._infifo_bit_in_packed[i],
                           data_out=self._infifo_bit_out_data[i])
            self.wire(self._infifo_bit_out_valid[i], ~self._infifo_bit[i].ports.empty)

# ==============================
# OUTPUT FIFO (primitive output fifo, bypassable by bypass_prim_outfifo)
# ==============================
        self._data_to_fifo = self.var("data_to_fifo", self.data_width, packed=True)
        self._pe_output = self.var("pe_output", self.data_width)

        self._outfifo = RegFIFO(data_width=self.data_width, width_mult=1, depth=self.fifo_depth,
                                defer_hrdwr_gen=self.defer_fifos)
        self._outfifo.add_attribute(SharedFifoAttr(direction="OUT"))

        self._bit_to_fifo = self.var("bit_to_fifo", 1)
        self._pe_bit_output = self.var("pe_bit_output", 1)

        self._outfifo_bit = RegFIFO(1, width_mult=1, depth=self.fifo_depth, defer_hrdwr_gen=self.defer_fifos)
        self._outfifo_bit.add_attribute(SharedFifoAttr(direction="OUT"))

        self._outfifo_out = self.var("outfifo_out", self.data_width, packed=True)
        self._outfifo_bit_in = self.var("outfifo_bit_in", 1, packed=True)
        self._outfifo_bit_out = self.var("outfifo_bit_out", 1, packed=True)
        self.wire(self._outfifo_bit_in, self._bit_to_fifo)

        self.wire(self._data_out, kts.ternary(self._bypass_prim_outfifo, self._pe_output, self._outfifo_out))
        self.wire(self._data_out_p, self._outfifo_bit_out)

        self._outfifo_push = self.var("outfifo_push", 1)
        self._outfifo_bit_push = self.var("outfifo_bit_push", 1)
        self._outfifo_pop = self.var("outfifo_pop", 1)
        self._outfifo_full = self.var("outfifo_full", 1)
        self._outfifo_bit_pop = self.var("outfifo_bit_pop", 1)
        self._outfifo_bit_full = self.var("outfifo_bit_full", 1)

        self.add_child(f"output_fifo",
                       self._outfifo,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._outfifo_push,
                       pop=self._outfifo_pop,
                       data_in=self._data_to_fifo,
                       data_out=self._outfifo_out)

        self.wire(self._valid_out, kts.ternary(self._bypass_prim_outfifo, self._outfifo_push, ~self._outfifo.ports.empty))
        self.wire(self._outfifo_pop, self._ready_in)
        self.wire(self._outfifo_full, self._outfifo.ports.full)

        self.add_child(f"output_bit_fifo",
                       self._outfifo_bit,
                       clk=self._gclk,
                       rst_n=self._rst_n,
                       clk_en=self._clk_en,
                       push=self._outfifo_bit_push,
                       pop=self._outfifo_bit_pop,
                       data_in=self._outfifo_bit_in,
                       data_out=self._outfifo_bit_out)

        self.wire(self._valid_bit_out, ~self._outfifo_bit.ports.empty)
        self.wire(self._outfifo_bit_pop, self._ready_bit_in)
        self.wire(self._outfifo_bit_full, self._outfifo_bit.ports.full)

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
                       data0=self._infifo_out_data[0],
                       data1=self._infifo_out_data[1],
                       data2=self._infifo_out_data[2],
                       bit0=self._infifo_bit_out_data[0],
                       bit1=self._infifo_bit_out_data[1],
                       bit2=self._infifo_bit_out_data[2],
                       O0=self._pe_output,
                       O1=self._pe_bit_output)

        @always_comb
        def fifo_push():
            self.all_active_inputs_valid = ((self._infifo_out_valid & self._active_inputs_encoding) == self._active_inputs_encoding)
            self.all_active_bit_inputs_valid = ((self._infifo_bit_out_valid & self._active_bit_inputs_encoding) == self._active_bit_inputs_encoding)
            self.output_16b_ready = kts.ternary(self._bypass_prim_outfifo, kts.const(1, 1), ~self._outfifo_full)

            self._outfifo_push = 0
            self._data_to_fifo = 0
            self._infifo_pop[0] = 0
            self._infifo_pop[1] = 0
            self._infifo_pop[2] = 0
            self.output_rv_transaction_ack = 0

            self._outfifo_bit_push = 0
            self._bit_to_fifo = 0
            self._infifo_bit_pop[0] = 0
            self._infifo_bit_pop[1] = 0
            self._infifo_bit_pop[2] = 0

            # A constant PE pushes its constant to the output fifo indefinitely
            if self._is_constant_pe:
                self._outfifo_push = self._active_16b_output
                self._data_to_fifo = self._pe_output

                self._outfifo_bit_push = self._active_1b_output
                self._bit_to_fifo = self._pe_bit_output

            # Fire when every active input holds a token and every active output has room
            elif (self.all_active_inputs_valid & self.output_16b_ready &
                    self.all_active_bit_inputs_valid & ~self._outfifo_bit_full):
                self.output_rv_transaction_ack = kts.ternary(self._bypass_prim_outfifo, self._ready_in, ~self._outfifo_full)

                self._outfifo_push = self._active_16b_output
                self._data_to_fifo = self._pe_output
                self._infifo_pop[0] = kts.ternary(self._bypass_prim_outfifo, self._infifo_out_valid[0] & self._active_inputs_encoding[0] & self.output_rv_transaction_ack, self._infifo_out_valid[0] & self._active_inputs_encoding[0])
                self._infifo_pop[1] = kts.ternary(self._bypass_prim_outfifo, self._infifo_out_valid[1] & self._active_inputs_encoding[1] & self.output_rv_transaction_ack, self._infifo_out_valid[1] & self._active_inputs_encoding[1])
                self._infifo_pop[2] = kts.ternary(self._bypass_prim_outfifo, self._infifo_out_valid[2] & self._active_inputs_encoding[2] & self.output_rv_transaction_ack, self._infifo_out_valid[2] & self._active_inputs_encoding[2])

                self._outfifo_bit_push = self._active_1b_output
                self._bit_to_fifo = self._pe_bit_output
                self._infifo_bit_pop[0] = self._infifo_bit_out_valid[0] & self._active_bit_inputs_encoding[0]
                self._infifo_bit_pop[1] = self._infifo_bit_out_valid[1] & self._active_bit_inputs_encoding[1]
                self._infifo_bit_pop[2] = self._infifo_bit_out_valid[2] & self._active_bit_inputs_encoding[2]

        self.add_code(fifo_push)

        if self.add_clk_enable:
            kts.passes.auto_insert_clock_enable(self.internal_generator)
            clk_en_port = self.internal_generator.get_port("clk_en")
            clk_en_port.add_attribute(ControlSignalAttr(False))

        if self.add_flush:
            self.add_attribute("sync-reset=flush")
            kts.passes.auto_insert_sync_reset(self.internal_generator)
            flush_port = self.internal_generator.get_port("flush")
            flush_port.add_attribute(ControlSignalAttr(True))

        if self.do_config_lift:
            lift_config_reg(self.internal_generator)

    def get_memory_ports(self):
        return [[None]]

    def get_config_mode_str(self):
        return "alu"

    def get_bitstream(self, config_kwargs):
        '''
        Same configuration keys as OnyxPE.get_bitstream. A static app has no target
        here: bypass_rv=True is rejected.
        '''
        assert not config_kwargs.get('bypass_rv', False), \
            "the dense ready-valid PE has no static mode (bypass_rv)"
        config = [("tile_en", 1)]
        if 'active_inputs' in config_kwargs:
            active_inputs_encoding = config_kwargs['active_inputs']
        else:
            active_inputs_encoding = config_kwargs.get('num_sparse_inputs', 0)
        config += [('active_inputs_encoding', active_inputs_encoding),
                   ('active_bit_inputs_encoding', config_kwargs.get('active_bit_inputs', 0)),
                   ('active_16b_output', config_kwargs.get('active_16b_output', 0)),
                   ('active_1b_output', config_kwargs.get('active_1b_output', 0)),
                   ('is_constant_pe', config_kwargs.get('is_constant_pe', 0)),
                   ('bypass_prim_outfifo', config_kwargs.get('bypass_prim_outfifo', 0))]

        sub_config = self.my_alu.get_bitstream(op=config_kwargs['op'])
        for config_name, config_value in sub_config:
            config += [(f"{self.my_alu.instance_name}_{config_name}", config_value)]
        return config


if __name__ == "__main__":
    pe_dut = OnyxDenseRVPE(data_width=16, defer_fifos=False, do_config_lift=False)
    verilog(pe_dut, filename="onyx_dense_rv_pe.sv", optimize_if=False)
