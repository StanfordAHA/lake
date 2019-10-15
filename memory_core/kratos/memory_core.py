
from lake.modules.sram_stub import SRAMStub
from lake.modules.two_port_sram_stub import TwoPortSRAMStub
from lake.modules.pipe_reg import PipeReg
from lake.modules.virtual_remap_table import VirtualRemapTable
from lake.modules.linebuffer_control import LineBufferControl
from lake.modules.fifo_control import FIFOControl
from lake.modules.sram_control import SRAMControl
from lake.modules.doublebuffer_control import DoubleBufferControl
from kratos import *

class MemoryCore(Generator):
    def __init__(self,
                data_width,
                mem_width,
                mem_depth,
                banks,
                iterator_support,
                use_sram_stub):

        super().__init__("memory_core")

        self.data_width = data_width
        self.mem_width = mem_width
        self.mem_depth = mem_depth
        self.banks = banks
        self.iterator_support = iterator_support
        self.use_sram_stub = use_sram_stub

        ##### PORT DEFS: begin
        self._clk = self.clock("clk")
        self._reset = self.reset("reset")

        self._flush = self.input("flush", 1)
        self._clk_en = self.input("clk_en", 1)

        self.full_addr = clog2(self.banks) + clog2(self.mem_depth)
        self.addr_width_macro = clog2(self.mem_depth)

        self._addr_in = self.input("addr_in", self.full_addr)
        self._data_in = self.input("data_in", self.data_width)
        self._data_out = self.output("data_out", self.data_width)
        self._wen_in = self.input("wen_in", 1)
        self._ren_in = self.input("ren_in", 1)
        self._valid_out = self.output("valid_out", 1)

        self._chain_in = self.input("chain_in", self.data_width)
        self._chain_out = self.output("chain_out", self.data_width)
        self._chain_wen_in = self.input("chain_wen_in", 1)
        self._chain_valid_out = self.output("chain_valid_out", 1)

        self._full = self.output("full", 1)
        self._empty = self.output("empty", 1)
        self._almost_full = self.output("almost_full", 1)
        self._almost_empty = self.output("almost_empty", 1)

        self._switch_db = self.input("switch_db", 1)
        self._rate_matched = self.input("rate_matched", 1)

        self._config_addr = self.input("config_addr", 32)
        self._config_data = self.input("config_data", 32)
        self._config_read = self.input("config_read", 1)
        self._config_write = self.input("config_write", 1)

        self.sram_features = (self.mem_depth * self.banks)//256

        self._config_en_sram = self.input("config_en_sram", self.sram_features)

        for i in range(self.sram_features):
            self.output(f"read_data_sram_{i}", 32)

        self._read_config_data = self.output("read_config_data", 32)

        ### Config Regs
        self._stencil_width = self.input("stencil_width", 16)
        self._arbitrary_addr = self.input("arbitrary_addr", 1)
        self._starting_addr = self.input("starting_addr", 16)
        self._iter_cnt = self.input("iter_cnt", 32)
        self._dimensionality = self.input("dimensionality", 4)

        for i in range(self.iterator_support):
            self.input(f"stride_{i}", 16)
            self.input(f"range_{i}", 32)

        self._circular_en = self.input("circular_en", 1)
        self._almost_count = self.input("almost_count", 4)
        self._enable_chain = self.input("enable_chain", 1)
        self._mode = self.input("mode", 2)
        self._tile_en = self.input("tile_en", 1)
        self._chain_idx = self.input("chain_idx", 4)
        self._depth = self.input("depth", 16)
        ###

        ##### PORT DEFS: end

        ##### LOCAL SIGNALS: begin

        ### Linebuffer Signals
        self._lb_addr = self.var("lb_addr", self.addr_width_macro, size=self.banks, explicit_array=True, packed=True)
        self._lb_mem_data_out = self.var("lb_mem_data_out", self.mem_width, size=self.banks, explicit_array=True, packed=True)
        self._lb_wen = self.var("lb_wen", self.banks)
        self._lb_cen = self.var("lb_cen", self.banks)
        self._lb_valid_out = self.var("lb_valid_out", 1)
        self._ren_lb_to_fifo = self.var("ren_lb_to_fifo", 1)
        ### Fifo Signals
        self._fifo_addr = self.var("fifo_addr", self.addr_width_macro, size=self.banks, explicit_array=True, packed=True)
        self._fifo_mem_data_out = self.var("fifo_mem_data_out", self.mem_width, size=self.banks, explicit_array=True, packed=True)
        self._fifo_out = self.var("fifo_out", self.mem_width)
        self._fifo_wen = self.var("fifo_wen", self.banks)
        self._fifo_cen = self.var("fifo_cen", self.banks)
        self._fifo_almost_full = self.var("fifo_almost_full", 1)
        self._fifo_almost_empty = self.var("fifo_almost_empty", 1)
        self._fifo_valid_out = self.var("fifo_valid_out", 1)
        self._fifo_full = self.var("fifo_full", 1)
        self._fifo_empty = self.var("fifo_empty", 1)
        self._num_words_mem_fifo_to_lb = self.var("num_words_mem_fifo_to_lb", 16)
        ### SRAM Signals
        self._sram_addr = self.var("sram_addr", self.addr_width_macro, size=self.banks, explicit_array=True, packed=True)
        self._sram_mem_data_out = self.var("sram_mem_data_out", self.mem_width, size=self.banks, explicit_array=True, packed=True)
        self._sram_out = self.var("sram_out", self.mem_width)
        self._sram_wen = self.var("sram_wen", self.banks)
        self._sram_cen = self.var("sram_cen", self.banks)
        ### UB Signals
        self._db_addr = self.var("db_addr", self.addr_width_macro, size=self.banks, explicit_array=True, packed=True)
        self._db_mem_data_out = self.var("db_mem_data_out", self.mem_width, size=self.banks, explicit_array=True, packed=True)
        self._db_out = self.var("db_out", self.mem_width)
        self._db_wen = self.var("db_wen", self.banks)
        self._db_cen = self.var("db_cen", self.banks)
        self._db_valid_out = self.var("db_valid_out", 1)

        ### Muxed to mem signals
        self._mem_data_out = self.var("mem_data_out", self.mem_width, size=self.banks, explicit_array=True, packed=True)
        self._mem_data_in = self.var("mem_data_in", self.mem_width, size=self.banks, explicit_array=True, packed=True)
        self._mem_addr = self.var("mem_addr", self.addr_width_macro, size=self.banks, explicit_array=True, packed=True)
        self._mem_ren = self.var("mem_ren", self.banks)
        self._mem_wen = self.var("mem_wen", self.banks)
        self._mem_cen = self.var("mem_cen", self.banks)
        self._mem_cen_int = self.var("mem_cen_int", self.banks)

        ### Various signals
        self._stride = self.var("stride", 16, size=self.iterator_support, explicit_array=True, packed=True)
        self._range = self.var("range", 32, size=self.iterator_support, explicit_array=True, packed=True)

        self._data_in_int = self.var("data_in_int", self.mem_width)
        self._wen_in_int = self.var("wen_in_int", 1)

        self._gclk_in = self.var("gclk_in", 1)

        self.bank_addr_width = max(clog2(self.banks), 1)
        self._sram_sel = self.var("sram_sel", self.bank_addr_width)
        ##### LOCAL_SIGNALS: end

        ##### GENERATION LOGIC: begin
        self.add_code(self.sram_sel_reg)

        for i in range(self.iterator_support):
            self.wire(self._stride[i], self.ports[f"stride_{i}"])
            self.wire(self._range[i], self.ports[f"range_{i}"])

        self.wire(self._gclk_in, self._tile_en & self._clk)
        self.wire(self._read_config_data, zext(self._mem_data_out[self._sram_sel], self._read_config_data.width))

        ## Chaining logic
        self.wire(self._data_in_int, ternary(self._enable_chain, self._chain_in, self._data_in), comment_str="Choose between the data in and chained data")
        self.wire(self._wen_in_int, ternary(self._enable_chain, self._chain_wen_in, self._wen_in))
        self.wire(self._chain_out, ternary(self._enable_chain & self._chain_wen_in, self._chain_in, self._data_out))
        self.wire(self._chain_valid_out, (self._enable_chain & self._chain_wen_in) | self._valid_out)

        self.sram_reads_per_bank = self.mem_depth / 256

        for i in range(self.banks):
            self.wire(self._mem_cen[i], (self._mem_cen_int[i] & (self._mem_wen[i] | self._mem_ren[i]) & (self._clk_en | self._config_en_sram.r_or())))

        # Get the output for read_data_sram
        j = 0
        k = 0
        for i in range(self.sram_features):
            self.wire(self.ports[f"read_data_sram_{i}"], zext(self._mem_data_out[k], 32))
            j = (j + 1) % self.sram_reads_per_bank
            if j == 0:
                k = k + 1

        # Mux the different signals to the sram interface
        self.add_code(self.mux_sram_signals)

        ### Finally plop down all the controllers
        self.instantiate_lb()
        self.instantiate_fifo()
        self.instantiate_sram()
        self.instantiate_db()
        self.instantiate_memory()
        ##### GENERATION LOGIC: end

    @always((posedge, "clk"), (posedge, "reset"))
    def sram_sel_reg(self):
        if self._reset:
            self._sram_sel = 0
        elif self._clk_en | self._config_en_sram.r_or():
                self._sram_sel = self._config_en_sram[3] | self._config_en_sram[2]

    def mux_sram_signals(self):
        if self._config_en_sram.r_or():
            for i in range(self.banks):
                self._mem_cen_int[i] = 1
            self._mem_wen[0] = self._config_write & (self._config_en_sram[0] | self._config_en_sram[1])
            self._mem_wen[1] = self._config_write & (self._config_en_sram[2] | self._config_en_sram[3])
            self._mem_wen[0] = self._config_read & (self._config_en_sram[0] | self._config_en_sram[1])
            self._mem_wen[1] = self._config_read & (self._config_en_sram[2] | self._config_en_sram[3])
            self._mem_addr[0] = concat(self._config_en_sram[3] | self._config_en_sram[1], self._config_addr[31, 24])
            self._mem_addr[1] = concat(self._config_en_sram[3] | self._config_en_sram[1], self._config_addr[31, 24])
            self._mem_data_in[0] = self._config_data[15, 0]
            self._mem_data_in[1] = self._config_data[15, 0]
            self._mem_ren = (2 ** self._mem_ren.width) - 1

            self._data_out = self._mem_data_out[self._sram_sel]
            self._valid_out = 0
            self._almost_full = 0
            self._almost_empty = 0
            self._full = 0
            self._empty = 0

        elif self._mode == 0:
            self._mem_cen_int = self._fifo_cen
            self._mem_wen = self._fifo_wen
            self._mem_ren = (2 ** self._mem_ren.width) - 1
            self._mem_addr = self._fifo_addr
            self._mem_data_in = self._fifo_mem_data_out
            self._data_out = self._fifo_out
            self._valid_out = self._lb_valid_out
            self._almost_full = self._fifo_almost_full
            self._almost_empty = self._fifo_almost_empty
            self._full = self._fifo_full
            self._empty = self._fifo_empty

        elif self._mode == 1:
            self._mem_cen_int = self._fifo_cen
            self._mem_wen = self._fifo_wen
            self._mem_ren = (2 ** self._mem_ren.width) - 1
            self._mem_addr = self._fifo_addr
            self._mem_data_in = self._fifo_mem_data_out
            self._data_out = self._fifo_out
            self._valid_out = self._fifo_valid_out
            self._almost_full = self._fifo_almost_full
            self._almost_empty = self._fifo_almost_empty
            self._full = self._fifo_full
            self._empty = self._fifo_empty

        elif self._mode == 2:
            self._mem_cen_int = self._sram_cen
            self._mem_wen = self._sram_wen
            self._mem_ren = (2 ** self._mem_ren.width) - 1
            self._mem_addr = self._sram_addr
            self._mem_data_in = self._sram_mem_data_out
            self._data_out = self._sram_out
            self._valid_out = 1
            self._almost_full = 0
            self._almost_empty = 0
            self._full = 0
            self._empty = 0

        elif self._mode == 3:
            self._mem_cen_int = self._db_cen
            self._mem_wen = self._db_wen
            self._mem_ren = (2 ** self._mem_ren.width) - 1
            self._mem_addr = self._db_addr
            self._mem_data_in = self._db_mem_data_out
            self._data_out = ternary(self._enable_chain & self._chain_wen_in, self._chain_in, self._db_out)
            self._valid_out = (self._enable_chain & self._chain_wen_in) | self._db_valid_out
            self._almost_full = 0
            self._almost_empty = 0
            self._full = 0
            self._empty = 0

        else:
            self._mem_cen_int = (2 ** self._mem_cen_int.width) - 1
            self._mem_wen = 0
            self._mem_ren = 0
            self._mem_addr = 0
            self._mem_data_in = 0
            self._data_out = 0
            self._valid_out = 0
            self._almost_full = 0
            self._almost_empty = 0
            self._full = 0
            self._empty = 0

    def instantiate_lb(self):
        self.add_child("lb_ctrl", LineBufferControl(), comment="===== LINEBUFFER =====//")
        self.wire(self["lb_ctrl"].ports.clk, self._gclk_in)
        self.wire(self["lb_ctrl"].ports.clk_en, self._clk_en)
        self.wire(self["lb_ctrl"].ports.reset, self._reset)
        self.wire(self["lb_ctrl"].ports.flush, self._flush)
        self.wire(self["lb_ctrl"].ports.wen, self._wen_in_int)
        self.wire(self["lb_ctrl"].ports.depth, self._depth)
        self.wire(self["lb_ctrl"].ports.valid, self._lb_valid_out)
        self.wire(self["lb_ctrl"].ports.num_words_mem, self._num_words_mem_fifo_to_lb)
        self.wire(self["lb_ctrl"].ports.ren_to_fifo, self._ren_lb_to_fifo)
        self.wire(self["lb_ctrl"].ports.stencil_width, self._stencil_width)

    def instantiate_fifo(self):
        self.add_child("fifo_ctrl", FIFOControl(self.data_width, self.banks, self.mem_depth), comment="===== FIFO CONTROL =====//")
        self.wire(self["fifo_ctrl"].ports.clk, self._gclk_in)
        self.wire(self["fifo_ctrl"].ports.clk_en, self._clk_en)
        self.wire(self["fifo_ctrl"].ports.reset, self._reset)
        self.wire(self["fifo_ctrl"].ports.flush, self._flush)
        self.wire(self["fifo_ctrl"].ports.ren, ternary(self._mode == 1, self._ren_in, self._ren_lb_to_fifo))
        self.wire(self["fifo_ctrl"].ports.wen, self._wen_in_int)
        self.wire(self["fifo_ctrl"].ports.data_in, self._data_in_int)
        self.wire(self["fifo_ctrl"].ports.data_out, self._fifo_out)
        self.wire(self["fifo_ctrl"].ports.almost_empty, self._fifo_almost_empty)
        self.wire(self["fifo_ctrl"].ports.almost_full, self._fifo_almost_full)
        self.wire(self["fifo_ctrl"].ports.empty, self._fifo_empty)
        self.wire(self["fifo_ctrl"].ports.full, self._fifo_full)
        self.wire(self["fifo_ctrl"].ports.valid, self._fifo_valid_out)
        self.wire(self["fifo_ctrl"].ports.depth, self._depth)
        self.wire(self["fifo_ctrl"].ports.fifo_to_mem_data, self._fifo_mem_data_out)
        self.wire(self["fifo_ctrl"].ports.fifo_to_mem_cen, self._fifo_cen)
        self.wire(self["fifo_ctrl"].ports.fifo_to_mem_wen, self._fifo_wen)
        self.wire(self["fifo_ctrl"].ports.fifo_to_mem_addr, self._fifo_addr)
        self.wire(self["fifo_ctrl"].ports.mem_to_fifo_data, self._mem_data_out)
        self.wire(self["fifo_ctrl"].ports.num_words_mem, self._num_words_mem_fifo_to_lb)
        self.wire(self["fifo_ctrl"].ports.almost_count, self._almost_count)
        self.wire(self["fifo_ctrl"].ports.circular_en, self._circular_en)

    def instantiate_sram(self):
        self.add_child("sram_ctrl", SRAMControl(self.data_width, self.banks, self.mem_depth), comment="===== SRAM CONTROL =====//")
        self.wire(self["sram_ctrl"].ports.clk, self._gclk_in)
        self.wire(self["sram_ctrl"].ports.clk_en, self._clk_en)
        self.wire(self["sram_ctrl"].ports.reset, self._reset)
        self.wire(self["sram_ctrl"].ports.flush, self._flush)

        self.wire(self["sram_ctrl"].ports.data_in, self._data_in)
        self.wire(self["sram_ctrl"].ports.wen, self._wen_in)
        self.wire(self["sram_ctrl"].ports.data_out, self._sram_out)
        self.wire(self["sram_ctrl"].ports.ren, self._ren_in)

        self.wire(self["sram_ctrl"].ports.addr_in, zext(self._addr_in, 16))

        self.wire(self["sram_ctrl"].ports.sram_to_mem_data, self._sram_mem_data_out)
        self.wire(self["sram_ctrl"].ports.sram_to_mem_cen, self._sram_cen)
        self.wire(self["sram_ctrl"].ports.sram_to_mem_wen, self._sram_wen)
        self.wire(self["sram_ctrl"].ports.sram_to_mem_addr, self._sram_addr)
        self.wire(self["sram_ctrl"].ports.mem_to_sram_data, self._mem_data_out)

    def instantiate_db(self):
        self.add_child("db_ctrl", DoubleBufferControl(self.data_width, self.mem_depth, self.banks, self.iterator_support), comment="===== DOUBLEBUFFER =====//")

        self.wire(self["db_ctrl"].ports.clk, self._gclk_in)
        self.wire(self["db_ctrl"].ports.clk_en, self._clk_en)
        self.wire(self["db_ctrl"].ports.reset, self._reset)
        self.wire(self["db_ctrl"].ports.flush, self._flush)
        self.wire(self["db_ctrl"].ports.wen, self._wen_in)
        self.wire(self["db_ctrl"].ports.ren, self._ren_in)
        self.wire(self["db_ctrl"].ports.data_in, self._data_in)
        self.wire(self["db_ctrl"].ports.data_out, self._db_out)

        self.wire(self["db_ctrl"].ports.doublebuffer_data_in, self._db_mem_data_out)
        self.wire(self["db_ctrl"].ports.doublebuffer_cen_mem, self._db_cen)
        self.wire(self["db_ctrl"].ports.doublebuffer_wen_mem, self._db_wen)
        self.wire(self["db_ctrl"].ports.doublebuffer_addr_mem, self._db_addr)
        self.wire(self["db_ctrl"].ports.doublebuffer_data_out, self._mem_data_out)

        self.wire(self["db_ctrl"].ports.addr_in, self._addr_in)

        self.wire(self["db_ctrl"].ports.depth, self._depth)
        self.wire(self["db_ctrl"].ports.valid, self._db_valid_out)
        self.wire(self["db_ctrl"].ports.switch, self._switch_db)
        self.wire(self["db_ctrl"].ports.chain_idx, self._chain_idx)

        self.wire(self["db_ctrl"].ports.arbitrary_addr, self._arbitrary_addr)
        self.wire(self["db_ctrl"].ports.starting_addr, self._starting_addr)
        self.wire(self["db_ctrl"].ports.iter_cnt, self._iter_cnt)
        self.wire(self["db_ctrl"].ports.dimensionality, self._dimensionality)
        self.wire(self["db_ctrl"].ports.stride, self._stride)
        self.wire(self["db_ctrl"].ports.range, self._range)

        self.wire(self["db_ctrl"].ports.rate_matched, self._rate_matched)
        self.wire(self["db_ctrl"].ports.stencil_width, zext(self._stencil_width, 32))

    def instantiate_memory(self):
        for i in range(self.banks):
            self.add_child(f"mem_inst_{i}", SRAMStub(self.data_width, self.mem_depth), comment=f"===== SRAM MACRO {i} =====//")
            self.wire(self[f"mem_inst_{i}"].ports.i_data, self._mem_data_in[i])
            self.wire(self[f"mem_inst_{i}"].ports.o_data, self._mem_data_out[i])
            self.wire(self[f"mem_inst_{i}"].ports.i_clk, self._gclk_in)
            self.wire(self[f"mem_inst_{i}"].ports.i_cen, self._mem_cen[i])
            self.wire(self[f"mem_inst_{i}"].ports.i_wen, self._mem_wen[i] & (self._clk_en | self._config_en_sram.r_or()))
            self.wire(self[f"mem_inst_{i}"].ports.i_addr, self._mem_addr[i])

if __name__ == "__main__":
    mc_dut = MemoryCore(16, 16, 512, 2, 6, 1)
    verilog(mc_dut, filename="memory_core.sv", check_active_high=False, output_dir="build")