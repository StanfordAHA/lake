from kratos import *
from lake.top.lake_top import LakeTop
from lake.utils.sram_macro import SRAMMacroInfo
from lake.passes.passes import lift_config_reg

import kratos as kts


class LakeChain(Generator):
    def __init__(self,
                 data_width=16,  # CGRA Params
                 mem_width=64,
                 mem_depth=512,
                 banks=2,
                 input_iterator_support=6,  # Addr Controllers
                 output_iterator_support=6,
                 interconnect_input_ports=1,  # Connection to int
                 interconnect_output_ports=3,
                 mem_input_ports=1,
                 mem_output_ports=1,
                 use_sram_stub=1,
                 sram_macro_info=SRAMMacroInfo(),
                 read_delay=1,  # Cycle delay in read (SRAM vs Register File)
                 rw_same_cycle=False,  # Does the memory allow r+w in same cycle?
                 agg_height=4,
                 max_agg_schedule=32,
                 input_max_port_sched=32,
                 output_max_port_sched=32,
                 align_input=1,
                 max_line_length=128,
                 max_tb_height=1,
                 tb_range_max=128,
                 tb_sched_max=64,
                 max_tb_stride=15,
                 num_tb=1,
                 tb_iterator_support=2,
                 multiwrite=1,
                 max_prefetch=64,
                 config_data_width=16,
                 config_addr_width=8,
                 num_tiles=2,
                 remove_tb=False,
                 fifo_mode=False,
                 add_clk_enable=False,
                 add_flush=False):
        super().__init__("LakeChain", debug=True)

        fw_int = int(mem_width / data_width)
        data_words_per_set = 2 ** config_addr_width
        sets = int((fw_int * mem_depth) / data_words_per_set)

        sets_per_macro = max(1, int(mem_depth / data_words_per_set))
        total_sets = max(1, banks * sets_per_macro)

        self._clk = self.clock("clk")
        self._rst_n = self.reset("rst_n")

        self._data_in = self.input("data_in",
                                   data_width,
                                   size=interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)
        self._addr_in = self.input("addr_in",
                                   data_width,
                                   size=interconnect_input_ports,
                                   packed=True,
                                   explicit_array=True)

        self._wen = self.input("wen", interconnect_input_ports)
        self._ren = self.input("ren", interconnect_output_ports)

        self._config_data_in = self.input("config_data_in",
                                          config_data_width)

        self._config_addr_in = self.input("config_addr_in",
                                          config_addr_width)

        self._config_data_out = self.output("config_data_out", config_data_width,
                                            size=(num_tiles, total_sets),
                                            explicit_array=True,
                                            packed=True)

        self._config_read = self.input("config_read", 1)
        self._config_write = self.input("config_write", 1)
        self._config_en = self.input("config_en", total_sets)

        self._data_out = self.output("data_out",
                                     data_width,
                                     size=(num_tiles, interconnect_output_ports),
                                     packed=True,
                                     explicit_array=True)

        self._data_out_inter = self.var("data_out_inter",
                                        data_width,
                                        size=(num_tiles, interconnect_output_ports),
                                        packed=True,
                                        explicit_array=True)

        self._valid_out = self.output("valid_out",
                                      interconnect_output_ports,
                                      size=num_tiles,
                                      packed=True,
                                      explicit_array=True)

        self._valid_out_inter = self.var("valid_out_inter",
                                         interconnect_output_ports,
                                         size=num_tiles,
                                         packed=True,
                                         explicit_array=True)

        self._enable_chain_output = self.input("enable_chain_output", 1)

        self._chain_data_out = self.output("chain_data_out",
                                           data_width,
                                           size=interconnect_output_ports,
                                           packed=True,
                                           explicit_array=True)

        self._chain_valid_out = self.output("chain_valid_out",
                                            interconnect_output_ports)

        self._tile_output_en = self.var("tile_output_en",
                                        1,
                                        size=(num_tiles, interconnect_output_ports),
                                        packed=True,
                                        explicit_array=True)

        self.is_valid_ = self.var("is_valid",
                                  1,
                                  size=interconnect_output_ports,
                                  packed=True,
                                  explicit_array=True)

        self.valids = self.var("valids",
                               clog2(num_tiles),
                               size=interconnect_output_ports,
                               packed=True,
                               explicit_array=True)

        for i in range(num_tiles):
            tile = LakeTop(data_width=data_width,
                           mem_width=mem_width,
                           mem_depth=mem_depth,
                           banks=banks,
                           input_iterator_support=input_iterator_support,
                           output_iterator_support=output_iterator_support,
                           interconnect_input_ports=interconnect_input_ports,
                           interconnect_output_ports=interconnect_output_ports,
                           mem_input_ports=mem_input_ports,
                           mem_output_ports=mem_output_ports,
                           use_sram_stub=use_sram_stub,
                           sram_macro_info=sram_macro_info,
                           read_delay=read_delay,
                           rw_same_cycle=rw_same_cycle,
                           agg_height=agg_height,
                           max_agg_schedule=max_agg_schedule,
                           input_max_port_sched=input_max_port_sched,
                           output_max_port_sched=output_max_port_sched,
                           align_input=align_input,
                           max_line_length=max_line_length,
                           max_tb_height=max_tb_height,
                           tb_range_max=tb_range_max,
                           tb_sched_max=tb_sched_max,
                           max_tb_stride=max_tb_stride,
                           num_tb=num_tb,
                           tb_iterator_support=tb_iterator_support,
                           multiwrite=multiwrite,
                           max_prefetch=max_prefetch,
                           config_data_width=config_data_width,
                           config_addr_width=config_addr_width,
                           num_tiles=num_tiles,
                           remove_tb=remove_tb,
                           fifo_mode=fifo_mode,
                           add_clk_enable=add_clk_enable,
                           add_flush=add_flush)

            self.add_child(f"tile_{i}", tile,
                           clk=self._clk,
                           rst_n=self._rst_n,
                           enable_chain_output=self._enable_chain_output,
                           # tile index
                           chain_idx_input=i,
                           chain_idx_output=0,
                           tile_output_en=self._tile_output_en[i],
                           # broadcast input data to all tiles
                           data_in=self._data_in,
                           addr_in=self._addr_in,
                           wen=self._wen,
                           ren=self._ren,
                           config_data_in=self._config_data_in,
                           config_addr_in=self._config_addr_in,
                           config_data_out=self._config_data_out[i],
                           config_read=self._config_read,
                           config_write=self._config_write,
                           config_en=self._config_en,
                           # used if output chaining not enabled
                           data_out=self._data_out_inter[i],
                           valid_out=self._valid_out_inter[i],
                           # unused currently?
                           tile_en=1,
                           # UB mode
                           mode=0)

        self.add_code(self.set_data_out)
        self.add_code(self.set_valid_out)
        self.add_code(self.set_chain_outputs)

        # config regs
        lift_config_reg(self.internal_generator)

    @always_comb
    def set_data_out(self):
        self._data_out = self._data_out_inter

    @always_comb
    def set_chain_outputs(self):
        for i in range(interconnect_output_ports):
            self.valids[i] = 0
            self.is_valid_[i] = 0
        for i in range(num_tiles):
            for j in range(interconnect_output_ports):
                if (self._tile_output_en[i][j] == 1):
                    # keeps track of while tile has data/valid for this port
                    self.valids[j] = i
                    # indicates that this port value has been updated for some tile
                    self.is_valid_[j] = 1
        for i in range(interconnect_output_ports):
            if (self._enable_chain_output == 0) | (self.is_valid_[i] == 0):
                self._chain_data_out[i] = 0
                self._chain_valid_out[i] = 0
            else:
                self._chain_data_out[i] = self._data_out_inter[self.valids[i]][i]
                self._chain_valid_out[i] = self._valid_out_inter[self.valids[i]][i]

    @always_comb
    def set_valid_out(self):
        for i in range(num_tiles):
            for j in range(interconnect_output_ports):
                if self._tile_output_en[i][j] == 0:
                    self._valid_out[i][j] = 0
                elif self._enable_chain_output:
                    self._valid_out[i][j] = 0
                else:
                    self._valid_out[i][j] = self._valid_out_inter[i][j]


if __name__ == "__main__":
    dut = LakeChain(num_tiles=2)
    verilog(dut, filename="top_chain.sv",
            optimize_if=False)
