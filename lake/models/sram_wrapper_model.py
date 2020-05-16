from lake.models.model import Model
from lake.models.sram_model import SRAMModel
import kratos as kts


# sram wrapper model
class SRAMWrapperModel(Model):

    def __init__(self,
                 use_sram_stub,
                 sram_name,
                 data_width,
                 fw_int,
                 mem_depth,
                 mem_input_ports,
                 mem_output_ports,
                 address_width,
                 bank_num,
                 num_tiles,
                 # configuration registers passed down from top level
                 enable_chain_input,
                 enable_chain_output,
                 chain_idx_input,
                 chain_idx_output):

        # generation parameters
        self.use_sram_stub = use_sram_stub
        self.sram_name = sram_name
        self.data_width = data_width
        self.fw_int = fw_int
        self.mem_depth = mem_depth
        self.mem_input_ports = mem_input_ports
        self.mem_output_ports = mem_output_ports
        self.address_width = address_width
        self.bank_num = bank_num
        self.num_tiles = num_tiles

        # configuration registers passed down from top level
        self.enable_chain_input = enable_chain_input
        self.enable_chain_output = enable_chain_output
        self.chain_idx_input = chain_idx_input
        self.chain_idx_output = chain_idx_output

        self.chain_idx_bits = max(1, kts.clog2(num_tiles))

        self.prev_wen = 0
        self.prev_cen = 0

        self.sram = SRAMModel(data_width,
                              fw_int,
                              mem_depth,
                              num_tiles)

        # no configuration space for sram, no need to set config

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val
        # no explicit configuration space for sram wrapper module

    def interact(self, data_in, addr, cen, wen, wtsel, rtsel):

        # set chain_idx_tile
        # set mem addr
        if self.num_tiles == 1:
            chain_idx_tile = 0
            addr_sram = addr
        else:
            chain_idx_tile = addr >> (self.address_width - self.chain_idx_bits - 1)
            addr = addr & (2**(self.address_width - self.chain_idx_bits - 1) - 1)

        # set chain wen
        if (self.num_tiles == 1) or (self.enable_chain_input == 0):
            wen_chain = wen
        # enable chain input
        else:
            # write
            if wen:
                if self.chain_idx_input == chain_idx_tile:
                    wen_chain = wen
                else:
                    wen_chain = 0
            # read
            else:
                wen_chain = 0

        # set chain cen
        if (self.num_tiles == 1):
            cen_chain = cen
        # write
        elif wen:
            if self.enable_chain_input == 1:
                if self.chain_idx_input == chain_idx_tile:
                    cen_chain = cen
                else:
                    cen_chain = 0
            else:
                cen_chain = cen
        # read
        else:
            if self.enable_chain_output == 1:
                if self.chain_idx_output == chain_idx_tile:
                    cen_chain = cen
                else:
                    cen_chain = 0
            else:
                cen_chain = cen

        # registered output, watch output for combinational inputs
        # set valid data
        # read
        if not self.prev_wen:
            if self.enable_chain_output == 1:
                if self.chain_idx_output == chain_idx_tile:
                    valid_data = self.prev_cen
                else:
                    valid_data = 0
            else:
                valid_data = self.prev_cen
        # write
        else:
            valid_data = 0

        # inputs for valid data, registered output
        self.prev_wen = wen
        self.prev_cen = cen

        # cannot have self.use_sram_stub = False because we do not have an external
        # SRAM module to use...please use SRAM Stub
        assert self.use_sram_stub == 1, \
            "Do not have an external SRAM module to use...please use SRAM Stub"

        if self.use_sram_stub:
            data_out = self.sram.interact(wen=wen_chain,
                                          cen=cen_chain,
                                          addr=addr_sram,
                                          data=data_in)

        return data_out, valid_data

    def get_rd_reg(self):
        return list(self.sram.rd_reg)
