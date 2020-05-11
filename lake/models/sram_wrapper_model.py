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
                 num_tiles):

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

        self.chain_idx_bits = max(1, kts.clog2(num_tiles))

        # configuration registers
        self.config = {}
        self.config["enable_chain_input"] = 0
        self.config["enable_chain_output"] = 0
        self.config["chain_idx_input"] = 0
        self.config["chain_idx_output"] = 0

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

    def interact(self, data_in, addr, cen, wen, wtsel, rtsel):

        # set chain_idx_tile
        # set mem addr
        if self.num_tiles == 1:
            chain_idx_tile = 0
            addr_sram = addr
        else:
            addr_bin = bin(addr)
            chain_idx_tile = addr_bin[self.address_width - self.chain_idx_bits, self.address_width]
            addr_sram = addr_bin[0, self.address_width - self.chain_idx_bits]

        # set chain wen
        if (self.num_tiles == 1) or (self.config["enable_chain_input"] == 0):
            wen_chain = wen
        # enable chain input
        else:
            # write
            if wen:
                if self.config["chain_idx_input"] == chain_idx_tile:
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
            if self.config["enable_chain_input"] == 1:
                if self.config["chain_idx_input"] == chain_idx_tile:
                    cen_chain = cen
                else:
                    cen_chain = 0
            else:
                cen_chain = cen
        # read
        else:
            if self.config["enable_chain_output"] == 1:
                if self.config["chain_idx_outut"] == chain_idx_tile:
                    cen_chain = cen
                else:
                    cen_chain = 0
            else:
                cen_chain = cen

        # set valid data
        # read
        if not wen:
            if self.config["enable_chain_output"] == 1:
                if self.config["chain_idx_outut"] == chain_idx_tile:
                    valid_data = cen
                else:
                    valid_data = 0
            else:
                valid_data = cen
        # write
        else:
            valid_data = 0

        if self.use_sram_stub:
            data_out = self.sram.interact(wen=wen_chain,
                                          cen=cen_chain,
                                          addr=addr_sram,
                                          data=data_in,
                                          chain_idx_input=self.config["chain_idx_input"])
        else:
            AssertionError("Do not have external SRAM module to use...please use SRAM Stub")

        return data_out, valid_data
