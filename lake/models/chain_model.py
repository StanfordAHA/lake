from lake.models.model import Model
import kratos as kts


# chain model
class ChainModel(Model):

    def __init__(self,
                 data_width,
                 interconnect_output_ports,
                 chain_idx_bits,
                 enable_chain_output,
                 chain_idx_output):

        # generation parameters
        self.data_width = data_width
        self.interconnect_output_ports = interconnect_output_ports
        self.chain_idx_bits = chain_idx_bits

        # configuration registers passed down from top level
        self.enable_chain_output = enable_chain_output
        self.chain_idx_output = chain_idx_output

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

    def interact(self, curr_tile_valid_out, curr_tile_data_out, chain_valid_in, chain_data_in):

        chain_data_out_inter = []
        chain_valid_out_inter = []
        for i in range(self.interconnect_output_ports):
            if chain_valid_in[i] == 0:
                chain_data_out_inter.append(curr_tile_data_out[i])
                chain_valid_out_inter.append(curr_tile_valid_out[i])
            else:
                chain_data_out_inter.append(chain_data_in[i])
                chain_valid_out_inter.append(chain_valid_in[i])

        # all combinational outputs
        # set data_out_tile
        if self.enable_chain_output:
            data_out_tile = chain_data_out_inter
        else:
            data_out_tile = curr_tile_data_out

        # set valid_out_tile
        valid_out_tile = []
        if self.enable_chain_output:
            if not (self.chain_idx_output == 0):
                for i in range(self.interconnect_output_ports):
                    valid_out_tile.append(0)
            else:
                valid_out_tile = chain_valid_out_inter
        else:
            valid_out_tile = curr_tile_valid_out

        # set chain_data_out
        chain_data_out = chain_data_out_inter

        # set chain_valid_out
        chain_valid_out = []
        if (self.chain_idx_output == 0) or \
                (not self.enable_chain_output):
            for i in range(self.interconnect_output_ports):
                chain_valid_out.append(0)
        else:
            chain_valid_out = chain_valid_out_inter

        return chain_data_out, chain_valid_out, data_out_tile, valid_out_tile
