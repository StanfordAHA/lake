from lake.models.model import Model
from lake.models.addr_gen_model import AddrGenModel
import math as mt
import kratos as kts
import numpy as np


class InputAddrCtrlModel(Model):

    def __init__(self,
                 interconnect_input_ports,
                 mem_depth,
                 banks,
                 num_tiles,
                 iterator_support,
                 max_port_schedule,
                 address_width,
                 data_width,
                 fetch_width):

        self.interconnect_input_ports = interconnect_input_ports
        self.mem_depth = mem_depth
        self.banks = banks
        self.num_tiles = num_tiles
        self.iterator_support = iterator_support
        self.address_width = address_width
        self.max_port_schedule = max_port_schedule
        self.data_width = data_width
        self.fetch_width = fetch_width
        self.fw_int = int(self.fetch_width / self.data_width)

        self.config = {}

        # Create child address generators
        self.addr_gens = []
        for i in range(self.interconnect_input_ports):
            new_addr_gen = AddrGenModel(iterator_support=self.iterator_support,
                                        address_width=self.address_width)
            self.addr_gens.append(new_addr_gen)

        self.mem_addr_width = kts.clog2(self.num_tiles * self.mem_depth)

        # Get local list of addresses
        self.addresses = []
        self.data_out = []
        for i in range(self.banks):
            self.addresses.append(0)
            self.data_out.append(0)

        # Initialize the configuration
        for i in range(self.interconnect_input_ports):
            self.config[f"address_gen_{i}_starting_addr"] = 0
            self.config[f"address_gen_{i}_dimensionality"] = 0
            for j in range(self.iterator_support):
                self.config[f"address_gen_{i}_strides_{j}"] = 0
                self.config[f"address_gen_{i}_ranges_{j}"] = 0

        # Set up the wen
        self.wen = []
        self.mem_addresses = []
        # self.port_sels = []
        for i in range(self.banks):
            self.wen.append(0)
            self.mem_addresses.append(0)
            # self.port_sels.append(0)

        self.port_out_exp = []
        for i in range(self.banks):
            inter = []
            for j in range(self.interconnect_input_ports):
                inter.append(0)
            self.port_out_exp.append(inter)

    def set_config(self, new_config):
        # Configure top level
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val
        # Configure children
        for i in range(self.interconnect_input_ports):
            addr_gen_config = {}
            addr_gen_config["starting_addr"] = self.config[f"address_gen_{i}_starting_addr"]
            addr_gen_config["dimensionality"] = self.config[f"address_gen_{i}_dimensionality"]
            for j in range(self.iterator_support):
                addr_gen_config[f"strides_{j}"] = self.config[f"address_gen_{i}_strides_{j}"]
                addr_gen_config[f"ranges_{j}"] = self.config[f"address_gen_{i}_ranges_{j}"]
            self.addr_gens[i].set_config(addr_gen_config)

    # Retrieve the current addresses from each generator
    def interact(self, valid, data_in, wen_en):
        '''
        Returns (wen, data, addrs, port_out)
        '''
        wen = self.get_wen(valid, wen_en)
        data_out = self.get_data_out(valid, data_in)
        port_out = self.get_port_out(valid)
        addrs = self.get_addrs(valid)
        self.step_addrs(valid)
        return(wen, data_out, addrs, port_out)

    def get_port_out(self, valid):
        for i in range(self.banks):
            for j in range(self.interconnect_input_ports):
                if (valid[j]):
                    self.port_out_exp[i][j] = 1
                else:
                    self.port_out_exp[i][j] = 0

        port_out = []
        for i in range(self.interconnect_input_ports):
            new_tmp = []
            for j in range(self.banks):
                new_tmp.append(self.port_out_exp[j][i])

            concat = np.bitwise_or.reduce(new_tmp)
            port_out.append(concat)

        return port_out

    def get_addrs(self, valid):
        for i in range(self.banks):
            self.addresses[i] = 0
        for i in range(self.interconnect_input_ports):
            if (valid[i]):
                to_get = self.addr_gens[i]
                if(self.banks == 1):
                    self.addresses[0] = to_get.get_address()
                    break
                else:
                    self.addresses[to_get.get_address() >> (self.mem_addr_width)] = to_get.get_address()
        return self.addresses

    def get_data_out(self, valid, data_in):
        assert len(data_in) == self.interconnect_input_ports, "Should feed proper length data"
        for i in range(self.banks):
            self.data_out[i] = [0 for z in range(self.fw_int)]
            # self.data_out[i] = [0] * len(data_in[0])
        for i in range(self.interconnect_input_ports):
            if (valid[i]):
                to_get = self.addr_gens[i]
                if(self.banks == 1):
                    self.data_out[0] = data_in[i]
                    break
                else:
                    self.data_out[to_get.get_address() >> (self.mem_addr_width)] = data_in[i]
        return self.data_out

    # Get the wen for the current valid input
    def get_wen(self, valid, wen_en):
        for i in range(self.banks):
            self.wen[i] = 0
        for i in range(self.interconnect_input_ports):
            if (valid[i]):
                to_get = self.addr_gens[i]
                if(self.banks == 1):
                    self.wen[0] = wen_en[i]
                    break
                else:
                    self.wen[to_get.get_address() >> (self.mem_addr_width)] = wen_en[i]
        return self.wen

    # Step the addresses based on valid
    def step_addrs(self, valid):
        for i, valid_input in enumerate(valid):
            if valid_input:
                to_step = self.addr_gens[i]
                to_step.step()

        # Not implemented
    def update_ports(self):
        raise NotImplementedError

    def peek(self):
        raise NotImplementedError
