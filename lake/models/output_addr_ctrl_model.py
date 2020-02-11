from lake.models.model import Model
from lake.models.addr_gen_model import AddrGenModel
import math as mt
import kratos as kts


class OutputAddrCtrlModel(Model):

    def __init__(self,
                 interconnect_output_ports,
                 mem_depth,
                 banks,
                 iterator_support,
                 address_width,
                 data_width,
                 fetch_width):

        self.interconnect_output_ports = interconnect_output_ports
        self.mem_depth = mem_depth
        self.banks = banks
        self.iterator_support = iterator_support
        self.address_width = address_width
        self.data_width = data_width
        self.fetch_width = fetch_width
        self.fw_int = int(self.fetch_width / self.data_width)

        self.config = {}

        # Create child address generators
        self.addr_gens = []
        for i in range(self.interconnect_output_ports):
            new_addr_gen = AddrGenModel(mem_depth=self.mem_depth,
                                        iterator_support=self.iterator_support,
                                        address_width=self.address_width)
            self.addr_gens.append(new_addr_gen)

        self.mem_addr_width = kts.clog2(self.mem_depth)

        # Get local list of addresses
        self.addresses = []
        for i in range(self.interconnect_output_ports):
            self.addresses.append(0)

        # Initialize the configuration
        for i in range(self.interconnect_output_ports):
            self.config[f"address_gen_{i}_starting_addr"] = 0
            self.config[f"address_gen_{i}_dimensionality"] = 0
            for j in range(self.iterator_support):
                self.config[f"address_gen_{i}_strides_{j}"] = 0
                self.config[f"address_gen_{i}_ranges_{j}"] = 0

        # Set up the wen
        self.ren = []
        self.mem_addresses = []
        for i in range(self.banks):
            self.ren.append([])
            for j in range(self.interconnect_output_ports):
                self.ren[i].append(0)
        for i in range(self.interconnect_output_ports):
            self.mem_addresses.append(0)

    def set_config(self, new_config):
        # Configure top level
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val
        # Configure children
        for i in range(self.interconnect_output_ports):
            addr_gen_config = {}
            addr_gen_config["starting_addr"] = self.config[f"address_gen_{i}_starting_addr"]
            addr_gen_config["dimensionality"] = self.config[f"address_gen_{i}_dimensionality"]
            for j in range(self.iterator_support):
                addr_gen_config[f"stride_{j}"] = self.config[f"address_gen_{i}_strides_{j}"]
                addr_gen_config[f"range_{j}"] = self.config[f"address_gen_{i}_ranges_{j}"]
            self.addr_gens[i].set_config(addr_gen_config)

    def interact(self, valid_in, step_in):
        '''
        Returns (ren, addrs)
        '''
        ren = self.get_ren(valid_in)
        addrs = self.get_addrs()
        self.step_addrs(valid_in, step_in)
        return (ren, addrs)

    # Retrieve the current addresses from each generator
    def get_addrs(self):
        for i in range(self.interconnect_output_ports):
            to_get = self.addr_gens[i]
            self.addresses[i] = to_get.get_address() % self.mem_depth
        return self.addresses

    def get_addrs_full(self):
        for i in range(self.interconnect_output_ports):
            to_get = self.addr_gens[i]
            self.addresses[i] = to_get.get_address()
        return self.addresses

    # Get the ren for the current valid input
    def get_ren(self, valid):
        for i in range(self.banks):
            for j in range(self.interconnect_output_ports):
                self.ren[i][j] = 0
        for i in range(self.interconnect_output_ports):
            if(valid[i]):
                if(self.banks == 1):
                    self.ren[0][i] = 1
                else:
                    self.ren[self.get_addrs_full()[i] >> (self.mem_addr_width)][i] = 1
        return self.ren

    # Step the addresses based on valid
    def step_addrs(self, valid, step):
        for i, valid_input in enumerate(valid):
            if valid_input & step[i]:
                to_step = self.addr_gens[i]
                to_step.step()

        # Not implemented
    def update_ports(self):
        raise NotImplementedError

    def peek(self):
        raise NotImplementedError
