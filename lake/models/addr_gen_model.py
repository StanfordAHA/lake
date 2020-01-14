from lake.models.model import Model

class AddrGenModel(Model):

    def __init__(self, mem_depth, iterator_support, address_width):
        self.mem_depth = mem_depth
        self.iterator_support = iterator_support
        self.address_width = address_width

        self.config = {}

        self.config["starting_addr"] = 0
        self.config["dimensionality"] = 0

        self.dim_cnt = []

        self.address = 0

        for i in range(self.iterator_support):
            self.config[f"range_{i}"] = 0
            self.config[f"stride_{i}"] = 0
            self.dim_cnt.append(0)

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val
        for i in range(self.iterator_support):
            self.dim_cnt[i] = 0
        self.address = 0 + self.config["starting_addr"]

    def get_address(self):
        return self.address

    def step(self):
        for i in range(self.config["dimensionality"]):
            if(i == 0):
                update_curr = True

            if update_curr:
                self.dim_cnt[i] = self.dim_cnt[i] + 1
                if(self.dim_cnt[i] == self.config[f"range_{i}"]):
                    self.dim_cnt[i] = 0
                else:
                    break
            else:
                break
        self.address = self.config["starting_addr"]
        for i in range(self.config["dimensionality"]):
            self.address = self.address + (self.dim_cnt[i]*self.config[f"stride_{i}"])
    #print("New Addr: " + str(self.address))