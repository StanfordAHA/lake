from lake.models.model import Model
#from lake.models.agg_model import AggModel
from lake.models.addr_gen_model import AddrGenModel

class InputAddrCtrlModel(Model):

    def __init__(self, 
               interconnect_input_ports,
               mem_depth,
               banks,
               iterator_support,
               max_port_schedule,
               address_width
    ):

        self.interconnect_input_ports = interconnect_input_ports
        self.mem_depth = mem_depth
        self.banks = banks
        self.iterator_support = iterator_support
        self.address_width = address_width
        self.max_port_schedule = max_port_schedule

        self.config = {}

        self.addr_gens = []
        for i in range(self.interconnect_input_ports):
            new_addr_gen = AddrGenModel(mem_depth=self.mem_depth, 
                                        iterator_support=self.iterator_support,
                                        address_width=self.address_width
                                        )
            #new_addr_gen.set_config()
            self.addr_gens.append(new_addr_gen)

        self.addresses = []
        for i in range(self.interconnect_input_ports):
            self.addresses.append(0)

        for i in range(self.interconnect_input_ports):
            self.config[f"starting_addr_p_{i}"] = 0
            self.config[f"dimensionality_{i}"] = 0
            for j in range(self.iterator_support):
                self.config[f"stride_p_{i}_{j}"] = 0
                self.config[f"range_p_{i}_{j}"] = 0

        self.sched_ptrs = []
        for i in range(self.banks):
            self.config[f"port_periods_{i}"] = 0
            self.sched_ptrs.append(0)
            for j in range(self.max_port_schedule):
                self.config[f"port_sched_b_{i}_{j}"] = 0

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

    def insert(self, in_data, valid):
        if valid:
            #print(f"inserting {in_data} into buffer {self.config[f'in_sched_{self.in_sched_ptr}']}")
            to_insert = self.aggs[self.config[f"in_sched_{self.in_sched_ptr}"]]
            to_insert.insert(in_data, valid)
            if(to_insert.get_valid_out()):
                self.in_sched_ptr += 1
                if(self.in_sched_ptr >= self.config['in_period']):
                    self.in_sched_ptr = 0

    def get_valid_out(self):
        valid_check_agg = self.aggs[self.config[f"out_sched_{self.out_sched_ptr}"]]
        #print(f"valid is now {valid_check_agg.get_valid_out()}")
        return valid_check_agg.get_valid_out()

    def get_item(self):
        valid_check_agg = self.aggs[self.config[f"out_sched_{self.out_sched_ptr}"]]
        self.out_sched_ptr += 1
        if(self.out_sched_ptr >= self.config['out_period']):
            self.out_sched_ptr = 0
        return valid_check_agg.get_data_out()

    def peek(self):
        pass
