from lake.models.model import Model
from lake.models.tb_model import TBModel


# transpose buffer model
class TBAModel(Model):

    def __init__(self,
                 word_width,
                 fetch_width,
                 num_tb,
                 tb_height,
                 max_range,
                 max_range_inner):

        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_width
        self.num_tb = num_tb
        self.tb_height = tb_height
        self.max_range = max_range
        self.max_range_inner = max_range_inner

        # configuration registers
        self.config = {}
        self.config["range_outer"] = 1
        self.config["range_inner"] = 1
        self.config["stride"] = 1
        self.config["indices"] = [0]
        self.config["dimensionality"] = 1
        self.config["starting_addr"] = 0

        self.output_valid_all = []
        for i in range(self.num_tb):
            self.output_valid_all.append(0)

        self.tbs = []
        for i in range(self.num_tb):
            self.tbs.append(TBModel(self.word_width,
                                    self.fetch_width,
                                    self.num_tb,
                                    self.tb_height,
                                    self.max_range,
                                    self.max_range_inner))
            self.tbs[i].set_config(self.config)

        self.arbiter_rdy_all = []
        for i in range(self.num_tb):
            self.arbiter_rdy_all.append(0)

        self.tb_to_interconnect_data = []
        for i in range(self.tb_height):
            self.tb_to_interconnect_data.append(0)

        self.tb_to_interconnect_valid = 0
        self.tb_arbiter_rdy = 0

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

        self.output_valid_all = []
        for i in range(self.num_tb):
            self.output_valid_all.append(0)

        self.tbs = []
        for i in range(self.num_tb):
            self.tbs.append(TBModel(self.word_width,
                                    self.fetch_width,
                                    self.num_tb,
                                    self.tb_height,
                                    self.max_range,
                                    self.max_range_inner))
            self.tbs[i].set_config(self.config)

        self.arbiter_rdy_all = []
        for i in range(self.num_tb):
            self.arbiter_rdy_all.append(0)

        self.tb_to_interconnect_data = []
        for i in range(self.tb_height):
            self.tb_to_interconnect_data.append(0)

        self.tb_to_interconnect_valid = 0
        self.tb_arbiter_rdy = 0

    def set_tb_outputs(self):
        for i in range(self.num_tb):
            self.output_valid_all[i] = self.tbs[i].get_output_valid()
            if self.output_valid_all[i] == 1:
                self.tb_to_interconnect_data = self.tbs[i].get_col_pixels()
        valid_count = sum(self.output_valid_all)
        if valid_count > 0:
            self.tb_to_interconnect_valid = 1
        else:
            self.tb_to_interconnect_valid = 0

    def send_tba_rdy(self):
        for i in range(self.num_tb):
            self.arbiter_rdy_all[i] = self.tbs[i].get_rdy_to_arbiter()
        rdy_count = sum(self.arbiter_rdy_all)
        if rdy_count > 0:
            self.tb_arbiter_rdy = 1
        else:
            self.tb_arbiter_rdy = 0

    def get_ready(self):
        for i in range(self.num_tb):
            # return self.tb_arbiter_rdy
            return self.tbs[i].get_rdy_to_arbiter()

    def print_tba(self):
        print("output valid all ", self.output_valid_all)
        print("arbiter rdy all ", self.arbiter_rdy_all)
        print("tb arbiter rdy ", self.tb_arbiter_rdy)

    def print_tba_tb(self, input_data, valid_data, ack_in, tb_index_for_data, ren):
        self.tbs[0].print_tb(input_data, valid_data, ack_in, ren)
        print()

    def tba_main(self, input_data, valid_data, ack_in, tb_index_for_data, ren, mem_valid_data):
        ret_data = 0
        ret_valid = 0
        ret_rdy = 0
        for i in range(self.num_tb):
            if i == tb_index_for_data:
                valid_data_i = valid_data
                ack_in_i = ack_in
            else:
                valid_data_i = 0
                ack_in_i = 0

            (ret_data, ret_valid, ret_rdy) = self.tbs[i].interact(
                input_data, valid_data_i, ack_in_i, ren, mem_valid_data)

        # self.set_tb_outputs()
        # self.send_tba_rdy()
        # self.print_tba()

        return ret_data, ret_valid
