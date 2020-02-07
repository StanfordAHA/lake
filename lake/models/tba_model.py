from lake.models.model import Model
from lake.models.tb_model import TBModel


# transpose buffer model
class TBModel(Model):

    def __init__(self,
                 word_width,
                 fetch_width,
                 num_tb,
                 tb_height,
                 max_range):

        # generation parameters
        self.word_width = word_width
        self.fetch_width = fetch_width
        self.num_tb = num_tb
        self.tb_height = tb_height
        self.max_range = max_range

        # configuration registers
        self.config = {}
        self.config["range_outer"] = 1
        self.config["range_inner"] = 1
        self.config["stride"] = 1
        self.config["indices"] = [0]

        self.valid_data_all = []
        for i in range(self.num_tb):
            self.valid_data_all.append(0)

        self.data_valid_all = []
        for i in range(self.num_tb):
            tb_pixels = []
            for j in range(self.tb_height):
                tb_pixels.append(0)
            self.data_valid_all.append(tb_pixels)

        self.output_valid_all = []
        for i in range(self.num_tb):
            self.output_valid_all.append(0)

        self.tbs = []
        for i in range(self.num_tb):
            self.tbs.append(TBModel(word_width, fetch_width, num_tb, tb_height, max_range))
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

    def set_valid_data_all(self, valid_data, tb_index_for_data):
        if not valid_data:
            for i in range(self.num_tb):
                self.valid_data_all[i] = 0
        else:
            for i in range(self.num_tb):
                if i == tb_index_for_data:
                    self.valid_data_all[i] = 1
                else:
                    self.valid_data_all[i] = 0

    def set_tb_outputs(self):
        for i in range(self.num_tb):
            self.output_valid_all[i] = self.tbs[i].get_output_valid()
            self.output_data_all[i] = self.tbs[i].get_col_pixels()
            if self.output_valid_all[i] == 1:
                self.tb_to_interconnect_data = self.output_data_all[i]
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

    def print_tba(self):

    def transpose_buffer(self, input_data, valid_data, ack_in):
        self.set_valid_data_all()
        self.set_tb_outputs()
        self.send_tba_rdy()
        self.print_tba()
        return
