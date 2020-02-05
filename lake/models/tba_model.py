from lake.models.model import Model


# transpose buffer aggregation model
class TBAModel(Model):

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

        # initialize transpose buffer
        self.tb = []
        for i in range(2 * self.tb_height):
            row = []
            for j in range(self.fetch_width):
                row.append(0)
            self.tb.append(row)

        self.row_index = 0
        self.input_buf_index = 0
        self.out_buf_index = 1

        self.col_pixels = []
        for i in range(self.tb_height):
            self.col_pixels.append(0)

        self.output_index = 0
        self.index_inner = 0
        self.index_outer = 0
        self.curr_out_start = 0
        self.output_valid = 0
        self.pause_tb = 1
        self.pause_output = 1
        self.prev_pause_output = 1
        self.prev_prev_pause_output = 1
        self.started = 0
        self.rdy_to_arbiter = 1
        self.pause = 1

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val
