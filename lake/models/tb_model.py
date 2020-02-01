from lake.models.model import Model


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
        self.pause = 1
        self.rdy_to_arbiter = 1

    def input_to_tb(self, input_data, valid_data):
        # maybe add start data logic
        if valid_data:
            self.tb[self.row_index + self.tb_height * self.input_buf_index] = input_data
            if self.row_index == self.tb_height - 1:
                self.row_index = 0
                self.input_buf_index = 1 - self.input_buf_index
            else:
                self.row_index = self.row_index + 1

    def output_from_tb(self, valid_data, ack_in):
        # maybe add pause_output for beginning
        self.output_index_abs = self.index_outer * self.config["stride"] + \
            self.config["indices"][self.index_inner]
        self.output_index = self.output_index_abs % self.fetch_width

        self.col_pixels = []
        for i in range(self.tb_height):
            self.col_pixels.append(self.tb[i + self.tb_height * (1 - self.out_buf_index)][self.output_index])

        if self.index_inner == self.config["range_inner"] - 1:
            self.index_inner = 0
            if self.index_outer == self.config["range_outer"] - 1:
                self.index_outer = 0
            else:
                self.index_outer = self.index_outer + 1
        else:
            self.index_outer = self.index_outer
            if self.pause != 1:
                self.index_inner = self.index_inner + 1

        if self.index_inner == self.config["range_inner"] - 1:
            if self.index_outer == self.config["range_outer"] - 1:
                self.pause_tb = 1 - valid_data
            else:
                self.pause_tb = 0
        elif self.pause_tb:
            self.pause_tb = 1 - valid_data
        elif self.pause == 0:
                self.pause_tb = 0

        self.output_index_abs = self.index_outer * self.config["stride"] + \
            self.config["indices"][self.index_inner]
        self.output_index = self.output_index_abs % self.fetch_width

        if ((self.output_index_abs % self.fetch_width == 0) and
                (self.output_index_abs != self.curr_out_start)):
            self.curr_out_start = self.output_index_abs
            self.out_buf_index = 1 - self.out_buf_index
            self.rdy_to_arbiter = 1
        elif ack_in:
            self.rdy_to_arbiter = 0

        self.col_pixels = []
        for i in range(self.tb_height):
            self.col_pixels.append(self.tb[i + self.tb_height * (1 - self.out_buf_index)][self.output_index])

        print("pause ", self.pause)
        print("prev prev pause output ", self.prev_prev_pause_output)
        if self.pause_tb or self.pause_output or (self.prev_pause_output == 1 and self.pause_output == 0) or (self.prev_prev_pause_output == 1 and self.prev_pause_output == 0 and self.pause_output == 0):
            self.output_valid = 0
            self.pause = 1
        else:
            self.output_valid = 1
            self.pause = 0


        self.prev_prev_pause_output = self.prev_pause_output
        self.prev_pause_output = self.pause_output
        if self.pause_tb:
            self.pause_output = 1
        elif self.pause_output & self.row_index < self.tb_height - 1:
            self.pause_output = 1
        else:
            self.pause_output = 0

        if self.prev_pause_output == 1 and self.pause_output == 0 and self.started == 0:
            self.pause_output = self.prev_pause_output
            self.started = 1

    def print_tb(self):
        print("tb: ", self.tb)
        print("row index: ", self.row_index)
        print("input buf index: ", self.input_buf_index)
        print("out buf index ", self.out_buf_index)
        print("col pixels ", self.col_pixels)
        print("output index ", self.output_index)
        print("index inner ", self.index_inner)
        print("index outer ", self.index_outer)
        print("curr out start ", self.curr_out_start)
        print("output valid ", self.output_valid)
        print("pause tb ", self.pause_tb)
        print("pause output ", self.pause_output)
        print("prev pause output ", self.prev_pause_output)
        print("started ", self.started)
        print("rdy ", self.rdy_to_arbiter)

    def transpose_buffer(self, input_data, valid_data, ack_in):
        print("input data", input_data)
        print("valid data ", valid_data)
        print("ack in ", ack_in)

        self.input_to_tb(input_data, valid_data)
        self.output_from_tb(valid_data, ack_in)
        self.print_tb()
        return self.col_pixels, self.output_valid, self.rdy_to_arbiter, self.index_inner, self.index_outer
