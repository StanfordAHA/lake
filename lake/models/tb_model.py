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
        self.config["dimensionality"] = 1

        # initialize transpose buffer
        if self.fetch_width == 1:
            self.tb = []
            for i in range(2 * self.tb_height):
                self.tb.append(0)
        else:
            self.tb = []
            for i in range(2 * self.tb_height):
                row = []
                for j in range(self.fetch_width):
                    row.append(0)
                self.tb.append(row)

        self.row_index = 0
        self.input_buf_index = 0
        self.out_buf_index = 1
        self.prev_out_buf_index = 0

        self.ret_col_pixels = 0
        self.ret_output_valid = 0
        self.ret_rdy_to_arb = 1

        self.col_pixels = []
        for i in range(self.tb_height):
            self.col_pixels.append(0)

        self.output_index = 0
        self.index_inner = 0
        self.index_outer = 0
        self.curr_out_start = 0
        self.this_iter_curr_out_start = 0
        self.output_valid = 0
        self.pause_tb = 1
        self.pause_output = 0
        self.rdy_to_arbiter = 1
        self.prev_output_valid = 0
        self.start_data = 0
        self.old_start_data = 0
        self.prev_rdy_to_arbiter = 1
        self.rdy2 = 1
        self.prev_rdy_to_arbiter2 = 1
        self.prev_pause_output = 1
        self.prev_pause_tb = 1
        self.started = 0
        self.output_index_abs = 0

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

        if self.fetch_width == 1:
            self.tb = []
            for i in range(2 * self.tb_height):
                self.tb.append(0)
        else:
            self.tb = []
            for i in range(2 * self.tb_height):
                row = []
                for j in range(self.fetch_width):
                    row.append(0)
                self.tb.append(row)

        self.row_index = 0
        self.input_buf_index = 0
        self.out_buf_index = 1
        self.prev_out_buf_index = 0

        self.ret_col_pixels = 0
        self.ret_output_valid = 0
        self.ret_rdy_to_arb = 1

        self.col_pixels = []
        for i in range(self.tb_height):
            self.col_pixels.append(0)

        self.output_index = 0
        self.index_inner = 0
        self.index_outer = 0
        self.curr_out_start = 0
        self.output_valid = 0
        self.pause_tb = 1
        self.pause_output = 0
        self.rdy_to_arbiter = 1
        self.ret_col_pixels = 0
        self.ret_output_valid = 0
        self.ret_rdy_to_arb = 1
        self.prev_output_valid = 0
        self.start_data = 0
        self.old_start_data = 0
        self.output_index_abs = 0

    def get_col_pixels(self):
        return self.col_pixels

    def get_output_valid(self):
        return self.output_valid

    def get_rdy_to_arbiter(self):
        return self.rdy_to_arbiter

    def output_from_tb(self, input_data, valid_data, ack_in, ren):

        self.ret_col_pixels = self.col_pixels
        self.ret_output_valid = self.output_valid
        self.ret_rdy_to_arb = self.rdy_to_arbiter

        # Grab current values...
        index_inner_curr = self.index_inner
        index_outer_curr = self.index_outer
        pause_tb_curr = self.pause_tb
        row_index_curr = self.row_index
        input_buf_index_curr = self.input_buf_index
        # tb
        # col pixels
        output_index_curr = self.output_index
        prev_output_valid_curr = self.prev_output_valid
        output_valid_curr = self.output_valid
        out_buf_index_curr = self.out_buf_index
        curr_out_start_curr = self.curr_out_start
        prev_out_buf_index_curr = self.prev_out_buf_index
        start_data_curr = self.start_data
        old_start_data_curr = self.old_start_data
        rdy_to_arbiter_curr = self.rdy_to_arbiter
        tb_curr = self.tb.copy()

        # Perform combinational updates...
        if pause_tb_curr:
            self.pause_output = 1
        elif start_data_curr and (not old_start_data_curr):
            self.pause_output = 1
        else:
            self.pause_output = 1 - ren

        if self.config["dimensionality"] == 1:
            self.output_index_abs = index_outer_curr * self.config["stride"]
        else:
            self.output_index_abs = index_outer_curr * self.config["stride"] + \
                self.config["indices"][index_inner_curr]

        # Stateful updates...

        if self.config["dimensionality"] == 0:
            self.output_valid = 0
            self.col_pixels = [0]
        else:
            # Index outer
            if self.config["dimensionality"] == 1:
                if index_outer_curr == self.config["range_outer"] - 1:
                    if not self.pause_output:
                        self.index_outer = 0
                    else:
                        self.index_outer = index_outer_curr
                elif pause_tb_curr:
                    self.index_outer = index_outer_curr
                elif not self.pause_output:
                    self.index_outer = index_outer_curr + 1
            else:
                if index_inner_curr == self.config["range_inner"] - 1:
                    if index_outer_curr == self.config["range_outer"] - 1:
                        if not self.pause_output:
                            self.index_outer = 0
                        else:
                            self.index_outer = index_outer_curr
                    elif not self.pause_output:
                        self.index_outer = index_outer_curr + 1
            # Index inner
            if self.config["dimensionality"] == 1:
                self.index_inner = 0
            else:
                if index_inner_curr == self.config["range_inner"] - 1:
                    if not self.pause_output:
                        self.index_inner = 0
                elif pause_tb_curr:
                    self.index_inner = index_inner_curr
                elif not self.pause_output:
                    self.index_inner = index_inner_curr + 1
            # Pause tb
            if self.config["dimensionality"] == 1:
                if index_outer_curr == self.config["range_outer"] - 1:
                    if not self.pause_output:
                        self.pause_tb = 1 - valid_data
                    else:
                        self.pause_tb = 0
                elif pause_tb_curr:
                    self.pause_tb = 1 - valid_data
                elif not self.pause_output:
                    self.pause_tb = 0
            else:
                if index_inner_curr == self.config["range_inner"] - 1:
                    if index_outer_curr == self.config["range_outer"] - 1:
                        if not self.pause_output:
                            self.pause_tb = 1 - valid_data
                        else:
                            self.pause_tb = 0
                    else:
                        self.pause_tb = 0
                elif pause_tb_curr == 1:
                    self.pause_tb = 1 - valid_data
                elif not self.pause_output:
                    self.pause_tb = 0
            # Row index + input_buf_index + input tb
            if valid_data:
                self.tb[row_index_curr + self.tb_height * input_buf_index_curr] = input_data
                if row_index_curr == self.tb_height - 1:
                    self.row_index = 0
                    self.input_buf_index = 1 - input_buf_index_curr
                else:
                    self.row_index = row_index_curr + 1
            # output from tb
            self.col_pixels = []
            for i in range(self.tb_height):
                if self.fetch_width == 1:
                    self.col_pixels.append(tb_curr[i + self.tb_height * (1 - out_buf_index_curr)])
                else:
                    self.col_pixels.append(
                        tb_curr[i + self.tb_height * (1 - out_buf_index_curr)][output_index_curr])
            # Set output index
            self.output_index = self.output_index_abs % self.fetch_width

            # Set prev output valid
            if self.pause_output:
                self.prev_output_valid = 0
            else:
                self.prev_output_valid = 1

            # Set output valid
            self.output_valid = prev_output_valid_curr

            # Set out buf index
            if (self.output_index_abs >= curr_out_start_curr + self.fetch_width):
                self.out_buf_index = 1 - out_buf_index_curr

            # Set curr out start
            if self.output_index_abs >= curr_out_start_curr + self.fetch_width:
                self.curr_out_start = curr_out_start_curr + self.fetch_width

            # Set pref out buf index
            self.prev_out_buf_index = out_buf_index_curr

            # Set start data
            if valid_data and (not start_data_curr):
                self.start_data = 1
            # Set old start data
            self.old_start_data = start_data_curr
            # Set rdy to arbiter
            if start_data_curr and not old_start_data_curr:
                self.rdy_to_arbiter = 1
            elif prev_out_buf_index_curr != out_buf_index_curr:
                self.rdy_to_arbiter = 1
            elif self.tb_height != 1:
                if row_index_curr != self.tb_height - 1:
                    self.rdy_to_arbiter = 1
            elif ack_in:
                self.rdy_to_arbiter = 0

    def print_tb(self, input_data, valid_data, ack_in, ren):
        print("INPUTS")

        print("input data: ", input_data)
        print("valid data: ", valid_data)
        print("ack in ", ack_in)
        print("ren ", ren)

        print("input index ", self.tb_height * self.input_buf_index + self.row_index)
        print("tb: ", self.tb)
        print("row index: ", self.row_index)
        print("input buf index: ", self.input_buf_index)
        print("out buf index ", self.out_buf_index)
        print("prev out buf index ", self.prev_out_buf_index)
        print("col pixels ", self.col_pixels)
        print("output index abs", self.output_index_abs)
        print("output index ", self.output_index)
        print("index inner ", self.index_inner)
        print("index outer ", self.index_outer)
        print("curr out start ", self.curr_out_start)
        print("output valid ", self.output_valid)
        print("pause tb ", self.pause_tb)
        print("pause output ", self.pause_output)
        print("rdy ", self.rdy_to_arbiter)
        print("start data", self.start_data)
        print("old start data", self.old_start_data)
        print("col pixels", self.col_pixels)
        print("output valid", self.output_valid)

    def interact(self, input_data, valid_data, ack_in, ren):
        # print("before")
        # self.print_tb(input_data, valid_data, ack_in)
        self.output_from_tb(input_data, valid_data, ack_in, ren)
        # print(" ")
        # print("after")
        # self.print_tb(input_data, valid_data, ack_in, ren)
        # print(" ")
        return self.ret_col_pixels, self.ret_output_valid, self.ret_rdy_to_arb
