from lake.models.model import Model


# transpose buffer model
class TBModel(Model):

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

        # initialize transpose buffer
        self.tb = []
        if self.fetch_width == 1:
            for i in range(2 * self.tb_height):
                self.tb.append(0)
        else:
            for i in range(2 * self.tb_height):
                row = []
                for j in range(self.fetch_width):
                    row.append(0)
                self.tb.append(row)

        self.tb_valid = []
        for i in range(2 * self.tb_height):
            self.tb_valid.append(0)

        self.mask_valid = 0

        self.row_index = 0
        self.input_buf_index = 0
        self.out_buf_index = 1
        self.prev_out_buf_index = 0

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
        self.start_data = 0
        self.old_start_data = 0
        self.output_index_abs = 0
        self.prev_out2 = 1
        self.prev_col_pixels2 = []
        self.prev_col_pixels3 = []
        self.on_next_line = 0
        self.switch_next_line = 0
        self.switch_out_buf = 0

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

        self.tb = []
        if self.fetch_width == 1:
            for i in range(2 * self.tb_height):
                self.tb.append(0)
        else:
            for i in range(2 * self.tb_height):
                row = []
                for j in range(self.fetch_width):
                    row.append(0)
                self.tb.append(row)

        self.tb_valid = []
        for i in range(2 * self.tb_height):
            self.tb_valid.append(0)

        self.mask_valid = 0

        self.row_index = 0
        self.input_buf_index = 0
        self.out_buf_index = 1
        self.prev_out_buf_index = 0

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
        self.prev_col_pixels = 0
        self.start_data = 0
        self.old_start_data = 0
        self.output_index_abs = 0
        self.on_next_line = 0
        self.switch_next_line = 0
        self.switch_out_buf = 0

    def get_col_pixels(self):
        return self.col_pixels

    def get_output_valid(self):
        return self.output_valid

    def get_rdy_to_arbiter(self):
        return self.rdy_to_arbiter

    def output_from_tb(self, input_data, valid_data, ack_in, ren, mem_valid_data):

        # Grab current values...
        index_inner_curr = self.index_inner
        index_outer_curr = self.index_outer
        pause_tb_curr = self.pause_tb
        row_index_curr = self.row_index
        input_buf_index_curr = self.input_buf_index
        # tb
        # col pixels
        output_index_curr = self.output_index
        output_valid_curr = self.output_valid
        out_buf_index_curr = self.out_buf_index
        curr_out_start_curr = self.curr_out_start
        prev_out_buf_index_curr = self.prev_out_buf_index
        start_data_curr = self.start_data
        old_start_data_curr = self.old_start_data
        rdy_to_arbiter_curr = self.rdy_to_arbiter
        on_next_line_curr = self.on_next_line
        tb_curr = self.tb.copy()

        # Combinational updates

        if ((index_outer_curr == 0) and (not on_next_line_curr) and
                ((self.config["dimensionality"] == 1) or
                    ((self.config["dimensionality"] == 2) & (index_inner_curr == 0)))):
            self.switch_next_line = 1
        else:
            self.switch_next_line = 0

        if pause_tb_curr:
            self.pause_output = 1
        else:
            self.pause_output = 1 - ren

        if self.config["dimensionality"] == 1:
            self.output_index_abs = index_outer_curr * self.config["stride"] + self.config["starting_addr"]
        else:
            self.output_index_abs = index_outer_curr * self.config["stride"] + \
                self.config["indices"][index_inner_curr] + \
                self.config["starting_addr"]

        self.output_index = self.output_index_abs % self.fetch_width

        if self.switch_next_line | (self.output_index_abs >= curr_out_start_curr + self.fetch_width):
            self.switch_out_buf = 1
        else:
            self.switch_out_buf = 0

        # output from tb
        if self.config["dimensionality"] == 0:
            self.col_pixels = [0]
        else:
            self.col_pixels = []
            for i in range(self.tb_height):
                if self.fetch_width == 1:
                    self.col_pixels.append(tb_curr[i + self.tb_height * (1 -
                                                   (out_buf_index_curr ^
                                                       self.switch_out_buf))])
                else:
                    self.col_pixels.append(
                        tb_curr[i + self.tb_height * (1 - (out_buf_index_curr ^
                                self.switch_out_buf))][self.output_index])

        # Set mask valid
        if out_buf_index_curr ^ self.switch_out_buf:
            self.mask_valid = self.tb_valid[0]
        else:
            self.mask_valid = self.tb_valid[1]

        # Set output valid
        if self.config["dimensionality"] == 0:
            self.output_valid = 0
        elif self.pause_output:
            self.output_valid = 0
        else:
            self.output_valid = self.mask_valid

        # Stateful updates...

        if self.config["dimensionality"] != 0:
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
            if index_outer_curr == self.config["range_outer"] - 1:
                if (self.config["dimensionality"] == 1) or \
                        ((self.config["dimensionality"] == 2) and
                            (index_inner_curr == self.config["range_inner"] - 1)):
                    if not self.pause_output:
                        if (not rdy_to_arbiter_curr) or valid_data:
                            self.pause_tb = 0
                        else:
                            self.pause_tb = 1
            elif pause_tb_curr:
                self.pause_tb = 1 - valid_data

            # Row index + input_buf_index + input tb
            if valid_data:
                self.tb[row_index_curr + self.tb_height * input_buf_index_curr] = input_data
                self.tb_valid[row_index_curr + self.tb_height * input_buf_index_curr] = mem_valid_data
                if row_index_curr == self.tb_height - 1:
                    self.row_index = 0
                    self.input_buf_index = 1 - input_buf_index_curr
                else:
                    self.row_index = row_index_curr + 1

            # Set out buf index
            if not start_data_curr:
                self.out_buf_index = 1
            elif self.config["dimensionality"] == 1:
                if (index_outer_curr == 0) and (not on_next_line_curr):
                    self.out_buf_index = 1 - out_buf_index_curr
                elif (self.output_index_abs >= curr_out_start_curr + self.fetch_width):
                    self.out_buf_index = 1 - out_buf_index_curr
            elif self.config["dimensionality"] == 2:
                if (index_inner_curr == 0) and (index_outer_curr == 0) and (not on_next_line_curr):
                    self.out_buf_index = 1 - out_buf_index_curr
                elif (self.output_index_abs >= curr_out_start_curr + self.fetch_width):
                    self.out_buf_index = 1 - out_buf_index_curr

            # Set on next line
            if self.config["dimensionality"] == 1:
                if (index_outer_curr == 0) and (not on_next_line_curr):
                    self.on_next_line = 1
                elif (index_outer_curr == self.config["range_outer"] - 1):
                    self.on_next_line = 0
            elif self.config["dimensionality"] == 2:
                if (index_inner_curr == 0) and (index_outer_curr == 0) and (not on_next_line_curr):
                    self.on_next_line = 1
                elif (index_inner_curr == self.config["range_inner"] - 1):
                    if (index_outer_curr == self.config["range_outer"] - 1):
                        self.on_next_line = 0

            # Set curr out start
            if index_outer_curr == 0:
                if self.config["dimensionality"] == 1:
                    self.curr_out_start = 0
                elif (self.config["dimensionality"] == 2) and (index_inner_curr == 0):
                    self.curr_out_start = 0
            elif self.output_index_abs >= curr_out_start_curr + self.fetch_width:
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
            elif self.switch_out_buf:
                self.rdy_to_arbiter = 1
            elif self.tb_height != 1:
                if row_index_curr != self.tb_height - 1:
                    self.rdy_to_arbiter = 1
            elif ack_in:
                self.rdy_to_arbiter = 0

    def print_tb(self, input_data, valid_data, ack_in, ren, mem_valid_data):
        print("INPUTS")

        print("input data: ", input_data)
        print("valid data: ", valid_data)
        print("ack in ", ack_in)
        print("ren ", ren)
        print("mem valid data", mem_valid_data)

        print("input index ", self.tb_height * self.input_buf_index + self.row_index)
        print("tb: ", self.tb)
        print("row index: ", self.row_index)
        print("input buf index: ", self.input_buf_index)
        print("out buf index ", self.out_buf_index)
        print("prev out buf index ", self.prev_out_buf_index)
        print("col pixels ", self.col_pixels)
        print("prev col pixels ", self.prev_col_pixels)
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
        print("out valid", self.output_valid)
        print("switch out buf", self.switch_out_buf)
        print("mask valid", self.mask_valid)
        print("tb valid", self.tb_valid)

    def interact(self, input_data, valid_data, ack_in, ren, mem_valid_data):
        # print("before")
        # self.print_tb(input_data, valid_data, ack_in, ren)
        self.output_from_tb(input_data, valid_data, ack_in, ren, mem_valid_data)
        # print(" ")
        # print("after")
        # self.print_tb(input_data, valid_data, ack_in, ren, mem_valid_data)
        # print(" ")
        return self.col_pixels, self.output_valid, self.rdy_to_arbiter
