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
        self.pause_output = 1
        self.rdy_to_arbiter = 1
        self.start_data = 0
        self.old_start_data = 0
        self.prev_rdy_to_arbiter = 1
        self.rdy2 = 1
        self.prev_rdy_to_arbiter2 = 1
        self.prev_pause_output = 1
        self.prev_pause_tb = 1
        self.started = 0
        self.output_index_abs = 0
        self.prev_started = 0
        self.prev_out2 = 1
        self.prev_output_valid2 = 0
        self.prev_col_pixels2 = []
        self.prev_col_pixels3 = []
        self.output_valid_prior = 0

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
        self.prev_out_buf_index = 0

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
        self.pause_output = 1
        self.rdy_to_arbiter = 1
        self.prev_col_pixels = 0
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
        self.prev_started = 0
        self.prev_out2 = 1
        self.prev_output_valid2 = 0
        self.prev_col_pixels2 = []
        self.prev_col_pixels3 = []
        self.output_valid_prior = 0
        self.top_col_pixels = []
        self.top_output_valid = 0

    def get_col_pixels(self):
        return self.top_col_pixels

    def get_output_valid(self):
        return self.top_output_valid

    def get_rdy_to_arbiter(self):
        return self.rdy_to_arbiter

    def output_from_tb(self, input_data, valid_data, ack_in, ren):
        self.prev_output_valid2 = self.prev_output_valid
        self.prev_col_pixels2 = self.prev_col_pixels

        self.prev_ii = self.index_inner
        self.prev_io = self.index_outer

        self.prev_output_valid = self.output_valid
        self.prev_col_pixels = self.col_pixels
        
        if self.config["dimensionality"] == 0:
            self.output_valid = 0
            self.col_pixels = [0]
        else:
            if self.start_data and not self.old_start_data:
                self.rdy_to_arbiter = 1
            elif self.prev_out_buf_index != self.out_buf_index:
                self.rdy_to_arbiter = 1
            elif self.tb_height != 1:
                if self.row_index != self.tb_height - 1:
                    self.rdy_to_arbiter = 1
            elif ack_in:
                self.rdy_to_arbiter = 0

            self.old_start_data = self.start_data
            if valid_data and (not self.start_data):
                self.start_data = 1

            self.output_valid = self.output_valid_prior
            if self.pause_output:
                self.output_valid_prior = 0
            else:
                self.output_valid_prior = 1

            if self.config["dimensionality"] == 1:
                if self.index_outer == self.config["range_outer"] - 1:
                    if not self.pause_output:
                        self.pause_tb = 1 - valid_data
                    else:
                        self.pause_tb = 0
                elif self.pause_tb:
                    self.pause_tb = 1 - valid_data
                elif not self.pause_output:
                    self.pause_tb = 0
            else:
                if self.index_inner == self.config["range_inner"] - 1:
                    if self.index_outer == self.config["range_outer"] - 1:
                        if not self.pause_output:
                            self.pause_tb = 1 - valid_data
                        else:
                            self.pause_tb = 0
                    else:
                        self.pause_tb = 0
                elif self.pause_tb == 1:
                    self.pause_tb = 1 - valid_data
                elif not self.pause_output:
                    self.pause_tb = 0


            if self.config["dimensionality"] == 1:
                if self.index_outer == self.config["range_outer"] - 1:
                    if not self.pause_output:
                        self.index_outer = 0
                    else:
                        self.index_outer = self.index_outer
                elif self.pause_tb:
                    self.index_outer = self.index_outer
                elif not self.pause_output:
                    self.index_outer = self.index_outer + 1
            else:
                if self.index_inner == self.config["range_inner"] - 1:
                    if self.index_outer == self.config["range_outer"] - 1:
                        if not self.pause_output:
                            self.index_outer = 0
                        else:
                            self.index_outer = self.index_outer
                    elif not self.pause_output:
                        self.index_outer = self.index_outer + 1

            if self.config["dimensionality"] == 1:
                self.index_inner = 0
            else:
                if self.index_inner == self.config["range_inner"] - 1:
                    if not self.pause_output:
                        self.index_inner = 0
                elif self.pause_tb:
                    self.index_inner = self.index_inner
                elif not self.pause_output:
                    self.index_inner = self.index_inner + 1

            if valid_data:
                self.tb[self.row_index + self.tb_height * self.input_buf_index] = input_data
                if self.row_index == self.tb_height - 1:
                    self.row_index = 0
                    self.input_buf_index = 1 - self.input_buf_index
                else:
                    self.row_index = self.row_index + 1

            self.col_pixels = []
            for i in range(self.tb_height):
                self.col_pixels.append(
                    self.tb[i + self.tb_height * (1 - self.out_buf_index)][self.output_index])
            #self.this_iter_curr_out_start = self.curr_out_start
            #if self.output_index_abs >= self.curr_out_start + self.fetch_width:
            #    self.curr_out_start = self.curr_out_start + self.fetch_width

            if self.config["dimensionality"] == 1:
                self.output_index_abs = self.index_outer * self.config["stride"]
            else:
                self.output_index_abs = self.index_outer * self.config["stride"] + \
                    self.config["indices"][self.index_inner]
            self.output_index = self.output_index_abs % self.fetch_width

            self.this_iter_curr_out_start = self.curr_out_start
            if self.output_index_abs >= self.curr_out_start + self.fetch_width:
                self.curr_out_start = self.curr_out_start + self.fetch_width


            #if (self.prev_ii == 0) and (self.prev_io == 0):
            #    self.prev_out_buf_index = 0
            #else:
            self.prev_out_buf_index = self.out_buf_index

#            if (self.index_inner == 0) and (self.index_outer == 0):
#                self.out_buf_index = 1
            
            if (self.output_index_abs >= self.this_iter_curr_out_start + self.fetch_width):
                self.out_buf_index = 1 - self.out_buf_index

            if self.pause_tb:
                self.pause_output = 1
            elif self.start_data and (not self.old_start_data):
                self.pause_output = 1
            else:
                self.pause_output = 1 - ren

            if self.config["dimensionality"] == 1:
                self.top_output_valid = self.prev_output_valid
                self.top_col_pixels = self.prev_col_pixels2
            else:
                self.top_output_valid = self.output_valid
                self.top_col_pixels = self.prev_col_pixels

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
        print("prev col pixels ", self.prev_col_pixels)
        print("output index abs", self.output_index_abs)
        print("output index ", self.output_index)
        print("index inner ", self.index_inner)
        print("index outer ", self.index_outer)
        print("curr out start ", self.curr_out_start)
        print("prev output valid", self.prev_output_valid)
        print("output valid ", self.output_valid)
        print("pause tb ", self.pause_tb)
        print("pause output ", self.pause_output)
        print("rdy ", self.rdy_to_arbiter)
        print("prev rdy ", self.prev_rdy_to_arbiter)
        print("prev rdy 2", self.prev_rdy_to_arbiter2)
        print("prev output valid", self.prev_output_valid)
        print("start data", self.start_data)
        print("old start data", self.old_start_data)
        print("started", self.started)
        print("col pixels", self.col_pixels)
        print("out valid", self.output_valid)
        print("prev col", self.prev_col_pixels)
        print("prev val", self.prev_output_valid)
        print("prev col2", self.prev_col_pixels2)
        print("prev val2", self.prev_output_valid2)

    def interact(self, input_data, valid_data, ack_in, ren):
        # print("before")
        # self.print_tb(input_data, valid_data, ack_in)
        self.output_from_tb(input_data, valid_data, ack_in, ren)
        # print(" ")
        # print("after")
        # self.print_tb(input_data, valid_data, ack_in, ren)
        # print(" ")
        # return self.prev_col_pixels2, self.prev_output_valid, self.rdy_to_arbiter
        return self.prev_col_pixels2, self.prev_output_valid, self.rdy_to_arbiter
