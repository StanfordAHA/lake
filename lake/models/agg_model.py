from lake.models.model import Model


class AggModel(Model):

    def __init__(self,
                 num_elts):

        self.num_elts = num_elts

        self.shift_reg = []
        for i in range(self.num_elts):
            self.shift_reg.append(0)

        self.word_count = 0
        self.valid_out = 0
        self.curr_valid = 0
        self.sr_copy = 0

    def set_config(self, **kwargs):
        self.word_count = 0
        self.valid_out = 0
        return

    def get_valid_out(self):
        return self.curr_valid

    def get_data_out(self):
        return self.sr_copy

    def insert(self, data, valid_in):
        self.curr_valid = self.valid_out
        self.sr_copy = self.shift_reg.copy()
        if valid_in:
            self.shift_reg[self.word_count] = data
            self.word_count = self.word_count + 1
            if(self.word_count >= self.num_elts):
                self.word_count = 0
                self.valid_out = 1
            else:
                self.valid_out = 0
        return self.curr_valid

