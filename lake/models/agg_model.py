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

    def interact(self, data, valid_in, align):
        '''
            Returns (agg_out, valid_out, next_full)
        '''

        self.curr_valid = self.valid_out
        self.sr_copy = self.shift_reg.copy()
        word_count_curr = self.word_count

        next_full = (valid_in & (word_count_curr == self.num_elts - 1)) | align
        if valid_in:
            if next_full:
                self.valid_out = 1
                self.word_count = 0
            else:
                self.valid_out = 0
                self.word_count = word_count_curr + 1
            self.shift_reg[word_count_curr] = data
        else:
            self.valid_out = 0
        return (self.sr_copy, self.curr_valid, next_full)
