from lake.models.model import Model


class AggAlignerModel(Model):
    '''
    Model for agg aligner
    '''
    def __init__(self,
                 data_width,
                 max_line_length):

        self.max_line_length = max_line_length
        self.data_width = data_width

        self.config = {}
        self.config["line_length"] = 0

        self.count = 0

    def set_config(self, new_config):
        for key, config_val in new_config.items():
            if key not in self.config:
                AssertionError("Gave bad config...")
            else:
                self.config[key] = config_val

        assert self.config["line_length"] <= self.max_line_length, "Exceeded max line length"

    def interact(self, data_in, valid_in):
        '''
        Returns (data_out, valid, align)
        '''
        if valid_in == 0:
            return (data_in, 0, 0)
        else:
            align = 0
            self.count += 1
            if(self.count == self.config["line_length"]):
                self.count = 0
                align = 1
            return (data_in, 1, align)
